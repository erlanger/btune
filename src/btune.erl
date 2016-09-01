-module(btune).

%% Exports
-export([bcast/2,listen/1,unlisten/1,
         reg/1,reg/2,unreg/1,
         match/2,match/1,match_rnd/1,
         count/1,
         lookup_values/1,lookup_values/2,
         get_pid/1,get_pid/2
        ]).

% {via, btune, Key} exports
-export([register_name/2,
         unregister_name/1,
         whereis_name/1,
         send/2]).

-ifdef(TEST).
-export([test_start/0,startvm/2]).
-endif.

-type key()   :: term().
% Key is any `term()', however in order to use the `plisteners(...)'
% functions the key needs to be a tuple.

-include_lib("stdlib/include/ms_transform.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-define(TIMEOUT,3000).


%% API
%% ===

% @doc Send `Msg' to processes registered with `Key' in all connected nodes.
% @end
-spec bcast(Key::key(),Msg::any()) -> any().
bcast(Key,Msg) ->
   gproc:bcast([node()|nodes()],{p,l,Key},Msg).

% @doc Registers the calling process with `Key'.
% @end
-spec reg(Key::key()) -> true.
reg(Key) ->
   gproc:reg({p,l,Key}).

% @doc Registers the calling process with `Key' and `Value'.
% @end
-spec reg(Key::key(),Value::term()) -> true.
reg(Key,Value) ->
   gproc:reg({p,l,Key},Value).

% @doc Unregisters the `Key' that was previously registered by the
%      calling process.
% @end
-spec unreg(Key::key()) -> true.
unreg(Key) ->
   gproc:unreg({p,l,Key}).

% @doc Adds the calling process to the list of listeners of  `Key'.
% @end
-spec listen(Key::key()) -> true.
listen(Key) ->
   reg(Key).

% @doc Removes the calling process from the list of listeners of  `Key'.
% @end
-spec unlisten(Key::key()) -> true.
unlisten(Key) ->
   unreg(Key).

% @doc Like {@link match/1} but returns one random entry.
%      If there is no match a `no_match' error is raised.
% @end
-spec match_rnd(Pattern::term()) -> {key(),pid(),Value::term()}.
match_rnd(Pattern) ->
   case match(Pattern) of
      [] -> error(no_match);
      L  -> lists:nth( rand:uniform(length(L)), L)
   end.

% @doc match keys againts `Pattern' as in {@link ets:match/2}.
%      The easiest way to use this function is to think of
%      pattern matching in erlang. Simply use a '_' if you
%      don't want to match on that value and an actual `term()'
%      if you want to match.
%
%      For example ``{'_',hello,'_'}'' will match
%      ``{ "hi",hello, "it is me" }'', but not
%      ``{ "hi",hey, "it is me"}''.
%      Here is an example:
%      ```
%      n1@host> btune:listen({mykey,param1,param2}).
%      true
%      ...
%      n2@host> btune:listen({mykey,param3,param4}).
%      true
%      ...
%      n1@host> btune:match({mykey,'_','_'}).
%      [{{mykey,param1,param2},<0.43,0>,undefined},
%         {{mykey,param3,param4},<3332.43.0>,undefined}]
%      '''
%      If there is no match an empty list `[]' is returned.
%
%      A default timeout of 3 seconds is presumed.
%      Use {@link match/2} if you want a different one.
% @end
-spec match(Pattern::term()) -> [{key(),pid(),Value::term()}].
match(Pattern) ->
   match(Pattern,?TIMEOUT).

% @doc Same as {@link match/1} but with a specific timeout in milliseconds.
% @end
-spec match(Pattern::term(),Timeout::integer()) -> [{key(),pid(),Value::term()}].
match(Pattern,Timeout) ->
   MS=[{{{p,l,Pattern},'_','_'}, %Pid and Value are not used to match (but are returned)
          [],                    %Guard is empty
        ['$$']}],                %Return list of expected [{p,l,Key},Pid,Value]

   R=mcall(gproc,select,[{l,p},MS],Timeout),
   getresult(lists:append(R)).

% @doc Returns true if there are no listeners for keys that match the given `Pattern',
%      otherwise returns the number of matches for `Pattern'.
%
%      The `Pattern' is the same as in {@link match/1}.
% @end
-spec count(Pattern::term()) -> true|integer().
count(Pattern) ->
   R=match(Pattern),
   length(R).

% @private
% @doc Returns `[{pid(),value()]' list for  `Key' in the cluster.
% @equiv listeners(Key,DEFAULT_TIMEOUT)
% @end
-spec lookup_values(Key::key()) -> [{pid(), Value::term()}].
lookup_values(Key) ->
   lookup_values(Key,?TIMEOUT).

% @private
% @doc   Same as {@link lookup_values/1} but with the a specified timeout.
% @end
-spec lookup_values(Key::key(),Timeout::integer()) -> [{pid(), Value::term()}].
lookup_values(Key,Timeout) ->
  R=mcall(gproc,lookup_values,[{p,l,Key}],Timeout),
  lists:append(R).

% @doc Returns the `pid()' of the first process matching `Key'
%      in the cluster, but it does not include any listeners.
%
%      The `pid()' returned is that of the first process in the cluster
%      that was registered with a gproc `{n,l,Key}'. This is the
%      case when using `{via,gproc,{n,l,Key}}' in any of the gen_xxx
%      behaviours. By first, it is meant the node that responds first.
%
%      btune always uses gproc keys of the form `{p,l,Key}' when you
%      call `btune:listen(...)', but many times the listener wants to
%      send a message back to a server. Servers can be registered using
%      `{via,gproc,{n,l,Key}}' in any of the gen_xxx `start_link(...)'
%      functions, and this function will help the listener to get the
%      pid of that sever in the cluster.
%
% @equiv get_pid(Key,DEFAULT_TIMEOUT)
% @end
-spec get_pid(Key::key()) -> [{pid(), Value::term()}].
get_pid(Key) ->
   get_pid(Key,?TIMEOUT).

% @doc   Same as {@link get_pid/1} but with the a specified timeout.
% @end
-spec get_pid(Key::key(),Timeout::integer()) -> pid().
get_pid(Key,Timeout) ->
   R=mcall(gproc,lookup_pids,[{n,l,Key}],Timeout),
   case lists:append(R) of
      []  -> error(badarg);
      L0  -> lists:nth(1,L0)
   end.

%% Global-like api to use in {via, btune, Key}
%% specifications

% @doc Registers a process in gproc with an `{n,l,Name}' key.
%      This function is provided so that `{via, btune, Name}' can
%      be used in any of the gen_xxx behaviours.
% @end
-spec register_name(Name, Pid) -> 'yes' | 'no'
      when Name :: term(), Pid :: pid().
register_name(Name, Pid) when is_pid(Pid) ->
   gproc:register_name({n,l,Name},Pid).

% @doc Unregisters a process in gproc with an `{n,l,Name}' key.
%      This function is provided so that `{via, btune, Name}' can
%      be used in any of the gen_xxx behaviours.
% @end
-spec unregister_name(Key::key()) -> true.
unregister_name(Key) ->
   gproc:unregister_name({n,l,Key}).

% @doc Returns the `pid()' of a process that was previously
%      registered with `register_name/2'.
%      This function is provided so that `{via, btune, Name}' can
%      be used in any of the gen_xxx behaviours.
% @end
-spec whereis_name(Key::key()) -> pid() | undefined.
whereis_name(Key) ->
   try
      get_pid(Key)
   catch
      error:badarg ->
         undefined
   end.

% @doc Sends `Msg' to a process that was registered with
%      `register_name/2'. The first node that responds
%      having a  process with an `{n,l,Key}' determines
%      where the message is sent.
%      This function is provided so that `{via, btune, Name}' can
%      be used in any of the gen_xxx behaviours.
% @end
-spec send(Key::key(), Msg::any()) -> Msg::any().
send(Key,Msg) ->
   gproc:send(get_pid(Key),Msg).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal utility functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mcall(M,F,A,Timeout) ->
   {R,_} = rpc:multicall(M,F,A,Timeout),
   R.

%get keys from list of the form [[{p,l,Key},Pid,Value},...],[...]]
getresult([]) ->
   [];

getresult([[{p,l,Key},Pid,Val]|R]) ->
   lists:append([{Key,Pid,Val}],getresult(R)).


%%%---------------------------------------------------------------------
%%% Unit testing
%%%---------------------------------------------------------------------

-ifdef(EUNIT).

-define(tt(T,F), {T,timeout, 20, ?_test(F)}).
-define(run(Proc,RetVal,M,F,A),?assertMatch(RetVal when RetVal =/= timeout,
                                     begin
                                        global:send(Proc, {self(), func, M, F, A}),
                                        receive V0 -> V0 after 3000 -> timeout end
                                     end)).
-define(reg(Proc,T,V), begin global:send(Proc, {self(), reg, T, V}), receive ok -> ok after 1000 -> timeout end end).
-define(recvmatch(Value), ?assertMatch(Value,begin receive V -> V after 4000 -> timeout end end)).
-define(NOERROR(Expr),try Expr catch _:_ -> ok end).

%FIXME: this should probably use slave:start_link or eunit {node,...}
% @private
startvm(Name,Code) ->
   Node = atom_to_list(node()),
   Exe = "erl -noinput -sname " ++ Name
            ++ " -setcookie btune -pa ebin -pa ../deps/**/ebin "
            ++ " -eval net_adm:ping'(" ++ Node ++ ")'"
            ++ " -eval " ++ Code,
   ?debugFmt("Starting ~p",[Exe]),
   P = open_port({spawn,Exe},[exit_status]),
   receive {alive,From} ->   {P,From}
             after 5000 ->   close(P), io:format("Unable to start test VM"), vm_not_started
   end.

close(P) when is_port(P) ->
   P ! {self(), close};
close(_P) ->
   ok.

% @private
test_start() ->
   timer:sleep(500),
   io:format(user,"gproc start=~p",[application:start(gproc)]),
   global:send(btune_test,{alive,self()}),
   loop().

loop() ->
   receive
      {From,reg,Tuple,Value} ->
         gproc:reg(Tuple,Value),
         From ! ok, loop();

      {From, func, M, F, A} ->
         From ! apply(M,F,A), loop();

      {{Key,From},Msg} ->
         %os:cmd("espeak 'received " ++ Msg ++ "'"),
         From ! {Key,Msg}, loop();

      stop -> halt(0)

   after 7000 ->
         halt(0)
   end.

exec_test_() ->
    {setup,
        fun() ->
            %Start nodes
            {ok,_}=net_kernel:start([node0,shortnames]),
            true=erlang:set_cookie(node(),btune),
            yes=global:register_name(btune_test,self()),
            ok=application:start(gproc),
            {P1,Proc1}=startvm("node1","btune:test_start'()'"),
            {P2,Proc2}=startvm("node2","btune:test_start'()'"),
            yes=global:register_name(node1,Proc1),
            yes=global:register_name(node2,Proc2),
            {P1,P2}
        end,
        fun({P1,P2}) ->
            close(P1),
            close(P2)
        end,
        [
            ?tt("bcast()",test_bcast()),
            ?tt("lookup_values()",test_lookup_values()),
            ?tt("get_pid()",test_get_pid()),
            ?tt("get_pid() w bad key",test_badarg_get_pid()),
            ?tt("match()",test_match()),
            ?tt("match() w empty result",test_empty_match()),
            ?tt("match_rnd()",test_match_rnd()),
            ?tt("match_rnd() w empty result",test_empty_match_rnd()),
            ?tt("unlisten()",test_unlisten()),
            ?tt("reg(Key,Val)",test_reg()),
            ?tt("reg(Key)",test_reg1()),
            ?tt("count()",test_count())
        ]
    }.

test_bcast() ->
   ?assertMatch(
      [{_Pid,undefined},{_Pid1,undefined}],
      begin
         Pid = self(),
         ?run(node1,true,btune,listen,[{bkey,Pid}]),
         ?run(node2,true,btune,listen,[{bkey,Pid}]),
         btune:bcast({bkey,Pid},{{bkey,Pid},"Hello!"}),

         ?recvmatch({bkey,"Hello!"}),
         ?recvmatch({bkey,"Hello!"}),
         btune:lookup_values({bkey,Pid})
      end).

test_lookup_values() ->
   ?assertMatch(
      [{_Pid,{vnode1,v2node1}},{_Pid1,{vnode2,v2node2}}],
      begin
         ok=?reg(node1,{p,l,mykey},{vnode1,v2node1}),
         ok=?reg(node2,{p,l,mykey},{vnode2,v2node2}),
         btune:lookup_values(mykey)
      end).

test_get_pid() ->
   ?assertMatch(
      ok,
      begin
         ok=?reg(node1,{n,l,mygetpidkey},dummyvalue1),
         ok=?reg(node2,{n,l,mygetpidkey},dummyvalue1),
         Pid=btune:get_pid(mygetpidkey),
         PidNode1=global:whereis_name(node1),
         %The tuple is is a trick to make eunit show Pid
         %and PidNode1 values if it fails
         ?assertEqual(Pid,PidNode1)
      end).

test_badarg_get_pid() ->
   ?assertError(
      badarg,
      begin
         %Non-existant key
         btune:get_pid(mygetpidkey2)
      end).

test_reg() ->
   ?assertMatch(
      [{regkey,_,{one,two}}],
      begin
         btune:reg(regkey,{one,two}),
         btune:match(regkey)
      end).

test_reg1() ->
   ?assertMatch(
      [{regkey1,_,undefined}],
      begin
         btune:reg(regkey1,undefined),
         btune:match(regkey1)
      end).

test_match() ->
   ?assertMatch(
      [{{{bkey,10},_},_,undefined},
       {{{bkey,30},_},_,undefined},
       {{{bkey,20},_},_,undefined},
       {{{bkey,40},_},_,undefined}],
      begin
         Pid=self(),
         ?run(node1,true,btune,listen,[{{bkey,10},Pid}]),
         ?run(node2,true,btune,listen,[{{bkey,20},Pid}]),
         ?run(node1,true,btune,listen,[{{bkey,30},Pid}]),
         ?run(node2,true,btune,listen,[{{bkey,40},Pid}]),
         btune:match({{bkey,'_'},'_'})
      end).

test_empty_match() ->
   ?assertEqual(
      [],
      begin
         btune:match({some_strange_key,noise})
      end).

test_match_rnd() ->
   ?assert(
      begin
         Pid=self(),
         ?run(node1,true,btune,listen,[{{bkey1,10},Pid}]),
         ?run(node2,true,btune,listen,[{{bkey1,20},Pid}]),
         ?run(node1,true,btune,listen,[{{bkey1,30},Pid}]),
         ?run(node2,true,btune,listen,[{{bkey1,40},Pid}]),
         {{{bkey1,Num1},_},_,undefined} = btune:match_rnd({{bkey1,'_'},'_'}),
         {{{bkey1,Num2},_},_,undefined} = btune:match_rnd({{bkey1,'_'},'_'}),
         {{{bkey1,Num3},_},_,undefined} = btune:match_rnd({{bkey1,'_'},'_'}),
         {{{bkey1,Num4},_},_,undefined} = btune:match_rnd({{bkey1,'_'},'_'}),
         Num1 =/= Num2 orelse Num2 =/= Num3 orelse Num3 =/= Num4
      end).

test_empty_match_rnd() ->
   ?assertError(
      no_match,
      begin
         btune:match_rnd({some_strange_key,noise})
      end).
test_unlisten() ->
   [?assertMatch(
      [{{{newkey,10},Pid},_,undefined}],
      begin
         Pid=self(),
         ?run(node1,true,btune,listen,  [{{newkey,10},Pid}]),
         btune:match({{newkey,'_'},'_'})
      end),
   ?assertMatch(
      [],
      begin
         Pid=self(),
         ?run(node1,true,btune,unlisten,[{{newkey,10},Pid}]),
         btune:match({{newkey,'_'},'_'})
        end)].

test_count() ->
   ?assertMatch(
      [ 1, 1, 0, 2 ],
      begin
         ?run(node1,true,btune,listen,[{nolis,10}]),
         ?run(node2,true,btune,listen,[{nolis,20}]),
         [count({nolis,10}), count({nolis,20}),
          count({nolis,30}), count({nolis,'_'}) ]
      end).
-endif.
