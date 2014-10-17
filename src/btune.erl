-module(btune).

%% Exports
-export([bcast/2,listen/1,unlisten/1,
         match/2,match/1,
         no_listener/1,
         lookup_values/1,lookup_values/2]).

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

% @doc Send `Msg' to all connected nodes.
% @end
-spec bcast(Key::key(),Msg::any()) -> any().
bcast(Key,Msg) ->
   gproc:bcast([node()|nodes()],{p,l,Key},Msg).

% @doc Adds the calling process to the list of listeners of  `Key'.
% @end
-spec listen(Key::key()) -> true.
listen(Key) ->
   gproc:reg({p,l,Key}).

% @doc Removes the calling process from the list of listeners of  `Key'.
% @end
-spec unlisten(Key::key()) -> true.
unlisten(Key) ->
   gproc:unreg({p,l,Key}).

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
%      [{{mykey,param1,param2},<0.43,0>,undefined},{{mykey,param3,param4},<3332.43.0>,undefined}]
%      '''
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
   getresult(R).

% @doc Returns true if there are no listeners for keys that match the given `Pattern',
%      otherwise returns the number of matches for `Pattern'.
%
%      The `Pattern' is the same as in {@link match/1}.
% @end
-spec no_listener(Pattern::term()) -> true|integer().
no_listener(Pattern) ->
   R=match(Pattern),
   case R of
      []   -> true;
      _Any -> length(R)
   end.

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
   mcall(gproc,lookup_values,[{p,l,Key}],Timeout).



%% Internal utility functions
%% ==========================
mcall(M,F,A,Timeout) ->
   {R,_} = rpc:multicall(M,F,A,Timeout),
   %Force lists:append to return a list
   %  seems to be a bug in lists:append not to return a list
   %  if there are no lists inside
   R0=lists:append(R),
   case is_list(R0) of
      true  -> R0;
      false -> [R0]
   end.

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
            ?tt("match()",test_match()),
            ?tt("unlisten()",test_unlisten()),
            ?tt("no_listener()",test_nolistener())
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

test_nolistener() ->
   ?assertMatch(
      [ 1, 1, true, 2 ],
      begin
         ?run(node1,true,btune,listen,[{nolis,10}]),
         ?run(node2,true,btune,listen,[{nolis,20}]),
         [no_listener({nolis,10}), no_listener({nolis,20}),
          no_listener({nolis,30}), no_listener({nolis,'_'}) ]
      end).
-endif.
