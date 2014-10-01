-module(btune).

%% Exports
-export([bcast/2,listen/1,unlisten/1,
         match/2,match/1,
         plisteners/1,plisteners/2,
         listeners/1,listeners/2]).

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
   gproc:bcast([node()|nodes()],{p,l,Key},{Key,Msg}).

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
%      [{mykey,param1,param2},{mykey,param3,param4}]
%      '''
%
%      A default timeout of 3 seconds is presumed.
%      Use {@link match/2} if you want a different one.
% @end
-spec match(Pattern::tuple()) -> [key()].
match(Pattern) ->
   match(Pattern,?TIMEOUT).

% @doc Same as {@link pmatch/1} but with a specific timeout in milliseconds.
% @end
-spec match(Pattern::tuple(),Timeout::integer()) -> [key()].
match(Pattern,Timeout) ->
   MS=[{{{p,l,Pattern},'_','_'}, %Pid and Value are not used
          [],                    %Guard is empty
        ['$$']}],                %Return list of expected values

   R=mcall(gproc,select,[{l,p},MS],Timeout),
   getkeys(R).
    


% @private
% @doc Partial match a tuple against registered keys in all nodes.
%
%      This function is meant to provide a simple match scheme
%      to simplify searching for keys that match the elements
%      of the given tuple:
%
%      ```
%      n1@host> btune:listen({mykey,param1,param2}).
%      true
%      ...
%      n2@host> btune:listen({mykey,param3,param4}).
%      true
%      ...
%      n1@host> btune:plisteners({mykey}).
%      [{mykey,param1,param2},{mykey,param3,param4}]
%      '''
%      A default timeout of 3 seconds is presumed. Use
%      {@link plisteners/2} for a different timeout.
%
% @end
-spec plisteners(PartialKey::tuple()) -> [key()].
plisteners(PartialKey) when is_tuple(PartialKey) ->
   plisteners(PartialKey,?TIMEOUT).

% @private
% @doc   Same as {@link plisteners/1} with a specified timeout in milliseconds.
% @end
-spec plisteners(PartialKey::tuple(),Timeout::integer()) -> [key()].
plisteners(PartialKey,Timeout) when is_tuple(PartialKey) ->
   G = [ { '==', {element,N,'$1'},element(N,PartialKey)} || N <- lists:seq(1,tuple_size(PartialKey)) ],
   MS=[{{{p,l,'$1'},'_','_'}, %Pid and Value are not used
          G,                  %Guard to match elements in PartialKey
        ['$1']}],             %Return list of keys

   mcall(gproc,select,[{l,p},MS],Timeout).

% @private
% @doc Returns `[{pid(),value()]' list for all listeners of `Key'.
% @equiv listeners(Key,DEFAULT_TIMEOUT)
% @end
-spec listeners(Key::key()) -> [{pid(), term()}].
listeners(Key) ->
   listeners(Key,?TIMEOUT).

% @private
% @doc   Same as {@link listeners/1} but with the a specified timeout.
% @end
-spec listeners(Key::key(),Timeout::integer()) -> [{pid(), term()}].
listeners(Key,Timeout) ->
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
getkeys([]) ->
   [];

getkeys([[{p,l,Key},_,_]|R]) ->
   lists:append([Key],getkeys(R)).


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
            ?tt("listeners()",test_listeners()),
            ?tt("bcast()",test_bcast()),
            ?tt("match()",test_match()),
            ?tt("unlisten()",test_unlisten())
        ]
    }.

test_bcast() ->
   ?assertMatch(
      [{_Pid,undefined},{_Pid1,undefined}],
      begin
         Pid = self(),
         ?run(node1,true,btune,listen,[{bkey,Pid}]),
         ?run(node2,true,btune,listen,[{bkey,Pid}]),
         btune:bcast({bkey,Pid},"Hello!"),

         ?recvmatch({bkey,"Hello!"}),
         ?recvmatch({bkey,"Hello!"}),
         btune:listeners({bkey,Pid})
      end).

test_listeners() ->
   ?assertMatch(
      [{_Pid,{vnode1,v2node1}},{_Pid1,{vnode2,v2node2}}],
      begin
         ok=?reg(node1,{p,l,mykey},{vnode1,v2node1}),
         ok=?reg(node2,{p,l,mykey},{vnode2,v2node2}),
         btune:listeners(mykey)
      end).

test_match() ->
   ?assertMatch(
      [{{bkey,10},_},{{bkey,30},_},{{bkey,20},_},{{bkey,40},_}],
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
      [{{newkey,10},Pid}],
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
-endif.
