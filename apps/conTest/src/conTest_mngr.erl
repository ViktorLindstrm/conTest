-module(conTest_mngr).

-behaviour(gen_server).

%% API functions
-export([start_link/0,
         set_timer/1,
         start_timer/0,
         stop_timer/0,
         delete_node/1,
         solicit_node/1,
         solicit_all/0,
         start_node/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {nodes}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Nodes = ets:new(nodes,[set]),
    {ok, #state{nodes=Nodes}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop_timer, _From, State) ->
  Reply = gen_server:call(conTest_timer,stop),
  {reply, Reply, State};
handle_call(start_timer, _From, State) ->
  Reply = gen_server:call(conTest_timer,start),
  {reply, Reply, State};
handle_call({set_timer,Time}, _From, State) ->
  Reply = gen_server:call(conTest_timer,{set,Time*1000}),
  {reply, Reply, State};

handle_call({add_node,IP}, _From, State = #state{nodes=Nodes}) ->
  SupReply = conTest_supsup:start_child(IP),
  {ok,Pid} = SupReply,
  ets:insert(Nodes,{IP,Pid}),
  Reply = ok,
  {reply, Reply, State};

handle_call({delete_node,IP}, _From, #state{nodes=Nodes} = State) ->
  Reply = case ets:lookup(Nodes,IP) of
            [{IP,Pid}] ->
              ets:delete(Nodes,IP),
              supervisor:terminate_child(conTest_supsup,Pid);
            [] -> {error,no_such_node}
          end,
  {reply, Reply, State};

handle_call({solicit_node,IP}, _From, #state{nodes = Nodes} = State) ->
  Reply = case ets:lookup(Nodes,IP) of
            [{IP,Pid}] ->
              gen_server:call(Pid,{ping});
            [] ->
              {error,no_such_node}
          end,
  {reply,Reply,State};

handle_call(solicit_all,_From, #state{nodes = Nodes} = State) ->
  io:format("Soliciting~n"),
  Workers = ets:match(Nodes,'$1'),
  io:format("Got nodes~n"),
  RList = lists:map(fun([{_,Pid}]) -> 
                        {_, X } = gen_server:call(Pid,{ping}),
                        io:format("Asking ~p~n",[Pid]),
                        X
                    end,
                    Workers),
  Reply = {ok,RList},
  {reply,Reply,State}.
                   
  
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(timer, State) ->
  spawn(fun() -> timer_function() end),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_node(IP) ->
  gen_server:call(?MODULE,{add_node,IP}).
delete_node(IP) ->
  gen_server:call(?MODULE,{delete_node,IP}).
solicit_node(IP) ->
  gen_server:call(?MODULE,{solicit_node,IP}).
solicit_all() ->
  gen_server:call(?MODULE,solicit_all).
start_timer() ->
  gen_server:call(?MODULE,start_timer).
stop_timer() ->
  gen_server:call(?MODULE,stop_timer).
set_timer(Time) ->
  gen_server:call(?MODULE,{set_timer,Time}).
timer_function() ->
  solicit_all().
