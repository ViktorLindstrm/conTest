-module(conTest_ping).

-behaviour(gen_server).

%% API functions
-export([start_link/1,
         ping/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {ip=""}).

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
start_link(IP) ->
    gen_server:start_link(?MODULE, [IP],[]).

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
init([IP]) ->
    {ok, #state{ip=IP}}.

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
handle_call({ping}, _From, #state{ip = IP} = State) ->
    Result = os:cmd("ping -c 1 -W 1 "++IP ),
    TResult = parse_ip(Result),
    io:format("R : ~p~n",[TResult]),
    Reply  = case TResult of 
               {error, E} -> {error,E};
               _ -> {ok,TResult}
             end,
    %Reply = {ok,TResult},
    {reply, Reply, State}.

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
handle_cast(_Msg, State) ->
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
ping(IP) -> 
  gen_server:call(?MODULE,{ping,IP}).
parse_ip(Raw) ->
    Rows = string:tokens(Raw,"\n"),
    io:format("Rows : ~p~n",[Rows]),
    case length(Rows) > 1 of 
      true -> 
        [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10] = case length(Rows) > 3 of 
                                             true -> 
                                               string:tokens(lists:nth(4,Rows)," ");
                                             false ->
                                               string:tokens(lists:nth(3,Rows)," ")
                                           end,
        {X2++"_"++lists:sublist(X3,1,length(X3)-1),X1,lists:sublist(X5,1,length(X5)-1),X4,X7++"_"++lists:sublist(X8,1,length(X8)-1),X6,X9,X10};
      false -> 
        {error,unknown_host}
    end.

