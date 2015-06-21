%%%-------------------------------------------------------------------
%% @doc conTest top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(conTest_supsup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Mod, Type, Args), {Mod, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
start_child(IP) ->
  supervisor:start_child(?MODULE,[IP]).
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    C1 = ?CHILD(conTest_ping, worker, []),
    {ok, { {simple_one_for_one, 0, 1}, [C1]} }.

%%====================================================================
%% Internal functions
%%====================================================================
