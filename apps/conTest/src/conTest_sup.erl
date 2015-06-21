%%%-------------------------------------------------------------------
%% @doc conTest top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(conTest_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    C1 = ?CHILD(conTest_mngr, worker, []),
    C2 = ?CHILD(conTest_timer, worker, []),
    S1 = ?CHILD(conTest_supsup, supervisor, []),
    {ok, { {one_for_all, 0, 1}, [C1,C2,S1]} }.

%%====================================================================
%% Internal functions
%%====================================================================
