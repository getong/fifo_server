%%%-------------------------------------------------------------------
%% @doc fifo_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(fifo_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_worker/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER,          ?MODULE).
-define(MAX_RESTART,        1800).
-define(MAX_TIME,           3600).
-define(DEF_PORT,           2222).
-define(ACCEPTORS_NUMBER,      5).
-define(ACCEPTOR_SUPERVISOR, acceptor_supervisor).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%init([]) ->
%    {ok, { {one_for_all, 0, 1}, []} }.
init([]) ->
    AcceptorSupervisor = {
        ?ACCEPTOR_SUPERVISOR,
        {supervisor, start_link,[{local, acceptor_supervisor}, ?MODULE, [?ACCEPTOR_SUPERVISOR]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        [] 
    },
    WorkersSupervisor = {
        workers_sup,
        {supervisor,start_link,[{local, workers_sup}, ?MODULE, [worker_fq]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME},
        [AcceptorSupervisor, WorkersSupervisor]
        %[AcceptorSupervisor]
        %[]
    }};

%for acceptor_supervisor
init([?ACCEPTOR_SUPERVISOR]) ->
    lager:debug("ACCEPTOR_SUPERVISOR init 1", []),
    {ok, ListenSocket} = gen_tcp:listen(?DEF_PORT, fifo_s_utils:socket_options()++[{reuseaddr, true}]),
    lager:debug("ACCEPTOR_SUPERVISOR init 2", []),
    spawn_link(fun empty_listeners/0),
    lager:debug("ACCEPTOR_SUPERVISOR init 3", []),
    {ok,
        {{simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [{socket,
                {sockserv_serv, start_link, [ListenSocket]}, 
                temporary, 2000, worker, [sockserv_serv]}
            ]
    }};
% for workers_sup
init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {Module,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.    

%%====================================================================
%% Internal functions
%%====================================================================


start_worker(Opts) -> supervisor:start_child(workers_sup, [Opts]).

empty_listeners()  -> lists:foreach(fun(_)-> supervisor:start_child(?ACCEPTOR_SUPERVISOR, []) end, lists:seq(1, ?ACCEPTORS_NUMBER)).

