-module(sockserv_serv).
-behaviour(gen_server).
 
-record(state, {lsocket, socket_o}). 
 
-export([start_link/1]).
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2,
    code_change/3, 
    terminate/2
]).


start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).
    
init([Socket]) ->
    lager:debug("sockserv_serv Acceptor init ~p", [self()]),
    gen_server:cast(self(), accept),
    Opts = fifo_s_utils:socket_options(),
    {ok, #state{lsocket=Socket, socket_o=Opts}}.
    
handle_call(_E, _From, State) -> {noreply, State}.

handle_cast(accept, #state{lsocket=ListenSocket, socket_o=Opts}=State) ->
    lager:debug("sockserv_serv Acceptor accept cast ~p", [self()]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    gen_server:cast(self(), accept),
    {ok, Pid} = fifo_server_sup:start_worker(Opts),
    gen_tcp:controlling_process(Socket, Pid),
    worker_fq:set_socket(Pid, Socket, Opts),
    {noreply, State};
handle_cast(_E, State) -> {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

