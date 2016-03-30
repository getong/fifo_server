-module(worker_fq).
-behaviour(gen_server).
 
-record(state, {socket, opts, fifo}). 
 
-export([start_link/1]).
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2,
    code_change/3, 
    terminate/2,
    set_socket/3
]).


start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).
    
init([Opts]) ->
    lager:debug("worker_fq init ~p", [self()]),
    {ok, Q} = mkh_queue_sup:new_queue(),
    {ok, #state{opts=Opts, fifo=Q}}.
    
handle_call(_E, _From, State) -> 
    lager:debug("WFQ HCALL: ~p", [{_E, _From, State}]),
    {noreply, State}.

handle_cast({greeting, Socket}, State) ->
    inet:setopts(Socket, [{active, once}, {packet, 0}, {keepalive, true}, binary]),
    gen_tcp:send(Socket, <<"SUPER FIFO SERVER!\nCOMMANDS:\n1.in something\n2.out\n3.quit\n">>),   
    {noreply, State#state{socket=Socket}};
handle_cast(stop, State) -> 
    {stop, normal, State};
handle_cast(_E, State) -> 
    lager:debug("WFQ HCAST: ~p", [{_E, State}]),
    {noreply, State}.

handle_info(Info, #state{socket=Socket, fifo=Q}=State) ->
    lager:debug("WFQ HINFO: ~p", [{Info, State}]),
    inet:setopts(Socket, [{active, once}]),
    dialog(Socket, Info, Q),
    {noreply, State}.

terminate(_Reason, #state{socket=Socket, fifo=Q} = State) ->
    lager:debug("WFQ TERM: ~p", [{_Reason, State}]),
    gen_tcp:close(Socket),
    mkh_queue_sup:delete_queue(Q),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec set_socket(pid(), port(), _) -> ok.
set_socket(Pid, Socket, _) ->
    gen_server:cast(Pid, {greeting, Socket}),
    lager:debug("WFQ SS!!!",[]),
    ok.

-spec dialog(port(), {tcp_closed,_}|{tcp,_,binary()}, pid()) -> any().
dialog(_, {tcp_closed,_}, _)          -> gen_server:cast(self(), stop);
dialog(_, {tcp,_,<<"quit\r\n">>}, _)  -> gen_server:cast(self(), stop);
dialog(Socket, {tcp,_,<<"in ", Item/binary>>}, Q) ->
    L = size(Item) - 2,
    <<I:L/binary, "\r\n">> = Item,
    gen_server:cast(Q, {in, I}),
    gen_tcp:send(Socket, <<"OK\n">>);
dialog(Socket, {tcp,_,<<"out\r\n">>}, Q) ->
    case gen_server:call(Q, out) of
        {error, empty} -> gen_tcp:send(Socket, <<"Error:The queue is empty\n">>);
        {ok, Ret}      -> gen_tcp:send(Socket, <<Ret/binary, "\n">>)
    end;
dialog(Socket, _, _) -> 
    gen_tcp:send(Socket, <<"ERROR:unknown command. Try 'in','out' or quit\n">>).

