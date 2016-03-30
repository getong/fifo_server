%%%-------------------------------------------------------------------
%% @doc fifo_server public API
%% @end
%%%-------------------------------------------------------------------

-module(fifo_server_app).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
%% EUnit tests
-export([
    fifo_1_test/0,
    fifo_2_test/0
]).
-endif.

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    fifo_server_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%
% test functions
%
-ifdef(TEST).
fifo_1_test() ->
   ?assertMatch({ok,_}, application:ensure_all_started(fifo_server)),
   {ok, Socket} = gen_tcp:connect("localhost", 2222, [binary, {packet, 0}]),
   check_resp(Socket, <<"SUPER FIFO SERVER!\nCOMMANDS:\n1.in something\n2.out\n3.quit\n">>),
   gen_tcp:send(Socket, <<"test\r\n">>),
   check_resp(Socket, <<"ERROR:unknown command. Try 'in','out' or quit\n">>),
   gen_tcp:send(Socket, <<"out\r\n">>),
   check_resp(Socket, <<"Error:The queue is empty\n">>),
   gen_tcp:send(Socket, <<"in lalalal1\r\n">>),
   check_resp(Socket, <<"OK\n">>),
   gen_tcp:send(Socket, <<"in lalalal2\r\n">>),
   check_resp(Socket, <<"OK\n">>),
   gen_tcp:send(Socket, <<"in lalalal3\r\n">>),
   check_resp(Socket, <<"OK\n">>),
   gen_tcp:send(Socket, <<"out\r\n">>),
   check_resp(Socket, <<"lalalal1\n">>),
   gen_tcp:send(Socket, <<"out\r\n">>),
   check_resp(Socket, <<"lalalal2\n">>),
   gen_tcp:send(Socket, <<"out\r\n">>),
   check_resp(Socket, <<"lalalal3\n">>),
   gen_tcp:send(Socket, <<"out\r\n">>),
   check_resp(Socket, <<"Error:The queue is empty\n">>),
   ok. 

%
% test multiple connections
%
fifo_2_test() ->
   ?assertMatch({ok,_}, application:ensure_all_started(fifo_server)),
   {ok, Socket1} = gen_tcp:connect("localhost", 2222, [binary, {packet, 0}]),
   check_resp(Socket1, <<"SUPER FIFO SERVER!\nCOMMANDS:\n1.in something\n2.out\n3.quit\n">>),
   {ok, Socket2} = gen_tcp:connect("localhost", 2222, [binary, {packet, 0}]),
   check_resp(Socket2, <<"SUPER FIFO SERVER!\nCOMMANDS:\n1.in something\n2.out\n3.quit\n">>),
   {ok, Socket3} = gen_tcp:connect("localhost", 2222, [binary, {packet, 0}]),
   check_resp(Socket3, <<"SUPER FIFO SERVER!\nCOMMANDS:\n1.in something\n2.out\n3.quit\n">>),


   gen_tcp:send(Socket1, <<"test\r\n">>),
   check_resp(Socket1, <<"ERROR:unknown command. Try 'in','out' or quit\n">>),
   gen_tcp:send(Socket2, <<"test\r\n">>),
   check_resp(Socket2, <<"ERROR:unknown command. Try 'in','out' or quit\n">>),
   gen_tcp:send(Socket3, <<"test\r\n">>),
   check_resp(Socket3, <<"ERROR:unknown command. Try 'in','out' or quit\n">>),


   gen_tcp:send(Socket1, <<"out\r\n">>),
   check_resp(Socket1, <<"Error:The queue is empty\n">>),
   gen_tcp:send(Socket2, <<"out\r\n">>),
   check_resp(Socket2, <<"Error:The queue is empty\n">>),
   gen_tcp:send(Socket3, <<"out\r\n">>),
   check_resp(Socket3, <<"Error:The queue is empty\n">>),


   gen_tcp:send(Socket1, <<"in lalalal1\r\n">>),
   check_resp(Socket1, <<"OK\n">>),
   gen_tcp:send(Socket2, <<"in lalalal1\r\n">>),
   check_resp(Socket2, <<"OK\n">>),
   gen_tcp:send(Socket3, <<"in lalalal1\r\n">>),
   check_resp(Socket3, <<"OK\n">>),


   gen_tcp:send(Socket1, <<"in lalalal2\r\n">>),
   check_resp(Socket1, <<"OK\n">>),
   gen_tcp:send(Socket2, <<"in lalalal2\r\n">>),
   check_resp(Socket2, <<"OK\n">>),
   gen_tcp:send(Socket3, <<"in lalalal2\r\n">>),
   check_resp(Socket3, <<"OK\n">>),


   gen_tcp:send(Socket1, <<"in lalalal3\r\n">>),
   check_resp(Socket1, <<"OK\n">>),
   gen_tcp:send(Socket2, <<"in lalalal3\r\n">>),
   check_resp(Socket2, <<"OK\n">>),
   gen_tcp:send(Socket3, <<"in lalalal3\r\n">>),
   check_resp(Socket3, <<"OK\n">>),


   gen_tcp:send(Socket1, <<"out\r\n">>),
   check_resp(Socket1, <<"lalalal1\n">>),
   gen_tcp:send(Socket2, <<"out\r\n">>),
   check_resp(Socket2, <<"lalalal1\n">>),
   gen_tcp:send(Socket3, <<"out\r\n">>),
   check_resp(Socket3, <<"lalalal1\n">>),


   gen_tcp:send(Socket1, <<"out\r\n">>),
   check_resp(Socket1, <<"lalalal2\n">>),
   gen_tcp:send(Socket2, <<"out\r\n">>),
   check_resp(Socket2, <<"lalalal2\n">>),
   gen_tcp:send(Socket3, <<"out\r\n">>),
   check_resp(Socket3, <<"lalalal2\n">>),


   gen_tcp:send(Socket1, <<"out\r\n">>),
   check_resp(Socket1, <<"lalalal3\n">>),
   gen_tcp:send(Socket2, <<"out\r\n">>),
   check_resp(Socket2, <<"lalalal3\n">>),
   gen_tcp:send(Socket3, <<"out\r\n">>),
   check_resp(Socket3, <<"lalalal3\n">>),


   gen_tcp:send(Socket1, <<"out\r\n">>),
   check_resp(Socket1, <<"Error:The queue is empty\n">>),
   gen_tcp:send(Socket2, <<"out\r\n">>),
   check_resp(Socket2, <<"Error:The queue is empty\n">>),
   gen_tcp:send(Socket3, <<"out\r\n">>),
   check_resp(Socket3, <<"Error:The queue is empty\n">>),
   ok. 

check_resp(Socket, Expect) ->
   receive
        {tcp, Socket, Data} -> ?assertEqual(Expect, Data)            
   end.

-endif.


