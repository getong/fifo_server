-module(fifo_s_utils).

-export([
    socket_options/0
]).

socket_options() ->
    %[binary, {packet, 0}, {active, false}, {keepalive, true}, {backlog, 100}].
    [binary, {packet, 0}, {active, false}].
