fifo_server
=====

An OTP application

Build
-----

    $ ./rebar3 compile

Tests
-----
    
    $ ./rebar3 eunit

Release
-------

    $ ./rebar3 release

Run
---

    $ ./rebar3 release && _build/default/rel/fifo_server/bin/fifo_server start

Interactive run
---------------

    $ ./rebar3 release && _build/default/rel/fifo_server/bin/fifo_server console
