conTest
=====

Simple server to check connectivity of other network devices.
Currently uses linux ping for connection test.

Build
-----

    $ rebar3 release
    $ ./_build/default/rel/conTest/bin/conTest
    Browser -> http://localhost:8080
    
Requirements
-------------
Browser has to be websocket compatible

Erlang code is tested on OTP 18, but should work for
earlier releases too.
