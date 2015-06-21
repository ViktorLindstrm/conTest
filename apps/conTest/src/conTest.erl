-module(conTest).
-export([ping/1]).
ping(IP) -> 
  conTest_ping:ping(IP).
