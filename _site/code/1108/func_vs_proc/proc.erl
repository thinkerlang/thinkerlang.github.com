-module(proc).

-behaviour(testable).

-export([start/0, stop/1, test/2]).

loop() ->
		receive 
				{test, From, N} ->
						From ! {result, N},
						loop();
				stop ->
						ok
		end.

start() ->
		spawn(fun loop/0).

stop(Pid) ->
		Pid ! stop.

test(Pid, N) ->
		Pid ! {test, self(), N},
		receive
				{result, M} ->
						M
		end.

