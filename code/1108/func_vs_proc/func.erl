-module(func).

-behaviour(testable).

-export([start/0, stop/1, test/2]).

start() ->
		none.

stop(_Handle) ->
		ok.

test(_Handle, N) ->
		N.

