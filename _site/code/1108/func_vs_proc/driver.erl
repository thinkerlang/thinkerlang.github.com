-module(driver, [Imp]).

-export([test/0]).

test() ->
		Handle = Imp:start(),
		test(1000000, Handle, none).

test(0, Handle, Result) ->
		Imp:stop(Handle),
		Result;

test(N, Handle, _) ->
		test(N - 1, Handle, Imp:test(Handle, N)).


