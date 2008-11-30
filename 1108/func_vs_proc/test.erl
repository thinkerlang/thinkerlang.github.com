-module(test).

-export([test/0]).

test() ->
		Func = driver:new(func),
		Proc = driver:new(proc),
		{T1, Val1} = timer:tc(Func, test, []),
		{T2, Val2} = timer:tc(Proc, test, []),
		{{func, Val1, T1 / 1000}, {proc, Val2, T2 / 1000}}.

		
