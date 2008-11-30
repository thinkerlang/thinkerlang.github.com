-module(testable).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
		[{start, 0},
		 {stop, 1},
		 {test, 2}].
