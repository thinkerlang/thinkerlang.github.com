-module(arr).

-compile([export_all]).

data1(N) ->
    %% size implies fixed-size array 
    %% but lets be explicit
    array:new([{size, N}, {default, 0}, {fixed, true}]).

data2(N) ->
    %% extensible array
    array:new([{size, N}, {default, -1}, {fixed, false}]).

data3(N) ->
    erlang:make_tuple(N, 0).

data4(_) ->
    gb_trees:empty().

array_set(Array, I, Value) ->
    %% array indexing starts at 0
    array:set(I - 1, Value, Array).

tuple_set(Tuple, I, Value) ->
    %% tuple indexing starts at 1
    setelement(I, Tuple, Value).

tree_set(Tree, I, Value) ->
    gb_trees:enter(I, Value, Tree).

array_get(Array, I) ->
    array:get(I - 1, Array).

tuple_get(Tuple, I) ->
    element(I, Tuple).

tree_get(Tree, I) ->
    gb_trees:get(I, Tree).

get(_, _, 0) ->
    ok;

get(Fun, Data, N) ->
    Fun(Data, N),
    get(Fun, Data, N - 1).

set(_, Data, 0) ->
    Data;

set(Fun, Data, N) ->
    Data1 = Fun(Data, N, N),
    set(Fun, Data1, N - 1).

test() ->
    test(10000).

test(N) ->
    %% fixed-size array
    {S1, D1} = timer:tc(arr, set, [{arr, array_set}, data1(N), N]),
    {G1, _} = timer:tc(arr, get, [{arr, array_get}, D1, N]),
    %% extensible array
    {S2, D2} = timer:tc(arr, set, [{arr, array_set}, data2(N), N]),
    {G2, _} = timer:tc(arr, get, [{arr, array_get}, D2, N]),
    %% tuple
    {S3, D3} = timer:tc(arr, set, [{arr, tuple_set}, data3(N), N]),
    {G3, _} = timer:tc(arr, get, [{arr, tuple_get}, D3, N]),
    %% gb_trees
    {S4, D4} = timer:tc(arr, set, [{arr, tree_set}, data4(N), N]),
    {G4, _} = timer:tc(arr, get, [{arr, tree_get}, D4, N]),
    %% results
    io:format("Fixed-size array: get: ~8wµs, set: ~8wµs~n", [G1 , S1]),
    io:format("Extensible array: get: ~8wµs, set: ~8wµs~n", [G2 , S2]),
    io:format("Tuple:            get: ~8wµs, set: ~8wµs~n", [G3 , S3]),
    io:format("Tree:             get: ~8wµs, set: ~8wµs~n", [G4 , S4]),
    ok.

