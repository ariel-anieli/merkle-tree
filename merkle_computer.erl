-module(merkle_computer).

-export([compute_root/1]).

% Public API

compute_root(File) ->
    Start = erlang:system_time(millisecond),
    {ok, Data} = file:read_file(File),
    Leaves = binary:split(Data, [<<"\n">>], [global]),
    Root   = hash(Leaves),
    Duration = erlang:system_time(millisecond) - Start,

    #{file=> File, root=>Root, duration_ms=>Duration}.

% Utilities

pipe(Arg, Funcs) -> 
    lists:foldl(fun(Func, Arg) -> Func(Arg) end, Arg, Funcs).

maybe_into_bin(A) when is_binary(A) ->
    A;
maybe_into_bin(A) ->
    list_to_binary(A).

hash([A,B]) ->
    BinA = maybe_into_bin(A),
    BinB = maybe_into_bin(B),
    pipe(
      <<BinA/binary, BinB/binary>>,
      [
       fun(Str) -> crypto:hash(sha256, Str) end,
       fun binary:encode_hex/1,
       fun binary:bin_to_list/1,
       fun string:lowercase/1
      ]
     );

hash([A,B,C]) ->
    hash([hash([A,B]), C]);

hash(Leaves) when length(Leaves)>3 ->
    Middle = length(Leaves) div 2,
    {Head, Tail} = lists:split(Middle, Leaves),
    hash([hash(Head), hash(Tail)]).
