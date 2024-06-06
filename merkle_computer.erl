-module(merkle_computer).
-behaviour(gen_server).

-export([
	 compute_root/1,
	 init/1,
	 handle_call/3
	]).

% Public API

compute_root(File) ->
    Start = erlang:system_time(millisecond),
    {ok, Worker} = start(),
    {ok, Data}   = file:read_file(File),
    Leaves = binary:split(Data, [<<"\n">>], [global]),
    Root   = compute_root(Worker, Leaves),
    Duration = erlang:system_time(millisecond) - Start,

    #{file=> File, root=>Root, duration_ms=>Duration}.

% Private API

start() ->
    gen_server:start(?MODULE, [], []).

compute_root(Worker, Leaves) ->
    gen_server:call(Worker, {compute_root, Leaves}).

% Callbacks

init([]) ->
    {ok, []}.

handle_call({compute_root,[Head,Tail]}, From, State) ->
    {stop, normal, hash(Head, Tail), State};

handle_call({compute_root,[Head,Mid,Tail]}, From, State) ->
    {stop, normal, hash(hash(Head, Mid), Tail), State};

handle_call({compute_root,Leaves}, From, State) when length(Leaves)>3 ->
    Middle = length(Leaves) div 2,
    {Head, Tail} = lists:split(Middle, Leaves),

    {ok, HeadWorker} = start(),
    {ok, TailWorker} = start(),

    Hash = hash(compute_root(HeadWorker, Head), compute_root(TailWorker, Tail)),

    {stop, normal, Hash, State}.

% Utilities

pipe(Arg, Funcs) -> 
    lists:foldl(fun(Func, Arg) -> Func(Arg) end, Arg, Funcs).

maybe_into_bin(A) when is_binary(A) ->
    A;
maybe_into_bin(A) ->
    list_to_binary(A).

hash(A,B) ->
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
     ).
