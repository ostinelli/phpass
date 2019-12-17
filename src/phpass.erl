%% ==========================================================================================================
%% PHPASS - A simple implementation of PHPass’ Portable Hash.
%%
%% The MIT License (MIT)
%%
%% Copyright (c) 2019 Roberto Ostinelli <roberto@ostinelli.net>.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% ==========================================================================================================
-module(phpass).

%% API
-export([start/0, stop/0]).
-export([hash/1]).
-export([check/2]).

%% macro
-define(ITERATIONS, 8).
-define(ITOA64, <<"./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz">>).

%% ===================================================================
%% API
%% ===================================================================
-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(phpass),
    ok.

-spec stop() -> ok | {error, Reason :: any()}.
stop() ->
    application:stop(phpass).

-spec hash(Password :: binary()) -> Hash :: binary().
hash(Password) when is_binary(Password) ->
    crypt(Password, generate_salt()).

-spec check(Password :: binary(), Hash :: binary()) -> boolean().
check(Password, Hash) when is_binary(Password) andalso is_binary(Hash) ->
    case size(Hash) < 6 of
        true -> false;
        _ -> crypt(Password, Hash) =:= Hash
    end.

%% ===================================================================
%% Internal
%% ===================================================================
-spec generate_salt() -> Salt :: binary().
generate_salt() ->
    RandBytes = crypto:strong_rand_bytes(6),
    S1 = binary:at(?ITOA64, ?ITERATIONS + 5),
    S2 = encode64(RandBytes, 6),
    <<"$P$", S1, S2/binary>>.

-spec crypt(Password :: binary(), Setting :: binary()) -> Hash :: binary().
crypt(Password, Setting) ->
    Out = case binary:part(Setting, 0, 2) of
        <<"*0">> -> <<"*1">>;
        _ -> <<"*0">>
    end,
    Iter = index_of(binary:at(Setting, 3), ?ITOA64),
    crypt(Password, Setting, Iter, Out).

-spec crypt(
    Password :: binary(),
    Setting :: binary(),
    Iter :: non_neg_integer(),
    Out :: binary()
) -> Hash :: binary().
crypt(Password, Setting, Iter, Out) when Iter >= 8 andalso Iter =< 30 ->
    Count = 1 bsl Iter,
    Salt = binary:part(Setting, 4, 8),
    case size(Salt) =:= 8 of
        false ->
            Out;

        true ->
            %% loop hash
            Hash0 = crypto:hash(md5, <<Salt/binary, Password/binary>>),
            Hash = lists:foldl(fun(_C, Acc) ->
                crypto:hash(md5, <<Acc/binary, Password/binary>>)
            end, Hash0, lists:reverse(lists:seq(1, Count))),
            %% build hash
            S1 = binary:part(Setting, 0, 12),
            S2 = encode64(Hash, 16),
            <<S1/binary, S2/binary>>
    end;
crypt(_, _, _, Out) -> Out.

-spec index_of(Item :: byte(), Subject :: binary()) -> non_neg_integer() | not_found.
index_of(Item, Subject) -> index_of(Item, Subject, 1).
index_of(_, <<>>, _) -> not_found;
index_of(Item, <<Item, _/binary>>, Index) -> Index - 1;
index_of(Item, <<_, Tl/binary>>, Index) -> index_of(Item, Tl, Index + 1).

-spec encode64(Input :: binary(), Count :: non_neg_integer()) -> Out :: binary().
encode64(Input, Count) ->
    encode64(Input, Count, 0, <<"">>).

-spec encode64(
    Input :: binary(),
    Count :: non_neg_integer(),
    Cursor :: non_neg_integer(),
    Out :: binary()
) -> Encoded :: binary().
encode64(Input, Count, Cursor, Out) when Cursor < Count ->
    Value = binary:at(Input, Cursor),
    Cursor1 = Cursor + 1,
    B1 = binary:at(?ITOA64, Value band 16#3f),
    Out1 = <<Out/binary, B1>>,
    Value1 = case Cursor1 < Count of
        true -> Value bor (binary:at(Input, Cursor1) bsl 8);
        false -> Value
    end,
    B2 = binary:at(?ITOA64, (Value1 bsr 6) band 16#3f),
    Out2 = <<Out1/binary, B2>>,
    encode64(Input, Count, Cursor1, Value1, Out2);
encode64(_, _, _, Out) -> Out.

-spec encode64(
    Input :: binary(),
    Count :: non_neg_integer(),
    Cursor :: non_neg_integer(),
    Value :: byte(),
    Out :: binary()
) -> Encoded :: binary().
encode64(Input, Count, Cursor, Value, Out) when Cursor < Count ->
    Cursor1 = Cursor + 1,
    Value2 = case Cursor1 < Count of
        true -> Value bor (binary:at(Input, Cursor1) bsl 16);
        false -> Value
    end,
    B3 = binary:at(?ITOA64, (Value2 bsr 12) band 16#3f),
    Out1 = <<Out/binary, B3>>,
    case Cursor1 >= Count of
        true ->
            Out1;

        false ->
            Cursor2 = Cursor1 + 1,
            B4 = binary:at(?ITOA64, (Value2 bsr 18) band 16#3f),
            Out2 = <<Out1/binary, B4>>,
            encode64(Input, Count, Cursor2, Out2)
    end;
encode64(_, _, _, _, Out) -> Out.
