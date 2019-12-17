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

hash(Password) when is_binary(Password) ->
    RandBytes = crypto:strong_rand_bytes(6),
    crypt(Password, generate_salt(RandBytes)).

check(Password, Hash) when is_binary(Password) andalso is_binary(Hash) ->
    case size(Hash) < 6 of
        true -> false;
        _ -> crypt(Password, Hash) =:= Hash
    end.

%% ===================================================================
%% Internal
%% ===================================================================
generate_salt(RandBytes) ->
    S1 = binary:at(?ITOA64, ?ITERATIONS + 5),
    S2 = encode64(RandBytes, 6),
    <<"$P$", S1, S2/binary>>.

crypt(Password, Setting) ->
    Out0 = case binary:part(Setting, 0, 2) of
        <<"*0">> -> <<"*1">>;
        _ -> <<"*0">>
    end,
    Iter = index_of(binary:at(Setting, 3), ?ITOA64),

    case Iter >= 8 andalso Iter =< 30 of
        false ->
            Out0;

        true ->
            Count = 1 bsl Iter,
            Salt = binary:part(Setting, 4, 8),
            case size(Salt) =:= 8 of
                false ->
                    Out0;

                true ->
                    Hash0 = crypto:hash(md5, <<Salt/binary, Password/binary>>),
                    Hash = lists:foldl(fun(_C, Acc) ->
                        crypto:hash(md5, <<Acc/binary, Password/binary>>)
                    end, Hash0, lists:reverse(lists:seq(1, Count))),

                    S1 = binary:part(Setting, 0, 12),
                    S2 = encode64(Hash, 16),
                    <<S1/binary, S2/binary>>
            end
    end.

index_of(Item, Subject) -> index_of(Item, Subject, 1).
index_of(_, <<>>, _) -> not_found;
index_of(Item, <<Item, _/binary>>, Index) -> Index - 1;
index_of(Item, <<_, Tl/binary>>, Index) -> index_of(Item, Tl, Index + 1).

encode64(Input, Count) ->
    encode64(Input, Count, 0, <<"">>).

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
    case Cursor1 >= Count of
        true ->
            Out2;

        false ->
            Cursor2 = Cursor1 + 1,
            Value2 = case Cursor2 < Count of
                true -> Value1 bor (binary:at(Input, Cursor2) bsl 16);
                false -> Value1
            end,
            B3 = binary:at(?ITOA64, (Value2 bsr 12) band 16#3f),
            Out3 = <<Out2/binary, B3>>,
            case Cursor2 >= Count of
                true ->
                    Out3;

                false ->
                    Cursor3 = Cursor2 + 1,
                    B4 = binary:at(?ITOA64, (Value2 bsr 18) band 16#3f),
                    Out4 = <<Out3/binary, B4>>,
                    encode64(Input, Count, Cursor3, Out4)
            end
    end;
encode64(_, _, _, Out) -> Out.
