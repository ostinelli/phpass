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
-module(phpass_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests
%% ===================================================================
hash_and_check_generation_success_test() ->
    Password = <<"mypass">>,
    Hash = phpass:hash(Password),
    ?assertEqual(true, phpass:check(Password, Hash)).

hash_and_check_generation_long_pass_success_test() ->
    Password = <<"my-quite_longer-pass @#$%^&*(1234567890){}">>,
    Hash = phpass:hash(Password),
    ?assertEqual(true, phpass:check(Password, Hash)).

hash_and_check_generation_unicode_success_test() ->
    Password = <<"你好"/utf8>>,
    Hash = phpass:hash(Password),
    ?assertEqual(true, phpass:check(Password, Hash)).

hash_and_check_generation_when_empty_success_test() ->
    Password = <<"">>,
    Hash = phpass:hash(Password),
    ?assertEqual(true, phpass:check(Password, Hash)).

hash_and_check_known_test() ->
    ?assertEqual(true, phpass:check(<<"mypass">>, <<"$P$BNzlTqiPcLNUyBX3UsPCYKXhAeymcq.">>)),
    ?assertEqual(true, phpass:check(<<"mypass-other/!">>, <<"$P$BxdXqDhmnDF0M.AU0KlM9mU5IpagZ4.">>)),
    ?assertEqual(true, phpass:check(<<"unicode-åäö-pass"/utf8>>, <<"$P$Br.78pUezr/zPxQdA81h41JdHtf9Kh1">>)).

hash_and_check_known_fail_test() ->
    Password = <<"mypass">>,
    ?assertEqual(false, phpass:check(Password, <<"$P$BxdXqDhmnDF0M.AU0KlM9mU5IpagZ4.">>)),
    ?assertEqual(false, phpass:check(Password, <<"">>)),
    ?assertEqual(false, phpass:check(<<"">>, <<"">>)).
