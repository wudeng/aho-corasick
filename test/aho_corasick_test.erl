-module(aho_corasick_test).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

ahocorasick_test_() ->
    [
        aho_corasick_chn(),
        aho_corasick_eng()
    ].

aho_corasick_chn() ->
    Aho = aho_corasick:build_trie(["不要脸","敏感"]), 
	Result = aho_corasick:match("我不要脸怎么就敏感了", Aho),
	?_assertEqual(Result, ["敏感", "不要脸"]).

aho_corasick_eng() ->
    Aho = aho_corasick:build_trie(["BC", "ABCD"]),
	Result = aho_corasick:match("XABCDG", Aho),
    ?_assertEqual(["ABCD", "BC"], Result).
