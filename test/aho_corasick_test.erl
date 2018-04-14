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
    Aho = aho_corasick:build_tree(["去你妈的","你妈"]), 
	Result = aho_corasick:match("去你妈", Aho),
	?_assertEqual(Result, [{2,3,"你妈"}]).

aho_corasick_eng() ->
    Aho = aho_corasick:build_tree(["BC", "ABCD"]),
	Result = aho_corasick:match("ABC", Aho),
    ?_assertEqual([{2,3,"BC"}], Result).
