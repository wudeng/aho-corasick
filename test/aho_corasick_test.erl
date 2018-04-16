-module(aho_corasick_test).

-include_lib("eunit/include/eunit.hrl").

ahocorasick_test_() ->
    [
        {
            "test unicode",
            {
                setup,
                fun() -> aho_corasick:build_tree(["去你妈的","你妈"]) end,
                fun aho_corasick_chn/1
            }
        },
        {
            "test ascii code",
            {
                setup,
                fun() -> aho_corasick:build_tree(["BC","ABCD"]) end,
                fun aho_corasick_eng/1
            }
        }
    ].

aho_corasick_chn(Aho) ->
    [
        ?_assertEqual([{2,3,"你妈"}], aho_corasick:match("去你妈", Aho)),
        ?_assertEqual([], aho_corasick:match("测试", Aho))
    ].

aho_corasick_eng(Aho) ->
	Result = aho_corasick:match("ABC", Aho),
    ?_assertEqual([{2,3,"BC"}], Result).
