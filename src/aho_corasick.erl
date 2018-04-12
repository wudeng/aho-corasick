%% 
%% author: wudeng256@gmail.com 
%%
-module(aho_corasick).

-export([
    build_trie/1,
    match/2
]).


%% State is used to find node, every node is a map
%% state 0 is the root node
%%
%% Goto:  State -> Map{Char -> State}
%% Ouput: State -> String
%% Fail: State -> State

%% try to find patterns in string
match(String, {Goto, Fail, Output}) ->
    do_match(String, 0, {Goto, Fail, Output}, []).


do_match([], _, _, MatchList) ->
    MatchList;
do_match([Char|Tail], State, {Goto, Fail, Output}, MatchList) ->
    {NewState, NewMatchList} = do_match_inner(Char, State, {Goto, Fail, Output}, MatchList),
    do_match(Tail, NewState, {Goto, Fail, Output}, NewMatchList).


%% {NewState, NewMatchList} 
do_match_inner(Char, State, {Goto, Fail, Output}, MatchList) ->
    #{State := Node} = Goto,
    case maps:find(Char, Node) of
        error ->
            case State =:= 0 of
                true ->
                    {State, MatchList};
                false ->
                    NextState = maps:get(State, Fail, 0),
                    do_match_inner(Char, NextState, {Goto, Fail, Output}, MatchList)
            end;
        {ok, NextState} ->
            NewMatchList = get_output(NextState, {Goto, Fail, Output}, MatchList),
            {NextState, NewMatchList}
    end.

get_output(0, _, MatchList) ->
    MatchList;
get_output(State, {Goto, Fail, Output}, MatchList) ->
    FailState = maps:get(State, Fail, 0),
    NewMatchList = case maps:find(State, Output) of
        error ->
            MatchList;
        {ok, Pattern} ->
            [Pattern|MatchList]
    end,
    get_output(FailState, {Goto, Fail, Output}, NewMatchList).


build_trie(StringList) ->
    State = 0,
    Output = #{},
    Goto = #{0 => #{}},
    {NewGoto, NewOutput} = build_trie(StringList, Goto, Output, State),
    NewFail = build_fail(NewGoto),
    {NewGoto, NewFail, NewOutput}.

build_trie([], Goto, Output, _MaxState) ->
    {Goto, Output};
build_trie([String|Tail], Goto, Output, MaxState) ->
    {NewGoto, NewState, NewMaxState} = do_build_trie(String, Goto, 0, MaxState),
    build_trie(Tail, NewGoto, Output#{
        NewState => String
    }, NewMaxState).

do_build_trie([], Goto, State, MaxState) ->
    {Goto, State, MaxState};
do_build_trie([Char|Tail], Goto, State, MaxState) ->
    #{State := Node} = Goto,
    case maps:find(Char, Node) of
        error ->
            NewMaxState = MaxState + 1,
            do_build_trie(Tail, Goto#{
                NewMaxState => #{},
                State => Node#{Char => NewMaxState}
            }, NewMaxState, NewMaxState);
        {ok, NewState} ->
            do_build_trie(Tail, Goto, NewState, MaxState)
    end.

build_fail(Goto) ->
    #{0 := Node} = Goto,
    States = maps:values(Node),
    Fail = #{},
    do_build_fail(States, Goto, Fail).

do_build_fail([], _Goto, Fail) ->
    Fail;
do_build_fail([State|Tail], Goto, Fail) ->
    #{State := Node} = Goto,
    Kvs = maps:to_list(Node),
    FailState = maps:get(State, Fail, 0),

    %% 为当前节点的所有儿子节点构造失败指针
    NewFail = do_build_fail_inner(Kvs, FailState, Goto, Fail),

    %% 将儿子节点加入队列
    do_build_fail(Tail++maps:values(Node), Goto, NewFail).


%% 为节点构造失败指针
%% @param FailState 是当前节点的失败指针
do_build_fail_inner([], _FailState, _Goto, Fail) ->
    Fail;
do_build_fail_inner([{Char, State}|Tail], FailState, Goto, Fail) ->
    NewFail = do_build_fail_inner_1({Char, State}, FailState, Goto, Fail),
    do_build_fail_inner(Tail, FailState, Goto, NewFail).

%% 为某个儿子节点构造失败指针
do_build_fail_inner_1({Char, State}, FailState, Goto, Fail) ->
    #{FailState := Node} = Goto,
    case maps:find(Char, Node) of
        error ->
            case FailState =:= 0 of
                true -> %% 找不到，而且已经到了根节点，查找失败
                    Fail;
                false -> %% 找不到但是还没到根节点，继续往上找
                    NewFailState = maps:get(FailState, Fail, 0),
                    do_build_fail_inner_1({Char, State}, NewFailState, Goto, Fail)
            end;
        {ok, TheFailState} -> %% 找到最近的失败节点的儿子节点拥有当前儿子节点的值，查找成功
            Fail#{State => TheFailState}
    end.


