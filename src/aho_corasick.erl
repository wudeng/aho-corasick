%% 
%% author: wudeng256@gmail.com 
%%
-module(aho_corasick).

-export([
    build_tree/1,
    match/2
]).


%% build aho-corasick search tree from string patterns
build_tree(StringList) ->
    %% first build goto and output table
    {Goto, Output} = build_goto_output(StringList, _Goto=#{0 => #{}}, _Output=#{}, _State=0),
    %% then build fail table
    Fail = build_fail(Goto),
    {Goto, Fail, Output}.

%% State is used to locate node, every node is a map
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


build_goto_output([], Goto, Output, _MaxState) ->
    {Goto, Output};
build_goto_output([String|Tail], Goto, Output, MaxState) ->
    {NewGoto, NewState, NewMaxState} = add_pattern(String, Goto, 0, MaxState),
    NewOutput = Output#{NewState => String},
    build_goto_output(Tail, NewGoto, NewOutput, NewMaxState).

add_pattern([], Goto, State, MaxState) ->
    {Goto, State, MaxState};
add_pattern([Char|Tail], Goto, State, MaxState) ->
    #{State := Node} = Goto,
    {NewGoto, NewState, NewMaxState} = case maps:find(Char, Node) of
        error ->
            MaxState1 = MaxState + 1,
            NewNode = Node#{Char => MaxState1},
            {Goto#{MaxState1 => #{}, State => NewNode}, MaxState1, MaxState1};
        {ok, NextState} ->
            {Goto, NextState, MaxState}
    end,
    add_pattern(Tail, NewGoto, NewState, NewMaxState).

build_fail(#{0 := Node} = Goto) ->
    States = maps:values(Node),
    do_build_fail(States, Goto, _Fail=#{}).

%% build fail with bfs search
do_build_fail([], _Goto, Fail) ->
    Fail;
do_build_fail([State|Tail], Goto, Fail) ->
    #{State := Node} = Goto,

    %% find the starting point: the parent's fail node
    FailState = maps:get(State, Fail, 0),

    %% children
    Kvs = maps:to_list(Node),

    %% find fail node for all children
    NewFail = do_build_fail_inner(Kvs, FailState, Goto, Fail),

    %% add children states to the queue
    NewQueue = Tail ++ maps:values(Node),

    do_build_fail(NewQueue, Goto, NewFail).


%% 为节点构造失败指针
%% @param FailState 是当前节点的失败指针
do_build_fail_inner([], _FailState, _Goto, Fail) ->
    Fail;
do_build_fail_inner([{Char, State}|Tail], FailState, Goto, Fail) ->
    NewFail = find_fail_node({Char, State}, FailState, Goto, Fail),
    do_build_fail_inner(Tail, FailState, Goto, NewFail).

%% 为某个儿子节点构造失败指针
find_fail_node({Char, State}, FailState, Goto, Fail) ->
    #{FailState := Node} = Goto,
    case maps:find(Char, Node) of
        error ->
            case FailState =:= 0 of
                true -> %% 找不到，而且已经到了根节点，查找失败
                    Fail;
                false -> %% 找不到但是还没到根节点，继续往上找
                    NewFailState = maps:get(FailState, Fail, 0),
                    find_fail_node({Char, State}, NewFailState, Goto, Fail)
            end;
        {ok, TheFailState} -> %% 找到最近的失败节点的儿子节点拥有当前儿子节点的值，查找成功
            Fail#{State => TheFailState}
    end.

