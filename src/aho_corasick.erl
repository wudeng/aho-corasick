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
    %% then build failure table
    Failure = build_failure(Goto),
    {Goto, Failure, Output}.

%% State is used to locate node, every node is a map
%% state 0 is the root node
%%
%% Goto:  State -> Map{Char -> State}
%% Ouput: State -> String
%% Failure: State -> State

%% try to find patterns in string
%% the match index starts from 1
%% @return [{StartIndex, EndIndex, Pattern},...]
match(String, {Goto, Failure, Output}) ->
    do_match(String, 0, {Goto, Failure, Output}, _Index = 1, _MatchList = []).


do_match([], _, _, _Index, MatchList) ->
    MatchList;
do_match([Char|Tail], State, {Goto, Failure, Output}, Index, MatchList) ->
    {NewState, NewMatchList} = do_match_inner(Char, State, {Goto, Failure, Output}, Index, MatchList),
    do_match(Tail, NewState, {Goto, Failure, Output}, Index + 1, NewMatchList).


%% {NewState, NewMatchList} 
do_match_inner(Char, State, {Goto, Failure, Output}, Index, MatchList) ->
    #{State := Node} = Goto,
    case maps:find(Char, Node) of
        error ->
            case State =:= 0 of
                true ->
                    {State, MatchList};
                false ->
                    NextState = maps:get(State, Failure, 0),
                    do_match_inner(Char, NextState, {Goto, Failure, Output}, Index, MatchList)
            end;
        {ok, NextState} ->
            NewMatchList = get_output(NextState, {Goto, Failure, Output}, Index, MatchList),
            {NextState, NewMatchList}
    end.

get_output(0, _, _Index, MatchList) ->
    MatchList;
get_output(State, {Goto, Failure, Output}, Index, MatchList) ->
    NewMatchList = case maps:find(State, Output) of
        error -> MatchList;
        {ok, Pattern} -> [{Index-length(Pattern) + 1, Index, Pattern} | MatchList]
    end,
    FailureState = maps:get(State, Failure, 0),
    get_output(FailureState, {Goto, Failure, Output}, Index, NewMatchList).


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

build_failure(#{0 := Node} = Goto) ->
    States = maps:values(Node),
    do_build_failure(States, Goto, _Failure=#{}).

%% build failure with bfs search
do_build_failure([], _Goto, Failure) ->
    Failure;
do_build_failure([State|Tail], Goto, Failure) ->
    #{State := Node} = Goto,

    %% find the starting point: the parent's failure node
    FailureState = maps:get(State, Failure, 0),

    %% children
    Kvs = maps:to_list(Node),

    %% find failure node for all children
    NewFailure = do_build_failure_inner(Kvs, FailureState, Goto, Failure),

    %% add children states to the queue
    NewQueue = Tail ++ maps:values(Node),

    do_build_failure(NewQueue, Goto, NewFailure).


%% 为节点构造失败指针
%% @param FailureState 是当前节点的失败指针
do_build_failure_inner([], _FailureState, _Goto, Failure) ->
    Failure;
do_build_failure_inner([{Char, State}|Tail], FailureState, Goto, Failure) ->
    NewFailure = find_failure_node({Char, State}, FailureState, Goto, Failure),
    do_build_failure_inner(Tail, FailureState, Goto, NewFailure).

%% 为某个儿子节点构造失败指针
find_failure_node({Char, State}, FailureState, Goto, Failure) ->
    #{FailureState := Node} = Goto,
    case maps:find(Char, Node) of
        error ->
            case FailureState =:= 0 of
                true -> %% 找不到，而且已经到了根节点，查找失败
                    Failure;
                false -> %% 找不到但是还没到根节点，继续往上找
                    NewFailureState = maps:get(FailureState, Failure, 0),
                    find_failure_node({Char, State}, NewFailureState, Goto, Failure)
            end;
        {ok, TheFailureState} -> %% 找到最近的失败节点的儿子节点拥有当前儿子节点的值，查找成功
            Failure#{State => TheFailureState}
    end.

