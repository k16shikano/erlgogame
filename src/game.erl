-module(game).
-compile(export_all).
-record(state, {turn, field=[]}).

dealcards() ->
    %% even -> white,
    %% odd  -> black.
    L = [{unknown,X} || 
	    {_,X} <- lists:sort([{random:uniform(), N} || 
				    N <- lists:seq(1,16)])],
    dividefour(L).

dividefour(L) ->
    case length(L) > 4 of
	true -> {A,B} = lists:split(4,L),
		[lists:sort(A) | dividefour(B)];
	false -> [lists:sort(L)]
    end.

check(F, P, C, N)->
    {_, X} = lists:nth(C,(lists:nth(P,F))),
    (X + 1) div 2 == N.

opencard(F, P, C) ->
    PCards = lists:nth(P, F),
    replaceElem(F, P, replaceElem(PCards, C, toKnown(lists:nth(C, PCards)))).

replaceElem(L, N, E) ->
    lists:sublist(L, N-1) ++ [E] ++ lists:sublist(L, N+1, length(L)).

toKnown({_,V}) ->
    {known, V}.

hideUnknown(A) ->
    [hd(A) | 
     lists:map(fun(L) -> 
		       lists:map(fun ({unknown, X}) ->
					 {unknown, X rem 2};
				     ({known, X}) ->
					 {known, X}
				 end, L)
	       end, tl(A))].

get_int(Prompt) ->
    {N, _} = string:to_integer(io:get_line(Prompt)),
    N.

get_card(Prompt) ->
    N = hd(io:get_line(Prompt)) - 64 + 7,
    {N div 4, lists:nth(N rem 4 + 1, [4,3,2,1])}.

displayOthersCard({unknown, X}) ->
    if X == 0 ->
	    "[ ]";
       true ->
	    [<<"\e[", "47;30m">>, "[ ]", <<"\e[", "0m">>]
    end;
displayOthersCard({known, N}) ->
    if N rem 2 == 0 ->
	    ["[", integer_to_list((N + 1) div 2), "]"];
       true ->
	    [<<"\e[", "47;30m">>, "[", integer_to_list((N + 1) div 2), "]", <<"\e[", "0m">>]
    end.

displayMyCard({_, N}) ->
    if N rem 2 == 0 ->
	    ["[", integer_to_list((N + 1) div 2), "]"];
       true ->
	    [<<"\e[", "47;30m">>, "[", integer_to_list((N + 1) div 2), "]", <<"\e[", "0m">>]
    end.
	    
displayField(F, U) ->
    [Me, P1, P2, P3] = F,
    io:format("\n~s----------------------~s\n", [<<"\e[", "36m">>, <<"\e[", "0m">>]),
    io:format("     E   F   G   H    \n"),
    io:format("    ~s ~s ~s ~s\n",
	      lists:map(fun(X) -> displayOthersCard(X) end, lists:reverse(P2))),
    
    io:format("D~s               ~sI\n", [displayOthersCard(lists:nth(1, P1)), 
					  displayOthersCard(lists:nth(4, P3))]),
    io:format("C~s               ~sJ\n", [displayOthersCard(lists:nth(2, P1)), 
					  displayOthersCard(lists:nth(3, P3))]),
    io:format("B~s               ~sK\n", [displayOthersCard(lists:nth(3, P1)), 
					  displayOthersCard(lists:nth(2, P3))]),
    io:format("A~s               ~sL\n", [displayOthersCard(lists:nth(4, P1)), 
					  displayOthersCard(lists:nth(1, P3))]),

    io:format("\n"),
    io:format("    ~s ~s ~s ~s\n",
	      lists:map(fun(X) -> displayMyCard(X) end, Me)),
    io:format("\n").

loop (S = #state{}) ->

    displayField(hideUnknown(S#state.field), 1),
    {P,C} = get_card("Select Card (A-L) ... "),
    N = get_int("Atack Number (1-8) ... "),

    self() ! {self(), P, C, N, attack},
    receive
	{Player, Other, Card, Number, attack} ->
	    case check(S#state.field, Other, Card, Number) of
		true -> NField = opencard(S#state.field, Other, Card),
			Player ! {success, hideUnknown(NField)},
			loop(#state{field=NField});
		false -> Player ! {no, hideUnknown(S#state.field)},
			 loop(S)
	    end
    end.

start() ->
    F = game:dealcards(),
    spawn(game,loop,[#state{turn=self(),field=F}]).
