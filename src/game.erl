-module(game).
-compile(export_all).
-record(state, {turn, field=[]}).

dealcards() ->
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
    {unknown,N} == lists:nth(C,(lists:nth(P,F))).

opencard(F, P, C) ->
    PCards = lists:nth(P, F),
    replaceElem(F, P, replaceElem(PCards, C, toKnown(lists:nth(C, PCards)))).

replaceElem(L, N, E) ->
    lists:sublist(L, N-1) ++ [E] ++ lists:sublist(L, N+1, length(L)).

toKnown({_,V}) ->
    {known, V}.

hideUnknown(A) ->
    lists:map(fun(L) -> 
		  lists:map(fun ({unknown, _}) ->
				    {unknown, 0};
				({known, X}) ->
				    {known, X}
			    end, L)
	      end, A).

get_int(Prompt) ->
    {N, _} = string:to_integer(io:get_line(Prompt)),
    N.

loop (S = #state{}) ->
    P = get_int("Person?? "),
    C = get_int("Card?? "),
    N = get_int("Number?? "),
    self() ! {self(), P, C, N, attack},
    receive
	{Player, Other, Card, Number, attack} ->
	    case check(S#state.field, Other, Card, Number) of
		true -> NField = opencard(S#state.field, Other, Card),
			Player ! {success, hideUnknown(NField)},
			io:format("~w~n", [hideUnknown(NField)]),
			loop(#state{field=NField});
		false -> Player ! {no, hideUnknown(S#state.field)},
			 loop(S)
	    end
    end.

start() ->
    F = game:dealcards(),
    spawn(game,loop,[#state{turn=self(),field=F}]),
    loop(#state{field=F}).

