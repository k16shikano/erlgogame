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

loop (S = #state{}) ->
    receive
	{Player, Other, Card, Number, attack} ->
	    case check(S#state.field, Other, Card, Number) of
		true -> NField = opencard(S#state.field, Other, Card),
			Player ! {success, NField},
			loop(#state{field=NField});
		false -> Player ! no,
			 loop(S)
	    end
    end.

