-module(complex).
-compile(export_all).

modulo(X,Y) ->
    math:sqrt(math:pow(X,2) + math:pow(Y,2)).

square(X,Y) ->
    {
     math:pow(X,2) - math:pow(Y,2), 
     2*X*Y
    }.

divergence(X,Y,Zx,Zy,Iter) ->
    Module = modulo(Zx,Zy),

    if
	Module > 2.0 ->
	    10*Iter;
	Iter =< 0, Module =< 2.0 ->
	    0;
	Iter =< 0 ->
	    8*Iter;
	true ->
	    {Sx,Sy} = square(Zx,Zy),
	    {Rx, Ry}= {Sx+X, Sy+Y},
	    divergence(X,Y,Rx,Ry,Iter-1)
    end.
