-module(mandel).
-compile(export_all).

modulo({X,Y}) ->
    math:sqrt(math:pow(X,2) + math:pow(Y,2)).


square(X,Y) ->
    { math:pow(X,2) - math:pow(Y,2), 
      2*X*Y
    }.

divergence(X,Y,Zx,Zy,Iter) ->
    Module = modulo({Zx,Zy}),

    if
	Module > 2.0 ->
	    {0,0};
	Iter =< 0 ->
	    {Zx,Zy};
	true ->
	    {Sx,Sy} = square(Zx,Zy),
	    {Rx, Ry}= {Sx+X, Sy+Y},
	    divergence(X,Y,Rx,Ry,Iter-1)
    end.

range(A,B,Step) ->
    if
	A >= B ->
	    [];
	true ->
	    [A | range(A+Step, B, Step)]
    end.

int_bytes_ascii(N) ->
    binary:list_to_bin(integer_to_list(N)).

mandelbrot(P0, Pf, Step, Iters) ->
    { {X0,Y0}, {Xf,Yf} } = {P0, Pf},

    {Width,Height} = { trunc((abs(X0) + abs(Xf)) / Step),
		       trunc((abs(Y0) + abs(Yf)) / Step)},

    io:format("Width:~p\tHeight:~p\n", [Width, Height]),

    Coords = [{X,Y} || X <- range(X0, Xf, Step),
		       Y <- range(Y0, Yf, Step)],
    
    Res = lists:map( fun({X,Y}) ->
		       round(127*modulo(divergence(X,Y,0,0,Iters))) end,
	       Coords),
    
    
    io:format("~p\n", [lists:max(Res)]),

    Header = ["P5\n",
	      int_bytes_ascii(Width),
	      " ",
	      int_bytes_ascii(Height),
	      " 255\n",
	      binary:list_to_bin(Res)
	     ],

    {ok, Fd} = file:open("out.pgm", [raw, write, binary]),
    file:write(Fd, Header),
    file:close(Fd).
