-module(mandel).
-compile(export_all).

modulo({X,Y}) ->
    math:sqrt(math:pow(X,2) + math:pow(Y,2)).


square(X,Y) ->
    {
     math:pow(X,2) - math:pow(Y,2), 
     2*X*Y
    }.

divergence(X,Y,Zx,Zy,Iter) ->
    Module = modulo({Zx,Zy}),

    if
	Module > 2.0 ->
	    5*Iter;
	Iter =< 0, Module =< 2.0 ->
	    4*Iter;
	Iter =< 0 ->
	    5*Iter;
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

mandelbrot(Step, Iters) ->
    receive
	{Origin, P0, Pf, Idx, Rounding} ->
	    
	    { {X0,Y0}, {Xf,Yf} } = {P0, Pf},

	    {Width,Height} = { trunc(abs(Xf-X0) / Step),
			       trunc(abs(Yf+Rounding*Step-Y0) / Step) },

	    io:format("Width:~p\tHeight:~p\n", [Width, Height]),

	    Coords = [{X,Y} || Y <- range(Y0, Yf+Step*Rounding, Step),
			       X <- range(X0, Xf+Rounding*Step, Step)],

	    Res = lists:map( fun({X,Y}) ->
				     divergence(X,Y,0,0,Iters) end,
			     Coords),


	    io:format("~p\n", [lists:max(Res)]),

	    Contents = binary:list_to_bin(Res),

	    Origin ! {Idx, Contents},

	    mandelbrot(Step, Iters)
    end.

intervals(Interval, Num) ->
    Unit = Interval / float(Num),
    
    lists:map(fun(X) ->
		      Unit * X end,
	      lists:seq(0,Num)).
		      
start(Step, Cores) ->
    HeightIntervals = intervals(4, Cores),

    Unit = lists:nth(2,HeightIntervals) - lists:nth(1, HeightIntervals),
    TrueHeight  = trunc(Cores * (Unit/Step)),
    RoundHeight = trunc(Cores * trunc(Unit/Step)),
    RoundingError = TrueHeight - RoundHeight,

    ToAdd = [0 || _ <- lists:seq(1,Cores - 1)] ++ [RoundingError],
    
    io:format("~p\n", [ToAdd]),
    io:format("Exected Height: ~p\n", [TrueHeight]),

    Pids = [{Idx, spawn(mandel, mandelbrot, [Step, 50])} || Idx <- lists:seq(1,Cores)],
    
    lists:foreach(fun({Idx,Pid}) ->
    			  Pid !
    			      { self(),
    				{-2, lists:nth(Idx, HeightIntervals)-2},
    				{2, lists:nth(Idx+1, HeightIntervals)-2}, %-step?
    				Idx,
				lists:nth(Idx, ToAdd)
    			      } end,
    		  Pids),
    
    Results = [
    	       receive
    		   Tupla ->
    		       Tupla
    	       end || _ <- Pids],

    %% io:format("~p\n", [Results]),
    
    Out = binary:list_to_bin(
    	    lists:map(fun({_,B})->B end, 
    		      lists:sort(fun({Idx1,_}, {Idx2,_}) ->
    					 Idx1 < Idx2 end,
    				 Results))),
			 
 
    Header = ["P5\n",
    	      int_bytes_ascii(TrueHeight), %% Width
    	      " ",
    	      int_bytes_ascii(TrueHeight),
    	      " 255\n"
    	     ],

    {ok, Fd} = file:open("out.pgm", [raw, write, binary]),
    file:write(Fd, Header),
    file:write(Fd, Out),
    file:close(Fd).
    

