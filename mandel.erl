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


%%%%%%%%%%

range(A,B,Step) ->
    if
	A >= B ->
	    [];
	true ->
	    [A | range(A+Step, B, Step)]
    end.


intervals(Interval, Num) ->
    %% Same as range(0,Interval,Interval/Num), but with the last element included
    Unit = Interval / float(Num),
    
    lists:map(fun(X) ->
		      Unit * X end,
	      lists:seq(0,Num)).

int_bytes_ascii(N) ->
    binary:list_to_bin(integer_to_list(N)).

%%%%%%%%%%


mandelbrot(Step, Iters) ->
    receive
	{Origin, P0, Pf, Idx} ->

	    %% Unpacking StartingPoint and FinalPoint
	    { {X0,Y0}, {Xf,Yf} } = {P0, Pf},

	    %% Print dimensions
	    {Width,Height} = { trunc(abs(Xf-X0) / Step),
			       trunc(abs(Yf-Y0) / Step) },
	    io:format("Width:~p\tHeight:~p\n", [Width, Height]),

	    %% Generate the complex coordinates 
	    Coords = [{X,Y} || Y <- range(Y0, Yf, Step),
			       X <- range(X0, Xf, Step)],

	    Result = lists:map(fun({X, Y}) -> divergence(X, Y, 0, 0, Iters) end,
			    Coords),

	    %% Send the bytes calculated to root
	    io:format("~p completed\n", [Idx]),
	    Origin ! {Idx, binary:list_to_bin(Result)},

	    mandelbrot(Step, Iters)
    end.
		   
   
start(Dimension, Cores) ->
    HeightIntervals = intervals(4, Cores),
    
    %% Distance beetween a pixel to another (in complex plane)
    Step = 4.0 / Dimension,
    io:format("Step: ~p\n", [Step]),


    %% List of {OrderIdx, Pid}
    Pids = [{Idx, spawn(mandel, mandelbrot, [Step, 25])} || Idx <- lists:seq(1,Cores)],
    
    %% Send core-specific informations
    lists:foreach(
      fun({Idx,Pid}) -> Pid ! { 
				self(),
				{-2, lists:nth(Idx, HeightIntervals)-2},
				{2, lists:nth(Idx+1, HeightIntervals)-2}, %-step?
				Idx
			      } end,
      Pids),

    
    %% List of {OrderIdx, BinaryValues}
    Results = 
	[
	 receive
	     Tupla ->
		 Tupla
	 end || _ <- Pids
	],
    
    Out = binary:list_to_bin(
    	    lists:map(fun({_, Bin}) -> Bin end, 
    		      lists:sort(fun({Idx1, _}, {Idx2, _}) ->
    					 Idx1 < Idx2 end,
    				 Results))),
			 
 
    Header = ["P5\n",
    	      int_bytes_ascii(Dimension), %% Width
    	      " ",
    	      int_bytes_ascii(Dimension),
    	      " 255\n"
    	     ],

    {ok, Fd} = file:open("out.pgm", [raw, write, binary]), 
    file:write(Fd, Header),
    file:write(Fd, Out),
    file:close(Fd).
    

