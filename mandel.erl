-module(mandel).
-import(utils, [range/3, intervals/2, int_bytes_ascii/1]).
-import(complex, [divergence/5]).
-compile(export_all).


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
	    Coords = [{X,Y} || Y <- utils:range(Y0, Yf, Step),
			       X <- utils:range(X0, Xf, Step)],

	    Result = lists:map(fun({X, Y}) -> complex:divergence(X, Y, 0, 0, Iters) end,
			    Coords),

	    %% Send the bytes calculated to root
	    io:format("~p completed\n", [Idx]),
	    Origin ! {Idx, binary:list_to_bin(Result)},

	    mandelbrot(Step, Iters)
    end.
		   
   
start(Dimension, Cores) ->
    Intervals = utils:intervals(4, Cores),
    
    %% Distance beetween a pixel to another (in complex plane)
    Step = 4.0 / Dimension,
    io:format("Step: ~p\n", [Step]),


    %% List of {OrderIdx, Pid}
    Pids = [{Idx, spawn(mandel, mandelbrot, [Step, 25])} || Idx <- lists:seq(1,Cores)],
    
    %% Send core-specific informations
    lists:foreach(
      fun({Idx,Pid}) -> Pid ! { 
				self(),
				{-2, lists:nth(Idx, Intervals)-2},
				{2, lists:nth(Idx+1, Intervals)-2}, %-step?
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
    	      utils:int_bytes_ascii(Dimension), %% Width
    	      " ",
    	      utils:int_bytes_ascii(Dimension),
    	      " 255\n"
    	     ],

    {ok, Fd} = file:open("out.pgm", [raw, write, binary]), 
    file:write(Fd, Header),
    file:write(Fd, Out),
    file:close(Fd).
    

