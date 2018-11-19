-module(utils).
-compile(export_all).

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
