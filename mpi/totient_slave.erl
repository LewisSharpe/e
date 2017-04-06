-module (totient_slave).
-compile (export_all).

calcAndSend(Pid, Low, Hi)	->
	MySum = getTotientsSum(Low, Hi),
	Pid ! {self(), MySum}.


getTotientsSum(Low, Hi)	->
	lists:foldl(fun(X, Sum)	-> X + Sum end, 0, lists:map(fun getSmartTotient/1, lists:seq(Low, Hi))).


getSmartTotient(1)   ->  1;
getSmartTotient(Number)   -> first2(Number, Number).


first2(Number, Result) when(Number rem 2 == 0)	->
	first3(Number, Result - Result div 2);
first2(Number, Result)	->
	second(Number, Result).

first3(Number, _Result) when(Number rem 2 == 0)	->
	first3(Number div 2, _Result);
first3(Number, Result)	-> first2(Number, Result).


% Prepare auxiliary functions and pass it all to second2
second(Number, Result)	->
	LoopCondition = fun(AnIndex, ANumber)	->
		case (AnIndex * AnIndex) =< ANumber of
			true 	-> true;
			_ 		-> false
		end
	end,
	IfCondition = fun(AnIndex, ANumber)	-> 
		ANumber rem AnIndex =:= 0
	end,
	WhileCondition = fun(AnIndex, ANumber)	->
		ANumber rem AnIndex =:= 0
	end,
	second2(Number, Result, 3, LoopCondition, IfCondition, WhileCondition).

% Outer for loop
second2(Number, Result, Index, LoopCondition, IfCondition, WhileCondition)	->
	case LoopCondition(Index, Number) of
		true 	-> second3(Number, Result, Index, LoopCondition, IfCondition, WhileCondition);
		_ 		-> third(Number, Result)
	end.

% Inner if statement
second3(Number, Result, Index, LoopCondition, IfCondition, WhileCondition)	->
	case IfCondition(Index, Number) of
		true 	-> second4(Number, Result - Result div Index, Index, LoopCondition, IfCondition, WhileCondition);
		_ 		-> second2(Number, Result, Index + 2, LoopCondition, IfCondition, WhileCondition)
	end.

% Inner do while statement
second4(Number, Result, Index, LoopCondition, IfCondition, WhileCondition)	->
	case WhileCondition(Index, Number)	of
		true 	-> second4(Number div Index, Result, Index, LoopCondition, IfCondition, WhileCondition);
		_ 		-> second2(Number, Result, Index + 2, LoopCondition, IfCondition, WhileCondition)
	end.


third(Number, Result) when(Number > 1)	->
	Result - Result div Number;
third(_, Result)	-> Result.