-module (totient_server).
-export ([start_link/0]).
-record (state, {result = 0, procs, whoAsked, pids = [], now}).

% To run:
% -open Erlang shell: erl
% -compile server and slave: c(totient_server). c(totient_slave).
% -get and store shell's pid: Self = self().
% -spawn server and stor its pid: Pid = totient_server:start_link().
% -now server is ready to work, to calculate totient, send server a message of a form:
% ServersPid ! {YourPid, StartNumber, EndNumber, NumOfThreads}.
% -for example: Pid ! {Self, 1, 100000, 64}. 


start_link() -> spawn_link(fun init/0).


init() -> 
	loop(#state{}).


loop(State)	->
	receive
		{Pid, Low, Hi, Procs}	->
			Now = erlang:now(),
			splitList(Low, Hi, Procs),
			NewState = State#state{procs = Procs, whoAsked = Pid, now = Now},
			loop(NewState);
		{Pid, PartialResult}	->
			NewResult = PartialResult + State#state.result,
			NewPids = [Pid | State#state.pids],
			case length(NewPids) == State#state.procs of
				true ->
					{_, NewSecs, NewMiliSecs} = erlang:now(),
					{_, OldSecs, OldMiliSecs} = State#state.now,
					case (NewMiliSecs - OldMiliSecs) < 0 of
						true	-> 
							MiliSecsResult = 1000000 + NewMiliSecs - OldMiliSecs,
							SecsResult = 1000000 + NewSecs - OldSecs - 1000001;
						_ 		->
							MiliSecsResult = NewMiliSecs - OldMiliSecs,
							SecsResult = NewSecs - OldSecs
					end,
					io:format("{resultIs, ~p, timeElapsed, {seconds, ~p, microSecs, ~p}}~n", [NewResult, SecsResult, MiliSecsResult]),
					loop(#state{});
				_ 	-> 
					loop(State#state{result = NewResult, pids = NewPids})
			end;
		{terminate}	->
			io:format("Thanks for all, terminating!~n");
		_Message	->
			io:format("I (~p) got this: ~p and don't quite know what to do with it?!", [self(), _Message]),
			loop(State)
	end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BUNCH OF HELPERS						%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
splitList(Low, Hi, Procs)	->
	Res = (Hi - Low + 1) div Procs,
	splitList2(Low, Hi, Procs, Res).

splitList2(Low, Hi, 1, _Res)	->
	spawn(totient_slave, calcAndSend, [self(), Low, Hi]);
splitList2(Low, Hi, Procs, Res)	-> 
	spawn(totient_slave, calcAndSend, [self(), Low, Low + Res]),
	splitList2(Low + Res + 1, Hi, Procs - 1, Res).
	