-module('edmtcp').

-export([launch/1,
         checkpoint/1,
         restore/2,
         status/1,
         set_interval/2,
         terminate/2,
         flush_share/0]).

-define(PREFIX, "/usr/local/bin/").
-define(CKPTDIR, "/usr/local/share/edmtcp/").

-define(DMTCP_LAUNCH, ?PREFIX ++ "dmtcp_launch").
-define(DMTCP_RESTART, ?PREFIX ++ "dmtcp_restart").
-define(DMTCP_COMMAND, ?PREFIX ++ "dmtcp_command").

%%%===================================================================
%%% API
%%%===================================================================

launch(ProgramName) ->
   Port = runnoloop(?DMTCP_LAUNCH, 
             ["--new-coordinator", 
              "--ckptdir", ?CKPTDIR ++ ProgramName,
              "--port-file", ?CKPTDIR ++ filename:basename(ProgramName),
              ProgramName
             ]).
                       
checkpoint(ProgramName) ->
   Port = vrun(?DMTCP_COMMAND, 
                ["--coord-port", get_coord_port(ProgramName),
                 "--checkpoint"
                ]).
restore(ProgramName, CkptFile) ->
   Port = run(?DMTCP_RESTART, 
                ["--new-coordinator",
                 "--port-file", ?CKPTDIR ++ filename:basename(ProgramName),
                 ?CKPTDIR ++ CkptFile
                ]).

status(ProgramName) ->
   cmdpp(?DMTCP_COMMAND ++ " --status" ++ " --port " ++ get_coord_port(ProgramName)).
set_interval(ProgramName, Interval) ->
   cmdpp(?DMTCP_COMMAND ++ " --port " ++ get_coord_port(ProgramName) ++ 
         " --interval " ++ integer_to_list(Interval)).
terminate(ProgramName, Arg) ->
   case Arg of
      [] ->
         cmdpp(?DMTCP_COMMAND ++ " --port " ++ get_coord_port(ProgramName) ++ " --kill");
      quit ->
         cmdpp(?DMTCP_COMMAND ++ " --port " ++ get_coord_port(ProgramName) ++ " --quit")
   end.

flush_share() ->
   lists:foreach(fun(F) -> ok = file:delete(F) end, filelib:wildcard(?CKPTDIR ++ "*.dmtcp")),
   lists:foreach(fun(F) -> ok = file:delete(F) end, filelib:wildcard(?CKPTDIR ++ "dmtcp_restart*")).

%%%===================================================================
%%%  Utility functions for subprocesses as Erlang ports
%%%  Some adapted from nerves-utils
%%%===================================================================

get_coord_port(ProgramName) ->
   {ok, CoordPort} = file:read_file(?CKPTDIR ++ filename:basename(ProgramName)),
   PortString = binary_to_list(CoordPort),
   PortString.

run(Executable) ->
    run(Executable, [], []).

run(Executable, Args) ->
    run(Executable, Args, []).

run(Executable, Args, Input) ->
    case os:find_executable(Executable) of
      false ->
         exit(enoent);
      FoundExecutable ->
         Port = open_port({spawn_executable, FoundExecutable},
                  [exit_status, {args, Args}, stderr_to_stdout]),
         Port ! {self(), {command, Input}},
         loop_till_done(Port, <<>>)
    end.
    
runnoloop(Executable, Args) ->
    case os:find_executable(Executable) of
      false ->
         exit(enoent);
      FoundExecutable ->
         open_port({spawn_executable, FoundExecutable},
                  [exit_status, {args, Args}, stderr_to_stdout])
    end.

vrun(Executable, Args) ->
    case os:find_executable(Executable) of
      false ->
         exit(enoent);
      FoundExecutable ->
         Port = open_port({spawn_executable, FoundExecutable},
                  [exit_status, {args, Args}, stderr_to_stdout]),
         verbose_loop_till_done(Port)
    end.

loop_till_done(Port, Data) ->
    receive
      {Port, {data, NewData}} ->
         BinaryNewData = list_to_binary(NewData),
         ConcatenatedData = <<Data/binary, BinaryNewData/binary>>,
         loop_till_done(Port, ConcatenatedData);
      {Port, {exit_status, 0}} ->
         {ok, Data};
      {Port, {exit_status, ExitStatus}} ->
         {error, ExitStatus}
    end.

verbose_loop_till_done(Port) ->
    receive
      {Port, {data, NewData}} ->
         lists:foreach(fun(A) -> io:format("~s~n", [A]) end,
            string:tokens(NewData, "\n")),
         verbose_loop_till_done(Port);
      {Port, {exit_status, 0}} ->
         ok;
      {Port, {exit_status, ExitStatus}} ->
         {error, ExitStatus}
    end.

cmdpp(CmdLine) ->
    Output = os:cmd(CmdLine),
    lists:foreach(fun(A) -> io:format("~s~n", [A]) end,
		  string:tokens(Output, "\n")),
    ok.
