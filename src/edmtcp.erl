-module('edmtcp').

%% functions: start_coordinator, stop_coordinator, checkpoint, restore, rotate_images
-export([]).

-define(PREFIX, "/usr/local/bin/").

-define(DMTCP_COORDINATOR, ?PREFIX ++ "dmtcp_coordinator").
-define(DMTCP_LAUNCH, ?PREFIX ++ "dmtcp_launch").
-define(DMTCP_RESTART, ?PREFIX ++ "dmtcp_restart").
-define(DMTCP_COMMAND, ?PREFIX ++ "dmtcp_command").

%%%===================================================================
%%% API
%%%===================================================================

%% Control flow:
%% ...

start_coordinator() ->
   ok.
stop_coordinator() ->
   ok.
checkpoint() ->
   ok.
restore() ->
   ok.
rotate_images() ->
   ok.

%%%===================================================================
%%%  Utility functions for subprocesses as Erlang ports
%%%  Adapted from nerves-utils
%%%===================================================================

% Run the specified executable and return ok on success
-spec run(string()) -> {ok, binary()} | {error, non_neg_integer()}.
run(Executable) ->
    run(Executable, [], []).

% Run an executable with the arguments passed in as a list
-spec run(string(), [string()]) -> {ok, binary()} | {error, non_neg_integer()}.
run(Executable, Args) ->
    run(Executable, Args, []).

% Run an executable with arguments and send Input to it
-spec run(string(), [string()], iodata()) -> {ok, binary()} | {error, non_neg_integer()}.
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

% Run an executable with the arguments passed in as a list and print
% out the program's output as it comes in.
-spec vrun(string(), [string()]) -> ok | {error, non_neg_integer()}.
vrun(Executable, Args) ->
    case os:find_executable(Executable) of
      false ->
         exit(enoent);
      FoundExecutable ->
         Port = open_port({spawn_executable, FoundExecutable},
                  [exit_status, {args, Args}, stderr_to_stdout]),
         verbose_loop_till_done(Port)
    end.

-spec loop_till_done(port(), binary()) -> {ok, binary()} | {error, non_neg_integer()}.
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

-spec verbose_loop_till_done(port()) -> ok | {error, non_neg_integer()}.
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

% Run a command using os:cmd/1, but pretty print its output
% in nice lines instead of one really long string.
-spec cmdpp(string()) -> ok.
cmdpp(CmdLine) ->
    Output = os:cmd(CmdLine),
    lists:foreach(fun(A) -> io:format("~s~n", [A]) end,
		  string:tokens(Output, "\n")),
    ok.
