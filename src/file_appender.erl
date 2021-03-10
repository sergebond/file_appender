-module(file_appender).

-include_lib("eunit/include/eunit.hrl").

-include("file_appender.hrl").

%% API
-export([append/2]).

append(Path0, String0) ->
  Path = helper:to_string(Path0),
  String = helper:to_string(String0),
  ok = file_appender_process:append(Path, String).

%% Tests
append_test_() ->
  application:start(?APP_NAME),
  LinesCount = 10,
  FilePaths = lists:map(fun(_) -> random_filename() end, lists:seq(1,10)),
  {setup,
    fun() ->
      lists:foreach(fun(_) -> %% Append to each file 10 random lines
        lists:foreach(fun(FilePath) ->
          append(FilePath, helper:get_random_string(5)) end, FilePaths)
                    end, lists:seq(1, LinesCount)),
      timer:sleep(?TIMEOUT) %% Waiting for files release to read
    end,
    fun(_) -> lists:foreach(fun(FilePath) -> file:delete(FilePath) end, FilePaths) end,
    fun(_) ->
      ?_assert(lists:all(fun(FilePath) ->
          {ok, Binary} = file:read_file(FilePath),
          LinesCount == get_lines_count(Binary)
        end, FilePaths))
    end
  }.

append_exception_test_() ->
  [
    ?_assertException(error, badarg, append(1, "test")),
    ?_assertException(error, badarg, append('a', "test")),
    ?_assertException(error, badarg, append("/tmp/test", a))
    ].

%% helpers__
get_lines_count(Payload) ->
  length(binary:split(Payload, <<"\n">>, [global, trim_all])).

random_filename() ->
  filename:join([helper:get_tmp_dir_path(), helper:get_random_string(10) ++ ".txt"]).