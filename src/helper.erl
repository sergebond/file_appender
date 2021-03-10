-module(helper).
-author("sergeybondarchuk").
-include("file_appender.hrl").
-include_lib("kernel/include/file.hrl").

%% API
-compile(export_all).

to_string(Term) when is_binary(Term) -> binary_to_list(Term);
to_string(Term) when is_list(Term) -> Term;
to_string(_) -> erlang:error(badarg).

get_tmp_dir_path() ->
  case application:get_env(?APP_NAME, tmp_dir) of
    {ok, TmpDir} ->
      TmpDir;
    _ ->
      case os:getenv("TMPDIR") of
        false ->
          case os:getenv("TEMP") of
            false ->
              case os:getenv("TMP") of
                false ->
                  case write_tmp_dir("/tmp") of
                    false ->
                      Cwd =
                        case file:get_cwd() of
                          {ok, Dir} -> Dir;
                          _         -> "."
                        end,
                      case write_tmp_dir(Cwd) of
                        false -> false;
                        LTmp -> LTmp
                      end;
                    STmp -> STmp
                  end;
                Tmp -> Tmp
              end;
            Temp -> Temp
          end;
        TmpDir -> TmpDir
      end
  end.


write_tmp_dir(Path) ->
  case file:read_file_info(Path) of
    {ok, #file_info{type = directory, access = Access}} when Access =:= read_write; Access =:= write ->
      Path ++ "/";
    _ ->
      false
  end.

get_random_string(Length) ->
  AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890",
  get_random_string(Length, AllowedChars).

get_random_string(Length, AllowedChars) ->
  lists:foldl(fun(_, Acc) ->
    [pick_random(AllowedChars) | Acc] end, [], lists:seq(1, Length)).

pick_random(List) when is_list(List) ->
  lists:nth(rand:uniform(length(List)), List).