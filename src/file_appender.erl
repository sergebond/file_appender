-module(file_appender).

%% API
-export([append/2]).

append(Path0, String0) ->
  Path = to_string(Path0),
  String = to_string(String0),
  ok = file_appender_process:append(Path, String).

%% PRIVATE______________________________________________________________________________________________________________
to_string(Term) when is_binary(Term) -> binary_to_list(Term);
to_string(Term) when is_list(Term) -> Term;
to_string(_) -> throw({error, badarg}).

