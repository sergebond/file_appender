-module(file_appender_app).

-behaviour(application).
-include("file_appender.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  ?APP_NAME = ets:new(?APP_NAME, [set, public, named_table]),
  file_appender_sup:start_link().

stop(_State) ->
  ok.

%% internal functions
