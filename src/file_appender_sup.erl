-module(file_appender_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => simple_one_for_one},
  ChildSpecs = [
    #{id => file_appender_process, start => {file_appender_process, start_link, []}, restart => temporary}
  ],
  {ok, {SupFlags, ChildSpecs}}.