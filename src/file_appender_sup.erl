%%%-------------------------------------------------------------------
%% @doc file_appender top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(file_appender_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
%%init([]) ->
%%
%%  {ok, {SupFlags, ChildSpecs}}.

init([]) ->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpecs = [
      #{id => file_appender_process, start => {file_appender_process, start_link, []}, restart => temporary}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions


%%init([]) ->
%%  Procs = [{gun, {gun, start_link, []},
%%    temporary, 5000, worker, [gun]}],
%%  {ok, {{simple_one_for_one, 10, 10}, Procs}}.

%%init([]) ->
%%  Procs = [
%%    #{id => gun, start => {gun, start_link, []}, restart => temporary}
%%  ],
%%  {ok, {#{strategy => simple_one_for_one}, Procs}}.