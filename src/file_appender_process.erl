-module(file_appender_process).

-behaviour(gen_server).
-include("file_appender.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-export([append/2]).

-define(SERVER, ?MODULE).

-record(state, {
  io_device :: file:io_device(),
  timer_ref :: reference(),
  path :: string()
}).

append(Path, String) ->
  {ok, Pid} = get_worker(Path),
  gen_server:call(Pid, {append, String}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Path) ->
  gen_server:start_link(?MODULE, [Path], []).

init([Path]) ->
  {ok, IoDevice} = file:open(Path, [append]),
  State = #state{path = Path, io_device = IoDevice},
  error_logger:info_msg("File is opened ~p", [Path]),
  {ok, close_file_after_timeout(State)}.

handle_call({append, String}, _From, State = #state{io_device = IoDevice, path = Path}) ->
  ok = io:fwrite(IoDevice, "~s~n", [String]),
  error_logger:info_msg("Write line ~p to file ~p", [String, Path]),
  {reply, ok, close_file_after_timeout(State)};

handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

handle_info(close, State = #state{}) ->
  {stop, normal, State}.

terminate(_Reason, _State = #state{io_device = IoDevice, path = Path}) ->
  ok = file:close(IoDevice),
  true = ets:delete(?APP_NAME, Path),
  error_logger:info_msg("File is closed ~p", [IoDevice]),
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

close_file_after_timeout(#state{timer_ref = TimerRef} = State0) ->
  catch erlang:cancel_timer(TimerRef),
  State0#state{timer_ref = erlang:send_after(?TIMEOUT, self(), close)}.

get_worker(Path) ->
  case ets:lookup(?APP_NAME, Path) of
    [] ->
      {ok, Pid} = supervisor:start_child(file_appender_sup, [Path]),
      ets:insert(?APP_NAME, {Path, Pid}),
      {ok, Pid};
    [{Path, Pid}] ->
      {ok, Pid}
  end.