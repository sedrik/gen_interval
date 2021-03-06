%%%-------------------------------------------------------------------
%%% @doc
%%%  General abstraction for running code at a specified interval
%%% @end
%%%-------------------------------------------------------------------
-module(gen_interval).
-behaviour(gen_server).

-include("include/gen_interval.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, start_timeout/1, interval/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Callback definition
%% ------------------------------------------------------------------

-callback interval() ->
    Interval :: integer().
-callback handle_interval() ->
    ok.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(CM) ->
    gen_server:start_link(?MODULE, CM, []).

interval(Pid) ->
    gen_server:cast(Pid, interval).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(CM) ->
    Interval = CM:interval(),
    Remaining = Interval,
    State = #gen_interval_state{cm=CM, self=self(), interval=Interval,
                                remaining=Remaining},
    {ok, start_timeout(State)}.

handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast(interval, State) ->
    (State#gen_interval_state.cm):handle_interval(),
    Interval = State#gen_interval_state.interval,
    NewState = State#gen_interval_state{remaining=Interval},
    {noreply, start_timeout(NewState)};
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_timeout(#gen_interval_state{remaining=Remaining, self=Self}=State) when
        Remaining < 4294967295 ->
    Ref = timer:apply_after(Remaining, ?MODULE, interval, [Self]),
    State#gen_interval_state{tref=Ref};
start_timeout(#gen_interval_state{remaining=Remaining} = State) ->
    Time = Remaining - 4294967295,
    TimerState = State#gen_interval_state{remaining = Time},
    TRef = timer:apply_after(Time, ?MODULE, start_timeout, [TimerState]),
    TimerState#gen_interval_state{remaining=Time, tref=TRef}.
