-module(gen_interval_test).

-include_lib("eunit/include/eunit.hrl").
-include("include/gen_interval.hrl").

start_link_test() ->
    CM = callback_module,
    meck:new(CM),
    meck:expect(CM, interval, fun() -> 1000 end),
    {ok, _Pid} = gen_interval:start_link(CM),
    ?assertEqual(true, meck:called(CM, interval, [])),
    ?assert(meck:validate(CM)),
    meck:unload(CM).

init_test() ->
    CM = callback_module,
    meck:new(CM),
    meck:expect(CM, interval, fun() -> 1000 end),
    {ok, State} = gen_interval:init(CM),
    ?assertEqual(true, meck:called(CM, interval, [])),
    ?assertEqual(1000, State#gen_interval_state.interval),
    ?assertEqual(1000, State#gen_interval_state.remaining),
    ?assert(meck:validate(CM)),
    meck:unload(CM).

handle_cast_interval_test() ->
    CM = callback_module,
    meck:new(CM),
    meck:expect(CM, handle_interval, fun() -> ok end),
    State = #gen_interval_state{
            cm=CM,
            remaining=1000,
            interval=1000
            },
    {noreply, NewState} = gen_interval:handle_cast(interval,
                                                  State),
    ?assertEqual(true, meck:called(CM, handle_interval, [])),
    ?assertNotEqual(undefined, NewState#gen_interval_state.tref),
    % As we just have checked tref we reset it to undefined to check all the
    % otehr fields.
    ?assertEqual(State, NewState#gen_interval_state{tref=undefined}),
    ?assert(meck:validate(CM)),
    meck:unload(CM).

start_timeout_large_interval_test() ->
    CM = callback_module,
    meck:new(CM),
    meck:expect(CM, interval, fun() -> 4294967295 + 1000 end),
    {ok, NewState} = gen_interval:init(CM),
    ?assert(meck:called(CM, interval, [])),
    ?assertEqual(1000, NewState#gen_interval_state.remaining),
    ?assertEqual(4294967295 + 1000, NewState#gen_interval_state.interval),
    %TODO find a better way to test the timeref
    ?assertNotEqual(undefined, NewState#gen_interval_state.tref),
    ?assert(meck:validate(CM)),
    meck:unload(CM).

start_timeout_very_large_interval_test() ->
    CM = callback_module,
    meck:new(CM),
    meck:expect(CM, interval, fun() -> 4294967295 + 4294967295 + 1000 end),
    {ok, NewState} = gen_interval:init(CM),
    ?assert(meck:called(CM, interval, [])),
    ?assertEqual(4294967295 + 1000, NewState#gen_interval_state.remaining),
    ?assertEqual(4294967295 + 4294967295 + 1000,
                 NewState#gen_interval_state.interval),
    %TODO find a better way to test the timeref
    ?assertNotEqual(undefined, NewState#gen_interval_state.tref),
    ?assert(meck:validate(CM)),
    meck:unload(CM).

handle_call_undefined_test() ->
    ?assertEqual({noreply, state}, gen_interval:handle_call(msg, from, state)).

handle_cast_undefined_test() ->
    ?assertEqual({noreply, state}, gen_interval:handle_cast(msg, state)).

handle_info_undefined_test() ->
    ?assertEqual({noreply, state}, gen_interval:handle_info(msg, state)).

terminate_test() ->
    ?assertEqual(ok, gen_interval:terminate(reason, state)).

code_change_test() ->
    ?assertEqual({ok, state}, gen_interval:code_change(oldvsn, state, extra)).
