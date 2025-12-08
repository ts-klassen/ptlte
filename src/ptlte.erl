-module(ptlte).

-on_load(init/0).

-export([
    set_light/2,
    set_tower/5,
    set_buzzer/2,
    set_buzzer/4,
    reset/0
]).

-define(NIF_ERROR, not_loaded).

-define(COLOR_RED, 0).
-define(COLOR_YELLOW, 1).
-define(COLOR_GREEN, 2).
-define(COLOR_BLUE, 3).
-define(COLOR_WHITE, 4).

-define(LED_OFF, 16#0).
-define(LED_ON, 16#1).
-define(LED_PATTERN1, 16#2).
-define(LED_PATTERN2, 16#3).
-define(LED_PATTERN3, 16#4).
-define(LED_PATTERN4, 16#5).
-define(LED_KEEP, 16#F).

-define(BUZZ_OFF, 16#0).
-define(BUZZ_ON, 16#1).
-define(BUZZ_PATTERN1, 16#2).
-define(BUZZ_PATTERN2, 16#3).
-define(BUZZ_PATTERN3, 16#4).
-define(BUZZ_PATTERN4, 16#5).

-type color() :: 0..4.
-type led_state() :: 0..16#F.
-type buz_pattern() :: 0..16#F.
-type nibble() :: 0..16#F.

init() ->
    Priv = code:priv_dir(?MODULE),
    So = filename:join(Priv, "ptlte_nif"),
    case erlang:load_nif(So, 0) of
        ok -> ok;
        {error, {reload, _}} -> ok;
        Error -> Error
    end.

%% @doc Set a single LED's state (color id, pattern nibble).
-spec set_light(color(), led_state()) -> ok | {error, term()}.
set_light(Color, State) ->
    nif_set_light(Color, State).

%% @doc Set all LEDs at once (patterns are nibbles).
-spec set_tower(led_state(), led_state(), led_state(), led_state(), led_state()) ->
    ok | {error, term()}.
set_tower(Red, Yellow, Green, Blue, White) ->
    nif_set_tower(Red, Yellow, Green, Blue, White).

%% @doc Set buzzer pattern/limit with default pitches.
-spec set_buzzer(buz_pattern(), nibble()) -> ok | {error, term()}.
set_buzzer(Pattern, Limit) ->
    nif_set_buzzer(Pattern, Limit).

%% @doc Set buzzer with explicit pitch A/B nibbles.
-spec set_buzzer(buz_pattern(), nibble(), nibble(), nibble()) -> ok | {error, term()}.
set_buzzer(Pattern, Limit, PitchA, PitchB) ->
    nif_set_buzzer_ex(Pattern, Limit, PitchA, PitchB).

%% @doc Turn off all LEDs and buzzer.
-spec reset() -> ok | {error, term()}.
reset() ->
    nif_reset().

%% NIFs (filled in by native library)
nif_set_light(_Color, _State) ->
    erlang:nif_error(?NIF_ERROR).

nif_set_tower(_R, _Y, _G, _B, _W) ->
    erlang:nif_error(?NIF_ERROR).

nif_set_buzzer(_Pattern, _Limit) ->
    erlang:nif_error(?NIF_ERROR).

nif_set_buzzer_ex(_Pattern, _Limit, _PitchA, _PitchB) ->
    erlang:nif_error(?NIF_ERROR).

nif_reset() ->
    erlang:nif_error(?NIF_ERROR).
