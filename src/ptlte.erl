-module(ptlte).

-on_load(init/0).

-export([
    set_light/2,
    set_tower/5,
    set_buzzer/2,
    set_buzzer/4,
    reset/0,
    send_report/1
]).

-define(NIF_ERROR, not_loaded).

-define(REPORT_LEN, 8).
-define(COMMAND_VERSION, 16#00).
-define(COMMAND_ID, 16#00).

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
-define(BUZZER_KEEP, 16#F).
-define(BUZZER_OFF, 16#0).

-define(BUZZER_PITCH_DEFAULT_A, 16#0E).
-define(BUZZER_PITCH_DEFAULT_B, 16#0F).
-define(PITCH_OFF, 16#0).

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
    case {validate_color(Color), validate_nibble(State, state)} of
        {ok, ok} ->
            {LedRy, LedGb, LedW} = assemble_leds(Color, State),
            send_report(build_report(?BUZZER_KEEP, 0, LedRy, LedGb, LedW));
        {Error, _} when Error =/= ok ->
            Error;
        {_, Error} ->
            Error
    end.

%% @doc Set all LEDs at once (patterns are nibbles).
-spec set_tower(led_state(), led_state(), led_state(), led_state(), led_state()) ->
    ok | {error, term()}.
set_tower(Red, Yellow, Green, Blue, White) ->
    case validate_nibbles([{red, Red}, {yellow, Yellow}, {green, Green}, {blue, Blue}, {white, White}]) of
        {ok, [R, Y, G, B, W]} ->
            LedRy = (R bsl 4) bor Y,
            LedGb = (G bsl 4) bor B,
            LedW = W bsl 4,
            send_report(build_report(?BUZZER_KEEP, 0, LedRy, LedGb, LedW));
        Error ->
            Error
    end.

%% @doc Set buzzer pattern/limit with default pitches.
-spec set_buzzer(buz_pattern(), nibble()) -> ok | {error, term()}.
set_buzzer(Pattern, Limit) ->
    case validate_nibbles([{pattern, Pattern}, {limit, Limit}]) of
        {ok, [P, L]} ->
            Buzzer = (L bsl 4) bor P,
            Pitch = (?BUZZER_PITCH_DEFAULT_A bsl 4) bor ?BUZZER_PITCH_DEFAULT_B,
            Keep = (?LED_KEEP bsl 4) bor ?LED_KEEP,
            send_report(build_report(Buzzer, Pitch, Keep, Keep, ?LED_KEEP bsl 4));
        Error ->
            Error
    end.

%% @doc Set buzzer with explicit pitch A/B nibbles.
-spec set_buzzer(buz_pattern(), nibble(), nibble(), nibble()) -> ok | {error, term()}.
set_buzzer(Pattern, Limit, PitchA, PitchB) ->
    case validate_nibbles([{pattern, Pattern}, {limit, Limit}, {pitch_a, PitchA}, {pitch_b, PitchB}]) of
        {ok, [P, L, A, B]} ->
            Buzzer = (L bsl 4) bor P,
            Pitch = (A bsl 4) bor B,
            Keep = (?LED_KEEP bsl 4) bor ?LED_KEEP,
            send_report(build_report(Buzzer, Pitch, Keep, Keep, ?LED_KEEP bsl 4));
        Error ->
            Error
    end.

%% @doc Turn off all LEDs and buzzer.
-spec reset() -> ok | {error, term()}.
reset() ->
    send_report(build_report(?BUZZER_OFF, ?PITCH_OFF, ?LED_OFF, ?LED_OFF, ?LED_OFF)).

%% @doc Send a pre-built HID report to the device (8 bytes).
-spec send_report([byte()]) -> ok | {error, term()}.
send_report(Report) ->
    case validate_report(Report) of
        {ok, CleanReport} ->
            nif_send(CleanReport);
        Error ->
            Error
    end.

assemble_leds(?COLOR_RED, State) ->
    Keep = (?LED_KEEP bsl 4) bor ?LED_KEEP,
    {(State bsl 4) bor ?LED_KEEP, Keep, ?LED_KEEP bsl 4};
assemble_leds(?COLOR_YELLOW, State) ->
    Keep = (?LED_KEEP bsl 4) bor ?LED_KEEP,
    {(?LED_KEEP bsl 4) bor State, Keep, ?LED_KEEP bsl 4};
assemble_leds(?COLOR_GREEN, State) ->
    Keep = (?LED_KEEP bsl 4) bor ?LED_KEEP,
    {Keep, (State bsl 4) bor ?LED_KEEP, ?LED_KEEP bsl 4};
assemble_leds(?COLOR_BLUE, State) ->
    Keep = (?LED_KEEP bsl 4) bor ?LED_KEEP,
    {Keep, (?LED_KEEP bsl 4) bor State, ?LED_KEEP bsl 4};
assemble_leds(?COLOR_WHITE, State) ->
    Keep = (?LED_KEEP bsl 4) bor ?LED_KEEP,
    {Keep, Keep, State bsl 4}.

build_report(Buzzer, Pitch, LedRy, LedGb, LedW) ->
    [
        ?COMMAND_VERSION,
        ?COMMAND_ID,
        Buzzer,
        Pitch,
        LedRy,
        LedGb,
        LedW,
        0
    ].

validate_color(Color) when is_integer(Color), Color >= ?COLOR_RED, Color =< ?COLOR_WHITE ->
    ok;
validate_color(_) ->
    invalid_arg(color).

validate_nibbles(Pairs) ->
    validate_nibbles(Pairs, []).

validate_nibbles([{Field, Value} | Rest], Acc) ->
    case validate_nibble(Value, Field) of
        ok ->
            validate_nibbles(Rest, [Value | Acc]);
        Error ->
            Error
    end;
validate_nibbles([], Acc) ->
    {ok, lists:reverse(Acc)}.

validate_nibble(Value, _Field) when is_integer(Value), Value >= 0, Value =< 16#F ->
    ok;
validate_nibble(_Value, Field) ->
    invalid_arg(Field).

validate_report(Report) when is_list(Report) ->
    case length(Report) of
        ?REPORT_LEN ->
            case lists:all(fun is_byte/1, Report) of
                true -> {ok, Report};
                false -> invalid_arg(report_value)
            end;
        _ ->
            invalid_arg(report_length)
    end;
validate_report(_) ->
    invalid_arg(report).

is_byte(V) when is_integer(V), V >= 0, V =< 16#FF ->
    true;
is_byte(_) ->
    false.

invalid_arg(Field) ->
    {error, {invalid_arg, Field}}.

%% NIFs (filled in by native library)
nif_send(_Report) ->
    erlang:nif_error(?NIF_ERROR).
