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
-type color_alias() :: red | yellow | green | blue | white.
-type led_state() :: 0..16#F.
-type led_state_alias() ::
    led_off | led_on | led_pattern1 | led_pattern2 | led_pattern3 | led_pattern4 | led_keep.
-type buz_pattern() :: 0..16#F.
-type buz_pattern_alias() ::
    buzz_off | buzz_on | buzz_pattern1 | buzz_pattern2 | buzz_pattern3 | buzz_pattern4 |
    buzzer_keep | buzzer_off.
-type nibble() :: 0..16#F.
-type optnl(A) :: {value, A} | none.

-spec color_alias(color_alias()) -> optnl(color()).
color_alias(red) -> {value, ?COLOR_RED};
color_alias(yellow) -> {value, ?COLOR_YELLOW};
color_alias(green) -> {value, ?COLOR_GREEN};
color_alias(blue) -> {value, ?COLOR_BLUE};
color_alias(white) -> {value, ?COLOR_WHITE};
color_alias(_) ->
    none.

-spec led_state_alias(led_state_alias()) -> optnl(led_state()).
led_state_alias(led_off) -> {value, ?LED_OFF};
led_state_alias(led_on) -> {value, ?LED_ON};
led_state_alias(led_pattern1) -> {value, ?LED_PATTERN1};
led_state_alias(led_pattern2) -> {value, ?LED_PATTERN2};
led_state_alias(led_pattern3) -> {value, ?LED_PATTERN3};
led_state_alias(led_pattern4) -> {value, ?LED_PATTERN4};
led_state_alias(led_keep) -> {value, ?LED_KEEP};
led_state_alias(_) ->
    none.

-spec buz_pattern_alias(buz_pattern_alias()) -> optnl(buz_pattern()).
buz_pattern_alias(buzz_off) -> {value, ?BUZZ_OFF};
buz_pattern_alias(buzz_on) -> {value, ?BUZZ_ON};
buz_pattern_alias(buzz_pattern1) -> {value, ?BUZZ_PATTERN1};
buz_pattern_alias(buzz_pattern2) -> {value, ?BUZZ_PATTERN2};
buz_pattern_alias(buzz_pattern3) -> {value, ?BUZZ_PATTERN3};
buz_pattern_alias(buzz_pattern4) -> {value, ?BUZZ_PATTERN4};
buz_pattern_alias(buzzer_keep) -> {value, ?BUZZER_KEEP};
buz_pattern_alias(buzzer_off) -> {value, ?BUZZER_OFF};
buz_pattern_alias(_) ->
    none.

init() ->
    Priv = code:priv_dir(?MODULE),
    So = filename:join(Priv, "ptlte_nif"),
    case erlang:load_nif(So, 0) of
        ok -> ok;
        {error, {reload, _}} -> ok;
        Error -> Error
    end.

%% @doc Set a single LED's state (color id, pattern nibble).
-spec set_light(color() | color_alias(), led_state() | led_state_alias()) ->
    ok | {error, term()}.
set_light(Color, State) when is_integer(Color), is_integer(State) ->
    case {validate_color(Color), validate_nibble(State, state)} of
        {ok, ok} ->
            {LedRy, LedGb, LedW} = assemble_leds(Color, State),
            send_report(build_report(?BUZZER_KEEP, 0, LedRy, LedGb, LedW));
        {Error, _} when Error =/= ok ->
            Error;
        {_, Error} ->
            Error
    end;
set_light(Alias, State) when is_atom(Alias) ->
    case color_alias(Alias) of
        {value, Color} ->
            set_light(Color, State);
        none ->
            {error, {unknown_color, Alias}}
    end;
set_light(Color, Alias) when is_atom(Alias) ->
    case led_state_alias(Alias) of
        {value, State} ->
            set_light(Color, State);
        none ->
            {error, {unknown_led_state, Alias}}
    end.

%% @doc Set all LEDs at once (patterns are nibbles).
-spec set_tower(
    led_state() | led_state_alias(),
    led_state() | led_state_alias(),
    led_state() | led_state_alias(),
    led_state() | led_state_alias(),
    led_state() | led_state_alias()
) ->
    ok | {error, term()}.
set_tower(Red, Yellow, Green, Blue, White)
    when is_integer(Red), is_integer(Yellow), is_integer(Green), is_integer(Blue), is_integer(White)
->
    case validate_nibbles([{red, Red}, {yellow, Yellow}, {green, Green}, {blue, Blue}, {white, White}]) of
        {ok, [R, Y, G, B, W]} ->
            LedRy = (R bsl 4) bor Y,
            LedGb = (G bsl 4) bor B,
            LedW = W bsl 4,
            send_report(build_report(?BUZZER_KEEP, 0, LedRy, LedGb, LedW));
        Error ->
            Error
    end;
set_tower(Alias, Yellow, Green, Blue, White) when is_atom(Alias) ->
    case led_state_alias(Alias) of
        {value, State} -> set_tower(State, Yellow, Green, Blue, White);
        none -> {error, {unknown_led_state, Alias}}
    end;
set_tower(Red, Alias, Green, Blue, White) when is_atom(Alias) ->
    case led_state_alias(Alias) of
        {value, State} -> set_tower(Red, State, Green, Blue, White);
        none -> {error, {unknown_led_state, Alias}}
    end;
set_tower(Red, Yellow, Alias, Blue, White) when is_atom(Alias) ->
    case led_state_alias(Alias) of
        {value, State} -> set_tower(Red, Yellow, State, Blue, White);
        none -> {error, {unknown_led_state, Alias}}
    end;
set_tower(Red, Yellow, Green, Alias, White) when is_atom(Alias) ->
    case led_state_alias(Alias) of
        {value, State} -> set_tower(Red, Yellow, Green, State, White);
        none -> {error, {unknown_led_state, Alias}}
    end;
set_tower(Red, Yellow, Green, Blue, Alias) when is_atom(Alias) ->
    case led_state_alias(Alias) of
        {value, State} -> set_tower(Red, Yellow, Green, Blue, State);
        none -> {error, {unknown_led_state, Alias}}
    end.

%% @doc Set buzzer pattern/limit with default pitches.
-spec set_buzzer(buz_pattern() | buz_pattern_alias(), nibble()) -> ok | {error, term()}.
set_buzzer(Pattern, Limit) when is_integer(Pattern), is_integer(Limit) ->
    case validate_nibbles([{pattern, Pattern}, {limit, Limit}]) of
        {ok, [P, L]} ->
            Buzzer = (L bsl 4) bor P,
            Pitch = (?BUZZER_PITCH_DEFAULT_A bsl 4) bor ?BUZZER_PITCH_DEFAULT_B,
            Keep = (?LED_KEEP bsl 4) bor ?LED_KEEP,
            send_report(build_report(Buzzer, Pitch, Keep, Keep, ?LED_KEEP bsl 4));
        Error ->
            Error
    end;
set_buzzer(Alias, Limit) when is_atom(Alias) ->
    case buz_pattern_alias(Alias) of
        {value, Pattern} ->
            set_buzzer(Pattern, Limit);
        none ->
            {error, {unknown_buzzer_pattern, Alias}}
    end.

%% @doc Set buzzer with explicit pitch A/B nibbles.
-spec set_buzzer(buz_pattern() | buz_pattern_alias(), nibble(), nibble(), nibble()) ->
    ok | {error, term()}.
set_buzzer(Pattern, Limit, PitchA, PitchB)
    when is_integer(Pattern), is_integer(Limit), is_integer(PitchA), is_integer(PitchB)
->
    case validate_nibbles([{pattern, Pattern}, {limit, Limit}, {pitch_a, PitchA}, {pitch_b, PitchB}]) of
        {ok, [P, L, A, B]} ->
            Buzzer = (L bsl 4) bor P,
            Pitch = (A bsl 4) bor B,
            Keep = (?LED_KEEP bsl 4) bor ?LED_KEEP,
            send_report(build_report(Buzzer, Pitch, Keep, Keep, ?LED_KEEP bsl 4));
        Error ->
            Error
    end;
set_buzzer(Alias, Limit, PitchA, PitchB) when is_atom(Alias) ->
    case buz_pattern_alias(Alias) of
        {value, Pattern} ->
            set_buzzer(Pattern, Limit, PitchA, PitchB);
        none ->
            {error, {unknown_buzzer_pattern, Alias}}
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
