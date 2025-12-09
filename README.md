# ptlte

Erlang/OTP library that drives the Patlite LR6-USB tower (VID:PID `191a:8003`) via a Rust NIF. It sends the same HID reports as the vendor sample, but no Python is involved.

## Requirements

- Erlang/OTP (for this library)
- Rust toolchain (cargo/rustc)
- `libusb-1.0` available (system dev package recommended; otherwise the bundled libusb is built) and `pkg-config`
- Udev rule for access without sudo:
  - `/etc/udev/rules.d/99-patlite.rules` containing
    `SUBSYSTEM=="usb", ATTR{idVendor}=="191a", ATTR{idProduct}=="8003", MODE="0666"`
  - Reload rules and replug: `sudo udevadm control --reload && sudo udevadm trigger`

## Build

```bash
# from repo root
rebar3 compile
```

`rebar3 compile` calls cargo to build `native/ptlte_nif` and copies `priv/ptlte_nif.so`.

If you want to build manually or the hook is skipped:

```bash
cargo build --manifest-path native/ptlte_nif/Cargo.toml --release
mkdir -p priv
cp native/ptlte_nif/target/release/libptlte_nif.so priv/ptlte_nif.so
```

## Usage

```erlang
%% Red solid on
ptlte:set_light(0, 1).

%% Red + green solid
ptlte:set_tower(1, 0, 1, 0, 0).

%% Buzzer pattern 2, continuous (default pitches)
ptlte:set_buzzer(2, 0).

%% Buzzer pattern 2, timed (limit=3), explicit pitches (D7 then stop)
ptlte:set_buzzer(2, 3, 6, 0).

%% All off
ptlte:reset().

%% Raw send if you already built the 8-byte HID report (red solid example)
ptlte:send_report([0, 0, 15, 0, 31, 255, 240, 0]).
```

Return values: `ok` or `{error, device_not_found | {invalid_arg, Field} | {usb_error, String} | short_write}`.

## Parameter map

- Colors: `0` red, `1` yellow, `2` green, `3` blue, `4` white.
- LED states: `0` off, `1` solid, `2–5` built-in flash patterns, `15` keep current.
- Buzzer pattern: `0` off, `1` continuous, `2–5` pattern 1–4.
- Buzzer limit: `0` continuous, `1–15` timed.
- Pitches: nibbles `0–13` (A6..A7 per vendor table), `14` default A (D7), `15` default/stop B.
