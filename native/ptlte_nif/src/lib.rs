use rusb::UsbContext;
use rustler::{Encoder, Env, NifResult, Term};
use std::time::Duration;

mod atoms {
    rustler::atoms! {
        ok,
        error,
        device_not_found,
        invalid_arg,
        usb_error,
        short_write
    }
}

const VENDOR_ID: u16 = 0x191a;
const PRODUCT_ID: u16 = 0x8003;
const ENDPOINT_OUT: u8 = 0x01;
const TIMEOUT_MS: u64 = 1000;

const COMMAND_VERSION: u8 = 0x00;
const COMMAND_ID: u8 = 0x00;

const LED_KEEP: u8 = 0x0f;
const LED_OFF: u8 = 0x00;

const BUZZER_KEEP: u8 = 0x0f;
const BUZZER_OFF: u8 = 0x00;

const BUZZER_PITCH_DEFAULT_A: u8 = 0x0e;
const BUZZER_PITCH_DEFAULT_B: u8 = 0x0f;
const PITCH_OFF: u8 = 0x00;

#[derive(Debug)]
enum ControlError {
    DeviceNotFound,
    InvalidArg(&'static str),
    Usb(rusb::Error),
    ShortWrite,
}

impl From<rusb::Error> for ControlError {
    fn from(err: rusb::Error) -> Self {
        ControlError::Usb(err)
    }
}

type ControlResult<T> = Result<T, ControlError>;

fn map_result<'a>(env: Env<'a>, result: ControlResult<()>) -> NifResult<Term<'a>> {
    let term = match result {
        Ok(()) => atoms::ok().encode(env),
        Err(ControlError::DeviceNotFound) => (atoms::error(), atoms::device_not_found()).encode(env),
        Err(ControlError::InvalidArg(field)) => {
            (atoms::error(), (atoms::invalid_arg(), field)).encode(env)
        }
        Err(ControlError::ShortWrite) => (atoms::error(), atoms::short_write()).encode(env),
        Err(ControlError::Usb(err)) => (atoms::error(), (atoms::usb_error(), format!("{err}"))).encode(env),
    };

    Ok(term)
}

#[rustler::nif]
fn nif_set_light<'a>(env: Env<'a>, color: u8, state: u8) -> NifResult<Term<'a>> {
    map_result(env, control::set_light(color, state))
}

#[rustler::nif]
fn nif_set_tower<'a>(env: Env<'a>, red: u8, yellow: u8, green: u8, blue: u8, white: u8) -> NifResult<Term<'a>> {
    map_result(env, control::set_tower(red, yellow, green, blue, white))
}

#[rustler::nif]
fn nif_set_buzzer<'a>(env: Env<'a>, pattern: u8, limit: u8) -> NifResult<Term<'a>> {
    map_result(env, control::set_buzzer(pattern, limit))
}

#[rustler::nif]
fn nif_set_buzzer_ex<'a>(env: Env<'a>, pattern: u8, limit: u8, pitch_a: u8, pitch_b: u8) -> NifResult<Term<'a>> {
    map_result(env, control::set_buzzer_ex(pattern, limit, pitch_a, pitch_b))
}

#[rustler::nif]
fn nif_reset<'a>(env: Env<'a>) -> NifResult<Term<'a>> {
    map_result(env, control::reset())
}

mod control {
    use super::*;

    pub fn set_light(color: u8, state: u8) -> ControlResult<()> {
        let color = validate_color(color)?;
        let state = nibble(state, "state")?;

        let mut led_ry = (LED_KEEP << 4) | LED_KEEP;
        let mut led_gb = led_ry;
        let mut led_w = LED_KEEP << 4;

        match color {
            0 => {
                led_ry = (state << 4) | LED_KEEP;
            }
            1 => {
                led_ry = (LED_KEEP << 4) | state;
            }
            2 => {
                led_gb = (state << 4) | LED_KEEP;
            }
            3 => {
                led_gb = (LED_KEEP << 4) | state;
            }
            4 => {
                led_w = state << 4;
            }
            _ => unreachable!(),
        }

        let report = build_report(BUZZER_KEEP, 0, led_ry, led_gb, led_w);
        send_report(report)
    }

    pub fn set_tower(red: u8, yellow: u8, green: u8, blue: u8, white: u8) -> ControlResult<()> {
        let red = nibble(red, "red")?;
        let yellow = nibble(yellow, "yellow")?;
        let green = nibble(green, "green")?;
        let blue = nibble(blue, "blue")?;
        let white = nibble(white, "white")?;

        let led_ry = (red << 4) | yellow;
        let led_gb = (green << 4) | blue;
        let led_w = white << 4;

        let report = build_report(BUZZER_KEEP, 0, led_ry, led_gb, led_w);
        send_report(report)
    }

    pub fn set_buzzer(pattern: u8, limit: u8) -> ControlResult<()> {
        let pattern = nibble(pattern, "pattern")?;
        let limit = nibble(limit, "limit")?;

        let buzzer = (limit << 4) | pattern;
        let pitch = (BUZZER_PITCH_DEFAULT_A << 4) | BUZZER_PITCH_DEFAULT_B;

        let keep = (LED_KEEP << 4) | LED_KEEP;
        let report = build_report(buzzer, pitch, keep, keep, LED_KEEP << 4);
        send_report(report)
    }

    pub fn set_buzzer_ex(pattern: u8, limit: u8, pitch_a: u8, pitch_b: u8) -> ControlResult<()> {
        let pattern = nibble(pattern, "pattern")?;
        let limit = nibble(limit, "limit")?;
        let pitch_a = nibble(pitch_a, "pitch_a")?;
        let pitch_b = nibble(pitch_b, "pitch_b")?;

        let buzzer = (limit << 4) | pattern;
        let pitch = (pitch_a << 4) | pitch_b;

        let keep = (LED_KEEP << 4) | LED_KEEP;
        let report = build_report(buzzer, pitch, keep, keep, LED_KEEP << 4);
        send_report(report)
    }

    pub fn reset() -> ControlResult<()> {
        let report = build_report(BUZZER_OFF, PITCH_OFF, LED_OFF, LED_OFF, LED_OFF);
        send_report(report)
    }

    fn nibble(val: u8, field: &'static str) -> ControlResult<u8> {
        if val <= 0x0f {
            Ok(val)
        } else {
            Err(ControlError::InvalidArg(field))
        }
    }

    fn validate_color(color: u8) -> ControlResult<u8> {
        if color <= 4 {
            Ok(color)
        } else {
            Err(ControlError::InvalidArg("color"))
        }
    }

    fn build_report(buzzer: u8, pitch: u8, led_ry: u8, led_gb: u8, led_w: u8) -> [u8; 8] {
        [
            COMMAND_VERSION,
            COMMAND_ID,
            buzzer,
            pitch,
            led_ry,
            led_gb,
            led_w,
            0x00,
        ]
    }

    fn send_report(report: [u8; 8]) -> ControlResult<()> {
        let context = rusb::Context::new()?;
        let handle = context
            .open_device_with_vid_pid(VENDOR_ID, PRODUCT_ID)
            .ok_or(ControlError::DeviceNotFound)?;

        let _ = handle.set_auto_detach_kernel_driver(true);
        handle.claim_interface(0)?;

        let timeout = Duration::from_millis(TIMEOUT_MS);
        let written = handle.write_interrupt(ENDPOINT_OUT, &report, timeout)?;
        if written != report.len() {
            return Err(ControlError::ShortWrite);
        }

        Ok(())
    }
}

rustler::init!(
    "ptlte",
    [nif_set_light, nif_set_tower, nif_set_buzzer, nif_set_buzzer_ex, nif_reset],
    load = on_load
);

fn on_load<'a>(_env: Env<'a>, _info: Term<'a>) -> bool {
    true
}
