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
const REPORT_LEN: usize = 8;

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

#[rustler::nif(schedule = "DirtyIo")]
fn nif_send<'a>(env: Env<'a>, report: Vec<u8>) -> NifResult<Term<'a>> {
    map_result(env, send_report(report))
}

fn send_report(report: Vec<u8>) -> ControlResult<()> {
    if report.len() != REPORT_LEN {
        return Err(ControlError::InvalidArg("report_length"));
    }

    let mut buf = [0u8; REPORT_LEN];
    buf.copy_from_slice(&report[..]);

    let context = rusb::Context::new()?;
    let handle = context
        .open_device_with_vid_pid(VENDOR_ID, PRODUCT_ID)
        .ok_or(ControlError::DeviceNotFound)?;

    let _ = handle.set_auto_detach_kernel_driver(true);
    handle.claim_interface(0)?;

    let timeout = Duration::from_millis(TIMEOUT_MS);
    let written = handle.write_interrupt(ENDPOINT_OUT, &buf, timeout)?;
    if written != buf.len() {
        return Err(ControlError::ShortWrite);
    }

    Ok(())
}

rustler::init!("ptlte", [nif_send], load = on_load);

fn on_load<'a>(_env: Env<'a>, _info: Term<'a>) -> bool {
    true
}
