#!/usr/bin/env bash
set -euo pipefail

crate_dir="$(cd "$(dirname "$0")" && pwd -P)"
repo_root="$(cd "${crate_dir}/../.." && pwd -P)"

cargo build --manifest-path "${crate_dir}/Cargo.toml" --release

mkdir -p "${repo_root}/priv"
cp -f "${crate_dir}/target/release/libptlte_nif.so" "${repo_root}/priv/ptlte_nif.so"
