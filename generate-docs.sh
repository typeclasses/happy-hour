#!/usr/bin/env bash
set -eu
stack build happy-hour
stack runhaskell happy-hour-docs/happy-hour-docs.hs
