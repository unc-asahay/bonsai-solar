#!/usr/bin/env bash
# The gate: everything that has to be true before this app is worth
# looking at, in one command.
#
#   tools/gate.sh
#
# It builds, runs the model's own tests natively, checks the model
# against an independent implementation of the same physics written in
# another language, and then checks the compiled app in a real browser
# against a third implementation.

set -uo pipefail
cd "$(dirname "$0")/.."

PORT="${SOLAR_PORT:-8899}"
BASE="http://127.0.0.1:${PORT}"
FAILED=0

ok()   { printf '  \033[32mok\033[0m    %s\n' "$1"; }
bad()  { printf '  \033[31mFAIL\033[0m  %s\n' "$1"; FAILED=$((FAILED + 1)); }
step() { printf '\n\033[1m%s\033[0m\n' "$1"; }

if [ -t 1 ]; then BOLD=$'\033[1m'; GREEN=$'\033[32m'; RED=$'\033[31m'; RESET=$'\033[0m'
else BOLD=""; GREEN=""; RED=""; RESET=""; fi

# ---------------------------------------------------------------- the build

step "the build"
if timeout 600 opam exec --switch=bonsai-ui -- dune build 2>&1 | tail -20; then
  ok "it builds"
else
  bad "it does not build"
  exit 1
fi

# ------------------------------------------------------- the model's tests

step "the model, tested natively"
# The core library has no DOM dependency, so this links and runs
# natively — no browser, no JavaScript.
TEST_OUT=$("./_build/default/test/core/solar_test.exe" 2>&1)
TEST_STATUS=$?
echo "$TEST_OUT" | grep -E '^(PASS|FAIL)' | sed 's/^/  /'
SUMMARY=$(echo "$TEST_OUT" | grep -E '^[0-9]+ checks' | tail -1)
if [ "$TEST_STATUS" -eq 0 ]; then
  ok "the model passes its own checks — $SUMMARY"
else
  bad "the model's own checks failed — $SUMMARY"
fi

# ------------------------------------------- against another implementation

step "the model, against an independent implementation"
# tools/reference.py is the same physics written again in Python. If the
# two disagree about a number, one of them is wrong, and the gate has
# to say so rather than pick a favourite.
OCAML_VALUES=$(echo "$TEST_OUT" | grep -E '^[a-z_]+ = ' | sort)
PYTHON_VALUES=$(python3 tools/reference.py 2>&1 | grep -E '^[a-z_]+ = ' | sort)
if [ -z "$PYTHON_VALUES" ]; then
  bad "the reference implementation produced nothing"
else
  DIFF=$(diff <(echo "$OCAML_VALUES") <(echo "$PYTHON_VALUES") || true)
  if [ -z "$DIFF" ]; then
    ok "the OCaml and the Python agree on every value"
    echo "$OCAML_VALUES" | sed 's/^/        /'
  else
    bad "the two implementations disagree"
    echo "$DIFF" | sed 's/^/        /'
  fi
fi

# ------------------------------------------------------- the app, in a browser

step "the app, in a real browser"
if [ ! -d node_modules/playwright ]; then
  bad "playwright is not installed — run: npm install && npx playwright install chromium"
else
  python3 -m http.server "$PORT" --bind 127.0.0.1 >/dev/null 2>&1 &
  SERVER_PID=$!
  trap 'kill $SERVER_PID 2>/dev/null' EXIT
  for _ in $(seq 1 40); do
    if curl -sf -o /dev/null "$BASE/site/index.html"; then break; fi
    sleep 0.25
  done
  if timeout 300 node test/verify_app.mjs "$BASE" 2>&1 | sed 's/^/  /'; then
    ok "the app passes the browser gate"
  else
    bad "the app fails the browser gate"
  fi
  kill $SERVER_PID 2>/dev/null
  trap - EXIT
fi

printf '\n'
if [ "$FAILED" -eq 0 ]; then
  printf '%sGATE GREEN%s\n' "$GREEN" "$RESET"
  exit 0
else
  printf '%sGATE RED — %d failure(s)%s\n' "$RED" "$FAILED" "$RESET"
  exit 1
fi
