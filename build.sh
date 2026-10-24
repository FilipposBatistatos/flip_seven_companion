#!/usr/bin/env bash
# Builds elm.js next to index.html. Needs Elm 0.19.1+ (https://guide.elm-lang.org/install/elm.html).
set -euo pipefail
cd "$(dirname "$0")"

elm make src/Main.elm --optimize --output=elm.js

# Optional: minify if you have terser (npm i -g terser)
if command -v terser >/dev/null 2>&1; then
  terser elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
    | terser --mangle --output elm.min.js
  mv elm.min.js elm.js
fi

echo "Done. Upload index.html and elm.js to your site."
