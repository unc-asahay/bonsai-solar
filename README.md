# bonsai-solar

A solar calculator, in OCaml and Bonsai.

It answers the question a person with a roof actually asks: *if I put
panels here, how much do I get, and when do I get my money back?*

The arithmetic is not done in JavaScript. The solar geometry — where the
sun is, at what angle it strikes a tilted surface, how much of that
survives the atmosphere and the clouds of a real year — is solved in
OCaml, in a library with no browser in it, and the result is handed to
the page as a number.

## What it does

- **Sixteen cities**, and any latitude and longitude you care to type.
- **Roof tilt and direction**, from 0 to 90° and 0 to 360°, with the
  compass bearing spelled out.
- **Your array**: how many panels, how many watts each, how efficient.
- **Your bill**: your rate, your annual consumption, what you paid per
  watt to install.
- **The results**: annual production, a month-by-month chart, the sun's
  path across today's sky, savings, payback, lifetime savings, and the
  carbon avoided.

Every number updates as you move a control. There is no calculate
button, because there is nothing to wait for.

## The model

The geometry is the standard text-book one:

| Step | Method |
| --- | --- |
| Solar declination | Cooper's equation |
| Equation of time | Spencer's series |
| Sun's direction | east-north-up vector, from the hour angle |
| Incidence on a tilted surface | dot product with the panel's normal |
| Air mass | Kasten–Young |
| Clear-sky direct normal irradiance | Meinel |
| Sky diffuse | isotropic, 10% of global |
| Ground reflection | isotropic, 20% albedo |
| Integration | 10-minute steps, every day of the year |

The result is a **clear-sky** figure: what the roof would collect if
every day were a cloudless day. That is the right number for *comparing*
one roof against another, and it is deliberately not dressed up as
something it is not. In a real year you will collect less; the "system
losses" control is where you put that difference.

## How it is verified

`tools/gate.sh` runs everything. It is the same gate that runs on every
push.

| Check | What it proves |
| --- | --- |
| **30 native checks** | The model's own invariants: the solstice declination is 23.44°, the noon sun is due south in the northern hemisphere and due north in the southern, the best tilt at 39.7°N is near the latitude, south-facing beats north-facing, a tilted array beats a flat one, the annual extraterrestrial irradiation at the equator matches its closed form. |
| **Two implementations, compared** | The same physics written again in Python (`tools/reference.py`) has to agree with the OCaml on eight numbers, to 0.1 kWh/m². If the two implementations drift apart, the gate says so. |
| **19 browser checks** | The compiled app, in Chromium, against a third implementation written in JavaScript. The page's own numbers are read out of the DOM and compared: the headline, the twelve monthly bars and their sum, what happens when you change the city, the tilt, the direction. Plus: every control has a label bound to it, no console errors, and no horizontal overflow on a phone. |

The three implementations agreeing is the point. One implementation can
be consistently wrong; three rarely are.

## Layout

```
src/core/solar.ml    the model — no DOM, no Bonsai, natively testable
src/app/app.ml       the interface — Bonsai and virtual_dom
test/core/           the model's own checks, run natively
test/verify_app.mjs  the browser gate
tools/reference.py    the model again, in Python
tools/gate.sh        everything above, in one command
```

The split is deliberate. `src/core` links natively, so the physics can
be tested in milliseconds without a browser. `src/app` is the only part
that knows about the DOM.

## Building it

The Jane Street preview packages (`bonsai`, `bonsai_web`) are not on
opam.ocaml.org, so:

```sh
opam repository add jane-street https://ocaml.janestreet.com --set-default
opam switch create bonsai-ui 5.2.1 --packages=ocaml-base-compiler.5.2.1
opam install -y --switch=bonsai-ui \
  dune core bonsai bonsai_web bonsai_web_components ppx_html js_of_ocaml
npm install && npx playwright install chromium
./tools/gate.sh
```

Then:

```sh
opam exec --switch=bonsai-ui -- dune build
python3 -m http.server 8899
# and open http://127.0.0.1:8899/site/index.html
```
