// The browser gate.
//
// This is not a screenshot check. The app's whole point is that the
// numbers are right, so the gate measures the numbers the page actually
// renders and checks them against the model computed independently in
// JavaScript.
//
//   node test/verify_app.mjs [base-url]

import { chromium } from "playwright";

const BASE = process.argv[2] ?? "http://127.0.0.1:8899";

let passed = 0;
let failed = 0;

function check(name, ok, detail = "") {
  if (ok) {
    passed++;
    console.log(`PASS  ${name}${detail ? "  — " + detail : ""}`);
  } else {
    failed++;
    console.log(`FAIL  ${name}${detail ? "  — " + detail : ""}`);
  }
}

// -- the same model, independently, in JavaScript ----------------------------

const GSC = 1367.0;
const DIFFUSE_FRACTION = 0.10;
const ALBEDO = 0.2;
const rad = (d) => (d * Math.PI) / 180;

function declination(day) {
  return rad(23.45 * Math.sin(rad((360 * (day + 284)) / 365)));
}

function equationOfTime(day) {
  const b = rad((360 * (day - 81)) / 365);
  return 9.87 * Math.sin(2 * b) - 7.53 * Math.cos(b) - 1.5 * Math.sin(b);
}

function sunVector(lat, lon, tz, day, minutes) {
  const phi = rad(lat);
  const d = declination(day);
  const solar = minutes + 4 * (15 * tz - lon) + equationOfTime(day);
  const omega = rad(15 * (solar / 60 - 12));
  return {
    east: -Math.cos(d) * Math.sin(omega),
    north: Math.sin(d) * Math.cos(phi) - Math.cos(d) * Math.sin(phi) * Math.cos(omega),
    up: Math.sin(phi) * Math.sin(d) + Math.cos(phi) * Math.cos(d) * Math.cos(omega),
  };
}

function airMass(elevationRad) {
  const z = 90 - (elevationRad * 180) / Math.PI;
  if (z >= 90) return Infinity;
  return 1 / (Math.cos(rad(z)) + 0.50572 * Math.pow(96.07995 - z, -1.6364));
}

function extraterrestrial(day) {
  return GSC * (1 + 0.033 * Math.cos((2 * Math.PI * day) / 365));
}

function planeOfArray(lat, lon, tz, day, minutes, tilt, azimuth) {
  const s = sunVector(lat, lon, tz, day, minutes);
  const elevation = Math.asin(Math.max(-1, Math.min(1, s.up)));
  const am = airMass(elevation);
  if (!isFinite(am)) return 0;
  const dni = extraterrestrial(day) * Math.pow(0.7, Math.pow(am, 0.678));
  if (dni <= 0) return 0;
  const cosZ = Math.cos(Math.PI / 2 - elevation);
  const ghi = (dni * cosZ) / (1 - DIFFUSE_FRACTION);
  const dhi = ghi * DIFFUSE_FRACTION;
  const b = rad(tilt);
  const g = rad(azimuth);
  const ne = Math.sin(b) * Math.sin(g);
  const nn = Math.sin(b) * Math.cos(g);
  const nu = Math.cos(b);
  const cosIncidence = s.east * ne + s.north * nn + s.up * nu;
  const direct = cosIncidence > 0 ? dni * cosIncidence : 0;
  const diffuse = dhi * ((1 + Math.cos(b)) / 2);
  const reflected = ghi * ALBEDO * ((1 - Math.cos(b)) / 2);
  return direct + diffuse + reflected;
}

const DAYS_IN_MONTH = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

function annualEnergy(lat, lon, tz, tilt, azimuth) {
  let total = 0;
  let doy = 0;
  for (const d of DAYS_IN_MONTH) {
    for (let day = 1; day <= d; day++) {
      for (let m = 0; m < 1440; m += 10) {
        total += planeOfArray(lat, lon, tz, doy + day, m, tilt, azimuth) / 6;
      }
    }
    doy += d;
  }
  return total / 1000;
}

function acEnergy(lat, lon, tz, tilt, azimuth, watts, count, efficiency, losses) {
  const poa = annualEnergy(lat, lon, tz, tilt, azimuth);
  const area = (watts * count) / (efficiency * 1000);
  return poa * area * efficiency * (1 - losses);
}

// -- the page ----------------------------------------------------------------

const browser = await chromium.launch();
const page = await browser.newPage();
const consoleErrors = [];
page.on("console", (m) => {
  if (m.type() === "error") consoleErrors.push(m.text());
});
page.on("pageerror", (e) => consoleErrors.push(String(e)));

await page.goto(`${BASE}/site/index.html`, { waitUntil: "networkidle" });
await page.waitForSelector(".headline-value", { timeout: 20000 });

check("the page mounted", (await page.locator(".headline-value").count()) === 1);
check("there is a control panel", (await page.locator(".panel.controls").count()) === 1);
check("there is a results panel", (await page.locator(".panel.results").count()) === 1);

const headline = async () => (await page.locator(".headline-value").innerText()).replace(/,/g, "");
const acNow = Number(await headline());

// The default state: Denver, 39 degrees of tilt, facing south, ten 400 W
// panels at 20% efficiency, 14% losses.
const expected = acEnergy(39.7392, -104.9903, -7, 39, 180, 400, 10, 0.2, 0.14);
check(
  "the headline is the annual AC energy the model predicts",
  Math.abs(acNow - expected) < Math.max(2, expected * 0.002),
  `page ${acNow} vs model ${expected.toFixed(1)}`
);

// The monthly chart must have a bar for every month, and the sum of the
// months must be the annual figure.
const bars = await page.locator('[data-chart="monthly"] .bar').count();
check("the monthly chart has a bar for each of the twelve months", bars === 12, `${bars} bars`);

const energies = await page.$$eval('[data-chart="monthly"] .bar', (els) =>
  els.map((e) => Number(e.getAttribute("data-energy")))
);
const monthSum = energies.reduce((a, b) => a + b, 0);
const poaExpected = annualEnergy(39.7392, -104.9903, -7, 39, 180);
check(
  "the months sum to the annual sunlight on the panels",
  Math.abs(monthSum - poaExpected) < Math.max(2, poaExpected * 0.002),
  `${monthSum.toFixed(1)} vs ${poaExpected.toFixed(1)} kWh/m²`
);

const summer = energies[5];
const winter = energies[11];
check("June collects more than December", summer > winter, `${summer} vs ${winter}`);

check(
  "the sun path has an arc",
  (await page.locator('[data-chart="sun-path"] .sun-arc').count()) === 1
);

// -- the controls actually drive the numbers ----------------------------------

await page.selectOption("#location", "Miami, FL");
await page.waitForFunction(
  (before) => document.querySelector(".headline-value").innerText.replace(/,/g, "") !== before,
  String(acNow),
  { timeout: 10000 }
);
const miami = Number(await headline());
const miamiExpected = acEnergy(25.7617, -80.1918, -5, 39, 180, 400, 10, 0.2, 0.14);
check(
  "choosing Miami changes the output to Miami's",
  Math.abs(miami - miamiExpected) < Math.max(2, miamiExpected * 0.002),
  `page ${miami} vs model ${miamiExpected.toFixed(1)}`
);
check("Miami is sunnier than Denver at the same tilt", miami > acNow, `${miami} vs ${acNow}`);

// Turning the panels away from the sun must cost energy.
const tilt = page.locator("#s-Roof-tilt");
await tilt.fill("0");
await tilt.dispatchEvent("input");
await page.waitForTimeout(300);
const flat = Number(await headline());
check("flattening the array loses energy", flat < miami, `${flat} vs ${miami}`);
const flatExpected = acEnergy(25.7617, -80.1918, -5, 0, 180, 400, 10, 0.2, 0.14);
check(
  "the flattened array matches the model",
  Math.abs(flat - flatExpected) < Math.max(2, flatExpected * 0.002),
  `page ${flat} vs model ${flatExpected.toFixed(1)}`
);

// And the direction matters: south-facing must beat north-facing in the
// northern hemisphere. The array has to be tilted again first — a flat
// panel's direction cannot matter, because its normal points straight
// up whichever way you call it.
await tilt.fill("39");
await tilt.dispatchEvent("input");
await page.waitForTimeout(300);
const tilted = Number(await headline());
check("tilting it back wins the energy back", tilted > flat, `${tilted} vs ${flat}`);

const direction = page.locator("#s-Direction");
await direction.fill("0");
await direction.dispatchEvent("input");
await page.waitForTimeout(300);
const north = Number(await headline());
check("facing north loses energy against facing south", north < tilted, `${north} vs ${tilted}`);

await direction.fill("180");
await direction.dispatchEvent("input");
await page.waitForTimeout(300);
const south = Number(await headline());
check("facing south wins it back", Math.abs(south - tilted) < 2, `${south} vs ${tilted}`);

// -- the page is in order ----------------------------------------------------

const headings = await page.$$eval(".readout-label", (els) => els.map((e) => e.innerText));
check(
  "every headline number has a label",
  headings.length === 4 && headings.every((h) => h.length > 3),
  headings.join(", ")
);

const labelled = await page.$$eval(".field", (fields) =>
  fields.every((f) => {
    const label = f.querySelector("label");
    const input = f.querySelector(".input");
    return !!label && !!input && label.getAttribute("for") === input.getAttribute("id");
  })
);
check("every control has a label bound to it", labelled);

check("no console errors", consoleErrors.length === 0, consoleErrors.join(" | "));

// -- and it survives being looked at on a phone -------------------------------

const phone = await browser.newPage({ viewport: { width: 390, height: 844 } });
await phone.goto(`${BASE}/site/index.html`, { waitUntil: "networkidle" });
await phone.waitForSelector(".headline-value", { timeout: 20000 });
const overflow = await phone.evaluate(
  () => document.documentElement.scrollWidth - document.documentElement.clientWidth
);
check("no horizontal overflow on a phone", overflow <= 1, `${overflow}px`);
await phone.close();

await browser.close();

console.log(`\n${passed + failed} checks, ${failed} failure(s)`);
process.exit(failed === 0 ? 0 : 1);
