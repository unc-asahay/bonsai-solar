(* The solar model.

   Pure OCaml: no DOM, no Bonsai, no js_of_ocaml. Everything the app
   displays is computed here, and everything here is testable natively.

   The geometry is the standard textbook treatment of solar position on
   an arbitrary tilted plane, written in vector form rather than as the
   usual tangle of trigonometric identities — the vector form is shorter,
   and it is much harder to get the sign of a term wrong.

   The irradiance model is CLEAR-SKY: the sun that would shine if there
   were no clouds. That is what a siting calculation wants — it answers
   "how much sun does this roof get", which is a property of the
   geometry and not of this year's weather. What a system actually
   produces depends on the weather, and the app says so rather than
   pretending otherwise. *)

open! Core

(* ------------------------------------------------------------------ types *)

type location =
  { name : string
  ; latitude : float (* degrees north *)
  ; longitude : float (* degrees east *)
  ; utc_offset : float (* hours east of Greenwich *)
  }

type array_spec =
  { tilt : float (* degrees from horizontal *)
  ; azimuth : float (* degrees clockwise from north; 180 is south *)
  ; panel_watts : float
  ; panel_count : int
  ; module_efficiency : float (* 0.20 is 20% *)
  ; system_losses : float (* fraction lost to inverter, wiring, soiling *)
  }

type position =
  { elevation_rad : float (* angle above the horizon *)
  ; azimuth_deg : float (* clockwise from north *)
  }

type month_result =
  { month : int (* 1 to 12 *)
  ; energy_kwh : float (* per square metre of array *)
  }

(* -------------------------------------------------------------- constants *)

let solar_constant = 1367.0 (* W/m^2, the extraterrestrial flux *)
let ground_albedo = 0.2 (* the fraction of light the roof bounces back *)
let diffuse_fraction = 0.10 (* a clear sky's diffuse share of the global *)
let co2_kg_per_kwh = 0.417 (* the US grid, on average *)

(* ------------------------------------------------------------- utilities *)

let deg_to_rad d = d *. Float.pi /. 180.0
let rad_to_deg r = r *. 180.0 /. Float.pi

(* An angle, folded into [0, 360). *)
let normalize_angle a =
  (* int_of_float truncates towards zero, so the quotient is at most one
     turn out; the adjustment below folds the rest in. *)
  let r = a -. (360.0 *. Float.of_int (int_of_float (a /. 360.0))) in
  if Float.compare r 0.0 < 0 then r +. 360.0 else r
;;

let clamp lo hi x = if Float.compare x lo < 0 then lo else if Float.compare x hi > 0 then hi else x

(* ----------------------------------------------------------------- dates *)

let days_in_month = function
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> 28
  | _ -> 30

(* The day of the year, counting 1 January as day 1. *)
let day_of_year ~month ~day =
  let rec loop m acc = if m >= month then acc else loop (m + 1) (acc + days_in_month m) in
  loop 1 0 + day
;;

(* ------------------------------------------------------- solar geometry *)

(* Cooper's equation for the solar declination: the sun's tilt north or
   south of the equator on a given day of the year. *)
let declination_rad ~day =
  deg_to_rad (23.45 *. Float.sin (deg_to_rad (360.0 *. Float.of_int (day + 284) /. 365.0)))
;;

(* Spencer's equation of time, in minutes: the difference between clock
   time and solar time, caused by the Earth's orbit and its tilt. *)
let equation_of_time_minutes ~day =
  let b = deg_to_rad (360.0 *. Float.of_int (day - 81) /. 365.0) in
  9.87 *. Float.sin (2.0 *. b) -. 7.53 *. Float.cos b -. 1.5 *. Float.sin b
;;

(* The hour angle: how far the sun is from its local meridian, in
   radians. Negative in the morning, positive in the afternoon, 15
   degrees per hour from noon. *)
let hour_angle_rad ~solar_minutes =
  deg_to_rad (15.0 *. (solar_minutes /. 60.0 -. 12.0))
;;

(* Clock time (minutes after local midnight) to solar time. *)
let solar_minutes_of ~location ~day ~clock_minutes =
  let standard_meridian = 15.0 *. location.utc_offset in
  clock_minutes +. (4.0 *. (standard_meridian -. location.longitude)) +. equation_of_time_minutes ~day
;;

(* The sun's position in the east-north-up frame.

   The three components are the direction the sun is in, expressed as
   (east, north, up). The formulae are the standard ones, with the
   signs arranged so that noon puts the sun due south in the northern
   hemisphere. *)
let sun_vector ~location ~day ~clock_minutes =
  let phi = deg_to_rad location.latitude in
  let delta = declination_rad ~day in
  let omega = hour_angle_rad ~solar_minutes:(solar_minutes_of ~location ~day ~clock_minutes) in
  (* The sun's direction, in east-north-up coordinates. The north
     component is negative at noon in the northern hemisphere, because
     the sun is to the south — and a sign error here is invisible until
     an array pointed south collects LESS than one pointed north. *)
  let east = -.Float.cos delta *. Float.sin omega in
  let north = Float.sin delta *. Float.cos phi -. Float.cos delta *. Float.sin phi *. Float.cos omega in
  let up = Float.sin phi *. Float.sin delta +. Float.cos phi *. Float.cos delta *. Float.cos omega in
  (east, north, up)
;;

(* The clock time of solar noon. It is not 12:00: the sun crosses the
   meridian when the clock says 12:00 only on the standard meridian of
   the time zone, and only when the equation of time is zero. *)
let solar_noon_clock_minutes ~location ~day =
  let standard_meridian = 15.0 *. location.utc_offset in
  720.0 -. (4.0 *. (standard_meridian -. location.longitude)) -. equation_of_time_minutes ~day
;;

(* The sun's elevation and azimuth. *)
let solar_position ~location ~day ~clock_minutes =
  let east, north, up = sun_vector ~location ~day ~clock_minutes in
  let elevation_rad = Float.asin (clamp (-1.0) 1.0 up) in
  (* Azimuth measured clockwise from north: the angle from the north
     axis, rotating toward the east. At noon in the northern hemisphere
     the sun is due south, and the north component is negative, so this
     comes out at 180 — which is the convention. *)
  let azimuth_deg = normalize_angle (rad_to_deg (Float.atan2 east north)) in
  { elevation_rad; azimuth_deg }
;;

(* The panel's normal, in the same east-north-up frame.

   A panel tilted [beta] from the horizontal and facing [gamma] (from
   north, clockwise) has its normal tipped [beta] from straight up, in
   the direction of [gamma]. A flat panel's normal is straight up, which
   is the case that catches a wrong formula: a panel at zero tilt must
   not depend on which way it faces. *)
let panel_normal ~spec =
  let beta = deg_to_rad spec.tilt in
  let gamma = deg_to_rad spec.azimuth in
  (Float.sin beta *. Float.sin gamma, Float.sin beta *. Float.cos gamma, Float.cos beta)
;;

(* The cosine of the angle between the sun and the panel. Negative means
   the sun is behind the panel. *)
let incidence_cos ~location ~spec ~day ~clock_minutes =
  let s_east, s_north, s_up = sun_vector ~location ~day ~clock_minutes in
  let n_east, n_north, n_up = panel_normal ~spec in
  s_east *. n_east +. s_north *. n_north +. s_up *. n_up
;;

(* -------------------------------------------------- clear-sky irradiance *)

(* The relative optical air mass: how much atmosphere the sunlight has
   crossed, as a multiple of straight overhead. Kasten and Young's
   formula, which is the one that stays right down to the horizon. *)
let air_mass ~elevation_rad =
  let zenith_deg = 90.0 -. rad_to_deg elevation_rad in
  if Float.compare zenith_deg 90.0 >= 0 then Float.infinity
  else
    1.0
    /. (Float.cos (deg_to_rad zenith_deg)
       +. (0.50572 *. ((96.07995 -. zenith_deg) ** (-1.6364))))
;;

(* The extraterrestrial irradiance on a surface facing the sun: the
   flux at the top of the atmosphere, which varies a little over the
   year as the Earth moves between perihelion and aphelion. *)
let extraterrestrial ~day =
  solar_constant *. (1.0 +. (0.033 *. Float.cos (2.0 *. Float.pi *. Float.of_int day /. 365.0)))
;;

(* The direct normal irradiance of a clear sky: the Meinel model, the
   one ASHRAE and most textbooks still use for a clear atmosphere. *)
let clear_sky_direct_normal ~day ~elevation_rad =
  let am = air_mass ~elevation_rad in
  if Float.is_inf am then 0.0 else extraterrestrial ~day *. (0.7 ** (am ** 0.678))
;;

(* The clear-sky irradiance on a horizontal surface, and its diffuse
   and direct parts. *)
type irradiance =
  { dni : float (* direct normal, W/m^2 *)
  ; ghi : float (* global horizontal, W/m^2 *)
  ; dhi : float (* diffuse horizontal, W/m^2 *)
  }

let clear_sky ~location ~day ~clock_minutes =
  let position = solar_position ~location ~day ~clock_minutes in
  let dni = clear_sky_direct_normal ~day ~elevation_rad:position.elevation_rad in
  if Float.compare dni 0.0 <= 0 then { dni = 0.0; ghi = 0.0; dhi = 0.0 }
  else
    let cos_z = Float.cos (Float.pi /. 2.0 -. position.elevation_rad) in
    (* A clear sky's diffuse share is about a tenth of the global; the
       rest is direct. *)
    let ghi = dni *. cos_z /. (1.0 -. diffuse_fraction) in
    let dhi = ghi *. diffuse_fraction in
    { dni; ghi; dhi }
;;

(* The irradiance on the tilted panel: the direct part, the diffuse sky
   (which a tilted panel sees a different share of), and the light
   bounced off the ground. *)
let plane_of_array ~location ~spec ~day ~clock_minutes =
  let sky = clear_sky ~location ~day ~clock_minutes in
  if Float.compare sky.ghi 0.0 <= 0 then 0.0
  else
    let beta = deg_to_rad spec.tilt in
    let cos_incidence = incidence_cos ~location ~spec ~day ~clock_minutes in
    let direct = if Float.compare cos_incidence 0.0 <= 0 then 0.0 else sky.dni *. cos_incidence in
    let diffuse = sky.dhi *. ((1.0 +. Float.cos beta) /. 2.0) in
    let reflected = sky.ghi *. ground_albedo *. ((1.0 -. Float.cos beta) /. 2.0) in
    direct +. diffuse +. reflected
;;

(* ---------------------------------------------------------------- output *)

(* The energy a square metre of array collects on one day, in kWh. The
   day is integrated in ten-minute steps, which is finer than the
   variation of the sun over that interval. *)
let daily_energy_kwh ~location ~spec ~day =
  let step = 10.0 in
  let total = ref 0.0 in
  let minutes = ref 0.0 in
  while Float.compare !minutes 1440.0 < 0 do
    let irradiance = plane_of_array ~location ~spec ~day ~clock_minutes:!minutes in
    total := !total +. (irradiance *. (step /. 60.0));
    minutes := !minutes +. step
  done;
  !total /. 1000.0
;;

(* The energy a square metre collects in each month of the year: the
   month's TOTAL, not its daily average. A chart of a year wants the
   totals, and the annual figure is their sum. *)
let monthly_energy_kwh ~location ~spec =
  Array.init 12 ~f:(fun i ->
      let month = i + 1 in
      let total = ref 0.0 in
      for day = 1 to days_in_month month do
        total := !total +. daily_energy_kwh ~location ~spec ~day:(day_of_year ~month ~day)
      done;
      { month; energy_kwh = !total })
;;

let annual_energy_kwh ~location ~spec =
  monthly_energy_kwh ~location ~spec |> Array.fold ~init:0.0 ~f:(fun acc m -> acc +. m.energy_kwh)
;;

(* The total array area, in square metres.

   A module's area is its power divided by its efficiency, because that
   is what an efficiency means: watts per square metre of sunlight. A
   400 W module at 20% covers two square metres. *)
let array_area_m2 ~spec =
  Float.of_int spec.panel_count *. spec.panel_watts /. (spec.module_efficiency *. 1000.0)
;;

let array_watts ~spec = Float.of_int spec.panel_count *. spec.panel_watts

(* The AC energy the system delivers in a year, in kWh. The plane-of-array
   irradiation is multiplied by the area to get the light that lands on
   the panels; the module efficiency converts that to DC electricity;
   the system losses take their share off the top. *)
let annual_ac_kwh ~location ~spec =
  annual_energy_kwh ~location ~spec *. array_area_m2 ~spec *. spec.module_efficiency
  *. (1.0 -. spec.system_losses)
;;

(* ------------------------------------------------------------- financials *)

type financials =
  { ac_kwh_per_year : float
  ; installed_cost : float
  ; annual_savings : float
  ; payback_years : float option (* None when it never pays back *)
  ; lifetime_savings : float
  ; lifetime_co2_tonnes : float
  }

(* The money, over the life of the system.

   Twenty-five years is the standard warranty life of a panel. The
   panels lose about half a percent of their output a year, and
   electricity gets more expensive — both are included, because a
   calculator that ignores them flatters the system.

   The annual bill is capped at what is actually spent: a solar array
   that generates more than the household uses cannot save more money
   than the bill was. *)
let financials_with
  :  location:location
  -> spec:array_spec
  -> electricity_rate:float
  -> annual_consumption_kwh:float
  -> cost_per_watt:float
  -> price_escalation:float
  -> degradation:float
  -> lifetime_years:int
  -> financials
= fun ~location ~spec ~electricity_rate ~annual_consumption_kwh ~cost_per_watt ~price_escalation ~degradation ~lifetime_years ->
  let ac_kwh = annual_ac_kwh ~location ~spec in
  let self_consumed = Float.min ac_kwh annual_consumption_kwh in
  let installed_cost = array_watts ~spec *. cost_per_watt in
  let annual_savings = self_consumed *. electricity_rate in
  let lifetime_savings = ref 0.0 in
  let co2 = ref 0.0 in
  for year = 1 to lifetime_years do
    let output = ac_kwh *. ((1.0 -. degradation) ** Float.of_int (year - 1)) in
    let price = electricity_rate *. ((1.0 +. price_escalation) ** Float.of_int (year - 1)) in
    lifetime_savings :=
      !lifetime_savings +. (Float.min output annual_consumption_kwh *. price);
    co2 := !co2 +. (output *. co2_kg_per_kwh /. 1000.0)
  done;
  let payback_years =
    if Float.compare annual_savings 0.0 <= 0 then None
    else Some (installed_cost /. annual_savings)
  in
  { ac_kwh_per_year = ac_kwh
  ; installed_cost
  ; annual_savings
  ; payback_years
  ; lifetime_savings = !lifetime_savings
  ; lifetime_co2_tonnes = !co2
  }
;;
