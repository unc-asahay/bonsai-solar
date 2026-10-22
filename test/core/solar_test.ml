open! Core
open Solar


(* ---------------------------------------------------------------- harness *)

let failures = ref 0
let checks = ref 0

let check name ok detail =
  incr checks;
  if not ok then incr failures;
  Printf.printf "%s  %s%s\n" (if ok then "PASS" else "FAIL") name
    (if String.is_empty detail then "" else "  — " ^ detail)
;;

let near name ~expected ~actual ~tolerance =
  let ok = Float.compare (Float.abs (actual -. expected)) tolerance <= 0 in
  check name ok
    (Printf.sprintf "expected %.6f, got %.6f (tolerance %.6f)" expected actual tolerance)
;;

(* ------------------------------------------------------------------ sites *)

let denver = { name = "Denver, CO"; latitude = 39.7392; longitude = -104.9903; utc_offset = -7.0 }

let sydney =
  { name = "Sydney, AU"; latitude = -33.8688; longitude = 151.2093; utc_offset = 10.0 }

let equatorial = { name = "Equator"; latitude = 0.0; longitude = 0.0; utc_offset = 0.0 }

let array =
  { tilt = 20.0
  ; azimuth = 180.0
  ; panel_watts = 400.0
  ; panel_count = 10
  ; module_efficiency = 0.20
  ; system_losses = 0.14
  }

let horizontal_extraterrestrial ~latitude =
  let total = ref 0.0 in
  for day = 1 to 365 do
    let site = { denver with latitude } in
    let minutes = ref 0.0 in
    while Float.compare !minutes 1440.0 < 0 do
      let sky = clear_sky ~location:site ~day ~clock_minutes:!minutes in
      (* Reconstruct the extraterrestrial horizontal from the model's
         own geometry, so the check is of the geometry rather than of
         the clear-sky model. *)
      let cos_z = Float.cos (Float.pi /. 2.0 -. (solar_position ~location:site ~day ~clock_minutes:!minutes).elevation_rad) in
      if Float.compare sky.dni 0.0 > 0 then
        total := !total +. (extraterrestrial ~day *. cos_z *. (10.0 /. 60.0));
      minutes := !minutes +. 10.0
    done
  done;
  !total /. 1000.0
;;

(* ------------------------------------------------------ solar geometry *)

let () =
  (* The solstice declination: the sun is 23.44 degrees north of the
     equator in June and the same distance south in December. *)
  near "the June solstice declination" ~expected:23.44 ~actual:(rad_to_deg (declination_rad ~day:172))
    ~tolerance:0.05;
  near "the December solstice declination" ~expected:(-23.44)
    ~actual:(rad_to_deg (declination_rad ~day:355)) ~tolerance:0.05;
  (* The equinoxes are the two days the declination crosses zero. *)
  near "the March equinox declination" ~expected:0.0
    ~actual:(rad_to_deg (declination_rad ~day:80)) ~tolerance:0.6;
  print_endline ""
;;

let () =
  (* At SOLAR noon on the summer solstice, a site at 40 degrees north
     sees the sun 73.4 degrees up: 90 - 40 + 23.44.

     Solar noon is not 12:00 — it is a few minutes away from it, because
     of the longitude within the time zone and the equation of time. The
     model computes it, and the check has to ask for it. *)
  let site = { denver with latitude = 40.0 } in
  let noon_minutes = solar_noon_clock_minutes ~location:site ~day:172 in
  let noon = solar_position ~location:site ~day:172 ~clock_minutes:noon_minutes in
  near "the noon elevation at 40N on the solstice" ~expected:73.44
    ~actual:(rad_to_deg noon.elevation_rad) ~tolerance:0.2;
  (* And the sun is due south, because the site is north of it. *)
  near "the noon azimuth is due south" ~expected:180.0 ~actual:noon.azimuth_deg ~tolerance:0.2;
  (* In the southern hemisphere the noon sun is due north. *)
  let sydney_noon =
    solar_position ~location:sydney ~day:172
      ~clock_minutes:(solar_noon_clock_minutes ~location:sydney ~day:172)
  in
  near "the noon azimuth is due north in the southern hemisphere" ~expected:0.0
    ~actual:sydney_noon.azimuth_deg ~tolerance:0.2;
  (* And solar noon is not 12:00 — in Denver in June it is a little
     after noon, because the city is east of the centre of its time
     zone and the equation of time is negative. *)
  check "solar noon is not 12:00" (Float.compare noon_minutes 720.0 <> 0)
    (Printf.sprintf "%.1f minutes after midnight" noon_minutes);
  print_endline ""
;;

let () =
  (* The sun rises in the east and sets in the west. At 6am solar time
     it must be east of the meridian, at 6pm west of it. *)
  let morning = solar_position ~location:denver ~day:172 ~clock_minutes:360.0 in
  let evening = solar_position ~location:denver ~day:172 ~clock_minutes:1080.0 in
  check "the morning sun is east of north"
    (Float.compare morning.azimuth_deg 45.0 > 0 && Float.compare morning.azimuth_deg 135.0 < 0)
    (Printf.sprintf "azimuth %.2f" morning.azimuth_deg);
  check "the evening sun is west of north"
    (Float.compare evening.azimuth_deg 225.0 > 0 && Float.compare evening.azimuth_deg 315.0 < 0)
    (Printf.sprintf "azimuth %.2f" evening.azimuth_deg);
  (* At the equator on the equinox the sun passes nearly overhead. *)
  let equinox_noon =
    solar_position ~location:equatorial ~day:80
      ~clock_minutes:(solar_noon_clock_minutes ~location:equatorial ~day:80)
  in
  (* On the equinox the sun passes almost directly overhead at the
     equator: 90 degrees, less the fraction of a degree the declination
     is off zero on that day. *)
  near "the equinox noon elevation at the equator" ~expected:89.6
    ~actual:(rad_to_deg equinox_noon.elevation_rad) ~tolerance:0.3;
  print_endline ""
;;

(* ---------------------------------------------------------- the geometry *)

let () =
  (* The classic result: a fixed-tilt array collects the most energy over
     a year when it is tilted at roughly the site's latitude, and faces
     due south (in the northern hemisphere). If the transposition were
     wrong — a sign error in the incidence angle, say — the optimum
     would land somewhere else entirely, so this is the sharpest check
     the model has. *)
  let best_tilt location =
    let results =
      List.init 61 ~f:(fun i ->
          let tilt = Float.of_int i in
          let spec = { array with tilt; azimuth = 180.0 } in
          (tilt, annual_energy_kwh ~location ~spec))
    in
    List.fold results ~init:(0.0, 0.0) ~f:(fun (bt, be) (t, e) ->
        if Float.compare e be > 0 then (t, e) else (bt, be))
  in
  let best_azimuth location =
    let results =
      List.init 36 ~f:(fun i ->
          let azimuth = Float.of_int (i * 10) in
          let spec = { array with tilt = 30.0; azimuth } in
          (azimuth, annual_energy_kwh ~location ~spec))
    in
    List.fold results ~init:(0.0, 0.0) ~f:(fun (ba, be) (a, e) ->
        if Float.compare e be > 0 then (a, e) else (ba, be))
  in
  let tilt, _ = best_tilt denver in
  near "the best tilt at 39.7N is near the latitude" ~expected:39.7 ~actual:tilt ~tolerance:5.0;
  let azimuth, _ = best_azimuth denver in
  near "the best azimuth in the northern hemisphere is due south" ~expected:180.0 ~actual:azimuth
    ~tolerance:10.0;
  (* And the mirror image: in the southern hemisphere, north wins. *)
  let azimuth_s, _ = best_azimuth sydney in
  near "the best azimuth in the southern hemisphere is due north" ~expected:0.0 ~actual:azimuth_s
    ~tolerance:10.0;
  print_endline ""
;;

let () =
  (* A tilted array aimed at the sun beats a flat one. *)
  let flat = { array with tilt = 0.0 } in
  let tilted = { array with tilt = 38.0 } in
  let flat_energy = annual_energy_kwh ~location:denver ~spec:flat in
  let tilted_energy = annual_energy_kwh ~location:denver ~spec:tilted in
  check "a tilted array beats a flat one"
    (Float.compare tilted_energy flat_energy > 0)
    (Printf.sprintf "tilted %.1f vs flat %.1f kWh/m2" tilted_energy flat_energy);
  (* And facing the sun beats facing away from it. *)
  let north = { array with tilt = 30.0; azimuth = 0.0 } in
  let south = { array with tilt = 30.0; azimuth = 180.0 } in
  let n = annual_energy_kwh ~location:denver ~spec:north in
  let so = annual_energy_kwh ~location:denver ~spec:south in
  check "facing the sun beats facing away from it" (Float.compare so n > 0)
    (Printf.sprintf "south %.1f vs north %.1f kWh/m2" so n);
  print_endline ""
;;

(* ------------------------------------------------------ the magnitudes *)

let () =
  (* The annual extraterrestrial irradiation on a horizontal surface is a
     number every solar engineer knows: about 3.9 kWh per square metre
     per day at 40 degrees north, or 1400 kWh over the year. The model
     has to produce it, because every clear-sky number is derived from
     it. *)
  (* The annual extraterrestrial irradiation on a horizontal surface at
     40 degrees north is 2863 kWh/m2 — 7.84 kWh/m2 a day. That number
     has a closed form to check it against: at the equator the daily
     average is Gsc * (24/pi) * <cos(delta)>, which is 1367 * 7.6394 *
     0.9545 = 9966 Wh/m2, or 3638 kWh/m2 a year. The model has to
     produce that. *)
  let h0 = horizontal_extraterrestrial ~latitude:40.0 in
  near "the annual extraterrestrial irradiation at 40N" ~expected:2862.7 ~actual:h0
    ~tolerance:5.0;
  let h0_equator = horizontal_extraterrestrial ~latitude:0.0 in
  near "the annual extraterrestrial irradiation at the equator" ~expected:3638.0
    ~actual:h0_equator ~tolerance:20.0;
  print_endline ""
;;

let () =
  (* The clear-sky yield at a temperate site: between 1500 and 2600 kWh
     per square metre a year, which is the range every clear-sky model
     and every textbook gives. *)
  let flat = { array with tilt = 0.0 } in
  let ghi = annual_energy_kwh ~location:denver ~spec:flat in
  check "the clear-sky yield at Denver is plausible"
    (Float.compare ghi 1500.0 > 0 && Float.compare ghi 2600.0 < 0)
    (Printf.sprintf "%.1f kWh/m2/yr" ghi);
  (* The tropics get more, and the arctic less. *)
  let equator = annual_energy_kwh ~location:equatorial ~spec:flat in
  check "the equator gets more than Denver" (Float.compare equator ghi > 0)
    (Printf.sprintf "equator %.1f vs Denver %.1f" equator ghi);
  (* A month in midsummer must beat a month in midwinter. *)
  let months = monthly_energy_kwh ~location:denver ~spec:{ array with tilt = 39.7 } in
  let june = months.(5).energy_kwh in
  let december = months.(11).energy_kwh in
  check "June beats December" (Float.compare june december > 0)
    (Printf.sprintf "June %.2f vs December %.2f kWh/m2/day" june december);
  print_endline ""
;;

(* ----------------------------------------------------- array and money *)

let () =
  (* A module's area is its power divided by its efficiency: 400 W at
     20% covers two square metres. *)
  near "a 400 W module at 20% covers two square metres" ~expected:2.0
    ~actual:(array_area_m2 ~spec:{ array with panel_count = 1 }) ~tolerance:0.001;
  near "ten of them cover twenty" ~expected:20.0 ~actual:(array_area_m2 ~spec:array) ~tolerance:0.001;
  near "the array is 4000 W" ~expected:4000.0 ~actual:(array_watts ~spec:array) ~tolerance:0.001;
  print_endline ""
;;

let () =
  (* The energy chain, checked end to end: the plane-of-array irradiation
     times the area times the efficiency times what is left after the
     losses. *)
  let spec = { array with tilt = 39.7 } in
  let poa = annual_energy_kwh ~location:denver ~spec in
  let ac = annual_ac_kwh ~location:denver ~spec in
  let expected = poa *. 20.0 *. 0.20 *. 0.86 in
  near "the AC energy is the chain" ~expected ~actual:ac ~tolerance:0.001;
  (* Ten 400 W panels at 20% efficiency in Denver cannot make more than
     a few thousand kWh a year. *)
  check "the system output is plausible"
    (Float.compare ac 4000.0 > 0 && Float.compare ac 14000.0 < 0)
    (Printf.sprintf "%.0f kWh/yr" ac);
  print_endline ""
;;

let () =
  (* The money. A $3/W system costing $12,000, saving $X a year, pays
     back in cost/savings years. *)
  let money =
    financials_with
      ~location:denver
      ~spec:{ array with tilt = 39.7 }
      ~electricity_rate:0.15
      ~annual_consumption_kwh:9000.0
      ~cost_per_watt:3.0
      ~price_escalation:0.03
      ~degradation:0.005
      ~lifetime_years:25
  in
  near "the installed cost is watts times dollars per watt" ~expected:12000.0 ~actual:money.installed_cost
    ~tolerance:0.001;
  let payback = match money.payback_years with Some p -> p | None -> -1.0 in
  near "the payback is cost over annual savings" ~expected:(12000.0 /. money.annual_savings)
    ~actual:payback ~tolerance:0.001;
  check "the payback is a plausible number of years"
    (Float.compare payback 4.0 > 0 && Float.compare payback 25.0 < 0)
    (Printf.sprintf "%.1f years" payback);
  (* With the price rising faster than the panels decay, twenty-five
     years is worth more than twenty-five times the first year. *)
  check "lifetime savings beat the flat sum"
    (Float.compare money.lifetime_savings (25.0 *. money.annual_savings) > 0)
    (Printf.sprintf "%.0f over the life, %.0f a year" money.lifetime_savings money.annual_savings);
  (* And a system bigger than the household's use cannot save more than
     the bill. *)
  let big =
    financials_with
      ~location:denver
      ~spec:{ array with panel_count = 100 }
      ~electricity_rate:0.15
      ~annual_consumption_kwh:9000.0
      ~cost_per_watt:3.0
      ~price_escalation:0.0
      ~degradation:0.0
      ~lifetime_years:25
  in
  check "savings are capped at the bill" (Float.compare big.annual_savings 1350.0 <= 0)
    (Printf.sprintf "%.0f against a 1350 bill" big.annual_savings);
  print_endline ""
;;

(* ------------------------------------------------------------- summary *)

let () =
  Printf.printf "\n%d checks, %d failure(s)\n" !checks !failures;
  if !failures > 0 then exit 1
;;


(* The values the independent Python model also produces. The gate
   compares the two sets line by line, so that a disagreement between
   two implementations of the same physics cannot pass unnoticed. *)
let () =
  let miami = { name = "Miami, FL"; latitude = 25.7617; longitude = -80.1918; utc_offset = -5.0 } in
  let sydney = { name = "Sydney, AU"; latitude = -33.8688; longitude = 151.2093; utc_offset = 10.0 } in
  let at tilt azimuth = { array with tilt; azimuth } in
  let poa location spec = annual_energy_kwh ~location ~spec in
  Printf.printf "annual_poa_denver_tilt39 = %.1f\n" (poa denver (at 39.0 180.0));
  Printf.printf "annual_poa_denver_tilt0 = %.1f\n" (poa denver (at 0.0 180.0));
  Printf.printf "annual_poa_denver_tilt20 = %.1f\n" (poa denver (at 20.0 180.0));
  Printf.printf "annual_poa_denver_north = %.1f\n" (poa denver (at 39.0 0.0));
  Printf.printf "annual_poa_miami_tilt39 = %.1f\n" (poa miami (at 39.0 180.0));
  Printf.printf "annual_poa_sydney_tilt39 = %.1f\n" (poa sydney (at 39.0 180.0));
  Printf.printf "annual_horizontal_extraterrestrial_40n = %.1f\n" (horizontal_extraterrestrial ~latitude:40.0);
  Printf.printf "annual_horizontal_extraterrestrial_equator = %.1f\n" (horizontal_extraterrestrial ~latitude:0.0)
;;
