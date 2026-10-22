(* The solar calculator's interface.

   The state is one record of everything the user can change; every
   number on the page is derived from it. Nothing is computed twice and
   nothing can disagree with anything else. *)

open! Core
open! Bonsai_web
open Js_of_ocaml

module S = Solar

(* ----------------------------------------------------------------- state *)

type inputs =
  { location : S.location
  ; tilt : float
  ; azimuth : float
  ; panel_watts : float
  ; panel_count : int
  ; module_efficiency : float
  ; system_losses : float
  ; electricity_rate : float
  ; annual_consumption_kwh : float
  ; cost_per_watt : float
  }

let denver =
  { S.name = "Denver"; latitude = 39.7392; longitude = -104.9903; utc_offset = -7.0 }

(* Cities, with their coordinates. A calculator that makes you look up
   a latitude is a calculator people give up on. *)
let locations =
  [ ("San Francisco, CA", { S.name = "San Francisco"; latitude = 37.7749; longitude = -122.4194; utc_offset = -8.0 })
  ; ("Los Angeles, CA", { S.name = "Los Angeles"; latitude = 34.0522; longitude = -118.2437; utc_offset = -8.0 })
  ; ("Seattle, WA", { S.name = "Seattle"; latitude = 47.6062; longitude = -122.3321; utc_offset = -8.0 })
  ; ("Phoenix, AZ", { S.name = "Phoenix"; latitude = 33.4484; longitude = -112.0740; utc_offset = -7.0 })
  ; ("Denver, CO", denver)
  ; ("Chicago, IL", { S.name = "Chicago"; latitude = 41.8781; longitude = -87.6298; utc_offset = -6.0 })
  ; ("Miami, FL", { S.name = "Miami"; latitude = 25.7617; longitude = -80.1918; utc_offset = -5.0 })
  ; ("New York, NY", { S.name = "New York"; latitude = 40.7128; longitude = -74.0060; utc_offset = -5.0 })
  ; ("London, UK", { S.name = "London"; latitude = 51.5074; longitude = -0.1278; utc_offset = 0.0 })
  ; ("Berlin, DE", { S.name = "Berlin"; latitude = 52.5200; longitude = 13.4050; utc_offset = 1.0 })
  ; ("Delhi, IN", { S.name = "Delhi"; latitude = 28.6139; longitude = 77.2090; utc_offset = 5.5 })
  ; ("Singapore, SG", { S.name = "Singapore"; latitude = 1.3521; longitude = 103.8198; utc_offset = 8.0 })
  ; ("Tokyo, JP", { S.name = "Tokyo"; latitude = 35.6762; longitude = 139.6503; utc_offset = 9.0 })
  ; ("Sydney, AU", { S.name = "Sydney"; latitude = -33.8688; longitude = 151.2093; utc_offset = 10.0 })
  ; ("Cape Town, ZA", { S.name = "Cape Town"; latitude = -33.9249; longitude = 18.4241; utc_offset = 2.0 })
  ; ("Nairobi, KE", { S.name = "Nairobi"; latitude = -1.2921; longitude = 36.8219; utc_offset = 3.0 })
  ]

(* Find a city by name. Core's List has no assoc, so this is a fold. *)
let find_location name =
  List.fold locations ~init:None ~f:(fun found (candidate, loc) ->
    if String.equal candidate name then Some loc else found)
;;

let initial_inputs =
  { location = denver
  ; tilt = 39.0
  ; azimuth = 180.0
  ; panel_watts = 400.0
  ; panel_count = 10
  ; module_efficiency = 0.20
  ; system_losses = 0.14
  ; electricity_rate = 0.15
  ; annual_consumption_kwh = 9000.0
  ; cost_per_watt = 3.0
  }

(* ------------------------------------------------------------- formatting *)


(* A thousands separator. OCaml's format has no comma flag, so the
   grouping is done here rather than wished for. *)
let thousands n =
  let s = Printf.sprintf "%.0f" (Float.abs n) in
  let len = String.length s in
  let buf = Buffer.create (len + (len / 3)) in
  for i = 0 to len - 1 do
    if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char buf ',';
    Buffer.add_char buf s.[i]
  done;
  (if Float.compare n 0.0 < 0 then "-" else "") ^ Buffer.contents buf
;;

(* Money gets thousands separators too, because $39776 is harder to
   read at a glance than $39,776. *)
let money n =
  let magnitude = Float.abs n in
  if Float.compare magnitude 1000000.0 >= 0 then Printf.sprintf "$%.2fM" (n /. 1000000.0)
  else if Float.compare magnitude 1000.0 >= 0 then "$" ^ thousands n
  else Printf.sprintf "$%.2f" n
;;

(* A compass bearing. The angles are compared as integers, because the
   polymorphic comparison is shadowed in this switch and will not
   compare two floats. *)
let bearing_of azimuth_deg =
  let a =
    azimuth_deg -. (360.0 *. Float.of_int (Int.of_float (azimuth_deg /. 360.0)))
  in
  let eighth = 45.0 in
  let index = Int.of_float (Float.round (a /. eighth)) mod 8 in
  match index with
  | 0 -> "North"
  | 1 -> "North-east"
  | 2 -> "East"
  | 3 -> "South-east"
  | 4 -> "South"
  | 5 -> "South-west"
  | 6 -> "West"
  | _ -> "North-west"
;;

let clock_string minutes =
  let h = Int.of_float (minutes /. 60.0) in
  let mm = Int.of_float (minutes -. (Float.of_int h *. 60.0)) in
  Printf.sprintf "%d:%02d" h mm
;;

(* ------------------------------------------------------------- the model *)

let spec_of inputs =
  { S.tilt = inputs.tilt
  ; azimuth = inputs.azimuth
  ; panel_watts = inputs.panel_watts
  ; panel_count = inputs.panel_count
  ; module_efficiency = inputs.module_efficiency
  ; system_losses = inputs.system_losses
  }

let results_of inputs =
  let spec = spec_of inputs in
  let location = inputs.location in
  let months = S.monthly_energy_kwh ~location ~spec in
  let poa = S.annual_energy_kwh ~location ~spec in
  let ac = S.annual_ac_kwh ~location ~spec in
  let fin =
    S.financials_with
      ~location
      ~spec
      ~electricity_rate:inputs.electricity_rate
      ~annual_consumption_kwh:inputs.annual_consumption_kwh
      ~cost_per_watt:inputs.cost_per_watt
      ~price_escalation:0.03
      ~degradation:0.005
      ~lifetime_years:25
  in
  (months, poa, ac, fin)
;;

(* --------------------------------------------------------------- helpers *)

(* Vdom.Node.create takes its attributes as a required label, so the
   optional one here becomes a list that is never empty by accident. *)
let el ?attrs tag children =
  Vdom.Node.create tag ~attrs:(Option.value ~default:[] attrs) children

(* SVG elements are not HTML elements. virtual_dom keeps them apart, and
   an <svg> built with the ordinary constructor is an unknown element
   that the browser will not draw. *)
let svg_el ?attrs tag children =
  Vdom.Node.create_svg tag ~attrs:(Option.value ~default:[] attrs) children

let div ?attrs children = el ?attrs "div" children
let span ?attrs children = el ?attrs "span" children
(* Named with a suffix because "label" is also a field's name, and a
   parameter of the same name shadows the helper. *)
let label_tag ?attrs children = el ?attrs "label" children
let text t = Vdom.Node.text t

(* A labelled number field. *)
let number_field ~label ~value ~step ~on_change =
  let input_id = "f-" ^ String.map ~f:(fun c -> if Char.equal c ' ' then '-' else c) label in
  div
    ~attrs:[ Vdom.Attr.class_ "field" ]
    [ label_tag ~attrs:[ Vdom.Attr.for_ input_id ] [ text label ]
    ; el
        "input"
        ~attrs:
          [ Vdom.Attr.id input_id
          ; Vdom.Attr.class_ "input"
          ; Vdom.Attr.type_ "number"
          ; Vdom.Attr.value (Printf.sprintf "%g" value)
          ; Vdom.Attr.create "step" (Printf.sprintf "%g" step)
          ; Vdom.Attr.on_input (fun _ raw -> on_change raw)
          ]
        []
    ]
;;

(* A labelled slider, with the value shown beside it. *)
let slider_field ~label ~value ~min ~max ~step ~format ~on_change =
  let input_id = "s-" ^ String.map ~f:(fun c -> if Char.equal c ' ' then '-' else c) label in
  div
    ~attrs:[ Vdom.Attr.class_ "field slider" ]
    [ div
        ~attrs:[ Vdom.Attr.class_ "field-head" ]
        [ label_tag ~attrs:[ Vdom.Attr.for_ input_id ] [ text label ]
        ; span ~attrs:[ Vdom.Attr.class_ "field-value" ] [ text (format value) ]
        ]
    ; el
        "input"
        ~attrs:
          [ Vdom.Attr.id input_id
          ; Vdom.Attr.class_ "input"
          ; Vdom.Attr.type_ "range"
          ; Vdom.Attr.value (Printf.sprintf "%g" value)
          ; Vdom.Attr.create "min" (Printf.sprintf "%g" min)
          ; Vdom.Attr.create "max" (Printf.sprintf "%g" max)
          ; Vdom.Attr.create "step" (Printf.sprintf "%g" step)
          ; Vdom.Attr.on_input (fun _ raw -> on_change raw)
          ]
        []
    ]
;;

(* A big number with a label under it. *)
let readout ~value ~unit ~label =
  div
    ~attrs:[ Vdom.Attr.class_ "readout" ]
    [ div ~attrs:[ Vdom.Attr.class_ "readout-value" ] [ text value ]
    ; div ~attrs:[ Vdom.Attr.class_ "readout-unit" ] [ text unit ]
    ; div ~attrs:[ Vdom.Attr.class_ "readout-label" ] [ text label ]
    ]
;;

(* --------------------------------------------------------------- charts *)

let month_names =
  [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

(* The year's production, month by month, as bars. *)
let monthly_chart months ~width ~height =
  let max_energy =
    Array.fold months ~init:0.0 ~f:(fun acc m -> Float.max acc m.S.energy_kwh)
  in
  let count = Array.length months in
  let gap = 6.0 in
  let bar_width = (width -. (Float.of_int count *. gap)) /. Float.of_int count in
  let bars =
    Array.mapi
      months
      ~f:(fun i m ->
        let energy = m.S.energy_kwh in
        let h =
          if Float.compare max_energy 0.0 <= 0 then 0.0
          else (energy /. max_energy) *. height
        in
        let x = (Float.of_int i *. (bar_width +. gap)) +. gap in
        let y = height -. h in
        svg_el
          "rect"
          ~attrs:
            [ Vdom.Attr.class_ "bar"
            ; Vdom.Attr.create "x" (Printf.sprintf "%.2f" x)
            ; Vdom.Attr.create "y" (Printf.sprintf "%.2f" y)
            ; Vdom.Attr.create "width" (Printf.sprintf "%.2f" bar_width)
            ; Vdom.Attr.create "height" (Printf.sprintf "%.2f" h)
            ; Vdom.Attr.create "rx" "3"
            ; Vdom.Attr.create "data-month" (string_of_int m.S.month)
            ; Vdom.Attr.create "data-energy" (Printf.sprintf "%.1f" energy)
            ]
          [])
  in
  let labels =
    Array.mapi
      months
      ~f:(fun i _ ->
        let x = (Float.of_int i *. (bar_width +. gap)) +. gap in
        svg_el
          "text"
          ~attrs:
            [ Vdom.Attr.class_ "axis-label"
            ; Vdom.Attr.create "x" (Printf.sprintf "%.2f" (x +. (bar_width /. 2.0)))
            ; Vdom.Attr.create "y" (Printf.sprintf "%.2f" (height +. 16.0))
            ; Vdom.Attr.create "text-anchor" "middle"
            ]
          [ text month_names.(i) ])
  in
  (* The peak, named, because a bar chart with no scale is a picture
     rather than a reading. *)
  let peak_month, peak_energy =
    Array.fold months ~init:(0, 0.0) ~f:(fun (best_month, best_energy) m ->
        if Float.compare m.S.energy_kwh best_energy > 0
        then (m.S.month, m.S.energy_kwh)
        else (best_month, best_energy))
  in
  (* The peak is named in the note under the chart rather than inside
     the SVG, where a label near the top edge gets clipped. *)
  let peak_note =
    Printf.sprintf
      "Peak: %s, %.0f kWh/m². The bars are sunlight on the panels; the array turns that into the figure above."
      month_names.(peak_month - 1)
      peak_energy
  in
  ( svg_el
      "svg"
      ~attrs:
        [ Vdom.Attr.class_ "chart"
        ; Vdom.Attr.create "viewBox" (Printf.sprintf "0 0 %g %g" width (height +. 24.0))
        ; Vdom.Attr.create "data-chart" "monthly"
        ]
      (Array.to_list (Array.append bars labels))
  , peak_note )
;;

(* The sun's path across the sky today, with the array's normal.

   The projection is the one an installer would draw: azimuth runs left
   to right — north, east, south, west — and elevation runs up from the
   horizon. The roof's normal is the vertical line above the direction
   the panels face, capped at 90 minus the tilt, so the angle between
   it and the sun's arc is the angle that matters. *)
let sun_path_chart ~location ~day ~spec ~width ~height =
  let steps = 96 in
  let horizon = height in
  let plot = height -. 22.0 in
  let point azimuth elevation =
    ((azimuth /. 360.0) *. width, plot -. (Float.min 90.0 elevation /. 90.0 *. plot))
  in
  let samples =
    List.filter_map
      ~f:(fun i ->
        let minutes = Float.of_int i *. (1440.0 /. Float.of_int steps) in
        let p = S.solar_position ~location ~day ~clock_minutes:minutes in
        if Float.compare p.S.elevation_rad 0.0 <= 0 then None
        else Some (p.S.azimuth_deg, S.rad_to_deg p.S.elevation_rad))
      (List.init steps ~f:(fun i -> i))
  in
  let path =
    match samples with
    | [] -> ""
    | (azimuth, elevation) :: rest ->
      let head =
        let (px, py) = point azimuth elevation in
        Printf.sprintf "M %.2f,%.2f" px py
      in
      let tail =
        List.map
          ~f:(fun (az, el) ->
            let (px, py) = point az el in
            Printf.sprintf "L %.2f,%.2f" px py)
          rest
      in
      String.concat ~sep:" " (head :: tail)
  in
  let noon_minutes = S.solar_noon_clock_minutes ~location ~day in
  let noon = S.solar_position ~location ~day ~clock_minutes:noon_minutes in
  let noon_azimuth = noon.S.azimuth_deg in
  let noon_elevation = S.rad_to_deg noon.S.elevation_rad in
  let (noon_x, noon_y) = point noon_azimuth noon_elevation in
  (* The roof's normal: straight up from the horizon, at the direction
     the panels face, as high as 90 minus the tilt. *)
  let (normal_x, normal_y) = point spec.S.azimuth (90.0 -. spec.S.tilt) in
  (* The letters at the edges are anchored inward, so the first and
     last are not cut off by the edge of the chart. *)
  let compass =
    List.map
      ~f:(fun (name, anchor, x) ->
        svg_el
          "text"
          ~attrs:
            [ Vdom.Attr.class_ "compass"
            ; Vdom.Attr.create "x" (Printf.sprintf "%.2f" x)
            ; Vdom.Attr.create "y" (Printf.sprintf "%.2f" (horizon +. 14.0))
            ; Vdom.Attr.create "text-anchor" anchor
            ]
          [ text name ])
      [ ("N", "start", 3.0)
      ; ("E", "middle", width /. 4.0)
      ; ("S", "middle", width /. 2.0)
      ; ("W", "middle", (width *. 3.0) /. 4.0)
      ; ("N", "end", width -. 3.0)
      ]
  in
  let caption x y anchor name =
    svg_el
      "text"
      ~attrs:
        [ Vdom.Attr.class_ "chart-caption"
        ; Vdom.Attr.create "x" (Printf.sprintf "%.2f" x)
        ; Vdom.Attr.create "y" (Printf.sprintf "%.2f" y)
        ; Vdom.Attr.create "text-anchor" anchor
        ]
      [ text name ]
  in
  svg_el
    "svg"
    ~attrs:
      [ Vdom.Attr.class_ "chart sun-path"
      ; Vdom.Attr.create "viewBox" (Printf.sprintf "0 0 %g %g" width (horizon +. 20.0))
      ; Vdom.Attr.create "data-chart" "sun-path"
      ]
    ([ svg_el
         "line"
         ~attrs:
           [ Vdom.Attr.class_ "horizon"
           ; Vdom.Attr.create "x1" "0"
           ; Vdom.Attr.create "y1" (Printf.sprintf "%.2f" horizon)
           ; Vdom.Attr.create "x2" (Printf.sprintf "%.2f" width)
           ; Vdom.Attr.create "y2" (Printf.sprintf "%.2f" horizon)
           ]
         []
     ; svg_el
         "line"
         ~attrs:
           [ Vdom.Attr.class_ "normal"
           ; Vdom.Attr.create "x1" (Printf.sprintf "%.2f" normal_x)
           ; Vdom.Attr.create "y1" (Printf.sprintf "%.2f" horizon)
           ; Vdom.Attr.create "x2" (Printf.sprintf "%.2f" normal_x)
           ; Vdom.Attr.create "y2" (Printf.sprintf "%.2f" normal_y)
           ]
         []
     ; svg_el "path" ~attrs:[ Vdom.Attr.create "d" path; Vdom.Attr.class_ "sun-arc" ] []
     ; svg_el
         "circle"
         ~attrs:
           [ Vdom.Attr.class_ "sun-dot"
           ; Vdom.Attr.create "cx" (Printf.sprintf "%.2f" noon_x)
           ; Vdom.Attr.create "cy" (Printf.sprintf "%.2f" noon_y)
           ; Vdom.Attr.create "r" "5"
           ]
         []
     ; (* The labels are placed so they cannot run off the chart or
          collide with the compass: the roof's label goes beside its
          line, and noon's goes above the sun. *)
       caption
         (if Float.compare normal_x (width /. 2.0) > 0 then normal_x -. 6.0 else normal_x +. 6.0)
         ((horizon +. normal_y) /. 2.0)
         (if Float.compare normal_x (width /. 2.0) > 0 then "end" else "start")
         "your roof"
     ; caption noon_x (noon_y -. 10.0) "middle" "noon"
     ]
    @ compass)
;;

(* -------------------------------------------------------------- the page *)

(* Today's day of the year, in the browser's own timezone — which is the
   timezone the user is thinking in. *)
let today_day_of_year () =
  (* The browser knows what day it is, and in the timezone the user is
     thinking in. Asking it is both simpler and more correct than
     carrying a date library into the bundle. *)
  let today : int =
    Js.Unsafe.pure_js_expr
      "(() => { const d = new Date(); const t = [0,31,59,90,120,151,181,212,243,273,304,334]; const y = d.getFullYear(); const leap = (y % 4 === 0 && y % 100 !== 0) || y % 400 === 0; const c = t.slice(0, d.getMonth()).reduce((a, b) => a + b, 0); return c + d.getDate() + ((leap && d.getMonth() > 1) ? 1 : 0); })()"
  in
  today
;;

let page (graph : Bonsai.graph) =
  let state, set_state = Bonsai.state' initial_inputs graph in
  Bonsai.map (Bonsai.both state set_state)
    ~f:(fun (state, set_state) ->
      let set = set_state in
      let months, poa, ac, fin = results_of state in
      let spec = spec_of state in
      let today = today_day_of_year () in
      let set_float setter raw =
        match Float.of_string_opt (String.strip raw) with
        | None -> Effect.Ignore
        | Some v -> setter v
      in
      (* -- the controls -- *)
      let location_select =
        div
          ~attrs:[ Vdom.Attr.class_ "field" ]
          [ label_tag ~attrs:[ Vdom.Attr.for_ "location" ] [ text "Location" ]
          ; el
              "select"
              ~attrs:
                [ Vdom.Attr.id "location"
                ; Vdom.Attr.class_ "input"
                ; Vdom.Attr.on_change (fun _ev value ->
                    (* The handler is given the new value directly, so
                       there is no need to read it back out of the DOM. *)
                    match find_location value with
                    | None -> Effect.Ignore
                    | Some loc -> set (fun s -> { s with location = loc }))
                ]
              (List.map
                 ~f:(fun (name, loc) ->
                   (* The chosen option is marked, because a select's
                      value attribute does not choose an option — the
                      selected attribute on the option does. *)
                   el
                     "option"
                     ~attrs:
                       ([ Vdom.Attr.create "value" name ]
                       @ (if String.equal loc.S.name state.location.S.name
                          then [ Vdom.Attr.selected ]
                          else []))
                     [ text name ])
                 locations)
          ]
      in
      let controls =
        div
          ~attrs:[ Vdom.Attr.class_ "panel controls" ]
          [ div ~attrs:[ Vdom.Attr.class_ "panel-title" ] [ text "Your roof" ]
          ; location_select
          ; number_field
              ~label:"Latitude"
              ~value:state.location.S.latitude
              ~step:0.5
              ~on_change:(set_float (fun v ->
                           set (fun s -> { s with location = { s.location with S.latitude = v } })))
          ; number_field
              ~label:"Longitude"
              ~value:state.location.S.longitude
              ~step:0.5
              ~on_change:(set_float (fun v ->
                           set (fun s -> { s with location = { s.location with S.longitude = v } })))
          ; slider_field
              ~label:"Roof tilt"
              ~value:state.tilt
              ~min:0.0
              ~max:90.0
              ~step:1.0
              ~format:(fun v -> Printf.sprintf "%g°" v)
              ~on_change:(set_float (fun v -> set (fun s -> { s with tilt = v })))
          ; slider_field
              ~label:"Direction"
              ~value:state.azimuth
              ~min:0.0
              ~max:360.0
              ~step:5.0
              ~format:(fun v -> Printf.sprintf "%g° (%s)" v (bearing_of v))
              ~on_change:(set_float (fun v -> set (fun s -> { s with azimuth = v })))
          ; number_field
              ~label:"Panels"
              ~value:(Float.of_int state.panel_count)
              ~step:1.0
              ~on_change:(set_float (fun v ->
                           set (fun s -> { s with panel_count = Int.of_float v })))
          ; number_field
              ~label:"Watts per panel"
              ~value:state.panel_watts
              ~step:10.0
              ~on_change:(set_float (fun v -> set (fun s -> { s with panel_watts = v })))
          ; slider_field
              ~label:"Panel efficiency"
              ~value:(state.module_efficiency *. 100.0)
              ~min:10.0
              ~max:25.0
              ~step:0.5
              ~format:(fun v -> Printf.sprintf "%g%%" v)
              ~on_change:(set_float (fun v ->
                           set (fun s -> { s with module_efficiency = v /. 100.0 })))
          ; slider_field
              ~label:"System losses"
              ~value:(state.system_losses *. 100.0)
              ~min:0.0
              ~max:40.0
              ~step:1.0
              ~format:(fun v -> Printf.sprintf "%g%%" v)
              ~on_change:(set_float (fun v ->
                           set (fun s -> { s with system_losses = v /. 100.0 })))
          ; div ~attrs:[ Vdom.Attr.class_ "panel-title" ] [ text "Your bill" ]
          ; number_field
              ~label:"Electricity rate ($/kWh)"
              ~value:state.electricity_rate
              ~step:0.01
              ~on_change:(set_float (fun v -> set (fun s -> { s with electricity_rate = v })))
          ; number_field
              ~label:"Annual consumption (kWh)"
              ~value:state.annual_consumption_kwh
              ~step:500.0
              ~on_change:(set_float (fun v ->
                           set (fun s -> { s with annual_consumption_kwh = v })))
          ; number_field
              ~label:"Installed cost ($/watt)"
              ~value:state.cost_per_watt
              ~step:0.1
              ~on_change:(set_float (fun v -> set (fun s -> { s with cost_per_watt = v })))
          ]
      in
      (* -- the results -- *)
      let payback =
        match fin.S.payback_years with
        | Some p -> Printf.sprintf "%.1f" p
        | None -> "—"
      in
      let results =
        div
          ~attrs:[ Vdom.Attr.class_ "panel results" ]
          [ div
              ~attrs:[ Vdom.Attr.class_ "headline" ]
              [ div ~attrs:[ Vdom.Attr.class_ "headline-value" ] [ text (thousands ac) ]
              ; div ~attrs:[ Vdom.Attr.class_ "headline-unit" ] [ text "kWh a year" ]
              ; div
                  ~attrs:[ Vdom.Attr.class_ "headline-note" ]
                  [ text
                      (Printf.sprintf
                         "%.0f m² of panels on your roof, under a clear sky."
                         (S.array_area_m2 ~spec))
                  ]
              ]
          ; div
              ~attrs:[ Vdom.Attr.class_ "readouts" ]
              [ readout
                  ~value:(money fin.S.annual_savings)
                  ~unit:"a year"
                  ~label:"Saved on your bill"
              ; readout ~value:payback ~unit:"years" ~label:"Payback period"
              ; readout
                  ~value:(Printf.sprintf "%.0f" fin.S.lifetime_co2_tonnes)
                  ~unit:"tonnes"
                  ~label:"CO2 avoided over 25 years"
              ; readout
                  ~value:(money fin.S.lifetime_savings)
                  ~unit:"over 25 years"
                  ~label:"Total saved"
              ]
          ; div
              ~attrs:[ Vdom.Attr.class_ "chart-note" ]
              [ text
                  "Over 25 years: electricity rising 3% a year, panels losing 0.5% a year.                    Payback is the simple kind — the cost divided by the first year's saving."
              ]
          ; div
              ~attrs:[ Vdom.Attr.class_ "chart-block" ]
              [ div ~attrs:[ Vdom.Attr.class_ "chart-title" ] [ text "Production through the year" ]
              ; div
                  ~attrs:[ Vdom.Attr.class_ "chart-frame" ]
                  [ fst (monthly_chart months ~width:600.0 ~height:160.0) ]
              ; div
                  ~attrs:[ Vdom.Attr.class_ "chart-note" ]
                  [ text (snd (monthly_chart months ~width:600.0 ~height:160.0)) ]
              ]
          ; div
              ~attrs:[ Vdom.Attr.class_ "chart-block" ]
              [ div
                  ~attrs:[ Vdom.Attr.class_ "chart-title" ]
                  [ text
                      (Printf.sprintf
                         "Today's sun — solar noon at %s"
                         (clock_string (S.solar_noon_clock_minutes ~location:state.location ~day:today)))
                  ]
              ; div
                  ~attrs:[ Vdom.Attr.class_ "chart-frame" ]
                  [ sun_path_chart ~location:state.location ~day:today ~spec ~width:600.0 ~height:150.0 ]
              ; div
                  ~attrs:[ Vdom.Attr.class_ "chart-note" ]
                  [ text
                      "The dashed line is your roof's normal, standing on the horizon; the arc is today's sun." ]
              ]
          ; div
              ~attrs:[ Vdom.Attr.class_ "facts" ]
              [ div
                  ~attrs:[ Vdom.Attr.class_ "fact" ]
                  [ div ~attrs:[ Vdom.Attr.class_ "fact-label" ] [ text "Sunlight on the panels" ]
                  ; div
                      ~attrs:[ Vdom.Attr.class_ "fact-value" ]
                      [ text (Printf.sprintf "%.0f kWh/m²" poa) ]
                  ]
              ; div
                  ~attrs:[ Vdom.Attr.class_ "fact" ]
                  [ div ~attrs:[ Vdom.Attr.class_ "fact-label" ] [ text "Array size" ]
                  ; div
                      ~attrs:[ Vdom.Attr.class_ "fact-value" ]
                      [ text
                          (Printf.sprintf
                             "%d panels · %.1f m² · %.1f kW"
                             state.panel_count
                             (S.array_area_m2 ~spec)
                             (S.array_watts ~spec /. 1000.0))
                      ]
                  ]
              ; div
                  ~attrs:[ Vdom.Attr.class_ "fact" ]
                  [ div ~attrs:[ Vdom.Attr.class_ "fact-label" ] [ text "Installed cost" ]
                  ; div
                      ~attrs:[ Vdom.Attr.class_ "fact-value" ]
                      [ text (money fin.S.installed_cost) ]
                  ]
              ]
          ]
      in
      div
        ~attrs:[ Vdom.Attr.class_ "page" ]
        [ div
            ~attrs:[ Vdom.Attr.class_ "masthead" ]
            [ div ~attrs:[ Vdom.Attr.class_ "masthead-title" ] [ text "Solar calculator" ]
            ; div
                ~attrs:[ Vdom.Attr.class_ "masthead-sub" ]
                [ text
                    "A clear-sky model of what your roof would collect — the geometry, \
                     solved in OCaml."
                ]
            ]
        ; div ~attrs:[ Vdom.Attr.class_ "layout" ] [ controls; results ] ])

let () = Bonsai_web.Start.start ~bind_to_element_with_id:"solar-app" page
;;
