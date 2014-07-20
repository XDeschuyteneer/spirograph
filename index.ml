open Lwt
open Js
module Html = Dom_html

let pi = 4.0 *. atan 1.0;;

let deg_of_rad teta = teta *. (200. /. pi);;
let rad_of_deg teta = teta *. (pi /. 200.);;

let error f = Printf.ksprintf (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf (fun s -> Firebug.console##log(Js.string s)) f
let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert(Js.string s); failwith s) f

let by_id_coerce s f  =
  Js.Opt.get
    (f (Dom_html.getElementById s))
    (fun () -> raise Not_found);;

let get_input id =
  by_id_coerce id Dom_html.CoerceTo.input;;

let get_button id =
  by_id_coerce id Dom_html.CoerceTo.button;;

let set_button_click_event button f =
  button##onclick <-
    Html.handler
      (fun ev ->
       begin
         f ();
         Js._true;
       end);;

let create_canvas w h =
  let d = Html.window##document in
  let c = Html.createCanvas d in
  c##width <- w;
  c##height <- h;
  Dom.appendChild Html.window##document##body c;
  c;;

let create_color r g b =
  CSS.Color.string_of_t (CSS.Color.rgb r g b);;

let get_context canvas =
  canvas##getContext (Html._2d_);;

let stroke_rect ctx x y w h =
  ctx##beginPath ();
  ctx##rect (x, y, w, h);
  ctx##closePath ();
  ctx##stroke ();;

let stroke_circle ctx x y radius =
  ctx##beginPath ();
  ctx##arc (x, y, radius, 0, (2. *. pi), Js._true);
  ctx##closePath ();
  ctx##stroke ();;

let stroke_line ctx x1 y1 x2 y2 =
  ctx##beginPath ();
  ctx##moveTo (x1, y1);
  ctx##lineTo (x2, y2);
  ctx##closePath ();
  ctx##stroke ();;

let move_to ctx a =
  let x, y = a in
  ctx##moveTo (x, y);;

let clear_canvas ctx x y w h=
  ctx##beginPath ();
  ctx##clearRect (x, y, w, h);
  ctx##closePath ();
  ctx##stroke ();;


let r1 = 100.;;
let r2 = 30.;;
let r3 = 20.;;
let cx, cy = 200., 200.;;

let inner_center cx cy teta =
  let r = r1 -. r2 in
  cx +. r *. cos teta, cy +. r *. sin teta;;

let pen cx cy teta =
  let teta = -. teta in
  let r = r3 in
  cx +. r *. cos teta, cy +. r *. sin teta;;

let spirograph r k l t =
  let a = 1. -. k
  and b = (1. -. k) /. k in
  let x = r *. (a *. cos t +. l *. k *. cos (b *. t))
  and y = r *. (a *. sin t -. l *. k *. sin (b *. t)) in
  x, y;;

let alpha = ref 0.;;
let delta_teta = 5.;;
let delta_time = 1. /. 60.;;

let paused = ref false;;

let rec loop ctx () =
  begin
    if not !paused then
      begin
        let r = by_id_coerce "r" Dom_html.CoerceTo.input in
        let r' = by_id_coerce "R" Dom_html.CoerceTo.input in
        let r_value = (float_of_string (to_string r##value)) in
        let r'_value = (float_of_string (to_string r'##value)) in
        let color = (get_input "color_picker")##value in
        let a, b = if (r_value >= r'_value) then
                     let l = !alpha /. r'_value in
                     let k = 1. in
                     begin
                       spirograph r'_value k l !alpha
                     end
                   else
                     let l = delta_teta /. r_value in
                     let k = r_value /. r'_value in
                     begin
                       spirograph r'_value 0.7 k !alpha
                     end
        in
        ctx##strokeStyle <- color;
        stroke_rect ctx (a +. 200.) (b +. 200.) 1. 1.;
        (* calculate new alpha *)
        alpha := !alpha +. (rad_of_deg delta_teta);
      end;
    Dom_html._requestAnimationFrame (Js.wrap_callback (loop ctx));
  end;;

Dom_html.window##onload <- Dom.handler (fun _ ->
                                        begin
                                          let color = create_color 0 0 0 in
                                          let canvas = create_canvas 500 500 in
                                          let ctx = get_context canvas in
                                          ctx##lineWidth <- 1.;
                                          ctx##strokeStyle <- Js.string color;
                                          let button = get_button "play_pause" in
                                          let f () =
                                            begin
                                              paused := not !paused;
                                              if !paused then
                                                button##innerHTML <- (Js.string "play")
                                              else
                                                button##innerHTML <- (Js.string "pause");
                                            end in
                                          set_button_click_event button f;
                                          Firebug.console##log (Js.string "loaded-ocaml");
                                          Dom_html._requestAnimationFrame (Js.wrap_callback (loop ctx));
                                          Js._true
                                        end);;
