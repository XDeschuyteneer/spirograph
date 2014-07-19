open Lwt
open Js
module Html = Dom_html

let pi = 4.0 *. atan 1.0;;

let error f = Printf.ksprintf (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf (fun s -> Firebug.console##log(Js.string s)) f
let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert(Js.string s); failwith s) f

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

module Timer = struct
  let timer = ref (fun _ -> 0.)
  let init () = timer := (fun _ -> Unix.time ())
  let make () = !timer ()
  let get t = !timer () -. t
end

Timer.init ();;
let initial_time = ref (Timer.make ());;

let r1 = 90.;;
let r2 = 30.;;
let r3 = 50.;;
let cx, cy = 200., 200.;;

let inner_center cx cy teta =
  let r = r1 -. r2 in
  cx +. r *. cos teta, cy +. r *. sin teta;;

let pen cx cy teta =
  let teta = -. teta in
  let r = r3 in
  cx +. r *. cos teta, cy +. r *. sin teta;;

let alpha = ref 0.;;

let deg_of_rad teta = teta *. (200. /. pi);;
let rad_of_deg teta = teta *. (pi /. 200.);;

let rec loop ctx () =
  begin
    let timer = Timer.get !initial_time in
    if (timer >= 1.) then
      begin
        debug "alpha: %f" !alpha;
        alpha := !alpha +. (rad_of_deg 20.);
        clear_canvas ctx 0 0 500 500;
        stroke_circle ctx cx cy r1;
        (* let cx', cy' = cx +. r1 -. r2, cy in *)
        let cx', cy' = inner_center cx cy !alpha in
        stroke_circle ctx cx' cy' r2;
        ctx##strokeStyle <- Js.string (create_color 255 0 0);
        let a1, a2 = pen cx' cy' !alpha in
        stroke_line ctx cx' cy' a1 a2;
        (* stroke_circle ctx cx' cy' r3; *)
        ctx##strokeStyle <- Js.string (create_color 0 0 0);
        initial_time := Timer.make ();
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
                                          Firebug.console##log (Js.string "loaded-ocaml");
                                          Dom_html._requestAnimationFrame (Js.wrap_callback (loop ctx));
                                          Js._true
                                        end);;
