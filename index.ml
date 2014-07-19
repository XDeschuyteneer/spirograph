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

let stroke_line ctx a b =
  let x1, y1 = a
  and x2, y2 = b in
  ctx##beginPath ();
  ctx##moveTo (x1, y1);
  ctx##lineTo (x2, y2);
  ctx##closePath ();
  ctx##stroke ();;

let clear_canvas ctx x y w h=
  ctx##beginPath ();
  ctx##clearRect (x, y, w, h);
  ctx##closePath ();
  ctx##stroke ();;

let init_time = Unix.time ();;

let rec loop ctx () =
  begin
    debug "rendering";
    let t = Unix.time () in
    let ellapsed_time = (int_of_float (t -. init_time)) in
    let factor = 10 * ellapsed_time in
    if (100 + factor < 500) then
      begin
        let a = (50 + factor, 50)
        and b = (100 + factor, 100) in
        clear_canvas ctx 0 0 500 500;
        stroke_line ctx a b;
      end;
    Dom_html._requestAnimationFrame (Js.wrap_callback (loop ctx))
  end;;

Dom_html.window##onload <- Dom.handler (fun _ ->
                                        begin
                                          let color = create_color 0 0 0 in
                                          let canvas = create_canvas 500 500 in
                                          let ctx = get_context canvas in
                                          ctx##lineWidth <- 10.;
                                          ctx##strokeStyle <- Js.string color;
                                          Firebug.console##log (Js.string "loaded-ocaml");
                                          Dom_html._requestAnimationFrame (Js.wrap_callback (loop ctx));
                                          Js._true
                                        end);;
