open Lwt
open Js
module Html = Dom_html

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
  CSS.Color.string_of_t (CSS.Color.rgb r g b)

let get_context canvas =
  canvas##getContext (Html._2d_)

let stroke_line ?(color=(create_color 0 0 0)) ctx a b =
  let x1, y1 = a
  and x2, y2 = b in
  ctx##lineWidth <- 10.;
  ctx##strokeStyle <- Js.string color;
  ctx##beginPath ();
  ctx##moveTo (x1, y1);
  ctx##lineTo (x2, y2);
  ctx##closePath ();
  ctx##stroke ();;

Dom_html.window##onload <- Dom.handler (fun _ ->
                                        begin
                                          let color = create_color 255 0 0 in
                                          let canvas = create_canvas 500 500 in
                                          let ctx = get_context canvas in
                                          let a = (50, 50)
                                          and b = (100, 100) in
                                          stroke_line ctx a b;
                                          Firebug.console##log (Js.string "loaded-ocaml");
                                          Js._true
                                        end)
