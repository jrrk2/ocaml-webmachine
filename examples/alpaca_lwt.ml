(** An example ASCOM alpaca API, somewhat as described in https://www.ascom-standards.org/Documentation/Index.htm#dev *)

open Cohttp_lwt_unix
open Lwt.Infix

let serv_id = ref 0
let xsize = 3072
let ysize = 2080
let alpacah = Hashtbl.create 255

let _ = Hashtbl.replace alpacah ("camera", [("connected","abortexposure")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("camera", [("connected","bayeroffsetx")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("camera", [("connected","bayeroffsety")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("camera", [("connected","binx")]) (`Int 1)
let _ = Hashtbl.replace alpacah ("camera", [("connected","biny")]) (`Int 1)
let _ = Hashtbl.replace alpacah ("camera", [("connected","camerastate")]) (`Bool false)
let _ = Hashtbl.replace alpacah ("camera", [("connected","cameraxsize")]) (`Int xsize)
let _ = Hashtbl.replace alpacah ("camera", [("connected","cameraysize")]) (`Int ysize)
let _ = Hashtbl.replace alpacah ("camera", [("connected","canfastreadout")]) (`Bool false)
let _ = Hashtbl.replace alpacah ("camera", [("connected","cangetcoolerpower")]) (`Bool false)
let _ = Hashtbl.replace alpacah ("camera", [("connected","cansetccdtemperature")]) (`Bool false)
let _ = Hashtbl.replace alpacah ("camera", [("connected","ccdtemperature")]) (`Float 20.0)
let _ = Hashtbl.replace alpacah ("camera", [("connected","connected")]) (`Bool true)
let _ = Hashtbl.replace alpacah ("camera", [("connected","cooleron")]) (`Bool false)
let _ = Hashtbl.replace alpacah ("camera", [("connected","coolerpower")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("camera", [("connected","driverinfo")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("camera", [("connected","driverversion")]) (`Int 1)
let _ = Hashtbl.replace alpacah ("camera", [("connected","exposuremax")]) (`Float 60.0)
let _ = Hashtbl.replace alpacah ("camera", [("connected","exposuremin")]) (`Float 0.001)
let _ = Hashtbl.replace alpacah ("camera", [("connected","exposureresolution")]) (`Float 0.000001)
let _ = Hashtbl.replace alpacah ("camera", [("connected","gain")]) (`Int 200)
let _ = Hashtbl.replace alpacah ("camera", [("connected","gainmax")]) (`Int 430)
let _ = Hashtbl.replace alpacah ("camera", [("connected","gainmin")]) (`Int 0)
let _ = Hashtbl.replace alpacah ("camera", [("connected","gains")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("camera", [("connected","imagearray")]) (`List [])
let _ = Hashtbl.replace alpacah ("camera", [("connected","imageready")]) (`Bool true)
let _ = Hashtbl.replace alpacah ("camera", [("connected","interfaceversion")]) (`Int 1)
let _ = Hashtbl.replace alpacah ("camera", [("connected","maxadu")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("camera", [("connected","maxbinx")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("camera", [("connected","maxbiny")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("camera", [("connected","name")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("camera", [("connected","numx")]) (`Int xsize)
let _ = Hashtbl.replace alpacah ("camera", [("connected","numy")]) (`Int ysize)
let _ = Hashtbl.replace alpacah ("camera", [("connected","pixelsizex")]) (`Float 1.23799)
let _ = Hashtbl.replace alpacah ("camera", [("connected","pixelsizey")]) (`Float 1.23799)
let _ = Hashtbl.replace alpacah ("camera", [("connected","sensorname")]) (`String "SONY")
let _ = Hashtbl.replace alpacah ("camera", [("connected","sensortype")]) (`String "CMOS")
let _ = Hashtbl.replace alpacah ("camera", [("connected","startexposure")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("camera", [("connected","startx")]) (`Int 0)
let _ = Hashtbl.replace alpacah ("camera", [("connected","starty")]) (`Int 0)
let _ = Hashtbl.replace alpacah ("camera", [("connected","readoutmodes")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","abortslew")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","slewtocoordinates")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("focuser", [("interfaceversion","absolute")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("focuser", [("interfaceversion","connected")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("focuser", [("interfaceversion","driverinfo")]) (`Int 1)
let _ = Hashtbl.replace alpacah ("focuser", [("interfaceversion","driverversion")]) (`Int 1)
let _ = Hashtbl.replace alpacah ("focuser", [("interfaceversion","interfaceversion")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("focuser", [("interfaceversion","name")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("focuser", [("interfaceversion","position")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("focuser", [("interfaceversion","temperature")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("focuser", [("interfaceversion","ismoving")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("focuser", [("interfaceversion","maxstep")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("focuser", [("interfaceversion","move")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("rotator", [("connected","connected")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("rotator", [("connected","driverinfo")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("rotator", [("connected","driverversion")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("rotator", [("connected","interfaceversion")]) (`Int 1)
let _ = Hashtbl.replace alpacah ("rotator", [("connected","position")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("rotator", [("connected","ismoving")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("rotator", [("connected","moveabsolute")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","atpark")]) (`Bool false)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","canpark")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","canpulseguide")]) (`Bool false)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","cansetpierside")]) (`Bool false)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","cansettracking")]) (`Bool true)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","canslew")]) (`Bool true)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","canslewasync")]) (`Bool false)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","cansync")]) (`Bool false)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","connected")]) (`Bool true)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","declination")]) (`Float 89.2641)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","driverinfo")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","driverversion")]) (`Int 1)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","equatorialsystem")]) (`String "Local")
let _ = Hashtbl.replace alpacah ("telescope", [("connected","focallength")]) (`Float 400.0)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","interfaceversion")]) (`Int 1)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","name")]) (`String "MyStellina")
let _ = Hashtbl.replace alpacah ("telescope", [("connected","rightascension")]) (`Float 37.95456)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","sideofpier")]) (`String "unknown")
let _ = Hashtbl.replace alpacah ("telescope", [("connected","siteelevation")]) (`Float 12.0)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","sitelatitude")]) (`Float 52.2451)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","sitelongitude")]) (`Float 0.0795)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","tracking")]) (`Bool false)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","utcdate")]) (`Float 1655709051.0)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","unpark")]) (`Bool true)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","abortslew")]) (`Int 2)
let _ = Hashtbl.replace alpacah ("telescope", [("connected","slewtocoordinates")]) (`Int 2)

(* Apply the [Webmachine.Make] functor to the Lwt_unix-based IO module
 * exported by cohttp. For added convenience, include the [Rd] module
 * as well so you don't have to go reaching into multiple modules to
 * access request-related information. *)
module Wm = struct
  module Rd = Webmachine.Rd
  module UnixClock = struct
    let now = fun () -> int_of_float (Unix.gettimeofday ())
  end
  include Webmachine.Make(Cohttp_lwt_unix__Io)(UnixClock)
end

type image_bytes = {
    _MetadataVersion: Int32.t;         (* Bytes 0..3 - Metadata version = 1 *)
    _ErrorNumber: Int32.t;             (* Bytes 4..7 - Alpaca error number or zero for success *)
    _ClientTransactionID: Int32.t;     (* Bytes 8..11 - Client's transaction ID *)
    _ServerTransactionID: Int32.t;     (* Bytes 12..15 - Device's transaction ID *)
    _DataStart: Int32.t;               (* Bytes 16..19 - Offset of the start of the data bytes = 36 for version 1 *)
    _ImageElementType: Int32.t;        (* Bytes 20..23 - Element type of the source image array *)
    _TransmissionElementType: Int32.t; (* Bytes 24..27 - Element type as sent over the network *)
    _Rank: Int32.t;                    (* Bytes 28..31 - Image array rank *)
    _Dimension1: Int32.t;              (* Bytes 32..35 - Length of image array first dimension *)
    _Dimension2: Int32.t;              (* Bytes 36..39 - Length of image array second dimension *)
    _Dimension3: Int32.t;              (* Bytes 40..43 - Length of image array third dimension (0 for 2D array) *)
}

let image_bytes = 
{
    _MetadataVersion=1l;         (* Bytes 0..3 - Metadata version = 1 *)
    _ErrorNumber=0l;             (* Bytes 4..7 - Alpaca error number or zero for success *)
    _ClientTransactionID=0l;     (* Bytes 8..11 - Client's transaction ID *)
    _ServerTransactionID=0l;     (* Bytes 12..15 - Device's transaction ID *)
    _DataStart=44l;              (* Bytes 16..19 - Offset of the start of the data bytes = 36 for version 1 *)
    _ImageElementType=2l;        (* Bytes 20..23 - Element type of the source image array *)
    _TransmissionElementType=8l; (* Bytes 24..27 - Element type as sent over the network *)
    _Rank=2l;                    (* Bytes 28..31 - Image array rank *)
    _Dimension1=Int32.of_int xsize;           (* Bytes 32..35 - Length of image array first dimension *)
    _Dimension2=Int32.of_int ysize;           (* Bytes 36..39 - Length of image array second dimension *)
    _Dimension3=0l;              (* Bytes 40..43 - Length of image array third dimension (0 for 2D array) *)
}

let rs = ref (Rgb24.create 32 32)

let image_bytes_extract = function
   | 0 -> image_bytes._MetadataVersion         (* Bytes 0..3 - Metadata version = 1 *)
   | 1 -> image_bytes._ErrorNumber;            (* Bytes 4..7 - Alpaca error number or zero for success *)
   | 2 -> image_bytes._ClientTransactionID     (* Bytes 8..11 - Client's transaction ID *)
   | 3 -> image_bytes._ServerTransactionID     (* Bytes 12..15 - Device's transaction ID *)
   | 4 -> image_bytes._DataStart               (* Bytes 16..19 - Offset of the start of the data bytes = 36 for version 1 *)
   | 5 -> image_bytes._ImageElementType        (* Bytes 20..23 - Element type of the source image array *)
   | 6 -> image_bytes._TransmissionElementType (* Bytes 24..27 - Element type as sent over the network *)
   | 7 -> image_bytes._Rank                    (* Bytes 28..31 - Image array rank *)
   | 8 -> image_bytes._Dimension1              (* Bytes 32..35 - Length of image array first dimension *)
   | 9 -> image_bytes._Dimension2              (* Bytes 36..39 - Length of image array second dimension *)
   | 10 -> image_bytes._Dimension3             (* Bytes 40..43 - Length of image array third dimension (0 for 2D array) *)
   | n -> let x = n-11 in let rgb = Rgb24.get !rs (x / ysize) (x mod ysize) in Int32.of_int ((rgb.r + rgb.g + rgb.b) * 32767 / 765)

let process rgb24 =
rs := (Rgb24.resize None rgb24 xsize ysize)

let jpeg_open =
  let ximage = Jpeg.load "IMG_0002.jpeg" [] in
  let _ = match ximage with
   | Index8 index8 -> ignore index8; print_endline "index8"
   | Rgb24 rgb24 -> print_endline "rgb24"; process rgb24
   | Index16 index16 -> ignore index16; print_endline "index16"
   | Rgba32 rgba32 -> ignore rgba32; print_endline "rgba32"
   | Cmyk32 cmyk32 -> ignore cmyk32; print_endline "cmyk32 in"
  in
(*
  print_endline (string_of_int ximage#width);
  print_endline (string_of_int ximage#height);
*)
  ()

let dummy_image_string =
   String.init (Int32.to_int image_bytes._DataStart + xsize*ysize*2) (fun ix -> char_of_int (Int32.to_int (Int32.logand 255l (Int32.shift_right (image_bytes_extract (ix/4)) ((ix mod 4)*8)))))

class alpaca dev = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  method private get_body rd id =
(*
    let lst' = Uri.query rd.Wm.Rd.uri in
    let flatten = List.map (fun (itm,value) -> match itm with ("ClientTransactionID"|"ClientID") -> "(\"\",\"\")" | _ -> "(\""^itm^"\",[\""^String.concat "\";\"" value^"\"]") lst' in
    let flatten' = ("["^String.concat ";" flatten^"],") in
    ignore flatten';
*)
    let lst' = rd.Wm.Rd.path_info in
    let info = List.map (fun (itm,value) -> "(\""^itm^"\",\""^value^"\")") lst' in
    let info' = ("["^String.concat ";" info^"]") in
    let value = match Hashtbl.find_opt alpacah (dev, lst') with
        | Some contents ->
            print_endline ("(* (\""^dev^"\", "^info'^") ("^Yojson.Basic.to_string contents^") *)");
            contents
        | None ->
            print_endline ("let _ = Hashtbl.replace alpacah (\""^dev^"\", "^info'^") (`Int 2)");
            `Int 2 in
    let json = Yojson.Basic.to_string (`Assoc
      [("Value", value); ("ClientTransactionID", `Int (int_of_string id));
       ("ServerTransactionID", `Int !serv_id); ("ErrorNumber", `Int 0);
       ("ErrorMessage", `String "")]) in
    print_endline json;
    json

  method private of_urlencoded rd =
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
      let lst = String.split_on_char '&' body in
      let lst' = List.map (fun itm -> match String.split_on_char '=' itm with [a;b] -> (a,b) | _ -> ("","")) lst in
      (match lst' with
             | [("RightAscension", ra); ("Declination", dec); ("ClientID", _); ("ClientTransactionID", _)] ->
                 Hashtbl.replace alpacah ("telescope", [("connected","rightascension")]) (`Float (float_of_string ra));
                 Hashtbl.replace alpacah ("telescope", [("connected","declination")]) (`Float (float_of_string dec));
                 List.iter (fun (itm,value) -> print_endline (dev^": "^itm^"="^value)) lst';
             | _ -> ());
      incr serv_id;
      let body = self#get_body rd (List.assoc "ClientTransactionID" lst') in
      let resp_body = `String body in
      Wm.continue true { rd with Wm.Rd.resp_body }

  method private what rd =
    try Wm.Rd.lookup_path_info_exn "what" rd with Not_found -> "world"

  method private to_json rd =
    let lst' = Uri.query rd.Wm.Rd.uri in
    let json = self#get_body rd (String.concat "" (List.assoc "ClientTransactionID" lst')) in
    print_endline ("to_json: "^string_of_int (String.length json));
    Wm.continue (`String json) rd

  method private to_imagebytes rd =
      print_endline ("to_imagebytes: "^string_of_int (String.length dummy_image_string));
    Wm.continue (`String dummy_image_string) rd

  method! allowed_methods rd =
    Wm.continue [`GET; `PUT; `POST] rd

  method content_types_provided rd =
    Wm.continue [
      "application/json", self#to_json;
      "application/imagebytes", self#to_imagebytes;
    ] rd

  method content_types_accepted rd =
    Wm.continue [
      "application/x-www-form-urlencoded", self#of_urlencoded;
    ] rd

(*

  method! finish_request rd =
    let lst' = Uri.query rd.Wm.Rd.uri in
    let len = List.length lst' in
    if true then print_endline ("finish_request: "^string_of_int len);
    Wm.continue () rd

  method! options rd =
    print_endline "options";
    Wm.continue [] rd

  method! variances rd =
    print_endline "variances";
    let lst' = rd.Wm.Rd.path_info in
    List.iter (fun (itm,value) -> print_endline ("process_get: "^dev^": "^itm^"="^value)) lst';
    Wm.continue [] rd

  method! valid_content_headers rd =
    print_endline "valid_content_headers";
    let lst' = rd.Wm.Rd.path_info in
    List.iter (fun (itm,value) -> print_endline ("process_get: "^dev^": "^itm^"="^value)) lst';
    Wm.continue true rd

  method! malformed_request rd =
    print_endline "malformed_request";
    let lst' = Uri.query rd.Wm.Rd.uri in
    List.iter (fun (itm,value) -> print_endline ("process_get: "^dev^": "^itm^"="^String.concat ";" value)) lst';
    Wm.continue true rd
*)

end

let main () =
  (* listen on port 11111 *)
  let port = 11111 in
  (* the route table *)
  let routes = [
    ("/api/v1/camera/0/:connected", fun () -> new alpaca "camera") ;
    ("/api/v1/telescope/0/:connected", fun () -> new alpaca "telescope") ;
    ("/api/v1/rotator/0/:connected", fun () -> new alpaca "rotator") ;
    ("/api/v1/focuser/0/:interfaceversion", fun () -> new alpaca "focuser") ;
  ] in
  let callback (_ch, _conn) request body =
    let open Cohttp in
    (* Perform route dispatch. If [None] is returned, then the URI path did not
     * match any of the route patterns. In this case the server should return a
     * 404 [`Not_found]. *)
    Wm.dispatch' routes ~body ~request
    >|= begin function
      | None        -> (`Not_found, Header.init (), `String "Not found", [])
      | Some result -> result
    end
    >>= fun (status, headers, body, path) ->
      (* If you'd like to see the path that the request took through the
       * decision diagram, then run this example with the [DEBUG_PATH]
       * environment variable set. This should suffice:
       *
       *  [$ DEBUG_PATH= ./crud_lwt.native]
       *
       *)
      let path = Printf.sprintf " - %s" (String.concat ", " path) in
      Printf.eprintf "%d - %s %s%s"
        (Code.code_of_status status)
        (Code.string_of_method (Request.meth request))
        (Uri.path (Request.uri request))
        path;
      (* Finally, send the response to the client *)
      Server.respond ~headers ~body ~status ()
  in
  (* create the server and handle requests with the function defined above *)
  let conn_closed (ch, _conn) =
    Printf.printf "connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  let config = Server.make ~callback ~conn_closed () in
  Server.create  ~mode:(`TCP(`Port port)) config
  >>= (fun () -> Printf.eprintf "hello_lwt: listening on 0.0.0.0:%d%!" port;
      Lwt.return_unit)

let () =  Lwt_main.run (main ())
