
open Log

module StrHashtbl = CCHashtbl.Make(CCString)

let executors: Ctrl.executor StrHashtbl.t = StrHashtbl.create 7

let add_executor (name: string) (value: Ctrl.executor): unit =
  StrHashtbl.add executors name value

type invocation_result = (bool, exn) result

let invocation_result_to_yojson (src: invocation_result): Yojson.Safe.json =
  match src with
  | Ok result -> `Assoc [("ok", `Int 1); ("data", `Bool result)]
  | Error err -> `Assoc [("ok", `Int 0); ("err", `String (Printexc.to_string err))]

exception Invocation_error of string

let dispatch_command (cmd: Ctrl.command): invocation_result Lwt.t =
  match StrHashtbl.get executors cmd.Ctrl.task with
  | None -> Lwt.return (Error (Invocation_error "unknown backend"))
  | Some executor -> Lwt.map (fun x -> Ok x) (executor cmd)

let main_handler (_, conn) req body =
  ignore conn;
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/ws" ->
    let%lwt _ = Cohttp_lwt.Body.drain_body body in
    let out_fn: (Websocket.Frame.t option -> unit) ref = ref (fun _ -> ()) in
    let reply (data: invocation_result): unit =
      let str = data |> invocation_result_to_yojson |> Yojson.Safe.to_string in
      (!out_fn) (Some (Websocket.Frame.create ~content:str ()))
    in
    let%lwt (resp, frames_out_fn) =
    Websocket_cohttp_lwt.upgrade_connection req (fun { opcode; content; _ } ->
          match opcode with
          | Websocket.Frame.Opcode.Close -> ()
          | Websocket.Frame.Opcode.Text -> (
            log (sprintf "received %s" content);
            let cmd_json = Yojson.Safe.from_string content in
            match Ctrl.command_of_yojson cmd_json with
            | Ok cmd -> ignore (
                let%lwt ret = dispatch_command cmd in
                Lwt.return (reply ret)
              )
            | Error err -> reply (Error (Yojson.Json_error err)))
          | _ -> log "unknown frame"; reply (Error (Invocation_error "unknown frame"))
        )
    in
    out_fn := frames_out_fn;
    Lwt.return resp
  | path ->
    log (sprintf "%s not found" path);
    let%lwt resp = Cohttp_lwt_unix.Server.respond_string
      ~status: `Not_found
      ~body: "Not Found" ()
    in Lwt.return (`Response resp)

let conn_closed _: unit =
  log "connection closed"

let start_server (port: int) =
  let mode = `TCP (`Port port) in
  Cohttp_lwt_unix.Server.create ~mode
    (Cohttp_lwt_unix.Server.make_response_action
      ~callback:main_handler ~conn_closed())

let _ =
  print_endline "Hello World!";
  add_executor "dummy" Exec_dummy.executor;
  let port = 60437 in
  Lwt_main.run (start_server port)
