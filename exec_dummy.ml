
open Log

let executor (cmd: Ctrl.command): bool Lwt.t =
  log (sprintf "command received %s" (cmd |> Ctrl.command_to_yojson |> Yojson.Safe.to_string));
  Lwt.return true
