
open Printf

let sprintf = sprintf

let log_gettime (): string =
  let tm = Unix.gettimeofday () |> Unix.localtime in
  sprintf "%d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let log (msg: string): unit =
  printf "%s - %s\n" (log_gettime ()) msg;
  flush stdout
