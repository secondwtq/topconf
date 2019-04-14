
type command_kind =
  | Ping
  | Toggle (* Toggle play/pause *)
  | Backward
  | Forward
  | Prev
  | Next
[@@deriving yojson]

type auto_task_kind =
  | Front
  | Back
[@@deriving yojson]

type task =
  | Automatic of auto_task_kind
  | Specific of string
[@@deriving yojson]

type command = {
  task: task;
  kind: command_kind;
}
[@@deriving yojson]

(* {"task": "youtube", "kind": ["Toggle"]} *)

type executor = command -> bool Lwt.t
