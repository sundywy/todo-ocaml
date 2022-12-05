open Base
open Stdio
open Yojson.Safe
open Cmdliner

let input_text = "/Users/sundyyaputra/Projects/ocaml-projects/todo/.input.json"

type item = {
  id : int;
  task : string;
  finish : bool;
  created_at : float;
  completed_at : float option;
}
[@@deriving yojson { exn = true }]

let create_task_item id task_str =
  {
    id;
    task = String.concat ~sep:" " task_str;
    finish = false;
    created_at = Unix.time ();
    completed_at = None;
  }

let to_json_list tasks = `List (List.map ~f:item_to_yojson tasks)

let read_tasks () =
  from_file input_text |> Util.to_list |> List.map ~f:item_of_yojson_exn

let write_tasks tasks = to_file input_text (to_json_list tasks)

let add_task tasks task =
  let tasks = tasks @ [ task ] in
  write_tasks tasks

let to_string { task; finish; _ } =
  let complete = if finish then "[x]" else "[ ]" in
  String.concat ~sep:" " [ complete; task ]

let list_task () =
  let tasks = read_tasks () in
  List.iter ~f:(fun x -> print_endline (to_string x)) tasks

let list_task_cmd = Cmd.v (Cmd.info "list") Term.(const list_task $ const ())

let add_task task_strings =
  let tasks = read_tasks () in
  add_task tasks (create_task_item (List.length tasks + 1) task_strings)

let add_task_cmd =
  let task = Arg.(non_empty & pos_all string [] & info []) in
  Cmd.v (Cmd.info "add") Term.(const add_task $ task)

let cmd = Cmd.group (Cmd.info "todo") [ list_task_cmd; add_task_cmd ]
let () = Caml.exit (Cmd.eval cmd)
