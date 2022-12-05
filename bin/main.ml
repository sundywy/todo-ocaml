open Base
open Stdio
open Yojson.Safe

let input_text = "/Users/sundyyaputra/Projects/ocaml-projects/todo/.input.json"

type item = {
  id : int;
  task : string;
  finish : bool;
  created_at : float;
  completed_at : float option;
}
[@@deriving yojson { exn = true }]

type todo = AddTask of string | ListTask

let create_task_item id task =
  { id; task; finish = false; created_at = Unix.time (); completed_at = None }

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

let run_task task =
  let tasks = read_tasks () in
  match task with
  | AddTask todo_task ->
      add_task tasks (create_task_item (List.length tasks + 1) todo_task)
  | ListTask -> List.iter ~f:(fun x -> print_endline (to_string x)) tasks

let get_args () =
  let args = Sys.get_argv () |> Array.to_list in
  let texts = List.drop args 1 in
  if List.is_empty texts then ListTask
  else AddTask (String.concat ~sep:" " texts)

let () =
  let task = get_args () in
  run_task task
