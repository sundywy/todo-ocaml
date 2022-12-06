open Base
open Stdio
open Yojson.Safe

let input_text = ".input.json"

type item = {
  id : int;
  task : string;
  finish : bool;
  created_at : float;
  completed_at : float option;
}
[@@deriving yojson { exn = true }]

let get_latest_id tasks =
  List.last tasks |> Option.value_map ~default:1 ~f:(fun x -> x.id + 1)

let create_task_item id task =
  { id; task; finish = false; created_at = Unix.time (); completed_at = None }

let to_json_list tasks = `List (List.map ~f:item_to_yojson tasks)

let read_tasks () =
  try from_file input_text |> Util.to_list |> List.map ~f:item_of_yojson_exn
  with Sys_error _ -> []

let write_tasks tasks = to_file input_text (to_json_list tasks)

let add_task tasks task =
  let tasks = tasks @ [ task ] in
  write_tasks tasks

let to_string { task; finish; id; _ } =
  let complete = if finish then "[X]" else "[ ]" in
  Printf.sprintf "%s %d: %s" complete id task

let list_task () =
  let tasks = read_tasks () in
  List.iter ~f:(fun x -> print_endline (to_string x)) tasks

let add_task task_strings =
  let tasks = read_tasks () in
  let task = String.concat ~sep:" " task_strings in
  add_task tasks (create_task_item (get_latest_id tasks) task)

let rec find_and_filter ~p = function
  | [] -> (false, [])
  | x :: xs ->
      if p x then (true, xs)
      else
        let _, tasks' = find_and_filter ~p xs in
        (false, x :: tasks')

exception TaskNotFound

let delete_task id =
  let found, tasks' =
    read_tasks () |> find_and_filter ~p:(fun x -> x.id = id)
  in
  if found then write_tasks tasks' else raise TaskNotFound

let rec find_and_map ~f ~p = function
  | [] -> (false, [])
  | x :: xs ->
      if p x then (true, f x :: xs)
      else
        let _, tasks' = find_and_map ~f ~p xs in
        (false, x :: tasks')

let complete_task id =
  let found, tasks' =
    read_tasks ()
    |> find_and_map
         ~p:(fun x -> x.id = id)
         ~f:(fun x ->
           { x with finish = true; completed_at = Some (Unix.time ()) })
  in
  if found then write_tasks tasks' else raise TaskNotFound
