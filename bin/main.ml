open Base
open Stdio

let input_text = "./input.txt"

type todo = AddTask of string | ListTask

let read_file () = In_channel.read_lines input_text

let run_task task =
  let tasks = read_file () in
  let add_task text = Out_channel.write_lines input_text (tasks @ [ text ]) in
  match task with
  | AddTask text -> add_task text
  | ListTask -> List.iter ~f:print_endline tasks

let () =
  let args = Sys.get_argv () |> Array.to_list in
  let texts = List.drop args 1 in
  let task =
    if List.is_empty texts then ListTask
    else AddTask (String.concat ~sep:" " texts)
  in
  run_task task
