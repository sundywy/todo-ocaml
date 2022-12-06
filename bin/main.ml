open Cmdliner
open Todo

let list_task_cmd = Cmd.v (Cmd.info "list") Term.(const list_task $ const ())

let add_task_cmd =
  let task = Arg.(non_empty & pos_all string [] & info []) in
  Cmd.v (Cmd.info "add") Term.(const add_task $ task)

let delete_task_cmd =
  let id = Arg.(required & pos 0 (some int) None & info []) in
  Cmd.v (Cmd.info "delete") Term.(const delete_task $ id)

let complete_task_cmd =
  let id = Arg.(required & pos 0 (some int) None & info []) in
  Cmd.v (Cmd.info "complete") Term.(const complete_task $ id)

let cmd =
  Cmd.group (Cmd.info "todo")
    [ list_task_cmd; add_task_cmd; delete_task_cmd; complete_task_cmd ]

let () = Caml.exit (Cmd.eval cmd)
