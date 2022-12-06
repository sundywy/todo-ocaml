val list_task : unit -> unit
val add_task : string list -> unit
val delete_task : int -> unit
val complete_task : int -> unit

exception TaskNotFound
