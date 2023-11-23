module Exec

type Task<'S> = Context<'S> -> Context<'S>
and Context<'S> = {
    queue: Task<'S> list
    state: 'S
}

let scheduleTask task ctx = { ctx with queue = task :: ctx.queue }

let scheduleTasks tasks ctx = { ctx with queue = Seq.toList tasks @ ctx.queue }

let updateState state ctx = { ctx with state = state }

// Simple loop for executing scheduled tasks
let rec processQueue ctx =
    match ctx.queue with
    | task :: rest -> task { ctx with queue = rest } |> processQueue
    | [] -> ctx

let runTask task state = (processQueue { queue = [task]; state = state }).state

let runTasks tasks state = (processQueue { queue = Seq.toList tasks; state = state }).state
