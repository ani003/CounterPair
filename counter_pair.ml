open Lwt.Infix

let info = Irmin_unix.info

let (+) = Int64.add
let (-) = Int64.sub

module CounterPair : Irmin.Contents.S with type t = int64 * int64 = struct
  type t = int64 * int64
  
  let t = Irmin.Type.(pair int64 int64) 

  let merge ~old a b =
    let open Irmin.Merge.Infix in
    let (a1, a2) = a and (b1, b2) = b in
    old () >|=* function
    | None -> (a1 + b1, a2 + b2)
    | Some (o1, o2) -> (a1 + b1 - o1, a2 + b2 - o2)
  
  let merge = Irmin.Merge.(option (v t merge))

end

module Store = Irmin_unix.Git.FS.KV (CounterPair)

let init_counter t k = 
  Store.set_exn t ~info:(info "Initialising counters") k (0L, 0L)

let inc_first t k = 
  Store.get t k >>= fun c -> let (c1, c2) = c in
  Store.set_exn t ~info:(info "Incrementing first counter") k (c1 + 1L, c2)

let inc_second t k = 
  Store.get t k >>= fun c -> let (c1, c2) = c in
  Store.set_exn t ~info:(info "Incrementing second counter") k (c1, c2 + 1L)

let set_first t k v = 
  Store.get t k >>= fun c -> let (_, c2) = c in
  Store.set_exn t ~info:(info "Setting first counter to %Ld" v) k (v, c2)

let set_second t k v = 
  Store.get t k >>= fun c -> let (c1, _) = c in
  Store.set_exn t ~info:(info "Setting second counter to %Ld" v) k (c1, v)

let get_pair t k = Store.get t k

let get_first t k = get_pair t k >>= fun (c1, _) -> Lwt.return c1

let get_second t k = get_pair t k >>= fun (_, c2) -> Lwt.return c2

let path = ["tmp"; "count"]

let main () =
  let config = Irmin_git.config ~bare:true "/tmp/test" in
  Store.Repo.v config >>= fun repo ->
  Store.master repo >>= fun t ->
  Store.clone ~src:t ~dst:"v2" >>= fun x -> 
  init_counter t path >>= fun () ->
  inc_first t path >>= fun () ->
  inc_second t path >>= fun () ->
  get_pair t path >>= fun (t1, t2) ->
  Printf.printf "Checkpoint 1: master -> (%Ld, %Ld)\n" t1 t2;
  init_counter x path >>= fun () ->
  set_first x path 2L >>= fun () ->
  set_second x path 3L >>= fun () ->
  get_first x path >>= fun x1 ->
  get_second x path >>= fun x2 ->
  Printf.printf "Checkpoint 2: v2 -> (%Ld, %Ld)\n" x1 x2;
  Store.merge_into ~info:(info "Merging v2 into master") x ~into:t >>= (function
  | Ok () -> get_pair t path >>= fun (t3, t4) -> get_pair x path >>= fun (x3, x4) ->
              Printf.printf "Checkpoint 3: master -> (%Ld, %Ld) ; v2 -> (%Ld, %Ld)\n" t3 t4 x3 x4; Lwt.return_unit
  | Error _ -> failwith "Error!!!") >>= fun () ->
  inc_first t path >>= fun () ->
  inc_first t path >>= fun () ->
  inc_second t path >>= fun () ->
  get_pair t path >>= fun (t5, t6) ->
  get_pair x path >>= fun (x5, x6) ->
  Printf.printf "Checkpoint 4: master -> (%Ld, %Ld) ; v2 -> (%Ld, %Ld)\n" t5 t6 x5 x6;
  Store.merge_into ~info:(info "Merging master into v2") t ~into:x >>= function
  | Ok () -> get_pair t path >>= fun (t7, t8) -> get_pair x path >>= fun (x7, x8) ->
              Printf.printf "Checkpoint 5: master -> (%Ld, %Ld) ; v2 -> (%Ld, %Ld)\n" t7 t8 x7 x8; Lwt.return_unit
  | Error _ -> failwith "Error!!"

let () = Lwt_main.run (main ())
