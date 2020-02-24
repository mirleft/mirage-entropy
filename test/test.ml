open Lwt.Infix

let fpr ppf fmt = Format.fprintf ppf fmt

module Print_rng = struct
  type g = unit

  let block = 16

  let create () = ()

  let generate ~g:_ _n = assert false

  let reseed ~g:_ data =
    Format.printf "reseeding: %a@.%!" Cstruct.hexdump_pp data

  let accumulate ~g:_ =
    let print ~source data =
      Format.printf "accumulate: (src:%d) %a@.%!" source Cstruct.hexdump_pp data
    in
    `Acc print

  let seeded ~g:_ = true
end

let with_entropy act =
  Entropy.connect (module Print_rng) >>= fun t ->
  act () >>= fun res ->
  Entropy.disconnect t >|= fun () ->
  res

let () =
  OS.(Main.run (with_entropy (fun () ->
      Time.sleep_ns 1_000L)))
