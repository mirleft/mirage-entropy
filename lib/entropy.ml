(*
 * Copyright (c) 2014 Hannes Mehnert
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014-2016 David Kaloper Meršinjak
 * Copyright (c) 2015 Citrix Systems Inc
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

module Cpu_native = struct

  external cycles     : unit -> int  = "caml_cycle_counter" [@@noalloc]
  external random     : unit -> int  = "caml_cpu_random"    [@@noalloc]
  external rng_type   : unit -> int  = "caml_cpu_rng_type"  [@@noalloc]
  external detect     : unit -> unit = "caml_entropy_detect"

  let () = detect ()

  let cpu_rng =
    match rng_type () with
    | 0 -> None
    | 1 -> Some `Rdrand
    | 2 -> Some `Rdseed
    | _ -> assert false
end

open Lwt.Infix

type t = unit

type source = [
    `Timer
  | `Rdseed
  | `Rdrand
]

let sources () =
  `Timer ::
  match Cpu_native.cpu_rng with
  | Some x -> [x]
  | None   -> []

(* Note:
 * `bootstrap` is not a simple feedback loop. It attempts to exploit CPU-level
 * data races that lead to execution-time variability of identical instructions.
 * See Whirlwind RNG:
 *   http://www.ieee-security.org/TC/SP2014/papers/Not-So-RandomNumbersinVirtualizedLinuxandtheWhirlwindRNG.pdf
 *)
let bootstrap f =
  let outer     = 100
  and inner_max = 1024
  and a         = ref 0
  and cs        = Cstruct.create 2 in
  for i = 0 to outer - 1 do
    let tsc = Cpu_native.cycles () in
    let ()  = Cstruct.LE.set_uint16 cs 0 tsc ; f cs in
    for j = 1 to tsc mod inner_max do
      a := tsc / j - !a * i + 1
    done
  done ;
  Lwt.return_unit

let interrupt_hook () =
  match Cpu_native.cpu_rng with
  | None ->
      let buf = Cstruct.create 4 in fun () ->
        let a = Cpu_native.cycles () in
        Cstruct.LE.set_uint32 buf 0 (Int32.of_int a) ;
        buf
  | Some _ ->
      let buf = Cstruct.create 12 in fun () ->
        let a = Cpu_native.cycles ()
        and b = Cpu_native.random () in
        Cstruct.LE.set_uint32 buf 0 (Int32.of_int a) ;
        Cstruct.LE.set_uint64 buf 4 (Int64.of_int b) ;
        buf

(* XXX TODO
 *
 * Xentropyd. Detect its presence here, make it feed into `t.handler` as
 * `~source:1` and add a function providing initial entropy burst to
 * `t.inits`.
 *
 * Compile-time entropy. A function returning it could go into `t.inits`.
*)
let bootstrap_functions = [ bootstrap ]

let connect (type a) ?g (rng : a Mirage_crypto_rng.generator) =
  let rng = Mirage_crypto_rng.(create ?g rng) in
  let `Acc handler = Mirage_crypto_rng.accumulate (Some rng) in
  Lwt_list.iteri_p
    (fun i boot -> boot (handler ~source:i))
    bootstrap_functions >|= fun () ->
  let hook = interrupt_hook () in
  Mirage_crypto_rng.generator := rng;
  Mirage_runtime.at_enter_iter (fun () ->
      let `Acc handler = Mirage_crypto_rng.accumulate None in
      let e = hook () in
      handler ~source:0 e)

let disconnect () = Lwt.return_unit
