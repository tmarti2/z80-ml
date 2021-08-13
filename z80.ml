module type Memory = sig
  val read : int -> int
  val write : int -> int -> unit
end

module type Cpu = sig
  module M : Memory

  module Register : sig
    type t = [
        `A | `F |
        `A' | `F' |
        `B | `C |
        `B' | `C' |
        `D | `E |
        `D' | `E' |
        `H | `L |
        `H' | `L' |
        `IXH | `IXL |
        `IYH | `IYL |
        `AF | `AF' |
        `BC | `BC' |
        `DE | `DE' |
        `HL | `HL' |
        `I | `R |
        `IX | `IY |
        `SP | `PC]

    val get : t -> int
    val set : t -> int -> unit
    val reset : unit -> unit
  end

  val fetch_instr : unit -> unit
  val reset : unit -> unit
end

module Make (M : Memory) = struct
  module M = M

  let mask v bits mask =
    (v lsr bits) land mask

  module Register = struct

    let reg_A = ref 0x00
    let reg_A' = ref 0x00
    let reg_F = ref 0x00
    let reg_F' = ref 0x00
    let reg_B = ref 0x00
    let reg_B' = ref 0x00
    let reg_C = ref 0x00
    let reg_C' = ref 0x00
    let reg_D = ref 0x00
    let reg_D' = ref 0x00
    let reg_E = ref 0x00
    let reg_E' = ref 0x00
    let reg_H = ref 0x00
    let reg_H' = ref 0x00
    let reg_L = ref 0x00
    let reg_L' = ref 0x00
    let reg_I = ref 0x0000
    let reg_R = ref 0x0000
    let reg_IX = ref 0x0000
    let reg_IY = ref 0x0000
    let reg_PC = ref 0x0100 (* Program counter *)
    let reg_SP = ref 0xFFFE (* Stack pointer *)

    type t8 = [
        `A | `F |
        `A' | `F' |
        `B | `C |
        `B' | `C' |
        `D | `E |
        `D' | `E' |
        `H | `L |
        `IXH | `IXL |
        `IYH | `IYL |
        `H' | `L' ]

    type t16 = [
        `AF | `AF' |
        `BC | `BC' |
        `DE | `DE' |
        `HL | `HL' |
        `IX | `IY |
        `I  | `R  |
        `SP | `PC ]

    type t = [t8 | t16]

    let get8 (reg:t8) =
      match reg with
      | `A -> !reg_A | `F -> !reg_F
      | `A' -> !reg_A' | `F' -> !reg_F'
      | `B -> !reg_B | `C -> !reg_C
      | `B' -> !reg_B' | `C' -> !reg_C'
      | `D -> !reg_D | `E -> !reg_E
      | `D' -> !reg_D' | `E' -> !reg_E'
      | `H -> !reg_H | `L -> !reg_L
      | `H' -> !reg_H' | `L' -> !reg_L'
      | `IXH -> !reg_IX lsr 8
      | `IXL -> !reg_IX land 0xFF
      | `IYH -> !reg_IY lsr 8
      | `IYL -> !reg_IY land 0xFF

    let get16 (reg:t16) =
      match reg with
      | `AF -> (!reg_A lsl 8) lor !reg_F
      | `AF' -> (!reg_A' lsl 8) lor !reg_F'
      | `BC -> (!reg_B lsl 8) lor !reg_C
      | `BC' -> (!reg_B' lsl 8) lor !reg_C'
      | `DE -> (!reg_D lsl 8) lor !reg_E
      | `DE' -> (!reg_D' lsl 8) lor !reg_E'
      | `HL -> (!reg_H lsl 8) lor !reg_L
      | `HL' -> (!reg_H' lsl 8) lor !reg_L'
      | `I -> !reg_I
      | `R -> !reg_R
      | `IX -> !reg_IX
      | `IY -> !reg_IY
      | `PC -> !reg_PC
      | `SP -> !reg_SP

    let set8 (reg:t8) v =
      let v = v land 0xFF in
      match reg with
      | `A  -> reg_A  := v | `F  -> reg_F  := v
      | `A' -> reg_A' := v | `F' -> reg_F' := v
      | `B  -> reg_B  := v | `C  -> reg_C  := v
      | `B' -> reg_B' := v | `C' -> reg_C' := v
      | `D  -> reg_D  := v | `E  -> reg_E  := v
      | `D' -> reg_D' := v | `E' -> reg_E' := v
      | `H  -> reg_H  := v | `L  -> reg_L  := v
      | `H' -> reg_H' := v | `L' -> reg_L' := v
      | `IXH -> reg_IX := (v lsl 8) lor (get8 `IXL)
      | `IXL -> reg_IX :=  (!reg_IX land 0xFF00) lor v
      | `IYH -> reg_IY := (v lsl 8) lor (get8 `IYL)
      | `IYL -> reg_IY :=  (!reg_IY land 0xFF00) lor v

    let set16 (reg:t16) v =
      let v = v land 0xFFFF in
      match reg with
      | `PC -> reg_PC := v
      | `SP -> reg_SP := v
      | `IX -> reg_IX := v
      | `IY -> reg_IY := v
      | `I -> reg_I := v
      | `R -> reg_R := v
      | _ ->
        let l = mask v 8 0xFF in
        let r = mask v 0 0xFF in
        match reg with
        | `AF -> set8 `A l; set8 `F r
        | `AF' -> set8 `A' l; set8 `F' r
        | `BC -> set8 `B l; set8 `C r
        | `DE -> set8 `D l; set8 `E r
        | `HL -> set8 `H l; set8 `L r
        | _ -> assert false

    let get r =
      match r with
      | #t8 as r8 -> get8 r8
      | #t16 as r16 -> get16 r16

    let set r v =
      match r with
      | #t8 as r8 -> set8 r8 v
      | #t16 as r16 -> set16 r16 v

    let reset () =
      reg_A := 0x00;
      reg_A' := 0x00;
      reg_F := 0x00;
      reg_F' := 0x00;
      reg_B := 0x00;
      reg_B' := 0x00;
      reg_C := 0x00;
      reg_C' := 0x00;
      reg_D := 0x00;
      reg_D' := 0x00;
      reg_E := 0x00;
      reg_E' := 0x00;
      reg_H := 0x00;
      reg_H' := 0x00;
      reg_L := 0x00;
      reg_L' := 0x00;
      reg_I := 0x0000;
      reg_R := 0x0000;
      reg_IX := 0x0000;
      reg_IY := 0x0000;
      reg_PC := 0x0100;
      reg_SP := 0xFFFE

  end

  module R = Register

  module FlipFlop = struct
    let _IFF1 = ref false
    let _IFF2 = ref false
    type m = One | Zero | OneZero | Two
    let mode = ref One
  end

  module Stack = struct

    let push v =
      R.set `SP (R.get `SP - 1);
      M.write (R.get `SP) v

    let push v =
      push (v lsr 8);
      push (v land 0xFF)

    let pop () =
      let v = M.read (R.get `SP) in
      R.set `SP (R.get `SP + 1); v

    let pop () =
      let lo = pop () in
      let hi = pop () in
      lo lor (hi lsl 8)

  end

  let reset () =
    for i = 0 to 0xFFFF do M.write i 0x00 done;
    R.reset ()

  module Flag = struct (* S Z 5 H 3 P N C *)
    (* type t = Mask of int *)
    let mk_flag n = (1 lsl n)
    let flag_S = mk_flag 7
    let flag_Z = mk_flag 6
    let flag_5 = mk_flag 5
    let flag_H = mk_flag 4
    let flag_3 = mk_flag 3
    let flag_PV = mk_flag 2
    let flag_N = mk_flag 1
    let flag_C = mk_flag 0

    type t = [`S | `Z | `F5 | `H | `F3 | `PV | `N | `C]

    let getm (f:t) =
      match f with
      | `S -> flag_S | `Z -> flag_Z | `F5 -> flag_5
      | `H -> flag_H | `PV -> flag_PV | `F3 -> flag_3
      | `N -> flag_N | `C -> flag_C

    let set (f:t) v =
      let m = getm f in
      R.set `F @@
      if v
      then (R.get `F) lor m
      else (R.get `F) land (lnot m)

    let reset f = set f false

    let get (f:t) =
      if getm f land (R.get `F) != 0 then true else false

    let seti f i = set f (i = 1)
    let geti f = if get f then 1 else 0

    let update_S v = set `S ((v lsr 7) land 0x1 = 1)
    let update_Z v = set `Z (v = 0)
    let update_add_H ?(c=0) v v' =
      let v = v land 0xF in
      let v' = v' land 0xF in
      set `H ((v + v' + c) > 0xF)

    let update_parity =
      let m1 = 0x55 in
      (* 0b01010101... *)
      let m2 = 0x33 in
      (* 0b00110011... *)
      let m4 = 0x0f in
      (* 0b00001111... *)
      fun x ->
        let x = x - ((x lsr 1) land m1) in
        let x = (x land m2) + ((x lsr 2) land m2) in
        let nb_1 = (x + (x lsr 4)) land m4 in
        set `PV (nb_1 mod 2 = 0)

  end

  module Location = struct
    type t =
      | Im of int
      | Reg of Register.t
      | Addr8 of t
      | Addr16 of t

    let get = function
      | Im i -> i
      | Reg r -> R.get r
      | Addr8 (Im i) -> M.read (i land 0xFFFF)
      | Addr8 (Reg (#R.t16 as r16)) -> M.read (R.get r16)
      | Addr16 (Im i) ->
        let low = M.read (i land 0xFFFF) in
        let high = M.read ((i+1) land 0xFFFF) in
        low lor (high lsr 8)
      | _ -> assert false

    let set l v =
      match l with
      | Reg r -> R.set r v
      | Addr8 (Im i) ->
        M.write (i land 0xFFFF) v
      | Addr8 (Reg (#R.t16 as r16)) ->
        M.write (R.get r16) v
      | Addr16 (Im i) ->
        let high = mask v 8 0xFF in
        let low = mask v 0 0xFF in
        M.write (i land 0xFFFF) low;
        M.write ((i + 1) land 0xFFFF) high
      | _ -> assert false
  end

  let ( !! ) = Location.get
  let ( <<- ) = Location.set

  type addressing_mode =
    | Implied (* opcode only _NOP *)
    | Immediate (* one operand ADD A, N *)
    | Extended_Immediate (* 2 operands LD HL,NN *)
    | Register (* 3-bit field for 1 register AND R *)
    | Register_indirect (* LD A, (BC) *)
    | Extended_direct (* 2 operands LD A, (NN) *)
    | Zero_page
    | Relative (* one operand signed *)
    | Indexed of addressing_mode
    | Bit

  type prefixed = None of addressing_mode
                | Single of addressing_mode
                | Double of addressing_mode

  let rec addressing_mode_size = function
    | Implied | Register | Register_indirect | Zero_page | Bit -> 0
    | Immediate | Relative -> 1
    | Indexed am -> 1 + addressing_mode_size am
    | Extended_Immediate | Extended_direct -> 2

  let get_prefixed_size = function
    | None am -> addressing_mode_size am
    | Single am -> addressing_mode_size am + 1
    | Double am -> addressing_mode_size am + 2

  module Inst = struct

    (* Load *)
    let _LD ?(f=fun () -> ()) a b _ =
      let v = !!b in
      f ();
      a <<- v

    let interrupt_flags () =
      Flag.update_S (R.get `I);
      Flag.update_Z (R.get `I);
      Flag.reset `H;
      Flag.set `PV !FlipFlop._IFF2;
      Flag.reset `N

    (* Arithmetic OP *)
    let _DAA _ =
      let a = R.get `A in
      let _N = Flag.get `N in
      let _H = Flag.get `H in
      let t = ref 0 in
      if _H || (a land 0xF) > 9 then incr t;
      if Flag.get `C || a > 99 then (Flag.set `C true; t:=!t+2);
      if _N && not _H then
        Flag.reset `H
      else if _N && _H then
        Flag.set `H ((a land 0xF) < 6)
      else
        Flag.set `H ((a land 0xF) >= 0x0A);

      let new_a =
        a +
        match !t with
        | 1 -> if _N then 0xFA else 0x06
        | 2 -> if _N then 0xA0 else 0x60
        | 3 -> if _N then 0x9A else 0x66
        | _ -> assert false
      in
      R.set `A new_a;
      Flag.set `S (new_a land 0x80 = 0x80);
      Flag.update_Z new_a;
      Flag.update_parity new_a


    let _ADD ?(pv=(>)) ?c_opt s r v _ =
      let rv = !!r in
      let vv = !!v in
      let max, rv', vv' =
        match s with
        | 8 -> 0xFF, rv, vv
        | 16 -> 0xFFFF, (rv lsr 8), (vv lsr 8)
        | _ -> assert false
      in
      let c = match c_opt with None -> 0 | Some c' -> c' in
      let res = rv + vv + c in
      begin match c_opt with
        | None when s = 16 -> ()
        | _ ->
          Flag.update_S res;
          Flag.update_Z res;
          Flag.set `PV (res > max);
      end;
      Flag.update_add_H rv' vv' ~c;
      Flag.reset `N;
      Flag.set `C (pv res max);
      r <<- (res land max)

    let _ADD_A v _ =
      _ADD 8 (Reg `A) v ()

    let _ADC_A v _ =
      let c_opt = Flag.geti `C in
      _ADD ~c_opt 8 (Reg `A) v ()

    let _ADD_16 r v _ =
      _ADD 16 r v ()

    let _ADC_16 r v _ =
      let c_opt = Flag.geti `C in
      _ADD ~c_opt 16 r v ()

    let _SUB_A v _ =
      _ADD 8 ?c_opt:(Some 1) (Reg `A) (Im (lnot !!v)) ();
      Flag.set `N true

    let _SBC_A v _ =
      let c_opt = Flag.geti `C in
      _ADD ~pv:(<=) ~c_opt 8 (Reg `A) (Im (lnot !!v)) ();
      Flag.set `N true

    let _SBC_16 r v _ =
      let c_opt = Flag.geti `C in
      _ADD ~c_opt 16 r (Im (lnot !!v)) ();
      Flag.set `N true

    let _INC8 r _ =
      let oldc = Flag.get `C in
      let oldr = !!r in
      _ADD 8 r (Im 1) ();
      Flag.set `PV (oldr = 0x7F);
      Flag.set `C oldc

    let _INC16 r _ = r <<- ((!!r + 1) land 0xFFFF)

    let _DEC8 r _ =
      let oldc = Flag.get `C in
      let oldr = !!r in
      _ADD 8 r (Im (-1)) ();
      Flag.set `PV (oldr = 0x80);
      Flag.set `N true;
      Flag.set `C oldc

    let _DEC16 r _ = r <<- ((!!r - 1) land 0xFFFF)

    (* Logical *)
    let gen_op reset f v =
      let res = f (R.get `A) !!v in
      Flag.update_S res;
      Flag.update_Z res;
      Flag.set `H (not reset);
      Flag.update_parity res;
      Flag.reset `N;
      Flag.reset `C;
      R.set `A res

    let _AND v _ = gen_op false (land) v
    let _OR  v _ = gen_op true (lor) v
    let _XOR v _ = gen_op true (lxor) v
    let _NEG _ = R.set `A (lnot (R.get `A) + 1)

    (* Compare *)
    let _CP v _ =
      let a = R.get `A in
      let v = (lnot !!v) + 1 in
      let res = a + v in
      Flag.update_S res;
      Flag.update_Z res;
      Flag.update_add_H a v;
      Flag.set `PV (res > 0xFF);
      Flag.set `N true;
      Flag.set `PV (res > 0xFF)

    (* Rotate and shift *)
    let gen_shift dec r shift set_flags =
      let oldr = !!r in
      let c = mask oldr dec 0x1 in
      let v = shift oldr (Flag.geti `C) c in
      set_flags c v;
      v

    let a_flags c _ =
      Flag.reset `H;
      Flag.reset `N;
      Flag.seti `C c

    let cb_flags c v =
      Flag.update_S v;
      Flag.update_Z v;
      Flag.reset `H;
      Flag.update_parity v;
      Flag.reset `N;
      Flag.seti `C c

    let rlc_aux f r = gen_shift 7 r (fun n _ c -> (n lsl 1) land 0xFF lor c) f
    let _RLCA _ = R.set `A @@ rlc_aux a_flags (Reg `A)
    let _RLC r = rlc_aux cb_flags r
    let _RLC_CB r _ = r <<- _RLC r

    let rl_aux f r = gen_shift 7 r (fun n c _ -> (n lsl 1) land 0xFF lor c) f
    let _RLA _ = R.set `A @@ rl_aux a_flags (Reg `A)
    let _RL r = rl_aux cb_flags r
    let _RL_CB r _ = r <<- _RL r

    let rrc_aux f r = gen_shift 0 r (fun n _ c -> (n lsr 1) lor (c lsl 7)) f
    let _RRCA _ = R.set `A @@ rrc_aux a_flags (Reg `A)
    let _RRC r = rrc_aux cb_flags r
    let _RRC_CB r _ = r <<- _RRC r

    let rr_aux f r = gen_shift 0 r (fun n c _ -> (n lsr 1) lor (c lsl 7)) f
    let _RRA _ = R.set `A @@ rr_aux a_flags (Reg `A)
    let _RR r = rr_aux cb_flags r
    let _RR_CB r _ = r <<- _RR r

    let _SLA r = gen_shift 7 r (fun n _ _ -> (n lsl 1) land 0xFF) cb_flags
    let _SLA_CB r _ = r <<- _SLA r

    let _SLL r = gen_shift 7 r (fun n _ _ -> (n lsl 1) land 0xFF lor 1) cb_flags
    let _SLL_CB r _ = r <<- _SLL r

    let _SRA r = gen_shift 0 r (fun n _ _ -> (n lsr 1) lxor (n land 0x40)) cb_flags
    let _SRA_CB r _ = r <<- _SRA r

    let _SRL r =
      let res = gen_shift 0 r (fun n _ _ -> n lsr 1) cb_flags in
      Flag.reset `S;
      res
    let _SRL_CB r _ = r <<- _SRL r

    let _RD nah nal nmh nml =
      R.set `A ((nah lsl 4) lor nal);
      M.write (R.get `HL) ((nmh lsl 4) lor nml);
      let na = R.get `A in
      Flag.update_S na;
      Flag.update_Z na;
      Flag.reset `H;
      Flag.update_parity na;
      Flag.reset `N

    let _RRD _ =
      let ah = mask (R.get `A) 4 0xF in
      let al = mask (R.get `A) 0 0xF in
      let mh = mask (M.read (R.get `HL)) 4 0xF in
      let ml = mask (M.read (R.get `HL)) 0 0xF in
      _RD ah ml al mh

    let _RLD _ =
      let ah = mask (R.get `A) 4 0xF in
      let al = mask (R.get `A) 0 0xF in
      let mh = mask (M.read (R.get `HL)) 4 0xF in
      let ml = mask (M.read (R.get `HL)) 0 0xF in
      _RD ah mh ml al

    let _BIT b r _ =
      let bit = !!b in
      let v = !!r in
      let c = mask v bit 0x1 in
      Flag.update_Z c;
      Flag.set `H true;
      Flag.reset `N

    let _RES b r =
      let bit = !!b in
      let v = !!r in
      let mask = 1 lsl bit lxor 0xFF in
      v land mask
    let _RES_CB b r _ = r <<- _RES b r

    let _SET b r =
      let bit = !!b in
      let v = !!r in
      v lor (1 lsl bit)
    let _SET_CB b r _ = r <<- _SET b r

    let _CPL _ =
      Flag.set `H true;
      Flag.set `N true;
      R.set `A (R.get `A lxor 0xFF)

    let _CCF _ =
      let c = Flag.get `C in
      Flag.set `H c;
      Flag.reset `N;
      Flag.set `C (not c)

    let _SCF _ =
      Flag.reset `H;
      Flag.reset `N;
      Flag.set `C true

    (* Jump / Relative jump / Call *)
    let _JR ?(cc=true) e _ =
      if cc then begin
        let e = !!e in
        let s_e =
          if e > 0x7F
          then - (((lnot e) + 1) land 0xFF)
          else e
        in
        let new_addr = (R.get `PC + s_e + 2) land 0xFFFF in
        R.set `PC new_addr
      end

    let _JP ?(cc=true) v _ =
      if cc then R.set `PC !!v

    let _DJNZ e _ =
      let res = R.get `B - 1 in
      R.set `B res;
      _JR ~cc:(res != 0) e ()

    let _CALL ?(cc=true) v () =
      if cc then begin
        Stack.push (R.get `PC);
        R.set `PC !!v
      end

    let _RST v = _CALL ~cc:true v

    (* Flag test *)
    let _NZ () = not (Flag.get `Z)
    let _Z () = Flag.get `Z
    let _NC () = not (Flag.get `C)
    let _C () = Flag.get `C
    let _PO () = not (Flag.get `PV)
    let _PE () = Flag.get `PV
    let _P () = not (Flag.get `C)
    let _M () = Flag.get `C

    (* Exchange registers *)
    let _EX a b _ =
      let tmp = !!a in
      a <<- !!b;
      b <<- tmp

    let _EXX _ =
      _EX (Reg `BC) (Reg `BC') ();
      _EX (Reg `DE) (Reg `DE') ();
      _EX (Reg `HL) (Reg `HL') ()

    (* Stack operation *)
    let _POP r _ =
      r <<- Stack.pop ()

    let _PUSH r _ =
      Stack.push !!r

    let _RET ?(cc=true) _ =
      if cc then R.set `PC (Stack.pop ())

    let _RETN _ = _RET ();
      FlipFlop._IFF1 := !FlipFlop._IFF2

    let _RETI _ = _RET (); failwith "todo reti"

    (* Interruptions fliflops *)
    let _DI () =
      FlipFlop._IFF1 := false;
      FlipFlop._IFF2 := false

    let _EI () =
      FlipFlop._IFF1 := true;
      FlipFlop._IFF2 := true

    let _HALT _ = failwith "todo HALT"

    let _IM m _ = FlipFlop.mode := m

    let _IN (_:Location.t) _ = failwith "todo IN"
    let _OUT (_:Location.t) _ = failwith "todo OUT"

    let _NOP () = ()

    module Decoding = struct

      (* See z80.info/decoding *)
      let r = Location.[|Reg `B; Reg `C; Reg `D; Reg `E; Reg `H; Reg `L; Addr8 (Reg `HL); Reg `A|]
      let rp = Location.[|Reg `BC; Reg `DE; Reg `HL; Reg `SP|]
      let rp2 = Location.[|Reg `BC; Reg `DE; Reg `HL; Reg `AF|]
      let alu = [|_ADD_A;_ADC_A;_SUB_A;_SBC_A;_AND;_XOR;_OR;_CP|]
      let rot_CB = [|_RLC_CB;_RRC_CB;_RL_CB;_RR_CB;_SLA_CB;_SRA_CB;_SLL_CB;_SRL_CB|]
      let rot = [|_RLC;_RRC;_RL;_RR;_SLA;_SRA;_SLL;_SRL|]
      let cc = [|_NZ;_Z;_NC;_C;_PO;_PE;_P;_M|]
      let im = [|FlipFlop.Zero;FlipFlop.OneZero;FlipFlop.One;FlipFlop.Two|]

      let nhl r v =
        Location.Addr8 (Im (R.get r + v))

      let set_ir ir =
        match ir with
        | `IX ->
          r.(4) <- Reg `IXH;
          r.(5) <- Reg `IXL;
          rp.(2) <- Reg `IX;
          rp2.(2) <- Reg `IX
        | `IY ->
          r.(4) <- Reg `IYH;
          r.(5) <- Reg `IYL;
          rp.(2) <- Reg `IY;
          rp2.(2) <- Reg `IY

      let set_n () =
        r.(4) <- Reg `H;
        r.(5) <- Reg `L;
        rp.(2) <- Reg `HL;
        rp2.(2) <- Reg `HL

      (* See z80.info/decoding *)
      let extract op =
        (* let pre = mask op 8 0xF in *)
        let x = mask op 6 0x3 in
        let y = mask op 3 0x7 in
        let z = mask op 0 0x7 in
        let p = mask op 4 0x3 in
        let q = mask op 3 0x1 in
        (x,y,z,p,q)

      let get_am_fun_nx0 ?ir v1 v2 y z p q : addressing_mode * (unit -> unit) =
        let v21 = v1 lor (v2 lsl 8) in
        match z, q, y, p, ir with
        | 0, _, 0, _, _ -> Implied, _NOP
        | 0, _, 1, _, _ -> Implied, _EX (Reg `AF) (Reg `AF')
        | 0, _, 2, _, _  -> Immediate, _DJNZ (Im v1)
        | 0, _, 3, _, _  -> Relative, _JR (Im v1)
        | 0, _, _, _, _  -> Relative, _JR ~cc:(cc.(y) ()) (Im v1)
        | 1, 0, _, _, _ -> Extended_Immediate, _LD rp.(p) (Im v21)
        | 1, 1, _, _, _ -> Register, _ADD_16 rp.(2) rp.(p)
        | 2, 0, _, 0, _ -> Register_indirect, _LD (Addr8 (Reg `BC)) (Reg `A)
        | 2, 0, _, 1, _ -> Register_indirect, _LD (Addr8 (Reg `DE)) (Reg `A)
        | 2, 0, _, 2, _ -> Extended_direct, _LD (Addr16 (Im v21)) rp.(2)
        | 2, 0, _, 3, _ -> Extended_direct, _LD (Addr8 (Im v21)) (Reg `A)
        | 2, 1, _, 0, _ -> Register_indirect, _LD (Reg `A) (Addr8 (Reg `BC))
        | 2, 1, _, 1, _ -> Register_indirect, _LD (Reg `A) (Addr8 (Reg `DE))
        | 2, 1, _, 2, _ -> Extended_direct, _LD rp.(2) (Addr16 (Im v21))
        | 2, 1, _, 3, _ -> Extended_direct, _LD (Reg `A) (Addr8 (Im v21))
        | 3, 0, _, _, _ -> Register, _INC16 rp.(p)
        | 3, 1, _, _, _ -> Register, _DEC16 rp.(p)
        | 4, _, 6, _, Some ir -> Indexed Register, _INC8 (nhl ir v1)
        | 4, _, _, _, _ -> Register, _INC8 r.(y)
        | 5, _, 6, _, Some ir -> Indexed Register, _DEC8 (nhl ir v1)
        | 5, _, _, _, _ -> Register, _DEC8 r.(y)
        | 6, _, 6, _, Some ir -> Indexed Immediate, _LD (nhl ir v1) (Im v2)
        | 6, _, _, _, _ -> Immediate, _LD r.(y) (Im v2)
        | 7, _, 0, _, _ -> Implied, _RLCA
        | 7, _, 1, _, _ -> Implied, _RRCA
        | 7, _, 2, _, _ -> Implied, _RLA
        | 7, _, 3, _, _ -> Implied, _RRA
        | 7, _, 4, _, _ -> Implied, _DAA
        | 7, _, 5, _, _ -> Implied, _CPL
        | 7, _, 6, _, _ -> Implied, _SCF
        | 7, _, 7, _, _ -> Implied, _CCF
        | _ -> assert false

      let get_am_fun_nx1 ?ir v1 y z =
        if y = 6 && z = 6 then Implied, _HALT
        else
          match y, z, ir with
          | 6, 4, Some ir -> Indexed Register, _LD (nhl ir v1) (Reg `H)
          | 6, 5, Some ir -> Indexed Register, _LD (nhl ir v1) (Reg `L)
          | 4, 6, Some ir -> Indexed Register, _LD (Reg `H) (nhl ir v1)
          | 5, 6, Some ir -> Indexed Register, _LD (Reg `L) (nhl ir v1)
          | _ -> Register, _LD r.(y) r.(z)

      let get_am_fun_nx2 ?ir v1 y z =
        match z, ir with
        | 6, Some ir -> Indexed Register, alu.(y) (nhl ir v1)
        | _ -> Register, alu.(y) r.(z)

      let get_am_fun_nx3 v1 v2 y z p q =
        let v21 = v1 lor (v2 lsl 8) in
        match z, q, y, p with
        | 0, _, _, _ -> Implied, _RET ~cc:(cc.(y) ())
        | 1, 0, _, _ -> Register, _POP rp2.(p)
        | 1, 1, _, 0 -> Implied, _RET ~cc:true
        | 1, 1, _, 1 -> Implied, _EXX
        | 1, 1, _, 2 -> Register, _JP ~cc:true rp.(2)
        | 1, 1, _, 3 -> Register, _LD (Reg `SP) rp.(2)
        | 2, _, _, _ -> Extended_Immediate, _JP ~cc:(cc.(y) ()) (Im v21)
        | 3, _, 0, _ -> Extended_Immediate, _JP ~cc:true (Im v21)
        | 3, _, 1, _ -> failwith "CB prefix should not reach this"
        | 3, _, 2, _ -> Immediate, _IN (Im v1) (* Check AM *)
        | 3, _, 3, _ -> Immediate, _OUT (Im v1)  (* Check AM *)
        | 3, _, 4, _ -> Register_indirect, _EX (Addr16 (Im (R.get `SP))) rp.(2)
        | 3, _, 5, _ -> Register, _EX (Reg `DE) (Reg `HL)
        | 3, _, 6, _ -> Implied, _DI
        | 3, _, 7, _ -> Implied, _EI
        | 4, _, _, _ -> Extended_Immediate, _CALL ~cc:(cc.(y) ()) (Im v21)
        | 5, 0, _, _ -> Register, _PUSH rp2.(p)
        | 5, 1, _, 0 -> Extended_Immediate, _CALL ~cc:true (Im v21)
        | 5, 1, _, 1 -> failwith "DD prefix should not reach this"
        | 5, 1, _, 2 -> failwith "ED prefix should not reach this"
        | 5, 1, _, 3 -> failwith "FD prefix should not reach this"
        | 6, _, _, _ -> Immediate, alu.(y) (Im v1)
        | 7, _, _, _ -> Zero_page, _RST (Im (y*8))
        | _ -> assert false

      let get_am_fun_n ?ir v1 v2 x y z p q =
        match x with
        | 0 -> get_am_fun_nx0 ?ir v1 v2 y z p q
        | 1 -> get_am_fun_nx1 ?ir v1 y z
        | 2 -> get_am_fun_nx2 ?ir v1 y z
        | 3 -> get_am_fun_nx3 v1 v2 y z p q
        | _ -> assert false

      let get_am_fun_CB x y z =
        match x with
        | 0 -> Bit, rot_CB.(y) r.(z)
        | 1 -> Bit, _BIT (Im y) r.(z)
        | 2 -> Bit, _RES_CB (Im y) r.(z)
        | 3 -> Bit, _SET_CB (Im y) r.(z)
        | _ -> assert false

      let get_am_fun_PRECB ir d x y z =
        match x, z with
        | 0, 6 -> Bit, rot_CB.(y) (nhl ir d)
        | 0, _ -> Register, _LD r.(z) (Im (rot.(y) (nhl ir d)))
        | 1, _ -> Bit, _BIT (Im y) (nhl ir d)
        | 2, 6 -> Bit, _RES_CB (Im y) (nhl ir d)
        | 2, _ -> Register, _LD r.(z) (Im (_RES (Im y) (nhl ir d)))
        | 3, 6 -> Bit, _SET_CB (Im y) (nhl ir d)
        | 3, _ -> Register, _LD r.(z) (Im (_SET (Im y) (nhl ir d)))
        | _ -> assert false

      let get_am_fun_ED v1 v2 x y z q p =
        let v21 = v1 lor (v2 lsl 8) in
        match x, z, q, y with
        | 0, _, _, _ | 3, _, _, _ -> Implied, _NOP
        | 1, 0, _, 6 -> Register, _IN r.(y)
        | 1, 0, _, _ -> Register, _IN r.(y)
        | 1, 1, _, 6 -> Register, _OUT r.(y)
        | 1, 1, _, _ -> Register, _OUT r.(y)
        | 1, 2, 0, _ -> Register, _SBC_16 (Reg `HL) rp.(p)
        | 1, 2, 1, _ -> Register, _ADC_16 (Reg `HL) rp.(p)
        | 1, 3, 0, _ -> Extended_direct, _LD (Addr16 (Im v21)) rp.(p)
        | 1, 3, 1, _ -> Extended_direct, _LD rp.(p) (Addr16 (Im v21))
        | 1, 4, _, _ -> Implied, _NEG
        | 1, 5, _, 1 -> Implied, _RETI
        | 1, 5, _, _ -> Implied, _RETN
        | 1, 6, _, _ -> Implied, _IM (im.(y mod 4))
        | 1, 7, _, 0 -> Implied, _LD (Reg `I) (Reg `A)
        | 1, 7, _, 1 -> Implied, _LD (Reg `R) (Reg `A)
        | 1, 7, _, 2 -> Implied, _LD ~f:interrupt_flags (Reg `A) (Reg `I)
        | 1, 7, _, 3 -> Implied, _LD ~f:interrupt_flags (Reg `A) (Reg `R)
        | 1, 7, _, 4 -> Implied, _RRD
        | 1, 7, _, 5 -> Implied, _RLD
        | 1, 7, _, _ -> Implied, _NOP
        | 2, _, _, _ when z <= 3 && y >= 4 -> Implied, failwith "todo bli"
        | _ -> assert false

      let decode op =
        let b1 = R.get `PC + 1 in
        let b2 = R.get `PC + 2 in
        let b3 = R.get `PC + 3 in
        let v1 = M.read b1 in
        let v2 = M.read b2 in
        let v3 = M.read b3 in
        match op with
        | 0xCB ->
          let x,y,z,_,_= extract v1 in
          let am, f = get_am_fun_CB x y z in
          Single am, f
        | 0xED ->
          let x,y,z,p,q= extract v1 in
          let am, f = get_am_fun_ED v1 v2 x y z p q in
          Single am, f
        | 0xDD | 0xFD ->
          let ir = match op with | 0xDD -> `IX | 0xFD -> `IY | _ -> assert false in
          begin match v1 with
            | 0xDD | 0xED | 0xFD -> None Implied, _NOP
            | 0xCB ->
              set_ir ir;
              let x,y,z,_,_ = extract v3 in
              let am, f = get_am_fun_PRECB ir v2 x y z in
              set_n ();
              Double (Indexed am), f
            | _ ->
              set_ir ir;
              let x,y,z,p,q = extract v1 in
              let am, f = get_am_fun_n ~ir v2 v3 x y z p q in
              set_n ();
              Single am, f
          end
        | _ ->
          let x,y,z,p,q = extract op in
          let am, f = get_am_fun_n v1 v2 x y z p q in
          None am, f

    end

  end

  let fetch_instr () =
    let opcode = M.read @@ R.get `PC in
    let am, f = Inst.Decoding.decode opcode in
    let am_size = get_prefixed_size am in
    R.set `PC (R.get `PC + am_size + 1);
    f ()

end