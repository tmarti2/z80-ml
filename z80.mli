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

module Make : functor (M : Memory)  -> Cpu