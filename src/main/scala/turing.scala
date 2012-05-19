package turing

sealed trait Cell

trait X extends Cell
trait A extends Cell
trait Zero extends Cell
trait One extends Cell

case object X extends X
case object A extends A
case object Zero extends Zero
case object One extends One

case class Tape[+L <: Cells, +C <: Cell, +R <: Cells](left: L, current: C, right: R)

final class TapeOps[L <: Cells, C <: Cell, R <: Cells](t: Tape[L,C,R]) {
  def run[S <: MachineState](s: S)(implicit rs: RunningState[L,C,R,S]) : rs.Out = rs(t)
}

object Tape {
  implicit def tapeOps[L <: Cells, C <: Cell, R <: Cells](t : Tape[L,C,R]) : TapeOps[L,C,R] = new TapeOps(t)
}

sealed trait Cells

final case class ::[+H <: Cell, +T <: Cells](head: H, tail: T) extends Cells {
  override def toString = head+" :: "+tail.toString
}

trait TNil extends Cells {
  def ::[H <: Cell](h: H) = turing.::(h,this)
  override def toString = "TapeEnd"
}

case object TNil extends TNil

final class CellsOps[L <: Cells](l : L) {
  def ::[H <: Cell](h : H) : H :: L = turing.::(h, l)
}

object Cells {
  implicit def cellsOps[L <: Cells](l : L) : CellsOps[L] = new CellsOps(l)
}

trait MachineState
trait Halt extends MachineState
case object Halt extends Halt

trait LeftState extends MachineState
trait RightState extends MachineState

case class Transition[FromState <: MachineState,
                      ToState <: MachineState,
                      FromCell <: Cell,
                      Write <: Cell](write: Write)

trait RunningState[TL <: Cells, TC <: Cell, TR <: Cells, S <: MachineState] {
  type Out
  def apply[S](tape: Tape[TL, TC, TR]): Out
}

trait RunningStateAux[TL <: Cells, TC <: Cell, TR <: Cells, S <: MachineState, Out] {
  def apply[S](tape: Tape[TL, TC, TR]): Out
}

object RunningState {
  implicit def runningstate[TL <: Cells, TC <: Cell, TR <: Cells, S <: MachineState, Out0](implicit runningstateaux: RunningStateAux[TL, TC, TR, S, Out0]) = new RunningState[TL, TC, TR, S] {
    type Out = Out0
    def apply(tape: Tape[TL, TC, TR]): Out = runningstateaux(tape)
  }
}

object RunningStateAux {
  implicit def halted[TL <: Cells, TC <: Cell, TR <: Cells] : RunningStateAux[TL, TC, TR, Tape[TL, TC, TR], Halt.type] =
    new RunningStateAux[TL, TC, TR, Tape[TL, TC, TR], Halt.type] {
      def apply(tape: Tape[TL, TC, TR]): Tape[TL, TC, TR] = tape
    }

  implicit def previousLeftState[TLH <: Cell, 
                                 TLT <: Cells, 
                                 TC <: Cell, 
                                 TR <: Cells,
                                 UC <: Cell,
                                 S <: MachineState,
                                 N <: MachineState,
                                 Out]
  (implicit transition: Transition[S, N, TLH, UC],
   nextRunningState: RunningStateAux[TLT,UC,TC :: TR, S, Out]) =
     new RunningStateAux[TLH :: TLT, TC, TR, S, Out] {
       def apply(tape: Tape[TLH :: TLT, TC, TR]) : Out = {
         nextRunningState(Tape(tape.left.tail, transition.write, turing.::(tape.current, tape.right)))
       }
     }

  implicit def previousRightState[TL <: Cells, 
                                  TC <: Cell, 
                                  TRH <: Cell, 
                                  TRT <: Cells,
                                  UC <: Cell,
                                  S <: MachineState,
                                  N <: MachineState,
                                  Out]
  (implicit transition: Transition[S, N, TRH, UC],
   nextRunningState: RunningStateAux[TC :: TL, UC, TRT, S, Out]) =
     new RunningStateAux[TL, TC, TRH :: TRT, S, Out] {
       def apply(tape: Tape[TL, TC, TRH :: TRT]) : Out = {
         nextRunningState(Tape(turing.::(tape.current, tape.left), transition.write, tape.right.tail))
       }
     }
}

