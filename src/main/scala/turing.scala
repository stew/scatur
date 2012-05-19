package turing

sealed trait Cell

trait X extends Cell
trait Zero extends Cell
trait One extends Cell

case object X extends X
case object Zero extends Zero
case object One extends One

/** 
 * Tape
 *   
 * a tape is made up of a current cell, a list of cells to
 * the left of current and a list of cells to the right of
 * current
 */
case class Tape[+L <: Cells, +C <: Cell, +R <: Cells](left: L, current: C, right: R)

final class TapeOps[L <: Cells, C <: Cell, R <: Cells](t: Tape[L,C,R]) {
  def run[S <: MachineState](s: S)(implicit rs: RunningState[L,C,R,S]) : rs.Out = rs(t,s)
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

/** 
 * MachineState
 *
 * A state that the FSM might be in
 */
trait MachineState
trait Halt extends MachineState
case object Halt extends Halt

/** 
 * LeftState
 * 
 * when the machine transitions to a LeftState, the tape moves one
 * cell to the left  
 */
trait LeftState extends MachineState

/** 
 * RightState
 * 
 * when the machine transitions to a RightState, the tape moves one
 * cell to the right  
 */
trait RightState extends MachineState


/** 
 * Transition
 * 
 * a transition defines how the FSM moves from one state to another.
 * it says, when we are in the FromState, and the current cell on
 * the tape matches FromCell, we should write Write to the tape and
 * transition to ToState
 *
 * all possible transition should be in implicit scope
 */
case class Transition[FromState <: MachineState,
                      ToState <: MachineState,
                      FromCell <: Cell,
                      Write <: Cell](fromState: FromState, toState: ToState, write: Write)


/** 
 * RunningState
 * 
 * running state is a combination of the current Tape and the current
 * MachineState  
 */
trait RunningState[TL <: Cells, TC <: Cell, TR <: Cells, S <: MachineState] {
  type Out
  def apply(tape: Tape[TL, TC, TR], state: S): Out
}

object RunningState {
  implicit def runningstate[TL <: Cells, TC <: Cell, TR <: Cells, S <: MachineState, Out0](implicit runningstateaux: RunningStateAux[TL, TC, TR, S, Out0]) = new RunningState[TL, TC, TR, S] {
    type Out = Out0
    def apply(tape: Tape[TL, TC, TR], state: S): Out = runningstateaux(tape, state)
  }
}

trait RunningStateAux[TL <: Cells, TC <: Cell, TR <: Cells, S <: MachineState, Out] {
  def apply(tape: Tape[TL, TC, TR], state: S): Out
}

object RunningStateAux {
  implicit def halted[TL <: Cells, 
                      TC <: Cell, 
                      TR <: Cells] : RunningStateAux[TL, TC, TR, Halt, Tape[TL, TC, TR]] =
    new RunningStateAux[TL, TC, TR, Halt, Tape[TL, TC, TR]] {
      def apply(tape: Tape[TL, TC, TR], state: Halt): Tape[TL, TC, TR] = tape
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
   nextRunningState: RunningStateAux[TLT,UC,TC :: TR, N, Out]) =
     new RunningStateAux[TLH :: TLT, TC, TR, S, Out] {
       def apply(tape: Tape[TLH :: TLT, TC, TR], state: S) : Out = {
         nextRunningState(Tape(tape.left.tail, transition.write, turing.::(tape.current, tape.right)), transition.toState)
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
   nextRunningState: RunningStateAux[TC :: TL, UC, TRT, N, Out]) =
     new RunningStateAux[TL, TC, TRH :: TRT, S, Out] {
       def apply(tape: Tape[TL, TC, TRH :: TRT], state: S) : Out = {
         nextRunningState(Tape(turing.::(tape.current, tape.left), transition.write, tape.right.tail), transition.toState)
       }
     }
}

