//    This software is Copyright 2012, Mike (stew) O'Connor <stew@vireo.org>
//
//  This software is dual licensed under the GPL-3 and the Apache 2.0
//  license.  Feel free to use, modify, and redistribute this software
//  under the terms of either license.  Both licenses appear verbatim in
//  the file named COPYING which you should have received as part of this
//  software.

package turing

/** 
* Cell
*   
* The tape is made of of a linear sequence of Cells.
* There are three types of cells, X (empty), One, Zero
*/
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

/**
 * TapeOps
 *
 * pimp the Tape object to provice the run method which actually
 * calculates the result
 */
final class TapeOps[L <: Cells, C <: Cell, R <: Cells](t: Tape[L,C,R]) {
  def run[S <: MachineState](s: S)(implicit rs: RunningState[L,C,R,S]) : rs.Out = rs(t,s)
}

object Tape {
  implicit def tapeOps[L <: Cells, C <: Cell, R <: Cells](t : Tape[L,C,R]) : TapeOps[L,C,R] = new TapeOps(t)
}

/** 
 * Cells
 *   
 * a heterogenous list of cells which are either X, One, or Zero
 *
 * This is used to store the list of cells to the right or left
 * of the current cell
 */
sealed trait Cells

final case class ::[+H <: Cell, +T <: Cells](head: H, tail: T) extends Cells {
  override def toString = head+" :: "+tail.toString
}

/** 
 * TNil
 *   
 * marks either the left or right end of a tape
 */
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

/** 
 * RunningStateAux
 *   
 * this is just a rearrangement of the above which has our much desired
 * Out type as another type argument
 */
trait RunningStateAux[TL <: Cells, TC <: Cell, TR <: Cells, S <: MachineState, Out] {
def apply(tape: Tape[TL, TC, TR], state: S): Out
}

object RunningStateAux {
  /** 
  * halted
  *
  * provide an implicit RunningStateAux object for any Tape for a running
  * machine in the Halt state.  If a machine enters the halt state, we now
  * know what the Tape in the Out position looks like
  */ 
  implicit def halted[TL <: Cells, 
                      TC <: Cell, 
                      TR <: Cells] : RunningStateAux[TL, TC, TR, Halt.type, Tape[TL, TC, TR]] =
    new RunningStateAux[TL, TC, TR, Halt.type, Tape[TL, TC, TR]] {
      def apply(tape: Tape[TL, TC, TR], state: Halt.type): Tape[TL, TC, TR] = tape
    }


  /** 
   * previousLeftState
   *
   * if we have an implicit Transition available that takes us from some
   * LeftState state to a state for which there is alread an implicit
   * RunningStateAux, we supply an implicit RunningAuxState for the the
   * tape in the state it would be in before the LeftState was reached
   * 
  */ 
  implicit def previousLeftState[TLH <: Cell, 
                                 TLT <: Cells, 
                                 TC <: Cell, 
                                 TR <: Cells,
                                 UC <: Cell,
                                 S <: LeftState,
                                 N <: MachineState,
                                 Out]
  (implicit transition: Transition[S, N, TLH, UC],
   nextRunningState: RunningStateAux[TLT,UC,TC :: TR, N, Out]) =
     new RunningStateAux[TLH :: TLT, TC, TR, S, Out] {
       // remove the head from the tape on the left of current the
       // center of the tape becomes whatever the state transition is
       // supposed to write
       // right of the tape gets the previous center pushed onto the head
       def apply(tape: Tape[TLH :: TLT, TC, TR], state: S) : Out = {
         nextRunningState(Tape(tape.left.tail, transition.write, turing.::(tape.current, tape.right)), transition.toState)
       }
     }

  /** 
   * previousRightState
   *
   * if we have an implicit Transition available that takes us from some
   * RightState state to a state for which there is alread an implicit
   * RunningStateAux, we supply an implicit RunningAuxState for the the
   * tape in the state it would be in before the RightState was reached
   * 
   */ 
  implicit def previousRightState[TL <: Cells, 
                                  TC <: Cell, 
                                  TRH <: Cell, 
                                  TRT <: Cells,
                                  UC <: Cell,
                                  S <: RightState,
                                  N <: MachineState,
                                  Out]
  (implicit transition: Transition[S, N, TRH, UC],
   nextRunningState: RunningStateAux[TC :: TL, UC, TRT, N, Out]) =
     new RunningStateAux[TL, TC, TRH :: TRT, S, Out] {
       // remove the head from the tape on the right of current the
       // center of the tape becomes whatever the state transition is
       // supposed to write
       // left of the tape gets the previous center pushed onto the head
       def apply(tape: Tape[TL, TC, TRH :: TRT], state: S) : Out = {
         nextRunningState(Tape(turing.::(tape.current, tape.left), transition.write, tape.right.tail), transition.toState)
       }
     }
}

