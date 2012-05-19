import turing._
import turing.Halt._
import turing.RunningState._
import turing.RunningStateAux._

//val startTape = Tape(One :: Zero :: One :: TNil, One, One :: Zero :: One :: TNil)

object TestAdd extends Application {

  class Adding extends LeftState
  case object Adding extends Adding
  
  implicit val doneAdding = Transition[Adding, Halt.type, Zero,One](Adding,Halt,One)
  implicit val stillAdding = Transition[Adding,Adding,One,Zero](Adding,Adding,Zero)

  val emptyTape = Tape[TNil,One,TNil](TNil,One,TNil)

//  implicit val rsa : RunningStateAux[TNil,One,TNil,Halt,Tape[TNil,One,TNil]]= 
//    RunningStateAux.halted
  implicit val rs : RunningState[TNil,One,TNil,Halt]  = RunningState.runningstate

  emptyTape.run(Halt)
}
