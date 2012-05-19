import turing._
import turing.Halt._
import turing.RunningState._
import turing.RunningStateAux._

//val startTape = Tape(One :: Zero :: One :: TNil, One, One :: Zero :: One :: TNil)

object TestAdd extends Application {
  class AddState extends LeftState

  implicit val doneAdding = Transition[AddState,turing.Halt.type,Zero.type,One.type](One)
  implicit val stillAdding = Transition[AddState,AddState,One.type,Zero.type](Zero)

  val emptyTape = Tape(TNil,One,TNil)

  emptyTape.run(Halt)
}
