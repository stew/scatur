package turing

//val startTape = Tape(One :: Zero :: One :: TNil, One, One :: Zero :: One :: TNil)

object TestTrivial extends App {

  val emptyTape = Tape[TNil,One,TNil](TNil,One,TNil)
  val completed = emptyTape.run(Halt)
}
