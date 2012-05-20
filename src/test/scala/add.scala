//    This software is Copyright 2012, Mike (stew) O'Connor <stew@vireo.org>
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.

package turing

object TestAdd extends App {

  class FindEnd extends RightState
  case object FindEnd extends FindEnd

  class Adding extends LeftState
  case object Adding extends Adding
  
  class FindBeginning extends LeftState
  case object FindBeginning extends FindBeginning

  class RightOneMore extends RightState
  case object RightOneMore extends RightOneMore
  
  implicit val findEndOne = Transition[FindEnd.type, FindEnd.type, One.type, One.type](FindEnd,FindEnd,One)
  implicit val findEndZero = Transition[FindEnd.type, FindEnd.type, Zero.type, Zero.type](FindEnd,FindEnd,Zero)
  implicit val findEndX = Transition[FindEnd.type, Adding.type, X.type, X.type](FindEnd,Adding,X)


  implicit val addingOne = Transition[Adding.type, Adding.type, One.type, Zero.type](Adding,Adding,Zero)
  implicit val addingZero = Transition[Adding.type, FindBeginning.type, Zero.type, One.type](Adding,FindBeginning,One)
  implicit val addingX = Transition[Adding.type, FindBeginning.type, X.type, One.type](Adding,FindBeginning,One)


  implicit val fbOne = Transition[FindBeginning.type, FindBeginning.type, One.type, One.type](FindBeginning,FindBeginning,One)

  implicit val fbZero = Transition[FindBeginning.type, FindBeginning.type, Zero.type, Zero.type](FindBeginning,FindBeginning,Zero)

  implicit val fbX = Transition[FindBeginning.type, RightOneMore.type, X.type, X.type](FindBeginning,RightOneMore,X)

  implicit val romX = Transition[RightOneMore.type, Halt.type, X.type, X.type](RightOneMore,Halt,X)
  implicit val romOne = Transition[RightOneMore.type, Halt.type, One.type, One.type](RightOneMore,Halt,One)
  implicit val romZero = Transition[RightOneMore.type, Halt.type, Zero.type, Zero.type](RightOneMore,Halt,Zero)

  val startTape = Tape( X :: TNil, One, One :: Zero :: One :: X :: X :: TNil)


  val completed = startTape.run(FindEnd)


  // this is a utility to witness that a type is the type we think it
  // is without influencing the compiler's type inference
  def typed[T](t : => T) {}

  // if this compiles, then "completed" is the type we think it is
  typed[Tape[X :: TNil, One, One :: One :: Zero :: X :: X :: TNil ]]( completed)

}
