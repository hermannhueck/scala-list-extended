package app

import richlist._

object ListApp extends App {

  println("\n-----")

  println("----- zipWith")
  val res1 = List(1,2,3).zipWith(List(10,20,30))(_ + _)
  println(res1)
  assert(res1 == List(11,22,33))

  println("-----")
}
