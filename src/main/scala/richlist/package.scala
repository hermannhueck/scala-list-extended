package object richlist {

  implicit class Richlist[A](l: List[A]) {

    def zipWith[B, C](l2: List[B])(f: (A, B) => C): List[C] =
      l.zip(l2).map {
        case (a, b) => f(a, b)
      }
  }
}
