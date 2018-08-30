package object richlist { self =>

  implicit class Richlist[A](thisl: List[A]) {

    def zipWith[B, C](thatl: List[B])(f: (A, B) => C): List[C] =
      thisl.zip(thatl).map {
        case (a, b) => f(a, b)
      }

    def zipAllWith[B, C](thatl: List[B], thisDefault: A, thatDefault: B)(f: (A, B) => C): List[C] =
      thisl.zipAll(thatl, thisDefault, thatDefault).map {
        case (a, b) => f(a, b)
      }

    def intersperse(a: A): List[A] = thisl match {
      case Nil => List()
      case x :: Nil => List(x)
      case x :: xs => x :: a :: xs.intersperse(a)
    }

    // uses implicit conversion: A => List[B]
    def concat2[B](implicit conv: A => List[B]): List[B] =
      thisl.flatten

    // uses implicit evidence: B <:< List[A]
    def concat3[B](implicit ev: B <:< List[A]): List[A] =
      thisl.asInstanceOf[List[B]].flatten

    def concatMap[B](f: A => List[B]): List[B] =
      thisl.flatMap(f)

    def and(implicit ev: A <:< Boolean): Boolean =
      !thisl.contains(false)

    def or(implicit ev: A <:< Boolean): Boolean =
      thisl.contains(true)

    def any(p: A => Boolean): Boolean =
      thisl.exists(p)

    def all(p: A => Boolean): Boolean =
      thisl.forall(p)

    def elem(a: A): Boolean =
      thisl.contains(a)

    def notElem(a: A): Boolean =
      !thisl.contains(a)

    def isInfixOf(that: List[A]): Boolean =
      that.indexOfSlice(thisl) >= 0

    def isPrefixOf(that: List[A]): Boolean =
      that.indexOfSlice(thisl) == 0

    def isSuffixOf(that: List[A]): Boolean =
      that.length - that.indexOfSlice(thisl) == thisl.length

    def partition2(p: A => Boolean): (List[A], List[A]) =
      thisl.foldRight((List.empty[A], List.empty[A])) { case (a, (trues, falses)) =>
        if (p(a)) (a :: trues, falses) else (trues, a :: falses)
      }

    def span2(p: A => Boolean): (List[A], List[A]) = {

      def go(acc: List[A], rest: List[A]): (List[A], List[A]) = rest match {
        case Nil => (acc.reverse, Nil)
        case x :: xs if !p(x) => (acc.reverse, x :: xs)
        case x :: xs => go(x :: acc, xs)
      }

      go(List.empty[A], thisl)
    }

    def break(p: A => Boolean): (List[A], List[A]) =
      span2(!p(_))

    def elemIndex(a: A): Option[Int] =
      Option(thisl.indexOf(a)).filter(_ >= 0)

    def elemIndex2(a: A): Option[Int] =
      thisl.elemIndices(a).headOption

    def elemIndices(elem: A): List[Int] =
      thisl.zipWithIndex.foldRight(List.empty[Int]) { case ((a, index), acc) =>
        if (a == elem) index :: acc else acc
      }

    def elemIndices2(elem: A): List[Int] =
      findIndices(_ == elem)

    def findIndex(p: A => Boolean): Option[Int] =
      Option(thisl.indexWhere(p)).filter(_ >= 0)

    def findIndices(p: A => Boolean): List[Int] =
      thisl.zipWithIndex.foldRight(List.empty[Int]) { case ((a, index), acc) =>
        if (p(a)) index :: acc else acc
      }

    def zip3[B, C](l2: List[B], l3: List[C]): List[(A, B, C)] =
      thisl zip l2 zip l3 map { case ((a, b), c) => (a, b, c) }

    def zip4[B, C, D](l2: List[B], l3: List[C], l4: List[D]): List[(A, B, C, D)] =
      (thisl zip l2) zip (l3 zip l4) map { case ((a, b), (c, d)) => (a, b, c, d) }

    def zipWith3[B, C, D](l2: List[B], l3: List[C])(f: (A, B, C) => D): List[D] =
      thisl.zip3(l2, l3).map {
        case (a, b, c) => f(a, b, c)
      }

    def zipWith4[B, C, D, E](l2: List[B], l3: List[C], l4: List[D])(f: (A, B, C, D) => E): List[E] =
      thisl.zip4(l2, l3, l4).map {
        case (a, b, c, d) => f(a, b, c, d)
      }

    def filterDuplicates: List[A] =
      thisl.foldLeft(List.empty[A]) { (acc, a) =>
        if (acc.contains(a)) acc else a :: acc
      }.reverse
    def nub: List[A] = thisl.filterDuplicates

    def delete(a: A): List[A] = {
      val (left, right) = thisl.break(_ == a)
      if (right.nonEmpty && right.head == a)
        left ++ right.tail
      else
        thisl
    }

    def difference(thatl: List[A]): List[A] = (thisl, thatl) match { // Haskell: \\
      case (Nil, _) => Nil
      case (l1, Nil) => l1
      case (l1, head :: tail) => l1.delete(head).difference(tail)
    }

    def union2(thatl: List[A]): List[A] =
      thisl ++ thatl.distinct.difference(thisl.distinct)

    def insert(a: A)(implicit ord: Ordering[A]): List[A] = thisl match {
      case Nil => List(a)
      case x :: xs if ord.lteq(a, x) => a :: x :: xs
      case x :: xs => x :: xs.insert(a)
    }
  }


  implicit class Richlist2[A](thisl: List[List[A]]) {

    def intercalate(la: List[A]): List[A] =
      thisl.intersperse(la).flatten

    def transposeExt: List[List[A]] = {

      def go(rest: List[List[A]], acc: List[List[A]]): List[List[A]] = rest match {
        case Nil => acc
        case as :: ass =>
          val lla: List[List[A]] = as map (List(_))
          val newAcc: List[List[A]] = lla.zipAllWith(acc, Nil, Nil) { (xs, ys) =>
            if (xs.isEmpty) ys else xs.head :: ys
          }
          go(ass, newAcc)
      }

      go(thisl, Nil) map (_.reverse)
    }

    def concat: List[A] = thisl.flatten
  }


  implicit class Richlist3(thisl: List[String]) {

    def unlines: String =
      thisl.mkString("\n")

    def unwords: String =
      thisl.mkString(" ")
  }


  object RichList {

    def iterate[A](x: A, count: Int)(f: A => A): List[A] = {

      def go(x: A, f: A => A, acc: List[A], count: Int): List[A] =
        if (count <= 0) acc
        else go(f(x), f, acc :+ x, count-1)

      go(x, f, List.empty[A], count)
    }

    def lines(str: String): List[String] =
      str.split('\n').toList

    def words(str: String): List[String] =
      str.split("\\W+").toList
  }
}
