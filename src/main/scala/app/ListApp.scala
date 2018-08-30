package app

import richlist._

object ListApp extends App {

  println("\n===== Extended List Functions (inspired by Haskell's List")

  println("----- zipWith")
  val res01 = List(1,2,3).zipWith(List(10,20,30))(_ + _)
  println(res01)
  assert(res01 == List(11,22,33))

  println("----- intersperse")
  val res02 = "MONKEY".toList.intersperse('.').mkString
  println(res02)
  assert(res02 == "M.O.N.K.E.Y")
  val res03 = List(1,2,3,4,5,6).intersperse(0)
  println(res03)
  assert(res03 == List(1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6))

  println("----- intercalate")
  val res04 = List("hey", "there", "guys").map(_.toList).intercalate(" ".toList).mkString
  println(res04)
  assert(res04 == "hey there guys")
  val res05 = List(List(1,2,3), List(4,5,6), List(7,8,9)).intercalate(List(0,0,0))
  println(res05)
  assert(res05 == List(1, 2, 3, 0, 0, 0, 4, 5, 6, 0, 0, 0, 7, 8, 9))

  println("----- transpose from standard library (requires all list to have the same size)")
  val res06 = List(List(1,2,3), List(4,5,6), List(7,8,9)).transpose
  println(res06)
  assert(res06 == List(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9)))
  println("----- transposeExt (also operates on Lists with different sizes)")
  val res07 = List(List(1,2,3), List(4,5,6), List(7,8,9)).transposeExt
  println(res07)
  assert(res07 == List(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9)))
  val res08 = List("hey", "there", "guys").map(_.toList).transposeExt.map(_.mkString)
  println(res08)
  assert(res08 == List("htg","ehu","yey","rs","e"))
  println("----- Adding polinomials with transposeExt: (3x2 + 5x + 9) + (10x3 + 9) + (8x3 + 5x2 + x - 1)")
  val res09 = List(List(0,3,5,9), List(10,0,0,9), List(8,5,1,-1)).transposeExt map (_.sum)
  println(res09)
  assert(res09 == List(18, 8, 6, 17))

  println("----- concat is flatten")
  val res10 = List("foo","bar","car").map(_.toList).concat.mkString
  println(res10)
  assert(res10 == "foobarcar")
  val res11 = List(List(3,4,5), List(2,3,4), List(2,1,1)).concat
  println(res11)
  assert(res11 == List(3, 4, 5, 2, 3, 4, 2, 1, 1))

  println("----- concatMap is flatMap")
  val res12 = List(1,2,3).concatMap(List.fill(4)(_))
  println(res12)
  assert(res12 == List(1,1,1,1,2,2,2,2,3,3,3,3))

  println("----- and")
  val res13 = List(5,6,7,8).map(_ > 4).and
  println(res13)
  assert(res13)
  val res14 = List(4,4,4,3,4).map(_ == 4).and
  println(res14)
  assert(!res14)

  println("----- or")
  val res15 = List(2,3,4,5,6,1).map(_ == 4).or
  println(res15)
  assert(res15)
  val res16 = List(1,2,3).map(_ > 4).or
  println(res16)
  assert(!res16)

  println("----- any")
  val res17 = List(2,3,4,5,6,1).any(_ == 4)
  println(res17)
  assert(res17)
  val res18 = "HEYGUYSwhatsup".toList.any(('A' to 'Z').toList.elem(_))
  println(res18)
  assert(res18)

  println("----- all")
  val res19 = List(6,9,10).any(_ > 4)
  println(res19)
  assert(res19)
  val res20 = "HEYGUYSwhatsup".toList.all(('A' to 'Z').toList.elem(_))
  println(res20)
  assert(!res20)

  println("----- iterate")
  val res21 = RichList.iterate(1, 10)(_ * 2)
  println(res21)
  assert(res21 == List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512))
  val res22 = RichList.iterate("haha".toList, 3)(_ ++ "haha".toList) map (_.mkString)
  println(res22)
  assert(res22 == List("haha", "hahahaha", "hahahahahaha"))

  println("----- splitAt")
  val res23 = "heyman".toList.splitAt(3)
  println(res23)
  assert(res23 == ("hey".toList, "man".toList))
  val res24 = "heyman".toList.splitAt(100)
  println(res24)
  assert(res24 == ("heyman".toList, "".toList))
  val res25 = "heyman".toList.splitAt(-3)
  println(res25)
  assert(res25 == ("".toList, "heyman".toList))
  val (a26, b26) = "foobar".toList.splitAt(3)
  val res26 = (b26 ++ a26).mkString
  println(res26)
  assert(res26 == "barfoo")

  println("----- isInfixOf")
  val res27 = "cat".toList.isInfixOf("im a cat burglar".toList)
  println(res27)
  assert(res27)
  val res28 = "Cat".toList.isInfixOf("im a cat burglar".toList)
  println(res28)
  assert(!res28)
  val res29 = "cats".toList.isInfixOf("im a cat burglar".toList)
  println(res29)
  assert(!res29)

  println("----- isPrefixOf")
  val res30 = "hey".toList.isPrefixOf("hey there!".toList)
  println(res30)
  assert(res30)
  val res31 = "hey".toList.isPrefixOf("oh hey there!".toList)
  println(res31)
  assert(!res31)

  println("----- isSuffixOf")
  val res32 = "there!".toList.isSuffixOf("oh hey there!".toList)
  println(res32)
  assert(res32)
  val res33 = "there!".toList.isSuffixOf("oh hey there".toList)
  println(res33)
  assert(!res33)

  println("----- partition")
  val res34 = "BOBsidneyMORGANeddy".toList.partition(_.isUpper)
  println(res34)
  assert(res34 == ("BOBMORGAN".toList, "sidneyeddy".toList))
  val res35 = List(1,3,5,6,3,2,1,0,3,7).partition(_>3)
  println(res35)
  assert(res35 == (List(5,6,7), List(1,3,3,2,1,0,3)))

  println("----- span2")
  val res36 = "BOBsidneyMORGANeddy".toList.span2(_.isUpper)
  println(res36)
  assert(res36 == ("BOB".toList, "sidneyMORGANeddy".toList))

  println("----- break")
  val res37 = "BOBsidneyMORGANeddy".toList.break(_.isUpper)
  println(res37)
  assert(res37 == ("".toList, "BOBsidneyMORGANeddy".toList))
  val res38 = "BOBsidneyMORGANeddy".toList.break(_ == 'M')
  println(res38)
  assert(res38 == ("BOBsidney".toList, "MORGANeddy".toList))

  println("----- find")
  val res39 = List(1,2,3,4,5,6).find(_>4)
  println(res39)
  assert(res39.contains(5))
  val res40 = List(1,2,3,4,5,6).find(_>9)
  println(res40)
  assert(res40.isEmpty)

  println("----- elemIndex")
  val res41 = List(1,2,3,4,5,6).elemIndex(4)
  println(res41)
  assert(res41.contains(3))
  val res42 = List(1,2,3,4,5,6).elemIndex(10)
  println(res42)
  assert(res42.isEmpty)

  println("----- elemIndex2")
  val res43 = List(1,2,3,4,5,6).elemIndex2(4)
  println(res43)
  assert(res43.contains(3))
  val res44 = List(1,2,3,4,5,6).elemIndex2(10)
  println(res44)
  assert(res44.isEmpty)

  println("----- elemIndices")
  val res45 = "Where are the spaces?".toList.elemIndices(' ')
  println(res45)
  assert(res45 == List(5,9,13))

  println("----- findIndex")
  val res46 = List(5,3,2,1,6,4).findIndex(_ == 4)
  println(res46)
  assert(res46.contains(5))
  val res47 = List(5,3,2,1,6,4).findIndex(_ == 7)
  println(res47)
  assert(res47.isEmpty)

  println("----- findIndices")
  val res48 = "Where Are The Caps?".toList.findIndices(_.isUpper)
  println(res48)
  assert(res48 == List(0,6,10,14))

  println("----- zip3, zip4, zipWith3, zipWith4")
  val res49 = List(1,2,3).zipWith3(List(4,5,2,2), List(2,2,3))(_ + _ + _)
  println(res49)
  assert(res49 == List(7,9,8))
  val res50 = List(2,3,3).zip4(List(2,2,2), List(5,5,3), List(2,2,2))
  println(res50)
  assert(res50 == List((2,2,5,2), (3,2,5,2), (3,2,3,2)))

  println("----- lines")
  val res51 = RichList.lines("first line\nsecond line\nthird line")
  println(res51)
  assert(res51 == List("first line","second line","third line"))

  println("----- unlines")
  val res52 = List("first line","second line","third line").unlines
  println(res52)
  assert(res52 == "first line\nsecond line\nthird line")

  println("----- words")
  val res53 = RichList.words("hey these are the words in this sentence")
  println(res53)
  assert(res53 == List("hey","these","are","the","words","in","this","sentence"))
  val res54 = RichList.words("hey these           are    the words in this\nsentence")
  println(res54)
  assert(res54 == List("hey","these","are","the","words","in","this","sentence"))

  println("----- unwords")
  val res55 = List("hey","there","mate").unwords
  println(res55)
  assert(res55 == "hey there mate")

  println("----- nub, filterDuplicates ^= distinct")
  val res56 = List(1,2,3,4,3,2,1,2,3,4,3,2,1).nub
  println(res56)
  assert(res56 == List(1,2,3,4))
  val res57 = "Lots of words and stuff".toList.nub.mkString
  println(res57)
  assert(res57 == "Lots fwrdanu")

  println("----- distinct")
  val res56a = List(1,2,3,4,3,2,1,2,3,4,3,2,1).distinct
  println(res56a)
  assert(res56a == List(1,2,3,4))
  val res57a = "Lots of words and stuff".toList.distinct.mkString
  println(res57a)
  assert(res57a == "Lots fwrdanu")

  println("----- delete")
  val res58 = "hey there ghang!".toList.delete('h').mkString
  println(res58)
  assert(res58 == "ey there ghang!")
  val res59 = "hey there ghang!".toList.delete('h').delete('h').mkString
  println(res59)
  assert(res59 == "ey tere ghang!")
  val res60 = "hey there ghang!".toList.delete('h').delete('h').delete('h').mkString
  println(res60)
  assert(res60 == "ey tere gang!")

  println("----- diff")
  val res61 = (1 to 10).toList.diff(List(2,5,9))
  println(res61)
  assert(res61 == List(1,3,4,6,7,8,10))
  val res62 = "Im a big baby".toList.diff("big".toList).mkString
  println(res62)
  assert(res62 == "Im a  baby")

  println("----- difference")
  val res63 = (1 to 10).toList.difference(List(2,5,9))
  println(res63)
  assert(res63 == List(1,3,4,6,7,8,10))
  val res64 = "Im a big baby".toList.difference("big".toList).mkString
  println(res64)
  assert(res64 == "Im a  baby")

  println("----- union")
  val res65 = "hey man".toList.union("man what's up".toList).mkString
  println(res65)
  assert(res65 == "hey manman what's up")
  val res66 = (1 to 7).toList.union((5 to 10).toList)
  println(res66)
  assert(res66 == List(1, 2, 3, 4, 5, 6, 7, 5, 6, 7, 8, 9, 10))

  println("----- union2, Haskell-like (removes duplicate elements of 2nd list")
  val res67 = "hey man".toList.union2("man what's up".toList).mkString
  println(res67)
  assert(res67 == "hey manwt'sup")
  val res68 = (1 to 7).toList.union2((5 to 10).toList)
  println(res68)
  assert(res68 == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

  println("----- intersect")
  val res69 = (1 to 7).toList.intersect((5 to 10).toList)
  println(res69)
  assert(res69 == List(5, 6, 7))

  println("----- insert")
  val res70 = List(3,5,1,2,8,2).insert(4)
  println(res70)
  assert(res70 == List(3,4,5,1,2,8,2))
  val res71 = List(1,3,4,4,1).insert(4)
  println(res71)
  assert(res71 == List(1,3,4,4,4,1))
  val res72 = List(1,2,3,5,6,7).insert(4)
  println(res72)
  assert(res72 == List(1,2,3,4,5,6,7))
  val res73 = (('a' to 'f').toList ++ ('h' to 'z').toList).insert('g')
  println(res73)
  assert(res73 == ('a' to 'z').toList)
  val res74 = List(1,2,4,3,2,1).insert(3)
  println(res74)
  assert(res74 == List(1,2,3,4,3,2,1))

  println("-----")
}
