import patmat.Huffman

val x = "abcd".toList
val y = x.filter(a => a.toInt > 200).headOption
val y2 = x.find(a => a.toInt > 200)
y == y2
val aa = "aaabbaacchhggddccvvggbbddaa".toList
val b = Huffman.times(aa)
val c = List[String]().sortWith(_.length > _.length)
val d = List(('t', 2), ('e', 1), ('x', 3))
val e = Huffman.makeOrderedLeafList(d)
