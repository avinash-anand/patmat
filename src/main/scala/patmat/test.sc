import patmat.Huffman

val x = "abcd".toList
val y = x.filter(a => a.toInt > 200).headOption
val y2 = x.find(a => a.toInt > 200)
y == y2
val aa = "aaabbaacchhggddccvvggbbddaa".toList
val b = Huffman.times(aa)
