package patmat

import common._

import scala.annotation.tailrec

/**
  * Assignment 4: Huffman coding
  *
  */
object Huffman {

  /**
    * A huffman code is represented by a binary tree.
    *
    * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
    * The weight of a `Leaf` is the frequency of appearance of the character.
    *
    * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
    * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
    * leaves.
    */
  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree


  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Fork(left, right, chars, weight) => this.weight(left) + this.weight(right)
    case Leaf(char, weight) => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(left, right, chars, weight) => this.chars(left) ::: this.chars(right)
    case Leaf(char, weight) => List(char)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  // Part 2: Generating Huffman trees

  /**
    * In this assignment, we are working with lists of characters. This function allows
    * you to easily create a character list from a given string.
    */
  def string2Chars(str: String): List[Char] = str.toList

  /**
    * This function computes for each unique character in the list `chars` the number of
    * times it occurs. For example, the invocation
    *
    * times(List('a', 'b', 'a'))
    *
    * should return the following (the order of the resulting list is not important):
    *
    * List(('a', 2), ('b', 1))
    *
    * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
    * character and an integer. Pairs can be constructed easily using parentheses:
    *
    * val pair: (Char, Int) = ('c', 1)
    *
    * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
    *
    * val theChar = pair._1
    * val theInt  = pair._2
    *
    * Another way to deconstruct a pair is using pattern matching:
    *
    * pair match {
    * case (theChar, theInt) =>
    * println("character is: "+ theChar)
    * println("integer is  : "+ theInt)
    * }
    */
  def times(chars: List[Char]): List[(Char, Int)] = {
    def createList(list: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = list match {
      case Nil => acc
      case x :: xs =>
        val (char, count) = (x, list.count(_ == x))
        createList(xs.filterNot(_ == char), (char, count) :: acc)
    }

    createList(chars, Nil)
    //    chars match {
    //      case Nil => Nil
    //      case x :: xs =>
    //        val (char, count) = (x, chars.count(_ == x))
    //        (char, count) :: times(xs.filterNot(_ == char))
    //    }
  }

  /**
    * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
    *
    * The returned list should be ordered by ascending weights (i.e. the
    * head of the list should have the smallest weight), where the weight
    * of a leaf is the frequency of the character.
    */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def createList(list: List[(Char, Int)], accLeaves: List[Leaf]): List[Leaf] = list match {
      case Nil => accLeaves
      case x :: xs => createList(xs, Leaf(x._1, x._2) :: accLeaves)
    }

    val leaves = createList(freqs, Nil)
    leaves.sortWith(_.weight < _.weight)
  }

  /**
    * Checks whether the list `trees` contains only one single code tree.
    */
  def singleton(trees: List[CodeTree]): Boolean = trees.lengthCompare(1) == 0

  /**
    * The parameter `trees` of this function is a list of code trees ordered
    * by ascending weights.
    *
    * This function takes the first two elements of the list `trees` and combines
    * them into a single `Fork` node. This node is then added back into the
    * remaining elements of `trees` at a position such that the ordering by weights
    * is preserved.
    *
    * If `trees` is a list of less than two elements, that list should be returned
    * unchanged.
    */
  def combine(trees: List[CodeTree]): List[CodeTree] = {
    trees match {
      case Nil => trees
      case x :: Nil => trees
      case x1 :: x2 :: xs => (makeCodeTree(x1, x2) :: xs) sortWith ((a1, a2) => weight(a1) < weight(a2))
    }
  }

  /**
    * This function will be called in the following way:
    *
    * until(singleton, combine)(trees)
    *
    * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
    * the two functions defined above.
    *
    * In such an invocation, `until` should call the two functions until the list of
    * code trees contains only one single tree, and then return that singleton list.
    *
    * Hint: before writing the implementation,
    * - start by defining the parameter types such that the above example invocation
    * is valid. The parameter types of `until` should match the argument types of
    * the example invocation. Also define the return type of the `until` function.
    * - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
    */
  def until(singletonFn: List[CodeTree] => Boolean, combineFn: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (singletonFn(trees)) trees else until(singletonFn, combineFn)(combineFn(trees))
  }

  /**
    * This function creates a code tree which is optimal to encode the text `chars`.
    *
    * The parameter `chars` is an arbitrary text. This function extracts the character
    * frequencies from that text and creates a code tree based on them.
    */
  def createCodeTree(chars: List[Char]): CodeTree = {
    val freqOfChars: List[(Char, Int)] = times(chars)
    val orderedLeafList: List[Leaf] = makeOrderedLeafList(freqOfChars)
    val list = until(singleton, combine)(orderedLeafList)
    list.head
  }


  // Part 3: Decoding

  type Bit = Int
  /**
    * Decoding also starts at the root of the tree.
    * Given a sequence of bits to decode, we successively read the bits,
    * and for each 0, we choose the left branch, and for each 1 we choose the right branch.
    * When we reach a leaf, we decode the corresponding character and then start again at the root of the tree.
    */
  /**
    * This function decodes the bit sequence `bits` using the code tree `tree` and returns
    * the resulting list of characters.
    * Your implementation must traverse the coding tree for each character,
    * a task that should be done using a helper function.
    */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    // Non-tail recursive way
    //    def decodeHelper(currTree: CodeTree, bits: List[Bit]): List[Char] = bits match {
    //      case Nil => currTree match {
    //        case Fork(_, _, _, _) =>
    //          println("This shouldn't be reached until invalid bit sequence")
    //          Nil
    //        case Leaf(char, _) =>
    //          println(s"This means single leaf tree - char - $char")
    //          List(char)
    //      }
    //      case _ => currTree match {
    //        case Fork(left, right, _, _) => bits.head match {
    //          case 0 =>
    //            println(s"this means take left fork and decrease bits - bits = $bits")
    //            decodeHelper(left, bits.tail)
    //          case 1 =>
    //            println(s"this means take right fork and decrease bits - bits = $bits")
    //            decodeHelper(right, bits.tail)
    //        }
    //        case Leaf(char, _) =>
    //          println(s"this means a leaf is reached and start decoding rest of bits from root - char - $char")
    //          List(char) ::: decodeHelper(tree, bits)
    //      }
    //    }
    //    decodeHelper(tree, bits)

    //Tail recursion - bit confusing
    def decodeHelper2(root: CodeTree, currTree: CodeTree, bits: List[Bit], acc: List[Char]): List[Char] = {
      bits match {
        case Nil => currTree match {
          case Fork(_, _, _, _) => acc // This means invalid encoding key
          case Leaf(c, _) => c :: acc // this means single leaf tree
        }
        case _ => currTree match {
          case Fork(left, right, _, _) => bits.head match {
            case 0 => decodeHelper2(root, left, bits.tail, acc)
            case 1 => decodeHelper2(root, right, bits.tail, acc)
          }
          case Leaf(c, _) => decodeHelper2(root, root, bits, c :: acc)
        }
      }
    }

    val reversedChars = decodeHelper2(tree, tree, bits, Nil)
    reversedChars.reverse
  }


  /**
    * A Huffman coding tree for the French language.
    * Generated from the data given at
    * http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
    */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
    * What does the secret message say? Can you decode it?
    * For the decoding use the `frenchCode` Huffman tree defined above.
    **/
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
    * Write a function that returns the decoded secret
    */
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
    * For a given Huffman tree,
    * one can obtain the encoded representation of a character
    * by traversing from the root of the tree to the leaf containing the character.
    * Along the way, when a left branch is chosen, a 0 is added to the representation,
    * and when a right branch is chosen, 1 is added to the representation.
    */
  /**
    * This function encodes `text` using the code tree `tree`
    * into a sequence of bits.
    */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def loop(codeTree: CodeTree, remainingText: List[Char], acc: List[Bit]): List[Bit] = {
      remainingText match {
        case Nil => acc
        case head :: tail =>
          val bitsOfHead = traverseToFindHead(head, tree)
          loop(tree, tail, acc ::: bitsOfHead)
      }
    }

    def traverseToFindHead(char: Char, tree: CodeTree): List[Bit] = {
      def loop(tree: CodeTree, acc: List[Bit]): List[Bit] = {
        tree match {
          case Leaf(c, _) if c == char => acc
          case Fork(left, _, _, _) if chars(left).contains(char) => loop(left, 0 :: acc)
          case Fork(_, right, _, _) if chars(right).contains(char) => loop(right, 1 :: acc)
        }
      }

      loop(tree, Nil)
    }

    loop(tree, text, Nil)
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
    * This function returns the bit sequence that represents the character `char` in
    * the code table `table`.
    */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table.toMap.get(char) match {
    case Some(list) => list
    case None => Nil
  }

  /**
    * The creation of the table is defined by convert
    * which traverses the coding tree and constructs the character table.
    */
  /**
    * Given a code tree, create a code table which contains, for every character in the
    * code tree, the sequence of bits representing that character.
    *
    * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
    * a valid code tree that can be represented as a code table. Using the code tables of the
    * sub-trees, think of how to build the code table for the entire tree.
    */
  def convert(tree: CodeTree): CodeTable = {
    tree match {
      case Leaf(char, _) => List((char, Nil))
      case Fork(left, right, _, _) => mergeCodeTables(convert(left), convert(right))
    }
  }

  /**
    * This function takes two code tables and merges them into one. Depending on how you
    * use it in the `convert` method above, this merge method might also do some transformations
    * on the two parameter code tables.
    */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a.map(e => (e._1, 0 :: e._2)) ::: b.map(e => (e._1, 1 :: e._2))

  /**
    * This function encodes `text` according to the code tree `tree`.
    *
    * To speed up the encoding process, it first converts the code tree to a code table
    * and then uses it to perform the actual encoding.
    */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val codeTable = convert(tree)
//    def loop(text: List[Char], acc: List[Bit]): List[Bit] = {
//      text match {
//        case Nil => acc
//        case head :: tail => loop(tail, codeBits(codeTable)(head) ::: acc)
//      }
//    }
//    val reverseEncoding = loop(text, Nil)
//    reverseEncoding.reverse
    text flatMap codeBits(convert(tree))
  }
}
