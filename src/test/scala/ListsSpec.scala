import org.scalatest.{FlatSpec, Matchers}

class ListsSpec extends FlatSpec with Matchers {
  "last" should "return the last element of a list" in new Lists {
    last(List(1, 1, 2, 3, 5, 8)) shouldBe 8
  }

  "penultimate" should "return the second to last element of a list" in new Lists {
    penultimate(List(1, 1, 2, 3, 5, 8)) shouldBe 5
  }

  "nth" should "return the nth element of a list" in new Lists {
    nth(2, List(1, 1, 2, 3, 5, 8)) shouldBe 2
  }

  "length" should "return the number of elements in a list" in new Lists {
    length(List(1, 1, 2, 3, 5, 8)) shouldBe 6
  }

  "reverse" should "return the reverse list of elements" in new Lists {
    reverse(List(1, 1, 2, 3, 5, 8)) shouldBe List(8, 5, 3, 2, 1, 1)
  }

  "isPalindrome" should "return true for List(1, 2, 3, 2, 1)" in new Lists {
    isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
  }

  "isPalindrome" should "return false for List(4, 2, 3, 2, 1)" in new Lists {
    isPalindrome(List(4, 2, 3, 2, 1)) shouldBe false
  }

  "flatten" should "return a list of lists flattened to a single list" in new Lists {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldBe List(1, 1, 2, 3, 5, 8)
  }

  "compress" should "eliminate consecutive duplicates of a list" in new Lists {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List('a, 'b, 'c, 'a, 'd, 'e)
  }

  "pack" should "pack consecutive duplicates of list elements into sublists" in new Lists {
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List(
      List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)
    )
  }

  "encode" should "implement the so-called run-length encoding data compression method" in new Lists {
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
  }

  "encodeModified" should "encode in such a way that if an element has no duplicates it is simply copied into the result list" in new Lists {
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
  }

  "decode" should "uncompress an encoded list" in new Lists {
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) shouldBe List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  }

  "encodeDirect" should "implement the so-called run-length encoding data compression method directly" in new Lists {
    encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
  }

  "duplicate" should "duplicate the elements of a list" in new Lists {
    duplicate(List('a, 'b, 'c, 'c, 'd)) shouldBe List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  }

  "duplicateN" should "duplicate the elements of a list a given number of times" in new Lists {
    duplicateN(3, List('a, 'b, 'c, 'c, 'd)) shouldBe List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  }

  "drop" should "drop every Nth element from a list" in new Lists {
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }

  "split" should "split a list into two parts" in new Lists {
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe(List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  "slice" should "extract a slice from a list" in new Lists {
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List('d, 'e, 'f, 'g)
  }

  "rotate" should "rotate a list N places to the left" in new Lists {
    rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  }

  "removeAt" should "remove the Kth element from a list" in new Lists {
    removeAt(1, List('a, 'b, 'c, 'd)) shouldBe(List('a, 'c, 'd), 'b)
  }

  "insertAt" should "insert an element at a given position into a list" in new Lists {
    insertAt('new, 1, List('a, 'b, 'c, 'd)) shouldBe List('a, 'new, 'b, 'c, 'd)
  }

  "range" should "create a list containing all integers within a given range" in new Lists {
    range(4, 9) shouldBe List(4, 5, 6, 7, 8, 9)
  }

  "randomSelect" should "extract a given number of randomly selected elements from a list" in new Lists {
    val rs: List[Symbol] = randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    rs.length shouldBe 3
    rs.distinct.length shouldBe 3
  }

  "lotto" should "draw N different random numbers from the set 1..M" in new Lists {
    val l: List[Int] = lotto(6, 49)
    l.length shouldBe 6
    l.distinct.length shouldBe 6
  }

  "randomPermute" should "generate a random permutation of the elements of a list" in new Lists {
    randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)) should not be List('a, 'b, 'c, 'd, 'e, 'f)
  }

  "combinations" should "generate the combinations of K distinct objects chosen from the N elements of a list" in new Lists {
    val c: List[List[Symbol]] = combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
    assert(c.distinct.length == c.length)
  }
}
