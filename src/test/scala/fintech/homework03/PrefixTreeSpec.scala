package fintech.homework03

import org.scalatest.{FlatSpec, Matchers}

class PrefixTreeSpec extends FlatSpec with Matchers {
  it should "work well with strings" in {
    val tree: Tree[Char, Int] = new Tree[Char, Int](Map.empty, None)

    val with42: Tree[Char, Int] = tree.put("abcd", 42)
    with42.sub("ab").sub("cd").get should be(42)

    val withDouble: Tree[Char, AnyVal] = with42.put("abcde", 13.0)
    withDouble.sub("ab").sub("cd").get should be(42)
    withDouble.sub("ab").sub("cde").get should be(13.0)
  }

  it should "work with different types" in {
    val tree: Tree[Any, Any] = new Tree[Any, Any](Map.empty, None)

    val anyTree: PrefixTree[Any, Any] = tree.put("ab", 42).put(Seq(1, 2, 3), "ab")

    anyTree.sub(Seq(1, 2, 3)).get should be("ab")
    anyTree.sub("ab").get should be(42)
  }

  it should "return empty prefix tree if key doesn't exist" in {
    val tree: Tree[Char, Int] = new Tree[Char, Int](Map.empty, None)

    val with42: PrefixTree[Char, Int] = tree.put("ab", 42)
    with42.sub("cd") == new Tree[Char, Int](Map.empty, None) should be(true)
  }

  it should "return empty prefix tree if tree is empty" in {
    val tree: Tree[Char, Int] = new Tree[Char, Int](Map.empty, None)
    tree.sub("cd") == new Tree[Char, Int](Map.empty, None) should be(true)
  }

  it should "throw exception if key doesn't exist" in {
    val tree: Tree[Char, Int] = new Tree[Char, Int](Map.empty, None)

    val with42: Tree[Char, Int] = tree.put("ab", 42)
    var thrown = false
    try {
      with42.sub("cd").get
    }
    catch {
      case exception: Exception => thrown = true
    }
    thrown should be(true)
  }

  "Equal prefix trees" should "be equal if the have equal subtrees and equal value" in {
    val tree: Tree[Char, Int] = new Tree[Char, Int](Map.empty, None)

    val tree1: Tree[Char, Int] = tree.put("ab", 42)
    val tree2: Tree[Char, Int] = tree.put("ab", 42)
    val tree3: Tree[Char, Int] = tree.put("ab", 43)
    val tree4: Tree[Char, Int] = tree.put("ac", 42)

    tree1 == tree2 should be(true)
    tree3 == tree2 should be(false)
    tree4 == tree2 should be(false)
  }
}