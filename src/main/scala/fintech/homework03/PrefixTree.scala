package fintech.homework03


// Реализовать интерфейс PrefixTree
// Интерфейс позволяет складывать объекты произвольного класса V по заданному "пути" Seq[K] в дерево
// и изымать их используя комбинацию методов sub и get

// Например, можно на каждом "уровне" дерева хранить Option[V] и Map[K, PrefixTree[K, V]]

trait PrefixTree[K, +V] {
  def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U]

  def sub(path: Seq[K]): PrefixTree[K, V]

  def get: V
}

class Tree[K, +V](val prefixTrees: Map[K, Tree[K, V]], val value: Option[V]) extends PrefixTree[K, V] {

  def put[U >: V](path: Seq[K], value: U): Tree[K, U] = {
    if (path.isEmpty) new Tree[K, U](prefixTrees, Some[U](value))
    else if (prefixTrees.contains(path.head)) {
      val subTree = prefixTrees(path.head).put(path.tail, value)
      new Tree(prefixTrees + (path.head -> subTree), this.value)
    }
    else new Tree[K, U](prefixTrees + (path.head -> new Tree[K, U](Map.empty, None).put(path.tail, value)), this.value)
  }

  override def hashCode(): Int = {
    val prime = 97
    var hash = 1
    hash = prime * hash + prefixTrees.hashCode()
    hash = prime * hash + value.hashCode()
    hash
  }

  override def equals(a: Any): Boolean = {
    a match {
      case tree: Tree[K, V] => tree.prefixTrees == prefixTrees && tree.value.getOrElse() == value.getOrElse()
      case _ => false
    }
  }

  def sub(path: Seq[K]): Tree[K, V] = {
    if (path.isEmpty) this
    else if (prefixTrees.contains(path.head))
      prefixTrees(path.head).sub(path.tail)
    else new Tree[K, V](Map.empty, None)
  }

  def get: V = value.get
}
