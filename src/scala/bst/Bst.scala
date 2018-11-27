package src.scala.bst


case class Bst[T: Ordering](value: T, left: Option[Bst[T]], right: Option[Bst[T]]){
  def insert(newValue: T): Bst[T] = {
    if(Ordering[T].lteq(newValue, value))
      new Bst(value, insert(newValue, left), right)
    else 
      new Bst(value, left, insert(newValue, right))
  }
  private def insert(newValue: T, tree: Option[Bst[T]]): Option[Bst[T]] = tree match {
    case Some(t) => Some(t.insert(newValue))
    case None => Some(Bst(newValue))
  }
}


object Bst {
  def apply[T: Ordering](value: T): Bst[T] = new Bst(value, None, None)

  def constructBstFromList[T: Ordering](values: List[T]) = values match {
    case x :: xs => xs.foldLeft(Bst(x))((tree, value) => tree.insert(value))
    case Nil => throw new IllegalArgumentException("Cannot create tree from empty list")
  }
  
  def balanced[T: Ordering](tree: Option[Bst[T]]): Int = tree match {
    case None => 0
    case Some(t) => {
      val l = balanced(t.left)
      val r = balanced(t.right)
      if(l < 0 || r < 0 || math.abs(l - r) > 1) -1 else 1 + math.max(l,r)
    }

  }

  def isBalanced[T: Ordering](tree: Bst[T]): Boolean = if (balanced(Some(tree)) - 1 > 0 ) true else false

  def main (args:Array[String]){

    val bst: Bst[Int] = Bst.constructBstFromList(List(4, 2, 1, 3, 6, 7, 5))

    print("Is Balanced  - ")
    println(isBalanced(bst))
  }
}
