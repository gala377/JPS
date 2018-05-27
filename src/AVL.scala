package src

class AVL[T] {


  case class Node (value: T, left: Option[Node], right: Option[Node]) {
    def this(value: T) = this(value, None, None)
  }




  def depth(): Int = {
    root match {
      case Some(node) => subtree_depth(node)
      case None => 0
    }
  }

  private def subtree_depth(node: Node): Int = {

  }

}
