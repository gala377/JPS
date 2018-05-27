package src

object TreePrinter {

  def print_tree[T](tree: AVL[T]): Unit = {
    val root = tree.getRoot()
    print_node(root, 0)
  }

  private def print_node[T](node_opt:  Option[Node[T]], depth: Int): Unit = {
    val node  = node_opt match {
      case None => return
      case Some(opt_val) => opt_val
    }
    for(i <- 0 to depth) {
      print("----")
    }
    print(node.value)
    print("\n")
    print_node(node.left, depth+1)
    print_node(node.right, depth+1)
  }
}
