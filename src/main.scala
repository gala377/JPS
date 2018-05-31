package src

object main extends App {
  override def main(args: Array[String]): Unit = {
    val tree = new AVL[Int]
    var full_tree = tree.add(9)
    full_tree = full_tree.add(11)
    full_tree = full_tree.add(9)
    full_tree = full_tree.add(11)
    full_tree = full_tree.add(13)
    full_tree = full_tree.add(2)
    full_tree = full_tree.add(6)
    full_tree = full_tree.add(9)

    val tree2 = new AVL[Int]
    var full_tree2 = tree2.add(9)
    full_tree2 = full_tree2.add(3)
    full_tree2 = full_tree2.add(4)
    full_tree2 = full_tree2.add(13)
    full_tree2 = full_tree2.add(-1)
    full_tree2 = full_tree2.add(32)
    full_tree2 = full_tree2.add(6)

    var final_tree= full_tree.intersection(full_tree2)
    TreePrinter.print_tree(final_tree)
  }
}
