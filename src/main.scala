package src

object main extends App {
  override def main(args: Array[String]): Unit = {
    val tree = new AVL[Int]
    var full_tree = tree.add(9)
    full_tree = full_tree.add(5)
    full_tree = full_tree.add(11)
    full_tree = full_tree.add(0)
    full_tree = full_tree.add(17)
    full_tree = full_tree.add(13)
    full_tree = full_tree.add(2)
    full_tree = full_tree.add(6)
    full_tree = full_tree.remove(9)

    println(full_tree.height())
    TreePrinter.print_tree(full_tree)
  }
}
