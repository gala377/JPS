package src

import scala.annotation.tailrec
import scala.math.Ordering

/**
  * Holds generic value of type T, reference to the left child and reference to the right child.
  * @param value: Data in the node
  * @param left: Left child node
  * @param right: Right child node
  * @tparam T: Type of data
  */
case class Node[T](value: T, left: Option[Node[T]], right: Option[Node[T]]) {
  def this(value: T) = this(value, None, None)
}

/**
  * Implements AVL tree
  * @param root root Node of the tree
  * @tparam T: Type of data in the tree
  */
class AVL[T](root: Option[Node[T]]) {
  def this() = this(None)

  /**
    * Returns root node
    * @return root node
    */
  def getRoot(): Option[Node[T]] = {
    root
  }

  /**
    * Returns height of tree(distance from the root node to the furthest leaf)
    * @return Height of the tree
    */
  def height(): Int = {
    height(root)
  }

  private def height(node: Option[Node[T]]): Int = {
    node match {
      case None => -1
      case Some(node) => {
        val left = height(node.left)
        val right = height(node.right)
        1 + scala.math.max(left, right)
      }
    }
  }

  /**
    * Returns a tree containing only union of values in both trees ( every value in the first tree and every value in the second tree)
    * @param tree tree to do union with
    * @param ordering ordering implementation of type T
    * @return AVL tree object that is a union of both trees
    */
  def merge(tree: AVL[T])(implicit ordering: Ordering[T]): AVL[T] = merge(this, tree.getRoot())

  private def merge(tree: AVL[T], node: Option[Node[T]])(implicit ordering: Ordering[T]): AVL[T] = node match {
    case None => tree
    case Some(node) => {
      val new_tree = tree.add(node.value)
      val tree_with_left = merge(new_tree, node.left)
      merge(tree_with_left, node.right)
    }
  }

  /**
    * Returns a tree containing only subset of values in both trees ( values that are in both trees )
    * @param tree tree to do intersection with
    * @param ordering ordering implementation of type T
    * @return new AVL tree object that is an intersection of both trees
    */
  def intersection(tree: AVL[T])(implicit ordering: Ordering[T]): AVL[T] = intersection(new AVL[T], tree.getRoot())

  private def intersection(tree: AVL[T], node: Option[Node[T]])(implicit ordering: Ordering[T]): AVL[T] = node match {
    case None => tree
    case Some(node) => {
      if(find(node.value)){
        val new_tree = tree.add(node.value)
        val tree_with_left = intersection(new_tree, node.left)
        intersection(tree_with_left, node.right)
      } else {
        val tree_with_left = intersection(tree, node.left)
        intersection(tree_with_left, node.right)
      }
    }
  }


  /**
    * Returns balance of the tree ( height of the right subtree minus height of the left subtree )
    * @return Balance of tree
    */
  def balance(): Int = balance(root)

  private def balance(node: Option[Node[T]]): Int = node match {
      case Some(node) => {
        val left = height(node.left)
        val right = height(node.right)
        right - left
      }
      case None => 0
    }

  private def rebalance(): AVL[T] = {
    new AVL[T](rebalance(root))
  }

  private def rebalance(node_opt: Option[Node[T]]): Option[Node[T]] = node_opt match {
      case None => None
      case Some(node) => {
        val l_bal = rebalance(node.left)
        val r_bal = rebalance(node.right)
        val new_node = Node(node.value, l_bal, r_bal)

        if (balance(Some(new_node)) == -2) {
          new_node.left match{
            case None => Some(new_node)
            case Some(l_node) => {
              if (height(l_node.left) >= height(l_node.right))
                rotateRight(Some(new_node))
              else {
                val rotated = Node(new_node.value, rotateLeft(new_node.left), new_node.right)
                rotateRight(Some(rotated))
              }
            }
          }
        } else if (balance(Some(new_node)) == 2) {
          new_node.right match{
            case None => Some(new_node)
            case Some(r_node) => {
              if (height(r_node.right) >= height(r_node.left))
                rotateLeft(Some(new_node))
              else {
                val rotated = Node(new_node.value, new_node.left, rotateRight(new_node.right))
                rotateLeft(Some(rotated))
              }
            }
          }
        }
        else Some(new_node)
      }
  }

  /**
    * Returns true if object is in the tree, otherwise false
    * @param value object to find in the tree
    * @param ordering ordering implementation of type T
    * @return true if object in the tree, otherwise false
    */
  def find(value: T)(implicit ordering: Ordering[T]): Boolean = {
    find(value, root)
  }

  @tailrec
  private def find(value: T, node: Option[Node[T]])(implicit ordering: Ordering[T]): Boolean = node match {
    case None => false
    case Some(node) => {
      val Node(node_val, left, right) = node
      ordering.compare(value, node_val) match {
        case 1 => find(value, right)
        case 0 => true
        case -1 => find(value, left)
      }
    }
  }

  /**
    * Adds new object to the tree if it is not already in the tree
    * @param value object to add
    * @param ordering ordering implementation of type T
    * @return new AVL tree containing new object
    */
  def add(value: T)(implicit ordering: Ordering[T]): AVL[T] = {
    val new_root = add(value, root)
    val tree: AVL[T] = new AVL[T](Some(new_root))
    tree.rebalance()
  }

  private def add(value: T, node: Option[Node[T]])(implicit ordering: Ordering[T]): Node[T] = node match {
    case Some(node) => {
      val Node(node_val, left, right) = node
      ordering.compare(value, node_val) match {
        case 1 => Node(node_val, left, Some(add(value, right)))
        case 0 => node
        case -1 => Node(node_val, Some(add(value, left)), right)
      }
    }
    case None => Node(value, None, None)
  }


  /**
    * Removes given object from the tree if found
    * @param value object to remove
    * @param ordering ordering implementation of type T
    * @return new AVL tree with removed object
    */
  def remove(value: T)(implicit ordering: Ordering[T]): AVL[T] = {
    val tree = new AVL[T](remove(value, root))
    tree.rebalance()
  }

  private def remove(value: T, node: Option[Node[T]])(implicit ordering: Ordering[T]): Option[Node[T]] = node match {
    case None => None
    case Some(node) => {
      val Node(node_val, left, right) = node
      ordering.compare(value, node_val) match {
        case 1 => Some(Node(node_val, left, remove(value, right)))
        case 0 => {
          node match {
            case Node(_, None, None) => None
            case Node(_, None, Some(_)) => right
            case Node(_, Some(_), None) => left
            case Node(_, Some(_), Some(_)) =>{
              val min_right_val = min(right)
              val temp = remove(min_right_val, right)
              Some(Node(min_right_val, left, temp))
            }
          }
        }
        case -1 => Some(Node(node_val, remove(value, left), right))
      }
    }
  }

  private def mergeNodes(left: Option[Node[T]], right: Option[Node[T]]): Option[Node[T]] = right match {
    case None => sys.error("This should never happen")
    case Some(right) => {
      val Node(node_val, rl_tree, rr_tree) = right
      if(right.left.isEmpty) Some(Node(node_val, left, rr_tree))
      else Some(Node(node_val, mergeNodes(left, rl_tree), rr_tree))
    }
  }

  /**
    * Returns minimum object from the tree
    * @param ordering ordering implementation of type T
    * @return Object of type T that is minimal from all objects in the tree
    */
  def min()(implicit ordering: Ordering[T]): T = {
    min(root)
  }

  @tailrec
  private def min(node: Option[Node[T]]): T = node match {
    case None => sys.error("Min on empty tree!")
    case Some(node) => {
      node match {
        case Node(node_val, None, _) => node_val
        case Node(_, left, _) => min(left)
      }
    }
  }

  /**
    * Returns maximum object from the tree
    * @param ordering ordering implementation of type T
    * @return Object of type T that is maximal from all objects in the tree
    */
  def max()(implicit ordering: Ordering[T]): T = {
    max(root)
  }

  @tailrec
  private def max(node: Option[Node[T]]): T = node match {
    case None => sys.error("Max on empty tree!")
    case Some(node) => {
      node match {
        case Node(node_val, _, None) => node_val
        case Node(_, _, right) => max(right)
      }
    }
  }

  private def rotateLeft(node: Option[Node[T]]): Option[Node[T]] = node match{
    case None => sys.error("Trying to rotate empty tree!")
    case Some(node) => {
      val Node(root_node_val, l_tree, r_tree) = node
      val Node(right_node_val, lr_tree, rr_tree) = r_tree match {
        case None => sys.error("R tree is empty!")
        case Some(node) => node
      }
      val new_root = Node(
        right_node_val,
        Some(Node(root_node_val, l_tree, lr_tree)),
        rr_tree)
      Some(new_root)
    }
  }

  private def rotateRight(node: Option[Node[T]]): Option[Node[T]] = node match {
    case None => sys.error("Trying to rotate empty tree!")
    case Some(node) => {
      val Node(root_node_val, l_tree, r_tree) = node
      val Node(left_node_val, ll_tree, rl_tree) = l_tree match {
        case None => sys.error("L tree is empty!")
        case Some(node) => node
      }
      val new_root = Node(
        left_node_val,
        ll_tree,
        Some(Node(root_node_val, rl_tree, r_tree)))
      Some(new_root)
    }
  }
}