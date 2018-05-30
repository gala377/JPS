package src

import scala.annotation.tailrec
import scala.math.Ordering

case class Node[T](value: T, left: Option[Node[T]], right: Option[Node[T]]) {
  def this(value: T) = this(value, None, None)
}

class AVL[T](root: Option[Node[T]]) {
  def this() = this(None)

  def getRoot(): Option[Node[T]] = {
    root
  }

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
                // Changed here
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
                // Changed here
                val rotated = Node(new_node.value, new_node.left, rotateRight(new_node.right))
                rotateLeft(Some(rotated))
              }
            }
          }
        }
        else Some(new_node)
      }
  }

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
        case 0 => root match {
            case None => Node(value, None, None)
            case Some(node) => node
          }
        case -1 => Node(node_val, Some(add(value, left)), right)
      }
    }
    case None => Node(value, None, None)
  }

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