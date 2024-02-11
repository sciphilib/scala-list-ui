package data

import scala.reflect.ClassTag

class Node[T](var data: T, var prev: Option[Node[T]] = None, var next: Option[Node[T]] = None)

class MyList[T] {
  var head: Option[Node[T]] = None
  var tail: Option[Node[T]] = None
  var size: Int = 0

  def foreach(f: T => Unit): Unit = {
    var current = head
    while (current.isDefined) {
      f(current.get.data)
      current = current.get.next
    }
  }

  def clear(): Unit = {
    head = None
    tail = None
    size = 0
  }

  def add(data: T): Unit = {
    val newNode = new Node[T](data)

    tail match {
      case Some(currentTail) =>
        currentTail.next = Some(newNode)
        newNode.prev = Some(currentTail)
        tail = Some(newNode)
      case None =>
        head = Some(newNode)
        tail = Some(newNode)
    }

    size += 1
  }

  def addAt(index: Int, data: T): Unit = {
    if (index < 0 || index > size) throw new IndexOutOfBoundsException

    val newNode = new Node[T](data)
    var current = head
    var currentIndex = 0

    if (index == 0) {
      newNode.next = head
      head.foreach(_.prev = Some(newNode))
      head = Some(newNode)
      if (tail.isEmpty) tail = Some(newNode)
    } else {
      while (current.isDefined && currentIndex < index - 1) {
        current = current.get.next
        currentIndex += 1
      }

      newNode.next = current.get.next
      newNode.prev = current
      current.get.next.foreach(_.prev = Some(newNode))
      current.get.next = Some(newNode)

      if (newNode.next.isEmpty) tail = Some(newNode)
    }

    size += 1
  }

  def delete(data: T): Boolean = {
    var current = head

    while (current.isDefined) {
      if (current.get.data == data) {
        current.get.prev match {
          case Some(prevNode) => prevNode.next = current.get.next
          case None => head = current.get.next
        }

        current.get.next match {
          case Some(nextNode) => nextNode.prev = current.get.prev
          case None =>
        }

        size -= 1
        return true
      }
      current = current.get.next
    }

    false
  }

  def deleteAt(index: Int): Unit = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException

    if (index == 0) {
      head = head.get.next
      head.foreach(_.prev = None)
      if (head.isEmpty) tail = None
    } else {
      var current = head
      var currentIndex = 0

      while (current.isDefined && currentIndex < index - 1) {
        current = current.get.next
        currentIndex += 1
      }

      val toDelete = current.get.next
      current.get.next = toDelete.get.next
      toDelete.get.next.foreach(_.prev = current)

      if (current.get.next.isEmpty) tail = current
    }
    size-=1
  }

  def traverse(f: T => Unit): Unit = {
    var current = head
    while (current.isDefined) {
      f(current.get.data)
      current = current.get.next
    }
  }

  def toScalaList: List[T] = {
    var scalaList: List[T] = List()
    var current = head

    while (current.isDefined) {
      scalaList = scalaList :+ current.get.data
      current = current.get.next
      if (current == head) return scalaList
    }

    scalaList
  }

  def toScalaArray(implicit tag: ClassTag[T]): Array[T] = {
    val scalaArray = new Array[T](size)
    var current = head
    var index = 0

    while (current.isDefined) {
      scalaArray(index) = current.get.data
      index += 1
      current = current.get.next
    }

    scalaArray
  }
  
}

object MyList {
  def fromScalaList[T](scalaList: List[T]): MyList[T] = {
    val myList = new MyList[T]
    scalaList.foreach(myList.add)
    myList
  }

  def fromScalaArray[T](scalaArray: Array[T]): MyList[T] = {
    val myList = new MyList[T]
    for (item <- scalaArray) {
      myList.add(item)
    }
    myList
  }

  def mergeSortMyList[T](myList: MyList[T])(implicit ord: Ordering[T]): MyList[T] = {
    val scalaList = myList.toScalaList
    val sortedScalaList = mergeSort(scalaList)
    MyList.fromScalaList(sortedScalaList)
  }

  def mergeSortMyListImp[T](myList: MyList[T])(implicit tag: ClassTag[T], ord: Ordering[T]): MyList[T] = {
    val scalaArray = myList.toScalaArray
    val sortedScalaArray = mergeSortImperative(scalaArray)
    MyList.fromScalaArray(sortedScalaArray)
  }

  private def mergeSort[T](list: List[T])(implicit ord: Ordering[T]): List[T] = {
    def merge(left: List[T], right: List[T]): List[T] = (left, right) match {
      case (Nil, _) => right
      case (_, Nil) => left
      case (leftHead :: leftTail, rightHead :: rightTail) =>
        if (ord.lteq(leftHead, rightHead))
          leftHead :: merge(leftTail, right)
        else
          rightHead :: merge(left, rightTail)
    }

    val n = list.length / 2
    if (n == 0) list
    else {
      val (left, right) = list.splitAt(n)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def mergeSortImperative[T](arr: Array[T])(implicit tag: ClassTag[T], ord: Ordering[T]): Array[T] = {
    def merge(left: Array[T], right: Array[T]): Array[T] = {
      val result = Array.ofDim[T](left.length + right.length)
      var (i, j, k) = (0, 0, 0)

      while (i < left.length && j < right.length) {
        if (ord.lteq(left(i), right(j))) {
          result(k) = left(i)
          i += 1
        } else {
          result(k) = right(j)
          j += 1
        }
        k += 1
      }

      while (i < left.length) {
        result(k) = left(i)
        i += 1
        k += 1
      }

      while (j < right.length) {
        result(k) = right(j)
        j += 1
        k += 1
      }

      result
    }

    def sort(arr: Array[T], l: Int, r: Int): Array[T] = {
      if (l < r) {
        val m = l + (r - l) / 2
        val left = sort(arr.slice(l, m + 1), 0, m)
        val right = sort(arr.slice(m + 1, r + 1), 0, m)
        merge(left, right)
      } else {
        arr.slice(l, r + 1)
      }
    }

    sort(arr, 0, arr.length - 1)
  }

}
