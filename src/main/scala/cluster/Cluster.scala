package cluster

/**
  * Cluster.
  *
  * @param id - Simple integer id for the cluster (for tracking purposes during clustering).
  * @param elements - Cluster elements.
  * @tparam T - Type of elements.
  */
case class Cluster[+T](id: Int, elements: Iterable[T]) {

  /**
    * Cluster size.
    *
    * @return # of cluster elements.
    */
  def size: Int = this.elements.size

}

