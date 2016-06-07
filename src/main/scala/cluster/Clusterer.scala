package cluster


/**
  * Clusterer.
  *
  * @tparam T - Type of elements.
  */
trait Clusterer[T] {

  /**
    * Performs clustering.
    *
    * @return Clustering.
    */
  def cluster: Clustering[T]

}

