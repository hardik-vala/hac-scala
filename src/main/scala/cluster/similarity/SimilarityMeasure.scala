package cluster.similarity


/**
  * Similarity measure (must be symmetric).
  *
  * @tparam T - Type of elements.
  */
trait SimilarityMeasure[T] {

  /**
    * Calculates the similarity between the two elements.
    *
    * @param o1 - First element.
    * @param o2 - Second element.
    * @tparam U - Type of elements.
    * @return Similarity score.
    */
  def calculate[U <: T](o1: U, o2: U): Double

}

