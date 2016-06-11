package cluster.hierarchical

import cluster.similarity.SimilarityMeasure
import cluster.{Cluster, Clusterer}


/**
  * Hierarchical clusterer.
  *
  * @param elements - Elements to cluster.
  * @param similarityMeasure - Similarity measure.
  * @param linkageCriterion - Linkage criterion.
  * @tparam T - Type of elements.
  */
abstract class HierarchicalClusterer[T](elements: Iterable[T],
                                        similarityMeasure: SimilarityMeasure[T],
                                        linkageCriterion: Linkage) extends Clusterer[T] {

  /** Similarity matrix for elements. */
  lazy val similarityMatrix: Map[(T, T), Double] = this.elements.toList.combinations(2).flatMap(comb => {
    val sim: Double = this.similarityMeasure.calculate(comb.head, comb.last)
    Seq((comb.head, comb.last) -> sim, (comb.last, comb.head) -> sim)
  }).toMap

  /**
    * Calculate the similarity between two clusters.
    *
    * @param c1 - First cluster.
    * @param c2 - Second cluster.
    * @return Similarity score.
    */
  def calcSimilarity(c1: Cluster[T], c2: Cluster[T]): Double = this.linkageCriterion match {
    case SINGLE_LINKAGE => c1.elements.map(e1 => c2.elements.map(this.similarityMatrix.get(e1, _).get).max).max
    case COMPLETE_LINKAGE => c1.elements.map(e1 => c2.elements.map(this.similarityMatrix.get(e1, _).get).min).min
    case AVERAGE_LINKAGE =>
      c1.elements.map(e1 => c2.elements.map(this.similarityMatrix.get(e1, _).get).sum).sum / (c1.size * c2.size)
  }

}

