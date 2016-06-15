package cluster.hierarchical

import cluster.Cluster
import cluster.similarity.SimilarityMeasure


/**
  * Hierarchical agglomerative clusterer.
  *
  * @param elements - Elements to cluster.
  * @param similarityMeasure - Similarity measure.
  * @param linkageCriterion - Linkage criterion.
  * @tparam T - Type of elements.
  */
class HierarchicalAgglomerativeClusterer[T](elements: Iterable[T],
                                            similarityMeasure: SimilarityMeasure[T],
                                            linkageCriterion: Linkage)
  extends HierarchicalClusterer[T](elements, similarityMeasure, linkageCriterion) {

  /**
    * Perform clustering.
    *
    * @return Clustering.
    */
  override def cluster: HierarchicalAgglomerativeClustering[T] = {
    // Create a clustering with the initial level initialized with singleton clusters corresponding to the elements.
    val clustering: HierarchicalAgglomerativeClustering[T] = new HierarchicalAgglomerativeClustering[T](this.elements)

    // Perform n - 1 merges, where n is the # of elements.
    for (_ <- 0 until this.elements.size - 1) {
      // Pair of clusters in the current level with the highest similarity, plus the similarity score.
      val (cMax1: Cluster[T], cMax2: Cluster[T], score: Double) = clustering
        .lastLevel.clusters
        .combinations(2)
        .map(l => (l.head, l.last, this.calcSimilarity(l.head, l.last)))
        .maxBy(_._3)

      // Construct a new level in the clustering by merging the pair of clusters.
      clustering.toNewLevel(cMax1.id, cMax2.id, score)
    }

    clustering
  }

}

object HierarchicalAgglomerativeClusterer {

  def apply[T](elements: Iterable[T], similarityMeasure: SimilarityMeasure[T], linkageCriterion: Linkage) =
   new HierarchicalAgglomerativeClusterer(elements, similarityMeasure, linkageCriterion)

}

