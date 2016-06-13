package cluster.hierarchical

import cluster.{Cluster, Clustering}

import scala.collection.mutable.ArrayBuffer


/**
  * Hierarchical cluster level.
  *
  * @param clusters - Clusters in level represented as a sequence.
  * @param level - Level score.
  * @tparam T - Type of cluster elements.
  */
case class Level[T](clusters: IndexedSeq[Cluster[T]], level: Double) {

  override def toString: String = this.level.toString + "\t" +
    this.clusters.map("Cluster(" + _.elements.mkString(", ") + ")").mkString("\n\t")

}

/**
  * Stores a hierarchical agglomerative clustering of elements.
  *
  * @param elements - Cluster elements.
  * @tparam T - Type of elements.
  */
class HierarchicalAgglomerativeClustering[T](elements: Iterable[T]) extends Clustering[T] {

  // Sequence of clustering levels, ordered from initial to most recent level. The clusters at each level are Id'd by
  // their index in the level sequence.
  private var levels: ArrayBuffer[Level[T]] = {
    // Initial level, with level score 0.0.
    val initLevel = Level(this.elements.zipWithIndex.map({ case (e, i) => Cluster(i, Iterable(e)) }).toIndexedSeq, 0.0)
    ArrayBuffer(initLevel)
  }

  /**
    * Return the last/most recent level.
    *
    * @return Last/Most recent level.
    */
  def lastLevel: Level[T] = this.levels.last

  /**
    * Constructs the next level in the clustering procedure by merging two clusters at the most recent level.
    *
    * @param id1 - Id of first cluster to merge.
    * @param id2 - Id of second cluster to merge.
    * @param level - Score for new level.
    */
  def toNewLevel(id1: Int , id2: Int, level: Double): Unit = {
    // First cluster to merge.
    val c1: Cluster[T] = this.lastLevel.clusters(id1)
    // Second cluster to merge.
    val c2: Cluster[T] = this.lastLevel.clusters(id2)
    // All the other clusters.
    val otherClusters: IndexedSeq[Cluster[T]] = this.lastLevel.clusters.filter(c => c.id != id1 && c.id != id2)
    // Merged clusters.
    val mergedCluster = Cluster(otherClusters.size, c1.elements ++ c2.elements)

    // The clusters in the new level are re-indexed, with the merged cluster appended to the end.
    this.levels += Level(otherClusters.zipWithIndex.map({ case (c, i) => Cluster(i, c.elements) }) :+ mergedCluster,
      level)
  }

  override def toString: String = this.levels.reverse.map(_.toString).mkString("\n")

}

