package cluster.hierarchical

import cluster.Cluster


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

