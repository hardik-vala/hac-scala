package cluster.hierarchical

/**
  * Linkage criterion.
  */
sealed trait Linkage {

  /**
    * Linkage criterion as a string.
    *
    * @return Linkage criterion string.
    */
  def name: String

  override def toString: String = this.name

}

/** Single linkage. */
case object SINGLE_LINKAGE extends Linkage { override def name: String = "SINGLE" }

/** Complete linkage. */
case object COMPLETE_LINKAGE extends Linkage { override def name: String = "COMPLETE" }

/** Average linkage. */
case object AVERAGE_LINKAGE extends Linkage { override def name: String = "AVERAGE" }

