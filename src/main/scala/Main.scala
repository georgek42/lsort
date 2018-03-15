object Main extends App {
    println(sort(List(-12,213,3242,454,265,23464,-11111)))

    // O(N) time sort
    def sort(l: List[Int]): List[Int] = {
        var low: Option[Int] = None
        var high: Option[Int] = None
        l.foreach(a => {
            (((low.isDefined && a < low.get) || low.isEmpty), ((high.isDefined && a > high.get) || high.isEmpty)) match {
                case (true, false) => low = Some(a)
                case (false, true) => high = Some(a)
                case (true, true) => low = Some(a)
                case _ =>
            }
        })
        val range = high.get - low.get
        var buf = Array.fill[Option[Int]](range)(None)
        l.foreach(a => {
            buf((a + Math.abs(low.get)) % range) = Some(a)
        })
        buf.filter { case Some(b) => true case _ => false }.map { b => b.get }.toList
    }
}