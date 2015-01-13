package functionalset

abstract class FunctionalSet[T] extends Function1[T, Boolean] {
  
  def apply(x : T) : Boolean
  
  def elems : Seq[T]
  
  def isComplement : Boolean = false
  
  def \/(that : FunctionalSet[T]) = {
    val tempElems = this.elems
    val containFun = this.contains(_)
    new FunctionalSet[T] {
    def elems = {
      tempElems ++ that.elems
    }
    def apply(x : T) = 
      (containFun(x) || that(x))
  }
 }
    
  
  def /\(that : FunctionalSet[T]) = {
    val tempElems = this.elems.filter { x => that(x) }
    if (tempElems.isEmpty == false) {
      val containFun = this.contains(_)
      new FunctionalSet[T] {
        def elems = {
          tempElems.filter(x => that.elems.contains(x))
        }
        def apply(y : T) = 
          containFun(y) && that(y)
      } 
    } else {
      FunctionalSet.emptySet[T]
    }
    
  }
  
  def unary_~ = {
    val temp = elems
    val containsFun = this.contains(_)
    new FunctionalSet[T] {
      override def isComplement = true
      def elems = temp
      def apply(x : T) = 
      !containsFun(x)
    }
  }
  
  override def toString = {
    "{" + elems.mkString(" , ") + "}" + (if (isComplement) "'" else "")
  }
    
  override def equals(that : Any) = {
    that.isInstanceOf[FunctionalSet[T]] && 
    that.asInstanceOf[FunctionalSet[T]].elems == this.elems &&
    that.asInstanceOf[FunctionalSet[T]].isComplement == this.isComplement
  }
  
  def contains(y : T) = 
    this(y)
  
}

object FunctionalSet {
  
  def singleton[T](x : T) = new FunctionalSet[T] {
    
    def apply(y : T) = 
      x.hashCode() == y.hashCode()
    
    def elems = List(x)
    
  }
  
  def emptySet[T] : FunctionalSet[T] = new FunctionalSet[T] {
    
    def apply(x : T) = false
    
    def elems = List()
    
  }
  
  implicit def toSequence[T](x : T) = List(x)
  
  def apply[T](xs : T*) = 
    xs.map(x => singleton(x)).foldLeft(emptySet[T])(_ \/ _)
  
//  def apply[T](xs : Seq[T]) = 
//    xs.map(x => singleton(x)).foldLeft(emptySet[T])(_ \/ _)
    
    
  def runExample = {
    
    val s1 = FunctionalSet(3)
    println(s1)
    val s2 = FunctionalSet(6, 9)
    println(s2)
    val s3 = s1 \/ s2
    println(s3)
    val s4 = s1 /\ s2
    println(s4)
    val s5 = ~s3
    println(s5)
    println(s3 == FunctionalSet(3, 6, 9))
    println(s3(9))
    println(s3(2))
    println(s5(2))
    println(s5(6))
    val s6 = FunctionalSet(2,4,10)
    println(s3 /\ s6)
    println(s6 /\ s5)
    
    
  }
  
}