package isbn

object ISBNValidator {
  
  private def toAppropriateInt(ch : Char) = ch match {
    case 'X' => 10
    case _ => ch - '0'
  }
  
  private def zipWith[T, U, V](f : (T, U) => V)(xs : Seq[T], ys : Seq[U]) = 
    xs.zip(ys).map {case (x, y) => f(x, y)}
  
  def isISBNValid(str : Seq[Char]) = 
    zipWith((x : Int, y : Int) => x * y)(
        str.filterNot(x => x == '-').map(toAppropriateInt), 
        (str.length to 1)).sum % 11 == 0

}