object mt2 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(56); 
  println("Welcome to the Scala worksheet");$skip(309); 
  
  // problem 1
  def zip[S, T](list1: List[S], list2: List[T]): List[(S, T)] = {
     if (list1.size != list2.size) throw new Exception("Lists must have same sizes")
     if (list1 == Nil) Nil
     else {
        val pairs = zip(list1.tail, list2.tail)
        (list1.head, list2.head):: pairs
     }
  };System.out.println("""zip: [S, T](list1: List[S], list2: List[T])List[(S, T)]""");$skip(54); val res$0 = 
  
  zip(List(1, 2, 3), List("one", "two", "three"));System.out.println("""res0: List[(Int, String)] = """ + $show(res$0));$skip(45); val res$1 = 
  
  zip(List("pi", "e"), List(3.14, 2.71));System.out.println("""res1: List[(String, Double)] = """ + $show(res$1));$skip(232); 
  
  def unzip[S, T](pairs: List[(S, T)]): (List[S], List[T]) = {
     if (pairs == Nil) (Nil, Nil)
     else {
        val pairsTail =unzip(pairs.tail)
        (pairs.head._1::pairsTail._1, pairs.head._2::pairsTail._2)
     }
  };System.out.println("""unzip: [S, T](pairs: List[(S, T)])(List[S], List[T])""");$skip(61); val res$2 = 
  
  unzip(zip(List(1, 2, 3), List("one", "two", "three")));System.out.println("""res2: (List[Int], List[String]) = """ + $show(res$2));$skip(198); 
  
  
  // problem 2
  def pipe[T, S](f: T=>S, g: T=>S): T=>S = {
     def h(t: T): S =
        try {
           f(t)
        } catch {
           case e: Exception => g(t)
        }
     h _
  };System.out.println("""pipe: [T, S](f: T => S, g: T => S)T => S""");$skip(67); 
  
  val toInteger = pipe((s: String) => s.toInt, (s: String)=>0);System.out.println("""toInteger  : String => Int = """ + $show(toInteger ));$skip(25); val res$3 = 
  
  toInteger("12345");System.out.println("""res3: Int = """ + $show(res$3));$skip(22); val res$4 = 
  toInteger("123x45");System.out.println("""res4: Int = """ + $show(res$4));$skip(63); 
  
  
  // problem 3
  def isPal(s: String) = s == s.reverse;System.out.println("""isPal: (s: String)Boolean""");$skip(33); 
  def length(s: String) = s.size;System.out.println("""length: (s: String)Int""");$skip(47); 
  def max(n: Int, m: Int) = if(n < m) m else n;System.out.println("""max: (n: Int, m: Int)Int""");$skip(126); 
  
  // map-reduce solution
  def maxPal1(words: List[String]): Int =
     words.filter(isPal _).map(length _).reduce(max _);System.out.println("""maxPal1: (words: List[String])Int""");$skip(60); val res$5 = 
     
  maxPal1(List("mom", "rotator", "cowbells", "dad"));System.out.println("""res5: Int = """ + $show(res$5));$skip(294); 
  // tail recursive solution
  def maxPal2(words: List[String]): Int = {
     def helper(result: Int, unseen: List[String]): Int =
        if (unseen == Nil) result
        else helper(if (isPal(unseen.head)) max(length(unseen.head), result) else result, unseen.tail)
     helper(0, words)
  };System.out.println("""maxPal2: (words: List[String])Int""");$skip(58); val res$6 = 
  
   maxPal2(List("mom", "rotator", "cowbells", "dad"))
  
  // problem 4
trait Value

trait Expression {
  def execute: Value
}

trait Literal extends Value with Expression {
   def execute = this
}

class Number (val value: Double) extends Literal {
 override def toString = value.toString
 }
object Number {
   def apply(value: Double) = new Number(value)
}

class Boole (val value: Boolean) extends Literal {
 override def toString = value.toString
 }
object Boole {
   def apply(value: Boolean) = new Boole(value)

}

class Sum(val operand1: Expression, val operand2: Expression) extends Expression {
   def execute =  {
     val arg1 = operand1.execute
     val arg2 = operand2.execute
     if (!arg1.isInstanceOf[Number] || !arg2.isInstanceOf[Number]) {
       throw new Exception("sum inputs must be numbers")
     }
     val num1 = arg1.asInstanceOf[Number]
     val num2 = arg2.asInstanceOf[Number]
     new Number(num1.value + num2.value)
   }
}


// and a companion object
object Sum {
   def apply(operand1: Expression, operand2: Expression) = new Sum(operand1, operand2)
}

class And(val operand1: Expression, val operand2: Expression) extends Expression {
   def execute =  {
     val arg1 = operand1.execute
     val arg2 = operand2.execute
     if (!arg1.isInstanceOf[Boole] || !arg2.isInstanceOf[Boole]) {
       throw new Exception("sum inputs must be numbers")
     }
     val num1 = arg1.asInstanceOf[Boole]
     val num2 = arg2.asInstanceOf[Boole]
     new Boole(num1.value && num2.value)
   }
}

// and a companion object
object And {
   def apply(operand1: Expression, operand2: Expression) = new And(operand1, operand2)
};System.out.println("""res6: Int = """ + $show(res$6));$skip(1675); 

var exp: Expression = Sum(Number(42), Sum(Number(3.14), Number(2.71)));System.out.println("""exp  : mt2.Expression = """ + $show(exp ));$skip(37); 
   println("value = " + exp.execute);$skip(51); 
   exp = Sum(Number(2), Sum(Number(3), Number(5)));$skip(37); 
   println("value = " + exp.execute);$skip(58); 
   exp = And(Boole(true), And(Boole(false), Boole(true)));$skip(37); 
   println("value = " + exp.execute)}
  
}
