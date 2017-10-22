object mt2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  // problem 1
  def zip[S, T](list1: List[S], list2: List[T]): List[(S, T)] = {
     if (list1.size != list2.size) throw new Exception("Lists must have same sizes")
     if (list1 == Nil) Nil
     else {
        val pairs = zip(list1.tail, list2.tail)
        (list1.head, list2.head):: pairs
     }
  }                                               //> zip: [S, T](list1: List[S], list2: List[T])List[(S, T)]
  
  zip(List(1, 2, 3), List("one", "two", "three")) //> res0: List[(Int, String)] = List((1,one), (2,two), (3,three))
  
  zip(List("pi", "e"), List(3.14, 2.71))          //> res1: List[(String, Double)] = List((pi,3.14), (e,2.71))
  
  def unzip[S, T](pairs: List[(S, T)]): (List[S], List[T]) = {
     if (pairs == Nil) (Nil, Nil)
     else {
        val pairsTail =unzip(pairs.tail)
        (pairs.head._1::pairsTail._1, pairs.head._2::pairsTail._2)
     }
  }                                               //> unzip: [S, T](pairs: List[(S, T)])(List[S], List[T])
  
  unzip(zip(List(1, 2, 3), List("one", "two", "three")))
                                                  //> res2: (List[Int], List[String]) = (List(1, 2, 3),List(one, two, three))
  
  
  // problem 2
  def pipe[T, S](f: T=>S, g: T=>S): T=>S = {
     def h(t: T): S =
        try {
           f(t)
        } catch {
           case e: Exception => g(t)
        }
     h _
  }                                               //> pipe: [T, S](f: T => S, g: T => S)T => S
  
  val toInteger = pipe((s: String) => s.toInt, (s: String)=>0)
                                                  //> toInteger  : String => Int = <function1>
  
  toInteger("12345")                              //> res3: Int = 12345
  toInteger("123x45")                             //> res4: Int = 0
  
  
  // problem 3
  def isPal(s: String) = s == s.reverse           //> isPal: (s: String)Boolean
  def length(s: String) = s.size                  //> length: (s: String)Int
  def max(n: Int, m: Int) = if(n < m) m else n    //> max: (n: Int, m: Int)Int
  
  // map-reduce solution
  def maxPal1(words: List[String]): Int =
     words.filter(isPal _).map(length _).reduce(max _)
                                                  //> maxPal1: (words: List[String])Int
     
  maxPal1(List("mom", "rotator", "cowbells", "dad"))
                                                  //> res5: Int = 7
  // tail recursive solution
  def maxPal2(words: List[String]): Int = {
     def helper(result: Int, unseen: List[String]): Int =
        if (unseen == Nil) result
        else helper(if (isPal(unseen.head)) max(length(unseen.head), result) else result, unseen.tail)
     helper(0, words)
  }                                               //> maxPal2: (words: List[String])Int
  
   maxPal2(List("mom", "rotator", "cowbells", "dad"))
                                                  //> res6: Int = 7
  
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
}

var exp: Expression = Sum(Number(42), Sum(Number(3.14), Number(2.71)))
                                                  //> exp  : mt2.Expression = mt2$$anonfun$main$1$Sum$2@643b1d11
   println("value = " + exp.execute)              //> value = 47.85
   exp = Sum(Number(2), Sum(Number(3), Number(5)))
   println("value = " + exp.execute)              //> value = 10.0
   exp = And(Boole(true), And(Boole(false), Boole(true)))
   println("value = " + exp.execute)              //> value = false
  
}