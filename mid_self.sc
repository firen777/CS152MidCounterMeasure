import jdk.internal.org.objectweb.asm.commons.TryCatchBlockSorter

object mid_self {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  def zip1[S,T](list1:List[S], list2:List[T]):List[(S,T)]={
  	if (list1.length!=list2.length) throw new Exception("Lists must have same sizes")
  	var ptr1 = list1
  	var ptr2 = list2
  	var result = Nil:List[(S,T)]
  	if (ptr1!=Nil && ptr2!=Nil)
  		result = List((ptr1.head, ptr2.head))
  	while(ptr1!=Nil&&ptr2!=Nil){
  		ptr1 = ptr1.tail
  		ptr2 = ptr2.tail
  		if (ptr1!=Nil && ptr2!=Nil)
  			result = result ::: List((ptr1.head, ptr2.head))
  	}
  	result
  }                                               //> zip1: [S, T](list1: List[S], list2: List[T])List[(S, T)]
  zip1(List("pi", "e"), List(3.14, 2.71))         //> res0: List[(String, Double)] = List((pi,3.14), (e,2.71))
  
  def zip2[S,T](list1:List[S], list2:List[T]):List[(S,T)]={
  	if (list1.length!=list2.length) throw new Exception("Lists must have same sizes")
  	if (list1==Nil||list2==Nil) Nil
  	else (list1.head, list2.head) :: zip2(list1.tail,list2.tail)
  }                                               //> zip2: [S, T](list1: List[S], list2: List[T])List[(S, T)]
  zip2(List("pi", "e"), List(3.14, 2.71))         //> res1: List[(String, Double)] = List((pi,3.14), (e,2.71))
  
  def zip3[S,T](list1:List[S], list2:List[T]):List[(S,T)]={
  	if (list1.length!=list2.length) throw new Exception("Lists must have same sizes")
  	def helper[S,T](curr:List[(S,T)], l1:List[S], l2:List[T]):List[(S,T)]={
  		if (l1==Nil || l2==Nil) curr
  		else {
  			val newCurr = curr ::: List((l1.head, l2.head))
  			helper(newCurr, l1.tail, l2.tail)
  		}
  	}
  	helper(List(), list1, list2)
  }                                               //> zip3: [S, T](list1: List[S], list2: List[T])List[(S, T)]
  var s = zip3(List("pi", "e"), List(3.14, 2.71)) //> s  : List[(String, Double)] = List((pi,3.14), (e,2.71))
  s                                               //> res2: List[(String, Double)] = List((pi,3.14), (e,2.71))
  
  def unzip[S,T] (list:List[(S,T)]):(List[S],List[T]) = {
  	(list.map(_._1), list.map(_._2))
  }                                               //> unzip: [S, T](list: List[(S, T)])(List[S], List[T])
  unzip(s)                                        //> res3: (List[String], List[Double]) = (List(pi, e),List(3.14, 2.71))
  
  def pipe[S,T](f: T=>S, g: T=>S):T=>S = {
  	def h(x: T):S = {
  		try {
  			f(x)
  		} catch {
  			case e:Exception => g(x)
  		}
  	}
  	h _
  }                                               //> pipe: [S, T](f: T => S, g: T => S)T => S
  
  val toInteger = pipe((s: String) => s.toInt, (s: String)=>0)
                                                  //> toInteger  : String => Int = <function1>
  toInteger("345")                                //> res4: Int = 345
  toInteger("34c")                                //> res5: Int = 0
  
  def maxPal(words: List[String]): Int = {
  	words.map(x=>{if(x.equals(x.reverse)) x.length() else 0}).reduce((x,y)=>{if(x>y) x else y})
  }                                               //> maxPal: (words: List[String])Int
  
  maxPal(List("mom", "rotator", "cowbells", "dad"))
                                                  //> res6: Int = 7
                                                  
                                                  
}