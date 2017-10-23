import jdk.internal.org.objectweb.asm.commons.TryCatchBlockSorter

object mid_self {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(128); 
  println("Welcome to the Scala worksheet");$skip(469); 
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
  };System.out.println("""zip1: [S, T](list1: List[S], list2: List[T])List[(S, T)]""");$skip(42); val res$0 = 
  zip1(List("pi", "e"), List(3.14, 2.71));System.out.println("""res0: List[(String, Double)] = """ + $show(res$0));$skip(251); 
  
  def zip2[S,T](list1:List[S], list2:List[T]):List[(S,T)]={
  	if (list1.length!=list2.length) throw new Exception("Lists must have same sizes")
  	if (list1==Nil||list2==Nil) Nil
  	else (list1.head, list2.head) :: zip2(list1.tail,list2.tail)
  };System.out.println("""zip2: [S, T](list1: List[S], list2: List[T])List[(S, T)]""");$skip(42); val res$1 = 
  zip2(List("pi", "e"), List(3.14, 2.71));System.out.println("""res1: List[(String, Double)] = """ + $show(res$1));$skip(406); 
  
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
  };System.out.println("""zip3: [S, T](list1: List[S], list2: List[T])List[(S, T)]""");$skip(50); 
  var s = zip3(List("pi", "e"), List(3.14, 2.71));System.out.println("""s  : List[(String, Double)] = """ + $show(s ));$skip(4); val res$2 = 
  s;System.out.println("""res2: List[(String, Double)] = """ + $show(res$2));$skip(101); 
  
  def unzip[S,T] (list:List[(S,T)]):(List[S],List[T]) = {
  	(list.map(_._1), list.map(_._2))
  };System.out.println("""unzip: [S, T](list: List[(S, T)])(List[S], List[T])""");$skip(11); val res$3 = 
  unzip(s);System.out.println("""res3: (List[String], List[Double]) = """ + $show(res$3));$skip(153); 
  
  def pipe[S,T](f: T=>S, g: T=>S):T=>S = {
  	def h(x: T):S = {
  		try {
  			f(x)
  		} catch {
  			case e:Exception => g(x)
  		}
  	}
  	h _
  };System.out.println("""pipe: [S, T](f: T => S, g: T => S)T => S""");$skip(66); 
  
  val toInteger = pipe((s: String) => s.toInt, (s: String)=>0);System.out.println("""toInteger  : String => Int = """ + $show(toInteger ));$skip(19); val res$4 = 
  toInteger("345");System.out.println("""res4: Int = """ + $show(res$4));$skip(19); val res$5 = 
  toInteger("34c");System.out.println("""res5: Int = """ + $show(res$5));$skip(145); 
  
  def maxPal(words: List[String]): Int = {
  	words.map(x=>{if(x.equals(x.reverse)) x.length() else 0}).reduce((x,y)=>{if(x>y) x else y})
  };System.out.println("""maxPal: (words: List[String])Int""");$skip(55); val res$6 = 
  
  maxPal(List("mom", "rotator", "cowbells", "dad"));System.out.println("""res6: Int = """ + $show(res$6))}
                                                  
                                                  
}
