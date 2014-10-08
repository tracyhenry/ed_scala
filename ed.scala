import scala.io.Source
import util.Random

object HelloWorld
{
	def main(args : Array[String])
	{
		
		//println(thresholdLevenshtein("abbabba", "bbaaaabaa", 100))		
		//println(thresholdLCS("abbabba", "bbaaaabaa", 100))
		var n = 0
		var linesIterator = Source.fromFile("querylog").getLines
		var lines = new Array[String](2000000)
		while (linesIterator.hasNext)
		{
			lines(n) = linesIterator.next()
			n += 1
		}
		
		val numberPair = 200000
		val randomDis = 50
		var s1 = new Array[Int](numberPair)
		var s2 = new Array[Int](numberPair)
		for (i <- 0 until numberPair)
		{
			val x = i
			val y = x + math.abs(Random.nextInt) % randomDis + 1
			s1(i) = x
			s2(i) = y
		}

		println("Start running thresholdLevenshtein...")

		//run
		var st = System.currentTimeMillis
		var ans1 = 0
		for (i <- 0 until numberPair)
			ans1 = thresholdLevenshtein(lines(s1(i)), lines(s2(i)), 5)

		var en = System.currentTimeMillis
		println("thresholdLevenshtein finished! Total running time : " + ((en - st) / 1000.0).toString + "s.")

		st = System.currentTimeMillis
		var ans2 = 0
		for (i <- 0 until numberPair)
			ans2 = thresholdLCS(lines(s1(i)), lines(s2(i)), 5)
		en = System.currentTimeMillis
		println("thresholdLCS finished! Total running time : " + ((en - st) / 1000.0).toString + "s.")
			

	}

	def thresholdLevenshtein(_s: String, _t: String, threshold: Int): Int = 
	{
		val (s, t) = if (_s.length > _t.length) (_s, _t) else (_t, _s)
		val slen = s.length
		val tlen = t.length
 
		var prev = Array.fill[Int](tlen+1)(Int.MaxValue)
		var curr = Array.fill[Int](tlen+1)(Int.MaxValue)
		for (n <- 0 until math.min(tlen+1, threshold+1)) prev(n) = n
 
		for (row <- 1 until (slen+1)) 
		{
			curr(0) = row
		   	val min = math.min(tlen+1, math.max(1, row - threshold))
			val max = math.min(tlen+1, row + threshold + 1)
			// println("row[" + row + "] min=" + min + " max=" + max)
 
			if (min > 1) curr(min-1) = Int.MaxValue
			for (col <- min until max) {
				curr(col) = if (s(row-1) == t(col-1)) prev(col-1)
                			    else math.min(prev(col-1), math.min(curr(col-1), prev(col))) + 1
			}     		
			// println("row[" + row + "]: " + curr.mkString(" "))
			prev = curr
			curr = Array.fill[Int](tlen+1)(Int.MaxValue)
		
		}
 
		if (prev(tlen) <= threshold) return prev(tlen)
		return threshold + 1
	}

	def thresholdLCS(_s : String, _t : String, threshold : Int): Int = 
	{
		val (s, t) = if (_s.length > _t.length) (_s, _t) else (_t, _s)
		val n = s.length
		val m = t.length

		if (n - m > threshold) return threshold + 1

		var V = new Array[Array[Int]](threshold * 3 + 2)
		for (i <- 0 until V.length)
			V(i) = Array.fill[Int](2)(Int.MinValue)

		V(-1 + threshold + 1)(-1 & 1) = 0
		
		for (p <- 0 until threshold + 1)
		{
			val f = p & 1
			val g = f ^ 1

			for (k <- threshold + 1 - p until threshold + 1 + p + 1)
			{
				V(k)(f) = math.max(math.max(V(k)(g), V(k + 1)(g)) + 1, V(k - 1)(g))
				val d = k - threshold - 1;
				if (V(k)(f) >= 0 && V(k)(f) + d >= 0)
					while (V(k)(f) < n && V(k)(f) + d < m && s(V(k)(f)) == t(V(k)(f) + d))
						V(k)(f) += 1
			}
			if (V(m - n + threshold + 1)(f) >= n) return p			
		}
		return threshold + 1

	}	































}
