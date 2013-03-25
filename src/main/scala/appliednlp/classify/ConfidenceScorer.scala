package appliednlp.classify

/**
 * An application that takes a gold labeled file and a file containing
 * predictions, and then computes the accuracy for the top-third most
 * confident instances and the accuracy for the bottom-third (least 
 * confident instances).
 */
object ConfidenceScorer {

  def main(args: Array[String]) {
	// Extract gold standard labels
	val gs = io.Source.fromFile(args(0)).getLines.map{ line =>
				val example = line.split(",")
				example(example.size - 1)
			}
	
	// Extract predictions
	val predictions = io.Source.fromFile(args(1)).getLines.map{ line =>
				val predict = line.split(" ")
				(predict(1), predict(0))
			}
	
	val sortedConfidenceScores = predictions.zip(gs)
					.map(x => (x._1._1, x._1._2, x._2))
					.toArray.sortBy(_._1)
					.reverse

	val numExamples = sortedConfidenceScores.size
	val topThird = (numExamples.toDouble / 3).ceil.toInt
	val midThird = (2 * numExamples.toDouble / 3).ceil.toInt

	val highConfidenceAcc = 100 * sortedConfidenceScores
			.take(topThird)
			.count(x => x._2 == x._3).toDouble / topThird
	println("High confidence accuracy: " + highConfidenceAcc)

	val midConfidenceAcc = 100 * sortedConfidenceScores
			.slice(topThird, midThird)
			.count(x => x._2 == x._3).toDouble / (midThird - topThird)
	println("Mid confidence accuracy: " + midConfidenceAcc)

	val lowConfidenceAcc = 100 * sortedConfidenceScores
			.slice(midThird, numExamples)
			.count(x => x._2 == x._3).toDouble / (numExamples - midThird)
	println("Low confidence accuracy: " + lowConfidenceAcc)
  }  

}
