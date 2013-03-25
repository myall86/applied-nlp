package appliednlp.classify

import nak.core.AttrVal
import chalk.lang.eng.PorterStemmer

import java.io.File


/**
 * An object that sets up the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 */
object TextFeaturesOpts {

  import org.rogach.scallop._
  
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
For usage see below:
	     """)
    val help = opt[Boolean]("help", noshort = true, descr = "Show this message")
    val verbose = opt[Boolean]("verbose")
    val featuresFile = opt[String]("featuresFile",short='f', descr="File to generate features from.")
    val inputFile = trailArg[String]("inputfile", descr = "Input file to create features from.")
  }
}


/**
 * An application for extracting features from the 20 Newsgroups data set for 
 * classification.
 */
object TextFeatures {

	def generateFeatures(featuresFile: String): Array[String] =
	{
		io.Source.fromFile(featuresFile).getLines.map{ line =>
			line.split("\t")(2).split(" ")
		}
		.flatten
		.toArray
		.distinct
	}

	def extractFeatures(features: Array[String], parentDirName: String, file: File) 
	{
		if(file.isDirectory)
			file.listFiles.foreach(subFile => extractFeatures(features, file.getName, subFile))
		else 
		{
			val text = io.Source.fromFile(file.getPath, "ISO-8859-1").getLines.mkString(" ").toLowerCase
			val formatedFeatures = features.map { feature =>
				if(text.contains(feature)) feature ++ "=1,"
				else feature ++ "=0,"
			}
			.mkString("")
			println(formatedFeatures + parentDirName)
		}
	}

  def main(args: Array[String]) {

    // Parse and get the command-line options
    val opts = TextFeaturesOpts(args)
   
    val featuresFile = opts.featuresFile()

	val features = generateFeatures(featuresFile)
	//println("#features = " + features.size)

	val inputFile = opts.inputFile()

	val file = new File(inputFile) 
	extractFeatures(features, "", file)
  }

}



