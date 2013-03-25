package appliednlp.classify

import nak.core.AttrVal
import chalk.lang.eng.PorterStemmer


/**
 * An object that sets up the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 */
object PpaFeaturesOpts {

  import org.rogach.scallop._
  
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
For usage see below:
	     """)
    val help = opt[Boolean]("help", noshort = true, descr = "Show this message")
    val verbose = opt[Boolean]("verbose")
    val bitstringsSource = opt[String]("bitstrings", descr = "File containing bitstrings")
    val extendedFeatures = opt[Boolean]("extended",short='e', descr="Use extended features.")
    val inputFile = trailArg[String]("inputfile", descr = "Input file to create features from.")
  }
}


/**
 * An application for extracting features from the PPA native format for 
 * classification.
 */
object PpaFeatures {

  /**
   * The main method -- do the work. Don't change it.
   */
  def main(args: Array[String]) {

    // Parse and get the command-line options
    val opts = PpaFeaturesOpts(args)
   
    val inputFile = opts.inputFile()

    val bitstrings = opts.bitstringsSource.get match {
      case Some(bitstringsSource) =>
        io.Source.fromFile(bitstringsSource).getLines.map { line =>
          val Array(word, bitstring) = line.split("\\s+")
          (word -> BitVector(bitstring))
        }.toMap

      case None => new collection.immutable.HashMap[String, BitVector]()
    }

    val featureExtractor =
      if (opts.extendedFeatures()) new ExtendedFeatureExtractor(bitstrings)
      else BasicFeatureExtractor

    io.Source.fromFile(inputFile).getLines.foreach { line =>
      val Array(id, verb, noun, prep, prepObj, attach) = line.split(" ")
      val features = featureExtractor(verb, noun, prep, prepObj)
      println(features.map(_.toString).mkString(",") + "," + attach)
    }

  }

}

/**
 * A trait for classes that can extract features from the information in
 * the PPA files.
 */
trait FeatureExtractor {
  
  /**
   * Given the verb, noun, preposition, and prepositional object,
   * create a set of AttrVal objects. (A "feature" is an attribute with a
   * value.) 
   */
  def apply(verb: String, noun: String, prep: String, prepObj: String): Iterable[AttrVal]
}

/**
 * The simplest feature extractor: each word gets a feature, where the 
 * attribute is the type of the word. 
 */
object BasicFeatureExtractor extends FeatureExtractor {

  override def apply(
    verb: String, noun: String, prep: String, prepObj: String): Iterable[AttrVal] = {
    List(
      AttrVal("verb", verb),
      AttrVal("noun", noun),
      AttrVal("prep", prep),
      AttrVal("prep_obj", prepObj))
  }

}

/**
 * An extended feature extractor. It is your job to fill this out further.
 */
class ExtendedFeatureExtractor(bitvectors: Map[String, BitVector])
  extends FeatureExtractor {

  lazy val stemmer = new PorterStemmer

  override def apply(
    verb: String, noun: String, prep: String, prepObj: String): Iterable[AttrVal] = {

    // Use the basic feature extractor to get the basic features (no need to 
    // duplicate effort and specify it again).
    val basicFeatures = BasicFeatureExtractor(verb, noun, prep, prepObj)

    // Extract more features
	val numBits = 8
	val verbBitstring = if(bitvectors.contains(verb)) bitvectors(verb).toString.reverse.take(numBits)
			else ""
	val nounBitstring = if(bitvectors.contains(noun)) bitvectors(noun).toString.reverse.take(numBits)
			else ""
	val prepBitstring = if(bitvectors.contains(prep)) bitvectors(prep).toString.reverse.take(numBits)
			else ""
	val prepObjBitstring = if(bitvectors.contains(prepObj)) bitvectors(prepObj).toString.reverse.take(numBits)
			else ""

	val extendedFeatures = Iterable(
		AttrVal("verb+prep", verb ++ "+" ++ prep),
		AttrVal("verb+prep_obj", verb ++ "+" ++ prepObj),
		AttrVal("noun+prep", noun ++ "+" ++ prep),
		AttrVal("noun+prep_obj", noun ++ "+" ++ prepObj),
		AttrVal("prep+prep_obj", prep ++ "+" ++ prepObj),
		AttrVal("verb_form", wordForm(verb)),
		AttrVal("noun_form", wordForm(noun)),
		AttrVal("prep_obj_form", wordForm(prepObj)),
		AttrVal("stem", stemmer(verb)),
		AttrVal("verb_bits", verbBitstring),
		AttrVal("noun_bits", nounBitstring),
		AttrVal("prep_bits", prepBitstring),
		AttrVal("prep_obj_bits", prepObjBitstring)
	)
	
    // Return the features. 
    basicFeatures ++ extendedFeatures
  }

	private def wordForm(word: String): String = 
	{
		try
		{
			word.replaceAll(",", "").toDouble
			"number"
		}
		catch
		{
			case e: Exception => 
			{
				if(word.contains("%")) "%"
				else if(isX_xForm(word)) "X-x"
				else if(isXXForm(word)) "XX"
				else "unspecified"
			}
		}	
	}

	private def isX_xForm(word: String): Boolean = 
	{
		if(word.length < 1 || word(0).isLower) return false
		for(i <- 1 to word.length - 1)	
			if(word(i).isUpper) return false
		return true	
	}

	private def isXXForm(word: String): Boolean = 
	{
		if(word.length < 1) return false
		for(i <- 0 to word.length - 1)	
			if(word(i).isLower) return false
		return true	
	}

}

/**
 * This is an entirely cruddy, slow implementation of a bit vector,
 * not using any bitwise ops, etc., but it should suffice for this problem.
 *
 * And, yes, we are using Ints where it could be Booleans, and we could have
 * the wrong values in there, but this keeps it easy, and again, is sufficient
 * for this problem.
 * 
 * Feel free to add more capability to this if it helps you create better
 * features.
 */
class BitVector(bits: IndexedSeq[Int]) {

  /**
   * Get the bit value at the given index.
   */
  def apply(index: Int) = bits(index)

  /**
   * Get the integer value of the bits
   */
  lazy val toInt = Integer.parseInt(bits.mkString, 2)

  /**
   *  Keep the top bits up to the given index, and then make the remaining bits
   *  zero.
   */
  def keepTopBits(index: Int) =
    new BitVector(bits.take(index) ++ Vector.fill(bits.length - index)(0))

  /**
   * Concatenate the bits together.
   */
  override def toString = bits.mkString
}

/**
 * Companion object to the BitVector class.
 */
object BitVector {

  /**
   * Create a bit vector from a string of zeros and ones.
   */
  def apply(bitstring: String) =
    new BitVector(bitstring.split("").drop(1).map(_.toInt).toIndexedSeq)
}



