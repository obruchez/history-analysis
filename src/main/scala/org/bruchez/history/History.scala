package org.bruchez.history

import java.io._

object History {
  def main(args: Array[String]) {
    if (args.size < 2) {
      println("File(s) needed")
      for (arg <- args) println(arg)
      return
    }

    val inFiles = args.init
    val outFile = args.last

    val out = new PrintWriter(new File(outFile), "UTF-8")

    try {
      inFiles.foreach(dumpFileResults(out, _))
      //if (inFiles.size > 1) dumpAllFileResults(out, inFiles)
    } finally {
      out.close()
    }
  }

  def fileToClass(file: String): String = {
    val fileParts = file.split('.')
    if (fileParts.size <= 1) file else fileParts.init.mkString(".")
  }

  def dumpFileResults(out: PrintWriter, file: String) {
    dumpResults(out, fileToClass(file), CSV.parse(scala.io.Source.fromFile(file).mkString+"\n"))
  }

  def dumpAllFileResults(out: PrintWriter, files: Seq[String]) {
    val lines = files.map(file => CSV.parse(scala.io.Source.fromFile(file).mkString+"\n")).reduce(_ ++ _)
    dumpResults(out, "Toutes les classes ("+files.map(fileToClass).reduceLeft(_+" / "+_)+")", lines)
  }

  def dumpResults(out: PrintWriter, header: String, lines: List[List[String]]) {
    val stars = "*" * (header.length + 4)
    out.println(stars)
    out.println("* %s *".format(header))
    out.println(stars)
    out.println()

    val fixedValues = Map("oui" -> "Oui", "non" -> "Non")
    val trimmedLines =
      for (line <- lines) yield {
        for (cell <- line) yield {
          val trimmed = cell.trim
          fixedValues.get(trimmed).getOrElse(trimmed)
        }
      }

    implicit val keys = Key.keys(trimmedLines)
    //keys.foreach(key => out.println(s"${key.fixedLengthNumber} ${key.name}"))

    implicit val answers = Answer.answersFromLines(trimmedLines)
    //answers.foreach(answer => { answer.dump(out); out.println("===") })

    out.println("Réponses: "+answers.size)
    out.println()

    // Totals
    out.println("Totaux")
    out.println("------")
    out.println()
    Answer.dumpTotals(out, Answer.totals(answers))
    out.println()

    Answer.dumpCorrelations(
      out,
      fromKeyNumbers = Seq("2"),
      toKeyNumbers = Seq("4.1", "4.2", "4.3", "4.4"))

    Answer.dumpCorrelations(
      out,
      fromKeyNumbers = Seq("3.5", "3.6"),
      toKeyNumbers = Seq("4.1", "4.2", "4.3", "4.4"))

    Answer.dumpCorrelations(
      out,
      fromKeyNumbers = Seq("3.1", "3.2", "3.3"),
      toKeyNumbers = Seq("4.2", "4.3", "4.4"))
  }
}

case class Key(number: String, name: String, possibleValues: Seq[String]) {
  def fixedLengthNumber: String = " " * (4 - number.length) + number
}

object Key {
  private val RowsPerAnswer = 51

  def keys(lines: List[List[String]]): Seq[Key] = {
    val firstAnswer = lines.take(RowsPerAnswer)
    val linesWithPrevious = firstAnswer.zip(List("", "") :: firstAnswer.init)

    for {
      (line, previousLine) <- linesWithPrevious.filter(_._1(1).nonEmpty)
    } yield Key(
      number = Some(line(0)).filter(_.nonEmpty).getOrElse(previousLine(0)),
      name = line(1),
      possibleValues = line.drop(2).filterNot(_.isEmpty))
  }

  def findByNumber(number: String)(implicit keys: Seq[Key]): Option[Key] =
    keys.find(_.number == number)
}

case class Answer(
    number: Int,
    keyValues: Map[String, Seq[(String, Int)]] = Map()) {
  def dump(out: PrintWriter) {
    out.println("Numéro: " + number)
    out.println("Valeurs:")
    for ((key, values) <- keyValues) {
      val valueCountStrings = values map { case (value, count) => s"$value ($count)" }
      out.println(s" - $key: ${Some(valueCountStrings).filterNot(_.isEmpty).map(_.reduceLeft(_+", "+_)).getOrElse("-")}")
    }
  }
}

object Answer {
  private val LastKey = "5"

  def apply(lines: Seq[Seq[String]]): Answer = {
    @scala.annotation.tailrec
    def answerFromLines(remainingLines: Seq[Seq[String]], answer: Answer): Answer = {
      val key = if (remainingLines(0)(1).nonEmpty) remainingLines(0)(1) else remainingLines(1)(1)
      assert(key.nonEmpty)

      def countFromString(value: String, count: String): Option[Int] = Some(count).filterNot(_.isEmpty).map(_.toInt)

      val values = remainingLines(0).drop(2).zipWithIndex map { case (value, index) =>
        (value, remainingLines(1).lift(2 + index).getOrElse(""))
      } filter { case (value, count) =>
        value.nonEmpty && countFromString(value, count).nonEmpty
      } map { case (value, count) =>
        (value, countFromString(value, count).get)
      }

      val keyValues = answer.keyValues + (key -> values)

      val newAnswer = answer.copy(keyValues = keyValues)

      // Skip at least two rows + any extra empty row
      val linesToDrop = 2 + remainingLines.drop(2).takeWhile(cellsEmpty).size

      if (remainingLines.size <= linesToDrop)
        newAnswer
      else
        answerFromLines(remainingLines.drop(linesToDrop), newAnswer)
    }

    answerFromLines(lines.drop(1), Answer(number = lines(0)(0).toInt))
  }

  @scala.annotation.tailrec
  def answersFromLines(remainingLines: Seq[Seq[String]], parsedAnswers: Seq[Answer] = Seq()): Seq[Answer] = {
    val nextAnswerOption = remainingLines.zipWithIndex.find(li => li._1(0) == LastKey && li._2 != 0).map(_._2 + 2)

    nextAnswerOption match {
      case None => parsedAnswers
      case Some(nextAnswer) =>
        answersFromLines(
          remainingLines.drop(nextAnswer).dropWhile(_(0).isEmpty),
          parsedAnswers :+ Answer(remainingLines.take(nextAnswer)))
    }
  }

  def cellsEmpty(line: Seq[String]): Boolean = line.map(_.isEmpty).fold(true)(_ && _)
  def isHeaderLine(line: Seq[String]): Boolean = line(0).map(_.isDigit).fold(true)(_ && _) && cellsEmpty(line.tail)

  def valuesAndCountsAsString(key: Key, valuesAndCounts: Map[String, (Int, Int)], answerCount: Int): String = {
    val sortedValuesAndCounts = valuesAndCounts.toSeq.sortBy(_._1).reverse.sortBy(_._2._1).reverse

    Some(sortedValuesAndCounts map { kv =>
      val percentage = 100.0 * kv._2._2.toDouble / answerCount
      assert(percentage <= 100.0)

      val percentageString =", %.2f%%".format(percentage)

      kv._1+" (%d%s)".format(kv._2._1, percentageString)
    }).filter(_.nonEmpty).map(_.reduceLeft(_+", "+_)).getOrElse("-")
  }

  case class Totals(
    // First int value is total counting duplicate values, second int value is total counting duplicate values as 1
    totalsByKeyAndValue: Map[String, Map[String, (Int, Int)]],
    answerCount: Int)

  def totals(
      answers: Seq[Answer],
      withKeyValues: Boolean = true): Totals = {
    val mutableTotals = collection.mutable.Map[String, collection.mutable.Map[String, (Int, Int)]]()

    for (answer <- answers) {
      def addKeyAndValue(key: String, value: String, count: Int = 1) {
        val keyMap = mutableTotals.getOrElse(key, collection.mutable.Map[String, (Int, Int)]())
        mutableTotals.put(key, keyMap)

        val valueCount = keyMap.getOrElse(value, (0, 0))
        keyMap.put(value, (valueCount._1 + count, valueCount._2 + 1))
      }

      if (withKeyValues) {
        for ((key, values) <- answer.keyValues; value <- values) addKeyAndValue(key, value._1, value._2)
      }
    }

    Totals(mutableTotals.toMap.map(kv => kv._1 -> kv._2.toMap), answers.size)
  }

  def dumpTotals(out: PrintWriter, totals: Totals)(implicit keys: Seq[Key]) {
    for (key <- keys; totalsByValue <- totals.totalsByKeyAndValue.get(key.name)) {
      val valueCount = totalsByValue.map(_._2._1).fold(0)(_ + _)
      val valueCountString = " ("+valueCount+")"

      out.println(
        s" - ${key.fixedLengthNumber} ${key.name}$valueCountString: "+
        Answer.valuesAndCountsAsString(key, totalsByValue, totals.answerCount))
    }
  }

  def filteredAnswers(answers: Seq[Answer], key: String, value: String): Seq[Answer] =
    answers.filter(_.keyValues(key).map(_._1).toSet.contains(value))

  def filteredAnswers(answers: Seq[Answer], keys: Set[String]): Seq[Answer] =
    answers.map(answer => answer.copy(keyValues = answer.keyValues.filter(kv => keys.contains(kv._1))))

  def dumpCorrelations(out: PrintWriter,
                       fromKeyNumbers: Seq[String],
                       toKeyNumbers: Seq[String])(implicit keys: Seq[Key], answers: Seq[Answer]) {
    for {
      toKeyNumber <- toKeyNumbers
      toKey = Key.findByNumber(toKeyNumber).get
    } {
      out.println(toKey.name)
      out.println("-" * toKey.name.size)
      out.println()

      for {
        fromKeyNumber <- fromKeyNumbers
        fromKey = Key.findByNumber(fromKeyNumber).get
        fromKeyPossibleValue <- fromKey.possibleValues

      } {
        val answersForValue = Answer.filteredAnswers(answers, fromKey.name, fromKeyPossibleValue)

        if (answersForValue.nonEmpty) {
          out.println(s"${fromKey.name} = $fromKeyPossibleValue:")
          Answer.dumpTotals(out, Answer.totals(Answer.filteredAnswers(answersForValue, Set(toKey.name))))
          out.println()
        }
      }
    }
  }
}
