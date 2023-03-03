package net.andimiller

import cats.effect.IO
import org.scalajs.dom.document
import org.scalajs.dom.html.Input
import tyrian.Html.*
import tyrian.Navigation
import tyrian.*

import java.nio.ByteBuffer
import scala.scalajs.js.annotation.*
import scala.util.hashing.MurmurHash3

@JSExportTopLevel("TyrianApp")
object Tuple extends TyrianApp[Msg, Model]:

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) =
    (Model("", 0, 10, List.empty, MergeType.Sum), Cmd.None)

  // we're just going to cheat and hash it twice with murmur3 32-bit
  def hash(s: String): Long =
    ByteBuffer
      .allocate(8)
      .putInt(MurmurHash3.stringHash(s))
      .putInt(MurmurHash3.stringHash(s.reverse))
      .getLong(0)

  private def hashToTheta(hash: Long) =
    ((hash.toDouble / Long.MaxValue) / 2) + 0.5

  def enterInput(model: Model): Model = {
    val newHash = hash(model.input)
    val rows =
      if (model.rows.map(_.hash).contains(newHash))
        model.rows.map {
          case r if r.hash == newHash =>
            val result = model.mergeType match
              case MergeType.Sum => r.value + model.value
              case MergeType.Min => Math.min(r.value, model.value)
              case MergeType.Max => Math.max(r.value, model.value)
            r.copy(value = result)
          case r => r
        }
      else
        Row(
          model.input,
          hash(model.input),
          hashToTheta(hash(model.input)),
          model.value,
          true
        ) :: model.rows
    val keepMe = rows.sortBy(_.theta).take(model.topK).toSet
    val rows2 = rows.map { row =>
      row.copy(
        keep = keepMe.contains(row)
      )
    }
    model.copy(
      input = "",
      rows = rows2
    )
  }

  def recalculate(model: Model): Model = {
    val keepMe = model.rows.sortBy(_.theta).take(model.topK).toSet
    val rows2 = model.rows.map { row =>
      row.copy(
        keep = keepMe.contains(row)
      )
    }
    model.copy(
      rows = rows2
    )
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case Msg.NewKey(k)        => (model.copy(input = k), Cmd.None)
    case Msg.NewValue(v)      => (model.copy(value = v), Cmd.None)
    case Msg.SetMergeType(mt) => (model.copy(mergeType = mt), Cmd.None)
    case Msg.Clear            => init(Map.empty)
    case Msg.AddExamples =>
      (
        model.copy(topK = 10),
        Cmd.combineAll((('a' to 'z').zip(1 to 26)).toList.map { case (k, v) =>
          Cmd.Batch(
            Cmd.Emit(Msg.NewKey(k.toString)),
            Cmd.Emit(Msg.NewValue(v.toLong)),
            Cmd.Emit(Msg.EnterInput)
          )
        } ++ List(Cmd.Emit(Msg.NewK("10"))))
      )
    case Msg.NewK(s) =>
      (
        recalculate(
          s.toIntOption.map(i => model.copy(topK = i)).getOrElse(model)
        ),
        Cmd.None
      )
    case Msg.EnterInput =>
      (
        enterInput(model),
        Cmd.SideEffect {
          document.getElementById("input-box-key").asInstanceOf[Input].value =
            ""
          document.getElementById("input-box-value").asInstanceOf[Input].value =
            ""
        }
      )
    case Msg.NoOp => (model, Cmd.None)

  def view(model: Model): Html[Msg] =
    div()(
      div(cls := "container")(
        div(cls := "row")(
          h3("Tuple Sketch TopK"),
          p(
            "Tuple Sketches are an extension of Theta Sketches where we can also keep track of a value, and merge them"
          ),
          h4("Purpose"),
          p(
            "Since we've kept a sampling of the values, we can do operations like average, percentile or sum and extrapolate the result for the whole input set"
          ),
          h4("Simulation"),
          p(
            "In this simulation we're keeping Longs as our values, and making Sum, Min and Max available as merge functions, but we could use any data type, and any merge function."
          )
        ),
        div(cls := "row")(
          div(id := "inputs", cls := "col m3 s12")(
            h5("Inputs"),
            button(
              cls := "waves-effect waves-light btn",
              onClick(Msg.Clear)
            )("Clear"),
            button(
              cls := "waves-effect waves-light btn",
              onClick(Msg.AddExamples)
            )("Add Examples"),
            input(
              id          := "input-box-key",
              placeholder := "New string to add",
              onInput(s => Msg.NewKey(s)),
              onKeyDown { e =>
                if (e.keyCode == 13)
                  Msg.EnterInput
                else
                  Msg.NoOp
              }
            ),
            input(
              id          := "input-box-value",
              placeholder := "Value to associate",
              onInput(s => Msg.NewValue(s.toLong)),
              onKeyDown { e =>
                if (e.keyCode == 13)
                  Msg.EnterInput
                else
                  Msg.NoOp
              }
            ),
            div(cls := "input-field")(
              label(attribute("for", "k"))("K value (how many items to keep)"),
              br(),
              input(
                id          := "k",
                placeholder := "10",
                onInput(s => Msg.NewK(s))
              )
            ),
            div(cls := "input-field")(
              form(action := "#")(
                List(
                  label(attribute("for", "mergeType"))(
                    "Method to use when we have to merge values"
                  )
                ) ++
                  MergeType.values.flatMap { mt =>
                    List(
                      label(
                        input(
                          `type` := "radio",
                          id     := mt.toString,
                          name   := "mergeType",
                          value  := mt.toString,
                          if (mt == MergeType.Sum) attribute("checked", "")
                          else attribute("notchecked", ""),
                          onInput(Msg.SetMergeType(mt))
                        ),
                        span(mt.toString)
                      )
                    )
                  }: _*
              )
            )
          ),
          div(id := "data", cls := "col s6")(
            h5("Data"),
            table()(
              thead(
                tr(
                  th("Input"),
                  th("Hash"),
                  th("Theta"),
                  th("Value"),
                  th("Included")
                )
              ),
              tbody(
                model.rows.sortBy(_.theta).zipWithIndex.map { case (row, idx) =>
                  val bgStyle =
                    if (row.keep) "background-color: lightgray" else ""
                  tr(style := bgStyle)(
                    td(row.input),
                    td(row.hash.toString),
                    if (idx == model.topK - 1)
                      b(`class` := "green-text")(td(row.theta.toString))
                    else td(row.theta.toString),
                    td(row.value.toString),
                    td(row.keep.toString)
                  )
                }
              )
            )
          ),
          div(id := "output", cls := "col m3 s12")(
            h5("Outputs"),
            p(
              List(
                text(s"Estimate Formula: ${model.topK} - 1 / "),
                b(`class` := "green-text")(
                  s"${model.topTheta.getOrElse(0d)}"
                )
              )
            ),
            p(
              List(
                text("Estimated unique items: "),
                b(`class` := "orange-text")(
                  model.estimate.toString
                )
              )
            ),
            p(s"Actual unique items: ${model.uniques}"),
            hr(),
            p(
              List(
                text("Sum of Included Values: "),
                b(`class` := "red-text")(model.includedSum.toString)
              )
            ),
            p(
              List(
                text(s"Estimated Sum Formula: "),
                b(`class` := "red-text")(model.includedSum.toString),
                text(" * ("),
                b(`class` := "orange-text")(model.estimate.toString),
                text(s" / ${model.topK})")
              )
            ),
            p(
              List(
                text(s"Estimated Sum: "),
                b(`class` := "purple-text")(model.estimatedSum.toString)
              )
            ),
            p(s"Actual Sum: ${model.sum}"),
            hr(),
            p(
              List(
                text(s"Estimated Average Formula: "),
                b(`class` := "purple-text")(model.estimatedSum.toString),
                text(" / "),
                b(`class` := "orange-text")(model.estimate.toString)
              )
            ),
            p(s"Estimated Average: ${model.estimatedAverage}"),
            p(s"Actual Average: ${model.average}")
          )
        )
      )
    )

  def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.None

case class Row(
    input: String,
    hash: Long,
    theta: Double,
    value: Long,
    keep: Boolean
)

case class Model(
    input: String,
    value: Long,
    topK: Int,
    rows: List[Row],
    mergeType: MergeType
) {
  def topTheta: Option[Double] = rows.filter(_.keep).map(_.theta).maxOption

  def estimate: Double =
    (topK - 1).toDouble / topTheta.getOrElse(0d)
  def uniques: Long =
    rows.map(_.input).distinct.size
  def sum: Long =
    rows.map(_.value).sum
  def estimatedSum: Long =
    (rows.filter(_.keep).map(_.value).sum * (estimate / topK)).toLong
  def includedSum: Long =
    rows.filter(_.keep).map(_.value).sum
  def average: Long =
    if (rows.isEmpty) 0 else rows.map(_.value).sum / rows.length
  def estimatedAverage: Long =
    (rows
      .filter(_.keep)
      .map(_.value)
      .sum * (estimate / topK) / estimate).toLong
}

enum MergeType:
  case Sum
  case Min
  case Max

enum Msg:
  case NewKey(k: String)
  case NewValue(v: Long)
  case SetMergeType(mt: MergeType)
  case AddExamples
  case NewK(s: String)
  case Clear
  case EnterInput
  case NoOp
