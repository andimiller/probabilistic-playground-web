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
object ProbabilisticPlayground extends TyrianApp[Msg, Model]:

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) =
    (Model("", 10, List.empty), Cmd.None)

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
        model.rows
      else
        Row(
          model.input,
          hash(model.input),
          hashToTheta(hash(model.input)),
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
    case Msg.NewInput(s) => (model.copy(input = s), Cmd.None)
    case Msg.Clear       => init(Map.empty)
    case Msg.AddOneHundred =>
      (
        model,
        Cmd.combineAll((1 to 100).toList.map { i =>
          Cmd.Batch(
            Cmd.Emit(Msg.NewInput(i.toString)),
            Cmd.Emit(Msg.EnterInput)
          )
        })
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
          document.getElementById("input-box").asInstanceOf[Input].value = ""
        }
      )
    case Msg.NoOp => (model, Cmd.None)

  def view(model: Model): Html[Msg] =
    div()(
      div(cls := "container")(
        div(cls := "row")(
          h3("Theta Sketch TopK"),
          p(
            "Theta Sketches are a type of probabilistic data structure which store a list of hashes for items it's seen, and uses those to estimate how many unique items have been seen."
          ),
          p(
            "This interactive demo allows you to input strings and see how they'd be entered into a Top K Theta Sketch."
          ),
          h5("Configuration"),
          p(
            "The one configuration parameter is K, this is how many hashes we should keep track of, I have set the default to 10"
          ),
          h5("Internal Data"),
          p("For each item you can see the following:"),
          ul(cls := "collection")(
            li(cls := "collection-item")("Input shows the raw string input"),
            li(cls := "collection-item")(
              "Hash shows the input hashed into a 64-bit integer using murmur3"
            ),
            li(cls := "collection-item")(
              "Theta shows this hash moved into the range of 0.0 to 1.0, which is how this algorithm operates"
            ),
            li(cls := "collection-item")(
              "Included shows whether this item would be kept by a real sketch with the current configuration"
            )
          ),
          h5("Formula"),
          p("The formula to calculate the estimate is:"),
          blockquote("(K - 1) / topTheta"),
          p(
            "Where K is the K from the configuration, and topTheta is the highest value of theta we've stored"
          )
        ),
        div(cls := "row")(
          div(id := "inputs", cls := "col s3")(
            h5("Inputs"),
            button(
              cls := "waves-effect waves-light btn",
              onClick(Msg.AddOneHundred)
            )("Add 100 items"),
            button(
              cls := "waves-effect waves-light btn",
              onClick(Msg.Clear)
            )("Clear"),
            input(
              id          := "input-box",
              placeholder := "New string to add",
              onInput(s => Msg.NewInput(s)),
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
                  th("Included")
                )
              ),
              tbody(
                model.rows.sortBy(_.theta).map { row =>
                  val bgStyle =
                    if (row.keep) "background-color: lightgray" else ""
                  tr(style := bgStyle)(
                    td(row.input),
                    td(row.hash.toString),
                    td(row.theta.toString),
                    td(row.keep.toString)
                  )
                }
              )
            )
          ),
          div(id := "output", cls := "col s3")(
            h5("Outputs"),
            p(
              s"Estimate Formula: ${model.topK} - 1 / ${model.topTheta.getOrElse(0d)}"
            ),
            p(
              s"Estimated unique items: ${model.estimate}"
            ),
            p(s"Actual unique items: ${model.uniques}")
          )
        )
      )
    )

  def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.None

case class Row(input: String, hash: Long, theta: Double, keep: Boolean)

case class Model(
    input: String,
    topK: Int,
    rows: List[Row]
) {
  def topTheta: Option[Double] = rows.filter(_.keep).map(_.theta).maxOption

  def estimate: Double =
    (topK - 1).toDouble / topTheta.getOrElse(0d)
  def uniques: Long =
    rows.map(_.input).distinct.size
}

enum Msg:
  case NewInput(s: String)
  case NewK(s: String)
  case AddOneHundred
  case Clear
  case EnterInput
  case NoOp
