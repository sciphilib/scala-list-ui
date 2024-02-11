import scalafx.application.JFXApp3
import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout.VBox
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.ButtonType

import scalafx.stage.FileChooser
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.fasterxml.jackson.core.`type`.TypeReference
import java.io.File

import scala.reflect.ClassTag

import data.MyList
import data.Point2D
import data.Point2DImplicits
import data.Point2DImplicits.point2DOrdering

object MyListApp extends JFXApp3 {
  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "MyList Manager"
      width = 800
      height = 600

      scene = new Scene {
        val contentBox = new VBox {
          padding = Insets(50)
          spacing = 10
          fillWidth = true
          val sortButton = new Button("Sort") {
            maxWidth = Double.MaxValue
          }
          val serializeButton = new Button("Serialize to JSON") {
            maxWidth = Double.MaxValue
          }
          val deserializeButton = new Button("Deserialize from JSON") {
            maxWidth = Double.MaxValue
          }
          val comboBox = new ComboBox(Seq("Int", "Point2D")) {
            maxWidth = Double.MaxValue
            promptText = "Select Data Type"
          }
          val myPoint2DList = new MyList[Point2D]()
          val listView = new ListView[String]() {
            items = ObservableBuffer[String]()
            maxWidth = Double.MaxValue
          }

          val addButton = new Button("Add") { maxWidth = Double.MaxValue }
          val addAtIndexButton = new Button("Add at Index") {
            maxWidth = Double.MaxValue
          }
          val deleteButton = new Button("Delete") { maxWidth = Double.MaxValue }
          val deleteAtIndexButton = new Button("Delete at Index") {
            maxWidth = Double.MaxValue
          }
          val indexField = new TextField() { maxWidth = Double.MaxValue }

          addButton.disable = true
          addAtIndexButton.disable = true
          deleteButton.disable = true
          deleteAtIndexButton.disable = true
          sortButton.disable = true

          comboBox.selectionModel().selectedItem.onChange { (_, _, newValue) =>
            val isTypeSelected = newValue != null
            addButton.disable = !isTypeSelected
            addAtIndexButton.disable = !isTypeSelected
            deleteButton.disable = !isTypeSelected
            deleteAtIndexButton.disable = !isTypeSelected
            sortButton.disable = !isTypeSelected
          }

          children = Seq(
            comboBox,
            listView,
            new VBox {
              spacing = 5
              fillWidth = true
              children = Seq(
                new VBox {
                  spacing = 5
                  fillWidth = true
                  children = Seq(
                    addButton,
                    indexField,
                    addAtIndexButton,
                    deleteButton,
                    deleteAtIndexButton,
                    sortButton,
                    serializeButton,
                    deserializeButton
                  )
                }
              )
            }
          )

          addButton.onAction = _ => {
            comboBox.value.value match {
              case "Int" =>
                getIntFromUser() match {
                  case Some(value) => listView.items.value += value.toString
                  case None        =>
                }
              case "Point2D" =>
                getPoint2DFromUser() match {
                  case Some((x, y)) =>
                    myPoint2DList.add(Point2D(x, y))
                    updateListView()
                  case None =>
                }
              case _ =>
            }
          }

          def updateListView(): Unit = {
            listView.items.value.clear()
            myPoint2DList.foreach(p => listView.items.value += s"Point2D(${p.x}, ${p.y})")
          }

          addAtIndexButton.onAction = _ => {
            val idx = indexField.text.value.toIntOption
              .getOrElse(-1)
            if (idx >= 0 && idx <= listView.items.value.size) {
              comboBox.value.value match {
                case "Int" =>
                  getIntFromUser() match {
                    case Some(value) =>
                      listView.items.value.insert(
                        idx,
                        value.toString
                      )
                    case None =>
                  }
                case "Point2D" =>
                  getPoint2DFromUser() match {
                    case Some((x, y)) =>
                      myPoint2DList.addAt(idx, Point2D(x, y))
                      updateListView()
                    case None =>
                  }
                case _ =>
                  new Alert(AlertType.Warning) {
                    title = "Warning"
                    headerText = "Type Not Selected"
                    contentText = "Please select a data type before adding."
                  }.showAndWait()
              }
            } else {
              new Alert(AlertType.Warning) {
                title = "Warning"
                headerText = "Invalid Index"
                contentText = "Please enter a valid index."
              }.showAndWait()
            }
          }

          deleteButton.onAction = _ => {
            val selectedIndex = listView.selectionModel.value.getSelectedIndex
            if (selectedIndex >= 0) {
              val selectedString = listView.items.value(selectedIndex)
              parsePoint2D(selectedString) match {
                case Some(point2D) =>
                  myPoint2DList.delete(point2D)
                  updateListView()
                case None =>
                  println("Error: Could not parse the selected item back into a Point2D object.")
              }
            }
          }

          deleteAtIndexButton.onAction = _ => {
            val idx = indexField.text.value.toIntOption.getOrElse(-1)
            if (idx >= 0 && idx < listView.items.value.size) {
              myPoint2DList.deleteAt(idx)
              updateListView()
            }
          }

          sortButton.onAction = _ => {
            comboBox.value.value match {
              case "Int" =>
                val sortedInts = listView.items.value.map(_.toInt).sorted
                listView.items.value.clear()
                listView.items.value ++= sortedInts.map(_.toString)

              case "Point2D" =>
                val sortedPoints = listView.items.value.map { s =>
                  val Array(x, y) = s.stripPrefix("Point2D(").stripSuffix(")").split(", ")
                  Point2D(x.toDouble, y.toDouble)
                }.sorted(Point2DImplicits.point2DOrdering).map(point => s"Point2D(${point.x}, ${point.y})")
                listView.items.value.clear()
                listView.items.value ++= sortedPoints

              case _ =>
                new Alert(AlertType.Warning) {
                  title = "Warning"
                  headerText = "Data Type Not Selected"
                  contentText = "Please select a data type for sorting."
                }.showAndWait()
            }
          }

          def deserializeListFromJsonFile(primaryStage: scalafx.stage.Window): Unit = {
            val fileChooser = new FileChooser {
              title = "Open JSON File"
              extensionFilters += new FileChooser.ExtensionFilter("JSON files (*.json)", "*.json")
            }
            val file = fileChooser.showOpenDialog(primaryStage)
            if (file != null) {
              try {
                val mapper = new ObjectMapper() with ScalaObjectMapper
                mapper.registerModule(DefaultScalaModule)
                val pointsList: Seq[Point2D] = mapper.readValue(file, new TypeReference[Seq[Point2D]] {})
                myPoint2DList.clear()
                pointsList.foreach(myPoint2DList.add)
                updateListView()
              } catch {
                case ex: Exception =>
                  ex.printStackTrace()
              }
            }
          }

          def getPointsListFromUI(): Seq[Point2D] = {
            listView.items().toSeq.collect {
              case s if parsePoint2D(s).isDefined => parsePoint2D(s).get
            }
          }

          def updateUIWithPointsList(pointsList: Seq[Point2D]): Unit = {
            listView.items().clear()
            listView.items() ++= pointsList.map(p => s"Point2D(${p.x}, ${p.y})")
          }

          def parsePoint2D(str: String): Option[Point2D] = {
            val Point2DRegex = """Point2D\((-?\d+\.?\d*),\s*(-?\d+\.?\d*)\)""".r
            str match {
              case Point2DRegex(x, y) => Some(Point2D(x.toDouble, y.toDouble))
              case _ => None
            }
          }

          serializeButton.onAction = _ => {
            val pointsList: Seq[Point2D] = getPointsListFromUI()
            serializeListToJsonFile(pointsList, stage)
          }

          deserializeButton.onAction = _ => {
            val pointsList = deserializeListFromJsonFile(stage)
          }

        }
        root = contentBox

        width.onChange { (_, _, newWidth) =>
          contentBox.padding = Insets(
            height() * 0.25,
            newWidth.doubleValue() * 0.25,
            height() * 0.25,
            newWidth.doubleValue() * 0.25
          )
        }

        height.onChange { (_, _, newHeight) =>
          contentBox.padding = Insets(
            newHeight.doubleValue() * 0.25,
            width() * 0.25,
            newHeight.doubleValue() * 0.25,
            width() * 0.25
          )
        }
      }
    }
  }

  def getIntFromUser(): Option[Int] = {
    val dialog = new TextInputDialog()
    dialog.title = "Input Integer"
    dialog.contentText = "Please enter an integer:"
    val result = dialog.showAndWait()

    result match {
      case Some(input) => input.toIntOption
      case None        => None
    }
  }

  def getPoint2DFromUser(): Option[(Double, Double)] = {
    val dialog = new Dialog[(Double, Double)]() {
      title = "Input Point2D"
      headerText = "Please enter x and y coordinates:"
      dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
      val xField = new TextField() { promptText = "x" }
      val yField = new TextField() { promptText = "y" }
      dialogPane().content = new VBox {
        spacing = 10
        children = Seq(xField, yField)
      }

      resultConverter = dialogButton => {
        if (dialogButton == ButtonType.OK) {
          (xField.text.value.toDoubleOption, yField.text.value.toDoubleOption) match {
            case (Some(x), Some(y)) => (x, y)
            case _ => null
          }
        } else null
      }
    }

    val result = dialog.showAndWait()
    result.asInstanceOf[Option[(Double, Double)]]
  }

  def parsePoint2D(str: String): Option[Point2D] = {
    val pattern = "Point2D\\((-?\\d+\\.?\\d*), (-?\\d+\\.?\\d*)\\)".r
    str match {
      case pattern(x, y) => Some(Point2D(x.toDouble, y.toDouble))
      case _ => None
    }
  }

  def serializeListToJsonFile(list: Seq[Point2D], primaryStage: scalafx.stage.Window): Unit = {
    val fileChooser = new FileChooser {
      title = "Save as JSON"
      extensionFilters += new FileChooser.ExtensionFilter("JSON files (*.json)", "*.json")
    }
    val file = fileChooser.showSaveDialog(primaryStage)
    if (file != null) {
      try {
        val mapper = new ObjectMapper() with ScalaObjectMapper
        mapper.registerModule(DefaultScalaModule)
        mapper.writerWithDefaultPrettyPrinter().writeValue(file, list)
      } catch {
        case ex: Exception => ex.printStackTrace()
      }
    }
  }

}
