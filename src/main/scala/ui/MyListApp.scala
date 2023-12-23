import scalafx.application.JFXApp3
import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ComboBox, ListView, TextField}
import scalafx.scene.layout.VBox
import scalafx.collections.ObservableBuffer

object MyListApp extends JFXApp3 {
  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "MyList Manager"
      scene = new Scene {
        content = new VBox {
          val comboBox = new ComboBox(Seq("Int", "Point2D"))
          val listView = new ListView[String]()
          val inputField = new TextField()
          val addButton = new Button("Add")
          val addAtIndexButton = new Button("Add at Index")
          val deleteButton = new Button("Delete")
          val deleteAtIndexButton = new Button("Delete at Index")
          val indexField = new TextField()

          children = Seq(
            comboBox,
            listView,
            new VBox {
              children = Seq(
                inputField,
                new VBox {
                  children = Seq(addButton, indexField, addAtIndexButton, deleteButton, deleteAtIndexButton)
                }
              )
            }
          )

          addButton.onAction = _ => {
            val item = comboBox.value.value match {
              case "Int" => inputField.text.value
              case "Point2D" => inputField.text.value
            }
            listView.items.value += item
          }

          addAtIndexButton.onAction = _ => {
            val idx = indexField.text.value.toIntOption.getOrElse(0)
            val item = comboBox.value.value match {
              case "Int" => inputField.text.value
              case "Point2D" => inputField.text.value
            }
            if (idx >= 0 && idx <= listView.items.value.size) {
              listView.items.value.insert(idx, item)
            }
          }

          deleteButton.onAction = _ => {
            val selectedIndex = listView.selectionModel.value.getSelectedIndex
            if (selectedIndex >= 0) {
              listView.items.value.remove(selectedIndex)
            }
          }

          deleteAtIndexButton.onAction = _ => {
            val idx = indexField.text.value.toIntOption.getOrElse(-1)
            if (idx >= 0 && idx < listView.items.value.size) {
              listView.items.value.remove(idx)
            }
          }
        }
      }
    }
  }
}
