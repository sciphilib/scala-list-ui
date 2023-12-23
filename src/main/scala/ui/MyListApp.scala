import scalafx.application.JFXApp3
import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ComboBox, ListView, TextField}
import scalafx.scene.layout.VBox
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets

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
          val comboBox = new ComboBox(Seq("Int", "Point2D")) {
            maxWidth = Double.MaxValue
            promptText = "Select Data Type"
          }
          val listView = new ListView[String]() { maxWidth = Double.MaxValue }
          val inputField = new TextField() { maxWidth = Double.MaxValue }
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

          comboBox.selectionModel().selectedItem.onChange { (_, _, newValue) =>
            val isTypeSelected = newValue != null
            addButton.disable = !isTypeSelected
            addAtIndexButton.disable = !isTypeSelected
            deleteButton.disable = !isTypeSelected
            deleteAtIndexButton.disable = !isTypeSelected
          }

          children = Seq(
            comboBox,
            listView,
            new VBox {
              spacing = 5
              fillWidth = true
              children = Seq(
                inputField,
                new VBox {
                  spacing = 5
                  fillWidth = true
                  children = Seq(
                    addButton,
                    indexField,
                    addAtIndexButton,
                    deleteButton,
                    deleteAtIndexButton
                  )
                }
              )
            }
          )

          addButton.onAction = _ => {
            val item = comboBox.value.value match {
              case "Int"     => inputField.text.value
              case "Point2D" => inputField.text.value
            }
            listView.items.value += item
          }

          addAtIndexButton.onAction = _ => {
            val idx = indexField.text.value.toIntOption.getOrElse(0)
            val item = comboBox.value.value match {
              case "Int"     => inputField.text.value
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
}
