import scala.io.StdIn.readLine
object inventory {
  case class Product(id: Int, name: String, quantity: Int, price: Double)
  val inventory1:Map[Int, Product] = Map(
    101 -> Product(101, "Apple", 10, 100.0),
    102 -> Product(102, "Banana", 20, 200.0),
    103 -> Product(103, "Orange", 30, 250.0),
    104 -> Product(104, "Grapes", 40, 180.0),
    105 -> Product(105, "Mango", 50, 150.0)
  )
  val inventory2:Map[Int, Product] = Map(
    105 -> Product(105, "Mango", 60, 120.0),
    106 -> Product(106, "Papaya", 70, 130.0),
    104 -> Product(104, "Grapes", 80, 200.0),
    109 -> Product(109, "Kiwi", 90, 160.0)
    )
    def printProductNames(inventory: Map[Int, Product]): Unit = {
      inventory.foreach{
        case (id, product) => println(product.name)}
    }
    def caltTotalValues(inventory: Map[Int, Product]): Double = {
        var tot: Double = 0.0
        inventory.foreach{
          case (id, product) => tot+=product.price*product.quantity
        }
        tot
    }
    def checkEmpty(inventory: Map[Int, Product]): Boolean = {
      inventory.isEmpty
    }
    def isEmpty(inventory: Map[Int, Product]): Unit = {
      print("Is Inventory Empty: ")
      checkEmpty(inventory) match {
        case true => println("Yes, Inventory is Empty")
        case false => println("No,Inventory is not Empty")
      }
    }

    def mergeInventories(inventory1: Map[Int, Product], inventory2: Map[Int, Product]): Map[Int, Product] = {
      (inventory1.keySet ++ inventory2.keySet).map{ id =>
        val product1 = inventory1.get(id)
        val product2 = inventory2.get(id)
        val mergedProduct=(product1, product2) match {
          case (Some(p1), Some(p2)) => Product(p1.id, p1.name, p1.quantity+p2.quantity,math.max(p1.price, p2.price))
          case (Some(p1), None) => p1
          case (None, Some(p2)) => p2
          case _ => throw new IllegalStateException("Impossible")
        }
        id->mergedProduct
      }.toMap
    }
    def printProducts(inventory: Map[Int, Product]): Unit = {
      inventory.foreach{
        case (id, product) => println(id+" "+product.name+" "+product.quantity+" "+product.price)}
    }

    def checkAvailability(inventory: Map[Int, Product], id: Int): Boolean = {
      inventory.contains(id)
    }
    def showDetails(inventory: Map[Int, Product], id: Int): Unit = {
      checkAvailability(inventory, id) match {
        case true => {val foundedProduct=inventory(id);println(foundedProduct.id+" "+foundedProduct.name+" "+foundedProduct.quantity+" "+foundedProduct.price)}
        case _ => println("Product Not Found")
      }
    }

    def main(args: Array[String]): Unit = {
      println("Items in Inventory 1 : ")
      printProductNames(inventory1)
      println("\nProduct Values in Inventory 1: " + caltTotalValues(inventory1))
      isEmpty(inventory1)      
      val mergedInventory = mergeInventories(inventory1, inventory2)
      println("\nMerged Inventory: ")
      printProducts(mergedInventory)

      showDetails(inventory1, readLine("Enter Product ID to check Details in Inventory 1: ").toInt);
      
    }
}





