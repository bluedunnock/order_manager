package lebara.interview

import org.scalatest.{Entry, FlatSpec}
import org.scalatest.Matchers._

/**
  * Created by vumaasha on 2/2/17.
  */
class OrderManagerSpec extends FlatSpec {

  val tv = Product("Television", 100.0, Some("electronics"))
  val computer = Product("Computer", 200.0, Some("computer"))
  val shoe = Product("Shoe", 50.0, Some("shoes"))
  val jeans = Product("jeans", 10.0, Some("clothing"))
  val skirt = Product("skirt", 30.0, Some("clothing"))

  private val products: Seq[(Product, Int)] = (tv, 1) :: (computer, 2) :: (shoe, 3) :: (jeans, 4) :: (skirt, 5) :: Nil
  val testOrder = Order(products)
  val testUser = User("tester", testOrder :: Nil)
  val productNames = testOrder.items.map(_._1.name)
  val productList = testOrder.items.map(_._1).toList

  "product names" should "match" in {
    val names = OrderManager.productNamesFromOrder(testOrder)
    names should equal(productNames)
  }

  it should "get all user products" in {
    OrderManager.productsFromUser(testUser) should equal(productList)
  }

  it should "compute total price correctly" in {
    OrderManager.orderTotal(testOrder) equals (840.0)
  }

  it should "filter categories correctly" in {
    OrderManager.productsInCategory(productList, "clothing").size equals (2)
  }

  it should "match category sizes" in {
    OrderManager.productsByCategory(productList).map(x => (x._1, x._2.size)) should contain("shoes" -> 1)
  }

}
