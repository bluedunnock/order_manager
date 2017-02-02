package lebara.interview


/**
  * Created by vumaasha on 2/2/17.
  */

/**
  * Scala test v1.
  *
  * We have the following simplistic data model representing an e-commerce business:
  *
  * A user has a name and a list of orders. All attributes are required.
  *
  * An order is comprised of a list of products along with associated quantities.
  *
  * A product has a name, price and category. All attributes are required, except for category.
  *
  * Develop a representation of this data model and implement the following methods in Scala.
  * Include any code you developed in support of your solution.
  */

trait ScalaInterview {

  // 1 - Given an order, produce a list of product names.
  def productNamesFromOrder(order: Order): List[String]

  // 2 - Given a user, produce a list of products from all of their orders.
  def productsFromUser(user: User): List[Product]

  // 3 - Given an order, calculate the total price.
  def orderTotal(order: Order): Double

  // 4 - Given a list of products and a category name, produce a list of products in that category.
  // Make sure the solution handles a possibly missing category on a product.
  def productsInCategory(products: List[Product], category: String): List[Product]

  // 5 - Given list of products, produce a map of category to a list of products in that category.
  // Make sure the solution handles a possibly missing category on a product.
  def productsByCategory(products: List[Product]): Map[String, List[Product]]
}

case class User(name: String, orders: Seq[Order])

case class Product(name: String, price: Double, category: Option[String])

case class Order(items: Seq[(Product, Int)])

object OrderManager extends ScalaInterview {
  // 1 - Given an order, produce a list of product names.
  override def productNamesFromOrder(order: Order): List[String] = {
    for {
      itemQty <- order.items
      item = itemQty._1
      quantity = itemQty._2
    } yield {
      item.name
    }
  }.toList

  // 2 - Given a user, produce a list of products from all of their orders.
  override def productsFromUser(user: User): List[Product] = {
    for {
      order <- user.orders
      itemQty <- order.items
      product = itemQty._1
    } yield product
  }.toList

  // 3 - Given an order, calculate the total price.
  override def orderTotal(order: Order): Double = {
    order.items.map(x => x._1.price * x._2).reduce(_ + _)
  }

  // 4 - Given a list of products and a category name, produce a list of products in that category.
  override def productsInCategory(products: List[Product], category: String): List[Product] = {
    for {
      product <- products
      productCategory <- product.category if productCategory.equals(category)
    } yield product
  }

  // 5 - Given list of products, produce a map of category to a list of products in that category.
  override def productsByCategory(products: List[Product]): Map[String, List[Product]] = {
    products.groupBy(_.category.getOrElse("None"))
  }
}
