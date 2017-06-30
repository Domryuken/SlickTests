import slick.driver.MySQLDriver.api._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends App{

  val db = Database.forConfig("mysqlDB")
  val peopleTable = TableQuery[People]
  val dropPeopleCmd = DBIO.seq(peopleTable.schema.drop)
  val initPeopleCmd = DBIO.seq(peopleTable.schema.create)

  dropDB



  def dropDB = {
    val dropFuture = Future{db.run(dropPeopleCmd)}

    Await.result(dropFuture, Duration.Inf).andThen{
      case Success(_) => initialisePeople
      case Failure(error) => println("dropping tables is hard " + error.getMessage)
        initialisePeople
    }
  }

  def initialisePeople = {
    val setupFuture = Future{
      db.run(initPeopleCmd)
    }
    Await.result(setupFuture,Duration.Inf).andThen{
      case Success(_) => runQuery
      case Failure(error) =>
        println("Initialising is hard " + error.getMessage)
    }
  }

  def runQuery ={
    val insertPeople = Future{
      val query = peopleTable ++= Seq(
        (10,"Jack","Wood",36),
        (20,"Tim","Brown",24)
      )
      println(query.statements.head)
      db.run(query)
    }
    Await.result(insertPeople,Duration.Inf).andThen {
      case Success(_) => updatePeople
      case Failure(error) => println("Doing stuff is hard " + error.getMessage)
    }
  }

  def updatePeople = {
    val updateFuture = Future{
      val person = for{p <- peopleTable if p.id === 1} yield p.fName
      val updateAction = person.update("Dumb Name")
      db.run(updateAction)
    }
    Await.result(updateFuture,Duration.Inf).andThen {
      case Success(_) => listPeople
      case Failure(error) => println("updateing is hard " + error.getMessage)
    }
  }


  def listPeople = {
    val queryFuture = Future{
      db.run(peopleTable.result).map(_.foreach {
        case(id,fName, lName, age) => println(s" $id $fName $lName $age")
      })
    }
    Await.result(queryFuture,Duration.Inf).andThen{
      case Success(_) => db.close()
      case Failure(error) =>
        println("Listing is hard " + error.getMessage)
    }
  }


  class People(tag: Tag) extends Table[(Int, String, String, Int)](tag, "PEOPLE") {
    def id = column[Int]("PER_ID", O.PrimaryKey, O.AutoInc)
    def fName = column[String]("PER_FNAME")
    def lName = column[String]("PER_LNAME")
    def age = column[Int]("PER_AGE")
    def * = (id, fName, lName, age)
  }

}
