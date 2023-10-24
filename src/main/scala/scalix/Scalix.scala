package scalix

import org.json4s.*
import org.json4s.native.JsonMethods.*

import java.util
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.Breaks.*

object Scalix extends App {

  implicit val formats: Formats = DefaultFormats

  private val key = Config.api_key
  /*
  private val url =  s"https://api.themoviedb.org/3/search/person?query=Jason+Mamoa&api_key=$key"
  private val source = Source.fromURL(url)
  private val contents = source.mkString
  println(s"Contents: $contents")
  private val json = parse(contents)
  println(s"Json: $json")
  */

  /*
  val id = findActorId("Tom","Cruise")
  val id2 = findActorId("Anne","Hathaway")

  val movies = findActorMovies(id2.getOrElse(0))

  val movieid = movies.head._1

  println(movieid)
  val director = findMovieDirector(movieid)

  println(director)

   */
  val col = collaboration(new FullName("Matt","Damon"), new FullName("Ben","Affleck"))
  println(col)

  def findActorId(name: String, surname: String): Option[Int] =
    val urlActor = s"https://api.themoviedb.org/3/search/person?query=$name+$surname&api_key=$key"
    val src = Source.fromURL(urlActor)
    val cont = src.mkString
    val parsed = parse(cont)
    val id = ((parsed \ "results" )(0) \ "id").extractOpt[Int]
    //println(s"Contents: $cont")
    id

  def findActorMovies(actorId: Int): Set[(Int, String)] =
    val urlMovies = s"https://api.themoviedb.org/3/person/$actorId/movie_credits?language=en-US&api_key=$key"
    val src = Source.fromURL(urlMovies)
    val cont = src.mkString
    val parsed = parse(cont)
    val movies: List[(Int,String)] = (parsed \ "cast").extract[List[Map[String,Any]]].map
      { e => (e("id"), e("title")) match {
        case (int : BigInt,string : String) => (int.toInt,string)
        case (i,s) => {println("Type error")
                        (0,"None")}
        }
      }
    //println(s"Contents: $cont")
    movies.toSet

  def findMovieDirector(movieId: Int): Option[(Int, String)] =
    var result: Option[(Int, String)] = None
    val urlDir = s"https://api.themoviedb.org/3/movie/$movieId/credits?language=en-US&api_key=$key"
    val src = Source.fromURL(urlDir)
    val cont = src.mkString
    val parsed = parse(cont)
    val crew = (parsed \ "crew" ).extract[List[Map[String,Any]]]
    breakable {
      for (c <- crew) {
        if (c("job") == "Director") {
          (c("id"), c("name")) match {
            case (int: BigInt, string: String) => result = Some((int.toInt, string))
            case _ => println("Type error")
          }
          break
        }
      }
    }
    //println(s"Contents: $cont")
    result

  case class FullName (name: String, surname: String)

  def collaboration(actor1: FullName, actor2: FullName): Set[(String, String)] =
    val list : ListBuffer[(String,String)] = ListBuffer.empty
    val actor1id = findActorId(actor1.name, actor1.surname)
    val movies1 = if (actor1id.isDefined) findActorMovies(actor1id.get) else Set.empty
    val actor2id = findActorId(actor2.name, actor2.surname)
    val movies2 = if (actor2id.isDefined) findActorMovies(actor2id.get) else Set.empty
    val movies = movies1.intersect(movies2)
    for ((movieId,movieName) <- movies) {
      findMovieDirector(movieId) match {
        case Some((_, directorName)) => list += ((directorName,movieName))
        case _ =>
      }
    }
    list.toSet
}
