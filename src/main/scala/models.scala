import scala.util.control.Exception

class NotFoundDiceException(msg: Any = "") extends Exception(msg.toString)
class OutSideDiceNumberException(msg: Any = "") extends Exception(msg.toString)

trait Dice {
  val id: Int
  val maxNumber: Int
  
  private var _pack: Option[Pack] = None
  def pack = _pack
  def pack_=(pack: Option[Pack]) {
    val dice = this
    
    _pack foreach { _.dices -= dice }

    _pack = pack
    
    _pack foreach { _.dices += dice }
  }

  def isTalon = pack.map(_.isInstanceOf[Talon]).getOrElse(false)
  def isBoard = pack.map(_.isInstanceOf[Board]).getOrElse(false)
  def isStock = pack.map(_.isInstanceOf[Stock]).getOrElse(false)  
  
  private var _number = 0
  def number = _number
  def number_=(n: Int)  {
    if (n < 0 || n >= maxNumber) {
      throw new OutSideDiceNumberException("number: " + n)
    }
    
    _number = n
  }
  
  def shake(): this.type = {
    number = scala.util.Random.nextInt(maxNumber)
    this
  }
}

object CubeDice {
  val MaxNumber = 6
}

case class CubeDice(id: Int) extends Dice {  
  val maxNumber = CubeDice.MaxNumber
  override def toString = "CubeDice(%d, %d)".format(id, number)
}

// Deck だと Dice とかぶって紛らわしいので Pack にした。
// どうやらイギリスでは Deck を Pack というらしい。
sealed trait Pack {
  val dices = scala.collection.mutable.ArrayBuffer[Dice]()
  
  def send(that: Pack)(choice: => Dice): Option[Dice] = {    
    Exception.catching(classOf[NoSuchElementException]).opt(choice) map {
      dice =>
      dice.pack = Option(that)
      dice
    }
  }

  def numbers: List[Int] = dices.toList map { _.number }
}

class Talon extends Pack {
  talon =>
  
  def sendTo(board: Board): List[Dice] = {
    val size = if (board.dices.isEmpty) 3 else 2
    List.range(0, size) map {
      _ =>
      send(board) {
        talon.dices.head.shake()
      }
    } flatten
  }
}

class Board extends Pack {
  board =>

  def sendTo(board: Stock, choiceDice: Dice): Option[Dice] = {
    dices.find(_ == choiceDice) flatMap {
      send(board)(_)
    }
  }

  def sendTo(talon: Talon, dices: List[Dice]) {
    dices foreach {
      send(talon)(_)
    }
  }

  // 名前分かりづらい気がするなー
  def drop(numbers: List[Int]): List[Dice] = {
    dices.filter { numbers contains _.number } toList
  }
}

class Stock extends Pack {
  stock =>

  def sendTo(talon: Talon): Option[Dice] = {
    if (stock.dices.size > 3) {
      send(talon)(stock.dices.head)
    } else {
      None
    }
  }

  // 刻子
  def same: Option[List[Int]] = {
    if (dices.size != 3) return None
    
    val num = dices.head.number
    
    if (numbers == List(num, num, num)) Option(List(num)) else None
  }

  // 順子
  def order: Option[List[Int]] = {
    if (dices.size != 3) return None

    val sortNumbers = numbers.sortWith(_ < _)
    val center = sortNumbers(1)

    if (List(center - 1, center, center + 1) == sortNumbers) {
      Option(List(center - 1, center + 1))
    } else {
      None
    }
  }
  
  def success: Option[List[Int]] = {
    def side(nums: List[Int]) = {
      val min = nums.min - 1
      val max = nums.max + 1
      
      List(min, max).distinct filter {
        num =>
        num >= 0 && num < CubeDice.MaxNumber
      }
    }
    
    // ここまぁまぁカッコイイ感がある（きりっ
    (same map side) orElse (order map side)
  }
}

class Game {
  // うーむ。。。ふらぐェ。。。
  var isFinished = false
  
  var highScore = 0
  var score = 0
  
  val talon = new Talon
  val board = new Board
  val stock = new Stock
  val dices: List[Dice] = (1 to 13) map { CubeDice(_) } toList
  def packs: List[Pack] = List(talon, board, stock)

  def setup() {
    isFinished = false
    
    score = 0
    
    packs foreach { _.dices.clear }
    
    dices foreach { _.pack = Option(talon) }
  }

  def deal(): List[Dice] = talon sendTo board
  
  def call(dice: Dice): Option[Dice] = {
    // こう出来たらかっちょいいと思いました（きり
    //board send dice to stock
    board.sendTo(stock, dice)

    stock sendTo talon
  }

  def drop(): List[Dice] = {
    // この文面だけでコードの方みないで何やってるか分かるだろうか？
    // 「ストックが成功ならボードのドロップに変換して回す（きり」
    // 英語力 と scala力 くだしあ；ω；    
    stock.success.map(board.drop) map {
      dices =>
      board.sendTo(talon, dices)
      score += dices.size * dices.size
      if (board.dices.isEmpty) score += 20
      if (score > highScore) highScore = score
      dices
    } getOrElse(List.empty[Dice])
  }

  def checkFinish() {
    if (board.dices.size >= 9) isFinished = true
  }
}
