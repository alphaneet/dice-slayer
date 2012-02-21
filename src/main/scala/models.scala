import scala.util.control.Exception

class NotFoundDiceException(msg: Any = "") extends Exception(msg.toString)
class OutSideDiceNumberException(msg: Any = "") extends Exception(msg.toString)

trait Dice {
  val id: Int
  val maxNumber: Int
  
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
trait Pack {
  val dices = scala.collection.mutable.LinkedHashSet[Dice]()
  
  def send(that: Pack)(choice: => Dice) {    
    Exception.catching(classOf[NoSuchElementException]).opt(choice) foreach {
      dice =>
      this.dices -= dice    
      that.dices += dice
    }
  }

  def numbers: List[Int] = dices.toList map { _.number }
}

class Talon extends Pack {
  talon =>
  
  def sendTo(board: Board) {
    val size = if (board.dices.isEmpty) 3 else 2
    (1 to size) foreach {
      _ =>
      send(board) {
        talon.dices.head.shake()
      }
    }
  }
}

class Board extends Pack {
  board =>

  def sendTo(board: Stock, choiceDice: Dice) {
    dices.find(_ == choiceDice) foreach {
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

  def sendTo(talon: Talon) {
    if (stock.dices.size > 3) {
      send(talon)(stock.dices.head)
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
  def packs: List[Pack] = List(talon, board, stock)

  def setup() {
    isFinished = false
    score = 0
    
    packs foreach {
      _.dices.clear
    }
    
    (1 to 12) foreach {
      talon.dices += CubeDice(_)
    }

    talon sendTo board
  }

  def deal() {
    talon sendTo board    
  }
  
  def call(dice: Dice) {
    // こう出来たらかっちょいいと思いました（きり
    //board send dice to stock
    board.sendTo(stock, dice)

    stock sendTo talon

    // この文面だけでコードの方みないで何やってるか分かるだろうか？
    // 「ストックが成功ならボードのドロップに変換して回す（きり」
    // 英語力 と scala力 くだしあ；ω；    
    stock.success.map(board.drop) foreach {
      dices =>
      board.sendTo(talon, dices)
      score += dices.size * dices.size
      if (board.dices.isEmpty) score += 20
      if (score > highScore) highScore = score       
    }
  }

  def checkFinish() {
    if (board.dices.size >= 9) isFinished = true
  }
}