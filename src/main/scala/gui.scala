import processing.core._
import processing.core.PConstants._

class Applet extends PApplet {
  implicit val applet = this

  implicit def pair2PVector(p: Pair[Int, Int]) = new PVector(p._1, p._2)
  
  val game = new Game

  val backgroundImage = loadImage("background.png")
  val diceImages: List[PGraphics] = (1 to CubeDice.MaxNumber) map {
    i =>
    val size = 64
    val g = createGraphics(size, size, JAVA2D)

    g.beginDraw()

    g.smooth()
    g.strokeWeight(5)
    
    g.stroke(0)
    g.fill(255)
    g.rect(0, 0, size - 1, size -1)

    g.textFont(createFont("", 20))
    g.textAlign(CENTER)
    g.fill(0)
    val des = g.textDescent.toInt
    g.text(i, size >> 1, (size >> 1) + des + (des >> 1))
    
    g.endDraw()
    
    g
  } toList  
  
  //
  // TK: DiceSprite を ArrayBuffer にして可変で作る！！
  //
  case class DiceSprite(dice: Dice) extends java.awt.Rectangle {
    // こんなものは消す！！！！！
    def init() {
      isDisplay = false
      isActive  = false
      arriveAction = None      
    }
    
    var isDisplay = false

    var isActive = false
    var speed  = 0.0f

    var arriveAction: Option[() => Unit] = None
    def arriveAction(action: => Unit) {
      arriveAction = Option(action _)
    }

    private var vec = new PVector()    
    private var _target = new PVector()
    def target = _target
    def target_=(target: PVector) {
      _target = target      
      isActive = true
      
      vec = target.get()
      vec.sub(new PVector(x, y))
      vec.normalize()
    }

    override def contains(x: Int, y: Int) = {
      super.contains(x + (width >> 1), y + (height >> 1))
    }
  
    def draw() = if (isDisplay) image(diceImages(dice.number), x, y, width, height)
    
    def move() {
      if (!isActive) return
      
      if (target.dist(new PVector(x, y)) <= speed) {
        isActive = false        
        x = target.x.toInt
        y = target.y.toInt

        arriveAction foreach { _() }
        arriveAction = None
      } else {
        x += (vec.x * speed).toInt
        y += (vec.y * speed).toInt
      }
    }
  }
  class DiceSpriteListWrapper(xs: List[DiceSprite]) {
    def apply(dice: Dice) = xs find { _.dice == dice }
    def filterBy(pack: Pack) = xs filter { pack.dices contains _.dice }
  }
  implicit def DiceSpriteList2Wrapper(xs: List[DiceSprite]) = {
    new DiceSpriteListWrapper(xs)
  }

  def containsBoardSpriteWithMouse: Option[DiceSprite] = diceSprites find {
    sprite =>
    sprite.dice.isBoard && sprite.contains(mouseX, mouseY)
  }

  //
  // TK: DiceSprite を ArrayBuffer にして可変で作る！！
  // 
  val diceSprites: List[DiceSprite] = game.dices.map { DiceSprite(_) }

  def startGame() {
    game.setup()

    diceSprites foreach { _.init() }
    
    deal()
  }
  
  def deal() {
    def getX(index: Int) = alignX(index = index, margin = 80)
    def getY(index: Int) = 100 + (index / 3) * 60
    def getIndex(dice: Dice) = game.board.dices.indexOf(dice)
    
    game.deal.foreach {
      case dice =>
      diceSprites(dice) foreach {
        s =>
        s.init()
        
        val index = getIndex(dice)

        s.x = getX(index) + (width + 30)
        s.y = getY(index)
                
        s.speed  = 30.0f
        s.width  = 50
        s.height = 50
        s.isDisplay = true
      }      
    }

    diceSprites.filter(_.dice.isBoard) foreach {
      s =>
      val index = getIndex(s.dice)
      s.target = (getX(index), getY(index))
    }
  }
    
  override def setup() {
    size(300, 400, JAVA2D)

    textFont(createFont("", 40))
    
    smooth()
    
    frameRate(24)

    startGame()
  }

  def alignX(index: Int, margin: Int): Int = {
    (width >> 1) + ((index % 3) - 1) * margin
  }
      
  override def draw() {    
    background(backgroundImage)

    imageMode(CENTER)
    diceSprites foreach {
      sprite =>        
      sprite.draw()
      sprite.move()
    }

    fill(255)
    textSize(15)
    textAlign(CORNER)
    text("ハイスコア：" + game.highScore, 10, 20)    
    text("スコア：" + game.score, 10, 40)
    text("残サイコロ x " + game.talon.dices.size, 10, 60)
    text("リトライ: R キー", 10, 390)

    if (game.isFinished) {
      noStroke()      
      fill(0, 0, 0, 200)
      rectMode(CENTER)
      rect(width >> 1, (height >> 1) - 10, width, 80)

      textSize(20)
      textAlign(CENTER)
      fill(255)
      text("ゲームオーバー", width >> 1, (height >> 1) - 15)
      text("クリックでリトライ", width >> 1, (height >> 1) + 15)
    }

    containsBoardSpriteWithMouse foreach {
      s =>
      rectMode(CENTER)
      fill(255, 221, 75, 80)
      rect(s.x, s.y, s.width - 2, s.height - 2)
    }
  }

  override def mousePressed() {
    if (game.isFinished) {
      startGame()
    } else {
      containsBoardSpriteWithMouse foreach {
        clickedSprite =>

        def dropDice(dice: Dice) {
          diceSprites(dice) foreach {
            sprite =>
            sprite.arriveAction { sprite.isDisplay = false }
            sprite.target = (-sprite.width, sprite.y)
          }
        }
        
        (game call clickedSprite.dice) foreach { dropDice }
        
        clickedSprite.width  = 64
        clickedSprite.height = 64

        diceSprites.filter(_.dice.isStock) foreach {
          sprite =>
          val index = game.stock.dices.indexOf(sprite.dice)
          val x = alignX(index = index, margin = sprite.width - 2)
          sprite.target = (x, 340)
        }

        game.drop foreach { dropDice }

        deal()

        game.checkFinish()
      }
    }
  }

  override def keyPressed() {
    if (key == 'r' || key == 'R') startGame()
  }  
}

object Application extends Applet {
  override def setup() {
    super.setup()
    frame.setTitle("ダイススレイヤー")
  }
  
  def main(args: Array[String]) = runSketch()
}
