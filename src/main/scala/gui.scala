import processing.core._
import processing.core.PConstants._

class Applet extends PApplet {
  applet =>
    
  val game = new Game

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
  
  override def setup() {
    size(300, 400, JAVA2D)

    textFont(createFont("", 40))
    
    smooth()
    
    noLoop()
//    frameRate(24)

    game.setup()
  }

  def alignX(index: Int, size: Int, margin: Int): Int = {
    val center = (width >> 1) - (size >> 1)
    val diff   = ((index % 3) - 1) * margin
    center + diff
  }

  def boardX(index: Int) = alignX(index, size = boardSize, margin = 80)
  def boardY(index: Int) = 90 + (index / 3) * 60
  val boardSize = 50
  
  def containsBoardDicesWithMouse: Option[Dice] = {
    game.board.dices.zipWithIndex.find {
      case (dice, index) =>
      val x = boardX(index)
      val y = boardY(index)
      val s = boardSize
      
      (new java.awt.Rectangle(x, y, s, s)).contains(mouseX, mouseY)
    } map {
      case (dice, _) =>
      dice
    }
  }
    
  override def draw() {    
    background(255)

    game.board.dices.zipWithIndex foreach {
      case (dice, index) =>              
        
      val x = boardX(index)
      val y = boardY(index)
      
      image(diceImages(dice.number), x, y, boardSize, boardSize)
    }
    
    game.stock.dices.zipWithIndex foreach {
      case (dice, index) =>

      val s = 64
      val x = alignX(index = index, size = s, margin = s - 2)
      
      val y = 290
      image(diceImages(dice.number), x, y, s, s)
    }

    fill(0)
    textSize(15)
    textAlign(CORNER)
    text("ハイスコア：" + game.highScore, 10, 20)    
    text("スコア：" + game.score, 10, 40)
    text("残サイコロ x " + game.talon.dices.size, 10, 60)
    text("リトライ: R キー", 10, 390)

    if (game.isFinished) {
      noStroke()      
      fill(255, 255, 255, 200)
      rectMode(CENTER)
      rect(width >> 1, (height >> 1) - 10, width, 80)

      textSize(20)
      textAlign(CENTER)
      fill(255, 64, 64)
      text("ゲームオーバー", width >> 1, (height >> 1) - 15)
      text("クリックでリトライ", width >> 1, (height >> 1) + 15)
    }
  }

  override def mousePressed() {
    if (game.isFinished) {
      game.setup()

      redraw()
    } else {          
      containsBoardDicesWithMouse foreach {
        dice =>
          
        game call dice
        
        game.deal()

        game.checkFinish()
        
        redraw()
      }
    }
  }

  override def keyPressed() {
    if (key == 'r' || key == 'R') {
      game.setup()
      redraw()
    }
  }  
}

object Application extends Applet {
  override def setup() {
    super.setup()
    frame.setTitle("ダイススレイヤー")
  }
  
  def main(args: Array[String]) = runSketch()
}
