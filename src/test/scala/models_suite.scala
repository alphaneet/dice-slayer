import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class DiceSuite extends FunSuite with ShouldMatchers {
  test("#number の値は 0 <-> (maxNumber - 1) の間の値である") {
    val dice = new Dice { val id = 1; val maxNumber = 6 }
    
    dice.number = 0 
    dice.number = 5 
    evaluating { dice.number = 6 } should produce [OutSideDiceNumberException]
  }  
}

class PackSuite extends FunSuite with ShouldMatchers {
  test("#numbers は dices の number を List にしたもの") {
    val pack = new Pack {
      List(0, 2, 4, 5).zipWithIndex foreach {
        case (num, id) =>
        dices += new CubeDice(id) { number = num }
      }
    }

    pack.numbers should be (List(0, 2, 4, 5))
  }
  
}

class BoardSuite extends FunSuite with ShouldMatchers {
  test("#drop は引数と同じ Dice#number の dice のリストを返す") {
    trait Fixture {
      def create(id: Int, num: Int): Dice = new CubeDice(id) { number = num }

      val a = create(1, 0)
      val b = create(2, 0)
      val c = create(3, 1)
      val d = create(4, 1)
      val e = create(5, 1)
      val f = create(6, 2)
      val g = create(7, 3)
      val h = create(8, 4)
      
      val dices = List(a, b, c, d, e, f, g, h)
      val board = new Board
      dices foreach { board.dices += _ }
    }

    new Fixture {
      board.drop( List(0) )       should be ( List(a, b) )
      board.drop( List(2, 3, 4) ) should be ( List(f, g, h) ) 
      board.drop( List(0, 1) )    should be ( List(a, b, c, d, e))
    }    
  }
}

class StockSuite extends FunSuite with ShouldMatchers {

  def create(a: Int, b: Int, c: Int): Stock = {
    new Stock {
      List(a, b, c).zipWithIndex foreach {
        case (num, id) =>
        dices += new CubeDice(id) { number = num }
      }
    }
  }  

  test("#same が刻子ならば、Option(List(数値)) を返す。") {
    (new Stock).same     should be (None)
    create(1, 2, 1).same should be (None)
    create(3, 3, 3).same should be (Option(List(3)))
  }

  test("#order が順子ならば、Option(List(最小値, 最大値)) を返す。") {
    (new Stock).order should be (None)
    create(5, 0, 1).order should be (None)
    create(0, 1, 2).order should be (Option(List(0, 2)))    
    create(4, 2, 3).order should be (Option(List(2, 4)))    
  }

  test("#success は刻子か順子の時その隣りの数を返す") {
    (new Stock).success should be (None)
    create(2, 2, 4).success should be (None)
    create(1, 3, 5).success should be (None)

    create(1, 1, 1).success should be (Option(List(0, 2)))
    
    create(1, 2, 3).success should be (Option(List(0, 4)))
    create(2, 3, 4).success should be (Option(List(1, 5)))
  }

  test("#success の消す判定の最小値 <-> 最大値は繋がってない") {
    create(0, 0, 0).success should be (Option(List(1)))    
    create(5, 5, 5).success should be (Option(List(4)))    
    
    create(0, 1, 2).success should be (Option(List(3)))
    create(3, 4, 5).success should be (Option(List(2)))
  }
}
