package cw

/**
 * This class holds an instance of a simple game where 
 * a player moves on a field and collects bounties.
 * See the explanation sheet and comments in this file for details. The constructor builds an
 * instance of a game
 * 
 * @param wall A list of coordinates (as tuples) where walls exist. Example: The parameter List((0,0),(0,1)) puts two wall elements in the upper left corner and the position below.
 * @param bounty A list of bounties, each is a position and a function (i.e. a 3 value tuple). Example: List((0,0,(x: Int)=>x+1)) puts a bounty in the upper left corner which adds 1 to the score.
 * @param playerX The initial x position of the player.
 * @param playerY The initial y position of the player. If playerX and playerY are 0, the player starts in the upper left corner. As the player moves these positions update.
 */
class Game(wall: List[(Int, Int)], bounty: List[(Int,Int, Int=> Int)], var playerX: Int, var playerY: Int) {
  
  //the current grid, a 10x10 field, initially all cells are false (i.e. no walls)
  private var field: Array[Array[Boolean]] = Array.ofDim[Boolean](10, 10)
  
  //a separate grid holding bounties at relevant positions, initially all cells are set to null (i.e. no bounties)
  private var bounties: Array[Array[Int=>Int]] = Array.ofDim[Int=>Int](10, 10)
  
  /* Please note - to align with the overall case study (see explanation sheet), both of the above two-dimensional arrays 
   * should be accessed in the format field(col)(row) so field(2)(0) would retrieve the 3rd column and the 1st row (as indexing starts at zero), 
   * equivalent to an (x,y) coordinate of (2,0). You may therefore visualise each inner array as representing a column of data.
   */
  
  //the current score, initially 0
  private var score: Int = 0
  
  //the current X and Y save position, initially -1
  private var saveX: Int = -1
  private var saveY: Int = -1
  
  /* This code is executed as part of the constructor. 
   * It uses the list of walls provided to initialise the walls in the field array by setting given coordinates to true.
   * It then uses the list of bounties to initialise the corresponding array by setting given coordinates to the provided function.
   */
  wall.foreach(w => field(w._1)(w._2)=true)
  bounty.foreach(w => bounties(w._1)(w._2)=w._3)
  
 
  /**
   * Repeatedly run a sequence of commands. For example:
   *    for(i <- 1 to 5) println("Hello")
   * can be replaced by
   *    rpt(5)(println("Hello"))
   */
  def rpt (n: Int) ( commands: => Unit ) {
    for (i <- 1 to n) { commands }
  }
  
  /********************************************************************************
   * COURSEWORK STARTS HERE - COMPLETE THE DEFINITIONS OF EACH OF THE OPERATIONS
   * WE SUGGEST YOU RUN THE GameTest SUITE AFTER EVERY CHANGE YOU MAKE TO THESE
   * SO YOU CAN SEE PROGRESS AND CHECK THAT YOU'VE NOT BROKEN ANYTHING THAT USED
   * TO WORK.
   *******************************************************************************/
    
  
  /**
   * Returns the current position of the player as a tuple, in (x,y) order.
	 */
  def getPlayerPos(): (Int, Int) = {
    return (playerX,playerY);
  }
  
  
  /**
   * Updates saveX and saveY to the current player position.
   */
  def save(): Unit = {
    saveX = playerX
    saveY = playerY
  }
  
  /**
	 * Returns the current score.
 	 */
  def getScore(): Int = {
    return score
  }

  /**
	 * Returns the current save position as a tuple, in (x,y) order.
 	 */
  def getSavePos(): (Int, Int) =  {
    return (saveX, saveY);
  }

  /**
   * AL: Arrow Left.  Move the player one place to the left. If
   * there is a wall or the field ends, nothing happens. If there
   * is a bounty, it is collected. If 9 or more fields are covered 
   * from a saved position, the bounties in the rectangle are collected.
   */
  def al() {
    
    if(playerX - 1 >= 0 && field(playerX - 1)(playerY) == false){
      playerX = playerX - 1
      checkBounty();
      checkBounties();
    }
  }
  /**
   * AR: Arrow Right.  Move the player one place to the right. If
   * there is a wall or the field ends, nothing happens. If there
   * is a bounty, it is collected. If 9 or more fields are covered 
   * from a saved position, the bounties in the rectangle are collected.
   */
  def ar() {
    
    if(playerX + 1 <= 9 && field(playerX + 1)(playerY) == false) {
      playerX = playerX + 1
      checkBounty();
      checkBounties();
    }
  }
  
  /**
   * AU: Arrow Up.  Move the player one place up. If
   * there is a wall or the field ends, nothing happens. If there
   * is a bounty, it is collected. If 9 or more fields are covered 
   * from a saved position, the bounties in the rectangle are collected.
   */
  def au() {
 
    if(playerY - 1 >= 0 && field(playerX)(playerY - 1) == false) {
      playerY = playerY - 1
      checkBounty();
      checkBounties();
    }
  }
  
  /**
   * AD: Arrow Down.  Move the player one place down. If
   * there is a wall or the field ends, nothing happens. If there
   * is a bounty, it is collected. If 9 or more fields are covered 
   * from a saved position, the bounties in the rectangle are collected.
   */
  def ad() {
    
    if(playerY + 1 <= 9 && field(playerX)(playerY + 1) == false) {
      playerY = playerY + 1
      checkBounty();
      checkBounties();
    }
  }

  /**
   * AL: Arrow Left n.  Move the player n places to the left. If
   * there is a wall or the field ends, the player stops before 
   * the wall or end of field. Any bounties are collected and if 
   * 9 or more fields are covered from a saved position after an 
   * individual move, the bounties in the rectangle are collected.
   * Negative numbers or 0 as a parameter cause no effect.
   */
  def al(n: Int) {
	  rpt(n)(al)
	}
  
  /**
   * AR: Arrow Right n.  Move the player n places to the right. If
   * there is a wall or the field ends, the player stops before 
   * the wall or end of field. Any bounties are collected and if 
   * 9 or more fields are covered from a saved position after an 
   * individual move, the bounties in the rectangle are collected.
   * Negative numbers or 0 as a parameter cause no effect.
   */
  def ar(n: Int) {
	  rpt(n)(ar)
	}
  
  /**
   * AU: Arrow Up n.  Move the player n places to up. If
   * there is a wall or the field ends, the player stops before 
   * the wall or end of field. Any bounties are collected and if 
   * 9 or more fields are covered from a saved position after an 
   * individual move, the bounties in the rectangle are collected.
   * Negative numbers or 0 as a parameter cause no effect.
   */
  def au(n: Int) {
	  rpt(n)(au)
	  
  }

  /**
   * AD: Arrow Down n.  Move the player n places down. If
   * there is a wall or the field ends, the player stops before 
   * the wall or end of field. Any bounties are collected and if 
   * 9 or more fields are covered from a saved position after an 
   * individual move, the bounties in the rectangle are collected.
   * Negative numbers or 0 as a parameter cause no effect.
   */
  def ad(n: Int) {
	  rpt(n)(ad)
	  
  }
  
  /**
   * Checks if the current position is a bounty. A bounty exists if the cell is not
   * set to null. If a bounty does exist, increase the score, 
   * and then erase the bounty, i.e. set it back to null.
   */
  def checkBounty() {
    if(bounties (playerX)(playerY) != null){
      var bounty: Int => Int = bounties(playerX)(playerY)
      score = bounty(score)
      bounties(playerX)(playerY) = null
    }
  }
  
  //The methods beyond this point (aside to those in GameProducer which is a separate task) are more complex than those above.
  
  /**
   * This moves the player according to a string. The string can contain the 
   * letters l, r, u, d representing left, right, up, down moves.  If
   * there is a wall or the field ends, the individual move is not 
   * executed. Any further moves are done. Any bounties are collected and the
   * save position is evaluated.
   */
  
  def move(s: String) = s.map {
    
    case 'l' => al()
    case 'r' => ar()
    case 'u' => au()
    case 'd' => ad()
    case  _  => "error"
    
  }

  /**
   * Identifies the maximum overall bounty in the game. This is the sum 
   * of the current score and the possible score from applying all of the remaining bounties.
   * No bounties are collected here, only the max score is returned.
   */
  def maxBounty(): Int = {
    var maxValue: Int = score
    for(a <- 0 to 9; b <- 0 to 9){
      
      if(bounties(a)(b) != null){
        maxValue =  bounties (a)(b)(maxValue)
      }
   }
  
    return maxValue
  }
  
  /**
   * Checks if the rectangle defined by the current position and saved position 
   * covers nine or more positions. If yes, it collects bounties in it, increases the 
   * score, and erases the bounties.
   */
  def checkBounties() {
    
     if(((playerX - saveX)+1).abs * ((playerY - saveY) + 1).abs >= 9){
       
      for(x <- playerX to saveX; y <- playerY to saveY){
        
        checkBounty();
         
      }
       
      saveX = -1
      saveY = -1
    }
  }
  

  /**
   * This gives a string in the format for move, which collects the maximum bounty. No specific
   * requirements for the efficiency of the solution exist, but the solution must consist of a finite number 
   * of steps. The move is combined of a number of moves
   * given by suggestMove. If these are not possible, an empty string is returned. No bounties are collected 
   * and the player must be at the original position after the execution of the method.
   */
  def suggestSolution(): String = {
    ""
  }
  
  /**
   * This gives a string in the format for move, which moves from the current position to 
   * position x,y. No specific requirements for the efficiency of the solution exist. The move
   * cannot jump walls. The method is restricted to finding a path which is combined of a number of 
   * left and then a number of up movement, or left/down, or right/up, or right/down movements only.
   * If this is not possible due to walls, it returns an empty string. No actual move is done. If 
   * x or y are outside the field, an empty string is returned as well.
   */
  def suggestMove(x: Int, y: Int): String = {
    ""
  }
 
  /* This method is already implemented. You should not change it */
  /**
	 * Sets the savePos to the values of the parameters. This method is
	 * for testing only. Normally, save() is used for this purpose.
 	 */
  def setSavePos(saveX: Int, saveY: Int): Unit =  {
    this.saveX=saveX
    this.saveY=saveY
  }

}

/**
 * This object returns a standard instance of Game
 *
 */
object GameProducer{
  
    
  /**
 	 * @return A game with 
 	 * - walls in positions 3,0 3,1 and 3,2
 	 * - a bounty at 4,1 which increases score by 5
 	 * - a bounty at 3,3 which increases score by 10
 	 * - the player in position 0,0
	 */
  def initialiseTest1(): Game = {
    return new Game(List((3,0),(3,1),(3,2)), List((4,1,(x: Int) => x+5), (3,3,(x: Int) => x+10)), 0, 0)
  }

  /**
 	 * @return A game with 
 	 * - walls in positions 3,3 3,4 3,5 5,3 5,4 and 5,5
 	 * - a bounty at 4,4 which increases score by 1
 	 * - a bounty at 6,3 which increases score by 1 if the current score is 0 or else increases the score by 3
 	 * - the player in position 3,2
	 */
  def initialiseTest2(): Game = {
    return new Game(List((3,3),(3,4),(3,5),(5,3),(5,4),(5,5)), List((4,4,(x: Int) => x+1), (6,3, (x: Int) => if(x == 0){  x+1 }else { x+3 })), 3,2)
  }

  /**
 	 * @return A game with 
 	 * - walls in positions 3,0 3,1 and 3,2
 	 * - a bounty at 4,1 which increases score by 5
 	 * - a bounty at 3,3 which increases score by 10
 	 * - the player in position 4,1
	 */
  def initialiseTest3(): Game = {
    return new Game(List((3,0),(3,1),(3,2)), List((4,1,(x: Int) => x+5),(3,3,(x: Int) => x+10)), 4, 1)
  }
}