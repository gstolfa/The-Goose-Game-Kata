import scala.collection.mutable.HashMap

val players:HashMap[String, Int] = new HashMap
val scanner = new java.util.Scanner(System.in)
val goose_numbers = List(5, 9, 14, 18, 23, 27)
val command = scanner.nextLine()

executeCommand(command)

/** 
 *  Recognizes and executes the user's command.
 */
def executeCommand(command: String){
  if(("^move.*\\d,\\s*\\d$".r findFirstIn command) != None )
    moveWithNumber(command)
  else if(("^move .*$".r findFirstIn command) != None){
    movePlayerPlayngDice("""\s([a-zA-Z_]+)""".r.findAllIn(command).matchData.map(_.group(1)).toList(0))
  }
  else if(("^add player .*$".r findFirstIn command) != None)
    addPlayer(command.substring(11, command.length))
  else if(("^hello$".r findFirstIn command) != None)  
    println("Hello see you later!!!")
  else{
    println(s"Unrecognized command: $command")
    val newCommand = scanner.nextLine()
    executeCommand(newCommand)
  }	
}

/** 
 *  Return two random number tuple.
 */
def rollDice():(Int,Int)={
  val random = new scala.util.Random
  val dice_1 = 1 + random.nextInt(6)
  val dice_2 = 1 + random.nextInt(6)
  (dice_1, dice_2)
}

/** 
 *  Recognizes and executes the user's command.
 */
def movePlayerPlayngDice(name: String){		
  if(!isParticipantPlayer(name)) players += (name -> 0)
  val numbers = rollDice()
  move(numbers._1, numbers._2, name)
}

/** 
 *  Return true if name player is already in HashMap.
 */
def isParticipantPlayer(name: String):Boolean={
  var res = true
  try{
    players(name)      
  } 
  catch{
    case nsee: NoSuchElementException => res = false
  }    
  res
}

/** 
 *  Add player if his name is not already in players HashMap.
 */
def addPlayer(name: String){

  if(isParticipantPlayer(name)){
    println(s"$name: already existing player")	
  }
  else{
    players += (name -> 0)
    printPlayers()    	
  }

  val newCommand = scanner.nextLine()
  executeCommand(newCommand)    
}

/** 
 *  Print comma separated players list.
 */
def printPlayers()={
  var playersString = players.head._1
  players foreach (x => if(playersString != x._1) playersString += f", ${x._1}"  )
  println(s"players: $playersString")	
} 

/** 
 *  Move player with inputed dice number by the user.
 */
def moveWithNumber(command:String){

  val name = """\s([a-zA-Z_]+)\s*\d""".r.findAllIn(command).matchData.map(_.group(1)).toList(0)

  if(!isParticipantPlayer(name)) players += (name -> 0)

  val numbers = "\\d,\\s*\\d$".r findFirstIn command match {
                 case None => ""
                 case Some(s) => s 
                }

  val a = numbers.split(",")(0).trim.toInt  
  val b = numbers.split(",")(1).trim.toInt

  if(a < 1 || a > 6 || b < 1 || b > 6){
  	println("Dice number must be between 1 and 6. Move with dice ...")
  	movePlayerPlayngDice(name)
  }
  else{
    move(a, b, name)	
  }
}

/** 
 *  Replace "0" position to "Start".
 */
def convertZeroToStart(c:Int) : String = if(c == 0) "Start" else c.toString

/** 
 *  Move player with all rules.
 */
def move(dice_1: Int, dice_2: Int, player_name: String):Unit = {

  val current_position = players(player_name)
  
  var new_position = dice_1 + dice_2 + current_position

  if(current_position == 62) new_position = 63
  	
  // Victory	
  if(new_position == 63){  	
  	println(s"$player_name rolls $dice_1, $dice_2. $player_name moves from $current_position to $new_position. $player_name Wins!!")  	
  }
  // If you land on "The Goose", move again
  else if(goose_numbers.contains(new_position)){
    var textToPrint = s"$player_name rolls $dice_1, $dice_2. $player_name moves from $current_position to $new_position, The Goose."	  	
      while(goose_numbers.contains(new_position)){
        new_position = new_position + dice_1 + dice_2
        if(goose_numbers.contains(new_position)){
          textToPrint += s" $player_name moves again and goes to $new_position, The Goose."
        }
        else{ 
          textToPrint += s" $player_name moves again and goes to $new_position"	
        }
     }		
     executeMove(player_name, new_position, current_position, textToPrint)
  }  
  // Winning with the exact dice shooting
  else if(new_position > 63){
  	executeMove(player_name, (current_position + 1), current_position, f"$player_name rolls $dice_1, $dice_2. $player_name moves from $current_position to $new_position. $player_name bounces! $player_name returns to ${players(player_name)}")
  }
  // Get to "The Bridge"
  else if(new_position == 6){
  	executeMove(player_name, 12, current_position, s"$player_name rolls $dice_1, $dice_2. $player_name moves from $current_position to The Bridge. $player_name jumps to 12")
  }
  // Start
  else{
  	executeMove(player_name, new_position, current_position, f"$player_name rolls $dice_1, $dice_2. $player_name moves from ${convertZeroToStart(current_position)} to ${dice_1+dice_2+current_position}")
  }
}

/** 
 *  Move player.
 */
def executeMove(player_name: String, new_position : Int, current_position:Int, text_to_print: String):Unit={  
  
  var text_to_add = ""  
  val player_to_move = players.find(_._2==new_position)

  player_to_move match {
    case Some(i) => {
      players += ( i._1 -> current_position )
      text_to_add = s". On $new_position there is ${i._1}, who returns to $current_position"
    }
    case None => 
  }	

  players += ( player_name -> new_position )
  println(text_to_print + text_to_add)

  // da togliere
  //players.foreach(println)

  val command = scanner.nextLine()
  executeCommand(command)
}