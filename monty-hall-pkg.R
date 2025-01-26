#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Contestant selects a door
#' @description
#' 'select_door()' creates a function for the contestant to randomly chose one
#' of the three doors. 
#' @details
#' The 'create_game()' function generates the game by assigning two doors with
#' goats and one door with the car. The next step is for the contestant to 
#' randomly select one of the doors - they will not know if they got a goat or
#' car.
#' @param 
#' There are no arguments used in this function. 
#' @return 
#' The contestant has the chose of 3 doors and can only choose 1 of the doors.
#' @examples
#' Per the select_door(), contestant can choose a door from 1-3. 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host Opens a Door with a Goat. 
#' @description
#' The host will always open a door with a goat behind it and cannot open a door
#' that the contestant chose.Therefore, if the contestant chose a door with a 
#' goat, the host will open the second goat door. However, if the contestant
#' chose the door with the car, then the host would have two 'goat' doors to 
#' choose from.
#' @details
#' The statements listed inside the compound statement { }, ensure that the
#' host has either one or two doors to open. It also ensures that the host does
#' not open a 'car' door. 
#' @param 
#' The parameters included in the function are 'game' and 'a.pick'.This ensures
#' that the function runs 'this' particular game plus the contestant's first
#' choice door. 
#' @return 
#' The return will open a goat door.
#' @examples
#' the open_goat_door will always open a goat door depending on the numbers
#' assigned to each door and based on the contestant's first choice.
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Option to Change Doors
#' @description
#' 'change_door()' gives the contestant the option to either stay with their 
#' first choice or switch doors.
#' @details
#' If the contestant chooses to stay (stay=T), then their final pick will be the
#' same as their first picl; otherwise, the contestant chooses to switch doors,
#' their final decision will be a different door number.
#' @param 
#' The function is using the whether the contestant chose to stay, the host door,
#' and the first pick as the parameters. 
#' @return 
#' The return will either have the same door as the first initial pick or a new
#' door number.
#' @examples
#' If a goat door = 1 and I chose to 'stay' - the return value would be '1'.
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine if the Contestant Won
#' @description
#' 'determine_winner()' will display whether or not the contestant won or lost,
#' based on their final door pick.
#' @details
#' If contestant's final choice was the 'car' door, this will show WIN. If the
#' contestant's final choice was a 'goat' door, it will show LOSE.
#' @param 
#' This is based on the contestant's final pick and the particulr game's doors.
#' @return 
#' It will return a value of either "WIN" or "LOSE"
#' @examples
#' Sticking with my example above, if I chose to "STAY" with Door 1 = GOAT, my 
#' result would show "LOSE". 
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Simulation Set-up
#' @description
#' This code chunk will execute all of the previous functions of the game in
#' order. 
#' @details
#' By having a "game recipe", we will not need to individually run each function
#' every time we want to run a new game.
#' @param 
#' No parameters in this function.
#' @return 
#' It will return the results of the game based on the results of the
#' particular game values.
#' @examples
#' If for this game, door 3 is the 'car' door and I chose door 2 = goat. The
#' host will open door 1 = goat. From here, I chose to "SWITCH", indicating that
#' my final choice is door 1. I will receive the value of "LOSE". 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Adding a Game Loop 
#' @description
#' This code chunk indicates running the simulation 100 times to help with
#' result randomization. 
#' @details
#' The for() loop indicates that the game will run 100 separate times. From here,
#' each time that play_game() function is called, it will generate a new
#' 'game_outcome". 
#' @param 
#' In the play_n_games() the function indicates that the game will repeat 100
#' separate times.
#' @return 
#' The return value will give a percentage of the wins/losses of those 100 games
#' and also indicate the number of wins/losses if you choose to "STAY" or to
#' "SWITCH". 
#' @examples
#' For example, the results could show something similar to this:
# outcome
# strategy LOSE  WIN
# stay   0.80 0.20
# switch 0.77 0.23
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
