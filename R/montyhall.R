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
#' Contestant selects a door.
#' @description
#' select_door is a function that select a door at random.
#' @details
#' The game consists of three doors and the contestant selects
#' their initial door.
#' @param
#' No arguments are used by the function.
#' @return
#' The function returns a number between 1 and 3.
#' @examples
#' select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host reveals a goat door to the contestant.
#' @description
#' open_goat_door is a function that reveals a door that contains a goat.
#' @details
#' The host will open a door with a goat. If the initial door selected
#' is a car either remaining goat will be opened at random. If a goat
#' door is initially selected the door the host will reveal the remaining
#' goat door.
#' @param
#' This function uses the return value, a.pick, from select_door function
#' @return
#' The function returns a number between 1 and 3.
#' @examples
#' open_goat_door(game, a.pick)
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
#' Contestant given the opportunity to switch their initial door.
#' @description
#' change_door is a function that shows the contestant's strategy of
#' switching or keeping their door.
#' @details
#' After a goat door is revealed only the door initially selected and the other
#' closed door remain. The contestant will have the opportunity to keep their
#' initial door or change their door.
#' @param
#' The function uses the strategy used as an argument stay = TRUE
#' or stay = FALSE. It takes into account the door revealed by the
#' host, opened.door, and the initial door, a.pick.
#' @return
#' The function returns a number between 1 and 3.
#' @examples
#' change_door(stay = TRUE, opened.door, a.pick)
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
#' Determine whether contestant is a winner.
#' @description
#' determine_winner is a function that reveal whether the contestant's
#' door is winner; a car behind it.
#' @details
#' The contestant's door is revealed and it will be known whether the door
#' had a goat or a car behind it.
#' @param
#' The function takes into account the final.pick and the game.
#' @return
#' The function returns "WIN" or "LOSE"
#' @examples
#' determine_winner(final.pick, game)
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
#' Game results
#' @description
#' The function play_game plays the created game and gives you the
#' outcome of the game.
#' @details
#' The function runs the game and gives the strategy and outcome in
#' a data frame.
#' @param
#' No arguments are used by the function.
#' @return
#' The function returns a data frame showing the outcome by strategy.
#' @examples
#' play_game()
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
#' Looping the game.
#' @description
#' play_n_games() runs the game n number of times and gives you outcome
#' outcome by strategy.
#' @details
#' The game is simulated will allow users to run the game n number of times.
#' This will allows is to test the average win rate by strategy and let us
#' determine whcih strategy is dominant.
#' @param
#' The function takes the number of games to be run. 100 is default.
#' @return
#' The function returns a data frame with average win rate by outcome and
#' the results of each game by strategy.
#' @examples
#' play_n_games(n=10000)
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

