#' xox
#'
#' Start a game of noughts and crosses
#' @export

xox = function(){
requireNamespace("xox", quietly = TRUE)

  game_data <- matrix(ncol = 3, nrow = 3)

  console_board(game_data, "Let's play!")
  Sys.sleep(.5)

  winner = FALSE

  while(!winner) {

    console_board(game_data, "Your move: what will it be [row, column]?")

    user_input <-
      readline(prompt = "")


    made_move = 0

    while (made_move == 0) {
      if(identical(user_input,"exit")) stop("game aborted", call. = FALSE)

      if(!valid_move(user_input)){
        console_board(game_data, "Try again. Row then column separated with a comma (e.g. '1,3')")
        user_input <-
          readline(prompt = "")
        next
      }

      if (!available(game_data, user_input)[1]) {
        console_board(game_data, paste0(user_input, " is taken, try somewhere else [row, column]."))

        user_input <-
          readline(prompt = "")

      } else{
        made_move = 1
      }

    }


    moves <- strsplit(user_input, split = ",")[[1]]
    row = as.numeric(trimws(moves[1]))
    col = as.numeric(trimws(moves[2]))

    game_data[row, col] = 'A'

    console_board(game_data," ")


    winner <- check_winner(game_data, player = "A", ai = FALSE)

    if(winner){
      message("You win!!")
      next
    }

    if (sum(!is.na(game_data)) == 9) {
      winner <- TRUE
      message("DRAW!")
      next
    }


    console_board(game_data, "AI moving")
    Sys.sleep(1.5)

    move = unlist(ai(game_data)[, 1:2])
    game_data[move[1], move[2]] = "B"
    console_board(game_data, "Your move")

    winner <- check_winner(game_data, player = "B", ai = FALSE)

    if(winner) console_board(game_data, ""); message("AI wins!")

  }


}

#' available
#'
#' check to see if a desired move is possible

available = function(game_data,user_input){
  is.na(eval(parse(text = glue::glue("game_data[{user_input}]"))))
}

#' check_winner
#'
#' function to derive whether a user has won the game, or to output current best score

check_winner = function(gd, player = "A", ai = TRUE) {
  cols <- lapply(1:3, function(x) {
    sum(gd[, x] == player, na.rm = T)
  })
  rows <- lapply(1:3, function(x) {
    sum(gd[x, ] == player, na.rm = T)
  })

  diag1 <- sum(diag(gd) == player, na.rm = T)
  diag2 = sum(c(gd[1, 3],
                gd[2, 2],
                gd[3, 1]) == player, na.rm = T)
  if(ai){
    return(max(unlist(c(cols,rows,diag1,diag2))))
  }

  if (any(c(cols, rows, diag1, diag2) == 3 )) {

    return(TRUE)

  }else{

    return(FALSE)

  }
}

#' ai
#'
#' function to define ai moves
#' @param gd game_data

ai = function(gd){

  results <- data.frame(expand.grid(row = 1:3,col = 1:3))

  check_score = function(gd, x, player) {
    r = results[x, "row"]
    c = results[x, "col"]

    if (is.na(gd[r, c])) {
      gd[r, c] = player
      score <- check_winner(gd, player = player, ai = TRUE)

      gd[r,c] = NA

      score
    }else{
      NA
    }

  }

  results$score = unlist(lapply(seq_along(results[,1]), function(i) check_score(gd, i, "B")))
  results$enemy = unlist(lapply(seq_along(results[,1]), function(i) check_score(gd, i, "A")))

  if(any(na.omit(results$score == 3))){
    move = results[which(results$score == 3)[1], ]
    return(move)
  }

  if(sum(results$enemy == 3,na.rm = T) > 0){
    results = results[which(results$enemy == 3),]
  }

  move <- results[sample(which(results$score == max(results$score,na.rm = T)),1),]
  move

}


#' console_board
#' @param gb game data
#' @param status message to user

console_board = function(gb, status){
  cat("\014")
  flush.console()
  cat("'exit' to quit")
  cat("\n")
  cat("\n")
  cat("\n")
  gb[is.na(gb)] = " "
  gb[gb == "A"] = "X"
  gb[gb == "B"] = "O"
  gb = c(gb[1,], gb[2, ], gb[3, ])

  board <- ("\r
          |     |
       %s  |  %s  |  %s
     _____|_____|_____
          |     |
       %s  |  %s  |  %s
     _____|_____|_____
          |     |
       %s  |  %s  |  %s
          |     |

    ...................
    %s")

  cat(do.call(sprintf, c(fmt = board, append(as.list(gb), status))))


}

valid_move = function(user_input){
  grepl("^[1-3]\\,[1-3]$",user_input)
}



