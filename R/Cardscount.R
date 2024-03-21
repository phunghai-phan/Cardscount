#' play_blackjack
#'
#' Calculate the score of a hand in blackjack
#' @return The total score of the hand
#' @examples
#' # play_blackjack()
#' @details
#' The function computes the score by adding up the numeric value of number cards,
#' treating face cards (J, Q, K) as 10, and handling aces (A) based on whether
#' adding 11 would result in a bust (score > 21).
#' @export


play_blackjack <- function(x = readline("Enter the number of players: "),
                           y = readline(),
                           z = readline()) {
  # Ask the user for the number of players
  num_players_input <- x

  # Validate if the input is not empty and is an integer
  if (!grepl("^\\d+$", num_players_input)) {
    stop("Invalid input. Please enter a positive integer for the number of players.")
  }

  num_players <- as.integer(num_players_input)

  # Initialize a list to store player scores
  player_scores <- vector("list", length = num_players)

  # Collect cards for the dealer
  cat("Dealer, enter your cards (comma-separated): ")
  dealer_input <<- y
  dealer_cards <- strsplit(dealer_input, ",")[[1]]

  # Validate dealer cards
  valid_cards <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")
  for (card_value in dealer_cards) {
    if (!(card_value %in% valid_cards)) {
      stop("Invalid card value for the dealer: ", card_value)
    }
  }


  # Collect cards for each player
  for (i in 1:num_players) {
    cat("Enter cards for Player ", i, " (comma-separated): ")
    player_input <<- z
    player_cards <- strsplit(player_input, ",")[[1]]
    player_scores[[i]] <- calculate_blackjack_score(player_cards)
  }

  # Calculate dealer's score
  dealer_score <- calculate_blackjack_score(dealer_cards)

  # Display scores for each player
  for (i in 1:length(player_scores)) {
    cat("Player ", i, " score: ", player_scores[[i]], "\n")
  }
  cat("Dealer score: ", dealer_score, "\n")
}


#' Calculate the score of a hand in blackjack
#' description
#' Calculate the score of a hand in blackjack based on the numeric value of the cards and the rules of the game.
#' The function computes the score by adding up the numeric value of number cards,
#' treating face cards (J, Q, K) as 10, and handling aces (A) based on whether
#' adding 11 would result in a bust (score > 21).
#' @param cards A character vector of cards (e.g., "2", "J", "A")
#' @examples
#' # calculate_blackjack_score(c("2", "J", "A"))
#'
#' @return The total score of the hand
#' @details
#' The function computes the score by adding up the numeric value of number cards,
#' treating face cards (J, Q, K) as 10, and handling aces (A) based on whether
#' adding 11 would result in a bust (score > 21).


# Calculate the score of a hand in blackjack
calculate_blackjack_score <- function(cards) {
  # Initialize the score
  score <- 0
  # Count the number of aces in the hand
  num_aces <- 0

  # Valid card values
  valid_cards <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")

  # Validate input cards
  for (card_value in cards) {
    if (!(card_value %in% valid_cards)) {
      stop(paste("Invalid card value:", card_value))
    }
  }

  # Iterate over each card in the hand
  for (card_value in cards) {
    # If the card is a number card, add its value to the score
    if (card_value %in% c("2", "3", "4", "5", "6", "7", "8", "9", "10")) {
      score <- score + as.numeric(card_value)
    }
    # If the card is a face card, add 10 to the score
    else if (card_value %in% c("J", "Q", "K")) {
      score <- score + 10
    }
    # If the card is an ace, increment the number of aces
    else if (card_value == "A") {
      num_aces <- num_aces + 1
    }
  }

  # Adjust the score for aces
  while (num_aces > 0) {
    # If adding 11 would not bust the hand, add 11
    if (score + 11 <= 21) {
      score <- score + 11
    }
    # Otherwise, add 1
    else {
      score <- score + 1
    }
    num_aces <- num_aces - 1
  }
  # Check if the hand is a bust
  if (score > 21) {
    return("Bust")
  }

  return(score)
}
