library(testthat)

# Test case for a hand with numeric cards and an ace
test_that("Numeric cards and an ace", {
  cards <- c("2", "J", "A")
  expect_equal(calculate_blackjack_score(cards), 13)
})

# Test case for a hand with face cards only
test_that("Face cards only", {
  cards <- c("K", "Q", "J")
  expect_equal(calculate_blackjack_score(cards), "Bust")
})

# Test case for a hand with an ace and other cards
test_that("Ace and other cards", {
  cards <- c("A", "7", "9")
  expect_equal(calculate_blackjack_score(cards), 17)
})

# Test case for a bust hand
test_that("Bust hand", {
  cards <- c("10", "J", "Q")
  expect_equal(calculate_blackjack_score(cards), "Bust")
})
#
#
#
#
#
#
#
#

test_that("test play_blackjack", {
  expect_equal(play_blackjack(x = 1, y = "7,9", z = "10,A"), cat("Dealer, enter your cards (comma-separated): Enter cards for Player  1  (comma-separated): Player  1  score:  21
Dealer score:  16"))
})
