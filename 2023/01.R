# part one ----

library(tidyverse)

extract_and_flatten <- function(string) {
  stringr::str_extract_all(string, "[0-9]", simplify = TRUE) %>%
    stringr::str_flatten()
}

input <- readLines("01-input.txt")

input_with_digit_pairs <- input %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    digits = extract_and_flatten(value),
    .by  = "value"
  ) %>%
  dplyr::mutate(
    digit_length = stringr::str_length(digits)
  ) %>%
  dplyr::mutate(
    first_digit = stringr::str_sub(digits, 1, 1),
    last_digit = stringr::str_sub(digits, digit_length, digit_length)
  ) %>%
  dplyr::mutate(
    digit_pair = as.numeric(paste0(first_digit, last_digit))
  )
solution <- sum(input_with_digit_pairs$digit_pair)
solution

# part two ----

digits_as_words <- tibble::tibble(
  digits = 1:9,
  words = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
)
words <- stringr::str_c(c(digits_as_words$digits, digits_as_words$words), collapse = "|")

extract_and_flatten_words <- function(string, digit_word_lookup) {
  # wordlist: string of words which looks like word|something|1|2|hello|world
  wordlist <- stringr::str_c(c(digit_word_lookup$digits, digit_word_lookup$words), collapse = "|")
  extracted_words = stringr::str_match_all(string = string,
                                           pattern = glue::glue("(?=({words}.{words}))"))[[1]][,2]
  tibble::tibble(words = extracted_words) %>%
    dplyr::mutate(as_number = words2number::to_number(words)) %>%
    dplyr::pull(as_number) %>%
    stringr::str_flatten()
}

input_with_word_digit_pairs <- input %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    digits = extract_and_flatten_words(value, digits_as_words),
    .by  = "value"
  ) %>%
  dplyr::mutate(
    digit_length = stringr::str_length(digits)
  ) %>%
  dplyr::mutate(
    first_digit = stringr::str_sub(digits, 1, 1),
    last_digit = stringr::str_sub(digits, digit_length, digit_length)
  ) %>%
  dplyr::mutate(
    digit_pair = as.numeric(paste0(first_digit, last_digit))
  )
input_with_word_digit_pairs
solution2 <- sum(input_with_word_digit_pairs$digit_pair)
solution2
