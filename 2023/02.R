library(tidyverse)

# part one ----

cube_colours <- c("red", "green", "blue")

cube_count <- tibble::tibble(
  colour = c("red", "green", "blue"),
  count = c(12, 13, 14)
)

input <- readr::read_lines("data/02-input.txt")
game = input %>%
  tibble::as_tibble() %>%
  tibble::tibble(
    game_id = as.numeric(stringr::str_extract(value, "\\d+")),
    results = stringr::str_remove(value, "Game\\s\\d+:\\s")
  ) %>%
  dplyr::mutate(rounds = stringr::str_split(results, ";")) %>%
  tidyr::unnest(rounds) %>%
  dplyr::mutate(rounds = trimws(rounds)) %>%
  dplyr::mutate(round_id = row_number(), .by = "game_id", .after = "game_id") %>%
  dplyr::mutate(colour_count = stringr::str_split(rounds, ",")) %>%
  tidyr::unnest(colour_count) %>%
  dplyr::mutate(
    colour_count = trimws(colour_count),
    colour = stringr::word(colour_count, 2),
    colour_count = as.numeric(stringr::word(colour_count, 1))
  ) %>%
  group_by(game_id, round_id) %>%
  tidyr::complete(
    colour = cube_colours,
    value = value,
    results = results,
    rounds = rounds,
    fill = list(colour_count = 0)
  ) %>%
  dplyr::ungroup() %>%
  select(-value, -rounds, -results)

solution = game %>%
  dplyr::left_join(cube_count, by = "colour") %>%
  dplyr::rename("max_count" = "count") %>%
  dplyr::mutate(leq_max_count = colour_count <= max_count) %>%
  dplyr::mutate(all_leq_max_count = all(leq_max_count), .by = "game_id") %>%
  dplyr::filter(all_leq_max_count) %>%
  dplyr::pull(game_id) %>%
  unique() %>%
  sum()

solution

## part two ----

solution_two = game %>%
  dplyr::mutate(
    min_possible_colour = max(colour_count), .by = c("game_id", "colour")
  ) %>%
  dplyr::select(game_id, colour, min_possible_colour) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    game_power = prod(min_possible_colour), .by = "game_id"
  ) %>%
  dplyr::select(game_id, game_power) %>%
  dplyr::distinct() %>%
  dplyr::pull(game_power) %>%
  sum()

solution_two
