
# - Restrepo & Keskintürk --------------------------------------------------------------------- #
# - Data Cleaning and Survey Setup: NSYR - Potpourri ------------------------------------------ #
# --------------------------------------------------------------------------------------------- #

# - part 01: load packages and the data ------------------------------------------------------- #

rm(list = ls())
pacman::p_load(tidyverse, haven, labelled, janitor)

nsyr1 <- haven::read_dta("./data/raw/nsyr_w2.dta") |>
  zap_labels() |>
  clean_names(case = "snake")

nsyr2 <- haven::read_dta("./data/raw/nsyr_w3.dta") |>
  zap_labels() |>
  clean_names(case = "snake")

nsyr3 <- haven::read_dta("./data/raw/nsyr_w4.dta") |>
  zap_labels() |>
  clean_names(case = "snake")
names(nsyr3) <-
  gsub(names(nsyr3), pattern = "_w4", replacement = "")

# - part 02: landscape variables -------------------------------------------------------------- #

# time 1

nsyr1 <- nsyr1 |>
  select(ids, god, spiritua, wrldorig, moralrel, viewrel, relprvte, okayconv) |>
  mutate(
    
    god = recode(
      god,
      '1' = 1,
      '2' = 0,
      '3' = 0,
      .default = NA_real_
    ),
    
    spiritua = recode(
      spiritua,
      '1' = 0,
      '2' = 0,
      '3' = 1,
      .default = NA_real_
    ),
    
    wrldorig = recode(
      wrldorig,
      '1' = 1,
      '2' = 0,
      '3' = 0,
      .default = NA_real_
    ),
    
    moralrel = recode(
      moralrel,
      '1' = 0,
      '2' = 0,
      '3' = 0,
      '4' = 1,
      '5' = 1,
      .default = NA_real_
    ),
    
    viewrel = recode(
      viewrel,
      '1' = 1,
      '2' = 0,
      '3' = 0,
      .default = NA_real_
    ),
    
    relprvte = recode(
      relprvte,
      '1' = 0,
      '2' = 0,
      '3' = 0,
      '4' = 1,
      '5' = 1,
      .default = NA_real_
    ),
    
    okayconv = recode(
      okayconv,
      '1' = 1,
      '2' = 0,
      .default = NA_real_
    )
    
  ) |> drop_na() |>
  mutate(position = paste0(god, spiritua, wrldorig, moralrel, viewrel, relprvte, okayconv)) |>
  dplyr::select(ids, position)

# time 2

nsyr2 <- nsyr2 |>
  select(ids, god, spiritua = spirtual, wrldorig, moralrel, viewrel, relprvte, okayconv) |>
  mutate(
    
    god = recode(
      god,
      '1' = 1,
      '2' = 0,
      '3' = 0,
      .default = NA_real_
    ),
    
    spiritua = recode(
      spiritua,
      '1' = 0,
      '2' = 0,
      '3' = 1,
      .default = NA_real_
    ),
    
    wrldorig = recode(
      wrldorig,
      '1' = 1,
      '2' = 0,
      '3' = 0,
      .default = NA_real_
    ),
    
    moralrel = recode(
      moralrel,
      '1' = 0,
      '2' = 0,
      '3' = 0,
      '4' = 1,
      '5' = 1,
      .default = NA_real_
    ),
    
    viewrel = recode(
      viewrel,
      '1' = 1,
      '2' = 0,
      '3' = 0,
      .default = NA_real_
    ),
    
    relprvte = recode(
      relprvte,
      '1' = 0,
      '2' = 0,
      '3' = 0,
      '4' = 1,
      '5' = 1,
      .default = NA_real_
    ),
    
    okayconv = recode(
      okayconv,
      '1' = 1,
      '2' = 0,
      .default = NA_real_
    )
    
  ) |> drop_na() |>
  mutate(position = paste0(god, spiritua, wrldorig, moralrel, viewrel, relprvte, okayconv)) |>
  dplyr::select(ids, position)

# time 3

nsyr3 <- nsyr3 |>
  select(ids, god, spiritua = spiritual, wrldorig = wrldorigin, moralrel, viewrel, relprvte, okayconv) |>
  mutate(
    
    god = recode(
      god,
      '0' = 0,
      '1' = 1,
      '2' = 0,
      .default = NA_real_
    ),
    
    spiritua = recode(
      spiritua,
      '1' = 0,
      '2' = 0,
      '3' = 1,
      .default = NA_real_
    ),
    
    wrldorig = recode(
      wrldorig,
      '1' = 1,
      '2' = 0,
      '3' = 0,
      .default = NA_real_
    ),
    
    moralrel = recode(
      moralrel,
      '1' = 0,
      '2' = 0,
      '3' = 0,
      '4' = 1,
      '5' = 1,
      .default = NA_real_
    ),
    
    viewrel = recode(
      viewrel,
      '1' = 1,
      '2' = 0,
      '3' = 0,
      .default = NA_real_
    ),
    
    relprvte = recode(
      relprvte,
      '1' = 0,
      '2' = 0,
      '3' = 0,
      '4' = 1,
      '5' = 1,
      .default = NA_real_
    ),
    
    okayconv = recode(
      okayconv,
      '0' = 0,
      '1' = 1,
      .default = NA_real_
    )
    
  ) |> drop_na() |>
  mutate(position = paste0(god, spiritua, wrldorig, moralrel, viewrel, relprvte, okayconv)) |>
  dplyr::select(ids, position)

# - part 03: position space ------------------------------------------------------------------- #

# merge the waves
nsyr_pot <- 
  inner_join(nsyr1, nsyr2, by = "ids") |> inner_join(nsyr3, by = "ids")
colnames(nsyr_pot) <- c("ids", "pos1", "pos2", "pos3")

saveRDS(nsyr_pot, file = "./data/cleaned/nsyr_pot.rds")

# --------------------------------------------------------------------------------------------- #
