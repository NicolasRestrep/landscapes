
# - Restrepo & Keskintürk --------------------------------------------------------------------- #
# - Data Cleaning and Survey Setup: NSYR - Religious Beliefs ---------------------------------- #
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
  select(ids, aftrlife, angels, astrolgy, demons, god, miracles, reincar) |> 
  mutate_at(
    .vars = c(
      "aftrlife",
      "angels",
      "astrolgy",
      "demons",
      "god",
      "miracles",
      "reincar"
    ),
    list( ~ case_when(. == 1 ~ 1,
                      . == 2 ~ 0,
                      . == 3 ~ 0,
                      TRUE ~ NA_real_))) |>
  drop_na() |> 
  mutate(position = 
           paste0(aftrlife, angels, astrolgy, demons, god, miracles, reincar)) |> 
  select(ids, position)

# time 2

nsyr2 <- nsyr2 |>
  select(ids, aftrlife, angels, astrolgy, demons, god, miracles, reincar) |> 
  mutate_at(
    .vars = c(
      "aftrlife",
      "angels",
      "astrolgy",
      "demons",
      "god",
      "miracles",
      "reincar"
    ),
    list( ~ case_when(. == 1 ~ 1,
                      . == 2 ~ 0,
                      . == 3 ~ 0,
                      TRUE ~ NA_real_))) |> 
  drop_na() |> 
  mutate(position = 
           paste0(aftrlife, angels, astrolgy, demons, god, miracles, reincar)) |> 
  select(ids, position)

# time 3

nsyr3 <- nsyr3 |>
  rename(aftrlife = afterlife) |>
  select(ids, aftrlife, angels, astrolgy, demons, god, miracles, reincar) |> 
  mutate(god = recode(
    god,
    "0" = 2,
    "1" = 1,
    "2" = 3,
    .default = NA_real_
  )) |>
  mutate_at(
    .vars = c(
      "aftrlife",
      "angels",
      "astrolgy",
      "demons",
      "god",
      "miracles",
      "reincar"
    ),
    list( ~ case_when(. == 1 ~ 1,
                      . == 2 ~ 0,
                      . == 3 ~ 0,
                      TRUE ~ NA_real_))) |> 
  drop_na() |> 
  mutate(position = 
           paste0(aftrlife, angels, astrolgy, demons, god, miracles, reincar)) |> 
  select(ids, position)

# - part 03: position space ------------------------------------------------------------------- #

# merge the waves
nsyr.rel <- 
  inner_join(nsyr1, nsyr2, by = "ids") |> inner_join(nsyr3, by = "ids")
colnames(nsyr.rel) <- c("ids", "pos1", "pos2", "pos3")

saveRDS(nsyr.rel, file = "./data/cleaned/nsyr_rel.rds")

# --------------------------------------------------------------------------------------------- #
