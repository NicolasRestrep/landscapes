
# - Restrepo & Keskintürk --------------------------------------------------------------------- #
# - Data Cleaning and Survey Setup: NSYR - Sexuality ------------------------------------------ #
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
  select(ids, 
    abstain1, divrceok, mandecid, menwrk, unmarsex, wommar, wrkngmom) |>
  mutate(
    
    abstain1 = recode(
      abstain1,
      '1' = 1,
      '2' = 0,
      .default = NA_real_
    ),
    
    divrceok = recode(
      divrceok,
      '1' = 0,
      '2' = 1,
      .default = NA_real_
    ),
    
    mandecid = recode(
      mandecid,
      '1' = 1,
      '2' = 1,
      '4' = 0,
      '5' = 0,
      .default = NA_real_
    ),
    
    menwrk = recode(
      menwrk,
      '1' = 1,
      '2' = 1,
      '4' = 0,
      '5' = 0,
      .default = NA_real_
    ),
    
    unmarsex = recode(
      unmarsex,
      '1' = 0,
      '2' = 0,
      '4' = 1,
      '5' = 1,
      .default = NA_real_
    ),
    
    wommar = recode(
      wommar,
      '1' = 0,
      '2' = 0,
      '4' = 1,
      '5' = 1,
      .default = NA_real_
    ),
    
    wrkngmom = recode(
      wrkngmom,
      '1' = 0,
      '2' = 0,
      '4' = 1,
      '5' = 1,
      .default = NA_real_
    )
  ) |> drop_na() |>
  mutate(position = paste0(abstain1, divrceok, mandecid, menwrk, unmarsex, wommar, wrkngmom)) |>
  dplyr::select(ids, position)

# time 2

nsyr2 <- nsyr2 |>
  select(ids, 
         abstain1, divrceok, mandecid, menwrk, unmarsex, wommar, wrkngmom) |>
  mutate(
    
    abstain1 = recode(
      abstain1,
      '1' = 1,
      '2' = 0,
      .default = NA_real_
    ),
    
    divrceok = recode(
      divrceok,
      '1' = 0,
      '2' = 1,
      .default = NA_real_
    ),
    
    mandecid = recode(
      mandecid,
      '1' = 1,
      '2' = 1,
      '4' = 0,
      '5' = 0,
      .default = NA_real_
    ),
    
    menwrk = recode(
      menwrk,
      '1' = 1,
      '2' = 1,
      '4' = 0,
      '5' = 0,
      .default = NA_real_
    ),
    
    unmarsex = recode(
      unmarsex,
      '1' = 0,
      '2' = 0,
      '4' = 1,
      '5' = 1,
      .default = NA_real_
    ),
    
    wommar = recode(
      wommar,
      '1' = 0,
      '2' = 0,
      '4' = 1,
      '5' = 1,
      .default = NA_real_
    ),
    
    wrkngmom = recode(
      wrkngmom,
      '1' = 0,
      '2' = 0,
      '4' = 1,
      '5' = 1,
      .default = NA_real_
    )
  ) |> drop_na() |>
  mutate(position = paste0(abstain1, divrceok, mandecid, menwrk, unmarsex, wommar, wrkngmom)) |>
  dplyr::select(ids, position)

# time 3

nsyr3 <- nsyr3 |>
  select(ids, 
         abstain1, divrceok, mandecid = mandecide, menwrk = menwrkmenhme, 
         unmarsex, wommar = womenmar, wrkngmom) |>
  mutate(
    
    abstain1 = recode(
      abstain1,
      '1' = 0,
      '2' = 1,
      '3' = 0,
      .default = NA_real_
    ),
    
    divrceok = recode(
      divrceok,
      '0' = 1,
      '1' = 0,
      .default = NA_real_
    ),
    
    mandecid = recode(
      mandecid,
      '1' = 1,
      '2' = 1,
      '3' = 0,
      '4' = 0,
      .default = NA_real_
    ),
    
    menwrk = recode(
      menwrk,
      '1' = 1,
      '2' = 1,
      '3' = 0,
      '4' = 0,
      .default = NA_real_
    ),
    
    unmarsex = recode(
      unmarsex,
      '1' = 0,
      '2' = 0,
      '4' = 1,
      '5' = 1,
      .default = NA_real_
    ),
    
    wommar = recode(
      wommar,
      '1' = 0,
      '2' = 0,
      '3' = 1,
      '4' = 1,
      .default = NA_real_
    ),
    
    wrkngmom = recode(
      wrkngmom,
      '1' = 0,
      '2' = 0,
      '3' = 1,
      '4' = 1,
      .default = NA_real_
    )
  ) |> drop_na() |>
  mutate(position = paste0(abstain1, divrceok, mandecid, menwrk, unmarsex, wommar, wrkngmom)) |>
  dplyr::select(ids, position)

# - part 03: position space ------------------------------------------------------------------- #

# merge the waves
nsyr_sex <- 
  inner_join(nsyr1, nsyr2, by = "ids") |> inner_join(nsyr3, by = "ids")
colnames(nsyr_sex) <- c("ids", "pos1", "pos2", "pos3")

saveRDS(nsyr_sex, file = "./data/cleaned/nsyr_sex.rds")

# --------------------------------------------------------------------------------------------- #
