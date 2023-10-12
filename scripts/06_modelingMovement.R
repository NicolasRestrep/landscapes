
# - Restrepo & Keskintürk --------------------------------------------------------------------- #
# - Modeling Movement in the NSYR ------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------- #

# - part 01: load packages & data ------------------------------------------------------------- #

rm(list = ls())
pacman::p_load(tidyverse, janitor, igraph, ggraph, hrbrthemes, patchwork)
theme_set(theme_ipsum_rc())

d_rel <- readRDS("./data/cleaned/nsyr_rel.rds")
d_sex <- readRDS("./data/cleaned/nsyr_sex.rds")
d_pot <- readRDS("./data/cleaned/nsyr_pot.rds")

# define the position universe
position_string <- 7
positions <- 
  expand.grid(
    replicate(position_string, 0:1, simplify = F)) |>
  as_tibble() |> unite(col = position, sep = "") |> pull(position) |> sort()
position_length <- length(positions)

set.seed(11235)

# - part 02: function call -------------------------------------------------------------------- #

calculate_error <- 
  function(positions,
           distance_mat, weights_mat,
           movement_model,
           num_iterations,
           position_length,
           position_string) 
  {
    
    ### prep the matrix
    transition_matrix <-
      matrix(NA, ncol = position_length, nrow = position_length)
    colnames(transition_matrix) <- positions
    rownames(transition_matrix) <- positions
    
    ### movement models
    for (i in 1:nrow(transition_matrix)) {
      
      if (movement_model == "gravitation") {
        transition_matrix[i, ] <-
          (weights_mat[, 1] / 
             (distance_mat[positions[i],]) ^ 2) / sum((weights_mat[, 1] / (distance_mat[positions[i],]) ^ 2))
      } 
      
      else if (movement_model == "random") {
        transition_matrix[i, ] <- c(rep(1 / position_length, position_length))
      } 
      
      else if (movement_model == "distance") {
        transition_matrix[i, ] <-
          ((position_string + 2) - distance_mat[positions[i], ])^2 /
          sum(((position_string + 2) - distance_mat[positions[i], ])^2)
      } 
      
      else if (movement_model == "move_one") {
        transition_matrix[i, ] <-
          ifelse(distance_mat[positions[i], ] <= 2, 1 / (position_string + 1), 0)
      } 
      
      else {
        transition_matrix[i, ] <-
          ifelse(distance_mat[positions[i], ] == 1, 1, 0)
      }
    }
    
    ### error
    abs_errors <- vector(mode = "double", length = num_iterations)
    
    ### iterations
    for (z in 1:num_iterations) {
      realizations_matrix <-
        matrix(NA, ncol = position_length, nrow = position_length)
      colnames(realizations_matrix) <- positions
      rownames(realizations_matrix) <- positions
      
      for (j in 1:nrow(realizations_matrix)) {
        realizations_matrix[j, ] <-
          table(factor(
            sample(
              c(1:position_length),
              size = weights_mat[j, 1],
              prob = transition_matrix[j, ],
              replace = T
            ),
            levels = 1:position_length
          ))
      }
      abs_errors[z] <- sum(abs(evidence_matrix - realizations_matrix))
    }
    
    return(abs_errors)
  }

### - section 1: time 1 to time 2

# - part 03: religion ------------------------------------------------------------------------- #

# 1 / measuring distance
distance_mat <-
  matrix(0, ncol = position_length, nrow = position_length)
for (v in 1:nrow(distance_mat)) {
  for (j in 1:ncol(distance_mat)) {
    distance_mat[v, j] <-
      sum(abs(as.numeric(str_split(positions[v], "")[[1]]) -
                as.numeric(str_split(positions[j], "")[[1]]))) + 1
  }
}
rownames(distance_mat) <- colnames(distance_mat) <- positions

# 2 / measuring weight
weights_mat <-
  tibble(positions = positions) |> 
  ## weights from
  left_join(d_rel |> group_by(pos1) |> summarize(n1 = n()), 
            by = c("positions" = "pos1")) |>
  mutate(n1 = ifelse(is.na(n1) == TRUE, 0, n1)) |> 
  ## weights to
  left_join(d_rel |> group_by(pos2) |> summarize(n2 = n()), 
            by = c("positions" = "pos2")) |> 
  mutate(n2 = ifelse(is.na(n2) == TRUE, 0, n2)) |>
  select(n1, n2) |> as.matrix()

# 3 / measuring evidence matrix
evidence_matrix <- 
  matrix(0, nrow = position_length, ncol = position_length)
colnames(evidence_matrix) <- rownames(evidence_matrix) <- positions
for (t in 1:nrow(d_rel)) {
  evidence_matrix[d_rel$pos1[t], 
                  d_rel$pos2[t]] <- 
    evidence_matrix[d_rel$pos1[t], 
                    d_rel$pos2[t]] + 1
}

# 4 / runs for prediction errors
prediction_errors <-
  map(
    c("gravitation",
      "random",
      "distance",
      "move_one",
      "always_stay"),
    calculate_error,
    positions = positions,
    position_length = position_length,
    distance_mat = distance_mat,
    weights_mat = weights_mat,
    position_string = 7,
    num_iterations = 1e2
  )

# 5 / calculate error
rel_results <- tibble(
  movement_model = rep(c("gravitation", 
                         "random", 
                         "distance", 
                         "move_one", 
                         "always_stay"), 
                       each = 1e2), 
  abs_error = unlist(prediction_errors), 
  landscape = "Religion"
)

# - part 04: sexuality ------------------------------------------------------------------------ #

# 1 / measuring distance
distance_mat <-
  matrix(0, ncol = position_length, nrow = position_length)
for (v in 1:nrow(distance_mat)) {
  for (j in 1:ncol(distance_mat)) {
    distance_mat[v, j] <-
      sum(abs(as.numeric(str_split(positions[v], "")[[1]]) -
                as.numeric(str_split(positions[j], "")[[1]]))) + 1
  }
}
rownames(distance_mat) <- colnames(distance_mat) <- positions

# 2 / measuring weight
weights_mat <-
  tibble(positions = positions) |> 
  ## weights from
  left_join(d_sex |> group_by(pos1) |> summarize(n1 = n()), 
            by = c("positions" = "pos1")) |>
  mutate(n1 = ifelse(is.na(n1) == TRUE, 0, n1)) |> 
  ## weights to
  left_join(d_sex |> group_by(pos2) |> summarize(n2 = n()), 
            by = c("positions" = "pos2")) |> 
  mutate(n2 = ifelse(is.na(n2) == TRUE, 0, n2)) |>
  select(n1, n2) |> as.matrix()

# 3 / measuring evidence matrix
evidence_matrix <- 
  matrix(0, nrow = position_length, ncol = position_length)
colnames(evidence_matrix) <- rownames(evidence_matrix) <- positions
for (t in 1:nrow(d_sex)) {
  evidence_matrix[d_sex$pos1[t], 
                  d_sex$pos2[t]] <- 
    evidence_matrix[d_sex$pos1[t], 
                    d_sex$pos2[t]] + 1
}

# 4 / runs for prediction errors
prediction_errors <-
  map(
    c("gravitation",
      "random",
      "distance",
      "move_one",
      "always_stay"),
    calculate_error,
    positions = positions,
    position_length = position_length,
    distance_mat = distance_mat,
    weights_mat = weights_mat,
    position_string = 7,
    num_iterations = 1e2
  )

# 5 / calculate error
sex_results <- tibble(
  movement_model = rep(c("gravitation", 
                         "random", 
                         "distance", 
                         "move_one", 
                         "always_stay"), 
                       each = 1e2), 
  abs_error = unlist(prediction_errors), 
  landscape = "Sexuality and Gender"
)

# - part 05: eclectic ------------------------------------------------------------------------- #

# 1 / measuring distance
distance_mat <-
  matrix(0, ncol = position_length, nrow = position_length)
for (v in 1:nrow(distance_mat)) {
  for (j in 1:ncol(distance_mat)) {
    distance_mat[v, j] <-
      sum(abs(as.numeric(str_split(positions[v], "")[[1]]) -
                as.numeric(str_split(positions[j], "")[[1]]))) + 1
  }
}
rownames(distance_mat) <- colnames(distance_mat) <- positions

# 2 / measuring weight
weights_mat <-
  tibble(positions = positions) |> 
  ## weights from
  left_join(d_pot |> group_by(pos1) |> summarize(n1 = n()), 
            by = c("positions" = "pos1")) |>
  mutate(n1 = ifelse(is.na(n1) == TRUE, 0, n1)) |> 
  ## weights to
  left_join(d_pot |> group_by(pos2) |> summarize(n2 = n()), 
            by = c("positions" = "pos2")) |> 
  mutate(n2 = ifelse(is.na(n2) == TRUE, 0, n2)) |>
  select(n1, n2) |> as.matrix()

# 3 / measuring evidence matrix
evidence_matrix <- 
  matrix(0, nrow = position_length, ncol = position_length)
colnames(evidence_matrix) <- rownames(evidence_matrix) <- positions
for (t in 1:nrow(d_pot)) {
  evidence_matrix[d_pot$pos1[t], 
                  d_pot$pos2[t]] <- 
    evidence_matrix[d_pot$pos1[t], 
                    d_pot$pos2[t]] + 1
}

# 4 / runs for prediction errors
prediction_errors <-
  map(
    c("gravitation",
      "random",
      "distance",
      "move_one",
      "always_stay"),
    calculate_error,
    positions = positions,
    position_length = position_length,
    distance_mat = distance_mat,
    weights_mat = weights_mat,
    position_string = 7,
    num_iterations = 1e2
  )

# 5 / calculate error
pot_results <- tibble(
  movement_model = rep(c("gravitation", 
                         "random", 
                         "distance", 
                         "move_one", 
                         "always_stay"), 
                       each = 1e2), 
  abs_error = unlist(prediction_errors), 
  landscape = "Eclectic"
)

# - part 06: plots ---------------------------------------------------------------------------- #

all_results <- rbind(rel_results, 
                     sex_results, 
                     pot_results)

p1 <- all_results |>
  group_by(landscape, 
           movement_model)|>
  summarise(avg = mean(abs_error), 
            s_d = sd(abs_error), 
            upr = avg + 1.96*s_d, 
            lwr = avg - 1.96*s_d) |>
  mutate(movement_model = case_when(
    movement_model == "random" ~ "Random",
    movement_model == "always_stay" ~ "Always Stay",
    movement_model == "gravitation" ~ "Gravitation",
    movement_model == "distance" ~ "Distance",
    movement_model == "move_one" ~ "1 Move at Most")) |>
  mutate(movement_model = factor(movement_model,
                                 levels = c("Gravitation",
                                            "1 Move at Most",
                                            "Always Stay",
                                            "Distance",
                                            "Random"))) |> 
  mutate(landscape = factor(landscape,
                            levels = c("Religion",
                                       "Sexuality and Gender",
                                       "Eclectic"))) |> 
  ggplot(aes(x = movement_model, y = avg)) +
  geom_point(size = 1) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  labs(title = "Time 1 to Time 2",
       x = "Strategies", y = "Average Prediction Error") + 
  facet_wrap(~landscape) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

### - section 2: time 2 to time 3

# - part 07: religion ------------------------------------------------------------------------- #

# 1 / measuring distance
distance_mat <-
  matrix(0, ncol = position_length, nrow = position_length)
for (v in 1:nrow(distance_mat)) {
  for (j in 1:ncol(distance_mat)) {
    distance_mat[v, j] <-
      sum(abs(as.numeric(str_split(positions[v], "")[[1]]) -
                as.numeric(str_split(positions[j], "")[[1]]))) + 1
  }
}
rownames(distance_mat) <- colnames(distance_mat) <- positions

# 2 / measuring weight
weights_mat <-
  tibble(positions = positions) |> 
  ## weights from
  left_join(d_rel |> group_by(pos2) |> summarize(n1 = n()), 
            by = c("positions" = "pos2")) |>
  mutate(n1 = ifelse(is.na(n1) == TRUE, 0, n1)) |> 
  ## weights to
  left_join(d_rel |> group_by(pos3) |> summarize(n2 = n()), 
            by = c("positions" = "pos3")) |> 
  mutate(n2 = ifelse(is.na(n2) == TRUE, 0, n2)) |>
  select(n1, n2) |> as.matrix()

# 3 / measuring evidence matrix
evidence_matrix <- 
  matrix(0, nrow = position_length, ncol = position_length)
colnames(evidence_matrix) <- rownames(evidence_matrix) <- positions
for (t in 1:nrow(d_rel)) {
  evidence_matrix[d_rel$pos2[t], 
                  d_rel$pos3[t]] <- 
    evidence_matrix[d_rel$pos2[t], 
                    d_rel$pos3[t]] + 1
}

# 4 / runs for prediction errors
prediction_errors <-
  map(
    c("gravitation",
      "random",
      "distance",
      "move_one",
      "always_stay"),
    calculate_error,
    positions = positions,
    position_length = position_length,
    distance_mat = distance_mat,
    weights_mat = weights_mat,
    position_string = 7,
    num_iterations = 1e2
  )

# 5 / calculate error
rel_results <- tibble(
  movement_model = rep(c("gravitation", 
                         "random", 
                         "distance", 
                         "move_one", 
                         "always_stay"), 
                       each = 1e2), 
  abs_error = unlist(prediction_errors), 
  landscape = "Religion"
)

# - part 08: sexuality ------------------------------------------------------------------------ #

# 1 / measuring distance
distance_mat <-
  matrix(0, ncol = position_length, nrow = position_length)
for (v in 1:nrow(distance_mat)) {
  for (j in 1:ncol(distance_mat)) {
    distance_mat[v, j] <-
      sum(abs(as.numeric(str_split(positions[v], "")[[1]]) -
                as.numeric(str_split(positions[j], "")[[1]]))) + 1
  }
}
rownames(distance_mat) <- colnames(distance_mat) <- positions

# 2 / measuring weight
weights_mat <-
  tibble(positions = positions) |> 
  ## weights from
  left_join(d_sex |> group_by(pos2) |> summarize(n1 = n()), 
            by = c("positions" = "pos2")) |>
  mutate(n1 = ifelse(is.na(n1) == TRUE, 0, n1)) |> 
  ## weights to
  left_join(d_sex |> group_by(pos3) |> summarize(n2 = n()), 
            by = c("positions" = "pos3")) |> 
  mutate(n2 = ifelse(is.na(n2) == TRUE, 0, n2)) |>
  select(n1, n2) |> as.matrix()

# 3 / measuring evidence matrix
evidence_matrix <- 
  matrix(0, nrow = position_length, ncol = position_length)
colnames(evidence_matrix) <- rownames(evidence_matrix) <- positions
for (t in 1:nrow(d_sex)) {
  evidence_matrix[d_sex$pos2[t], 
                  d_sex$pos3[t]] <- 
    evidence_matrix[d_sex$pos2[t], 
                    d_sex$pos3[t]] + 1
}

# 4 / runs for prediction errors
prediction_errors <-
  map(
    c("gravitation",
      "random",
      "distance",
      "move_one",
      "always_stay"),
    calculate_error,
    positions = positions,
    position_length = position_length,
    distance_mat = distance_mat,
    weights_mat = weights_mat,
    position_string = 7,
    num_iterations = 1e2
  )

# 5 / calculate error
sex_results <- tibble(
  movement_model = rep(c("gravitation", 
                         "random", 
                         "distance", 
                         "move_one", 
                         "always_stay"), 
                       each = 1e2), 
  abs_error = unlist(prediction_errors), 
  landscape = "Sexuality and Gender"
)

# - part 09: eclectic ------------------------------------------------------------------------- #

# 1 / measuring distance
distance_mat <-
  matrix(0, ncol = position_length, nrow = position_length)
for (v in 1:nrow(distance_mat)) {
  for (j in 1:ncol(distance_mat)) {
    distance_mat[v, j] <-
      sum(abs(as.numeric(str_split(positions[v], "")[[1]]) -
                as.numeric(str_split(positions[j], "")[[1]]))) + 1
  }
}
rownames(distance_mat) <- colnames(distance_mat) <- positions

# 2 / measuring weight
weights_mat <-
  tibble(positions = positions) |> 
  ## weights from
  left_join(d_pot |> group_by(pos2) |> summarize(n1 = n()), 
            by = c("positions" = "pos2")) |>
  mutate(n1 = ifelse(is.na(n1) == TRUE, 0, n1)) |> 
  ## weights to
  left_join(d_pot |> group_by(pos3) |> summarize(n2 = n()), 
            by = c("positions" = "pos3")) |> 
  mutate(n2 = ifelse(is.na(n2) == TRUE, 0, n2)) |>
  select(n1, n2) |> as.matrix()

# 3 / measuring evidence matrix
evidence_matrix <- 
  matrix(0, nrow = position_length, ncol = position_length)
colnames(evidence_matrix) <- rownames(evidence_matrix) <- positions
for (t in 1:nrow(d_pot)) {
  evidence_matrix[d_pot$pos2[t], 
                  d_pot$pos3[t]] <- 
    evidence_matrix[d_pot$pos2[t], 
                    d_pot$pos3[t]] + 1
}

# 4 / runs for prediction errors
prediction_errors <-
  map(
    c("gravitation",
      "random",
      "distance",
      "move_one",
      "always_stay"),
    calculate_error,
    positions = positions,
    position_length = position_length,
    distance_mat = distance_mat,
    weights_mat = weights_mat,
    position_string = 7,
    num_iterations = 1e2
  )

# 5 / calculate error
pot_results <- tibble(
  movement_model = rep(c("gravitation", 
                         "random", 
                         "distance", 
                         "move_one", 
                         "always_stay"), 
                       each = 1e2), 
  abs_error = unlist(prediction_errors), 
  landscape = "Eclectic"
)

# - part 10: plots ---------------------------------------------------------------------------- #

all_results <- rbind(rel_results, 
                     sex_results, 
                     pot_results)

p2 <- all_results |>
  group_by(landscape, 
           movement_model)|>
  summarise(avg = mean(abs_error), 
            s_d = sd(abs_error), 
            upr = avg + 1.96*s_d, 
            lwr = avg - 1.96*s_d) |>
  mutate(movement_model = case_when(
    movement_model == "random" ~ "Random",
    movement_model == "always_stay" ~ "Always Stay",
    movement_model == "gravitation" ~ "Gravitation",
    movement_model == "distance" ~ "Distance",
    movement_model == "move_one" ~ "1 Move at Most")) |>
  mutate(movement_model = factor(movement_model,
                                 levels = c("Gravitation",
                                            "1 Move at Most",
                                            "Always Stay",
                                            "Distance",
                                            "Random"))) |> 
  mutate(landscape = factor(landscape,
                            levels = c("Religion",
                                       "Sexuality and Gender",
                                       "Eclectic"))) |> 
  ggplot(aes(x = movement_model, y = avg)) +
  geom_point(size = 1) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  labs(title = "Time 2 to Time 3",
       x = "Strategies", y = "Average Prediction Error") + 
  facet_wrap(~landscape) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

### - section 3: tie plots

png("./figures/figure_4.png",
    w = 10, h = 10, units = "in", res = 500)
p1 / p2
dev.off()

# --------------------------------------------------------------------------------------------- #
