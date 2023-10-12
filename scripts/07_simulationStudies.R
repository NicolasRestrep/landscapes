
# - Restrepo & Keskintürk --------------------------------------------------------------------- #
# - Simulation Studies ------------------------------------------------------------------------ #
# --------------------------------------------------------------------------------------------- #

# - part 01: load packages & data ------------------------------------------------------------- #

rm(list = ls())
pacman::p_load(tidyverse, janitor, igraph, ggraph, hrbrthemes, patchwork)
theme_set(theme_ipsum_rc())

# - part 02: function calls ------------------------------------------------------------------- #

# calculate distance
calculate_distance_strings <-
  function(pos1, pos2) {
    focal_string <-
      as.numeric(str_split(pos1, "")[[1]])
    target_string <-
      as.numeric(str_split(pos2, "")[[1]])
    abs_value <- abs(focal_string - target_string)
    return(c(distance = sum(abs_value)))
  }

# create unique positions
create_unique_positions <-
  function(position_string = 7) {
    positions <-
      expand.grid(replicate(position_string, 0:1, simplify = F)) |>
      as_tibble() |> unite(col = position, sep = "") |>
      pull(position) |> sort()
    return(positions)
  }

# create hamming space
create_hamming_space <-
  function(positions) {
    adjm <- matrix(NA, length(positions), length(positions))
    for (i in 1:length(positions)) {
      pos1 <- as.numeric(str_split(positions[i], "")[[1]])
      for (j in 1:length(positions)) {
        pos2 <- as.numeric(str_split(positions[j], "")[[1]])
        abs_value <- sum(abs(pos1 - pos2))
        if (abs_value == 1) {
          adjm[i, j] <- 1
        } else {
          adjm[i, j] <- 0
        }
      }
    }
    colnames(adjm) <- positions
    rownames(adjm) <- positions
    d <- graph_from_adjacency_matrix(adjm, mode = "undirected")
    return(d)
  }

# distance matrix
create_distance_matrix <- function(positions) {
  distance_mat <- matrix(0,
                         ncol = length(positions),
                         nrow = length(positions))
  for (v in 1:nrow(distance_mat)) {
    for (j in 1:ncol(distance_mat)) {
      dist_strings <- calculate_distance_strings(pos1 = positions[v],
                                                 pos2 = positions[j])
      distance_mat[v, j] <- dist_strings + 1
    }
  }
  rownames(distance_mat) <- colnames(distance_mat) <- positions
  return(distance_mat)
}

# get initial weights, given size
get_initial_weights <- function(path,
                                position_string) {
  d <- read_rds(path)
  init_weights <- d |>
    select(pos1) |>
    group_by(pos1) |>
    summarise(size = n()) |>
    ungroup() |>
    arrange(sort(pos1))
  possible_positions <-
    tibble(pos1 = create_unique_positions(position_string = position_string))
  init_weights <- possible_positions |>
    left_join(init_weights, by = "pos1")
  init_weights <- init_weights |>
    mutate(size = if_else(is.na(size),
                          0,
                          size)) |>
    arrange(sort(pos1))
  return(init_weights$size)
}

# get "observed" matrix
get_evidence_matrix <- function(position_length,
                                positions,
                                simulated_results) {
  evidence_matrix <-
    matrix(0, nrow = position_length, ncol = position_length)
  colnames(evidence_matrix) <-
    rownames(evidence_matrix) <- positions
  for (t in 1:nrow(simulated_results)) {
    evidence_matrix[simulated_results[t, 1], simulated_results[t, 2]] <-
      evidence_matrix[simulated_results[t, 1],
                      simulated_results[t, 2]] + 1
  }
  return(evidence_matrix)
}

# function for simulation
simulate_movement_model <- function(number_positions,
                                    num_turns,
                                    initial_weights = c(rep(1, times = (2 ^
                                                                          7))),
                                    movement_model) {
  ## generate positions
  positions <-
    create_unique_positions(position_string = number_positions)
  ## generate distance matrix
  distance_mat <-
    create_distance_matrix(positions = positions)
  ## generate landscape graph
  landscape <-
    create_hamming_space(positions = positions)
  ## add initial weights
  V(landscape)$size <- initial_weights
  ## set up the number of agents
  num_agents <- sum(V(landscape)$size)
  ## number of turns
  num_turns <- num_turns
  ## output matrix for bookkeeping
  output_mat <- matrix(NA_character_,
                       ncol = num_turns,
                       nrow = num_agents)
  # initial positions
  output_mat[, 1] <- rep(names(V(landscape)),
                         V(landscape)$size)
  weights_mat <- matrix(NA_real_,
                        ncol = num_turns,
                        nrow = length(positions))
  weights_mat[, 1] <- V(landscape)$size
  
  for (t in 2:num_turns) {
    weights <- weights_mat[, t - 1]
    for (i in 1:num_agents) {
      ## agent's current position
      agent_position <- which(positions == output_mat[i, t - 1])
      
      if (movement_model == "gravitation") {
        probs <-
          (weights / (distance_mat[output_mat[i, t - 1],]) ^ 2) /
          sum((weights / (distance_mat[output_mat[i, t - 1],]) ^ 2))
      } else if (movement_model == "random") {
        probs <-
          c(rep(1 / (2 ^ number_positions), times = (2 ^ number_positions)))
      } else if (movement_model == "distance") {
        probs <-
          ((number_positions + 2) - distance_mat[output_mat[i, t - 1],]) ^
          2 /
          sum(((number_positions + 2) - distance_mat[output_mat[i, t - 1],]) ^
                2)
      } else if (movement_model == "move_one") {
        probs <-
          ifelse(distance_mat[output_mat[i, t - 1],] <= 2, 1 / (number_positions + 1), 0)
      } else {
        probs <-
          ifelse(distance_mat[output_mat[i, t - 1],] == 1, 1, 0)
      }
      ## choose move
      choice <- sample(positions, size = 1, prob = probs)
      ## Record move
      output_mat[i, t] <- choice
    }
    if (length(table(output_mat[, t])) == length(positions)) {
      weights_mat[, t] <-  as.integer(table(output_mat[, t]))
    } else {
      new_weights <- table(output_mat[, t])
      missing_weights <-
        rep(0, length(positions) - length(table(output_mat[, t])))
      names(missing_weights) <-
        setdiff(positions, names(table(output_mat[, t])))
      weights_mat[, t] <-
        c(new_weights, missing_weights)[sort(names(c(new_weights, missing_weights)))]
    }
  }
  return(output_mat)
}

# calculate error
calculate_error <-
  function(init_weights,
           simulated_results,
           movement_model,
           num_iterations,
           position_string)
  {
    ## establish how many positions
    position_length <- 2 ^ position_string
    ## create positions
    positions <-
      create_unique_positions(position_string = position_string)
    ## create distance matrix
    distance_mat <- create_distance_matrix(positions = positions)
    ## create weights matrix
    weights_mat <- matrix(data = init_weights,
                          nrow = nrow(distance_mat),
                          ncol = 2)
    ## create evidence matrix
    evidence_matrix <- get_evidence_matrix(
      position_length = 2 ^ 7,
      positions = create_unique_positions(position_string = position_string),
      simulated_results = simulated_results
    )
    transition_matrix <-
      matrix(NA, ncol = position_length, nrow = position_length)
    colnames(transition_matrix) <- positions
    rownames(transition_matrix) <- positions
    for (i in 1:nrow(transition_matrix)) {
      if (movement_model == "gravitation") {
        transition_matrix[i,] <-
          (weights_mat[, 1] / (distance_mat[positions[i], ]) ^ 2) / 
          sum((weights_mat[, 1] / (distance_mat[positions[i], ]) ^ 2))
      } else if (movement_model == "random") {
        transition_matrix[i,] <-
          c(rep(1 / position_length, position_length))
      } else if (movement_model == "distance") {
        transition_matrix[i,] <-
          ((position_string + 2) - distance_mat[positions[i],]) ^ 2 /
          sum(((position_string + 2) - distance_mat[positions[i],]) ^ 2)
      } else if (movement_model == "move_one") {
        transition_matrix[i,] <-
          ifelse(distance_mat[positions[i],] <= 2, 1 / (position_string + 1), 0)
      } else {
        transition_matrix[i,] <-
          ifelse(distance_mat[positions[i],] == 1, 1, 0)
      }
    }
    abs_errors <- vector(mode = "double", length = num_iterations)
    for (z in 1:num_iterations) {
      realizations_matrix <-
        matrix(NA, ncol = position_length, nrow = position_length)
      colnames(realizations_matrix) <- positions
      rownames(realizations_matrix) <- positions
      for (j in 1:nrow(realizations_matrix)) {
        realizations_matrix[j,] <-
          table(factor(
            sample(
              c(1:position_length),
              size = weights_mat[j, 1],
              prob = transition_matrix[j,],
              replace = T
            ),
            levels = 1:position_length
          ))
      }
      abs_errors[z] <-
        sum(abs(evidence_matrix - realizations_matrix))
    }
    return(abs_errors)
  }

# - part 03: religion ------------------------------------------------------------------------- #

init_weights <- 
  get_initial_weights(path = "./data/cleaned/nsyr_rel.rds", position_string = 7)

generate_iterated_errors <- 
  function(number_iterations, number_sims, init_weights) 
    
    {
    its <- number_iterations
    percentage_changers <- 
      vector(mode = "numeric", length = its)
    pred_error_grav <- 
      vector(mode = "numeric", length = its)
    pred_error_rand <- 
      vector(mode = "numeric", length = its)
    pred_error_dist <- 
      vector(mode = "numeric", length = its)
    pred_error_move_one <- 
      vector(mode = "numeric", length = its)
    pred_error_stay <- 
      vector(mode = "numeric", length = its)
    
    for (z in 1:its) 
      
      {
      simulated_results <-
        simulate_movement_model(
          number_positions = 7,
          num_turns = 2,
          initial_weights = init_weights,
          movement_model = "gravitation"
        )
      percentage_changers[z] <-
        sum(simulated_results[, 1] != simulated_results[, 2]) / nrow(simulated_results)
      prediction_errors <-
        map(
          c("gravitation",
            "random",
            "distance",
            "move_one",
            "always_stay"),
          calculate_error,
          init_weights = init_weights,
          simulated_results = simulated_results,
          position_string = 7,
          num_iterations = number_sims
        )
      pred_error_grav[z] <- mean(prediction_errors[[1]])
      pred_error_rand[z] <- mean(prediction_errors[[2]])
      pred_error_dist[z] <- mean(prediction_errors[[3]])
      pred_error_move_one[z] <- mean(prediction_errors[[4]])
      pred_error_stay[z] <- mean(prediction_errors[[5]])
    }
  
    iterated_results <- tibble(
      iteration_id = 1:its,
      gravitation_error = pred_error_grav,
      random_error = pred_error_rand,
      distance_error = pred_error_dist,
      move_one_error = pred_error_move_one,
      always_stay_error = pred_error_stay,
      percentage_changers = percentage_changers
    )
    return(iterated_results)
}

rel_results <- 
  generate_iterated_errors(init_weights = init_weights, 
                           number_iterations = 50, 
                           number_sims = 1e2) |> 
  mutate(landscape = "Religion")

# - part 04: sexuality ------------------------------------------------------------------------ #

init_weights <- 
  get_initial_weights(path = "./data/cleaned/nsyr_sex.rds", position_string = 7)

generate_iterated_errors <- 
  function(number_iterations, number_sims, init_weights) 
    
  {
    its <- number_iterations
    percentage_changers <- 
      vector(mode = "numeric", length = its)
    pred_error_grav <- 
      vector(mode = "numeric", length = its)
    pred_error_rand <- 
      vector(mode = "numeric", length = its)
    pred_error_dist <- 
      vector(mode = "numeric", length = its)
    pred_error_move_one <- 
      vector(mode = "numeric", length = its)
    pred_error_stay <- 
      vector(mode = "numeric", length = its)
    
    for (z in 1:its) 
      
    {
      simulated_results <-
        simulate_movement_model(
          number_positions = 7,
          num_turns = 2,
          initial_weights = init_weights,
          movement_model = "gravitation"
        )
      percentage_changers[z] <-
        sum(simulated_results[, 1] != simulated_results[, 2]) / nrow(simulated_results)
      prediction_errors <-
        map(
          c("gravitation",
            "random",
            "distance",
            "move_one",
            "always_stay"),
          calculate_error,
          init_weights = init_weights,
          simulated_results = simulated_results,
          position_string = 7,
          num_iterations = number_sims
        )
      pred_error_grav[z] <- mean(prediction_errors[[1]])
      pred_error_rand[z] <- mean(prediction_errors[[2]])
      pred_error_dist[z] <- mean(prediction_errors[[3]])
      pred_error_move_one[z] <- mean(prediction_errors[[4]])
      pred_error_stay[z] <- mean(prediction_errors[[5]])
    }
    
    iterated_results <- tibble(
      iteration_id = 1:its,
      gravitation_error = pred_error_grav,
      random_error = pred_error_rand,
      distance_error = pred_error_dist,
      move_one_error = pred_error_move_one,
      always_stay_error = pred_error_stay,
      percentage_changers = percentage_changers
    )
    return(iterated_results)
  }

sex_results <- 
  generate_iterated_errors(init_weights = init_weights, 
                           number_iterations = 50, 
                           number_sims = 1e2) |> 
  mutate(landscape = "Sexuality and Gender")

# - part 05: eclectic ------------------------------------------------------------------------- #

init_weights <- 
  get_initial_weights(path = "./data/cleaned/nsyr_pot.rds", position_string = 7)

generate_iterated_errors <- 
  function(number_iterations, number_sims, init_weights) 
    
  {
    its <- number_iterations
    percentage_changers <- 
      vector(mode = "numeric", length = its)
    pred_error_grav <- 
      vector(mode = "numeric", length = its)
    pred_error_rand <- 
      vector(mode = "numeric", length = its)
    pred_error_dist <- 
      vector(mode = "numeric", length = its)
    pred_error_move_one <- 
      vector(mode = "numeric", length = its)
    pred_error_stay <- 
      vector(mode = "numeric", length = its)
    
    for (z in 1:its) 
      
    {
      simulated_results <-
        simulate_movement_model(
          number_positions = 7,
          num_turns = 2,
          initial_weights = init_weights,
          movement_model = "gravitation"
        )
      percentage_changers[z] <-
        sum(simulated_results[, 1] != simulated_results[, 2]) / nrow(simulated_results)
      prediction_errors <-
        map(
          c("gravitation",
            "random",
            "distance",
            "move_one",
            "always_stay"),
          calculate_error,
          init_weights = init_weights,
          simulated_results = simulated_results,
          position_string = 7,
          num_iterations = number_sims
        )
      pred_error_grav[z] <- mean(prediction_errors[[1]])
      pred_error_rand[z] <- mean(prediction_errors[[2]])
      pred_error_dist[z] <- mean(prediction_errors[[3]])
      pred_error_move_one[z] <- mean(prediction_errors[[4]])
      pred_error_stay[z] <- mean(prediction_errors[[5]])
    }
    
    iterated_results <- tibble(
      iteration_id = 1:its,
      gravitation_error = pred_error_grav,
      random_error = pred_error_rand,
      distance_error = pred_error_dist,
      move_one_error = pred_error_move_one,
      always_stay_error = pred_error_stay,
      percentage_changers = percentage_changers
    )
    return(iterated_results)
  }

pot_results <- 
  generate_iterated_errors(init_weights = init_weights, 
                           number_iterations = 50, 
                           number_sims = 1e2) |> 
  mutate(landscape = "Eclectic")

# - part 06: plots ---------------------------------------------------------------------------- #

all_sim_results <- rbind(rel_results, 
                         sex_results, 
                         pot_results)

# plot 1
png("./figures/figure_5.png",
    w = 10, h = 5, units = "in", res = 500)
all_sim_results |> 
  pivot_longer(2:6,
               names_to = "model", 
               values_to = "median_error") |> 
  group_by(landscape,
           model) |>
  summarize(
    avg = mean(median_error), 
    s_d = sd(median_error), 
    upr = avg + 1.96*s_d, 
    lwr = avg - 1.96*s_d
  ) |> 
  mutate(movement_model = case_when(
    model == "random_error" ~ "Random",
    model == "always_stay_error" ~ "Always Stay",
    model == "gravitation_error" ~ "Gravitation",
    model == "distance_error" ~ "Distance",
    model == "move_one_error" ~ "1 Move at Most")
  ) |>
  mutate(landscape = factor(landscape,
                            levels = c("Religion",
                                       "Sexuality and Gender",
                                       "Eclectic"))) |> 
  ggplot(aes(x = fct_reorder(movement_model, avg), y = avg)) +
  geom_point(size = 1) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  labs(x = "Strategies", y = "Average Prediction Error") + 
  facet_wrap(~landscape) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
dev.off()

# plot 2
png("./figures/figure_6.png",
    w = 8, h = 6, units = "in", res = 500)
all_sim_results |> 
  pivot_longer(2:6,
               names_to = "model", 
               values_to = "median_error") |> 
  group_by(landscape) |> 
  summarize(
    avg = mean(percentage_changers), 
    s_d = sd(percentage_changers), 
    upr = avg + 1.96*s_d, 
    lwr = avg - 1.96*s_d
  ) |> 
  mutate(landscape = factor(landscape,
                            levels = c("Religion",
                                       "Sexuality and Gender",
                                       "Eclectic"))) |> 
  ggplot(aes(x = fct_reorder(landscape,avg), y = avg)) +
  geom_point(size = 1) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  labs(x = "Strategies", y = "% Changers")
dev.off()

# --------------------------------------------------------------------------------------------- #
