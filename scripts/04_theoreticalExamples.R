
# - Restrepo & Keskintürk --------------------------------------------------------------------- #
# - Illustrations and Model Examples ---------------------------------------------------------- #
# --------------------------------------------------------------------------------------------- #

# - part 01: load packages -------------------------------------------------------------------- #

pacman::p_load(tidyverse, igraph, ggraph, haven, labelled, janitor, hrbrthemes, patchwork)
theme_set(theme_ipsum_rc())

# - part 02: cube ----------------------------------------------------------------------------- #

rm(list = ls()) # clean-up

## preps
adj_m <- matrix(NA, 8, 8)
posdf <- expand_grid(p1 = c(0, 1),
                     p2 = c(0, 1),
                     p3 = c(0, 1)) |>
  mutate(position = paste0(p1, p2, p3)) |> 
  pull(position) |> sort()

## connection matrix
for (i in 1:8) {
  pos1 <- as.numeric(str_split(posdf[i], "")[[1]])
  for (j in 1:8) {
    pos2 <- as.numeric(str_split(posdf[j], "")[[1]])
    abs_value <- sum(abs(pos1 - pos2))
    if (abs_value == 1) {
      adj_m[i, j] <- 1
    } else {
      adj_m[i, j] <- 0
    }
  }
}
colnames(adj_m) <- posdf
rownames(adj_m) <- posdf

## build the graph
d <- graph_from_adjacency_matrix(adj_m,
                                 mode = "undirected")
V(d)$size <- c(13, 5, 3, 2, 4, 2, 3, 14)
layout <- layout_with_kk(d)

## plot 1
p1 <- ggraph(d, layout = layout) +
  geom_edge_link(edge_color = "black") +
  geom_node_label(aes(label = name),
                      color = "black", 
                      family = "Arial Narrow") +
  theme_graph(base_family = "Roboto Condensed") +
  ggtitle("Cube")

## plot 2
p2 <- ggraph(d, layout = layout) +
  geom_edge_link(edge_color = "black") +
  geom_node_point(shape = 21, colour = "black", fill = "white", stroke = 0.75,
                  aes(size = size)) +
  scale_size(range = c(8, 15)) +
  geom_node_text(aes(label = name),
                 color = "black", 
                 family = "Arial Narrow") +
  theme_graph(base_family = "Roboto Condensed") +
  theme(legend.position = "none") +
  ggtitle("Cube with Weights")

png("./figures/figure_1.png",
    w = 12.5, h = 7.5, units = "in", res = 500)
p1 + p2
dev.off()

# - part 04: adjudication --------------------------------------------------------------------- #

rm(list = ls()) # clean-up

## example with three positions
position_string <- 3
positions <-
  expand.grid(replicate(position_string, 0:1, simplify = F)) |>
  as_tibble() |> unite(col = position, sep = "") |>
  pull(position) |> sort()
position_length <- length(positions)

## distance matrix
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

## matrix from in-text example
evidence_matrix <- matrix(c(3 , 1 , 2 , 0 , 2 , 1 , 4 , 0,  
                            0 , 1 , 1 , 0 , 1 , 0 , 2 , 0,   
                            0 , 1 , 1 , 0 , 0 , 0 , 1 , 0,   
                            0 , 1 , 0 , 1 , 0 , 0 , 0 , 0,  
                            0 , 0 , 0 , 2 , 1 , 0 , 1 , 0,   
                            0 , 0 , 0 , 0 , 1 , 0 , 1 , 0,   
                            0 , 2 , 0 , 0 , 0 , 0 , 0 , 1,  
                            0 , 3 , 1 , 2 , 2 , 3 , 2 , 1), 
                          nrow = 8, ncol = 8, byrow = T)
weights_mat <- matrix(c(13, 5, 3, 2, 4, 2, 3, 14,
                        colSums(evidence_matrix)),
                      ncol = 2,
                      nrow = 8)

## function call
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

## runs
set.seed(2808)

prediction_errors <-
  map(
    c("random",
      "move_one"),
    calculate_error,
    positions = positions,
    position_length = position_length,
    distance_mat = distance_mat,
    weights_mat = weights_mat,
    position_string = 3,
    num_iterations = 1e4
  )

# calculate error
pedf <- tibble(
  movement_model = rep(c("random", 
                         "move_one"), 
                       each = 1e4), 
  abs_error = unlist(prediction_errors)
)

png("./figures/figure_2.png",
    w = 8, h = 5, units = "in", res = 500)
pedf|>
  group_by(movement_model)|>
  summarise(avg = mean(abs_error), 
            s_d = sd(abs_error), 
            upr = quantile(abs_error, 0.75), 
            lwr = quantile(abs_error, 0.25))|>
  mutate(movement_model = case_when(
    movement_model == "random" ~ "Random",
    movement_model == "move_one" ~ "1 Move at Most"))|>
  ggplot(aes(x = fct_reorder(movement_model, avg), y = avg)) +
  geom_point(size = 1) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  labs(x = "Strategies", y = "Distribution of Prediction Error")
dev.off()

# --------------------------------------------------------------------------------------------- #
