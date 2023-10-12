
# - Restrepo & Keskintürk --------------------------------------------------------------------- #
# - Plot the Structure of the Landscapes ------------------------------------------------------ #
# --------------------------------------------------------------------------------------------- #

# - part 01: load packages -------------------------------------------------------------------- #

rm(list = ls())
pacman::p_load(tidyverse, igraph, ggraph, hrbrthemes, patchwork)
theme_set(theme_ipsum_rc())

# - part 02: data and preps ------------------------------------------------------------------- #

d_rel <- readRDS("./data/cleaned/nsyr_rel.rds")
d_sex <- readRDS("./data/cleaned/nsyr_sex.rds")
d_pot <- readRDS("./data/cleaned/nsyr_pot.rds")

# define the position universe
position_string <- 7
positions <- 
  expand.grid(
    replicate(position_string, 0:1, simplify = F)) |>
  as_tibble() |> unite(col = position, sep = "") |> pull(position) |> sort()

# hamming function
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

# - part 03: measure weight ------------------------------------------------------------------- #

# religion
weights_mat1 <-
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
rel_landscape <- create_hamming_space(positions)
V(rel_landscape)$size <- weights_mat1[, 1]

# sex
weights_mat2 <-
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
sex_landscape <- create_hamming_space(positions)
V(sex_landscape)$size <- weights_mat2[, 1]

# eclectic
weights_mat3 <-
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
pot_landscape <- create_hamming_space(positions)
V(pot_landscape)$size <- weights_mat3[, 1]

# - part 04: plots ---------------------------------------------------------------------------- #

set.seed(11235)

p1 <- ggraph(rel_landscape) +
  geom_edge_link(edge_color = "black", alpha = 0.25) +
  geom_node_point(
    shape = 21,
    colour = "black",
    fill = "white",
    stroke = 0.75,
    aes(size = size)
  ) +
  scale_size(range = c(1, 25)) +
  theme_graph(base_family = "Roboto Condensed") +
  theme(legend.position = "none") +
  ggtitle("Religion")

p2 <- ggraph(sex_landscape) +
  geom_edge_link(edge_color = "black", alpha = 0.25) +
  geom_node_point(
    shape = 21,
    colour = "black",
    fill = "white",
    stroke = 0.75,
    aes(size = size)
  ) +
  scale_size(range = c(1, 25)) +
  theme_graph(base_family = "Roboto Condensed") +
  theme(legend.position = "none") +
  ggtitle("Sexuality and Gender")

p3 <- ggraph(pot_landscape) +
  geom_edge_link(edge_color = "black", alpha = 0.25) +
  geom_node_point(
    shape = 21,
    colour = "black",
    fill = "white",
    stroke = 0.75,
    aes(size = size)
  ) +
  scale_size(range = c(1, 25)) +
  theme_graph(base_family = "Roboto Condensed") +
  theme(legend.position = "none") +
  ggtitle("Eclectic")

png("./figures/figure_3.png",
    w = 12.5, h = 15, units = "in", res = 500)
(p1 / p2) / p3
dev.off()

# --------------------------------------------------------------------------------------------- #
