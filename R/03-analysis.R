library(tidyverse)
library(here)
library(tidygraph)
# remotes::install_github("RoheLab/vsp")
library(vsp)
library(tidytext)
library(Matrix)
library(gghighlight)

users <- read_csv(
  here("data/hadley_guido_users.csv"),
  col_types = "ccliiiiiTlccccccccccTTTdddd" # treat user ids as character
)

follows <- read_csv(
  here("data/hadley_guido_edges.csv"),
  col_types = "cc" #  treat user ids as character
)

graph <- tbl_graph(edges = follows) %>%
  left_join(users, by = c("name" = "user_id"))

graph


# this is a hack where i discard some users without descriptions because
# getting a user x description word matrix when you have empty descriptions
# is a PITA

bio_words <- graph %>%
  as_tibble() %>%
  filter(!is.na(description)) %>%
  unnest_tweets(word, description) %>%
  cast_sparse(name, word)

appearances <- colSums(bio_words)

freq_bio_words <- bio_words[, appearances > 5]

graph_have_bio_words <- graph %>%
  activate(nodes) %>%
  filter(name %in% rownames(bio_words))

node_data_have_bio_words <- graph_have_bio_words %>%
  as_tibble()

user_ids <- node_data_have_bio_words %>%
  pull(name)

y_names <- c(
  "rstudio tidyverse team",
  "python language devs",
  "HCI, visualization & graphics",
  "democratic politicians",
  "tech thought leaders",
  "tenured statisticians",
  "r devs from minority groups",
  "ecology and genetics faculty",
  "python using data scientists",
  "python data tool devs",
  "rladies accounts",
  "open science",
  "r devs not at rstudio",
  "tech ceos and vc",
  "tech ethics",
  "open source devs",
  "generative artists",
  "data science managers",
  "tech critique/explainers",
  "jonathan haidt et al"
)

fa_desc <- vsp(
  graph_have_bio_words,
  rank = 20,
  rownames = user_ids,
  colnames = user_ids
) %>% 
  set_y_factor_names(y_names)

plot_ipr_pairs(fa_desc)

### NOTE: if you would like to add yourself to the individual factor loading
#     plot, you can do so below by adding your Twitter screen name to the
#     vector of Twitter handles

y_plot_data <- fa_desc %>%
  set_y_factor_names(y_names) %>%
  get_varimax_y(1:20) %>%
  left_join(node_data_have_bio_words, by = c("id" = "name")) %>%
  rename(full_name = name.y) %>%
  filter(
    screen_name %in% c(
      "hadleywickham", "gvanrossum", "wesmckinn", "eddelbuettel",
      "deaneckles", "timnitGebru", "karlrohe", "sherrirose",
      "daniela_witten", "alexhanna", "raziehnabi", "XiaoLiMeng1", "KLdivergence"
    )
  ) %>%
  pivot_longer(
    one_of(y_names),
    names_to = "factor",
    values_to = "percentile"
  ) %>%
  arrange(desc(factor)) %>%
  mutate(
    factor = forcats::fct_inorder(factor)
  )


y_plot_data %>%
  ggplot(aes(x = full_name, y = factor, fill = percentile)) +
  geom_tile() +
  scale_fill_gradient2(high = "#000000") +
  theme_classic(20) +
  labs(
    title = "Incoming factor (Y) loadings for selected Twitter users",
    fill = "Loading",
    caption = "Higher loadings indicates user and people loading on factor are followed similarly"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  here("slides/figures/loadings-all.pdf"),
  width = 16, height = 16 * 3 / 4, dpi = 100
)

plot_mixing_matrix(fa_desc) +
  theme(axis.text.x = element_text(angle = 90))

plot_varimax_z_pairs(fa_desc, 1:20)
plot_varimax_y_pairs(fa_desc, 1:5)

y_keywords <- bff(fa_desc$Y, freq_bio_words, num_best = 7)
z_keywords <- bff(fa_desc$Z, freq_bio_words, num_best = 7)

y_factor_words <- y_keywords %>%
  mutate(
    id = paste0("Y", vsp:::left_padded_sequence(1:20)),
    keywords = pmap_chr(select(y_keywords, -factor), paste, sep = ", ")
  ) %>%
  rename(name = factor) %>%
  select(id, name, keywords)

z_factor_words <- z_keywords %>%
  mutate(
    id = colnames(fa_desc$Z),
    words = pmap_chr(select(z_keywords, -factor), paste, sep = ", ")
  ) %>%
  select(factor, id, words)

y_factor_words
z_factor_words

nest_to_chr <- function(df) {
  paste(df$name.y, collapse = ", ")
}

y_hubs <- fa_desc %>%
  get_varimax_y(1:20) %>%
  pivot_longer(
    cols = one_of(y_names),
    names_to = "factor",
    values_to = "loading"
  ) %>%
  group_by(factor) %>%
  top_n(n = 7, wt = loading) %>%
  left_join(node_data_have_bio_words, by = c("id" = "name")) %>%
  select(factor, name.y) %>%
  nest(top_accounts = c(name.y)) %>%
  mutate(
    top_accounts = map_chr(top_accounts, nest_to_chr)
  ) %>%
  ungroup() %>%
  arrange(factor) %>%
  mutate(
    Factor = paste0("Y", vsp:::left_padded_sequence(1:20))
  ) %>%
  select(Factor, factor, top_accounts) %>%
  rename("Name" = "factor", "Top Accounts" = "top_accounts")

y_hubs


### check if we need regularization -- seems like we don't 

library(invertiforms)
library(furrr)
library(RSpectra)

plan(multicore, workers = 20)

A <- igraph::as_adjacency_matrix(graph)

num_tau <- 20
tau <- 10^seq(-2, 4, length.out = num_tau)

laplacians <- tibble(tau = tau) %>%
  mutate(
    scaler = future_map(tau, ~ RegularizedLaplacian(A, .x, .x)),
    L_tau = future_map(scaler, ~ transform(.x, A))
  )

decomposed <- laplacians %>%
  mutate(
    svd = future_map(L_tau, RSpectra::svds, k = 20)
  )

cumulative_participation <- function(U) {
  sum(rowSums(U^2)^2)
}

localization <- decomposed %>%
  mutate(
    u = map_dbl(svd, ~ cumulative_participation(.x$u)),
    v = map_dbl(svd, ~ cumulative_participation(.x$v))
  )

avg_deg <- mean(rowSums(A))

localization %>%
  select(-scaler, -L_tau, -svd) %>%
  gather(subspace, localization, u, v) %>%
  ggplot() +
  aes(tau, localization, color = subspace, group = subspace) +
  geom_line() +
  geom_vline(xintercept = avg_deg, linetype = "dashed") +
  scale_color_viridis_d(begin = 0.15, end = 0.85) +
  scale_x_log10() +
  theme_minimal()
