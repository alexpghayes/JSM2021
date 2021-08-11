library(tidyverse)
library(here)
library(rtweet)

users <- read_csv(
  here("data/hadley_guido/hadley_guido_users.csv"),
  col_types = "ccliiiiiTlccccccccccTTTdddd" # treat user ids as character
)

users

?get_timeline()

user_subset <- users %>%
  slice_max(approximate_personal_pagerank, n = 5000)

timelines <- get_timeline(user_subset$user_id, n = 1000, retryonratelimit = TRUE)

write_rds(timelines, here("data/hadley_guido/timelines.rds"))

timelines <- read_rds(here("data/hadley_guido/timelines.rds"))

library(tidytext)


tidy_tweets <- timelines %>%
  select(screen_name, text) %>%
  unnest_tweets(word, text) %>%
  anti_join(stop_words, by = "word")

tidy_tweets %>%
  count(word, sort = TRUE)

# https://juliasilge.com/blog/sherlock-holmes-stm/
# https://juliasilge.com/blog/evaluating-stm/
# https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf


library(quanteda)
library(stm)

tweets_sparse <- tidy_tweets %>%
  count(screen_name, word, sort = TRUE) %>%
  cast_sparse(screen_name, word, n)

tweets_sparse

topic_model <- stm(tweets_sparse, K = 20, init.type = "Spectral", verbose = FALSE)

summary(topic_model)


td_beta <- tidy(topic_model)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(
    topic = paste0("Topic ", topic),
    term = reorder_within(term, beta, topic)
  ) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    x = NULL, y = expression(beta),
    title = "Highest word probabilities for each topic",
    subtitle = "Different words are associated with different topics"
  ) +
  theme_classic()


td_gamma <- tidy(topic_model,
  matrix = "gamma",
  document_names = rownames(tweets_sparse)
)

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~topic, ncol = 3) +
  labs(
    title = "Distribution of document probabilities for each topic",
    subtitle = "Each topic is associated with 1-3 stories",
    y = "Number of tweets", x = expression(gamma)
  )


library(furrr)

plan(multisession, workers = 20)

many_models <- tibble(
  K = c(2, 5, 10, 20)
) %>%
  mutate(topic_model = future_map(K, ~ stm(tweets_sparse, K = ., verbose = FALSE)))

many_models

heldout <- make.heldout(tweets_sparse)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, tweets_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, tweets_sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result

k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicate that a good number of topics would be around 60") +
  theme_minimal()

k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  unnest(cols = c(exclusivity, semantic_coherence)) %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")


















