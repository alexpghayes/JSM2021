library(aPPR)
library(logger)
library(neocache)
library(here)
library(dplyr)
library(readr)

log_threshold(TRACE, namespace = "neocache")

nc_export_all_users("aPPR", here("data/aPPR-Users.csv"))
nc_export_all_follows("aPPR", here("data/aPPR-Follows.csv"))

users <- vroom::vroom(
  here("data/aPPR-Users.csv"),
  delim = ",",
  col_types = "ccliiiiiTlccccccccccTTT"
)

follows <- vroom::vroom(here("data/aPPR-Follows.csv"), delim = ",", col_types = "cc")

tracker <- readr::read_rds(here("data/tracker.rds"))

hadley_guido_users <- users %>% 
  semi_join(tracker$stats, by = c("user_id" = "name")) %>% 
  left_join(tracker$stats, by = c("user_id" = "name")) %>% 
  rename(
    "personal_pagerank_residual" = "r",
    "approximate_personal_pagerank" = "p",
    "degree_adjusted_personal_pagerank" = "degree_adjusted",
    "regularized_personal_pagerank" = "regularized"
  ) %>% 
  select(
    -in_degree,
    -out_degree
  )

write_csv(hadley_guido_users, here("data/hadley_guido_users.csv"))

hadley_guido_edges <- follows %>% 
  semi_join(hadley_guido_users, by = c("from" = "user_id")) %>% 
  semi_join(hadley_guido_users, by = c("to" = "user_id")) 

write_csv(hadley_guido_edges, here("data/hadley_guido_edges.csv"))
