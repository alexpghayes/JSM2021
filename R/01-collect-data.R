library(aPPR)
library(neocache)
library(glue)
library(here)

# NOTE: you need some hotfixes to rtweet and neo4r for this to work at the
# moment

# pak::pkg_install("alexpghayes/rtweet@tmp_fix")
# pak::pkg_install("alexpghayes/neo4r@tmp_fix")

set.seed(26)

tracker <- appr(
  neocache_graph(),
  seed = c("hadleywickham", "gvanrossum"),
  epsilon = 1e-6
)

write_rds(tracker, here(glue("data/tracker.rds")))
