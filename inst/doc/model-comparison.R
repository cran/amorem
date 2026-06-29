## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width = 6,
  fig.height = 3.5
)

## -----------------------------------------------------------------------------
library(amorem)

## -----------------------------------------------------------------------------
data(classroom_events)
data(classroom_actors)
nrow(classroom_events)
head(classroom_events, 3)
table(classroom_actors$role)

## -----------------------------------------------------------------------------
specs <- list(
  count        = c("reciprocity_count",
                   "transitivity_count"),
  continuous   = c("reciprocity_time_recent",
                   "transitivity_time_recent"),
  interrupted  = c("reciprocity_time_recent_interrupted",
                   "transitivity_time_recent_interrupted"))

## -----------------------------------------------------------------------------
res <- compare_models(classroom_events, specs, seed = 11)
res

## ----eval = requireNamespace("survival", quietly = TRUE)----------------------
compare_models(classroom_events, specs,
               n_controls = 3, seed = 11)

## -----------------------------------------------------------------------------
stat_set <- specs$interrupted
cc <- sample_non_events(classroom_events, n_controls = 1,
                        scope = "all", mode = "one", seed = 11)
cc_feat <- endogenous_features(cc, stats = stat_set)
for (st in stat_set) cc_feat[[st]][is.na(cc_feat[[st]])] <- 0

cases <- cc_feat[cc_feat$event == 1L, ]
ctrls <- cc_feat[cc_feat$event == 0L, ]
cases <- cases[order(cases$stratum), ]
ctrls <- ctrls[order(ctrls$stratum), ]

df <- data.frame(
  one      = rep(1, nrow(cases)),
  d_rec    = cases[[stat_set[1]]] - ctrls[[stat_set[1]]],
  d_trans  = cases[[stat_set[2]]] - ctrls[[stat_set[2]]])

fit <- glm(one ~ d_rec + d_trans - 1, family = "binomial", data = df)
summary(fit)$coefficients

## -----------------------------------------------------------------------------
set.seed(2026)
sim <- simulate_relational_events(
  n_events = 600,
  senders   = LETTERS[1:8],
  receivers = LETTERS[1:8],
  baseline_rate = 1,
  allow_loops = FALSE,
  endogenous_stats   = c("reciprocity_count", "transitivity_count"),
  endogenous_effects = c(reciprocity_count = 0.4, transitivity_count = 0.0))

# Among these three specs, the "count" spec is the true generative
# process. compare_models() should rank it first.
res2 <- compare_models(sim, specs, seed = 7)
res2

