## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
set.seed(2026)

## ----setup--------------------------------------------------------------------
library(amorem)

## ----data---------------------------------------------------------------------
data("dist_matrix", package = "amorem")

states <- rownames(dist_matrix)
n_states <- length(states)

# Convert metres → log-units, scaled to a usable range.
dist_log <- log(dist_matrix / 1e5 + 1)

## ----simulate-----------------------------------------------------------------
true_dist_effect <- sin(-dist_log / 1.5)

cc <- simulate_relational_events(
  n_events            = 600,
  senders             = states,
  receivers           = states,
  contribution_logits = true_dist_effect,
  baseline_rate       = 1,
  allow_loops         = FALSE,
  n_controls          = 1,
  endogenous_stats    = "reciprocity_exp_decay",
  endogenous_effects  = c(reciprocity_exp_decay = 0.4),
  half_life           = 2,
  risk                = "remove"
)
nrow(cc)
head(cc)

## ----unique-check-------------------------------------------------------------
events_only <- cc[cc$event == 1L, ]
nrow(events_only)
any(duplicated(paste(events_only$sender, events_only$receiver)))

## ----recovery, fig.alt = "recovered smooth distance effect"-------------------
get_dist <- function(s, r) {
  dist_log[cbind(match(s, states), match(r, states))]
}
cc$dist_val <- mapply(get_dist, cc$sender, cc$receiver)

cases    <- cc[cc$event == 1L, ]
controls <- cc[cc$event == 0L, ]
cases    <- cases[order(cases$stratum), ]
controls <- controls[order(controls$stratum), ]

fit_df <- data.frame(
  y          = 1,
  delta_dist = cases$dist_val - controls$dist_val,
  delta_r    = cases$reciprocity_exp_decay - controls$reciprocity_exp_decay
)

if (requireNamespace("mgcv", quietly = TRUE)) {
  library(mgcv)
  fit <- gam(y ~ s(delta_dist) + delta_r - 1, family = binomial, data = fit_df)
  summary(fit)

  x_grid <- seq(min(fit_df$delta_dist), max(fit_df$delta_dist), length.out = 200)
  pred   <- predict(fit,
                    newdata = data.frame(delta_dist = x_grid, delta_r = 0),
                    type = "link")
  plot(x_grid, pred, type = "l", lwd = 2,
       xlab = expression(Delta ~ "log-distance"),
       ylab = "estimated effect",
       main = "GAM smooth for distance (event - control)")
  abline(h = 0, lty = 2, col = "grey60")
}

