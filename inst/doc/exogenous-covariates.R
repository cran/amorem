## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.width = 6,
    fig.height = 4
)

## -----------------------------------------------------------------------------
library(amorem)

## -----------------------------------------------------------------------------
data("dist_matrix", package = "amorem")

# log-transform to compress the range
dist_log <- log(dist_matrix / 100000 + 1)

## -----------------------------------------------------------------------------
true_effect <- sin(-dist_log / 1.5)

## ----true-effect-curve--------------------------------------------------------
d_seq <- seq(0, max(dist_log), length.out = 200)
plot(d_seq, sin(-d_seq / 1.5),
    type = "l", lwd = 2, col = "red",
    xlab = "log-distance", ylab = "f(d)",
    main = "True non-linear distance effect"
)

## -----------------------------------------------------------------------------
set.seed(42)

states <- rownames(dist_matrix)

events <- simulate_relational_events(
    n_events        = 800,
    senders         = states,
    receivers       = states,
    contribution_logits = true_effect,
    allow_loops     = FALSE,
    n_controls      = 1
)

head(events)

## ----fit-gam------------------------------------------------------------------
library(mgcv)

get_dist <- function(s, r) {
    dist_log[cbind(match(s, states), match(r, states))]
}

events$dist_val <- mapply(get_dist, events$sender, events$receiver)

cases <- events[events$event == 1, ]
controls <- events[events$event == 0, ]
cases <- cases[order(cases$stratum), ]
controls <- controls[order(controls$stratum), ]

fit_df <- data.frame(
    y          = 1,
    delta_dist = cases$dist_val - controls$dist_val
)

fit <- gam(y ~ s(delta_dist) - 1, family = binomial, data = fit_df)
summary(fit)

## ----effect-plot--------------------------------------------------------------
x_grid <- seq(min(fit_df$delta_dist), max(fit_df$delta_dist), length.out = 300)
pred <- predict(fit, newdata = data.frame(delta_dist = x_grid), type = "link")

plot(x_grid, pred,
    type = "l", lwd = 2,
    xlab = expression(Delta ~ "log-distance"),
    ylab = "Estimated effect",
    main = "GAM-recovered smooth vs true effect"
)
abline(h = 0, lty = 2, col = "grey50")

