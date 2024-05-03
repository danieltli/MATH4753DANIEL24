ntickets <- function(N, gamma, p) {
  # Calculate probabilities for discrete case
  n_range <- N:(N*1.2)
  prob_discrete <- 1 - gamma - pbinom(N, n_range, p)

  # Find optimal n in discrete case
  n_optimal <- which.min(abs(prob_discrete))
  n_value <- n_range[n_optimal]
  layout(matrix(1:2, ncol=1, nrow=2))
  # Plot for discrete case
  plot(x = n_range,
       y = prob_discrete,
       xlab = "n",
       xlim = c(N, N*1.1),
       ylim = c(0, 1),
       pch = 21,
       bg = "cadetblue3",
       type = "b",
       lty = 1,
       main = "Discrete Version: Find n number of optimal tickets sold"
  )
  abline(v = n_value, col = "purple4")

  f2 <- function(k) {
    1 - gamma - pnorm(N + 0.05, mean = k * p, sd = sqrt(k * p * (1 - p)))
  }

  res <- uniroot(f2, interval = c(N, N*1.3))
  k_value <- res$root

  # Plot for continuous case
  plot(function(x) f2(x), xlim = c(N, N*1.1),
       main = "Continuous Version: Find n number of optimal tickets",
       xlab = "k", ylab = "")
  abline(v = k_value, col = "purple4")

  list(nd = n_value, nc = k_value, N = N, p = p, gamma = gamma)
}
