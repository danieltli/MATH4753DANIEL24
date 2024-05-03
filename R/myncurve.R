#' @name myncurve
#' @title Plot a Normal Distribution Curve and Calculate Probability
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the normal distribution.
#' @export
#' @param a Upper limit of integration for calculating the probability.
#' @return A list containing the mean (mu), standard deviation (sigma), and
#' probability (P(X <= a)).
#' @examples
#' myncurve(0, 1, 1.96)


myncurve = function(mu, sigma, a){
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma))

  xcurve <- seq(mu - 3 * sigma, a, length.out = 1000)
  ycurve <- dnorm(xcurve, mu, sigma)
  polygon(x = c(mu - 3 * sigma, xcurve, a), y = c(0, ycurve, 0), col = "skyblue")

  probability <- pnorm(a, mean = mu, sd = sigma)

  list(probability = probability)

}
