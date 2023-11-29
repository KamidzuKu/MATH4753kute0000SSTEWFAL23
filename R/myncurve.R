#' mync
#'
#' @param mu
#' @param sigma
#' @param a
#'
#' @return
#' @export
#'@importFrom graphics curve polygon text
#'@importFrom stats dnorm pnorm
#' @examples test <- myncurve(mu=7,sigma=2,a=5)
myncurve = function(mu, sigma, a) {
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma))

  xcurve = seq(mu - 3 * sigma, a, length = 1000)
  ycurve = dnorm(xcurve, mean = mu, sd = sigma)
  polygon(c(mu - 3 * sigma, xcurve, a), c(0, ycurve, 0), col = "Red")

  prob = pnorm(a, mean = mu, sd = sigma)
  prob = round(prob, 4)
  text(mu - 2 * sigma, 0.5 * dnorm(mu, mean = mu, sd = sigma), paste0("Area= ", prob))
  prob
}

