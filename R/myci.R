#' myci
#'
#' @param x - data sample
#'
#' @return ci a ci interval
#' @export
#'
#' @examples
myci <- function(x) {
  n <- length(x)
  sample_mean <- mean(x)
  sample_sd <- sd(x)
  alpha <- 0.05  # 1 - Confidence Level

  # Calculate critical t-value
  t_critical <- qt(1 - alpha / 2, df = n - 1)

  # Calculate standard error
  standard_error <- sample_sd / sqrt(n)

  # Calculate margin of error
  margin_of_error <- t_critical * standard_error

  # Calculate confidence interval
  ci[1] <- sample_mean - margin_of_error
  ci[2] <- sample_mean + margin_of_error

  # Return the result
  return(ci)
}
