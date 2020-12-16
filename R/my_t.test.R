#' One Sample t-test Function
#'
#' This function performs a one sample t-test.
#'
#' @param x A numeric vector of data.
#' @param alternative A character string specifying the alternative hypothesis.
#' @param mu A numeric input indicating the null hypothesis value of the mean.
#' @keywords inference
#'
#' @return A list with elements: the test statistic, the degrees of freedom,
#'   the value of \code{alternative}, and the numeric p-value.
#'
#' @examples
#' my_t.test(c(1,2,5,7), "two.sided", 0)
#' my_t.test(c(0,1,2,5,7,-2), "greater", 2)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # Calculate the standard error.
  standard_error <- sd(x) / sqrt(length(x))
  # Calculate the test statistic.
  test_stat <- (mean(x) - mu) / standard_error
  # Calculate the degree of freedom.
  df <- length(x) -1
  # If it is left-sided hypothesis.
  if (alternative == "less") {
    # Calculate the p-value.
    p_val <- pt(test_stat, df, lower.tail = TRUE)
    # If it is two-sided hypothesis.
  } else if (alternative == "two.sided") {
    # Calculate the p-value.
    p_val <- 2 * pt(abs(test_stat), df, lower.tail = FALSE)
    # If it is right-sided hypothesis.
  } else if (alternative == "greater") {
    # Calculate the p-value.
    p_val <- pt(test_stat, df, lower.tail = FALSE)
    # Otherwise, throw an error.
  } else {
    stop("Please type \"two.sided\", \"less\", or \"greater\".")
  }
  # Return the list.
  return (list("test_stat" = test_stat, "df" = df,
               "alternative" = alternative, "p_val" = p_val))
}
