#' Fitting Linear Models
#'
#' This function is used to fit linear models.
#'
#' @param formula A formula input that describes the model to be fitted.
#' @param data A data frame input containing the variables in the model.
#'
#' @return A table with columns for Estimate, Std. Error, t value, Pr(>|t|),
#'   and rows for each coefficient.
#'
#' @import ggplot2
#'
#' @examples
#' my_lm(mpg ~ hp*wt, mtcars)
#' my_lm(mpg ~ wt, mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  # Extract a model frame object.
  frame <- model.frame(formula, data)
  # Extract the model matrix.
  matrix <- model.matrix(formula, frame)
  # Extract the model response.
  response <- model.response(frame)
  # Calculate the coefficients.
  beta <- solve(t(matrix) %*% matrix) %*% t(matrix) %*% response
  # Calculate the degree of freedom.
  df <- nrow(matrix) - ncol(matrix)
  # Calculate the variance.
  variance <- sum((response - matrix %*% beta)^2 / df)
  # Calculate the standard error.
  se <- sqrt(diag(variance * solve(t(matrix) %*% matrix)))
  # Calculate the t-value.
  t_value <- beta / se
  # Calculate the p-value of the two-sided t test.
  Pr <- 2 * pt(abs(t_value), df, lower.tail = FALSE)
  # Combine all columns.
  my_result <- cbind(beta, se, t_value, Pr)
  # Change column names.
  colnames(my_result) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  # Return the table.
  return(as.table(my_result))
}
