#' Random Forest Cross-Validation
#'
#' This function predicts the output body mass by bill length, bill depth,
#'   and flipper length, and computes the cross-validation error using
#'   \code{penguins} data.
#'
#' @param k A numeric input representing the number of folds.
#'
#' @return A numeric output representing the cross-validation error.
#'
#' @examples
#' my_rf_cv(5)
#' my_rf_cv(10)
#'
#' @export
my_rf_cv <- function(k) {
  data2 <- drop_na(my_penguins) %>%
    select(-island, -sex, -year, -species)
  # Randomly categorize the penguins data with different fold.
  fold <- sample(rep(1:k, length = nrow(data2)))
  data2$fold <- fold

  # Store MSE for every fold.
  MSE <- rep(NA, k)
  # Loop through the number of fold.
  for (i in 1:k) {
    # Define the training data and test data.
    train <- data2 %>% dplyr::filter(fold != i) %>% select(-fold)
    test <- data2 %>% dplyr::filter(fold == i) %>% select(-fold)
    # Train the random forest model.
    model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm +
                            flipper_length_mm, data = train, ntree = 100)
    # Use the model and test data to predict the body mass.
    pred <- predict(model, test[, -4])
    # Compute and store MSE.
    MSE[i] <- mean((pred - test[[4]])^2)
  }
  # Return the average MSE.
  return(mean(MSE))
}
