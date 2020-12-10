#' k-Nearest Neighbors Cross-Validation
#'
#' This function predicts the output class and computes the cross-validation
#'   misclassification error.
#'
#' @param train A data frame with training data.
#' @param cl True class values of the training data.
#' @param k_nn A numeric input representing the number of neighbors.
#' @param k_cv A numeric input representing the number of folds.
#'
#' @return A list of predicted class for all observations and CV error.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "select"
#' @import stats dplyr magrittr randomForest class tidyr
#'
#' @examples
#' \dontrun{
#' data <- tidyr::drop_na(my_penguins) %>% dplyr::select(-island, -sex, -year)
#' train <- data %>% dplyr::select(-species)
#' species <- data$species
#' my_knn_cv(train, species, 1, 5)
#' my_knn_cv(train, species, 5, 5)
#' }
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # Randomly categorize the training data with different fold.
  fold <- sample(rep(1:k_cv, length = length(cl)))
  train$fold <- fold

  # Store the the misclassification rates
  mis_rate <- rep(NA, k_cv)
  # Loop through the number of fold and compute the misclassification rate.
  for (i in 1:k_cv) {
    data_train <- train %>% dplyr::filter(fold != i) %>% select(-fold)
    data_test <- train %>% dplyr::filter(fold == i) %>% select(-fold)
    cl_train <- cl[fold != i]
    cl_test <- cl[fold == i]
    # Predict the class of a certain fold using all other folds as the
    # training data.
    pred_vec <- knn(data_train, data_test, cl_train, k_nn)
    # Record the misclassification rate
    mis_rate[i] <- mean(pred_vec != cl_test)
  }

  train <- train %>% select(-fold)
  # Predict the class of the full data as both the training and the test data.
  class <- knn(train, train, cl, k_nn)
  # Compute the average misclassification rate.
  cv_error <- mean(mis_rate)
  # Return a list of predicted class and CV error.
  return(list(class = class, cv_error = cv_error))
}
