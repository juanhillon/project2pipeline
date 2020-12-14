#' Random forest cross validation functions
#'
#' This function predicts output with cv error based on the number of folds,
#'   this function works with \code{my_penguins}.
#'
#' @param k Numeric nunmber of folds.
#' @keywords prediction
#'
#' @return A numeric with the cross validation error.
#'
#' @importFrom dplyr filter
#' @importFrom randomForest randomForest
#' @importFrom stats predict
#' @importFrom utils data
#' @importFrom tidyr drop_na
#'
#' @examples
#' data(my_penguins)
#' my_data <- tidyr::drop_na(my_penguins)
#' my_rf_cv(2)
#'
#' @export
library(randomForest)
my_rf_cv <- function(k){
  #creates vector to store cross validation errors
  cv_errors_2 <- rep(NA, k)
  #gives each observation a fold
  fold <- sample(rep(1:k, length = nrow(my_data)))
  my_data$fold <- fold
  for (i in 1:k) {
    #creates training data out of data not in ith fold
    data_train <- my_data %>% dplyr::filter(fold != i)
    data_test <- my_data %>% dplyr::filter(fold == i)
    #creates model with randomForest()
    my_model <- randomForest(
      body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
      data = data_train,
      ntree = 100
    )
    #creates vector of predictions
    predictions_2 <- predict(my_model, data_test[, -1])
    cv_errors_2[i] <- mean((predictions_2 - data_test$body_mass_g)^2)
  }
  #returns mean standard error
  return(mean(cv_errors_2))
}
