
#' Title
#'
#' @param data
#' @param train_val
#' @param yvar
#' @param xvar1
#' @param xvar2
#' @param xvar3
#'
#' @return data.frame
#' @export
#'
#' @examples
linreg_model <- function(data, train_val, yvar, xvar1, xvar2, xvar3) {

  #Uniformly distributed random number between 0 and 1
  data$train_flag <- stats::runif(nrow(data))
  data$train_flag <-ifelse(data$train_flag > train_val, 1, 0)

  #maybe we should store the number of observations in the test and training data for the user to see?
  num_train <- sum(data$train_flag)
  num_test <- sum(1-data$train_flag)

  #subset training and testing data
  test <- subset(data, train_flag == 1)
  train <- subset(data, train_flag == 0)

  #create linear reg model
  reg <- stats::lm({{yvar}} ~ {{xvar1}} + {{xvar2}} + {{xvar3}}, data=train)

  #Generate predictions for all observations in the test data
  y_test_predictions_ols <- stats::predict(reg, newdata=test)

  #Generate predictions for all observations in the training data
  y_train_predictions_ols <- stats::predict(reg, newdata=train)

  #Generate squared prediction errors

  ols_test <- test |> dplyr::select({{yvar}})
  ols_train <- train |> dplyr::select({{yvar}})

  OLS_performance_testset <- (ols_test - y_test_predictions_ols)^2
  OLS_performance_trainset <- (ols_train - y_train_predictions_ols)^2

  #Report the root mean squared prediction error
  rmspe_test_ols <- sqrt(mean(OLS_performance_testset, na.rm=TRUE))
  rmspe_train_ols <- sqrt(mean(OLS_performance_trainset, na.rm=TRUE))

  rmspe_test_ols
  rmspe_train_ols

  Type <- c("Training", "Test")
  Obs <- c(num_train, num_test)
  Error <- c(rmspe_train_ols, rmspe_test_ols)

  df <- data.frame(Type, Obs, Error)

 return (df)

}
