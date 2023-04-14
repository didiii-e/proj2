#' Takes a dataset, splits it into training and testing data and generates a linear regression model,
#' then calculates the error between the model's prediction and the actual value for both the training and testing data
#'
#' @param data A data frame
#' @param train_val  proportion of the data set to use as training for the model
#' @param yvar variable being predicted by model
#' @param xvar1 prediction variable to generate the model
#' @param xvar2 prediction variable to generate the model
#' @param xvar3 prediction variable to generate the model
#'
#' @return data.frame
#' @export
#'
#' @examples
linreg_model <- function(data, train_val = .8, yvar, xvar1, xvar2, xvar3) {

  #######error check
  # Add in error if not character or factor for x
  yvar_data <- data |>
    dplyr::pull({{ yvar }})
  xvar1_data <- data |>
    dplyr::pull({{ xvar1 }})
  xvar2_data <- data |>
    dplyr::pull({{ xvar2 }})
  xvar3_data <- data |>
    dplyr::pull({{ xvar3}})

  if(!(is.numeric(yvar) | is.integer(yvar))){
    stop('y variable needs to of class numeric or integer')
  }

  if(!(is.numeric(xvar1) | is.integer(xvar1))){
    stop('first x variable needs to of class numeric or integer')
  }

  if(!(is.numeric(xvar2) | is.integer(xvar2))){
    stop('second x variable needs to of class numeric or integer')
  }

  if(!(is.numeric(xvar3) | is.integer(xvar3))){
    stop('third x variable needs to of class numeric or integer')
  }

  if(train_val > 1 | train_val < 0) {
    stop('train_val value must be between 0 and 1')
  }


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
