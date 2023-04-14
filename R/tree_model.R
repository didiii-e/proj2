#' Splits dataset into training and testing data, and generates a decision tree model to predict some variable,
#' and calculates the error between the model's predicted value and the actual value
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
tree_model <- function(data, train_val = .8, yvar, xvar1, xvar2, xvar3) {

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

  if(!(is.numerical(yvar) | is.integer(yvar))){
    stop('y variable needs to of class numeric or integer')
  }

  if(!(is.numerical(xvar1) | is.integer(xvar1))){
    stop('first x variable needs to of class numeric or integer')
  }

  if(!(is.numerical(xvar2) | is.integer(xvar2))){
    stop('second x variable needs to of class numeric or integer')
  }

  if(!(is.numerical(xvar3) | is.integer(xvar3))){
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

  tree <- rpart::rpart({{yvar}} ~ {{xvar1}} + {{xvar2}} + {{xvar3}},
                data=train,
                maxdepth = 3,
                cp=0)

  #Visualize the fitted decision tree
  #plot(tree, margin = 0.2) # plot tree
  #text(tree, cex = 0.5) # add labels to tree

  #Calculate predictions for all rows in test and training samples
  y_test_predictions_tree <- predict(tree, newdata=test)
  y_train_predictions_tree <- predict(tree, newdata=train)

  #Generate squared prediction errors
  tree_performance_testset <- (test$yvar - y_test_predictions_tree)^2
  tree_performance_trainset <- (train$yvar - y_train_predictions_tree)^2

  #Report the root mean squared prediction error
  rmspe_test_tree <- sqrt(mean(tree_performance_testset, na.rm=TRUE))
  rmspe_train_tree <- sqrt(mean(tree_performance_trainset, na.rm=TRUE))

  #Report the root mean squared prediction error
  rmspe_test_tree
  rmspe_train_tree

  Type <- c("Training", "Test")
  Obs <- c(num_train, num_test)
  Error <- c(rmspe_train_tree, rmspe_test_tree)

  df <- data.frame(Type, Obs, Error)

  return (df)

}
