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
tree_model <- function(data, train_val, yvar, xvar1, xvar2, xvar3) {

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
