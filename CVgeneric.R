CVgeneric <- function(class_fn, tr_feature, tr_label, K=10, loss_fn) {
  # AIC BIC? Yu Bin says 0-1 loss is good.
  # check inputs
  # features have to be pre-selected (input only the helpful features)
  # tr_feature must be a data.frame with valid column names
  tr_label <- as.factor(tr_label)
  
  # create folds
  n <- nrow(tr_feature)
  d <- ncol(tr_feature)
  fold_index <- createFolds(c(1:n), k=K, list = TRUE)
  
  # fit model & report CV
  CV_result <- data.frame()
  for (i in 1:K) {
    # create test data set
    test_index <- fold_index[[i]]
    temp_test_ft <- tr_feature[test_index,]
    temp_test_lb <- tr_label[test_index]
    temp_test <- cbind(temp_test_ft, temp_test_lb)
    colnames(temp_test) <- c(colnames(temp_test_ft), "expert")
    # create training data set
    temp_train_ft <- tr_feature[-test_index,]
    temp_train_lb <- tr_label[-test_index]
    temp_train <- cbind(temp_train_ft, temp_train_lb)
    colnames(temp_train) <- c(colnames(temp_train_ft), "expert")
    
    # train the model using training set:
    #temp_model <- class_fn(data = temp_train, expert ~.)
    #temp_pred <- predict(temp_model, temp_test_ft)$class
    temp_pred <- class_fn(temp_train, temp_test_ft)
    #temp_model <- train(expert ~., data = temp_train, method = class_fn)
    #temp_pred <- predict(temp_model, temp_test_ft)
    
    # apply loss function (comparing true labels & predicted labels for the testing set):
    temp_loss <- loss_fn(temp_test_lb, temp_pred)
    # update the CV_result
    CV_result <- rbind(CV_result, temp_loss)
  }
  mean_result <- round(mean(CV_result[,1]), digit = 5)
  CV_result <- rbind(round(CV_result, digit = 5), "-------" ,mean_result)
  rownames(CV_result) <- c((1:K), "-------" ,"average")
  return(CV_result)
}