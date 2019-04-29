#' @title assign color
#' @description map the numeric labels into color characters (using same color as the paper)
#' @param v numeric vector from expert feature of an image. eg:(image1$expert)
#' @return a vecotr of color characters
num2col <- function(v){
  color_vector <- character(length = length(v))
  color_vector[v == 1] <- "white"
  color_vector[v == 0] <- "black"
  color_vector[v == -1] <- "gray"
  return(color_vector)
}

#' @title assign class
#' @description map the numeric values into character class labels
#' @param v numeric vector from expert feature of an image. eg:(image1$expert)
#' @return a vector of class labels
num2class <- function(v){
  class_vector <- character(length = length(v))
  class_vector[v == 1] <- "cloudy"
  class_vector[v == 0] <- "unlabeled"
  class_vector[v == -1] <- "clear"
  return(class_vector)
}

#' @title percent
#' @description convert numeric vectors into character vectors, describing the percentage
#' @param x numeric vector
#' @return percentage vector (character)
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#' @title plot histogram of features
#' @description plot histogram of features by class
#' @param input_image data.frame of an image
#' @return a list of ggplot objects
feature_hist_by_class <- function(input_image, spec = "") {
  # other features
  NDAI <- ggplot(data=input_image, aes(x=NDAI, color=num2class(expert)))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of NDAI", spec, sep = " "))
  SD <- ggplot(data=input_image, aes(x=SD, color=num2class(expert)))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of SD", spec, sep = " "))
  CORR <- ggplot(data=input_image, aes(x=CORR, color=num2class(expert)))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of CORR", spec, sep = " "))
  # radiance reading
  DF <- ggplot(data=input_image, aes(x=DF, color=num2class(expert)))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of DF", spec, sep = " "))
  CF <- ggplot(data=input_image, aes(x=CF, color=num2class(expert)))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of CF", spec, sep = " "))
  BF <- ggplot(data=input_image, aes(x=BF, color=num2class(expert)))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of BF", spec, sep = " "))
  AF <- ggplot(data=input_image, aes(x=AF, color=num2class(expert)))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of AF", spec, sep = " "))
  AN <- ggplot(data=input_image, aes(x=AN, color=num2class(expert)))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of AN", spec, sep = " "))
  return(list(NDAI, SD, CORR, DF, CF, BF, AF, AN))
}

#' @title plot histogram of features (modified for PartIV.b)
#' @description plot histogram of features by class
#' @param input_image data.frame of an image
#' @return a list of ggplot objects
feature_hist_by_class_mod <- function(input_image, spec = "") {
  # other features
  NDAI <- ggplot(data=input_image, aes(x=NDAI, color=origin))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of NDAI", spec, sep = " "))
  SD <- ggplot(data=input_image, aes(x=SD, color=origin))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of SD", spec, sep = " "))
  CORR <- ggplot(data=input_image, aes(x=CORR, color=origin))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of CORR", spec, sep = " "))
  # radiance reading
  DF <- ggplot(data=input_image, aes(x=DF, color=origin))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of DF", spec, sep = " "))
  CF <- ggplot(data=input_image, aes(x=CF, color=origin))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of CF", spec, sep = " "))
  BF <- ggplot(data=input_image, aes(x=BF, color=origin))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of BF", spec, sep = " "))
  AF <- ggplot(data=input_image, aes(x=AF, color=origin))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of AF", spec, sep = " "))
  AN <- ggplot(data=input_image, aes(x=AN, color=origin))+
    geom_histogram(fill="white", alpha=0.5, position="identity", bins = 30)+
    labs(title = paste("Histogram of AN", spec, sep = " "))
  return(list(NDAI, SD, CORR, DF, CF, BF, AF, AN))
}

#' @title split a vector into chunks
#' @description splits a vecotr into chunks
#' @param x input vector
#' @param n number of chunks
#' @return a list of vectors
chunk2 <- function(x,n) {
  split(x, cut(seq_along(x), n, labels = FALSE)) 
}

#' @title image divider
#' @description divide a image into KxK sub-images
#' @param input_image the input_image stored as data.frame
#' @param K square root of number of sections
#' @return a KxK list of divided images
divide_image <- function(input_image, K = 3){
  output <- rep(list(NA), K^2)
  n <- nrow(input_image)
  x_chunk <- chunk2(sort(input_image$x), K)
  y_chunk <- chunk2(sort(input_image$y), K)
  t <- 0
  for (i in 1:K) {
    for (j in 1:K) {
      t <- t+1
      temp <- subset(input_image, x>=min(x_chunk[[i]])&x<=max(x_chunk[[i]])&y>=min(y_chunk[[j]])&y<=max(y_chunk[[j]]))
      output[[t]] <- temp
    }
  }
  output
}

#' @title image forger
#' @description forge K new images using pcs divided by the divide_image function
#' @param image_divided a list of outputs of divide_image functions
#' @param K square root of number of sections
#' @return a K element list of new images
forge_new_image <- function(images_divided, K){
  data_tr <- data.frame()
  data_va <- data.frame()
  data_te <- data.frame()
  selector_index <- chunk2(rep(1:3,K^2), K^2)
  for (i in 1:K^2) {
    temp_index <- sample(selector_index[[i]], size = 1)
    selector_index[[i]] <- selector_index[[i]][selector_index[[i]]!=temp_index]
    data_tr <- rbind(data_tr, images_divided[[temp_index]][[i]])
  }
  for (i in 1:K^2) {
    temp_index <- sample(selector_index[[i]], size = 1)
    selector_index[[i]] <- selector_index[[i]][selector_index[[i]]!=temp_index]
    data_va <- rbind(data_va, images_divided[[temp_index]][[i]])
  }
  for (i in 1:K^2) {
    temp_index <- selector_index[[i]]
    data_te <- rbind(data_te, images_divided[[temp_index]][[i]])
  }
  list(data_tr, data_va, data_te)
}

#' @title image sampler (simple random sampling)
#' @description forge K new images using pcs divided by the divide_image function
#' @param image_divided a list of outputs of divide_image functions
#' @param K square root of number of sections
#' @return a K element list of new images
forge_random <- function(images_divided, K){
  image_pool <- rep(list(NA), 3*K^2)
  t <- 0
  for (i in 1:3) {
    for (j in 1:K^2) {
      t <- t + 1
      image_pool[[t]] <- images_divided[[i]][[j]]
    }
  }
  data_tr <- data.frame()
  data_va <- data.frame()
  data_te <- data.frame()
  selector_index <- 1:(3*K^2)
  
  for (i in 1:K^2) {
    temp_index <- sample(selector_index, size = 1)
    selector_index <- selector_index[selector_index != temp_index]
    data_tr <- rbind(data_tr, image_pool[[temp_index]])
  }
  for (i in 1:K^2) {
    temp_index <- sample(selector_index, size = 1)
    selector_index <- selector_index[selector_index != temp_index]
    data_va <- rbind(data_va, image_pool[[temp_index]])
  }
  for (i in 1:K^2) {
    temp_index <- sample(selector_index, size = 1)
    selector_index <- selector_index[selector_index != temp_index]
    data_te <- rbind(data_te, image_pool[[temp_index]])
  }
  list(data_tr, data_va, data_te)
}

#' @title zero one loss function
#' @description returns 0-1 loss for prediction
#' @param true_label expert label of the testing set
#' @param pred_label predicted label
#' @return percentage of accuracy
zero_one_loss <- function(true_label, pred_label) {
  sum(true_label == pred_label) / length(true_label)
}

#' @title LDA·改
#' @description give prediction using LDA
#' @param training_data data.frame of the training set, including feature and label
#' @param tbp data.frame of features to be predicted
#' @return predicted labels
lda.changed <- function(training_data, tbp){
  temp_model <- lda(data = training_data, expert ~ NDAI + CORR + SD)
  return(predict(temp_model, tbp)$class)
}

#' @title QDA·改
#' @description give prediction using QDA
#' @param training_data data.frame of the training set, including feature and label
#' @param tbp data.frame of features to be predicted
#' @return predicted labels
qda.changed <- function(training_data, tbp){
  temp_model <- qda(data = training_data, expert ~ NDAI + CORR + SD)
  return(predict(temp_model, tbp)$class)
}

#' @title GLM·改
#' @description give prediction using GLM
#' @param training_data data.frame of the training set, including feature and label
#' @param tbp data.frame of features to be predicted
#' @return predicted labels
glm.changed <- function(training_data, tbp){
  temp_fit <- glm(data = training_data, expert ~ NDAI + CORR + SD, family = binomial)
  temp_probs <- predict(temp_fit, tbp, type = "response")
  temp_pred <- rep(-1, length(temp_probs))
  temp_pred[temp_probs > 0.5] <- 1
  #temp_pred[temp_probs > 0] <- 1
  return(temp_pred)
}

#' @title Decision Tree·改
#' @description give prediction using GLM
#' @param training_data data.frame of the training set, including feature and label
#' @param tbp data.frame of features to be predicted
#' @return predicted labels
tree.changed <- function(training_data, tbp){
  temp_fit <- rpart::rpart(data = training_data, expert ~ NDAI + CORR + SD, method = "class")
  temp_probs <- predict(temp_fit, tbp)
  temp_pred <- rep(-1, nrow(temp_probs))
  temp_pred[temp_probs[,2] > 0.5] <- 1
  #temp_pred[temp_probs > 0] <- 1
  return(temp_pred)
}

#' @title Convergency test for model
#' @description test how long does a model converge
#' @param input_data data.frame of data set
#' @param K number of folds used
#' @param model what model to use
#' @return a list of data.frames of convergency result
conv_test <- function(K, input_data, model){
  fold_index <- createFolds(c(1:nrow(input_data)), k=K, list = TRUE)
  conv_test <- input_data[fold_index[[K]],]
  pred_err <- numeric()
  means <- data.frame()
  for (i in 1:(K-1)) {
    conv_train <- data.frame()
    which_fold <- sample(1:K, i, replace = FALSE)
    for (j in which_fold) {
      conv_train <-rbind(conv_train, input_data[fold_index[[j]],])
    }
    temp_model <- model(data = conv_train, expert ~ NDAI + CORR + SD)
    temp_pred <- predict(temp_model, conv_test)$class
    temp_means <- data.frame(temp_model$means)
    temp_means <- cbind(temp_means, rownames(temp_means))
    means <- rbind(means, temp_means)
    pred_err <- c(pred_err, 1 - zero_one_loss(conv_test$expert, temp_pred))
  }
  convergence_result <- data.frame(train_size = round((1:(K-1))/K, digits = 3), error = pred_err)
  means <- as.data.frame(cbind(round((1:(K-1))/K, digits = 3), means))
  colnames(means) <- c("train_size", "NDAI", "CORR", "SD", "label")
  return(list(convergence_result, means))
}

#' @title misclassification trend finder
#' @description give mean difference of features between two group
#' @param img1 first image
#' @param imgTrue second image
#' @return data.frame of differences
feat_mean_diff_std <- function(img1, imgTrue){
  feat_diff <- data.frame()
  for (i in 1:ncol(imgTrue)) {
    temp_diff <- (mean(img1[,i]) - mean(imgTrue[,i])) / sd(imgTrue[,i])
    feat_diff <- rbind(feat_diff, temp_diff)
  }
  feat_diff <- cbind(colMeans(img1), colMeans(imgTrue), rep("|", ncol(imgTrue)) ,feat_diff)
  colnames(feat_diff) <- c("misclass_mean", "total_mean", "|", "mean_diff(standardized)")
  rownames(feat_diff) <- c(colnames(imgTrue))
  feat_diff <- feat_diff[4:11,]
  return(feat_diff)
}