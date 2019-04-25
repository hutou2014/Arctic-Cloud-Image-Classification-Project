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
#' @param K number of divider
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
#' @param K number of divider
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
  temp_model <- lda(data = training_data, expert ~.)
  return(predict(temp_model, tbp)$class)
}

#' @title QDA·改
#' @description give prediction using QDA
#' @param training_data data.frame of the training set, including feature and label
#' @param tbp data.frame of features to be predicted
#' @return predicted labels
qda.changed <- function(training_data, tbp){
  temp_model <- qda(data = training_data, expert ~.)
  return(predict(temp_model, tbp)$class)
}

#' @title GLM·改
#' @description give prediction using GLM
#' @param training_data data.frame of the training set, including feature and label
#' @param tbp data.frame of features to be predicted
#' @return predicted labels
glm.changed <- function(training_data, tbp){
  temp_fit <- glm(data = training_data, expert ~., family = binomial)
  temp_probs <- predict(temp_fit, tbp, type = "response")
  temp_pred <- rep(-1, length(temp_probs))
  temp_pred[temp_probs > 0.5] <- 1
  #temp_pred[temp_probs > 0] <- 1
  return(temp_pred)
}