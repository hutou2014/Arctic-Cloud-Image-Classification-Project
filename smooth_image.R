# read image_data (this step can take quite some time)
image1 <- read.csv(file = "image_data/image1.txt", sep ="", header = FALSE)
image2 <- read.csv(file = "image_data/image2.txt", sep ="", header = FALSE)
image3 <- read.csv(file = "image_data/image3.txt", sep ="", header = FALSE)

# change the column names for better understanding
feature_name <- c("y", "x", "expert", "NDAI", "SD", "CORR", "DF", "CF", "BF", "AF", "AN")
colnames(image1) <- feature_name
colnames(image2) <- feature_name
colnames(image3) <- feature_name

# Initialize the array for center coordinate of the megapixel
# since 3 images are almost the same in terms of x, y range, combine:
center.x = c()
center.y = c()
# find the unique values of original x and y coordinate
uni.x = unique(image1$x)
uni.y = sort(unique(image1$y))
# generage the coordinates of center pixel
for (i in seq(min(uni.x)+1, max(uni.x)-1, by=3)){
  i <- i - min(uni.x) + 1
  center.x = c(center.x, uni.x[i])
}
for (i in seq(min(uni.y)+1, max(uni.y)-1, by=3)){
  i <- i - min(uni.y) + 1
  center.y = c(center.y, uni.y[i])
}

divide_image_by_center_pixel <- function(image, centerX = center.x, centerY = center.y){
  new_img <- list()
  t <- 0
  for (x in centerX){
    for (y in centerY){
      t = t + 1
      new_img[[t]] <- rbind(image[which(image$x == x & image$y == y),],
                                  image[which(image$x == x+1 & image$y == y),],
                                  image[which(image$x == x-1 & image$y == y),],
                                  image[which(image$x == x & image$y == y+1),],
                                  image[which(image$x == x+1 & image$y == y+1),],
                                  image[which(image$x == x-1 & image$y == y+1),],
                                  image[which(image$x == x & image$y == y-1),],
                                  image[which(image$x == x+1 & image$y == y-1),],
                                  image[which(image$x == x-1 & image$y == y-1),])
    }
  }
  return(new_img)
}

# for each megapixel, average the 9 pixels. 
form_image_using_mega_pixel <- function(divided_image, cutoff = 0.5) {
  new.im <- data.frame()
  for (item in divided_image){
    if (nrow(item) == 9 & abs(mean(item$expert)) >= cutoff) {
      new.im <- rbind(new.im, colMeans(item))
    }
  }
  colnames(new.im) <- c("y", "x", "expert", "NDAI", "SD", "CORR", "DF", "CF", "BF", "AF", "AN")
  new.im$expert[new.im$expert >=0] <- 1
  new.im$expert[new.im$expert <0] <- -1
  return(new.im)
}

# generate new image (divided) using center pixels
im1_divided <- divide_image_by_center_pixel(image = image1)
im2_divided <- divide_image_by_center_pixel(image = image2)
im3_divided <- divide_image_by_center_pixel(image = image3)

new_im1 <- form_image_using_mega_pixel(divided_image = im1_divided)
new_im2 <- form_image_using_mega_pixel(divided_image = im2_divided)
new_im3 <- form_image_using_mega_pixel(divided_image = im3_divided)

write.csv(new_im1, file = "image_data/image1_blur.csv")
write.csv(new_im2, file = "image_data/image2_blur.csv")
write.csv(new_im3, file = "image_data/image3_blur.csv")




