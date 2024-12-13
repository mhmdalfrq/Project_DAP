# Install dan muat library yang diperlukan
install.packages("caret")
install.packages("GGally")
install.packages("class")
install.packages("tidyverse")
install.packages("FNN")

library(FNN)
library(tidyverse)
library(readxl)
library(ggplot2)
library(caret)
library(dplyr)
library(GGally)
library(class)

# Membaca data
df <- read.csv("~/df.csv")

# Menampilkan nama kolom dan beberapa baris pertama
print(names(df))
print(head(df))

#--------DATA PREPROCESSING-----#
# Melihat tipe data dari setiap kolom
str(df)

# Mengganti nilai yang hilang (NA) dengan mean dari kolom tersebut
df <- df %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))

# Mengecek apakah masih ada nilai yang hilang
sum(is.na(df))

#------DATA EXPLORATION-----#
# Statistik deskriptif
summary(df)

# Plot distribusi data MinTemp
ggplot(df, aes(x = MinTemp)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribusi MinTemp", x = "MinTemp", y = "Frequency")

#------DATA CLEANING-----#
# Menghapus duplikat
df <- df %>%
  distinct()

# Menghapus baris dengan nilai yang hilang (jika ada)
df <- na.omit(df)

sum(is.na(df))
print(head(df))

#------CORRELATION MATRIX-----#
# Menghitung matriks korelasi
cor_matrix <- cor(df %>% select_if(is.numeric))

# Plot matriks korelasi
ggcorr(cor_matrix, label = TRUE, label_alpha = TRUE)

#------SPLIT TRAIN TEST VALIDATION DATA-----#
# Mengatur target dan fitur
target <- "RainTomorrow"
features <- colnames(df)[colnames(df) != target]

# Split data menjadi training, validation, dan testing set
set.seed(42)
splitSample <- sample(1:3, size = nrow(df), prob = c(0.8, 0.1, 0.1), replace = TRUE)
train_data <- df[splitSample == 1, ]
validation_data <- df[splitSample == 2, ]
test_data <- df[splitSample == 3, ]

# Mengatur fitur dan target
train_x <- train_data[, features]
train_y <- train_data[, target]

validation_x <- validation_data[, features]
validation_y <- validation_data[, target]

test_x <- test_data[, features]
test_y <- test_data[, target]

# Fungsi untuk menyesuaikan panjang
adjust_length <- function(x, y) {
  if (nrow(x) != length(y)) {
    min_length <- min(nrow(x), length(y))
    x <- x[1:min_length, ]
    y <- y[1:min_length]
  }
  return(list(x = x, y = y))
}

# Menyesuaikan panjang train_x dan train_y
adjusted_train <- adjust_length(train_x, train_y)
train_x <- adjusted_train$x
train_y <- adjusted_train$y

# Menyesuaikan panjang validation_x dan validation_y
adjusted_validation <- adjust_length(validation_x, validation_y)
validation_x <- adjusted_validation$x
validation_y <- adjusted_validation$y

# Menyesuaikan panjang test_x dan test_y
adjusted_test <- adjust_length(test_x, test_y)
test_x <- adjusted_test$x
test_y <- adjusted_test$y

# Periksa panjang data
cat("Dimensi train_x:", dim(train_x), "\n")
cat("Panjang train_y:", length(train_y), "\n")

cat("Dimensi validation_x:", dim(validation_x), "\n")
cat("Panjang validation_y:", length(validation_y), "\n")

cat("Dimensi test_x:", dim(test_x), "\n")
cat("Panjang test_y:", length(test_y), "\n")

# Pastikan train_x dan train_y memiliki panjang yang sama
if (nrow(train_x) != length(train_y)) {
  stop("Panjang train_x dan train_y tidak sama.")
}

# Pastikan validation_x dan validation_y memiliki panjang yang sama
if (nrow(validation_x) != length(validation_y)) {
  stop("Panjang validation_x dan validation_y tidak sama.")
}

# Pastikan test_x dan test_y memiliki panjang yang sama
if (nrow(test_x) != length(test_y)) {
  stop("Panjang test_x dan test_y tidak sama.")
}

#------SCALE DATA-----#
# Select only numeric columns from train_x
train_x_numeric <- train_x %>% select_if(is.numeric)

# Remove columns with constant values
train_x_numeric <- train_x_numeric %>% 
  select_if(~ length(unique(.)) > 1)

# Scale the data using caret::preProcess()
preproc <- preProcess(train_x_numeric, method = "scale")
train_x_scaled <- predict(preproc, train_x_numeric)
test_x_scaled <- predict(preproc, test_x %>% select_if(is.numeric) %>% 
                           select_if(~ length(unique(.)) > 1))

#------KNN MODELING-----#
# Define the KNN model with k = 5 (you can change the value of k as needed)
k <- 5

# Calculate the predicted class labels using k-NN
pred_y <- knn(train_x_scaled, test_x_scaled, train_y, k = k, prob = TRUE)

# Evaluate the model using mean squared error (MSE)
mse <- mean((as.numeric(pred_y) - as.numeric(test_y))^2)
cat("MSE:", mse, "\n")

# Calculate accuracy
accuracy <- sum(pred_y == test_y) / length(test_y)
cat("Accuracy:", accuracy, "\n")

# Calculate precision, recall, and F1-score
conf_mat <- confusionMatrix(data = factor(pred_y), reference = factor(test_y))
cat("Precision:", conf_mat$byClass["Precision"], "\n")
cat("Recall:", conf_mat$byClass["Recall"], "\n")
cat("F1-score:", conf_mat$byClass["F1"], "\n")

# Print the confusion matrix
print(conf_mat)

