library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)
library(MASS)
library(factoextra)
library(stats)
library(kknn)
library(class)
library(corrplot)
library(plyr)
library(gridExtra)


library(readr)
data <- read_csv('AQI and Lat Long of Countries.csv', col_names = TRUE)

summary(data)
# Check for missing values in a specific column
any(is.na(data))

df <- data[, c(3:12)]

# encoding
df$`AQI Category` <- as.numeric(factor(df$`AQI Category`, levels = unique(df$`AQI Category`)))
df$`CO AQI Category` <- as.numeric(factor(df$`CO AQI Category`, levels = unique(df$`CO AQI Category`)))
df$`Ozone AQI Category` <- as.numeric(factor(df$`Ozone AQI Category`, levels = unique(df$`Ozone AQI Category`)))
df$`NO2 AQI Category` <- as.numeric(factor(df$`NO2 AQI Category`, levels = unique(df$`NO2 AQI Category`)))
df$`PM2.5 AQI Category` <- as.numeric(factor(df$`PM2.5 AQI Category`, levels = unique(df$`PM2.5 AQI Category`)))

df$`AQI Category` <- factor(df$`AQI Category`)
row_labels <- data[[4]]

# EDA
#scatter plot
ggplot(data, aes(x = `PM2.5 AQI Value`, y = `AQI Value`)) +
  geom_point(color = "#8182DA") +
  labs(title = "AQI Value vs. PM2.5 AQI Value",
       x = "PM2.5 AQI Value",
       y = "AQI Value")

# Distribution of AQI Categories
ggplot(data, aes(x = `AQI Category`, fill = `AQI Category`)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Good" = "green", "Moderate" = "#89B449", 
                               "Unhealthy for Sensitive Groups" = "#dbd468",
                               "Unhealthy" = "#fa9500", "Very Unhealthy" = "#eb6424",
                               "Hazardous" = "red")) +
  labs(title = "Distribution of AQI Categories",
       x = "AQI Category",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# Set the size of the plot
options(repr.plot.width=12, repr.plot.height=8)

# Create the violin plot
ggplot(data, aes(x = `AQI Category`, y = `AQI Value`, fill = `AQI Category`)) +
  geom_violin() +
  scale_fill_manual(values = c("Good" = "green", "Moderate" = "#89B449", 
                               "Unhealthy for Sensitive Groups" = "#dbd468",
                               "Unhealthy" = "#fa9500", "Very Unhealthy" = "#eb6424",
                               "Hazardous" = "red")) +
  labs(title = "Distribution of AQI Categories",
       x = "AQI Category",
       y = "AQI Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Select columns for correlation
selected_cols <- c('CO AQI Value', 'Ozone AQI Value', 'NO2 AQI Value', 'PM2.5 AQI Value')

# Calculate correlation matrix
corr_matrix <- cor(data[selected_cols])

# Set the size of the plot
options(repr.plot.width=10, repr.plot.height=8)

# Create the correlation matrix heatmap
corrplot(corr_matrix, method="color", type="upper", order="hclust", 
         addrect = 2, rect.col = "black", rect.lwd = 2, 
         tl.col = "black", tl.srt = 45, tl.cex = 0.8, 
         col= colorRampPalette(c("#f9fef7", "#8bc1d1", "#9067c6"))(100), 
         title="Correlation Matrix of Pollutant AQI Values",
         addCoef.col = "black",
         mar=c(2,2,4,4)) # Adjust the margins

df[, c(1, 3:10)] <- scale(df[, c(1, 3:10)])

# Split into test and train 80/20
set.seed(123)

size <- floor(0.8 *  nrow(df))


train_ind <- sample(seq_len(nrow(df)), size = size)

train_labels <- df[train_ind, 2]

columns_to_include <- c(-2, 1:10)

data_train <- df[train_ind, -c(2)]
data_test <- df[-train_ind, -c(2)]

data_test_labels <- row_labels[-train_ind]

# Create training and test labels
train_labels <- row_labels[train_ind]
test_labels <- row_labels[-train_ind]

# Define the training control
train_control <- trainControl(method = "cv",  # 10-fold cross-validation
                              number = 10)    # Number of folds

# Define the tuning grid (range of k values to try)
k_grid <- expand.grid(k = 1:40)

# Train the model using the train function and tuneGrid argument
knn_model <- train(x = data_train,
                   y = train_labels,
                   method = "knn",
                   trControl = train_control,
                   tuneGrid = k_grid)

plot(knn_model)

predictions <- knn(train = data_train,
                   test = data_test,
                   cl = train_labels,
                     k= best_k)
print(predictions)

# Notice that I am only getting 2 dimensions 
plot_predictions <- data.frame(data_test,
                               predicted = predictions,
                               actual = data_test_labels)



colnames(plot_predictions) <- c("AQI Value",
                                "Ozone AQI Value",
                                "Ozone AQI Category",
                                "NO2 AQI Value",
                                "NO2 AQI Category",
                                "CO AQI Value",
                                "CO AQI Category",
                                "PM2.5 AQI Value",
                                "PM2.5 AQI Category",
                                'predicted')


# Subset the first 100 rows of plot_predictions
plot_predictions_subset <- plot_predictions[1:100,]

# PREDICTION VISUALIZATION
p1 <- ggplot(plot_predictions_subset, aes(x = `Ozone AQI Value`, y = `AQI Value`, color = predicted, fill = predicted)) + 
  geom_point(size = 2) + 
  geom_text_repel(aes(label = data_test_labels[1:100]), hjust = 1, vjust = 2) +
  ggtitle("Predicted relationship between AQI Value and Ozone AQI Value in data_test (First 100 rows)") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

p2 <- ggplot(plot_predictions_subset, aes(x = `CO AQI Value`, y = `AQI Value`, color = predicted, fill = predicted)) + 
  geom_point(size = 2) + 
  geom_text_repel(aes(label = data_test_labels[1:100]), hjust = 1, vjust = 2) +
  ggtitle("Predicted relationship between AQI Value and CO AQI Value in data_test (First 100 rows)") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

p3 <- ggplot(plot_predictions_subset, aes(x = `NO2 AQI Value`, y = `AQI Value`, color = predicted, fill = predicted)) + 
  geom_point(size = 2) + 
  geom_text_repel(aes(label = data_test_labels[1:100]), hjust = 1, vjust = 2) +
  ggtitle("Predicted relationship between AQI Value and PM2.5 AQI Value in data_test (First 100 rows)") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

p4 <- ggplot(plot_predictions_subset, aes(x = `PM2.5 AQI Value`, y = `AQI Value`, color = predicted, fill = predicted)) + 
  geom_point(size = 2) + 
  geom_text_repel(aes(label = data_test_labels[1:100]), hjust = 1, vjust = 2) +
  ggtitle("Predicted relationship between AQI Value and PM2.5 AQI Value in data_test (First 100 rows)") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

grid.arrange(p1, p2, p3, p4, nrow = 4)

#visualisasi NO2 VS PM2.5
ggplot(plot_predictions_subset, aes(x = `PM2.5 AQI Value`, y = `NO2 AQI Value`, color = predicted, fill = predicted)) + 
  geom_point(size = 2) + 
  geom_text(aes(label = data_test_labels[1:100]), hjust = 1, vjust = 2) +
  ggtitle("Predicted relationship between AQI Value and PM2.5 AQI Value in data_test (First 100 rows)") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Visualisasi CO dan Ozone
ggplot(plot_predictions_subset, aes(x = `CO AQI Value`, y = `Ozone AQI Value`, color = predicted, fill = predicted)) + 
  geom_point(size = 2) + 
  geom_text(aes(label = data_test_labels[1:100]), hjust = 1, vjust = 2) +
  ggtitle("Predicted relationship between CO AQI Value and Ozone AQI Value in data_test (First 100 rows)") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Compute confusion matrix
conf_mat <- table(predictions, test_labels)
print(conf_mat)

# Calculate recall (sensitivity)
recall <- conf_mat[2, 2] / sum(conf_mat[2, ])

# Calculate precision
precision <- conf_mat[2, 2] / sum(conf_mat[, 2])

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the metrics
cat("Recall (Sensitivity):", recall, "\n")
cat("Precision:", precision, "\n")
cat("F1 Score:", f1_score, "\n")



