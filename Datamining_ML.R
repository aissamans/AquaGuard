###############################################################################
# Datamining_ML.R
# AquaGuard: Data Mining & Machine Learning for Intelligent Leak Detection
# Part of the Engineering Graduation Project at LYDEC
###############################################################################

##########################
# 1. K-Means Clustering for Exploratory Data Mining
##########################

# Read the dataset for clustering (replace with your actual file path)
clus_data <- read.csv2("YOUR_CLUSTERING_DATA_FILE.csv", header = TRUE)

# Convert relevant columns to factors if needed
clus_data$Debit <- as.factor(clus_data$Debit)
clus_data$Jour  <- as.factor(clus_data$Jour)
clus_data$Mois  <- as.factor(clus_data$Mois)

# Display a summary and structure for verification
summary(clus_data)
str(clus_data)

# Use a subset of the data for clustering (for example, first 500 rows)
data_train <- clus_data[1:500, ]

# Determine the optimal number of clusters by examining the within-cluster sum of squares
vec_withinss <- rep(0, 20)
for (i in 1:20) {
  km_res <- kmeans(data_train, centers = i)
  vec_withinss[i] <- km_res$tot.withinss
}
# (Optional) Plot the within-cluster sum of squares to visually inspect the "elbow"
plot(vec_withinss, type = 'l', ylab = "Within-Cluster Sum of Squares", xlab = "Number of Clusters")

# Choose an optimal number of clusters (e.g., 5) and perform k-means clustering
optimal_clusters <- 5
km_result <- kmeans(data_train, centers = optimal_clusters)
data_train$Clusters <- km_result$cluster

# Save the clustering results to a CSV file (replace with desired file path)
write.csv2(data_train, file = "YOUR_CLUSTERING_OUTPUT_FILE.csv", row.names = FALSE)


##########################
# 2. Random Forest Classification for Anomaly Detection
##########################

# Read the learning dataset to determine column classes (adjust file path as needed)
rf_types <- read.csv2("YOUR_LEARNING_DATA_FILE.csv", header = FALSE, nrows = 1)
col_classes <- rep("character", length(rf_types))
for (i in 1:length(col_classes)) {
  col_classes[i] <- as.character(rf_types[1, i])
}

# Read the learning and prediction datasets using the determined column classes
learning_data <- read.csv2("YOUR_LEARNING_DATA_FILE.csv", header = TRUE, skip = 1, colClasses = col_classes)
prediction_data <- read.csv2("YOUR_PREDICTION_DATA_FILE.csv", header = TRUE, colClasses = col_classes)

# Combine the learning data (for training) and the prediction data
data_rf <- rbind.data.frame(learning_data, prediction_data)
L <- nrow(learning_data)
P <- nrow(prediction_data)

# Load the randomForest library and set a seed for reproducibility
library(randomForest)
set.seed(100)

# Train the Random Forest model using the learning dataset.
# Assume that the target variable for classification is 'Clusters'
rf_model <- randomForest(Clusters ~ ., data = data_rf[1:L, ], method = "class")

# Predict clusters for the prediction dataset using the trained model
prediction_data$Clusters <- predict(rf_model, data_rf[(L + 1):(L + P), ], type = "class")

# Save the classification predictions to a CSV file (replace with desired file path)
write.csv2(prediction_data, file = "YOUR_RF_OUTPUT_FILE.csv", row.names = FALSE)
