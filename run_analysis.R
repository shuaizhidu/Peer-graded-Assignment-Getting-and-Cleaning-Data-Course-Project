# run_analysis.R
# Coursera Getting and Cleaning Data Project

# Load required packages
if (!require("data.table")) install.packages("data.table")
if (!require("reshape2")) install.packages("reshape2")
library(data.table)
library(reshape2)

# Set working directory
path <- getwd()

# Download and unzip dataset if not already available
url  <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile <- file.path(path, "dataset.zip")

if (!file.exists(zipfile)) {
  download.file(url, zipfile, mode="wb")
}
if (!dir.exists(file.path(path, "UCI HAR Dataset"))) {
  unzip(zipfile)
}

# Load activity labels and features
activityLabels <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt"),
                        col.names = c("classLabels", "activityNames"))
features <- fread(file.path(path, "UCI HAR Dataset/features.txt"),
                  col.names = c("index", "featureNames"))

# Extract mean and std features
featuresNeeded <- grep("(mean|std)\\(\\)", features$featureNames)
measurements   <- features$featureNames[featuresNeeded]

# Clean feature names
measurements <- gsub("^t", "Time", measurements)
measurements <- gsub("^f", "Frequency", measurements)
measurements <- gsub("Acc", "Accelerometer", measurements)
measurements <- gsub("Gyro", "Gyroscope", measurements)
measurements <- gsub("Mag", "Magnitude", measurements)
measurements <- gsub("BodyBody", "Body", measurements)
measurements <- gsub("\\(\\)", "", measurements)

# Helper function to load train/test sets
load_dataset <- function(type) {
  X <- fread(file.path(path, "UCI HAR Dataset", type, paste0("X_", type, ".txt")))[, featuresNeeded, with = FALSE]
  setnames(X, measurements)
  
  y <- fread(file.path(path, "UCI HAR Dataset", type, paste0("y_", type, ".txt")), col.names = "Activity")
  subject <- fread(file.path(path, "UCI HAR Dataset", type, paste0("subject_", type, ".txt")), col.names = "SubjectNo")
  
  cbind(Activity = y$Activity, SubjectNo = subject$SubjectNo, X)
}

# Load train and test datasets
train <- load_dataset("train")
test  <- load_dataset("test")

# Merge
merged <- rbind(train, test)

# Apply descriptive activity names
merged$Activity  <- factor(merged$Activity, 
                           levels = activityLabels$classLabels, 
                           labels = activityLabels$activityNames)
merged$SubjectNo <- as.factor(merged$SubjectNo)

# Melt and recast to find averages
melted <- melt(merged, id = c("SubjectNo", "Activity"))
tidyData <- dcast(melted, SubjectNo + Activity ~ variable, mean)

# Save tidy dataset
write.table(tidyData, "tidyData.txt", row.name = FALSE)
