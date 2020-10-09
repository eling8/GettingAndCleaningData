# Downloads the zipfile and unzips it into the UCI HAR Dataset folder
download_data <- function() {
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "w4-project.zip")
  unzip("w4-project.zip")
}

# Loads the test and train data and returns a single data frame
load_data <- function() {
  features = read.table("UCI HAR Dataset/features.txt")
  colnames(features) = c("id", "featureName")

  activity_labels = read.table("UCI HAR Dataset/activity_labels.txt")
  
  testData <- create_dataset(features, activity_labels, "test")
  trainData <- create_dataset(features, activity_labels, "train")
  
  rbind(testData, trainData)
}

# Creates a cleaned dataset for either test or train data. Gives descriptive
# names to columns, extracts only mean and std measurements, and includes both
# the subject ID and activity label into each row.
create_dataset <- function(features, activity_labels, test_or_train) {
  dir = paste0("UCI HAR Dataset/", test_or_train, "/")
  
  X = read.table(paste0(dir, "X_", test_or_train, ".txt"))
  colnames(X) = features$featureName
  X = X[,grep("std|mean", colnames(X))]
  
  subject = read.table(paste0(dir, "subject_", test_or_train, ".txt"))
  
  y = read.table(paste0(dir, "y_", test_or_train, ".txt"))
  y$activity = sapply(y$V1, function(x) activity_labels[x,]$V2)
  
  data = cbind(activity=y$activity, X)
  data = cbind(subject=subject$V1, data)
  data
}

# Using the cleaned dataset, returns a dataset with the average of each variable 
# grouped by each activity and each subject.
create_avg_dataset <- function(data) {
  library(dplyr)
  data %>% group_by(activity, subject) %>% summarize(across(everything(), mean))
}

# download_data()
data <- load_data()
avg_data <- create_avg_dataset(data)
write.table(avg_data, "cleaned_data.txt", row.name=FALSE)
