run_analysis <- function()
{
  # Import necessary Libraries
  library(plyr)
  library(dplyr)
  
  # Read features and extract only mean and standard deviation features
  features <- read.table("features.txt")[[2]]
  extracted_features <- grep("(mean[(]|std[(])",features)
  extracted_labels <- features[extracted_features]
  
  # Read Desired Training Data, combine with Subject data
  train_data <- read.table("train/X_train.txt", header = FALSE)
  train_subjects <- read.table("train/subject_train.txt")
  extracted_train_data <- train_data[,extracted_features]
  extracted_train_data <- cbind(train_subjects, extracted_train_data)
  
  # Read Desired Test Data, combine with Subject data
  test_data <- read.table("test/X_test.txt", header = FALSE)
  test_subjects <- read.table("test/subject_test.txt")
  extracted_test_data <- test_data[,extracted_features]
  extracted_test_data <- cbind(test_subjects, extracted_test_data)
  
  # Merge Test and Training Data
  merged_data <- rbind(extracted_train_data, extracted_test_data)
  
  # Read Activities
  train_act <- read.table("train/y_train.txt", header = FALSE)
  test_act <- read.table("test/y_test.txt", header = FALSE)
  merged_act <- rbind(train_act, test_act)
  
  # Replace Activity numbers with names
  act_names_table <- read.table("activity_labels.txt")
  act_keys <- act_names_table[[1]]
  act_values <- act_names_table[[2]]
  parsed_act <- mapvalues(merged_act[[1]], from = act_keys, to = act_values)
  
  # Add Activity column to Data Table
  full_data <- cbind(parsed_act, merged_data)
  
  # Add Headers
  names(full_data) <- c("Activity", "Subject", extracted_labels)
  
  # Take Average of each feature by Activity and Subject
  cleaned_data <- as.data.frame(group_by(full_data, Activity, Subject) %>% summarize_all(mean))
}
