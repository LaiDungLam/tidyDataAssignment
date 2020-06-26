run_analysis <- function () { 
  #load in dplyer package 
  library("dplyr")
  library("reshape2")
  
  
  #Load in data, label, and subjectIDs of train dataset
  train_data <- read.table("./data/train/X_train.txt")
  train_activity_label <- read.table("./data/train/y_train.txt") 
  train_subjects <- read.table("./data/train/subject_train.txt")
  
  #Load in datad, label, & subjectIds of test dataset 
  test_data <- read.table("./data/test/X_test.txt")
  test_activity_label <- read.table("./data/test/y_test.txt")
  test_subjects <- read.table("./data/test/subject_test.txt")

  #Load in features 
  features <- read.table ("./data/features.txt") 
  features <- t(features)
  #Load in activity names that implemeted in the research 
  activities <- read.table ("./data/activity_labels.txt")
  
  
  ###CREATING TIDY TRAIN DATASET
  #----------STEP 4: APPROPRIATELY LABELS THE DATA SET(TRAIN) WITH DESCRIPTIVE VARIABLE NAMES ---------------------------
  #set meaningful columns name for train dataset by combining it to the features 
  train_data <- rbind(features, train_data)
  colnames(train_data) <- train_data[2, ]
  train_data <- train_data[-c(1,2), ]
  
  #--------STEP 2: Extracts only the measurements on the mean and standard deviation for each measurement IN TRAIN DATASET-------
  mean_std <- names(train_data)[grepl("\\bmean\\b|std()", names(train_data))]
  train_data <- train_data[, mean_std]  
  
  #--------STEP 3: Uses descriptive activity names to name the activities in the data set-------------------------------
  train_activity_label <- tbl_df(train_activity_label)
  train_activity_label <- rename(ActivityId = V1, train_activity_label)
  train_activity_label <- train_activity_label %>% mutate (Activity = case_when(ActivityId == 1 ~"Walking", 
                                                        ActivityId == 2 ~ "Walking Upstairs",
                                                        ActivityId==3 ~ "Walking Downstairs",
                                                        ActivityId == 4 ~ "Sitting", 
                                                        ActivityId == 5 ~ "Standing", 
                                                        ActivityId == 6 ~ "Laying"))
  #Give train_subjects meaningful column name 
  train_subjects <- tbl_df(train_subjects)
  train_subjects <- rename(SubjectId = V1, train_subjects)
  
  #creating a tidy train data 
  train_data_tidy <- cbind(train_subjects, train_activity_label, train_data)
  
  
  #####CREATING TIDY TEST DATA SET 
  #-------STEP 4: APPROPRIATELY LABELS THE DATA SET(TEST) WITH DESCRIPTIVE VARIABLE NAMES ---------------------------
  #set meaningful columns name for train dataset by combining it to the features 
  test_data <- rbind(features, test_data)
  colnames(test_data) <- test_data[2, ]
  test_data <- test_data[-c(1,2), ]
  
  #--------STEP 2: Extracts only the measurements on the mean and standard deviation for each measurement-------
  
  mean_std <- names(test_data)[grepl("\\bmean\\b|std()", names(test_data))]
  test_data <- test_data[, mean_std]
  
  #--------STEP 3: Uses descriptive activity names to name the activities in the data set-------------------------------
  test_activity_label <- tbl_df(test_activity_label)
  test_activity_label <- rename(ActivityId = V1, test_activity_label)
  test_activity_label <- test_activity_label %>% mutate (Activity = case_when(ActivityId == 1 ~"Walking", 
                                                              ActivityId == 2 ~ "Walking Upstairs",
                                                              ActivityId==3 ~ "Walking Downstairs",
                                                              ActivityId == 4 ~ "Sitting", 
                                                              ActivityId == 5 ~ "Standing", 
                                                              ActivityId == 6 ~ "Laying"))
  #Give test_subjects column a proper label 
  test_subjects <- tbl_df(test_subjects)
  test_subjects <- rename(SubjectId = V1, test_subjects)
  
  #creating a tidy test data 
  test_data_tidy <- cbind(test_subjects, test_activity_label, test_data)
  
  #-------STEP 1 MERGES THE (TIDY) TRAINING AND THE (TIDY) TEST SETS TO CREATE ONE (TIDY) DATA SET-----------------
  dataset <- rbind(train_data_tidy, test_data_tidy)
  dataset <- dataset[, -2] #remove the column ActivityId
  
  #-------STEP 5 - Another tidy data set that show the average of each variable for each activity and each subject
  dataset_average <- dataset
  dataset_average[3:length(names(dataset_average))] <- sapply(dataset_average[3:length(names(dataset_average))], as.numeric)
  dataset_average <- dataset_average %>% group_by(SubjectId, Activity) %>% summarise_all(funs(mean))
  write.table(dataset_average, file="myTidyDataSet.txt",  row.names = FALSE)
  list(dataset_average = dataset_average, data = dataset)
  }