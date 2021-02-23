library(dplyr)
library(stringr)


## 0. Check the directory and files
getwd()
list.files()

## 1. Merges the training and the test sets to create one data set.
# Read the name of the variables
features <- read.table("./UCI HAR Dataset/features.txt")

# Read the training data
X_train <- tbl_df(read.table("./UCI HAR Dataset/train/X_train.txt", col.names = features[, 2]))
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "activity_labels_index")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

# Add the activity label and subject label in the training data
interim_train <- cbind(y_train, X_train)
X_train_merge <- cbind(subject_train, interim_train)

# Read the test data
X_test <- tbl_df(read.table("./UCI HAR Dataset/test/X_test.txt", col.names = features[, 2]))
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "activity_labels_index")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

#  Add the activity label and subject label in the test data
interim_test <- cbind(y_test, X_test)
X_test_merge <- cbind(subject_test, interim_test)

# Merge the training and the test sets to create one data set
train_test <- tbl_df(rbind(X_train_merge, X_test_merge))

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
extract <- grepl("mean.)|std.)", features[, 2])
extract_2 <- c(TRUE, TRUE, extract)
tt_extract <- train_test[, extract_2]

## 3. Uses descriptive activity names to name the activities in the data set
# Read the activity index and name
activity <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("activity_labels_index", "activity_labels"))

# Uses descriptive activity names
activity[,2] <- sub("WALKING_", "", activity[, 2])
activity[,2] <- str_sub(activity[, 2], start = 1, end = 3)

# Change from the activity index to activity name 
tt_extract_2 <- merge(tt_extract, activity, by = "activity_labels_index")
tt_extract_3 <- tbl_df(select(tt_extract_2, subject, activity_labels, tBodyAcc.mean...X :fBodyBodyGyroJerkMag.std..))

# Arrange the data so that subject gets ascending order
tt_extract_3 <- arrange(tt_extract_3, subject)

## 4. Appropriately labels the data set with descriptive variable names. 
var_org <- features[extract, 2]
var_desc <- gsub("Body", "B", var_org)
var_desc <- gsub("Gravity", "Gr", var_desc)
var_desc <- gsub("Acc", "A", var_desc)
var_desc <- gsub("mean.)", "m", var_desc)
var_desc <- gsub("std.)", "s", var_desc)
var_desc <- gsub("Jerk", "J", var_desc)
var_desc <- gsub("Gyro", "Gy", var_desc)
var_desc <- gsub("Mag", "M", var_desc)
var_desc <- gsub("\\-", "", var_desc)
colnames(tt_extract_3) <- c("subject", "activity_labels", var_desc)

## 5. From the data set in step 4, creates a second, independent tidy data set
##    with the average of each variable for each activity and each subject.

# average for the groub by the activities
summarise_activity <-
  tt_extract_3 %>%
  group_by(activity_labels) %>%
  summarise_each_(funs = mean, vars = var_desc)

# average for the groub by the subjects
summarise_subject <-
  tt_extract_3 %>%
  group_by(subject) %>%
  summarise_each_(funs = mean, vars = var_desc)

# Rename the variable name to merge
summarise_activity <- rename(summarise_activity, ave_by = activity_labels)
summarise_subject <- rename(summarise_subject, ave_by = subject)

# Merge the 2 data set to submit as one data
submission <- rbind(summarise_activity, summarise_subject)

# Output the data set for the submission
write.table(submission, "submission.txt", row.names =  FALSE)
