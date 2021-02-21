## This is Getting and Cleaning Data Course Project Assignment


# Unzip dataSet to /data directory
unzip(zipfile="./data/Dataset.zip", exdir="./data")


#define the path where the new folder has been unziped
pathdata = file.path("./data", "UCI HAR Dataset") # "file name" 


#create a file which has the 28 file names
files = list.files(pathdata, recursive = T)


#show the files
files


## Step 1. Creating the data set of training and test

#Reading training tables - xtrain / ytrain, subject train
xtrain <- read.table(file.path(pathdata, "train", "X_train.txt"), header = F)
ytrain <- read.table(file.path(pathdata, "train", "y_train.txt"), header = F)
subject_train = read.table(file.path(pathdata, "train", "subject_train.txt"), header = F)


#Reading the testing tables
xtest <- read.table(file.path(pathdata, "test", "X_test.txt"), header = F)
ytest <- read.table(file.path(pathdata, "test", "y_test.txt"), header = F)
subject_test <- read.table(file.path(pathdata, "test", "subject_test.txt"), header = F)


#Read the features data
features <- read.table(file.path(pathdata, "features.txt"), header = F)


#Read activity labels data
activityLabels <- read.table(file.path(pathdata, "activity_labels.txt"), header = F)


#Create Sanity and Column Values to the Train Data
colnames(xtrain) <- features[,2]
colnames(ytrain) <- "activityId"
colnames(subject_train) <- "subjectId"


#Create Sanity and column values to the test data
colnames(xtest) <- features[,2]
colnames(ytest) <- "activityId"
colnames(subject_test) <- "subjectId"


#Create sanity check for the activity labels value
colnames(activityLabels) <- c('activityId','activityType')


#Merging the train and test data 
mrg_train = cbind(ytrain, subject_train, xtrain)
mrg_test = cbind(ytest, subject_test, xtest)


#Create the main data table merging both table tables 
setAllInOne = rbind(mrg_train, mrg_test)


# Need step is to read all the values that are available
colNames = colnames(setAllInOne)

#Need to get a subset of all the mean and standards and the correspondongin activityID and subjectID 
mean_and_std = (grepl("activityId" , colNames) | grepl("subjectId" , colNames) | grepl("mean.." , colNames) | grepl("std.." , colNames))


#A subtset has to be created to get the required dataset
setForMeanAndStd <- setAllInOne[ , mean_and_std == T]

setWithActivityNames = merge(setForMeanAndStd, activityLabels, by='activityId', all.x=T)

# New tidy set has to be created 
secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]

#The last step is to write the output to a text file 
write.table(secTidySet, "secTidySet.txt", row.name=FALSE)


secTidySet

