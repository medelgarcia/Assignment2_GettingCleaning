# 
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# 
# Here are the data for the project: 
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# You should create one R script called run_analysis.R that does the following. 
# 1.- Merges the training and the test sets to create one data set.
# 2.- Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.- Uses descriptive activity names to name the activities in the data set
# 4.- Appropriately labels the data set with descriptive activity names. 
# 5.- Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

### Import the labels names
labels <- read.table(file = "UCI HAR Dataset/activity_labels.txt", header = FALSE, sep ="")
### Import the features names
features <- read.table(file = "UCI HAR Dataset/features.txt", header = FALSE, sep ="")

### import the train set
trainSubject <- read.table(file = "UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep ="")
trainData <- read.table(file = "UCI HAR Dataset/train/X_train.txt", header = FALSE, sep ="")
# 3.- Uses descriptive activity names to name the activities in the data set
# 4.- Appropriately labels the data set with descriptive activity names. 
names(trainData) <- features[,2] # set the column its feature mesure

### import the train label
trainlabels <- read.table(file = "UCI HAR Dataset/train/y_train.txt", header = FALSE, sep ="")
trainlabels <- as.factor(trainlabels[,1]) #make the data frame a factor variable
levels(trainlabels)<- labels[,2] #set the levels as its activities names
trainData <- cbind(trainSubject, trainlabels, trainData) #add the activity to the data
names(trainData)[2] <- "activity"
names(trainData)[1] <- "subject"


### import the test set
testSubject <- read.table(file = "UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep ="")
testData <- read.table(file = "UCI HAR Dataset/test/X_test.txt", header = FALSE, sep ="")
# 3.- Uses descriptive activity names to name the activities in the data set
# 4.- Appropriately labels the data set with descriptive activity names. 
names(testData) <- features[,2] # set the column its feature mesure

### import the test label
testlabels <- read.table(file = "UCI HAR Dataset/test/y_test.txt", header = FALSE, sep ="")
testlabels <- as.factor(testlabels[,1]) #make the data frame a factor variable
levels(testlabels)<- labels[,2] #set the levels as its activities names
testData <- cbind(testSubject ,testlabels, testData) #add the activity to the data
names(testData)[2] <- "activity"
names(testData)[1] <- "subject"

###
# 1.- Merges the training and the test sets to create one data set.
###
completeData <- rbind(trainData,testData) ### Merge the two Datasets = train + test by observation
completeData[,1] <- as.factor(completeData[,1])
### remove the unusseles variables. Keep complete dataset
rm(trainData, testData, trainlabels, testlabels, trainSubject, testSubject, features, labels)

#Look for the variables that contains mean an standart deviation
interestMesurement <- c("mean()", "std()")
interestDataIndex <- logical(dim(completeData)[2])
interestDataIndex[1] <- TRUE #The first column is the activity
interestDataIndex[2] <- TRUE #The first column is the subject
##built a logical vector where the columns has the array mean or std
for (i in seq(3,dim(completeData)[2])){
  meanORstd <- sum( interestMesurement %in% unlist(strsplit(names(completeData)[i],"-")))
  if(meanORstd){
    interestDataIndex[i] <- TRUE
  }
}
###
# 5.- Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
###
interestData <- cbind(interaction(completeData[,1],completeData[,2]), completeData[,interestDataIndex]) #Use the logical vector to get the data of interest
names(interestData)[1] <- "factor"
rm(completeData, interestDataIndex, interestMesurement, i, meanORstd) #Delete de useless info

### I have the interest tidy data into the variabe interestData ###
#interestData <- cbind(interaction (interestData[,1],interestData[,2]),interestData]
#generate a variable for the mean of the observations for a individual in a status
result <- data.frame()
for (i in levels(interestData[,1])){
  #interestData[,1] == i
  belonging <- interestData[interestData[,1] == i,1:3][1,]
  colapsed <- apply(X = interestData[interestData[,1] == i,4:dim(interestData)[2]],2,mean)
  completeLine <- cbind(belonging[1,],t(colapsed))
  result <- rbind(result, completeLine)
}
