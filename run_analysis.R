# Load data
olddir <- getwd()
setwd("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/")

XTrain <- read.table("Train/X_train.txt")
YTrain <- read.table("Train/Y_train.txt")
subTrain <- read.table("Train/subject_train.txt")

XTest <- read.table("Test/X_test.txt")
YTest <- read.table("Test/Y_test.txt")
subTest <- read.table("Test/subject_test.txt")

features <- read.table("features.txt")
activityLabels <- read.table("activity_labels.txt")

setwd(olddir)

# Initial Tidying
trainData <- cbind(subTrain, YTrain, XTrain)
names(trainData)[c(1,2)] <- c("ID", "activity")

testData <- cbind(subTest,YTest,XTest)
names(testData)[c(1,2)] <- c("ID", "activity")


# Merges the training and the test sets to create one data set.
merged <- rbind(trainData, testData)
merged <- merged[order(merged$ID),]
names(merged)[3:length(names(merged))] <- as.character(features[,2])


# Extracts only the measurements on the mean and standard deviation for each measurement. 
relevantCols <- grepl(pattern = "mean\\(\\)",x = names(merged)) | grepl(pattern = "std\\(\\)",x = names(merged)) # I don't include variables like meanFreq()
relevantCols[1:2] <- TRUE # Keep ID and activity
mergedFiltered <- merged[,relevantCols]

# Uses descriptive activity names to name the activities in the data set
mergedFiltered$activity <- activityLabels[mergedFiltered$activity,2] # Converts to factor

# Appropriately labels the data set with descriptive variable names. 
names(mergedFiltered) <- gsub("\\(|\\)","",names(mergedFiltered))
names(mergedFiltered) <- gsub("-", " ", names(mergedFiltered))
names(mergedFiltered) <- gsub('([[:upper:]])', ' \\1', names(mergedFiltered)) # split string by capital letters
names(mergedFiltered) <- gsub("t ", "Time domain - ",names(mergedFiltered))
names(mergedFiltered) <- gsub("f ", "Freq domain - ",names(mergedFiltered))
names(mergedFiltered) <- gsub("Acc", "Acceleration ",names(mergedFiltered))
names(mergedFiltered) <- gsub("X", "X-Axis",names(mergedFiltered))
names(mergedFiltered) <- gsub("Y", "Y-Axis",names(mergedFiltered))
names(mergedFiltered) <- gsub("Z", "Z-Axis",names(mergedFiltered))
names(mergedFiltered)[1] <- "ID"

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
mergedFiltered <- group_by(mergedFiltered, ID, activity)
newTidyData <- summarise_each(mergedFiltered, funs(mean))
write.table(x = newTidyData,file = "newTidyData.csv",row.names = FALSE)















