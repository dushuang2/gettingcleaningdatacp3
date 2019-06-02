#Getting and Cleaning Data Course Projectless 
#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as #described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to #clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

#One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

## 0 - Download and read files
setwd("./AlexPersonal/DataScience/Assignments/Course3_GettingandCleaningData")
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile = "./courseproject3.zip")
unzip("./courseproject3.zip")
list.files("./UCI HAR Dataset")

## 0 - Define data
activitylabel <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("id", "activity"))
features <- read.table("./UCI HAR Dataset/features.txt", col.names = c("feaid", "feature"))
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject") 
## each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = features$feature) 
## actual data
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "labelid")
## test labels
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", col.names = features$feature)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "labelid")


## 1 - Merges the training and the test sets to create one data set
Subject <- rbind(subject_train, subject_test) # append test Subject_id under train Subject_id
Ylabel <- rbind(y_train, y_test) # append test label under train label
Xdata <- rbind(x_train, x_test) # append test data under train data
fulldata <- cbind(Subject, Ylabel, Xdata) #put "Subject, Lable, data" as columns (2+561=563 columns)

## 2 - Step 2: Extracts only the measurements on the mean and standard deviation for each measurement
library(dplyr)
meanstd <- fulldata %>% select(subject, labelid, contains("mean"), contains("std"))

## 3 - Uses descriptive activity names to name the activities in the data set.
meanstd$labelid <- activitylabel[meanstd$labelid, 2] # Replace lableid "activity ID" values with "activity name (waling, sitting...). Similar to Excel Vlookup function."

## 4 - Appropriately labels the data set with descriptive variable names
names(meanstd)[2] = "activity" # name of column #2
names(meanstd)<-gsub("Acc", "Accelerometer", names(meanstd)) #global substitude "Acc" with "Accelerometer" in all table "meanstd" columns.
names(meanstd)<-gsub("Gyro", "Gyroscope", names(meanstd))
names(meanstd)<-gsub("Mag", "Magnitude", names(meanstd))
names(meanstd)<-gsub("BodyBody", "Body", names(meanstd))
names(meanstd)<-gsub("^t", "Time", names(meanstd))
names(meanstd)<-gsub("^f", "Frequency", names(meanstd))
names(meanstd)<-gsub("tBody", "TimeBody", names(meanstd))
names(meanstd)<-gsub("-mean()", "Mean", names(meanstd), ignore.case = TRUE)
names(meanstd)<-gsub("-std()", "STD", names(meanstd), ignore.case = TRUE)
names(meanstd)<-gsub("-freq()", "Frequency", names(meanstd), ignore.case = TRUE)
names(meanstd)<-gsub("angle", "Angle", names(meanstd))
names(meanstd)<-gsub("gravity", "Gravity", names(meanstd))

## 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidydata <- meanstd %>%
        group_by(subject, activity) %>%
        summarise_all(list(mean))
write.table(tidydata, "TidyDatac3.txt", row.name=FALSE)
