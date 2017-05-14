## This script cleans up data that was collected from the accelerometers
## from the Samsung Galaxy S smartphone. It ultimately produces a summary
## depicting the mean values of all mean and standard deviation variables
## in the full set of data, broken out by subject and the activity completed
## by that subject.

library(dplyr) 

## Read in all necessary files

## Table of test subjects
subjecttest <- read.table("subject_test.txt")

## Sensor data for test subjects
xtest <- read.table("X_test.txt")

## Table of train subjects
subjecttrain <- read.table("subject_train.txt")

## Sensor data for train subjects
xtrain <- read.table("X_train.txt")

## Table of features
features <- read.table("features.txt")

## Table of activity ids with each row corresponding to a test subject and a row
## of sensor data for the test subjects
ytest <- read.table("y_test.txt")

## Table of activity ids with each row corresponding to a train subject and a row
## of sensor data for the train subjects
ytrain <- read.table("y_train.txt")

## Table of activity labels comprising an identifier and its corresponding text label
activity_labels <- read.table("activity_labels.txt")
names(activity_labels)[1] <- "activityid"
names(activity_labels)[2] <- "activitydescription"

## Create a data frame with variables for the test subject identifier and activity identifier
ytest_subject_activity <- cbind(subjecttest, ytest)
names(ytest_subject_activity)[1] <- "subject"
names(ytest_subject_activity)[2] <- "activityid"

## Create a data frame with variables for the train subject identifier and activity identifier
ytrain_subject_activity <- cbind(subjecttrain, ytrain)
names(ytrain_subject_activity)[1] <- "subject"
names(ytrain_subject_activity)[2] <- "activityid"

## Combine the data frames containing subject, activity id into a single data frame
full_subject_activity <- rbind(ytest_subject_activity, ytrain_subject_activity)

## Combine the sensor data for test and training sets into one data frame and name their 
## variables according the the names in the features data frame.
xtest_xtrain <- rbind(xtest, xtrain)
names(xtest_xtrain) <- features$V2

## Combine the data frame containing subject and activity id with the full set of 
## sensor data into a single data frame
xtest_xtrain_activities <-cbind(full_subject_activity, xtest_xtrain)

## Join to the activity_labels data frame to pull in the activity description variable
## in order to produce more readable output.
full_data <- merge(activity_labels, xtest_xtrain_activities, by.x = "activityid", by.y = "activityid")

## Find variables corresponding to mean and std measurements and store
## them in a vector.
meanvars <- grep("mean\\(\\)",names(full_data), value=TRUE)
stdvars <- grep("std\\(\\)",names(full_data), value=TRUE)
meanstdvars <- c("subject", "activitydescription", meanvars, stdvars)

## Subset the data frame of full data by variables containing either mean
## or std measurements
meanstd <- full_data[meanstdvars]

## Order the data frame by activitydescription within subject
meanstd_ord <- arrange(meanstd, subject, activitydescription)

## Group the data by activitydescription within subject
meanstd_ord_grpd <- group_by(meanstd_ord, subject, activitydescription )

## Edit variables to more appropriate names
## Change all variables to lower case
names(meanstd_ord_grpd) <- tolower(names(meanstd_ord_grpd))

## Remove all parentheses from variable names
names(meanstd_ord_grpd) <- gsub("\\(\\)", "", names(meanstd_ord_grpd))

## Remove all dashes from variable names
names(meanstd_ord_grpd) <- gsub("-", "", names(meanstd_ord_grpd))

## Write out the word accelerate in variable names
names(meanstd_ord_grpd) <- gsub("acc", "accelerate", names(meanstd_ord_grpd))

## Summarize data to show mean of each variable by subject by activity
summarized <- summarize_each(meanstd_ord_grpd, funs(mean))

## write out final summarized result
write.table(summarized, "summarized_human_activity_smartphone_data.txt", row.names = FALSE)

