#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


#setup the workbench environment
setwd("~/Documents/DataScience/UCI HAR Dataset")
require("reshape2")
require("data.table")
require("plyr")
#read data tables in to r
features <- read.table("features.txt", header = FALSE, sep = " ")[ , 2]
activity_labels <- read.table("activity_labels.txt", header = FALSE, sep = " ")
subject_test <- read.table("./test/subject_test.txt")
X_test <- read.table("./test/X_test.txt")
Y_test <- read.table("./test/y_test.txt")
subject_train <- read.table("./train/subject_train.txt")
X_train <- read.table("./train/X_train.txt")
Y_train <- read.table("./train/y_train.txt")
#clean up and massage the data
names(activity_labels) <- c("activity_ID", "activity_label")
names(X_test) <- features
names(X_train) <- features
names(Y_test) <- "activity_ID"
names(Y_train) <- "activity_ID"
names(subject_test) <- "subject_ID"
names(subject_train) <- "subject_ID"

#step 1: Merge the training and the test sets to create one data set.
test_data <- cbind(subject_test, Y_test, X_test)
train_data <- cbind(subject_train, Y_train, X_train)
all.data <- rbind( test_data, train_data)

#step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
extract.loc <- grepl("mean|std", names(all.data))
extract.data1 <- all.data[, extract.loc]
extract.data2 <- all.data[, c(1, 2)]
extract.data <- cbind(extract.data2, extract.data1)

#step 3: Uses descriptive activity names to name the activities in the data set
#step 4: Appropriately labels the data set with descriptive variable names
extract.data <- join(extract.data, activity_labels, by = "activity_ID")
extract.data$activity_ID <- extract.data$activity_label
extract.data <- extract.data[1:(ncol(extract.data)-1)]
names(extract.data)[2] <-"activity"

#step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
melt.data <- melt(extract.data, id=c("subject_ID", "activity"))
summary.mean <- dcast(melt.data, subject_ID+activity ~ variable, mean)

#write the dataset to csv
write.csv(summary.mean, file = "./summary_mean.csv", row.names = FALSE)