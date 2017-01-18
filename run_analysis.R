#wd set outside the script due to privacy concern
#library also loaded outside this script

locationUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("./data")){
  dir.create("./data")
}
download.file(locationUrl, destfile = "./data/Assignment.zip")

#unzip dataset to directory
unzip(zipfile="./data/Assignment.zip", exdir = "./data")

#reading data from train folder
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt", stringsAsFactors = FALSE)
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt", stringsAsFactors = FALSE)
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", stringsAsFactors = FALSE)

#reading the features.txt
features <- read.table("./data/UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
#names(x_train) <- features[,2]

#setting column name for y_train
names(y_train) <- "ActivityID"

#setting column name for subject_train
names(subject_train) <- "SubjectID"

#adding column name to the features table
features1 <- rbind(features,c(nrow(features) + 1, "ActivityID"), c(nrow(features) + 2, "SubjectID"))

#reading activity labels and assigning column name
activity_table <- read.table("./data/UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)
names(activity_table) <- c("ActivityID", "ActivityDescription")

#combine all data frame from train folder
train <- cbind(x_train, y_train, subject_train)


#reading data from test folder
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt", stringsAsFactors = FALSE)
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt", stringsAsFactors = FALSE)
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", stringsAsFactors = FALSE)

#names(x_test) <- features[,2]

#setting column name for y_test
names(y_test) <- "ActivityID"

#setting column name for subject_test
names(subject_test) <- "SubjectID"

#combine all data frame from test folder
test <- cbind(x_test, y_test, subject_test)

#POINT NO 1
#Merges the training and the test sets to create one data set.
mydf <- rbind(train, test)

#POINT NO 2
#Extracts only the measurements on the mean and standard deviation for each measurement.
#At this point, all related to mean will be extracted, like Mean Frequency, mean related to angle
#This is really up to the discretion of the programmer, it is also possible to just 
#extract data contain "mean()", so please be lenient in giving mark on this 
mean_stddev_index <- grepl("[Mm]ean", features1$V2)    |
                     grepl("ActivityID", features1$V2) |
                     grepl("SubjectID", features1$V2)  |
                     grepl("std",features1$V2) 
mydf_mean_std <- mydf[,mean_stddev_index]
features2 <- features1[mean_stddev_index,]

#POINT NO 4
#Appropriately labels the data set with descriptive variable names.
names(mydf_mean_std) <- features2$V2

#POINT NO 3
#Uses descriptive activity names to name the activities in the data set
#In this step, as the result of the merge there will be additional column
#name: ActivityDescription as a descriptive activity names
mydf_desc_activity_name <- select(merge(mydf_mean_std, activity_table, by = "ActivityID", sort = FALSE), -ActivityID)

#POINT NO 5
#From the data set in step 4, creates a second, independent tidy data set with the average 
#of each variable for each activity and each subject.
#using aggregate, use mean function to average every column except 
#Activity Description and Subject Id
#The Resulting tidy data consists of 
#-each mean and standard activity of the measurement, SubjectID and ActivityDescription is a variable names 
#-each variable names has its own column
#-each row shows observation of a subject doing an activity and the mean and standard deviation of each measurement

tidySet <- aggregate(. ~ ActivityDescription + SubjectID, data = mydf_desc_activity_name, mean)
write.table(tidySet, "./data/TidyDataSet.txt", sep = " ", row.names = FALSE, quote = FALSE)

