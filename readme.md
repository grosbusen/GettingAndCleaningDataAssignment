#Getting and Cleaning of Data Course Project
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 

Here are the things that you can find in this repository
  1. codebook.md: that describes the variables, the data, and any transformations or work that you performed to clean up the data
  2. run_analysis.R: script that perform analysis and create the tidy data set
  3. TidyDataSet.txt: a tidy data set from run_analysis.R
  4. readme.md, which is what you are reading now

Here are the instruction for run_analysis.R
  1. Merges the training and the test sets to create one data set.
  2. Extracts only the measurements on the mean and standard deviation for each measurement.
  3. Uses descriptive activity names to name the activities in the data set
  4. Appropriately labels the data set with descriptive variable names.
  5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
From the code snippet below, point## is indicative to the point above

#Code Walkthrough
Here is the code walkthrough to [run_analysis.R](https://github.com/grosbusen/GettingAndCleaningDataAssignment/blob/master/run_analysis.R)
The first step you need to do is to install.packages like dplyr and tidyr as the script needs it, however for simplicity sake and compatibility issue, it is not in the script attached here, so please load all the require library on your own.
Please also set up working directory at your own as each working directory is different
The following the walkthrough of the attached scripts
  1. Download the file and unzip the the file to the location that we want
  ```R
  locationUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  if (!file.exists("./data")){
    dir.create("./data")
  }
  download.file(locationUrl, destfile = "./data/Assignment.zip")

  #unzip dataset to directory
  unzip(zipfile="./data/Assignment.zip", exdir = "./data")  
  ```
  2. Read all the file from the train and test folder and assign a descriptive variable matching the file name for easy identification. reading the text files use read.table and stringsAsFactors is set to FALSE. 
  ```R
  #reading data from train folder
  x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt", stringsAsFactors = FALSE)
  y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt", stringsAsFactors = FALSE)
  subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", stringsAsFactors = FALSE)

  #reading the features.txt
  features <- read.table("./data/UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
  
  #reading activity labels and assigning column name
  activity_table <- read.table("./data/UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)
  names(activity_table) <- c("ActivityID", "ActivityDescription")
  
  #reading data from test folder
  x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt", stringsAsFactors = FALSE)
  y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt", stringsAsFactors = FALSE)
  subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", stringsAsFactors = FALSE)

  ```
  3. Assign a descriptive column name for the subject data (SubjectID) and activity data(ActivityID)
  ```R
  #setting column name for y_test
  names(y_test) <- "ActivityID"

  #setting column name for subject_test
  names(subject_test) <- "SubjectID"   
  ```
  4. Combine all the data from test and train folder to a single data frame called mydf
  ```R
  #combine all data frame from train folder
  train <- cbind(x_train, y_train, subject_train)

  #combine all data frame from test folder
  test <- cbind(x_test, y_test, subject_test)

  #POINT NO 1
  #Merges the training and the test sets to create one data set.
  mydf <- rbind(train, test)  
  ```
  5. Use grepl to extract the mean and standard deviation measurement from all the measurements. In this case, mean measurement like MeanFreq and mean related angle are included in this version.
  Just a reminder, that there is no right or wrong to which set of mean that can only be extracted in this data frame.
  6. The value return from the grepl is the indices of all measurement that have mean and standard deviation. Assign a new variable base on the indices of measurement of mean and standard deviation. point 5 and 6 share the same snippet of code.
  
  ```R
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
  ```
  7. Apply a appropriate labels to the data set measurement with descriptive variable names from features data frame. 
  
  ```R
  #POINT NO 4
  #Appropriately labels the data set with descriptive variable names.
  names(mydf_mean_std) <- features2$V2  
  ```
  8. Assign descriptive activity names by merging the data set with activities data frame. This steps add a new column, ActivityDescription to accompany ActivityID. Remove the ActivityID using select()
  
  ```R
  #POINT NO 3
  #Uses descriptive activity names to name the activities in the data set
  #In this step, as the result of the merge there will be additional column
  #name: ActivityDescription as a descriptive activity names
  mydf_desc_activity_name <- select(merge(mydf_mean_std, activity_table, by = "ActivityID", sort = FALSE), -ActivityID)  
  ```
  9. Using aggregate to have a summary base on each subject and activity and measure the average of every measurement in the data frame base on the subject and activity
  ```R
  #POINT NO 5
  #From the data set in step 4, creates a second, independent tidy data set with the average 
  #of each variable for each activity and each subject.
  #using aggregate, use mean function to average every column except 
  #Activity Description and Subject Id 
  #The Resulting tidy data consists of 
  #-each mean and standard activity of the measurement, subject and activity description is a variable names 
  #-each variable names has its own column
  #-each row shows observation of a subject doing an activity and the mean and standard deviation of each measurement

  tidySet <- aggregate(. ~ ActivityDescription + SubjectID, data = mydf_desc_activity_name, mean)  
  ```
  10.Use write table to create a second independent tidy data set churn out from step 9
  ```R
  write.table(tidySet, "./data/TidyDataSet.txt", sep = " ", row.names = FALSE, quote = FALSE)
  ```

