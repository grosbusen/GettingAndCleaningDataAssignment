#Tidy Data Set information
The available [tidy data set](https://github.com/grosbusen/datasciencecoursera/blob/master/TidyDataSet.md) is a set consists of 30 subject, each subject did 6 activities 
and every single combination of subject and activity is measured using accelerometers and gyroscopes from the Samsung Galaxy S smartphone.
<br>The calculated dataset is a collection of average of each measurement for each activity and each subject. Each measurement is either a mean or standard deviation of the measurement as in Raw Data
<br>The raw data is taken from [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
<br>Here are the step taken from the raw data to form a new independent tidy data set:
  1. Download the file from the location provided
  2. Unzip the file and store it in the working directory
  3. Read data from train and test folder as well as features and activity tables
  4. Combine the data from train and test folder to form one data set
  5. Extracts only the measurements on the mean and standard deviation for each measurement by using grep and metacharacter to fish out all the mean and standard deviation
  6. Appropriately labels the data set with descriptive variable names by changing the column name with the name provided from features.txt
  7. Uses descriptive activity names to name the activities in the data set by merging the measurement data frame with the activity tables data frame
  8. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. We are using aggregate function to summarize the data base on the subject and activity and mean to calculate the average

#Subject Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years

#Activity Information
WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING

#Experiment Information
<br>Each person performed six activities wearing a smartphone (Samsung Galaxy S II) on the waist. 
<br>Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. 
<br>The experiments have been video-recorded to label the data manually.  

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). 
<br>The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. 
<br>The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. 
<br>From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

#Measurement or Variable Information
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ.  These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz.  Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise.

Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz.

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).  

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
These signals were used to estimate variables of the feature vector for each pattern ('-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.):  
- tBodyAcc-XYZ
- tGravityAcc-XYZ
- tBodyAccJerk-XYZ
- tBodyGyro-XYZ
- tBodyGyroJerk-XYZ
- tBodyAccMag
- tGravityAccMag
- tBodyAccJerkMag
- tBodyGyroMag
- tBodyGyroJerkMag
- fBodyAcc-XYZ
- fBodyAccJerk-XYZ
- fBodyGyro-XYZ
- fBodyAccMag
- fBodyAccJerkMag
- fBodyGyroMag
- fBodyGyroJerkMag

The set of variables that were estimated from these signals are the **_average_** of the following: 
- mean(): Mean value
- std(): Standard deviation
- meanFreq(): Weighted average of the frequency components to obtain a mean frequency
- angle(): Angle between to vectors, gravityMean, tBodyAccMean, tBodyAccJerkMean, tBodyGyroMean and tBodyGyroJerkMean

# Tidy Set Variable and its properties
variable name|max width|range|class
---|---|---|---
ActivityDescription|18|LAYING/SITTING/STANDING/WALKING<br>/WALKING_DOWNSTAIRS/WALKING_UPSTAIRS|character
SubjectID|2| 1 / 30|numeric
tBodyAcc-mean()-X|17|0.2215982 / 0.3014610|numeric
tBodyAcc-mean()-Y|20|-0.040513953 / -0.001308288|numeric
tBodyAcc-mean()-Z|19|-0.15251390 / -0.07537847|numeric
tBodyAcc-std()-X|20|-0.9960686 /  0.6269171|numeric
tBodyAcc-std()-Y|20|-0.9902409 /  0.6169370|numeric
tBodyAcc-std()-Z|20|-0.9876587 /  0.6090179|numeric
tGravityAcc-mean()-X|18|-0.6800432 /  0.9745087|numeric
tGravityAcc-mean()-Y|19|-0.4798948 /  0.9565938|numeric
tGravityAcc-mean()-Z|20|-0.4950887 /  0.9578730|numeric
tGravityAcc-std()-X|18|-0.9967642 / -0.8295549|numeric
tGravityAcc-std()-Y|18|-0.9942476 / -0.6435784|numeric
tGravityAcc-std()-Z|18|-0.9909572 / -0.6101612|numeric
tBodyAccJerk-mean()-X|18|0.0426881 / 0.1301930|numeric
tBodyAccJerk-mean()-Y|21|-0.03868721 /  0.05681859|numeric
tBodyAccJerk-mean()-Z|21|-0.06745839 /  0.03805336|numeric
tBodyAccJerk-std()-X|20|-0.9946045 /  0.5442730|numeric
tBodyAccJerk-std()-Y|19|-0.9895136 /  0.3553067|numeric
tBodyAccJerk-std()-Z|19|-0.99328831 /  0.03101571|numeric
tBodyGyro-mean()-X|20|-0.2057754 /  0.1927045|numeric
tBodyGyro-mean()-Y|20|-0.20420536 /  0.02747076|numeric
tBodyGyro-mean()-Z|21|-0.0724546 /  0.1791021|numeric
tBodyGyro-std()-X|19|-0.9942766 /  0.2676572|numeric
tBodyGyro-std()-Y|19|-0.9942105 /  0.4765187|numeric
tBodyGyro-std()-Z|19|-0.9855384 /  0.5648758|numeric
tBodyGyroJerk-mean()-X|19|-0.15721254 / -0.02209163|numeric
tBodyGyroJerk-mean()-Y|19|-0.07680899 / -0.01320228|numeric
tBodyGyroJerk-mean()-Z|20|-0.092499853 / -0.006940664|numeric
tBodyGyroJerk-std()-X|18|-0.9965425 /  0.1791486|numeric
tBodyGyroJerk-std()-Y|19|-0.9970816 /  0.2959459|numeric
tBodyGyroJerk-std()-Z|19|-0.9953808 /  0.1932065|numeric
tBodyAccMag-mean()|21|-0.9864932 /  0.6446043|numeric
tBodyAccMag-std()|19|-0.9864645 /  0.4284059|numeric
tGravityAccMag-mean()|21|-0.9864932 /  0.6446043|numeric
tGravityAccMag-std()|19|-0.9864645 /  0.4284059|numeric
tBodyAccJerkMag-mean()|19|-0.9928147 /  0.4344904|numeric
tBodyAccJerkMag-std()|19|-0.9946469 /  0.4506121|numeric
tBodyGyroMag-mean()|19|-0.9807408 /  0.4180046|numeric
tBodyGyroMag-std()|19|-0.9813727 /  0.2999760|numeric
tBodyGyroJerkMag-mean()|19|-0.99732253 /  0.08758166|numeric
tBodyGyroJerkMag-std()|19|-0.9976661 /  0.2501732|numeric
fBodyAcc-mean()-X|19|-0.9952499 /  0.5370120|numeric
fBodyAcc-mean()-Y|20|-0.9890343 /  0.5241877|numeric
fBodyAcc-mean()-Z|19|-0.9894739 /  0.2807360|numeric
fBodyAcc-std()-X|20|-0.9966046 /  0.6585065|numeric
fBodyAcc-std()-Y|20|-0.9906804 /  0.5601913|numeric
fBodyAcc-std()-Z|19|-0.9872248 /  0.6871242|numeric
fBodyAcc-meanFreq()-X|21|-0.6359130 /  0.1591236|numeric
fBodyAcc-meanFreq()-Y|20|-0.3795185 /  0.4665282|numeric
fBodyAcc-meanFreq()-Z|20|-0.5201148 /  0.4025326|numeric
fBodyAccJerk-mean()-X|20|-0.9946308 /  0.4743173|numeric
fBodyAccJerk-mean()-Y|20|-0.9893988 /  0.2767169|numeric
fBodyAccJerk-mean()-Z|19|-0.9920184 /  0.1577757|numeric
fBodyAccJerk-std()-X|20|-0.9950738 /  0.4768039|numeric
fBodyAccJerk-std()-Y|20|-0.9904681 /  0.3497713|numeric
fBodyAccJerk-std()-Z|20|-0.993107760 / -0.006236475|numeric
fBodyAccJerk-meanFreq()-X|19|-0.5760440 /  0.3314493|numeric
fBodyAccJerk-meanFreq()-Y|20|-0.6019714 /  0.1956773|numeric
fBodyAccJerk-meanFreq()-Z|20|-0.6275555 /  0.2301079|numeric
fBodyGyro-mean()-X|19|-0.9931226 /  0.4749624|numeric
fBodyGyro-mean()-Y|19|-0.9940255 /  0.3288170|numeric
fBodyGyro-mean()-Z|19|-0.9859578 /  0.4924144|numeric
fBodyGyro-std()-X|18|-0.9946522 /  0.1966133|numeric
fBodyGyro-std()-Y|19|-0.9943531 /  0.6462336|numeric
fBodyGyro-std()-Z|19|-0.9867253 /  0.5224542|numeric
fBodyGyro-meanFreq()-X|20|-0.3957702 /  0.2492094|numeric
fBodyGyro-meanFreq()-Y|20|-0.6668148 /  0.2731413|numeric
fBodyGyro-meanFreq()-Z|20|-0.5074909 /  0.3770741|numeric
fBodyAccMag-mean()|20|-0.9868006 /  0.5866376|numeric
fBodyAccMag-std()|20|-0.9876485 /  0.1786846|numeric
fBodyAccMag-meanFreq()|21|-0.3123380 /  0.4358469|numeric
fBodyBodyAccJerkMag-mean()|20|-0.9939983 /  0.5384048|numeric
fBodyBodyAccJerkMag-std()|19|-0.9943667 /  0.3163464|numeric
fBodyBodyAccJerkMag-meanFreq()|21|-0.1252104 /  0.4880885|numeric
fBodyBodyGyroMag-mean()|20|-0.9865352 /  0.2039798|numeric
fBodyBodyGyroMag-std()|18|-0.9814688 /  0.2366597|numeric
fBodyBodyGyroMag-meanFreq()|21|-0.4566387 /  0.4095216|numeric
fBodyBodyGyroJerkMag-mean()|19|-0.9976174 /  0.1466186|numeric
fBodyBodyGyroJerkMag-std()|19|-0.9975852 /  0.2878346|numeric
fBodyBodyGyroJerkMag-meanFreq()|21|-0.1829236 /  0.4263017|numeric
angle(tBodyAccMean,gravity)|21|-0.1630426 /  0.1291540|numeric
angle(tBodyAccJerkMean),gravityMean)|21|-0.120554 /  0.203260|numeric
angle(tBodyGyroMean,gravityMean)|21|-0.3893051 /  0.4441012|numeric
angle(tBodyGyroJerkMean,gravityMean)|21|-0.2236721 /  0.1823848|numeric
angle(X,gravityMean)|18|-0.9471165 /  0.7377844|numeric
angle(Y,gravityMean)|20|-0.8745677 /  0.4247612|numeric
angle(Z,gravityMean)|21|-0.8736494 /  0.3904444|numeric
