### First Step - Merge the training and the test files into one data set                                                                        
## Set the working directory
  setwd("H:/MyDocuments/UCI HAR Dataset/")

## Read features and activity data
  features<- read.table("features.txt",header=FALSE)
  activity<- read.table("activity_labels.txt",header=FALSE)

## Read train data
  subjectTrain<- read.table('./train/subject_train.txt',header=FALSE)
	xTrain<- read.table('./train/x_train.txt',header=FALSE)
	yTrain<- read.table('./train/y_train.txt',header=FALSE)

	## Assign column names to train data set
	colnames(activity)<- c('activityId', 'activity')
	colnames(subjectTrain)<- "subjectId"
	colnames(xTrain)<- features[,2] 
	colnames(yTrain)<- "activityId"

	## Column bind the train data set -> yTrain, subjectTrain & xTrain
	trainData <- cbind(yTrain, subjectTrain, xTrain)

## Read test data
	subjectTest<- read.table('./test/subject_test.txt',header=FALSE)
	xTest<- read.table('./test/x_test.txt',header=FALSE)
	yTest<- read.table('./test/y_test.txt',header=FALSE)

	## Assign column names to test data
	colnames(subjectTest)<- "subjectId"
	colnames(xTest)<- features[,2]
	colnames(yTest)<- "activityId"

	# Column bind the test data set -> yTest, subjectTest & xTest
	testData <- cbind(yTest, subjectTest, xTest)

# Row bind the train & test data sets together              
	tblData <- rbind(trainData,testData)
                                     
# Create vector for tblData column names - used to select mean() & stddev() columns
	colNames  <- colnames(tblData) 


### Second Step - Extract only the measurements on the mean and standard deviation for each measure
# Create logical vector - contains TRUE values for ID, mean(), & stddev() columns; FALSE for remaining columns
	logV <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

# Subset tblData based on the logV to keep only desired columns
	tblData <- tblData[logV == TRUE]


### Third Step - Apply descriptive activity names from activity labels to the data set
# Merge tblData with activity to include activity labels
	tblData <- merge(tblData , activity, by = 'activityId', all.x = TRUE)

# Update colNames vector to include the new column names
	colNames  <- colnames(tblData)


### Step Four - Appropriately label the data set with activity label descriptors
# Clean up variable names
	for (i in 1:length(colNames)) 
	{
  	colNames[i] <- gsub("\\()","",colNames[i])
  	colNames[i] <- gsub("-std$","_StdDev",colNames[i])
  	colNames[i] <- gsub("-mean","_Mean",colNames[i])
  	colNames[i] <- gsub("^t","time",colNames[i])
  	colNames[i] <- gsub("^f","freq",colNames[i])
  	colNames[i] <- gsub("BodyBody","Body",colNames[i])
  	colNames[i] <- gsub("Mag","Magnitude",colNames[i])
	}

# Reassign new column names in tblData
	colnames(tblData) <- colNames


### Step Five - From the data set in Step Four, creates a second, independent tidy data set with the average of each variable for each activity and each subject
# Create tblData_xactivity tbl without the activity column
	tblData_xactivity <- tblData[,names(tblData) != 'activity']

# Create tidyData tbl that includes variable means for each activity and subject combination
	tidyData <- aggregate(tblData_xactivity[,names(tblData_xactivity) != c('activityId','subjectId')],by=list(activityId=tblData_xactivity$activityId,subjectId = tblData_xactivity$subjectId),mean)

# Merge tidyData with the activity table to include activity label descriptors
	tidyData <- merge(tidyData,activity,by='activityId',all.x=TRUE)

# Export tidyData set as a text file to active directory
	write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t')
