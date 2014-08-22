## This function loads all the data into a single data.frame, extracts only mean and std features and averages the 
##    those means together for each subject and activity
run_analysis <- function() {
  
        # does the variable rawData exist
        #if (!exists("rawData") || is.na(rawData)){
          # Merges the training and the test sets to create one data set.
          test <- loadAndMerge("test", "X_test.txt", "y_test.txt", "subject_test.txt")
          train <- loadAndMerge("train", "X_train.txt", "y_train.txt", "subject_train.txt")
          rawData <- rbind(test, train)
        #}
        
        # Appropriately labels the data set with descriptive variable names. 
        # Uses descriptive activity names to name the activities in the data set
        featureFile <- read.table(file="features.txt")
        features <- subset(featureFile, select=V2)
        # bind the y column name and unlist the resulting data frame
        column_names <- unlist(rbind(data.frame(V2=c("subject", "activity")), features))
        # set the column names of the rawData to the features
        names(rawData) <- column_names
        
        # create a boolean vector of TRUE for mean and std columns and FALSE for the rest
        columns_required <- grep(pattern="subject|activity|Mean|Std", x=column_names, ignore.case=TRUE)
        # Extracts only the measurements on the mean and standard deviation for each measurement. 
        wideData <- rawData[, columns_required]
        
        # convert the df with many columns into a four column df 
        longData <- melt(wideData, id=c("subject","activity"))
        # group by subject and activity
        groupedData <- aggregate(value~subject+activity, data=longData, FUN=mean)
        
        # apply the activity labels
        activityLabels <- loadFile("activity_labels.txt")
        # merge the activity labels to the grouped data
        labeledData <- merge(x=groupedData, y=activityLabels, by.x="activity", by.y="V1")
        
        # extract the necessary columns
        tidyData <- subset(labeledData, select=c("subject", "V2", "value"))
        names(tidyData) <- c("subject", "activity", "mean.value")
        
        # Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        write.table(x=tidyData, file="course_proejct_output.txt", row.name=FALSE)
        
        #return the tidy data set
        tidyData
}

## This function merges the three files into a single data.frame
loadAndMerge <- function(path, xName, yName, sName){
        # load X file - features
        xFile <- loadFile(checkFileExists(paste(path, "/", xName, sep="")))
        
        # load Y file - activities
        yFile <- loadFile(checkFileExists(paste(path, "/", yName, sep="")))
        
        # load s file - subjects
        sFile <- loadFile(checkFileExists(paste(path, "/", sName, sep="")))
        
        data <- cbind(sFile, yFile, xFile)
        data
}

## This function loads the file into a data.frame
loadFile <- function(nameAndPath){
        tbl <- read.table(file = nameAndPath) 
        tbl
}

## This function checks that the file name exists 
checkFileExists <- function(nameAndPath){
        if (!file.exists(nameAndPath)){
                stop(paste("File does not exist: ", nameAndPath))
        }  
        
        nameAndPath
}
