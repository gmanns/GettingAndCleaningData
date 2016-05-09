# GettingAndCleaningData

###Load required packages
require(dplyr)
require(data.table)
require(tidyr)


ProjectPath<-getwd()

##1. Merge the training and test sets to create one data set
###read in test files
    xTest<-read.table(file.path(ProjectPath, "./UCI HAR Dataset/test/x_test.txt"))
    yTest<-read.table(file.path(ProjectPath, "./UCI HAR Dataset/test/y_test.txt"))
    subject_test<-read.table(file.path(ProjectPath, "./UCI HAR Dataset/test/subject_test.txt"))
  
###read in train files
    xTrain<-read.table(file.path(ProjectPath, "./UCI HAR Dataset/train/x_train.txt"))
    yTrain<-read.table(file.path(ProjectPath, "./UCI HAR Dataset/train/y_train.txt"))
    subject_train<-read.table(file.path(ProjectPath, "./UCI HAR Dataset/train/subject_train.txt"))
  
###read in data feature names and activity labels
    FeatureNames<-read.table(file.path(ProjectPath, "./UCI HAR Dataset/features.txt"))
    ActivityLabels<-read.table(file.path(ProjectPath, "./UCI HAR Dataset/activity_labels.txt"))
  
###merge files
    dataFeatures<-rbind(xTest, xTrain, deparse.level = 1)
    dataActivities<-rbind(yTest, yTrain, deparse.level = 1)
    dataSubject<-rbind(subject_test, subject_train, deparse.level = 1)
  
  
###rename headers for the data files
    names(dataFeatures)<-FeatureNames$V2
    setnames(dataActivities, "V1", "ActivityNum")
    setnames(dataSubject, "V1", "subject")
    setnames(ActivityLabels, names(ActivityLabels), c("ActivityNum", "ActivityName"))
  
  
### Merge into one data file
    dataSubjAct<- cbind(dataSubject, dataActivities)
    dataAll <- cbind(dataSubjAct, dataFeatures)




##2. Extract only the measurements on the mean and standard deviation for each measurement
###finding feature names with mean or std deviation
    MeanStd<-FeatureNames$V2[grep("mean\\(\\)|std\\(\\)", FeatureNames$V2)]
  
  
###subset on only those with mean or std deviation in feature name
    selectedFeatures<-c("subject", "ActivityNum", as.character(MeanStd))
    dataMeanStd<-subset(dataAll, select = selectedFeatures)


##3.use descriptive activity names to name the activities in the data set 

    dataMeanStd <- merge(ActivityLabels, dataMeanStd , by="ActivityNum", all.x=TRUE)
    dataMeanStd$ActivityName <- as.character(dataMeanStd$ActivityName)

##4. Appropriately label the data set with descriptive variable names

    names(dataMeanStd)<-gsub("^t", "Time", names(dataMeanStd))
    names(dataMeanStd)<-gsub("^f", "Frequency", names(dataMeanStd))
    names(dataMeanStd)<-gsub("Acc", "Accelerometer", names(dataMeanStd))
    names(dataMeanStd)<-gsub("Gyro", "Gyroscope", names(dataMeanStd))
    names(dataMeanStd)<-gsub("Mag", "Magnitude", names(dataMeanStd))
    names(dataMeanStd)<-gsub("BodyBody", "Body", names(dataMeanStd))



##5. Create a second, independent tidy data set with the average of each variable for each activity and each subject
    dataAggr<- aggregate(. ~ subject - ActivityName, data = dataMeanStd, mean) 
    dataTable<- tbl_df(arrange(dataAggr,subject,ActivityName))                    
    write.table(dataTable, file = "wk4Project-tidydata.txt",row.name=FALSE)

