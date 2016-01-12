re<-function(){
      
      #Merges the training and the test sets to create one data set.
      dfeautureTrain  <- read.table(file.path("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train", "X_train.txt" ),header = FALSE)
      dfeautureTest  <- read.table(file.path("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test", "X_test.txt" ),header = FALSE)
      
      dfeature<-rbind(dfeautureTrain,dfeautureTest)
      
      dSubjectTrain  <- read.table(file.path("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train", "subject_train.txt" ),header = FALSE)
      dSubjectTest  <- read.table(file.path("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test", "subject_test.txt" ),header = FALSE)
     
      dSubject<-rbind(dSubjectTrain,dSubjectTest)
      
      dActTrain  <- read.table(file.path("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train", "y_train.txt" ),header = FALSE)
      dActTest  <- read.table(file.path("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test", "y_test.txt" ),header = FALSE)
      
      dActivity<-rbind(dActTrain,dActTest)
      #Code for appropriate labels
      
      featureHeader <- read.table(file.path("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/", "features.txt"),head=FALSE)
      subjectHeader<-c("subject")
      activityHeader<-c("activity")
      
      names(dSubject)<-subjectHeader
      names(dActivity)<- activityHeader
    
      names(dfeature)<- featureHeader$V2
      
      fullData<-cbind(dfeature,dSubject,dActivity)
      
      #Extracts only the measurements on the mean and standard deviation for each measurement.
      
      subdataFeaturesNames<-featureHeader$V2[grep("mean\\(\\)|std", featureHeader$V2)]
      finalData<-subset(fullData,select=c(as.character(subdataFeaturesNames), "subject", "activity" ))
      print(head(finalData,10))
      
      #Uses descriptive activity names to name the activities in the data set
      labels <- read.table(file.path("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset", "activity_labels.txt"),header = FALSE)
      
      labels<- rename(labels,activity=V1)
      finalData<-join(finalData,labels,by="activity")
      finalData<-select(finalData,-(activity))
      finalData<- rename(finalData,activity=V2)
      
      names(finalData)<-gsub("Acc", "Accelerometer", names(finalData))
      names(finalData)<-gsub("Gyro", "Gyroscope", names(finalData))
      names(finalData)<-gsub("Mag", "Magnitude", names(finalData))
      names(finalData)<-gsub("BodyBody", "Body", names(finalData))
      print(head(finalData,10))
            
      #creates a second, independent tidy data set with the average of each variable for each activity and each subject.
     
      grouped<-aggregate(. ~subject + activity, finalData, mean)
      ordered<-grouped[order(grouped$subject,grouped$activity),]
      print(head(ordered))
      write.table(ordered, file = "tidydata.txt",row.name=FALSE)
}

