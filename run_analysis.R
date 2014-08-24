DeriveTidy1 <- function(dataRootDir = "UCI HAR Dataset") {
  # utility function
  FilePath <- function(file) {
    paste(dataRootDir,"/",file,sep="")
  }
  # file locations
  XTestFile <- FilePath("test/X_test.txt")
  XTrainFile <- FilePath("train/X_train.txt")
  FeaturesFile <- FilePath("features.txt")
  ActivityLabelsFile <- FilePath("activity_labels.txt")
  TestActivitiesFile <- FilePath("test/y_test.txt")
  TrainActivitiesFile <- FilePath("train/y_train.txt")
  SubjectTestFile <- FilePath("test/subject_test.txt")
  SubjectTrainFile <- FilePath("train/subject_train.txt")
  #merge training and test sets
  testSet <- read.table(XTestFile)
  trainingSet <- read.table(XTrainFile)
  allObservations <- rbind(testSet,trainingSet)
  #add feature names as column names
  featureNames <- read.table(FeaturesFile,stringsAsFactors=FALSE)[[2]]
  colnames(allObservations) <- featureNames
  #columns that have mean, std or activityLabel in their name
  allObservations <- allObservations[,grep("mean|std|activityLabel",featureNames)]
  #rename variable names to more readable form.
  varNames = names(allObservations)
  varNames <- gsub(pattern="^t",replacement="time",x=varNames)
  varNames <- gsub(pattern="^f",replacement="freq",x=varNames)
  varNames <- gsub(pattern="-?mean[(][)]-?",replacement="Mean",x=varNames)
  varNames <- gsub(pattern="-?std[()][)]-?",replacement="Std",x=varNames)
  varNames <- gsub(pattern="-?meanFreq[()][)]-?",replacement="MeanFreq",x=varNames)
  varNames <- gsub(pattern="BodyBody",replacement="Body",x=varNames)
  names(allObservations) <- varNames
  #use the activity names to name the activities in the set
  activityLabels <- read.table(ActivityLabelsFile,stringsAsFactors=FALSE)
  colnames(activityLabels) <- c("activityID","activityLabel")
  #label the data set with descriptive activity names
  testActivities <- read.table(TestActivitiesFile,stringsAsFactors=FALSE)
  trainingActivities <- read.table(TrainActivitiesFile,stringsAsFactors=FALSE)
  allActivities <- rbind(testActivities,trainingActivities)
  #assign a column name so we can merge on it
  colnames(allActivities)[1] <- "activityID"
  activities <- join(allActivities,activityLabels,by="activityID")
  #and add the column to the entire dataset
  allObservations <- cbind(activity=activities[,"activityLabel"],allObservations)
  #extra step: include the subject ids, for processing in the next step
  testSubjects <- read.table(SubjectTestFile,stringsAsFactors=FALSE)
  trainingSubjects <- read.table(SubjectTrainFile,stringsAsFactors=FALSE)
  allSubjects <- rbind(testSubjects,trainingSubjects)
  colnames(allSubjects) <- "subject"
  allObservations <- cbind(allSubjects,allObservations)
  sorted <- allObservations[order(allObservations$subject,allObservations$activity),]
  sorted
}
DeriveTidy2 <- function(rawData) {
  #create a long shaped dataset from a wide shaped dataset
  molten <- melt(rawData,id.vars= c("subject","activity"))
  #transform the long shaped dataset back into a wide shaped dataset, aggregating on subject
  #and activity using the mean function
  cast <- dcast(molten, subject+activity ~ variable, fun.aggregate=mean)
  cast
}
DeriveAndWriteDataSets <- function() {
  tidy1 <- DeriveTidy1()
  tidy2 <- DeriveTidy2(tidy1)
  write.csv(tidy1,file="tidy1.csv")
  write.csv(tidy2,file="tidy2.csv")
}
