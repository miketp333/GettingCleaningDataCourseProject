# ##### Getting and Cleaning Data Course Project ##### #

setwd("~/DataScienceCoursera/Getting_Cleaning_Data/CourseProject")
fpath <- "~/DataScienceCoursera/Getting_Cleaning_Data/CourseProject"

#Activity Data
ActivityTest  <- read.table(file.path(fpath, "test" , "Y_test.txt" ),header = FALSE)
ActivityTrain <- read.table(file.path(fpath, "train", "Y_train.txt"),header = FALSE)
#Subject Data
SubjectTest  <- read.table(file.path(fpath, "test" , "subject_test.txt"),header = FALSE)
SubjectTrain <- read.table(file.path(fpath, "train", "subject_train.txt"),header = FALSE)
#Features
FeaturesTest  <- read.table(file.path(fpath, "test" , "X_test.txt" ),header = FALSE)
FeaturesTrain <- read.table(file.path(fpath, "train", "X_train.txt"),header = FALSE)

#Merge tables by rows
activityData <- rbind(ActivityTrain,ActivityTest)
subjectData <- rbind(SubjectTrain, SubjectTest)
featuresData <- rbind(FeaturesTrain,FeaturesTest)

#Set variable names
names(activityData) <- c("activity")
names(subjectData) <- c("subject")
featureNames <- read.table(file.path(fpath, "features.txt"),head=FALSE)
names(featuresData) <- featureNames$V2

#Merge data
dataMerge <- cbind(subjectData, activityData)
df <- cbind(featuresData, dataMerge)

#Create df of only mean and stand dev measurements, and subject/activity ids
sdMeanFeatures <- featureNames$V2[grep("mean\\(\\)|std\\(\\)",featureNames$V2)]
desiredCols <- c(as.character(sdMeanFeatures),"subject","activity")
df <- df[,desiredCols]

#Name activities in df
activityLabels <- read.table(file.path(fpath, "activity_labels.txt"),header = FALSE)
df = merge(df,activityLabels,by.x="activity",by.y="V1",all.x=TRUE)
df$activty <- df$V2
df$V2 <- NULL
unique(df$subject)

#Clean Column Names
colNames = colnames(df)
for (i in 1:length(colNames)) {
    colNames[i] = gsub("\\()","",colNames[i])
    colNames[i] = gsub("-std$","StdDev",colNames[i])
    colNames[i] = gsub("-mean","Mean",colNames[i])
    colNames[i] = gsub("^(t)","time",colNames[i])
    colNames[i] = gsub("^(f)","freq",colNames[i])
    colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
    colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
    colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
    colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
    colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
    colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
    colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}
colnames(df) = colNames

#Create tidy data set
library(plyr)
df2 <- aggregate(. ~subject + activity, df, mean)
df2<- df2[order(df2$subject,df2$activity),]
write.table(df2, file = "tidydata.txt",row.name=FALSE)



