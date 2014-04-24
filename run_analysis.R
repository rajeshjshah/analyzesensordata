# This script is developed to analyze the Samsung data.
# Please carefully review README.md.
# Set the working directory as "/UCI HAR Dataset" for the successful execution of the code.
# Samsung data should be in working directory.

#Read all the data sets required for this assignment.

testdataset<- read.table("test/X_test.txt")
traindataset<-read.table("train/X_train.txt")
features <-read.table("features.txt")
subjectTestdata<-read.table("test/subject_test.txt")
activitytestdata<-read.table("test/y_test.txt")
subjectTraindata<-read.table("train/subject_train.txt")
activitytraindata<-read.table("train/y_train.txt")
activitylables<-read.table("activity_labels.txt")

#Merge the traindataset and testdataset in to one set

mergedDataset<-rbind(traindataset,testdataset)

#Extract the Columns with STD() and MEAN() from features data set

stdmeancols <- grep("STD()",toupper(features[,2]),fixed=TRUE)
stdmeancols <- append(stdmeancols, grep("MEAN()",toupper(features[,2]),fixed=TRUE),after=length(stdmeancols))
stdmeancols <- sort(stdmeancols)

#Extract the Columns with STD() and MEAN() from merged data set

extractstdmeancols <- subset(mergedDataset,select=c(stdmeancols))
extractstdmeancolnames <- features[stdmeancols,2]
colnames(extractstdmeancols) <-extractstdmeancolnames

#Join subject and activity columns of test and train

subtestact <- cbind(subjectTestdata,activitytestdata)
subtrainact <-cbind(subjectTraindata,activitytraindata)
mergesubact <-rbind(subtrainact,subtestact)
colnames(mergesubact) <- c("Subject","Activity")

#Merge the subject and activity along with merged data set

mergedwithsubact <-cbind(mergesubact,extractstdmeancols)
nocols <-ncol(mergedwithsubact)

#Calculate the mean and order the data properly

meandata <-aggregate(mergedwithsubact[,3:nocols], by=list(mergedwithsubact[,1],mergedwithsubact[,2]),FUN=mean, na.rm=TRUE)
colnames(meandata)<-names(mergedwithsubact)
meandata<-meandata[order(meandata[,1],meandata[,2]),]

#Update with correct activity names

Activity_Name <-activitylables[match(meandata[,2],activitylables[,1]),2]
meandata<-cbind(Activity_Name,meandata)
nocols <-ncol(meandata)
meandata<-meandata[,c(2,3,1,4:nocols)]

#Create tidy data set

write.table(meandata,file="tidy_data_set.txt", sep=" ",row.names=FALSE)

