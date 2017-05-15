#Read in all data
test <- read.table("test/X_test.txt")
train <- read.table("train/X_train.txt")
testlabels <- read.table("test/y_test.txt")
trainlabels <- read.table("train/y_train.txt")
activitylabels <- read.table("activity_labels.txt")
subjectlabelstrain <- read.table("train/subject_train.txt")
subjectlabelstest <- read.table("test/subject_test.txt")
features <- read.table("features.txt")

#Combine activity and subject labels with data sets
test <- cbind(testlabels,subjectlabelstest,test)
train <- cbind(trainlabels,subjectlabelstrain,train)

#Merge the training and the test sets to create one data set.
#row bind the sets
allData <- rbind(test,train)

#Re-label column headers with feature names
colnames(allData) <- c("Activity","Subject",as.character(features[,2]))

#Extracts only the measurements on the mean and standard deviation 
#for each measurement.

meanSD <- cbind(allData[,1:2],allData[,grep("mean|std",colnames(allData))])

#Use descriptive activity names to name the activities in the data set

meanSD <- merge(activitylabels,meanSD,by.x="V1",by.y="Activity",all.y=T)
meanSD <- meanSD[,-1]
names(meanSD)[1] <- "Activity"

#Appropriately labels the data set with descriptive variable names.

getListElement <- function(x,n) {x[n]}
splitNames <- strsplit(colnames(meanSD)[3:ncol(meanSD)],"-")

activities <- sapply(splitNames,getListElement,n=1)

measures <- sapply(splitNames,getListElement,n=2)
measures <- gsub("\\()","",measures)

axis <- sapply(splitNames,getListElement,n=3)
axis[!is.na(axis)] <- paste(axis[!is.na(axis)],"Axis",sep="")
axis[is.na(axis)] <- ""

timeFreq <- substr(activities,1,1)
activityType <- substr(activities,2,nchar(activities))
timeFreq <- ifelse(timeFreq=="t","time","frequency")
names(meanSD)[3:ncol(meanSD)] <- 
  paste(measures,axis,activityType,timeFreq,sep="")

#From the data set in step 4, 
#creates a second, independent tidy data set with the average of each 
#variable for each activity and each subject.

meanSD2<-aggregate(. ~Subject + Activity, meanSD, mean)
meanSD2<-meanSD2[order(meanSD2$Subject,meanSD2$Activity),]
