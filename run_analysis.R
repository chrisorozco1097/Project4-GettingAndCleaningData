# Reading the file
X_test <- read.table("/Users/christianorozco/Downloads/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("/Users/christianorozco/Downloads/UCI HAR Dataset/test/y_test.txt")
X_train <- read.table("/Users/christianorozco/Downloads/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("/Users/christianorozco/Downloads/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("/Users/christianorozco/Downloads/UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("/Users/christianorozco/Downloads/UCI HAR Dataset/test/subject_test.txt")
features <- read.table("/Users/christianorozco/Downloads/UCI HAR Dataset/features.txt")
features<- t(features[2])
activities<-read.table("/Users/christianorozco/Downloads/UCI HAR Dataset/activity_labels.txt")

#1 - Merges the training and the test sets to create one data set.
colnames(X_train)<-colnames(X_test)
X<- rbind(X_test,X_train)
colnames(y_train)<-colnames(y_test)
Y<-rbind(y_test,y_train)
colnames(subject_train)<-colnames(subject_test)
Subject<-rbind(subject_test,subject_train)
mergedData<-cbind(X,Y,Subject)

#2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_pos<-grep('mean',features)
std_pos<-grep('std',features)
positions<-sort(c(mean_pos,std_pos))
X_meanstd<-X[positions]
mergedData_meanstd<-cbind(X_meanstd,Y,Subject)

#3 - Uses descriptive activity names to name the activities in the data set
mergedData_act<-mergedData_meanstd
for (i in 1:length(mergedData_meanstd[,80])) {
  if(mergedData_meanstd[i,80]==1){
    mergedData_act[i,80]<-activities[1,2]
  }
  else if(mergedData_meanstd[i,80]==2){
    mergedData_act[i,80]<-activities[2,2]
  }
  else if(mergedData_meanstd[i,80]==3){
    mergedData_act[i,80]<-activities[3,2]
  }
  else if(mergedData_meanstd[i,80]==4){
    mergedData_act[i,80]<-activities[4,2]
  }
  else if(mergedData_meanstd[i,80]==5){
    mergedData_act[i,80]<-activities[5,2]
  }
  else{
    mergedData_act[i,80]<-activities[6,2]
  }
}
mergedData_act[,80]

#4 - Appropriately labels the data set with descriptive variable names. 
colnames(mergedData_act)<-c(features[positions],'activities','Subject')
mergedData_labels<-mergedData_act[order(mergedData_act$Subject),]

#5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Tidy_data<-mergedData_labels%>%group_by(activities,Subject)%>%summarise_each(funs(mean),colnames(mergedData_labels[1:79]))