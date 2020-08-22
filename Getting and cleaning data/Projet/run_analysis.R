run_analysis <- function()
{
  library(reshape2)
  
  #load activity and features files
  activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt",col.names = c("index","activityname"))
  features <- read.table("./UCI HAR Dataset/features.txt",col.names = c("index","featurename"))
  
  #load databases, give names to the variables
  traindata <- read.table("./UCI HAR Dataset/train/X_train.txt")
  colnames(traindata)<-features[,2]
  testdata <- read.table("./UCI HAR Dataset/test/X_test.txt")
  colnames(testdata)<-features[,2]
  
  #Extract only mean and std measurement 
  traindata<- traindata[,grepl("mean\\()|std",features[,2])]
  testdata<- testdata[,grepl("mean\\()|std",features[,2])]
  
  #load subject and labels files
  trainsubject <- read.table("./UCI HAR Dataset/train/subject_train.txt" ,col.names = "subject")
  trainlabels <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "index")
  
  testsubject <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
  testlabels <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "index")
  
  #join train and test labels with activity labels
  jtrdata <- join(trainlabels,activityLabels,by="index")
  jtsdata <- join(testlabels,activityLabels,by="index")
  
  #train and test data with subjects and activities
  train <- cbind(trainsubject,Activityname=jtrdata[,2],traindata)
  test <- cbind(testsubject,Activityname=jtsdata[,2],testdata)

  #merge data
  mdata <- rbind(train,test)
  
  #create a tidy data
  meltdata <- melt(mdata,id.vars=c("subject","Activityname"))
  tidydata <- dcast(meltdata, subject + Activityname ~ variable, fun.aggregate = mean, na.rm=T)
  
  write.table(tidydata,"Tidydata.txt",quote = FALSE)
  
}