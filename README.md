# Course3Week4
##Step 1: read all files into R;Before reading, files are already downloaded and unziped in the local folder C:/Users/Emma/Desktop/R/Course3/Week4/UCI HAR Datase"


st<-read.table("C:/Users/Emma/Desktop/R/Course3/Week4/UCI HAR Dataset/test/subject_test.txt")
xt<-read.table("C:/Users/Emma/Desktop/R/Course3/Week4/UCI HAR Dataset/test/X_test.txt")
yt<-read.table("C:/Users/Emma/Desktop/R/Course3/Week4/UCI HAR Dataset/test/y_test.txt")

str<-read.table("C:/Users/Emma/Desktop/R/Course3/Week4/UCI HAR Dataset/train/subject_train.txt")
xtr<-read.table("C:/Users/Emma/Desktop/R/Course3/Week4/UCI HAR Dataset/train/X_train.txt")
ytr<-read.table("C:/Users/Emma/Desktop/R/Course3/Week4/UCI HAR Dataset/train/y_train.txt")

## merge the training & test file

merge<- rbind(xt,xtr)
View(merge)

## read label & feature file
label <- read.table("C:/Users/Emma/Desktop/R/Course3/Week4/UCI HAR Dataset/activity_labels.txt")
feature <- read.table("C:/Users/Emma/Desktop/R/Course3/Week4/UCI HAR Dataset/features.txt")  

View(label)
View(feature)

##  select useful information from feature file

SelectFeature <- select(feature,2)
View(SelectFeature)

##  find out all mean() & std() names in SelectFeature file
ID <- grep("mean()|std()", SelectFeature$V2) 
View(ID)

## find mean() & std() data from merged file

newmerge<- merge[,ID]
names(newmerge)<-SelectFeature[ID,]
View(newmerge)

##  Give subject & activity data labels
subject<- rbind(str,st)
names(subject) <- "subject"
View(subject)

act<- rbind(ytr,yt)
names(act) <-"activity"
View(act)

## combine subject,act,newmerge files.
final<-cbind(subject,act,newmerge)

### Changed final$activity label to activity[,2] label
from <- factor(final$activity) 
levels(from) <- label[,2] ### linked newmerge & activity file
final$activity<-from
View(final)

## Step 8: Combine the column subject & activity to make it combined factor
twoinone<-unite(final,subject_activity,c("subject","activity"))
View(twoinone)

groupmean<-aggregate(twoinone[, 2:ncol(twoinone)], list(twoinone$subject_activity), mean)
View(groupmean)

## Get group average grouped by subject_activity column.
groupmean1<-twoinone %>% group_by(subject_activity) %>% summarise_at(.vars = names(.)[2:ncol(twoinone)], funs(mean(., na.rm=TRUE)))
View(groupmean1)

## separate the name
finalfinal<- separate(groupmean1,subject_activity,c("subject","activity"))
View(finalfinal)
