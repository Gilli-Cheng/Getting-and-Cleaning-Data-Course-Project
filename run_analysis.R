rm(list = ls())
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# made up the path >> download >> unzip
filename <- "UCI HAR Dataset.zip"
if(!file.exists(filename)){
        download.file(url = url,destfile = filename, method = "curl")
        unzip(zipfile = filename)
}

# read data
features <- read.table(file = "UCI HAR Dataset/features.txt",col.names = c("num", "features"))

train_set <- read.table(file = "UCI HAR Dataset/train/X_train.txt", col.names = features$features)
train_label <- read.table(file = "UCI HAR Dataset/train/y_train.txt", col.names = "label")
subject_train <- read.table(file = "UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

test_set <- read.table(file = "UCI HAR Dataset/test/X_test.txt", col.names = features$features)
test_label <- read.table(file = "UCI HAR Dataset/test/y_test.txt",col.names = "label")
subject_test <- read.table(file = "UCI HAR Dataset/test/subject_test.txt", col.names = "subject")


labels <- read.table(file = "UCI HAR Dataset/activity_labels.txt", col.names = c("label","activity"))

# merge ALLL together 
# participants(subject)/activity label/measurement

X <- rbind(train_set,test_set)
y <- rbind(train_label,test_label)
subject <- rbind(subject_train, subject_test)
merge <- cbind(subject,y,X)
# it cannot be merge afterward

library("dplyr")

#patterns <- c("mean","std")
#merge <- cbind(subject,y,merge[,grepl(paste(patterns,collapse = "|"),names(merge))==TRUE])
merge <- merge %>% 
        select(subject,label,contains("mean"),contains("std"))

merge <- merge(labels,merge,by.x = "label",by.y = "label") %>% 
        select(-(label))

converter <- function(x){
        as.data.frame(x)
        names(x) <- gsub("BodyBody","Body "    ,names(x))
        names(x) <- gsub("^t","Time "          ,names(x))
        names(x) <- gsub("Freq","Frequency "     ,names(x))
        names(x) <- gsub("^f","Frequency "     ,names(x))
        names(x) <- gsub("angle.","Angle "     ,names(x))
        names(x) <- gsub("Acc","Accelerometer ",names(x))
        names(x) <- gsub("Gyro","Gyroscope "   ,names(x))
        names(x) <- gsub("Mag","Magnitude "    ,names(x))
        names(x) <- gsub("Body","Body "        ,names(x))
        names(x) <- gsub("Jerk","Jerk "        ,names(x))
        names(x) <- gsub("gravity","Gravity "  ,names(x))
        names(x) <- gsub("Gravity","Gravity "  ,names(x))
        names(x) <- gsub(".std","-STD "  ,names(x))
        names(x) <- gsub(".mean","-Mean  "  ,names(x))
        names(x) <- gsub("Mean.","Mean"  ,names(x))
        names(x) <- gsub("  "," ",names(x))
        names(x) <- gsub(" ..$","",names(x))
        names(x) <- gsub("\\...","",names(x))
        as.data.frame(x)
}

merge <- converter(merge)

clean <- merge %>%
        group_by(subject,activity) %>%
        summarize_all(funs(mean))
#getwd()
write.table(clean, file = "./CleanData.txt",row.name = F)
write.table(clean,file = "./CleanData.csv", sep = ",")
