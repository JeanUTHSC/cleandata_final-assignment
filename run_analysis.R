library(tidyr)
library(dplyr)

# Read activity labels and feature information tables
activity_labels <- read.table("C:/Documents/R/jhu_assignment/UCI HAR Dataset/activity_labels.txt")
feature <- read.table("C:/Documents/R/jhu_assignment/UCI HAR Dataset/features.txt",header = FALSE)

# Load the test dataset and its subject and activity label tables.
test_subject <- read.table("C:/Documents/R/jhu_assignment/UCI HAR Dataset/test/subject_test.txt",header = FALSE)
test_set <- read.table("C:/Documents/R/jhu_assignment/UCI HAR Dataset/test/X_test.txt", header =FALSE)
test_label <- read.table("C:/Documents/R/jhu_assignment/UCI HAR Dataset/test/y_test.txt", header =FALSE)

#Load the training dataset and its subject and activity label tables.
train_set <- read.table("C:/Documents/R/jhu_assignment/UCI HAR Dataset/train/X_train.txt", header =FALSE)
train_labels <- read.table("C:/Documents/R/jhu_assignment/UCI HAR Dataset/train/y_train.txt", header =FALSE)
train_subject <- read.table("C:/Documents/R/jhu_assignment/UCI HAR Dataset/train/subject_train.txt", header =FALSE)

# Rename the column with descriptive names in activity label and subject tables for both test and training datasets
test_label <-rename(test_label, "activity_label"="V1")
test_subject <-rename(test_subject, "subject" = "V1")
train_labels <-rename(train_labels, "activity_label"="V1")
train_subject <-rename(train_subject, "subject" = "V1")

#Append activity label and subject tables as new columns to the test dataset
new_test_set<-cbind(test_set,test_label)
new_test_set<-cbind(new_test_set,test_subject)

Append activity label and subject tables as new columns to the training dataset
new_train_set <-cbind(train_set,train_labels)
new_train_set <-cbind(new_train_set,train_subject)

#Merge the test and training dataset with activity label and subject information
combine_set <- rbind(new_test_set, new_train_set)

#Extract the feature names for mean and standard deviation variables
mean_feature <-filter(feature, grepl("mean", feature$V2))
std_feature <-filter(feature, grepl("std", feature$V2))
mean_std_feature <- rbind(mean_feature,std_feature)

#Extract the mean and standard deviation measurements out of the merged new dataset which contains both test and training sets.
mean_std_combine <- select(combine_set, num_range("V", mean_std_feature$V1))

#Append the activity label and subject information as new columns to the extracted dataset
activity_combine <-select(combine_set,"activity_label")
mean_std_activity_combine <-cbind (mean_std_combine,activity_combine)
combine_subject <- select (combine_set, contains("subject"))
mean_std_activity_subject_combine<-cbind(mean_std_activity_combine, combine_subject)

#Convert the data type in activity label column from integer to character
mean_std_activity_subject_combine$activity_label <- as.character(mean_std_activity_subject_combine$activity_label)

#Take a look at the activity label information as described.
View(activity_labels)

#Recode the values in activity label column to be the descriptive activity names
mean_std_activity_subject_combine <- mean_std_activity_subject_combine %>% mutate(activity_label = replace(activity_label, activity_label=="1", "walking")) %>% mutate(activity_label = replace(activity_label, activity_label=="2", "walking_upstairs")) %>% mutate(activity_label = replace(activity_label, activity_label=="3", "walking_downstairs")) %>% mutate(activity_label = replace(activity_label, activity_label=="4", "sitting")) %>% mutate(activity_label = replace(activity_label, activity_label=="5", "standing")) %>% mutate(activity_label = replace(activity_label, activity_label=="6", "laying"))

#Take a look at the feature information which contains only mean and standard deviation columns
View(mean_std_feature)

#Take a look at the column names of the extracted dataset
colnames(mean_std_activity_subject_combine)

#Transform the feature label to match the format of extracted dataset measurement column names
mean_std_feature$V1 <-as.character(mean_std_feature$V1)
mean_std_feature$V1 <- paste0("V", mean_std_feature$V1)

#Rename the columns in extracted dataset with descriptive variable names as noted in feature information
oldnames <- as.character(mean_std_feature$V1) 
newnames <-as.character(mean_std_feature$V2)
mean_std_activity_subject_combine <- rename_at(mean_std_activity_subject_combine, vars(oldnames), ~newnames)

# Group the extracted dataset by activity and subject information
all_group <-group_by(mean_std_activity_subject_combine, activity_label,subject)

#Create an independent dataset which summarize the average of each variable for each activity and each suject
all_group <-summarize_each(all_group, funs(mean))

#Save the tidy dataset as CSV file
write.csv(all_group,"tidy data_UCI HAR.csv")
