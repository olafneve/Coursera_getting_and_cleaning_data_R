library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)

# load data

downloadURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dest_dir <- getwd()

download.file(downloadURL, dest_dir)
unzip("getdata_projectfiles_UCI HAR Dataset.zip")

testdata <- as.tibble(read.table("./UCI HAR Dataset/test/X_test.txt"))
testdata_subject <- as.tibble(read.table("./UCI HAR Dataset/test/subject_test.txt"))
testdata_y <- as.tibble(read.table("./UCI HAR Dataset/test/y_test.txt"))
traindata <- as.tibble(read.table("./UCI HAR Dataset/train/X_train.txt"))
traindata_subject <- as.tibble(read.table("./UCI HAR Dataset/train/subject_train.txt"))
traindata_y <- as.tibble(read.table("./UCI HAR Dataset/train/y_train.txt"))
features <- as.vector(read.table("./UCI HAR Dataset/features.txt"))
features_clean <- features[,2]
names (testdata) <- features_clean
names (traindata) <- features_clean
features_clean <- append(features_clean, c("activitycode", "subject"))

# inclusion of variable names, activity code, and subject code 

traindata <- add_column(traindata, traindata_y)
traindata <- add_column(traindata, traindata_subject)

testdata <- add_column(testdata, testdata_y)
testdata <- add_column(testdata, testdata_subject)

names (testdata) <- features_clean
names (traindata) <- features_clean

# merge test and train set 

data_merged <- bind_rows(traindata, testdata)
additional_row <- select(data_merged, activitycode:subject)


# extract measurements on the mean and the standard deviations of each measurement 

mean_std_data <- select (data_merged, grep("mean|std", names(data_merged), value=TRUE))
mean_std_data <- add_column(mean_std_data, additional_row)


# use describe activity names to name activities in the data set


activity_labels <- c("walking", "walking_upstairs", "walking_downstairs", "sitting", "standing", "laying")
mean_std_data$activitycode <- factor(mean_std_data$activitycode, levels = c(1,2, 3, 4, 5, 6), labels = activity_labels)


#Appropriately label the data set with descriptive labels

features_descriptive <- tolower(names(mean_std_data))
features_descriptive <- replace("tbody"= "time" , features_descriptive, )
features_descriptive <- sub("tbody", "time", features_descriptive)
features_descriptive <- sub("acc", "acceleration", features_descriptive)
features_descriptive <- sub("fbody", "frequency", features_descriptive)
features_descriptive <- sub("tgravity", "timegravity", features_descriptive)
features_descriptive <- sub("mag_", "magnitude_", features_descriptive)
features_descriptive <- gsub("\\(", "", features_descriptive)
features_descriptive <- gsub("\\)", "", features_descriptive)
features_descriptive
features_descriptive <- gsub("-", "_", features_descriptive)
names(mean_std_data) <- features_descriptive


# form a tidy dataset  with the average of each variable for each activity and each subject 

tidy_data_prep <- group_by(mean_std_data, subject, activitycode)
tidy_data <- summarise_all(tidy_data_prep, mean)
write.table(tidy_data, file = "tidy_data.txt", row.name = FALSE)
