setwd("/Users/viktort/Desktop/Data Processing Software/")


# Read Data
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n", "features"))
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("label", "activity"))

X_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$features)
y_train <- read.table("UCI HAR Dataset/train/Y_train.txt", col.names = "label")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

X_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$features)
y_test <- read.table("UCI HAR Dataset/test/Y_test.txt", col.names = "label")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")


# Q1. Merge the training and test sets to create one data set. 
merged_df <- cbind(rbind(X_train, X_test), rbind(y_train, y_test), rbind(subject_train, subject_test))

# Q2. Extracts only the measurements on the mean and standard deviation for each measurement.
df_extracted <- merged_df[, c(grep(".*mean.*|.*std.*", names(merged_df), ignore.case=TRUE), 562, 563)]


# Q3. Uses descriptive activity names to name the activities in the data set
df_extracted$activity <- activity_labels[merged_df$label, 2]


# Q4. Appropriately labels the data set with descriptive variable names. 
names(df_extracted)<-gsub("^t", "Time", names(df_extracted))
names(df_extracted)<-gsub("^f", "Frequency", names(df_extracted))
names(df_extracted)<- gsub("-freq()", "Frequency", names(df_extracted), ignore.case = TRUE)
names(df_extracted)<- gsub("Acc", "Accelerometer", names(df_extracted))
names(df_extracted)<-gsub("Acc", "Accelerometer", names(df_extracted))
names(df_extracted)<-gsub("Gyro", "Gyroscope", names(df_extracted))
names(df_extracted)<-gsub("Mag", "Magnitude", names(df_extracted))
names(df_extracted)<- gsub("tBody", "TimeBody", names(df_extracted))
names(df_extracted)<-gsub("BodyBody", "Body", names(df_extracted))

# Q5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
final <- df_extracted %>% group_by(subject, activity) %>% summarise_all(list(mean))
final



