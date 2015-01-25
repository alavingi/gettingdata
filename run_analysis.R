# read training data
training <- read.table("train/X_train.txt")

# read training subject data
train_subject <- read.table("train/subject_train.txt")

# read training activity data
train_activity <- read.table("train/y_train.txt")

# merge the above data frames
training$subject <- train_subject
training$activity <- train_activity

# repeat for test data

# read test data
testing <- read.table("test/X_test.txt")

# read test subject data
test_subject <- read.table("test/subject_test.txt")

# read test activity data
test_activity <- read.table("test/y_test.txt")

# merge the above data frames
subject <- rbind(train_subject, test_subject)
activity <- rbind(train_activity, test_activity)


# use only relevant columns in the two data sets
# read all features
features <- read.table("features.txt")

# extract mean and std columns
col_subset <- grep("mean|std", features$V2)
# add subject and activity
col_subset <- c(col_subset, 562:563)

# subset the data
training <- training[,col_subset]
testing <- testing[, col_subset]

# now merge the two files
all_data <- rbind(training, testing)
subject <- rbind(train_subject, test_subject)
subject <- subject[, "V1"]
activity <- rbind(train_activity, test_activity)
activity <- activity[, "V1"]


all_data$subject <- subject
all_data$activity <- activity

# clean column names

colnames(all_data) <- make.names(features$V2[col_subset], unique = TRUE)
colnames(all_data)[80] <- "subject"
colnames(all_data)[81] <- "activity"

# read activity descriptions
activities <- read.table("activity_labels.txt")
descriptions <- activities[,"V2"]
descriptions <- as.character(descriptions)

# use descriptive activity names
for (i in 1:nrow(all_data)) {
    index <- all_data[i,]$activity
    all_data[i,]$activity <- descriptions[as.numeric(index)]
    
}

# create empty data frame for summary data
summary <- read.csv(text = "", col.names = c("subject", "activity",make.names(features$V2[col_subset], unique = TRUE)))
summary[,82] <- NULL
summary[,83] <- NULL


# populate summary data
for (i in 1:30) {
    for (j in descriptions){
        x <- c(i, j)
        for (k in 1:79){
            x <- c(x, mean(all_data[all_data$subject ==i & all_data$activity == j,k]))
        }
        summary <- rbind(summary, x)
    }
}

# replace with descriptive column names for summary data
cols <- colnames(summary)
cols <- gsub("...X", "X-axis", cols)
cols <- gsub("...Y", "Yaxis", cols)
cols <- gsub("...Z", "Zaxis", cols)
cols <- gsub("mean", "Mean", cols)
cols <- gsub("std", "STD", cols)
cols <- gsub("Gyro", "Gyroscope", cols)
cols <- gsub("Acc", "Accelerometer", cols)
cols <- gsub("Mag", "Magnitude", cols)
cols <- gsub("^t", "time", cols)
cols <- gsub("^f", "frequency", cols)
cols <- gsub("..$", "", cols)

colnames(summary) <- cols

# write out the summary data
write.table(summary, file = "summary.csv", sep = ",", row.names = FALSE)




