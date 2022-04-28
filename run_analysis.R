# importing libraries
library(data.table)

# Download zip file
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dir.create("data", showWarnings = FALSE)
path_to_file <- file.path("data", "data.zip")
if (!file.exists(path_to_file)) {
  download.file(url, destfile = path_to_file, method = "curl")
}

# Find out what directories are in the zip file
dirs <- c()
for (file_path in unzip(path_to_file, list = TRUE, exdir = "data")["Name"]) {
  dirs <- union(dirs, strsplit(file_path, .Platform$file.sep, fixed = TRUE)[[1]][1])
}

# Check if this directory is already exists so that we don't unzip reapetedly
dirs_flag <- 0
for (dir in dirs) {
  if (!file.exists(file.path("data", dir))) {
    dirs_flag <- 1
    break
  }
}

# Unzipping if there is at least one missing directory
if (dirs_flag) {
  unzip(path_to_file, exdir = "data")
}

# read train data
train <- "train"

# read X_train
X_train_path <- shortPathName(file.path("data", dirs[1], train, paste("X_", train, ".txt", sep = "")))
X_train <- read.table(X_train_path)

# read y_train
y_train_path <- shortPathName(file.path("data", dirs[1], train, paste("y_", train, ".txt", sep = "")))
y_train <- read.table(y_train_path)

# read subject_train
subject_train_path <-shortPathName(file.path("data", dirs[1], train, paste("subject_", train, ".txt", sep = "")))
subject_train <- read.table(subject_train_path)

# cbinding train data
train_dataset <- cbind(X_train, y_train, subject_train)


# read test data
test <- "test"

# read X_test
X_test_path <- shortPathName(file.path("data", dirs[1], test, paste("X_", test, ".txt", sep = "")))
X_test <- read.table(X_test_path)

# read y_test
y_test_path <- shortPathName(file.path("data", dirs[1], test, paste("y_", test, ".txt", sep = "")))
y_test <- read.table(y_test_path)

# read subject_test
subject_test_path <-shortPathName(file.path("data", dirs[1], test, paste("subject_", test, ".txt", sep = "")))
subject_test <- read.table(subject_test_path)

# cbinding test data
test_dataset <- cbind(X_test, y_test, subject_test)

# 1. Merges the training and the test sets to create one data set.
dataset <- rbind(train_dataset, test_dataset)

# read table with column names
columns <- read.table(file.path("data", dirs[1], "features.txt"), col.names = c("index", "column_name"))

# get indices of the columns whose names contain either mean() or std()
indices_of_columns <- grep("(mean|std)\\(\\)", columns$column_name)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
dataset <- dataset[, c(indices_of_columns, c(562, 563))]

# 4. Appropriately labels the data set with descriptive variable names.
column_names <- columns[indices_of_columns, "column_name"]
setnames(dataset, colnames(dataset), c(as.character(column_names), c("activity", "subject")))

# 3. Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table(file.path("data", dirs[1], "activity_labels.txt"), col.names = c("index", "activity_label"))
dataset[["activity"]] <- factor(dataset$activity, labels = activity_labels[["activity_label"]])
dataset[["subject"]] <- factor(dataset$subject)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_dataset <- aggregate(dataset[, 1:(length(dataset) - 2)], by=list(activity = dataset$activity, subject=dataset$subject), mean)

write.table(tidy_dataset, "tidy_dataset.txt", sep="\t")
