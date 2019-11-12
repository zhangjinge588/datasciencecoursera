
# Step 1.
# Merges the training and the test sets to create one data set.
X_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
X_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

y_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)

train_data <- cbind(X_train, y_train)
test_data <- cbind(X_test, y_test)

data <- rbind(train_data, test_data)

################################################################################

# Step 2.
# Extracts only the measurements on the mean and standard deviation for each measurement.

feature_names <- read.table("UCI HAR Dataset/features.txt", header=FALSE)

names(data) <- append(as.character(feature_names$V2), "Label")

# Filter out columns that Not Measuring Mean or Standard Deviation.
valid_feature_names <- names(data)[grep("(.*-mean.*)|(.*-std.*)|Label", names(data))]

data_trimmed <- data[, valid_feature_names]

################################################################################

# Step 3.
# Uses descriptive activity names to name the activities in the data set.

# Replace all the string "Mag" to Magnitude".
names(data_trimmed) <- gsub("Mag", "Magnitude", names(data_trimmed))

# Replace all the string "Gyro" to "AngularVelocity",
# as suggested by the following link  under section "Attribute Information":
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
names(data_trimmed) <- gsub("Gyro", "AngularVelocity", names(data_trimmed))

# Replace all the string "Acc" to "Acceleration".
names(data_trimmed) <- gsub("Acc", "Acceleration", names(data_trimmed))

# Replace all the string "fBody" to "FrequencyBody".
names(data_trimmed) <- gsub("fBody", "FrequencyBody", names(data_trimmed))

# Replace all the string "tGravity" to "TimeGravity".
names(data_trimmed) <- gsub("tGravity", "TimeGravity", names(data_trimmed))

# Replace all the string "tBody" to "TimeBody".
names(data_trimmed) <- gsub("tBody", "TimeBody",names(data_trimmed))




################################################################################

# Step 4.
# Appropriately labels the data set with descriptive variable names.

activity_labels_data <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

activity_labels_list <- activity_labels_data$V2

names(activity_labels_list) <- activity_labels_data$V1

data_trimmed$Label <- sapply(data_trimmed$Label, function(x) activity_labels_list[x])

# Replace all the string "std" to "StandardDeviation".
names(data_trimmed) <- gsub("std", "StandardDeviation", names(data_trimmed))

# Replace all the string "meanFreq" to "WeightedAverage".
names(data_trimmed) <- gsub("meanFreq", "WeightedAverage", names(data_trimmed))

# Replace all the string "mean" to "Mean".
names(data_trimmed) <- gsub("mean", "Mean", names(data_trimmed))

# Replace all the string "()" to "".
names(data_trimmed) <- gsub("\\(\\)", "", names(data_trimmed))

# Replace all the string "BodyBody" to "Body".
names(data_trimmed) <- gsub("BodyBody", "Body",names(data_trimmed))

# Replace all the string "-" to "".
names(data_trimmed) <- gsub("-", "",names(data_trimmed))

################################################################################

# Step 5.
# From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

tidy_data <- colMeans(sapply(data_trimmed[,names(data_trimmed) != 'Label'], as.numeric), na.rm = TRUE)

names(tidy_data) <- sapply(names(tidy_data), function(x) paste0("AverageOf", x))

