#STEP 1: READ DATA & RENAME VARIABLES
#1.1:
# - READ the feature names from features.txt
# - READ the activity labels from activity_labels.txt

features <- read.table ("C:\\UCIHAR\\features.txt")
activities <- read.table ("C:\\UCIHAR\\activity_labels.txt")

#1.2:
# - READ the train data from local files:
#   subject_train (the volunteer ID), X_train (observed measurements), y_train (the activity ID)
# - RENAME the variable names in train.measurement with more descriptive & appropriate labels listed in features.txt
#          and the auto variable names of subject and activity to more descriptive labels (subject_ID & activity)
# - put them in the TRAIN data frame

train.ID <- read.table ("C:\\UCIHAR\\train\\subject_train.txt")
train.measurement <- read.table ("C:\\UCIHAR\\train\\X_train.txt")
train.activity <- read.table("C:\\UCIHAR\\train\\y_train.txt")

names(train.measurement) <- features$V2
names(train.activity) <- "activity"
names(train.ID) <- "subject.ID"

train <- data.frame(train.ID, train.activity, train.measurement)

#1.3:
# - READ the test data from local files:
#   subject_test (the volunteer ID), X_test (observed measurements), y_test (the activity ID)
# - RENAME the variable names in test.measurement with more descriptive & appropriate labels listed in features.txt
#          and the auto variable names of subject and activity to more descriptive labels (subject.ID & activity)
# - put them in the TEST data frame

test.ID <- read.table ("C:\\UCIHAR\\test\\subject_test.txt")
test.measurement <- read.table ("C:\\UCIHAR\\test\\X_test.txt")
test.activity <- read.table("C:\\UCIHAR\\test\\y_test.txt")

names(test.measurement) <- features$V2
names(test.activity) <- "activity"
names(test.ID) <- "subject.ID"
test <- data.frame(test.ID, test.activity, test.measurement)

#STEP 2: MERGE train and test datasets

join.data <- rbind(train,test)

#STEP 3: EXTRACT mean() and std() measurements only and include subject.ID and activity

column.selection <- grep("*mean*|*std*|subject.ID|activity",names(join.data))
selected.data <- join.data[,column.selection]

#STEP 4: SUBTITUTE activity ID with the appropriate activity names as in activity_labels.txt

for (i in 1:length(activities$V1)) selected.data$activity <- gsub(i,activities$V2[i], selected.data$activity)

#STEP 5: 
# - GENERATE dataset with average of each variables for each activity for each subjects
# - RENAME the feature names by adding "Mean_" to indicate that it's the mean value of that feature measurments.
#   (to reflect the correct description of the columns)
# - WRITE data to a text file separated by "tab"

final.data <- selected.data %>% group_by(subject.ID,activity) %>% summarise_each(funs(mean))

for (i in 3: length(final.data)) names(final.data)[i] <- paste("Mean",names(final.data)[i],sep="_")

write.table(final.data, file = "./final_data.txt", sep = "\t")
