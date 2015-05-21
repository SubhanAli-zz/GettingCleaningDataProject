#1. Merges the training and the test sets to create one data set.

#Reads in the data from the individual files
xtest <- read.table("X_test.txt")
ytest <- read.table("y_test.txt")
xtrain <- read.table("X_train.txt")
ytrain <- read.table("y_train.txt")
subjecttest <- read.table("subject_test.txt")
subjecttrain <- read.table("subject_train.txt")

#Joins the x,y test sets together and the x,y train sets together
#Inserts a column into each one indicating test or training data and activities
test <- cbind(DataSet = "test", Subject = subjecttest, Activites = ytest, xtest)
train <- cbind(DataSet = "train", Subject = subjecttrain, Activites = ytrain, xtrain)

#Merges the test and training datasets into one master set
data <- rbind(test,train)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 

#Adds in names for all of the columns
names <- as.character(read.table("features.txt")[, 2])
names <- append(c("DataSet","Subject", "Activities"),names)
names(data) <- names

#Returns the columns that have mean and standard deviation key terms in them
data[,grep(".*mean.*|.*std.*", names(data))]

#3. Uses descriptive activity names to name the activities in the data set

#Substitutes the activities from their numbers to the name of the actual activity
data$Activities <- gsub(1,"Walking", data$Activities)
data$Activities <- gsub(2,"Walking Upstairs", data$Activities)
data$Activities <- gsub(3,"Walking Downstairs", data$Activities)
data$Activities <- gsub(4,"Sitting", data$Activities)
data$Activities <- gsub(5,"Standing", data$Activities)
data$Activities <- gsub(6,"Laying", data$Activities)

#4. Appropriately labels the data set with descriptive variable names. 

#Cleans up what is in each of the data column labels
names(data) <- gsub("^t", "Time", names(data))
names(data) <- gsub("^f", "Frequency", names(data))
names(data) <- gsub("-mean\\(\\)", "Mean", names(data))
names(data) <- gsub("-std\\(\\)", "StdDev", names(data))
names(data) <- gsub("-", "", names(data))
names(data) <- gsub("BodyBody", "Body", names(data))

#5. From the data set in step 4, creates a second, independent tidy data set 
#   with the average of each variable for each activity and each subject.

tidyset <- aggregate(data, by=list(Activities = data$Activities, Subject=data$Subject), mean)
tidyset[,3] = NULL
tidyset[,3] = NULL
tidyset[,3] = NULL
write.table(tidyset, "tidyset.txt", sep="\t")