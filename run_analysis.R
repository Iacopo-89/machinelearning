#Merging train and test sets

x.train <- read.table('C:/Users/User/Documents/UCI HAR Dataset/train/X_train.txt')
x.test <- read.table('C:/Users/User/Documents//UCI HAR Dataset/test/X_test.txt')
x <- rbind(x.train, x.test)

subj.train <- read.table('C:/Users/User/Documents//UCI HAR Dataset/train/subject_train.txt')
subj.test <- read.table('C:/Users/User/Documents//UCI HAR Dataset/test/subject_test.txt')
subj <- rbind(subj.train, subj.test)

y.train <- read.table('C:/Users/User/Documents/UCI HAR Dataset/train/y_train.txt')
y.test <- read.table('C:/Users/User/Documents/UCI HAR Dataset/test/y_test.txt')
y <- rbind(y.train, y.test)

# Grabbing only the required features with "mean" and "std"

features <- read.table('C:/Users/User/Documents/UCI HAR Dataset/features.txt')
mean.sd <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
x.mean.sd <- x[, mean.sd]

# Data cleaning, renaming variables and remove/replace some characters

names(x.mean.sd) <- features[mean.sd, 2]
names(x.mean.sd) <- tolower(names(x.mean.sd)) 
names(x.mean.sd) <- gsub("\\(|\\)", "", names(x.mean.sd))

activities <- read.table('C:/Users/User/Documents/UCI HAR Dataset/activity_labels.txt')
activities[, 2] <- tolower(as.character(activities[, 2]))
activities[, 2] <- gsub("_", "", activities[, 2])

y[, 1] = activities[y[, 1], 2]
colnames(y) <- 'activity'
colnames(subj) <- 'subject'

# Appropriately labels the data set with descriptive activity names.
data <- cbind(subj, x.mean.sd, y)
str(data)
write.table(data, 'C:/Users/User/Documents/merged.txt', row.names = F)

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
average.df <- aggregate(x=data, by=list(activities=data$activity, subj=data$subject), FUN=mean)
average.df <- average.df[, !(colnames(average.df) %in% c("subj", "activity"))]
str(average.df)
write.table(average.df, 'C:/Users/User/Documents/average.txt', row.names = F)

