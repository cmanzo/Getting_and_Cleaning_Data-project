# download and unzip file
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', destfile='data.zip', method='curl')
unzip('data.zip')

# load activities labels and features
label_activities <- read.table('UCI HAR Dataset/activity_labels.txt', stringsAsFactors=F,colClasses=c("integer", "character"))
label_features   <- read.table('UCI HAR Dataset/features.txt', stringsAsFactors=F,colClasses=c("integer", "character"))

# Load test data and label with descriptive variable names


import_signal <- function (signal_file, prefix_columns_name){
  col_names = lapply(1:128, function(i){paste(prefix_columns_name,i, sep='')})
  unlist(col_names)
  var <- read.table(signal_file, colClasses=c("numeric"), col.names=col_names)
}


activities <- read.table('UCI HAR Dataset/test/y_test.txt', colClasses=c("integer"),col.names='activity')
subjects <- read.table('UCI HAR Dataset/test/subject_test.txt', colClasses=c("integer"),col.names='subject')
cleaned_columns_names <-  gsub('[()]','', gsub('-','_',label_features$V2))
features <- read.table('UCI HAR Dataset/test/X_test.txt', colClasses=c("numeric"),col.names=cleaned_columns_names)

body_acc_x_test  <- import_signal('UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt', 'body_acc_x_t')
body_acc_y_test  <- import_signal('UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt', 'body_acc_y_t')
body_acc_z_test  <- import_signal('UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt', 'body_acc_z_t')
body_gyro_x_test <- import_signal('UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt', 'body_gyro_x_t')
body_gyro_y_test <- import_signal('UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt', 'body_gyro_y_t')
body_gyro_z_test <- import_signal('UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt', 'body_gyro_z_t')
total_acc_x_test <- import_signal('UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt', 'total_acc_x_t')
total_acc_y_test <- import_signal('UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt', 'total_acc_y_t')
total_acc_z_test <- import_signal('UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt', 'total_acc_z_t')

test_frame <- cbind(subjects, activities, body_acc_x_test, body_acc_y_test,
                    body_acc_z_test,
                    body_gyro_x_test,body_gyro_y_test,body_gyro_z_test,
                    total_acc_x_test,total_acc_y_test,total_acc_z_test,
                    features)


# Load train data and label with descriptive variable names
activities <- read.table('UCI HAR Dataset/train/y_train.txt', colClasses=c("integer"),col.names='activity')
subjects <- read.table('UCI HAR Dataset/train/subject_train.txt', colClasses=c("integer"),col.names='subject')
cleaned_columns_names <-  gsub('[()]','', gsub('-','_',label_features$V2))
features <- read.table('UCI HAR Dataset/train/X_train.txt', colClasses=c("numeric"),col.names=cleaned_columns_names)



body_acc_x_train  <- import_signal('UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt', 'body_acc_x_t')
body_acc_y_train  <- import_signal('UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt', 'body_acc_y_t')
body_acc_z_train  <- import_signal('UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt', 'body_acc_z_t')
body_gyro_x_train <- import_signal('UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt', 'body_gyro_x_t')
body_gyro_y_train <- import_signal('UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt', 'body_gyro_y_t')
body_gyro_z_train <- import_signal('UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt', 'body_gyro_z_t')
total_acc_x_train <- import_signal('UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt', 'total_acc_x_t')
total_acc_y_train <- import_signal('UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt', 'total_acc_y_t')
total_acc_z_train <- import_signal('UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt', 'total_acc_z_t')

train_frame <- cbind(subjects, activities, body_acc_x_train, body_acc_y_train,
                    body_acc_z_train,
                    body_gyro_x_train,body_gyro_y_train,body_gyro_z_train,
                    total_acc_x_train,total_acc_y_train,total_acc_z_train,
                    features)

# join and label data
dataset <- rbind(train_frame, test_frame)

dataset$activity <-  unlist(lapply(dataset$activity,function(i){label_activities[i,2]}))




library(dplyr)
# calculate means
dataset <- select(dataset, matches('act|sub|mean|std', ignore.case=F))
grouped_dataset <- group_by(dataset, subject, activity)
to_dump <- summarise_each(grouped_dataset, funs(mean(., na.rm=T)), matches("."))

# write final file
write.table(to_dump, file='means.csv', row.name=F, sep=',')

