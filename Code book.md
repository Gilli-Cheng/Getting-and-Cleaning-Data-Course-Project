# Code book

*1. get the data*
download the dataset from the given url to my local, and unzip it.

*2. load the data*
`features <- features.txt` : 561 rows, 2 columns
        the 561 measurements from this experiment,will be use as column name later on.


`train_set <- test/X_train.txt` : 7352 rows, 561 columns
        train data
`train_label <- test/y_train.txt` : 7352 rows, 1 columns
        train data of activities’code labels
`subject_train <- test/subject_train.txt` : 7352 rows, 1 column
        21/30 participants being observed
        
`test_set <- test/X_test.txt` : 2947 rows, 561 columns
        test data
`test_label <- test/y_test.txt` : 2947 rows, 1 columns
        test data of activities’code labels
`subject_test <- test/subject_test.txt` : 2947 rows, 1 column
        9/30 participants being observed

`labels <- activity_labels.txt` : 6 rows, 2 columns
        a table of activities’code labels to the activities 

*3. put everything in order*
`X` : 10299 rows, 561 columns
        for the data  (train + test),by `rbind`
`y` : 10299 rows, 1 column 
        for the labels(train + test),by `rbind`
`subject` : 10299 rows, 1 column 
        for the subjects (train + test), by `rbind`
`merge` : 10299 rows, 563 columns 
        merged the 3 above it, by `cbind`

*4. dplyr to combine the label with actual activity name*
`merge` : 10299 rows, 88 column
        uses `select` to keep variable including subject, label, `contains` is used in                 this `select` argument to keep the columns with "mean" or "std". 
        `merge` the merge data with `labels` data to attain the activity name,
        `select` is used to drop the former activities’code labels column.

*5. set the data with descriptive variable names*
        All `Acc` in column’s name replaced by `Accelerometer`
        All `Gyro` in column’s name replaced by `Gyroscope`
        All `BodyBody` in column’s name replaced by `Body`
        All `Mag` in column’s name replaced by `Magnitude`
        All start with character `f` in column’s name replaced by `Frequency`
        All start with character `t` in column’s name replaced by `Time`

*6. the final clean up, and export data*
`Clean` : 180 rows, 88 columns
        `group_by` funciton is used on the data, by subject and activity.
        `fun(mean)` is used in `summarize_all`, to apply mean to all columns by               subject and activity.
uses `write.table` to export csv and txt file


