run_analysis <- function() {
  ## this function is built to produce two data sets from the data set contained in the following link
  ## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
  ## This script will assume the following files are located in the root directory
  ## activity_labels.txt
  ## features.txt
  ## subject_test.txt
  ## X_test.txt
  ## y_test.txt
  ## subject_train.txt
  ## X_train.txt
  ## y_train.txt
  
  ## This script will use only the columns of dat that pertain to Mean and Standard Deviation
  ## data sets
  
  ## This script will output a file label fitbit_data.csv to the root directory and contain a
  ## clean labeled data subset
  
  ## This script will also output a second file from the same clean data summarizing
  ## data column averages for every subject and activity. This file will be labeled
  ## as fitbit_data_summary.csv
  
  library(stringr)
  library(sqldf)
  library(data.table)
  
  ## ------- Determine Columns that contain Mean and Standard Deviation info --------------
  
  ## load features.txt into a data frame called header this is the column labels of the data
  header <- read.delim("features.txt",header = FALSE)
    
  ## label the first column as colname
  names(header) <- "colname"
  
  ## a column labeled rowid is added to the header dataframe.
  ## This will keep track of the column number allow for later subsetting
  header$rowid <- str_extract(header$colname,"[[:digit:]]+")
  
  ## adds a column to the header data frame that determines which columns to keep
  ## columns that contain the phrases mean() or Std() are kept
  
    ## these two variables hold the target values to be searched for
  test1 <- "mean\\(\\)"  ## the ( and the ) characters have to be escaped
  test2 <- "std\\(\\)"
  
    ## this looks through the column names and determines which rows contrain the target phrases
    ## a new column is added to identify columns that will be kept
  header$keep <- ifelse(grepl(test1,header$colname,ignore.case = TRUE),"KEEP",
                         ifelse(grepl(test2,header$colname,ignore.case = TRUE),"KEEP",""))
    
      ## This step reduces the header file to only include column that contain the target phrases
  header <- subset(header, header$keep == "KEEP")
  
    ## this step creates a variable label keepcols which contains the column numbers to be download from the data file
  keepcols <- as.numeric(header[ ,'rowid'])
  
  
  ##--------- Import data from the X_test.txt file -----------------------------
  
    ## this step brings in the data from X_test.txt but only brings in the columns that have the target phrases
  testdata <- fread("X_test.txt", select = keepcols)
  
  ## this step will label the data columns in testdata a more approprate label that comes from the features.txt file
    ## this step will make a list variable that contains the column names for the data
  collabel <- header[ ,'colname']
  
    ## this step removes the leading numbers and white space from the column names
  collabel <- str_trim(gsub("[[:digit:]]+","",collabel), side = "both")

    ## This step removes the () from the column label names  
  collabel <- gsub("[//(//)]+","",collabel)
  
    ## this step changes the - to _ from the column label names
  collabel <- gsub("[//-]+","_",collabel)
  
    ## this step will apply the column labels to the test data
  names(testdata) <- collabel

    ## this step adds a sequential row number to the table. this will allow the subject and activity type to be 
    ## tied to the observation
  testdata$rowid <- 1:nrow(testdata)
  
 
  
  ##-------------- Import detail regarding subject description of the observations -------
  
  ## this next set of steps will import the subject_test wich will allow the subject number to be attaced to the
  ## data record in testdata
   
    ## this step imports the file from the root directory
  subjectid <- read.delim("subject_test.txt",header = FALSE)

    ## this step labels the data column as Subject_ID
  names(subjectid) <- "Subject_ID"
  
    ## this step adds a sequential row number to the table. this will allow the subject id to be matched to the 
    ## observations in testdata table
  subjectid$rowid <- 1:nrow(subjectid)
  
    ## brings the subject identified into the testdata table.
  testdata <- merge(testdata, subjectid, by="rowid")
  
  
  ## -------------------- import activity_labels.txt --------------------------
  
  ## this next section brings in the data from activity_labels.txt file.  This file will be used to provide
  ## better observation description for the data in the testdata table
  
    ## this step imports the file from the root directory
  actlabel <- read.delim("activity_labels.txt",header = FALSE)
    
    ## this step provides a colum label to the data column
  names(actlabel) <- "act"
  
    ## this step creates a new Column colled ID which contains only the activity description
  actlabel$id <- gsub("[[:digit:]]+","",actlabel$act)
  
    ## these next steps creates a new column called rownum which contains the activity number
  actlabel$activity_type <- gsub("[a-zA-Z]+","",actlabel$act) ## removes all the numeric charachters
  actlabel$activity_type <- gsub("_","",actlabel$activity_type) ## removes the under score (_)
  actlabel$activity_type <- as.numeric(str_trim(actlabel$activity_type,side = "both")) ## removes any white space
  
  
  ## the next section will provide visibility to the activity type of the observation
  
    ## first step is to import the y_test.txt file which contains what activity type produced the observation
  actobs <- read.delim("y_test.txt",header = FALSE)
  
    ## this step labels the data column and activity type
  
  names(actobs) <- "activity_type"
    ## this step adds a column called rowid which will act as a joiner back to testdata
  
  actobs$rowid <- 1:nrow(actobs)
    ## this step labels the observation activity ids with the understable decode
  
  actobs <- merge(actobs,actlabel, by="activity_type")
    ## this step removes the redundant columns and leaves only the 2 needed columns
  
  actobs <- actobs[ ,-c(1,3)]
    ## this step takes the observation activity description and attaches it to the actual observation
  
  testdata <- merge(testdata,actobs, by="rowid")
   
  ## this step brings in the data from X_test.txt but only bringsin the columns that have the target phrases
  traindata <- fread("X_train.txt", select = keepcols)
  
  ## this step will apply the column labels to the train data
  names(traindata) <- collabel
  
  ## this step adds a sequential row number to the table. this will allow the subject and activity type to be 
  ## tied to the observation
  traindata$rowid <- 1:nrow(traindata)
  
  
  ## this next set of steps will import the subject_test wich will allow the subject number to be attaced to the
  ## data record in testdata
  
  ## this step imports the file from the root directory
  trainsubjectid <- read.delim("subject_train.txt",header = FALSE)
  
  ## this step labels the data column as Subject_ID
  names(trainsubjectid) <- "Subject_ID"
  
  ## this step adds a sequential row number to the table. this will allow the subject id to be matched to the 
  ## observations in testdata table
  trainsubjectid$rowid <- 1:nrow(trainsubjectid)
  
  ## brings the subject identified into the testdata table.
  traindata <- merge(traindata, trainsubjectid, by="rowid")
  
  ## the next section will provide visibility to the activity type of the observation
  
  ## first step is to import the y_test.txt file which contains what activity type produced the observation
  trainactobs <- read.delim("y_train.txt",header = FALSE)
  ## this step labels the data column and activity type
  names(trainactobs) <- "activity_type"
  ## this step adds a column called rowid which will act as a joiner back to testdata
  trainactobs$rowid <- 1:nrow(trainactobs)
  ## this step labels the observation activity ids with the understable decode
  trainactobs <- merge(trainactobs,actlabel, by="activity_type")
  ## this step removes the redundant columns and leaves only the 2 needed columns
  trainactobs <- trainactobs[ ,-c(1,3)]
  ## this step takes the observation activity description and attaches it to the actual observation
  traindata <- merge(traindata,trainactobs, by="rowid")
  
  
  ## this step will combine both the test data and the train data into one single data set
  alldata <- rbind(traindata,testdata)
  
  ## this step will export a file named fitbit_data.csv to the root directory
  write.table(alldata,"fitbit_data.csv",sep=",")
  
  alldatasum <- sqldf("select
                subject_id
                ,id 
                ,avg(tBodyAcc_mean_X) AS tBodyAcc_mean_X
                ,avg(tBodyAcc_mean_Y) AS tBodyAcc_mean_Y
                ,avg(tBodyAcc_mean_z) AS tBodyAcc_mean_z 
                ,avg(tBodyAcc_std_X) AS tBodyAcc_std_X
                ,avg(tBodyAcc_std_Y) AS tBodyAcc_std_Y
                ,avg(tBodyAcc_std_Z) AS tBodyAcc_std_Z
                ,avg(tGravityAcc_mean_X) AS tGravityAcc_mean_X
                ,avg(tGravityAcc_mean_Y) AS tGravityAcc_mean_Y
                ,avg(tGravityAcc_mean_Z) AS tGravityAcc_mean_Z
                ,avg(tGravityAcc_std_X) AS tGravityAcc_std_X
                ,avg(tGravityAcc_std_Y) AS tGravityAcc_std_Y
                ,avg(tGravityAcc_std_Z) AS tGravityAcc_std_Z
                ,avg(tBodyAccJerk_mean_X) AS tBodyAccJerk_mean_X
                ,avg(tBodyAccJerk_mean_Y) AS tBodyAccJerk_mean_Y
                ,avg(tBodyAccJerk_mean_Z) AS tBodyAccJerk_mean_Z
                ,avg(tBodyAccJerk_std_X) AS tBodyAccJerk_std_X
                ,avg(tBodyAccJerk_std_Y) AS tBodyAccJerk_std_Y
                ,avg(tBodyAccJerk_std_Z) AS tBodyAccJerk_std_Z
                ,avg(tBodyGyro_mean_X) AS tBodyGyro_mean_X
                ,avg(tBodyGyro_mean_Y) AS tBodyGyro_mean_Y
                ,avg(tBodyGyro_mean_Z) AS tBodyGyro_mean_Z
                ,avg(tBodyGyro_std_X) AS tBodyGyro_std_X
                ,avg(tBodyGyro_std_Y) AS tBodyGyro_std_Y
                ,avg(tBodyGyro_std_Z) AS tBodyGyro_std_Z
                ,avg(tBodyGyroJerk_mean_X) AS tBodyGyroJerk_mean_X
                ,avg(tBodyGyroJerk_mean_Y) AS tBodyGyroJerk_mean_Y
                ,avg(tBodyGyroJerk_mean_Z) AS tBodyGyroJerk_mean_Z
                ,avg(tBodyGyroJerk_std_X) AS tBodyGyroJerk_std_X
                ,avg(tBodyGyroJerk_std_Y) AS tBodyGyroJerk_std_Y
                ,avg(tBodyGyroJerk_std_Z) AS tBodyGyroJerk_std_Z
                ,avg(tBodyAccMag_mean) AS tBodyAccMag_mean
                ,avg(tBodyAccMag_std) AS tBodyAccMag_std
                ,avg(tGravityAccMag_mean) AS tGravityAccMag_mean
                ,avg(tGravityAccMag_std) AS tGravityAccMag_std
                ,avg(tBodyAccJerkMag_mean) AS tBodyAccJerkMag_mean
                ,avg(tBodyAccJerkMag_std) AS tBodyAccJerkMag_std
                ,avg(tBodyGyroMag_mean) AS tBodyGyroMag_mean
                ,avg(tBodyGyroMag_std) AS tBodyGyroMag_std
                ,avg(tBodyGyroJerkMag_mean) AS tBodyGyroJerkMag_mean
                ,avg(tBodyGyroJerkMag_std) AS tBodyGyroJerkMag_std
                ,avg(fBodyAcc_mean_X) AS fBodyAcc_mean_X
                ,avg(fBodyAcc_mean_Y) AS fBodyAcc_mean_Y
                ,avg(fBodyAcc_mean_Z) AS fBodyAcc_mean_Z
                ,avg(fBodyAcc_std_X) AS fBodyAcc_std_X
                ,avg(fBodyAcc_std_Y) AS fBodyAcc_std_Y
                ,avg(fBodyAcc_std_Z) AS fBodyAcc_std_Z
                ,avg(fBodyAccJerk_mean_X) AS fBodyAccJerk_mean_X
                ,avg(fBodyAccJerk_mean_Y) AS fBodyAccJerk_mean_Y
                ,avg(fBodyAccJerk_mean_Z) AS fBodyAccJerk_mean_Z
                ,avg(fBodyAccJerk_std_X) AS fBodyAccJerk_std_X
                ,avg(fBodyAccJerk_std_Y) AS fBodyAccJerk_std_Y
                ,avg(fBodyAccJerk_std_Z) AS fBodyAccJerk_std_Z
                ,avg(fBodyGyro_mean_X) AS fBodyGyro_mean_X 
                ,avg(fBodyGyro_mean_Y) AS fBodyGyro_mean_Y
                ,avg(fBodyGyro_mean_Z) AS fBodyGyro_mean_Z
                ,avg(fBodyGyro_std_X) AS fBodyGyro_std_X
                ,avg(fBodyGyro_std_Y) AS fBodyGyro_std_Y
                ,avg(fBodyGyro_std_Z) AS fBodyGyro_std_Z
                ,avg(fBodyAccMag_mean) AS fBodyAccMag_mean
                ,avg(fBodyAccMag_std) AS fBodyAccMag_std
                ,avg(fBodyBodyAccJerkMag_mean) AS fBodyBodyAccJerkMag_mean
                ,avg(fBodyBodyAccJerkMag_std) AS fBodyBodyAccJerkMag_std
                ,avg(fBodyBodyGyroMag_mean) AS fBodyBodyGyroMag_mean
                ,avg(fBodyBodyGyroMag_std) AS fBodyBodyGyroMag_std
                ,avg(fBodyBodyGyroJerkMag_mean) AS fBodyBodyGyroJerkMag_mean
                ,avg(fBodyBodyGyroJerkMag_std) AS fBodyBodyGyroJerkMag_std
              from alldata
              group by subject_id,id")
  
  write.table(alldatasum,"fitbit_data_summary.csv",sep=",")
  

  
  }