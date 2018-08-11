library(readr)
library(sqldf)

'%&%' <<- function(x, y)paste0(x,y)


run_analysis <<- function ()
{
  wd <<- getwd() %&% "\\LMsubmission"
  
  subject_train_data <<- read.table(wd %&% "\\subject_train.txt", quote="\"", comment.char = "")
  
  subject_test_data <<- read.table(wd %&% "\\subject_test.txt", quote="\"", comment.char = "")
  
  x_train_data <<- read.table(wd %&% "\\x_train.txt", quote="\"", comment.char = "")
  
  x_test_data <- read.table(wd %&% "\\x_test.txt", quote="\"", comment.char = "")
  
  y_train_data <- read.table(wd %&% "\\y_train.txt", quote="\"", comment.char = "")
  
  y_test_data <- read.table(wd %&% "\\y_test.txt", quote="\"", comment.char = "")
  
  activity_label_data <- read.table(wd %&% "\\activity_labels.txt", quote="\"", comment.char = "")

  df.activity.label.data.train <<- sqldf("select a.[V2], b.[V1] from [activity_label_data] a, [y_train_data] b  where a.[V1] = b.[V1]")
  
  df.named_x_train_data <<- cbind(x_train_data,df.activity.label.data.train$V2,subject_train_data$V1 )
  
  df.activity.label.data.test <<- sqldf("select a.[V2], b.[V1] from [activity_label_data] a, [y_test_data] b  where a.[V1] = b.[V1]")
  
  df.named_x_test_data <<- cbind(x_test_data,df.activity.label.data.test$V2, subject_test_data$V1 )
  
  colnames(df.named_x_test_data)[562] <- "Activity"
  colnames(df.named_x_train_data)[562] <- "Activity"
  
  colnames(df.named_x_test_data)[563] <- "Subject"
  colnames(df.named_x_train_data)[563] <- "Subject"
  
  
  df.named_x_train_test_data <<- rbind(df.named_x_train_data, df.named_x_test_data)
  
  df.features.list <<- read.table(wd %&% "\\features.txt", quote="\"", comment.char = "")

  for (i in seq_along(df.features.list$V1))
  {
    
    tempName <<- sqldf("select trim(a.V2) from [df.features.list] a where a.v1 = cast(" %&% as.character(i) %&% " as decimal)")
    
    colnames(df.named_x_train_test_data)[i] <<- gsub("-","",gsub("\\()","",tempName))
      
    }
  
  
  View(df.named_x_train_test_data[1:20])
  View(df.named_x_train_test_data[530:563])
  
  xmeancols <- df.named_x_train_test_data[grep("mean",names(df.named_x_train_test_data))]
  xstdcols <- df.named_x_train_test_data[grep("std",names(df.named_x_train_test_data))]
  xactivitycol <- df.named_x_train_test_data[grep("Activity",names(df.named_x_train_test_data))]
  xsubject <- df.named_x_train_test_data[grep("Subject",names(df.named_x_train_test_data))]
  
  df.tidy.averages <<- cbind(xmeancols,xstdcols,xactivitycol,xsubject)
  
  df.single.tidy <<- sqldf("select Subject, Activity, Avg(tBodyAccmeanX) as AvgtBodyAccmeanX,
                           Avg(tBodyAccmeanY) as AvgtBodyAccmeanY,
                           Avg(tBodyAccmeanZ) as AvgtBodyAccmeanZ,
                           Avg(tGravityAccmeanX) as AvgtGravityAccmeanX,
                           Avg(tGravityAccmeanY) as AvgtGravityAccmeanY,
                           Avg(tGravityAccmeanZ) as AvgtGravityAccmeanZ,
                           Avg(tBodyAccJerkmeanX) as AvgtBodyAccJerkmeanX,
                           Avg(tBodyAccJerkmeanY) as AvgtBodyAccJerkmeanY,
                           Avg(tBodyAccJerkmeanZ) as AvgtBodyAccJerkmeanZ,
                           Avg(tBodyGyromeanX) as AvgtBodyGyromeanX,
                           Avg(tBodyGyromeanY) as AvgtBodyGyromeanY,
                           Avg(tBodyGyromeanZ) as AvgtBodyGyromeanZ,
                           Avg(tBodyGyroJerkmeanX) as AvgtBodyGyroJerkmeanX,
                           Avg(tBodyGyroJerkmeanY) as AvgtBodyGyroJerkmeanY,
                           Avg(tBodyGyroJerkmeanZ) as AvgtBodyGyroJerkmeanZ,
                           Avg(tBodyAccMagmean) as AvgtBodyAccMagmean,
                           Avg(tGravityAccMagmean) as AvgtGravityAccMagmean,
                           Avg(tBodyAccJerkMagmean) as AvgtBodyAccJerkMagmean,
                           Avg(tBodyGyroMagmean) as AvgtBodyGyroMagmean,
                           Avg(tBodyGyroJerkMagmean) as AvgtBodyGyroJerkMagmean,
                           Avg(fBodyAccmeanX) as AvgfBodyAccmeanX,
                           Avg(fBodyAccmeanY) as AvgfBodyAccmeanY,
                           Avg(fBodyAccmeanZ) as AvgfBodyAccmeanZ,
                           Avg(fBodyAccmeanFreqX) as AvgfBodyAccmeanFreqX,
                           Avg(fBodyAccmeanFreqY) as AvgfBodyAccmeanFreqY,
                           Avg(fBodyAccmeanFreqZ) as AvgfBodyAccmeanFreqZ,
                           Avg(fBodyAccJerkmeanX) as AvgfBodyAccJerkmeanX,
                           Avg(fBodyAccJerkmeanY) as AvgfBodyAccJerkmeanY,
                           Avg(fBodyAccJerkmeanZ) as AvgfBodyAccJerkmeanZ,
                           Avg(fBodyAccJerkmeanFreqX) as AvgfBodyAccJerkmeanFreqX,
                           Avg(fBodyAccJerkmeanFreqY) as AvgfBodyAccJerkmeanFreqY,
                           Avg(fBodyAccJerkmeanFreqZ) as AvgfBodyAccJerkmeanFreqZ,
                           Avg(fBodyGyromeanX) as AvgfBodyGyromeanX,
                           Avg(fBodyGyromeanY) as AvgfBodyGyromeanY,
                           Avg(fBodyGyromeanZ) as AvgfBodyGyromeanZ,
                           Avg(fBodyGyromeanFreqX) as AvgfBodyGyromeanFreqX,
                           Avg(fBodyGyromeanFreqY) as AvgfBodyGyromeanFreqY,
                           Avg(fBodyGyromeanFreqZ) as AvgfBodyGyromeanFreqZ,
                           Avg(fBodyAccMagmean) as AvgfBodyAccMagmean,
                           Avg(fBodyAccMagmeanFreq) as AvgfBodyAccMagmeanFreq,
                           Avg(fBodyBodyAccJerkMagmean) as AvgfBodyBodyAccJerkMagmean,
                           Avg(fBodyBodyAccJerkMagmeanFreq) as AvgfBodyBodyAccJerkMagmeanFreq,
                           Avg(fBodyBodyGyroMagmean) as AvgfBodyBodyGyroMagmean,
                           Avg(fBodyBodyGyroMagmeanFreq) as AvgfBodyBodyGyroMagmeanFreq,
                           Avg(fBodyBodyGyroJerkMagmean) as AvgfBodyBodyGyroJerkMagmean,
                           Avg(fBodyBodyGyroJerkMagmeanFreq) as AvgfBodyBodyGyroJerkMagmeanFreq,
                           Avg(tBodyAccstdX) as AvgtBodyAccstdX,
                           Avg(tBodyAccstdY) as AvgtBodyAccstdY,
                           Avg(tBodyAccstdZ) as AvgtBodyAccstdZ,
                           Avg(tGravityAccstdX) as AvgtGravityAccstdX,
                           Avg(tGravityAccstdY) as AvgtGravityAccstdY,
                           Avg(tGravityAccstdZ) as AvgtGravityAccstdZ,
                           Avg(tBodyAccJerkstdX) as AvgtBodyAccJerkstdX,
                           Avg(tBodyAccJerkstdY) as AvgtBodyAccJerkstdY,
                           Avg(tBodyAccJerkstdZ) as AvgtBodyAccJerkstdZ,
                           Avg(tBodyGyrostdX) as AvgtBodyGyrostdX,
                           Avg(tBodyGyrostdY) as AvgtBodyGyrostdY,
                           Avg(tBodyGyrostdZ) as AvgtBodyGyrostdZ,
                           Avg(tBodyGyroJerkstdX) as AvgtBodyGyroJerkstdX,
                           Avg(tBodyGyroJerkstdY) as AvgtBodyGyroJerkstdY,
                           Avg(tBodyGyroJerkstdZ) as AvgtBodyGyroJerkstdZ,
                           Avg(tBodyAccMagstd) as AvgtBodyAccMagstd,
                           Avg(tGravityAccMagstd) as AvgtGravityAccMagstd,
                           Avg(tBodyAccJerkMagstd) as AvgtBodyAccJerkMagstd,
                           Avg(tBodyGyroMagstd) as AvgtBodyGyroMagstd,
                           Avg(tBodyGyroJerkMagstd) as AvgtBodyGyroJerkMagstd,
                           Avg(fBodyAccstdX) as AvgfBodyAccstdX,
                           Avg(fBodyAccstdY) as AvgfBodyAccstdY,
                           Avg(fBodyAccstdZ) as AvgfBodyAccstdZ,
                           Avg(fBodyAccJerkstdX) as AvgfBodyAccJerkstdX,
                           Avg(fBodyAccJerkstdY) as AvgfBodyAccJerkstdY,
                           Avg(fBodyAccJerkstdZ) as AvgfBodyAccJerkstdZ,
                           Avg(fBodyGyrostdX) as AvgfBodyGyrostdX,
                           Avg(fBodyGyrostdY) as AvgfBodyGyrostdY,
                           Avg(fBodyGyrostdZ) as AvgfBodyGyrostdZ,
                           Avg(fBodyAccMagstd) as AvgfBodyAccMagstd,
                           Avg(fBodyBodyAccJerkMagstd) as AvgfBodyBodyAccJerkMagstd,
                           Avg(fBodyBodyGyroMagstd) as AvgfBodyBodyGyroMagstd,
                           Avg(fBodyBodyGyroJerkMagstd) as AvgfBodyBodyGyroJerkMagstd
                           from [df.tidy.averages] group by activity, subject order by subject, Activity")  
  
  View(df.single.tidy)
  
  write.csv(df.single.tidy, wd %&% "\\single_tidy_dataset.csv")
  
  }