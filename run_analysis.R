#download packages
require(reshape2)
require(data.table)
require(dplyr)
require(knitr)
require(markdown)
#setting path
path <- getwd()
path

#download and unzip  dataset


path="C:\\Users\\rpande35\\Downloads"

pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive = TRUE)

#read all the files

dfSubTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
dfSubTest <- fread(file.path(pathIn, "test", "subject_test.txt"))

dfActTrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
dfActTest <- fread(file.path(pathIn, "test", "Y_test.txt"))


dfTrain <- fread(file.path(pathIn, "train", "X_train.txt"))
dfTest <- fread(file.path(pathIn, "test", "X_test.txt"))

##concatnating  the files on rows
dfSub <- rbind(dfSubTrain, dfSubTest)
setnames(dfSub, "V1", "subject")
dfAct <- rbind(dfActTrain, dfActTest)
setnames(dfAct, "V1", "activityNum")
df<- rbind(dfTrain, dfTest)

#merging the columns
dfSub<- cbind(dfSub, dfAct)
df <- cbind(dfSub, df)

#renaming the key
setkey(df, subject, activityNum)

dfFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dfFeatures, names(dfFeatures), c("featureNum", "featureName"))

#extracting mean nd std deviation

dfFeatures <- dfFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]


dfFeatures$featureCode <- dfFeatures[, paste0("V", featureNum)]
head(dfFeatures)
dfFeatures$featureCode

select <- c(key(df), dfFeatures$featureCode)
df <- df[, select, with = FALSE]


#Use descriptive activity names
dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

df <- merge(df, dtActivityNames, by = "activityNum", all.x = TRUE)



setkey(df, subject, activityNum, activityName)
#melt in tall format
df <- data.table(melt(df, key(df), variable.name = "featureCode"))

#merge activity name
df <- merge(df, dfFeatures[, list(featureNum, featureCode, featureName)], by = "featureCode", 
            all.x = TRUE)

#converting into categorical data
df$activity <- factor(df$activityName)
df$feature <- factor(df$featureName)

##grep the features
grepthis <- function(regex) {
  grepl(regex, df$feature)}


n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
df$fDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
df$fInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
df$fAcc <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
df$fVar <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
df$fJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
df$fMag <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))

## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
df$fAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

  
  
  
setkey(df, subject, activity, fDomain, fAcc, fInstrument, 
       fJerk, fMag, fVar, fAxis)

Tidydf <- df[, list(count = .N, average = mean(value)), by = key(df)]
write.table(Tidydf,file="tidyDataframe.txt",row.names = FALSE)

#knitting codebook
#knit("makeCodebook.Rmd", output = "C:\\Users\\rpande35\\Desktop\\mycodebook.md", encoding = "ISO8859-1", quiet = TRUE)

#creating markdown
#markdownToHTML("mycodebook.md", "C:\\Users\\rpande35\\Desktop\\codebook.html")
