################# the 0th step: setting the directory path & checking the files ##################################

getwd()
#I check the directory
hol<-getwd()
hol
#save the directory path
list.files()
#the next directory name is too long and I'm lazy to type, so I look it in the list of files (and directories)
lista<-list.files()
lista[119]
hol2<-paste(hol,lista[119],sep="/")
#and so I glue it to the former pathway
setwd(hol2)
getwd()
#OK, it was kinda unnecessary, but I figured it out by myself so I was proud and try to show off a bit
list.files()
#the next directory name is fortunately short
setwd("./UCI HAR Dataset")
list.files()
list.files()[1]
list.files()[2]
read.table(list.files()[2])
read.table(list.files()[1])
head(read.table(list.files()[2]))
head(read.table(list.files()[1]))
#I have read the README and the feature_info, but I check the files in R, too
#we have to go deeper
setwd("./test")
getwd()
list.files
list.files()
head(read.table(list.files()[2]))
miez<-read.table(list.files()[2])
str(miez)
#so there are the subjects, of course not all of 30, just some of them
head(read.table(list.files()[3]))
#so there are the measurements with 561 variables (my computer had a good time to load it)
head(read.table(list.files()[4]))
#and there are the labels aka movement forms
nrow(miez)
miez<-read.table(list.files()[2])
nrow(miez)
miez<-read.table(list.files()[3])
nrow(miez)
miez<-read.table(list.files()[4])
nrow(miez)
#just check whether row numbers are identical or not (kinda important)
setwd("../")
read.table(list.files()[1])
#this contains the labels (which number which movement)
read.table(list.files()[2])
#and this contains the name of measurement with words
setwd("./train")
getwd()
list.files()
miez<-read.table(list.files()[2])
str(miez)
miez<-read.table(list.files()[3])
str(miez)
miez<-read.table(list.files()[4])
str(miez)
# check the same in train as in test
#everything is OK

################# the 1st step: merge the training and test data set to one ###############################

########### renaming and combining the columns in the test data set ############

setwd("../")
list.files()
act<-read.table(list.files()[1])
str(act)
#make a new variable from the label-explaining thingy that I can you later
feat<-read.table(list.files()[2])
#same with the features thingy
setwd("./test")
list.files()
testsub<-read.table(list.files()[2])
testset<-read.table(list.files()[3])
testlab<-read.table(list.files()[4])
#make a new variable from the test-files, too
str(feat)
head(feat)
tail(feat)
names(testset)
#just look at the names that I have to replace and the names with which can replace
class(names(testset))
class(feat$V2)
#uh-oh, it's a factor!
feat$V2<-as.character(feat$V2)
#but that can be helped
class(feat$V2)
names(testset)<-feat$V2
str(testset)
unique(names(testset))
length(unique(names(testset)))
(unique(feat$V2))
length(unique(feat$V2))
unique(names(testset))
#it looks like the number of names is less than the actual number of variables
#but I decided not to care 
str(act)
act2<-as.character(act$V2)
#another factor->character transformation
testlab2<-as.integer(testlab$V1)
#make an integer vector from the numbers those represent the labels
testlab3<-act2[testlab2]
#replacing a numbers with respective character labels with this trick (halleluja Coursera forums!) 
### 3rd step embedded ###
str(testlab3)
testlab4<-as.data.frame(testlab3)
#but I guess it has to be a data frame to use cbind
#or not...?
names(testlab4)<-"label"
#rename accordingly
testnew<-cbind(testsub,testset,testlab4)
str(testnew)
#I notice I forgot to rename testsub, but it's ok
names(testnew)[1]<-"subject"
names(testnew)[1]
#I did it now

############# doing the same with the other (train) files THEN combine test & training ############

setwd("../")
setwd("./train")
#guess what? I simply copy-pasted the former part in Notepad++, replaced "tets" with "train", and...
trainsub<-read.table(list.files()[2])
trainset<-read.table(list.files()[3])
trainlab<-read.table(list.files()[4])
str(feat)
head(feat)
tail(feat)
names(trainset)
class(names(trainset))
class(feat$V2)
feat$V2<-as.character(feat$V2)
class(feat$V2)
names(trainset)<-feat$V2
str(trainset)
unique(names(trainset))
length(unique(names(trainset)))
(unique(feat$V2))
length(unique(feat$V2))
unique(names(trainset))
str(act)
act2<-as.character(act$V2)
trainlab2<-as.integer(trainlab$V1)
trainlab3<-act2[trainlab2]
### 3rd step embedded ###
str(trainlab3)
trainlab4<-as.data.frame(trainlab3)
names(trainlab4)<-"label"
trainnew<-cbind(trainsub,trainset,trainlab4)
str(trainnew)
names(trainnew)[1]<-"subject"
names(trainnew)[1]
#...it WORKED!!!
new<-rbind(testnew,trainnew)
str(new)
# it worked too!! happy so far.

################# the 2nd step: extracting measurements of mean and std values #######################################

allnames<-names(new)
#I collect the names of the new database in a character vector (it'll have the same length as the number of columns)
grep("-mean()",allnames,fixed=TRUE)
#I search for the number where the measures with means stay
class(grep("-mean()",allnames,fixed=TRUE))
#it is integer, goodie 
allmeans<-grep("-mean()",allnames,fixed=TRUE)
dfmeans<-new[allmeans]
#this way I can subset the columns with the means
str(dfmeans)
#it's 33 columns as has to be!!
allstds<-grep("-std()",allnames,fixed=TRUE)
dfstds<-new[allstds]
str(dfstds)
#do the same with standard deviations
new2<-cbind(as.data.frame(new[1]),as.data.frame(new[563]),dfstds,dfmeans)
#I cbind them while I not forget to add the subject and label columns (and data-framing them)
str(new2)
#SUCCESS

###################### the 3rd step: appropiate activity renaming ################################################

#I realize I already did that in th 1st step. Dear Marker, pls search for ### 3rd step embedded ### to check that and sorry for messing with you.

############################ the 4th step: appropriate and descriptive renaming of variables ########################

#what have I done to deserve this...
#it's OK, I got it. We have to replace abbrevations with whole names so everybody could get it.
renaming<-names(new2)
#I make a character vector from the names again
renaming<-gsub("Gyro","Gyroscope",renaming)
renaming<-gsub("Acc","Acceleration",renaming)
renaming<-gsub("Mag","Magnitude",renaming)
renaming<-gsub("tBody","timeBody",renaming)
renaming<-gsub("fBody","freqBody",renaming)
renaming<-gsub("tGravity","timeGravity",renaming)
#I replace all the snippets in the variable names with whole names
renaming[2]<-"activity"
#I realize "label" is also not descriptive enough so I rename it to "activity"
names(new2)<-renaming
#and finally I replace them in the data set

############################ the 5th step: calculating the averages according to the guideline and making a whole new, tidy data set ########

new2$subject<-as.factor(new2$subject)
#first I changed the subject to factor just in case. Because they are nut numbers, you know
unique(new2$activity)
walking<-subset(new2, activity=="WALKING")
#then I subset the whole database to activities THEN I calculate the averages according to subjects, THEN I'll re-merge them
#I know it's not very elegant, but I have no time to do better.
library(data.table)
walking.dt <- data.table(walking)
walking2<-walking.dt[,lapply(.SD,mean),by="subject",.SDcols=2:68]
#I get what I wanted (albeit in data table)
walking2$activity[walking2$activity == "4"] <- "WALKING"
#I rename the activity
sitting<-subset(new2, activity=="SITTING")
library(data.table)
sitting.dt <- data.table(sitting)
sitting2<-sitting.dt[,lapply(.SD,mean),by="subject",.SDcols=2:68]
sitting2$activity[sitting2$activity == "2"] <- "SITTING"
#now I "only" have to do the same with everything else
#it seems to work
laying<-subset(new2, activity=="LAYING")
library(data.table)
laying.dt <- data.table(laying)
laying2<-laying.dt[,lapply(.SD,mean),by="subject",.SDcols=2:68]
laying2$activity[laying2$activity == "1"] <- "LAYING"
standing<-subset(new2, activity=="STANDING")
library(data.table)
standing.dt <- data.table(standing)
standing2<-standing.dt[,lapply(.SD,mean),by="subject",.SDcols=2:68]
standing2$activity[standing2$activity == "3"] <- "STANDING"
walkingdown<-subset(new2, activity=="WALKING_DOWNSTAIRS")
library(data.table)
walkingdown.dt <- data.table(walkingdown)
walkingdown2<-walkingdown.dt[,lapply(.SD,mean),by="subject",.SDcols=2:68]
walkingdown2$activity[walkingdown2$activity == "5"] <- "WALKING_DOWNSTAIRS"
walkingup<-subset(new2, activity=="WALKING_UPSTAIRS")
library(data.table)
walkingup.dt <- data.table(walkingup)
walkingup2<-walkingup.dt[,lapply(.SD,mean),by="subject",.SDcols=2:68]
walkingup2$activity[walkingup2$activity == "6"] <- "WALKING_UPSTAIRS"
#everything seems to be OK
#and now I will bind them
hasitworked<-rbind(standing2,sitting2,laying2,walking2,walkingdown2,walkingup2)
str(hasitworked)
#proudly announce: yes, it has
write.table(hasitworked, file="tidy_data_set.txt", sep=";", quote=F, row.names=F)
#and the file also created. I'm done. (Well, not really, because GitHub and Codebook, but still...)


























