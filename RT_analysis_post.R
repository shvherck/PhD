#library for Anova
library(car)
#library to compute Cronbach's alpha
library(psych)
#load library to read excel files
library(readxl) 
library(writexl)
#load libraries to reshape (plyr for splitting etc.)
library(reshape)
library(plyr)
#load library to plot
library(lattice)
#load library for permutation tests
library(coin)
library(lmPerm)


#set working directory
setwd(dir = "C:/Users/u0125029/Documents/3. Onderzoek/3. Post-test 2019/5. Analyse/RT_task")

#read excel file RT_mean_post_withmeanreversals
RT_post <- read_excel("RT_mean_post_withmeanreversals.xlsx")
colnames(RT_post) <- c("subject", "group", "testretest", "meantrials", "meanreversals")
RT_post$subject <- sapply(strsplit(RT_post$subject, split='_', fixed=TRUE), function(x) (x[1]))
#remove the '_1 / _2" from the subject name
#x[1] indicates that you keep the part before '_', if you replace this by 2 you keep the part after '_'
head(RT_post)

#we still need to replace parameters/labels with actual stimuli (difference in RT)
RT_post$meanreversals <- floor(RT_post$meanreversals + 0.5)
#replacing the values only works with round labels
#r automatically rounds .5 down, working with floor(x + 0.5) rounds up form 0.5
label <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50)
stimulus <- c(699,648,600,555,514,476,441,408,378,350,324,300,278,257,238,221,204,189,175,162,150,139,129,119,110,102,95,88,81,75,70,64,60,55,51,47,44,41,38,35,32,30,28,26,24,22,20,19,17,16)
RT_post$threshold <- mapvalues(RT_post$meanreversals, from = label, to = stimulus)
#replace the label with the actual stimulus (difference in ms)
#the actual stimulus is written in a new columns called threshold

write_xlsx(RT_post, "RT_post_thresholds.xlsx")

#################################################
########## Descriptive statistics ###############
#################################################

RT_post <- read_excel("RT_post_thresholds.xlsx")
colnames(RT_post) <- c("subject", "group", "prepost", "testretest", "meantrials", "meanreversals", "threshold")

#create table with only threshold values, column pre and column post
RT_post_melt <- melt(RT_post, id.var=c("subject", "group", "prepost"), measure.var=c("threshold"))
RT_post_thresholds <- cast(subject + group ~ prepost , data = RT_post_melt, fun.aggregate=mean)
colnames(RT_post_thresholds) <- c("subject", "group", "pretest", "posttest")
#RT_pre_thresholds[is.na(RT_pre_thresholds)] <- 699 #dont do this yet, most missing values are because of missing post-test data
#replace missing values by highest possible threshold (cfr. Vanvooren et al., 2017)
head(RT_post_thresholds) 


#calculate mean thresholds for each group for pre-test and post-test
#GG_EE
RT_GGEE <- subset(RT_post_thresholds, group == 1)
mean(RT_GGEE$pretest, na.rm=TRUE) # 271.26
sd(RT_GGEE$pretest, na.rm=TRUE) # 179.99
mean(RT_GGEE$posttest, na.rm=TRUE) # 115.38
sd(RT_GGEE$posttest, na.rm=TRUE) # 88.64
#GG_NE
RT_GGNE <- subset(RT_post_thresholds, group == 2)
mean(RT_GGNE$pretest, na.rm=TRUE) # 191.77
sd(RT_GGNE$pretest, na.rm=TRUE) # 155.37
mean(RT_GGNE$posttest, na.rm=TRUE) # 137.79
sd(RT_GGNE$posttest, na.rm=TRUE) # 151.3
#Active_control
RT_AC <- subset(RT_post_thresholds, group == 3)
mean(RT_AC$pretest, na.rm=TRUE) # 276.48
sd(RT_AC$pretest, na.rm=TRUE) # 228.32
mean(RT_AC$posttest, na.rm=TRUE) # 202.4
sd(RT_AC$posttest, na.rm=TRUE) # 176.94
#Passive_control
RT_PC <- subset(RT_post_thresholds, group == 4)
mean(RT_PC$posttest, na.rm=TRUE) # 308.5
sd(RT_PC$posttest, na.rm=TRUE) # 271.78


######################
####### plots ########
######################

RT_post$fsubject <- factor(RT_post$subject)
RT_post$fgroup <- factor(RT_post$group, levels = c(1,2,3,4), labels = c("GG_EE", "GG_NE", "Active_Control", "Passive_Control"))
RT_post$fprepost <- factor(RT_post$prepost, levels = c(1,2), labels = c("pretest", "posttest"))

RT_post_melt <- melt(RT_post, id.var=c("fsubject", "fgroup", "fprepost"), measure.var=c("threshold"))
RT_post_wide <- cast(fsubject + fgroup ~ fprepost , data=RT_post_melt, mean)
head(RT_post_wide)

###---- interaction effect group - testing  ----###
data.plot.fgroupfprepost<- cast(RT_post_melt, fgroup*fprepost ~ ., mean)
names(data.plot.fgroupfprepost)[3] <- "threshold"
data.plot.fgroupfprepost
print(xyplot(threshold ~ fprepost, groups = fgroup, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.fgroupfprepost, ylim = c(100,350), main = "Intervention effect on rise time task", xlab = "Testing phase", ylab = "threshold"))

###---- boxplot ----###
print(bwplot(value~fgroup:fprepost, data=RT_post_melt))


