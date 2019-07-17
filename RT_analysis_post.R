# --------------------------------------------------------- #
# name              RT_analysis_post.R                      #
# description       used to analyse data from RT task       #
# version.string    R version 3.5.1 (2018-07-02)            #
# platform          x86_64-w64-mingw32                      #
# date created      17/07/2019                              #
# --------------------------------------------------------- #

# load libraries -----------

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
library(dplyr)
#load library to plot
library(lattice)
library(ggplot2)
#load library for permutation tests
library(coin)
library(lmPerm)

# prepare data ------


setwd(dir = "C:/Users/u0125029/Documents/3. Onderzoek/3. Post-test 2019/5. Analyse/RT_task")

RT_post           <- read_excel("RT_mean_post_withmeanreversals.xlsx")
colnames(RT_post) <- c("subject", "group", "testretest", "meantrials", "meanreversals")
RT_post$subject   <- sapply(strsplit(RT_post$subject, split='_', fixed=TRUE), function(x) (x[1]))               #remove the '_1 / _2" from the subject name
head(RT_post)                                                                                                   #x[1] indicates that you keep the part before '_', if you replace this by 2 you keep the part after '_'


# replace parameters/labels with thresholds
RT_post$meanreversals <- floor(RT_post$meanreversals + 0.5)           #replacing the values only works with round labels
                                                                      #r automatically rounds .5 down, working with floor(x + 0.5) rounds up from 0.5
label             <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                       20,21,22,23,24,25,26,27,28,29,30,31,32,33, 34,35,
                       36,37,38,39,40,41,42,43,44,45,46,47,48,49,50)
stimulus          <- c(699,648,600,555,514,476,441,408,378,350,324,300,
                       278,257,238,221,204,189,175,162,150,139,129,119,
                       110,102,95,88,81,75,70,64,60,55,51,47,44,41,38,
                       35,32,30,28,26,24,22,20,19,17,16)
RT_post$threshold <- mapvalues(RT_post$meanreversals, from = label, to = stimulus)

# create prepost column
label           <- c(11,12,21,22)
stimulus        <- c(1,1,2,2)
RT_post$prepost <- mapvalues(RT_post$testretest, from = label, to = stimulus)
RT_post         <- RT_post[c("subject", "group", "prepost", "testretest", "meantrials", "meanreversals", "threshold")]

# write data
write_xlsx(RT_post, "RT_post_thresholds.xlsx")

# keep lowest threshold per subject + prepost combination
RT_post <- merge(RT_post, aggregate(threshold ~ subject + prepost, data = RT_post, min))
# remove duplicates 
RT_post <- distinct(RT_post, subject,prepost,threshold, .keep_all=TRUE)               # use diff <- setdiff(df1, df2) to compare the data frames
                                                                                      # and check removed rows

# write data
write_xlsx(RT_post, "RT_post_final.xlsx")

# create data frame without PC
RT_post_withoutPC <- subset(RT_post, group < 4)


# descriptives ------

# prepare data for descriptives
RT_post_melt                  <- melt(RT_post, id.var=c("subject", "group", "prepost"), measure.var=c("threshold"))
RT_post_thresholds            <- cast(subject + group ~ prepost , data = RT_post_melt, fun.aggregate=mean)
colnames(RT_post_thresholds)  <- c("subject", "group", "pretest", "posttest")
head(RT_post_thresholds) 

# median + iqr                                  # non-normal data so median + IQR instead of mean + sd
descriptives            <- cast(RT_post_melt, group*prepost ~ ., c(median,IQR,quantile(RT_)))
names(descriptives)[3]  <- "median"
names(descriptives)[4]  <- "IQR"
descriptives

  
# plots ------

# prepare data for plotting 
RT_post$fsubject  <- factor(RT_post$subject)
RT_post$fgroup    <- factor(RT_post$group,   levels = c(1,2,3,4), labels = c("GG_EE", "GG_NE", "Active_Control", "Passive_Control"))
RT_post$fprepost  <- factor(RT_post$prepost, levels = c(1,2),     labels = c("pretest", "posttest"))

RT_post_melt  <- melt(RT_post, id.var=c("fsubject", "fgroup", "fprepost"), measure.var=c("threshold"))
RT_post_wide  <- cast(fsubject + fgroup ~ fprepost , data=RT_post_melt, mean)
head(RT_post_wide)

# USING MEDIAN + IQR

# prepare data

# create functions to include in cast
quantile25 = function(data){
  x = quantile(data, 0.25, na.rm=TRUE) 
  return(x)
}
quantile75 = function(data){
  x = quantile(data, 0.75, na.rm=TRUE) 
  return(x)
}

data.plot.fgroupfprepost            <- cast(RT_post_melt, fgroup*fprepost ~ ., c(median,quantile25,quantile75))
names(data.plot.fgroupfprepost)[3]  <- "median"
data.plot.fgroupfprepost
data.plot.fgroupfprepost            <- head(data.plot.fgroupfprepost, -1)               # remove PC (not informative yet)
data.plot.fgroupfprepost

# ggplot                                  # you need data frame with mean and sd for this, me
bar  <- ggplot(data.plot.fgroupfprepost, aes(x=fgroup, y=median, fill=fprepost)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile75), width=.2,
                position=position_dodge(.9)) 
print(bar)

# USING MEAN + SD - not ideal for skewed data
# interaction effect group - testing

# prepare data
data.plot.fgroupfprepost            <- cast(RT_post_melt, fgroup*fprepost ~ ., c(mean,sd))
names(data.plot.fgroupfprepost)[3]  <- "threshold"
data.plot.fgroupfprepost
data.plot.fgroupfprepost            <- head(data.plot.fgroupfprepost, -1)               # remove PC (not informative yet)
data.plot.fgroupfprepost

# xyplot
print(xyplot(threshold ~ fprepost, groups = fgroup, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.fgroupfprepost, ylim = c(0,350), main = "Intervention effect on rise time task", xlab = "Testing phase", ylab = "threshold"))

# ggplot                                  # you need data frame with mean and sd for this, me
bar  <- ggplot(data.plot.fgroupfprepost, aes(x=fgroup, y=threshold, fill=fprepost)) + 
            geom_bar(stat="identity", color="black", 
                position=position_dodge()) +
            geom_errorbar(aes(ymin=threshold-sd, ymax=threshold+sd), width=.2,
                position=position_dodge(.9)) 
print(bar)


line  <- ggplot(data.plot.fgroupfprepost, aes(x=fprepost, y=threshold, group=fgroup, color=fgroup)) + 
              geom_line() +
              geom_point()+
              geom_errorbar(aes(ymin=threshold-sd, ymax=threshold+sd), width=.2,
                   position=position_dodge(0.05))
print(line)


# boxplot
boxplot(value~fprepost:fgroup, data=RT_post_melt)             # some outliers
boxplot(RT_post_melt$value)













# sukkelruimte ------

boxplot(value~fprepost:fgroup, data=RT_post_melt)$out         # get actual outlier values

# assign outlier values into a vector
outliers <- boxplot(value~fprepost:fgroup, data=RT_post_melt, plot=FALSE)$out
# check the results
print(outliers)
# First you need find in which rows the outliers are
RT_post[which(RT_post$threshold %in% outliers),]




subset(RT_post, group < 4)

for (group_number in 1:4){
  # sukkel sukkel
}















# check normality of residuals
fitQQ <- lm(threshold ~ fprepost+fgroup, data=RT_post)
qqPlot(fitQQ, main="QQ Plot")
plot(fitQQ, 2)
library(MASS)
sresid <- studres(fitQQ) 
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
# Extract the residuals
fit_residuals <- residuals(object = fitQQ)
# Run Shapiro-Wilk test
shapiro.test(x = fit_residuals )
#####################################
# Shapiro-Wilk normality test       #
# p =  1.739e-06                    #
# normality assumption violated     #
#####################################
