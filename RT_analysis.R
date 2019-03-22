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
setwd(dir = "C:/Users/u0125029/Documents/3. Onderzoek/1. Pre-test 2018-2019/7. Analyse/RT_task")

#read excel file audiometry_pre
RT_pre <- read_excel("RT_mean_pre_withmeanreversals.xlsx")
colnames(RT_pre) <- c("subject", "group", "testretest", "meantrials", "meanreversals")
RT_pre$subject <- sapply(strsplit(RT_pre$subject, split='_', fixed=TRUE), function(x) (x[1]))
  #remove the '_1 / _2" from the subject name
  #x[1] indicates that you keep the part before '_', if you replace this by 2 you keep the part after '_'
head(RT_pre)

#we still need to replace parameters/labels with actual stimuli (difference in RT)
RT_pre$meanreversals <- floor(RT_pre$meanreversals + 0.5)
  #replacing the values only works with round labels
  #r automatically rounds .5 down, working with floor(x + 0.5) rounds up form 0.5
label <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50)
stimulus <- c(699,648,600,555,514,476,441,408,378,350,324,300,278,257,238,221,204,189,175,162,150,139,129,119,110,102,95,88,81,75,70,64,60,55,51,47,44,41,38,35,32,30,28,26,24,22,20,19,17,16)
RT_pre$threshold <- mapvalues(RT_pre$meanreversals, from = label, to = stimulus)
  #replace the label with the actual stimulus (difference in ms)
  #the actual stimulus is written in a new columns called threshold

write_xlsx(RT_pre, "RT_pre_thresholds.xlsx")

#################################################
########## Descriptive statistics ###############
#################################################

#create table with only threshold values, column test and column retest per subject
RT_pre_melt <- melt(RT_pre, id.var=c("subject", "group", "testretest"), measure.var=c("threshold"))
RT_pre_thresholds <- cast(subject + group ~ testretest , data = RT_pre_melt)
colnames(RT_pre_thresholds) <- c("subject", "group", "test", "retest")
RT_pre_thresholds[is.na(RT_pre_thresholds)] <- 699
  #replace missing values by highest possible threshold (cfr. Vanvooren et al., 2017)
head(RT_pre_thresholds)  

#calculate mean thresholds for test & retest
mean(RT_pre_thresholds$test)
sd(RT_pre_thresholds$test)
mean(RT_pre_thresholds$retest, na.rm=TRUE)
sd(RT_pre_thresholds$retest, na.rm=TRUE)

####################################################################################################
# DYSCO C1                           #  Vanvooren et al. (2017) HR  #  Vanvooren et al. (2017) LR  #
# test: 304.5495 / 209.1045          #  238 / 63                    #  204 / 88                    #
# retest: 335.7802 / 211.5446        #                              #                              #
####################################################################################################

#boxplots
boxplot(RT_pre_thresholds$test, main="Test")
boxplot(RT_pre_thresholds$retest, main="Retest")
  #no outliers


#examine correlations between test and retest values
scatterplot(test ~ retest, data = RT_pre_thresholds)
plot(test~retest, data=RT_pre_thresholds)

cor.test(~ test + retest,data = RT_pre_thresholds, method="spearman")

psych::alpha(x=RT_pre_thresholds[,3:4])


####################################################
# DYSCO C1               # Vanvooren et al. (2017) #
# r = 0.5312115          # r = 0.68                #
# p = 6.052e-08          # p < 0.001               #       
# Cronbach alpha = 0.7   # Cronbach alpha = 0.85   #
####################################################


#############################################
##############  ENTIRE DATASET  #############
#############################################

# check ANOVA assumptions

RT_pre$fsubject <- paste("S",factor(RT_pre$subject), sep="")
RT_pre$fgroup <- factor(RT_pre$group, levels = c(1,2,3), labels = c("GG_EE", "GG_NE", "ActiveControl"))
RT_pre$ftestretest <- factor(RT_pre$testretest, levels = c(1,2), labels = c("test", "retest"))

# check normality of residuals
fitQQ <- lm(threshold ~ ftestretest+fgroup, data=RT_pre)
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

# Perform permutation test (works with violated assumptions)

#using coin library, this treats the groups as independent samples
independence_test(threshold ~ fgroup, data=RT_pre)
##############################################
# Asymptotic General Independence test       #
# data = threshold by group                  #
# p = 0.01441                                #
# -> significant difference between groups   #
##############################################

#using lmPerm library
Permutation <- aovp(threshold ~ fgroup+ftestretest, data=RT_pre, perm="Exact")
summary(Permutation)
##############################################
# fgroup -> p = 0.02856                      #
# ftestretest -> p = 0.41096                 #
# -> significant difference between groups   #
##############################################

###### Perform Kruskal-Wallis test on mean of test & retest so we don't have two-way design (two-way design doesn't work with Kruskal-Wallis)
RT_pre_thresholds$meanthreshold <- (RT_pre_thresholds$test + RT_pre_thresholds$retest)/2
RT_pre_thresholds$fgroup <- factor(RT_pre_thresholds$group, levels = c(1,2,3), labels = c("GG_EE", "GG_NE", "ActiveControl"))
kruskal.test(meanthreshold ~ fgroup, data =  RT_pre_thresholds)µ
#####################################################
# Kruskal-Wallis rank sum test                      #
# data = mean threshold by group                    #
# p = 0.0631                                        #
# -> almost significant difference between groups   #
#####################################################
# p = .06, while p for test = .13 and for retest .22
# it is a little weird that p for meanthreshold is smaller than for threshold in only test because the differences between
# the means are actually larger in only test, but a possible explanation is that the standard deviation in the meanthreshold case 
# is smaller than in only test case (184 vs. 210) 

######---------------------------------------------------------- PLOTS -----------------------------------------------------#####

# you can't melt already molten data (it seems), so make a new data frame which is exactly the same as RT_pre_thresholds but is not molten like RT_pre_thresholds is
RT_pre_prepareformeltthresholds <- as.data.frame(RT_pre_thresholds)
RT_pre_melt_meanthreshold <- melt(RT_pre_prepareformeltthresholds, id.var=c("fgroup"), measure.var=c("meanthreshold"))

data.plot.group.meanthreshold <- cast(RT_pre_melt_meanthreshold, fgroup ~ ., mean)
names(data.plot.group.meanthreshold)[2] <- "meanthreshold"
data.plot.group.meanthreshold
print(xyplot(meanthreshold ~ fgroup, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.group.meanthreshold, ylim = c(200,400), main = "Main effect group mean threshold", xlab = "group", ylab = "mean threshold"))





RT_pre_meltforanova <- melt(RT_pre, id.var=c("fsubject", "fgroup", "ftestretest"), measure.var=c("threshold"))
RT_pre_wideforanova <- cast(fsubject + fgroup ~ ftestretest , data=RT_pre_meltforanova, mean)
head(RT_pre_wideforanova)

###---- main effect group  ----###
data.plot.fgroup    <- cast(RT_pre_meltforanova, fgroup ~ ., mean)
names(data.plot.fgroup)[2] <- "threshold"
data.plot.fgroup
print(xyplot(threshold ~ fgroup, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.fgroup, ylim = c(200,400), main = "Main effect group", xlab = "Group", ylab = "Threshold"))

###----- main effect testretest -----###
data.plot.ftestretest    <- cast(RT_pre_meltforanova, ftestretest ~ ., mean)
names(data.plot.ftestretest)[2] <- "threshold"
data.plot.ftestretest
print(xyplot(threshold ~ ftestretest, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.ftestretest, ylim = c(200,400), main = "Main effect test-retest", xlab = "Test", ylab = "Threshold"))


###---- interaction effect group - testing  ----###
data.plot.fgroupftestretest<- cast(RT_pre_meltforanova, fgroup*ftestretest ~ ., mean)
names(data.plot.fgroupftestretest)[3] <- "threshold"
data.plot.fgroupftestretest
print(xyplot(threshold ~ ftestretest, groups = fgroup, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.fgroupftestretest, ylim = c(200,400), main = "Group x testretest", xlab = "testretest", ylab = "threshold"))


########################################
########### ONLY TEST DATASET ##########
########################################

# group differences only for test
# Kruskal-Wallis is a non-parametric alternative to one-way ANOVA test, which extends the two-samples Wilcoxon test in the situation
# where there are more than two groups - when ANOVA assumptions are not met

RT_pre_test <- subset(RT_pre, testretest == 1)

# check normality residuals 
fitQQ_test <- lm(threshold ~ fgroup, data=RT_pre_test)
qqPlot(fitQQ_test, main="QQ Plot")
library(MASS)
sresid_test <- studres(fitQQ_test) 
hist(sresid_test, freq=FALSE, main="Distribution of Studentized Residuals")
xfit_test<-seq(min(sresid_test),max(sresid_test),length=40) 
yfit_test<-dnorm(xfit_test) 
lines(xfit_test, yfit_test)
# Extract the residuals
fit_residuals_test <- residuals(object = fitQQ_test)
# Run Shapiro-Wilk test
shapiro.test(x = fit_residuals_test )
#####################################
# Shapiro-Wilk normality test       #
# p =  0.0002426                    #
# normality assumption violated     #
#####################################

kruskal.test(threshold ~ fgroup, data =  RT_pre_test)
#################################################
# Kruskal-Wallis rank sum test                  #
# data =  threshold by group                    #
# p = 0.1301                                    #
# -> no significant difference between groups   #
#################################################
pairwise.wilcox.test(RT_pre_test$threshold, RT_pre_test$group, p.adjust.method = "BH")

######---------------------------------------------------------- PLOTS -----------------------------------------------------#####

RT_pre_melt_test <- melt(RT_pre_test, id.var=c("fgroup"), measure.var=c("threshold"))

data.plot.group.test <- cast(RT_pre_melt_test, fgroup ~ ., mean)
names(data.plot.group.test)[2] <- "threshold"
data.plot.group.test
print(xyplot(threshold ~ fgroup, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.group.test, ylim = c(200,400), main = "Main effect group test", xlab = "group", ylab = "threshold"))

##########################################
########### ONLY RETEST DATASET ##########
##########################################

RT_pre_retest <- subset(RT_pre, testretest == 2)

# check normality residuals 
fitQQ_retest <- lm(threshold ~ fgroup, data=RT_pre_retest)
qqPlot(fitQQ_retest, main="QQ Plot")
library(MASS)
sresid_retest <- studres(fitQQ_retest) 
hist(sresid_retest, freq=FALSE, main="Distribution of Studentized Residuals")
xfit_retest<-seq(min(sresid_retest),max(sresid_retest),length=40) 
yfit_retest<-dnorm(xfit_retest) 
lines(xfit_retest, yfit_retest)
# Extract the residuals
fit_residuals_retest <- residuals(object = fitQQ_retest)
# Run Shapiro-Wilk test
shapiro.test(x = fit_residuals_retest )
#####################################
# Shapiro-Wilk normality test       #
# p =  0.0003424                    #
# normality assumption violated     #
#####################################

#no significant differences between groups
kruskal.test(threshold ~ fgroup, data =  RT_pre_retest)
#################################################
# Kruskal-Wallis rank sum test                  #
# data =  threshold by group                    #
# p = 0.2161                                    #
# -> no significant difference between groups   #
#################################################
pairwise.wilcox.test(RT_pre_retest$threshold, RT_pre_retest$group, p.adjust.method = "BH")

######---------------------------------------------------------- PLOTS -----------------------------------------------------#####

RT_pre_melt_retest <- melt(RT_pre_retest, id.var=c("fgroup"), measure.var=c("threshold"))

data.plot.group.retest <- cast(RT_pre_melt_retest, fgroup ~ ., mean)
names(data.plot.group.retest)[2] <- "threshold"
data.plot.group.retest
print(xyplot(threshold ~ fgroup, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.group.retest, ylim = c(200,400), main = "Main effect group test", xlab = "group", ylab = "threshold"))



##--------------------------------------------------------------------------------------------------------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------##
##--------------------------------------------------------------------------------------------------------------------------------##
#replace unreliable thresholds by highest possible threshold (699)
#i002_1 / i006_1 / i006_2 / i009_1 / i010_1 / i010_2 / i017_1 / i017_2 / i035_1 / i047_1 / i063_1 / i063_2 / i064_2 / i074_2 / 
#i077_1 / i077_2 / i081_1 / i089_2 / i098_2 / i111_2 / i116_1 / i121_1 / i130_1 / i130_2 / i133_1 / i133_2 / i136_1 / i137_1 /
#i137_2 / i139_1 / i139_2 / i141_2 / i147_2 / i153_1 / i153_2

RT_pre_reliable <- read_excel("RT_pre_thresholds_reliable.xlsx")

#################################################
########## Descriptive statistics ###############
#################################################

#create table with only threshold values, column test and column retest per subject
RT_pre_reliable_melt <- melt(RT_pre_reliable, id.var=c("subject", "group", "testretest"), measure.var=c("threshold"))
RT_pre_reliable_thresholds <- cast(subject + group ~ testretest , data = RT_pre_reliable_melt)
colnames(RT_pre_reliable_thresholds) <- c("subject", "group", "test", "retest")
RT_pre_reliable_thresholds[is.na(RT_pre_reliable_thresholds)] <- 699
#replace missing values by highest possible threshold (cfr. Vanvooren et al., 2017)
head(RT_pre_reliable_thresholds)

#calculate mean thresholds for test & retest
mean(RT_pre_reliable_thresholds$test)
sd(RT_pre_reliable_thresholds$test)
mean(RT_pre_reliable_thresholds$retest, na.rm=TRUE)
sd(RT_pre_reliable_thresholds$retest, na.rm=TRUE)

####################################################################################################
# DYSCO C1                           #  Vanvooren et al. (2017) HR  #  Vanvooren et al. (2017) LR  #
# test: 316.7473 / 229.0498          #  238 / 63                    #  204 / 88                    #
# retest: 349.0989 / 229.8257        #                              #                              #
####################################################################################################

#boxplots
boxplot(RT_pre_reliable_thresholds$test, main="Test reliable")
boxplot(RT_pre_reliable_thresholds$retest, main="Retest reliable")
#no outliers


#examine correlations between test and retest values
scatterplot(test ~ retest, data = RT_pre_reliable_thresholds)
plot(test~retest, data=RT_pre_reliable_thresholds)

cor.test(~ test + retest,data = RT_pre_reliable_thresholds, method="spearman")

psych::alpha(x=RT_pre_reliable_thresholds[,3:4])

####################################################
# DYSCO C1               # Vanvooren et al. (2017) #
# r = 0.5304717          # r = 0.68                #
# p = 6.247e-08          # p < 0.001               #       
# Cronbach alpha = 0.71  # Cronbach alpha = 0.85   #
####################################################

#############################################
##############  ENTIRE DATASET  #############
#############################################

# check ANOVA assumptions

RT_pre_reliable$fsubject <- paste("S",factor(RT_pre_reliable$subject), sep="")
RT_pre_reliable$fgroup <- factor(RT_pre_reliable$group, levels = c(1,2,3), labels = c("GG_EE", "GG_NE", "ActiveControl"))
RT_pre_reliable$ftestretest <- factor(RT_pre_reliable$testretest, levels = c(1,2), labels = c("test", "retest"))

# check normality of residuals
fitQQ_reliable <- lm(threshold ~ ftestretest+fgroup, data=RT_pre_reliable)
qqPlot(fitQQ_reliable, main="QQ Plot")
plot(fitQQ_reliable, 2)
library(MASS)
sresid_reliable <- studres(fitQQ_reliable) 
hist(sresid_reliable, freq=FALSE, main="Distribution of Studentized Residuals")
xfit_reliable<-seq(min(sresid_reliable),max(sresid_reliable),length=40) 
yfit_reliable<-dnorm(xfit_reliable) 
lines(xfit_reliable, yfit_reliable)
# Extract the residuals
fit_residuals_reliable <- residuals(object = fitQQ_reliable)
# Run Shapiro-Wilk test
shapiro.test(x = fit_residuals_reliable )
#####################################
# Shapiro-Wilk normality test       #
# p =  3.334e-07                    #
# normality assumption violated     #
#####################################

# Perform permutation test (works with violated assumptions)

#using coin library, this treats the groups as independent samples
independence_test(threshold ~ fgroup, data=RT_pre_reliable)
##############################################
# Asymptotic General Independence test       #
# data = threshold by group                  #
# p = 0.008526                               #
# -> significant difference between groups   #
##############################################

#using lmPerm library
Permutation_reliable <- aovp(threshold ~ fgroup+ftestretest, data=RT_pre_reliable, perm="Exact")
summary(Permutation_reliable)
##############################################
# fgroup -> p = 0.0012                       #
# ftestretest -> p = 0.2005                  #
# -> significant difference between groups   #
##############################################

###### Perform Kruskal-Wallis test on mean of test & retest so we don't have two-way design (two-way design doesn't work with Kruskal-Wallis)
RT_pre_reliable_thresholds$meanthreshold <- (RT_pre_reliable_thresholds$test + RT_pre_reliable_thresholds$retest)/2
RT_pre_reliable_thresholds$fgroup <- factor(RT_pre_reliable_thresholds$group, levels = c(1,2,3), labels = c("GG_EE", "GG_NE", "ActiveControl"))
kruskal.test(meanthreshold ~ fgroup, data =  RT_pre_reliable_thresholds)
####################################################♣#
# Kruskal-Wallis rank sum test                      #
# data =  mean threshold by group                   #
# p = 0.05134                                       #
# -> almost significant difference between groups   #
#####################################################

######---------------------------------------------------------- PLOTS -----------------------------------------------------#####

# you can't melt already molten data (it seems), so make a new data frame which is exactly the same as RT_pre_thresholds but is not molten like RT_pre_thresholds is
RT_pre_prepareformeltthresholds_reliable <- as.data.frame(RT_pre_reliable_thresholds)
RT_pre_melt_meanthreshold_reliable <- melt(RT_pre_prepareformeltthresholds_reliable, id.var=c("fgroup"), measure.var=c("meanthreshold"))

data.plot.group.meanthreshold.reliable <- cast(RT_pre_melt_meanthreshold_reliable, fgroup ~ ., mean)
names(data.plot.group.meanthreshold.reliable)[2] <- "meanthreshold"
data.plot.group.meanthreshold.reliable
print(xyplot(meanthreshold ~ fgroup, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.group.meanthreshold.reliable, ylim = c(200,400), main = "Main effect group mean threshold", xlab = "group", ylab = "mean threshold"))





RT_pre_meltforanova_reliable <- melt(RT_pre_reliable, id.var=c("fsubject", "fgroup", "ftestretest"), measure.var=c("threshold"))
RT_pre_wideforanova_reliable <- cast(fsubject + fgroup ~ ftestretest , data=RT_pre_meltforanova_reliable, mean)
head(RT_pre_wideforanova_reliable)

###---- main effect group  ----###
data.plot.fgroup.reliable    <- cast(RT_pre_meltforanova_reliable, fgroup ~ ., mean)
names(data.plot.fgroup.reliable)[2] <- "threshold"
data.plot.fgroup.reliable
print(xyplot(threshold ~ fgroup, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.fgroup.reliable, ylim = c(200,400), main = "Main effect group", xlab = "Group", ylab = "Threshold"))

###----- main effect testretest -----###
data.plot.ftestretest.reliable    <- cast(RT_pre_meltforanova_reliable, ftestretest ~ ., mean)
names(data.plot.ftestretest.reliable)[2] <- "threshold"
data.plot.ftestretest.reliable
print(xyplot(threshold ~ ftestretest, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.ftestretest.reliable, ylim = c(200,400), main = "Main effect test-retest", xlab = "Test", ylab = "Threshold"))


###---- interaction effect group - testing  ----###
data.plot.fgroupftestretest.reliable <- cast(RT_pre_meltforanova_reliable, fgroup*ftestretest ~ ., mean)
names(data.plot.fgroupftestretest.reliable)[3] <- "threshold"
data.plot.fgroupftestretest.reliable
print(xyplot(threshold ~ ftestretest, groups = fgroup, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.fgroupftestretest.reliable, ylim = c(200,400), main = "Group x testretest", xlab = "testretest", ylab = "threshold"))


########################################
########### ONLY TEST DATASET ##########
########################################

# group differences only for test

RT_pre_test_reliable <- subset(RT_pre_reliable, testretest == 1)

# check normality residuals 
fitQQ_test_reliable <- lm(threshold ~ fgroup, data=RT_pre_test_reliable)
qqPlot(fitQQ_test_reliable, main="QQ Plot")
library(MASS)
sresid_test_reliable <- studres(fitQQ_test_reliable) 
hist(sresid_test_reliable, freq=FALSE, main="Distribution of Studentized Residuals")
xfit_test_reliable<-seq(min(sresid_test_reliable),max(sresid_test_reliable),length=40) 
yfit_test_reliable<-dnorm(xfit_test_reliable) 
lines(xfit_test_reliable, yfit_test_reliable)
# Extract the residuals
fit_residuals_test_reliable <- residuals(object = fitQQ_test_reliable)
# Run Shapiro-Wilk test
shapiro.test(x = fit_residuals_test_reliable )
#####################################
# Shapiro-Wilk normality test       #
# p =  3.456e-05                    #
# normality assumption violated     #
#####################################

kruskal.test(threshold ~ fgroup, data =  RT_pre_test_reliable)
#################################################
# Kruskal-Wallis rank sum test                  #
# data = threshold by group                     #
# p = 0.1041                                    #
# -> no significant difference between groups   #
#################################################


######---------------------------------------------------------- PLOTS -----------------------------------------------------#####

RT_pre_melt_test_reliable <- melt(RT_pre_test_reliable, id.var=c("fgroup"), measure.var=c("threshold"))

data.plot.group.test.reliable <- cast(RT_pre_melt_test_reliable, fgroup ~ ., mean)
names(data.plot.group.test.reliable)[2] <- "threshold"
data.plot.group.test.reliable
print(xyplot(threshold ~ fgroup, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.group.test.reliable, ylim = c(200,400), main = "Main effect group test", xlab = "group", ylab = "threshold"))

##########################################
########### ONLY RETEST DATASET ##########
##########################################

RT_pre_retest_reliable <- subset(RT_pre_reliable, testretest == 2)

# check normality residuals 
fitQQ_retest_reliable <- lm(threshold ~ fgroup, data=RT_pre_retest_reliable)
qqPlot(fitQQ_retest_reliable, main="QQ Plot")
library(MASS)
sresid_retest_reliable <- studres(fitQQ_retest_reliable) 
hist(sresid_retest_reliable, freq=FALSE, main="Distribution of Studentized Residuals")
xfit_retest_reliable<-seq(min(sresid_retest_reliable),max(sresid_retest_reliable),length=40) 
yfit_retest_reliable<-dnorm(xfit_retest_reliable) 
lines(xfit_retest_reliable, yfit_retest_reliable)
# Extract the residuals
fit_residuals_retest_reliable <- residuals(object = fitQQ_retest_reliable)
# Run Shapiro-Wilk test
shapiro.test(x = fit_residuals_retest_reliable )
#####################################
# Shapiro-Wilk normality test       #
# p =  0.0002182                    #
# normality assumption violated     #
#####################################

#no significant differences between groups
kruskal.test(threshold ~ fgroup, data =  RT_pre_retest_reliable)
#################################################
# Kruskal-Wallis rank sum test                  #
# data = threshold by group                     #
# p = 0.1936                                    #
# -> no significant difference between groups   #
#################################################
pairwise.wilcox.test(RT_pre_retest_reliable$threshold, RT_pre_retest_reliable$fgroup, p.adjust.method = "BH")

######---------------------------------------------------------- PLOTS -----------------------------------------------------#####

RT_pre_melt_retest_reliable <- melt(RT_pre_retest_reliable, id.var=c("fgroup"), measure.var=c("threshold"))

data.plot.group.retest.reliable <- cast(RT_pre_melt_retest_reliable, fgroup ~ ., mean)
names(data.plot.group.retest.reliable)[2] <- "threshold"
data.plot.group.retest.reliable
print(xyplot(threshold ~ fgroup, type=c("g", "b"), auto.key=list(colums=2, corner=c(0.1,0.9),lines=T), data=data.plot.group.retest.reliable, ylim = c(200,400), main = "Main effect group test", xlab = "group", ylab = "threshold"))





############################################################
## ANOVA code (can't be used - assumptions are violated   ##
############################################################

RT_pre$fsubject <- paste("S",factor(RT_pre$subject), sep="")
RT_pre$fgroup <- factor(RT_pre$group, levels = c(1,2,3), labels = c("GG_EE", "GG_NE", "ActiveControl"))
RT_pre$ftestretest <- factor(RT_pre$testretest, levels = c(1,2), labels = c("test", "retest"))

RT_pre_meltforanova <- melt(RT_pre, id.var=c("fsubject", "fgroup", "ftestretest"), measure.var=c("threshold"))
RT_pre_wideforanova <- cast(fsubject + fgroup ~ ftestretest , data=RT_pre_meltforanova, mean)
head(RT_pre_wideforanova)

testretest <- factor(rep(1:2, times=1))
idata <- data.frame(testretest)
idata

options(contrasts=c("contr.sum", "contr.poly"))
fit <- lm( cbind(test, retest) ~ fgroup, data=RT_pre_wideforanova)
out <- Anova(fit, type="III", test="Wilks", idata=idata, idesign=~testretest)
summary(out, multivariate=FALSE, univariate=TRUE)
