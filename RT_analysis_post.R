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
library(reshape2)
library(plyr)
library(dplyr)
library(tidyverse)
#load library to plot
library(lattice)
library(ggplot2)
library(gridExtra)
library(sm)
#load library for permutation tests
library(coin)
library(lmPerm)
library(ARTool)

# prepare data ------


setwd(dir = "C:/Users/u0125029/Documents/3. Onderzoek/3. Post-test 2019/5. Analyse/RT_task")

RT_post           = read_excel("RT_mean_post_withmeanreversals.xlsx")
colnames(RT_post) = c("subject", "group", "testretest", "meantrials", "meanreversals")
RT_post$subject   = sapply(strsplit(RT_post$subject, split='_', fixed=TRUE), function(x) (x[1]))               #remove the '_1 / _2" from the subject name
head(RT_post)                                                                                                   #x[1] indicates that you keep the part before '_', if you replace this by 2 you keep the part after '_'


# replace parameters/labels with thresholds
RT_post$meanreversals = floor(RT_post$meanreversals + 0.5)           #replacing the values only works with round labels
                                                                      #r automatically rounds .5 down, working with floor(x + 0.5) rounds up from 0.5
label             = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                       20,21,22,23,24,25,26,27,28,29,30,31,32,33, 34,35,
                       36,37,38,39,40,41,42,43,44,45,46,47,48,49,50)
stimulus          = c(699,648,600,555,514,476,441,408,378,350,324,300,
                       278,257,238,221,204,189,175,162,150,139,129,119,
                       110,102,95,88,81,75,70,64,60,55,51,47,44,41,38,
                       35,32,30,28,26,24,22,20,19,17,16)
RT_post$threshold = mapvalues(RT_post$meanreversals, from = label, to = stimulus)

# create prepost column
label           = c(11,12,21,22)
stimulus        = c(1,1,2,2)
RT_post$prepost = mapvalues(RT_post$testretest, from = label, to = stimulus)
RT_post         = RT_post[c("subject", "group", "prepost", "testretest", "meantrials", "meanreversals", "threshold")]

# write data
write_xlsx(RT_post, "RT_post_thresholds.xlsx")
RT_post_testretest = RT_post

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
descriptives            <- cast(RT_post_melt, group*prepost ~ ., c(median,IQR))
names(descriptives)[3]  <- "median"
names(descriptives)[4]  <- "IQR"
descriptives

  
# plots ------

# prepare entire dataset for plotting 
RT_post$fsubject      = factor(RT_post$subject)
RT_post$fgroup        = factor(RT_post$group,   levels = c(1,2,3,4), labels = c("GG_EE", "GG_NE", "Active_Control", "Passive_Control"))
RT_post$fprepost      = factor(RT_post$prepost, levels = c(1,2),     labels = c("pretest", "posttest"))

RT_post_melt          = melt(RT_post, id.var=c("fsubject", "fgroup", "fprepost"), measure.var=c("threshold"))
RT_post_wide          = cast(fsubject + fgroup ~ fprepost , data=RT_post_melt, mean)
head(RT_post_wide)

# prepare dataset without missing values for plotting (needed for aov)
RT_post_aov           = subset(RT_post_withoutPC, !subject%in%c('i011','i017','i061','i064','i080','i124','i133','i138','i177')) 

RT_post_aov$fsubject  = factor(RT_post_aov$subject)
RT_post_aov$fgroup    = factor(RT_post_aov$group,   levels = c(1,2,3), labels = c("GG_EE", "GG_NE", "Active_Control"))
RT_post_aov$fprepost  = factor(RT_post_aov$prepost, levels = c(1,2),     labels = c("pretest", "posttest"))

RT_post_melt_aov      = melt(RT_post_aov, id.var=c("fsubject", "fgroup", "fprepost"), measure.var=c("threshold"))
RT_post_wide_aov      = cast(fsubject + fgroup ~ fprepost , data=RT_post_melt_aov, mean)
head(RT_post_wide_aov)

# prepare testretest dataset for plotting
head(RT_post_testretest)
label           = c(11,12,21,22)
stimulus        = c(1,2,1,2)
RT_post_testretest$testretest    = mapvalues(RT_post_testretest$testretest, from = label, to = stimulus)

RT_post_testretest               = subset(RT_post_testretest, group < 4)
RT_post_testretest               = subset(RT_post_testretest, !subject%in%c('i011','i017','i061','i064','i080','i124','i133','i138','i177')) 

RT_post_testretest$fsubject      = factor(RT_post_testretest$subject)
RT_post_testretest$fgroup        = factor(RT_post_testretest$group,   levels = c(1,2,3,4), labels = c("GG_EE", "GG_NE", "Active_Control", "Passive_Control"))
RT_post_testretest$fprepost      = factor(RT_post_testretest$prepost, levels = c(1,2),     labels = c("pretest", "posttest"))
RT_post_testretest$ftestretest   = factor(RT_post_testretest$testretest, levels = c(1,2), labels = c("test", "retest"))

RT_post_melt_testretest          = melt(RT_post_testretest, id.var=c("fsubject", "fgroup", "fprepost", "ftestretest"), measure.var=c("threshold"))
RT_post_wide_testretest          = cast(fsubject + fgroup + fprepost ~ ftestretest , data=RT_post_melt_testretest, mean)
head(RT_post_wide_testretest)

pre               = subset(RT_post_testretest, fprepost == "pretest")
pre_melt          = melt(pre, id.var=c("fsubject", "fgroup", "ftestretest"), measure.var=c("threshold"))
pre_wide          = cast(fsubject + fgroup ~ ftestretest , data=pre_melt, mean)
head(pre_wide)

post              = subset(RT_post_testretest, fprepost == "posttest")
post_melt         = melt(post, id.var=c("fsubject", "fgroup", "ftestretest"), measure.var=c("threshold"))
post_wide         = cast(fsubject + fgroup ~ ftestretest , data=post_melt, mean)
head(post_wide)


# scatterplot
scatterplot       = ggplot(RT_post_wide_aov, aes(x=pretest, y=posttest, color=fgroup)) +
                         geom_point() + 
                     ggtitle(label="Pretest / posttest per kid") + labs(color="group")
print(scatterplot)

# boxplot
boxplot         = ggplot(RT_post_aov, aes(x=fgroup, y=threshold, fill=fprepost)) + 
                  geom_boxplot() 
dotplot         = boxplot + geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(0.75), dotsize=0.5, binwidth=15) +
                  labs(title="Intervention effect on rise time task",x="group", y = "threshold", fill="test phase")
dotplot

boxplot(RT_post_melt_aov$value)

# create functions and prepare data for ggplot
quantile25 = function(data){
  x = quantile(data, 0.25, na.rm=TRUE) 
  return(x)
}
quantile75 = function(data){
  x = quantile(data, 0.75, na.rm=TRUE) 
  return(x)
}

plot.median.iqr.aov            = cast(RT_post_melt_aov, fgroup*fprepost ~ ., c(median,quantile25,quantile75))
names(plot.median.iqr.aov)[3]  = "median"
plot.median.iqr.aov

# ggplot                                                              # you need data frame with median and quantiles for this
bar.median.iqr.aov  = ggplot(plot.median.iqr.aov, aes(x=fgroup, y=median, fill=fprepost)) + 
                        geom_bar(stat="identity", color="black", 
                           position=position_dodge()) +
                        geom_errorbar(aes(ymin=quantile25, ymax=quantile75), width=.2,
                           position=position_dodge(.9)) +
                      ggtitle(label = "Intervention effect on rise time task") + xlab(label="Group") + ylab(label="Median threshold") +
                      labs(fill = "Testing phase")                                     #change names axes and legend
print(bar.median.iqr.aov)

# spaghettiplot
spaghettiplot = ggplot(data = RT_post_aov, aes(x = fprepost, y = threshold, group = fsubject, colour = fgroup)) +
                 geom_line(size=1) 
spaghettiplot

RT_post_aov_1 = subset(RT_post_aov, fgroup=="GG_EE")
RT_post_aov_2 = subset(RT_post_aov, fgroup=="GG_NE")
RT_post_aov_3 = subset(RT_post_aov, fgroup=="Active_Control")

p1            = ggplot(data = RT_post_aov_1, aes(x = fprepost, y = threshold, group = fsubject, colour=fsubject)) +
                  geom_line(size=1) + labs(title="GG_EE", x="test phase") + theme(legend.position="none")
p2            = ggplot(data = RT_post_aov_2, aes(x = fprepost, y = threshold, group = fsubject, colour=fsubject)) +
                  geom_line(size=1) + labs(title="GG_NE", x="test phase") + theme(legend.position="none")
p3            = ggplot(data = RT_post_aov_3, aes(x = fprepost, y = threshold, group = fsubject, colour=fsubject)) +
                  geom_line(size=1) + labs(title="Active control", x="test phase") + theme(legend.position="none")

spaghettiplot_group = grid.arrange(p1, p2, p3, ncol = 2, top="Spaghetti plot per group")

# correlation plot test retest

plot(RT_post_wide_testretest$test, RT_post_wide_testretest$retest, xlab="Test", ylab="Retest", main="Test-retest reliability")
plot(pre_wide$test, pre_wide$retest, xlab="Test", ylab="Retest", main="Test-retest reliability pre-test")
plot(post_wide$test, post_wide$retest, xlab="Test", ylab="Retest", main="Test-retest reliability post-test")

# normality check ------

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
  # normality assumption violated

# check homogeneity of variances
leveneTest(threshold ~ fgroup*fprepost, data = RT_post)
  # homogeneity of variances assumption violated

# check equality of distributions for permutation testing
sm.density.compare(RT_post$threshold, RT_post$fgroup)
  # distributions appear to be quite equal


# analyses ------------

# order data for Kruskal.test (order by group)
RT_post_aov_ordered = RT_post_aov[order(RT_post_aov$fgroup),]
kruskal.test(threshold ~ fgroup, data =  RT_post_aov_ordered)                     # kruskal: when factor contains more than two levels
  # one-way analysis group: no sign difference between groups                       

wilcox.test(RT_post_wide_aov$pretest, RT_post_wide_aov$posttest, 
            alternative ="greater", paired = TRUE)                        # wilcoxon:  when comparing two conditions with paired observations
# mann-whitney: two conditions with independent observations
  # one-way analysis test phase: sign difference between pretest and posttest
  # Ha not possible in terms of mean, median, ... because of non-parametric test
  # Kids are more likely to have lower RT thresholds in the post-test compared
  # to the pre-test (course non-parametric testing p. 29)


# using lmPerm library

Permutation <- aovp(threshold ~ fgroup*fprepost + Error(fsubject/fprepost), data=RT_post_aov, perm="Exact")
summary(Permutation)

RT_post_aov_ph <- dcast(RT_post_aov, fsubject ~ fgroup + fprepost, value.var="threshold")
head(RT_post_aov_ph)

GGEEpre_vs_GGEEpost = wilcox.test(RT_post_aov_ph$GG_EE_pretest,                            # 0.0001410895  (holm & hochberg)
                                  RT_post_aov_ph$GG_EE_posttest,                           # 0.0001410895  (bonferroni)
                                  paired=TRUE, alternative="greater")$p.value                       
GGNEpre_vs_GGNEpost = wilcox.test(RT_post_aov_ph$GG_NE_pretest,                            # 0.0387299202  (holm & hochberg)                         
                                  RT_post_aov_ph$GG_NE_posttest,                           # 0.0580948803  (bonferroni)
                                  paired=TRUE, alternative="greater")$p.value              
ACpre_vs_ACpost     = wilcox.test(RT_post_aov_ph$Active_Control_pretest,                  # 0.2077287478   (holm & hochberg)
                                  RT_post_aov_ph$Active_Control_posttest,                 # 0.6231862435   (bonferroni)
                                  paired=TRUE, alternative="greater")$p.value              
  # important: cannot compute exact p-values because of ties (all cases) and zeroes (case 2 & 3)

p.adjust(c(GGEEpre_vs_GGEEpost, GGNEpre_vs_GGNEpost, ACpre_vs_ACpost), method="holm")

# test-retest reliability

cor.test(RT_post_wide_testretest$test, RT_post_wide_testretest$retest, method=c("pearson"))
cor.test(pre_wide$test, pre_wide$retest, method=c("pearson"))
cor.test(post_wide$test, post_wide$retest, method=c("pearson"))



# other options -----------

# PLOT INCLUDING ALL DATA #
# prepare data
plot.median.iqr            <- cast(RT_post_melt, fgroup*fprepost ~ ., c(median,quantile25,quantile75))
names(plot.median.iqr)[3]  <- "median"
plot.median.iqr
plot.median.iqr            <- head(plot.median.iqr, -1)               # remove PC (not informative yet)
plot.median.iqr

# ggplot                                                              # you need data frame with median and quantiles for this
bar.median.iqr  <- ggplot(plot.median.iqr, aes(x=fgroup, y=median, fill=fprepost)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=quantile25, ymax=quantile75), width=.2,
                position=position_dodge(.9)) +
  ggtitle(label = "Intervention effect on rise time task") + xlab(label="Group") + ylab(label="Median threshold") +
  labs(fill = "Testing phase")                                     #change names axes and legend
print(bar.median.iqr)


# we can do wilcox.test with wide data format (as done before), but also in long data format
RT_post_aov_ordered2 = RT_post_aov[order(RT_post_aov$fprepost, RT_post_aov$subject),]
wilcox.test(threshold~fprepost, data=RT_post_aov_ordered2,
            alternative ="greater", paired = TRUE) 
# for this option (long data) the data need to be ordered so that the first observation where fprepost = pretest is paired to the first
# observation where fprepost = posttest (this is the case in both RT_post_aov and RT_post_aov_ordered2)
# for wide format we don't need to order in a specific way because we have all thesholds on one line

# sukkelruimte ------

boxplot(value~fprepost:fgroup, data=RT_post_melt)$out         # get actual outlier values

# assign outlier values into a vector
outliers <- boxplot(value~fprepost:fgroup, data=RT_post_melt, plot=FALSE)$out
# check the results
print(outliers)
# First you need find in which rows the outliers are
RT_post[which(RT_post$threshold %in% outliers),]



# ARTool

# ART needs subject as first column and DV as last column with IV's in between
RT_post_ART <- RT_post[,c('subject','fgroup','fprepost','threshold')]
head(RT_post_ART)

m           <- art(threshold ~ fgroup * fprepost + (1|subject), data=RT_post_ART)
            # (1|subject):              anova run as lmer
            # Error(fsubject/fprepost): anova run as RM anova
summary(m)  # F values of ANOVAs on aligned responses not of interest are not all approx. 0: ART may not be appropriate
anova(m)
