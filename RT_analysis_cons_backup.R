# --------------------------------------------------------- #
#'#  name              RT_analysis_post.R                   #
#'   description       used to analyse data from RT task    #
#'   version.string    R version 3.5.1 (2018-07-02)         #
#'   platform          x86_64-w64-mingw32                   #
#'   date created      06/08/2020                           #
# --------------------------------------------------------- #

# load libraries -----------

#'## Load libraries
#'load library for Anova
library(car)
#'load library to read excel files
library(readxl) 
library(writexl)
#'load libraries to reshape (plyr for splitting etc.)
library(reshape)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyverse)
#'load library to plot
library(lattice)
library(ggplot2)
library(gridExtra)
library(sm)
#'load library for mixed models
library(lme4)
library(emmeans)
library(lmmeans)
library(lmerTest)
#'load library for robust lmm
library(robustlmm)
library(pbkrtest)
#'load library for robust anova & correlations
library(WRS2)
#'correlation matrix
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r") # necessary to use rquery.cormat
library(PerformanceAnalytics)
library(corrr)


# prepare data ------

#'## Prepare data
setwd(dir = "C:/Users/u0125029/Documents/3. Onderzoek/4. Consolidatie-test 2020/8. Analyses/1. Rise time")

d           = read_excel("RT_data_cons.xlsx")
colnames(d) = c("subject", "group", "testretest", "meantrials", "meanreversals")
d$subject   = sapply(strsplit(d$subject, split='_', fixed=TRUE), function(x) (x[1]))               #remove the '_1 / _2" from the subject name
head(d)                                                                                                  #x[1] indicates that you keep the part before '_', if you replace this by 2 you keep the part after '_'


# replace parameters/labels with thresholds
d$meanreversals = floor(d$meanreversals + 0.5)           #replacing the values only works with round labels
                                                                     #r automatically rounds .5 down, working with floor(x + 0.5) rounds up from 0.5
label             = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                       20,21,22,23,24,25,26,27,28,29,30,31,32,33, 34,35,
                       36,37,38,39,40,41,42,43,44,45,46,47,48,49,50)
stimulus          = c(699,648,600,555,514,476,441,408,378,350,324,300,
                       278,257,238,221,204,189,175,162,150,139,129,119,
                       110,102,95,88,81,75,70,64,60,55,51,47,44,41,38,
                       35,32,30,28,26,24,22,20,19,17,16)
d$threshold = mapvalues(d$meanreversals, from = label, to = stimulus)

# create prepost column
label           = c(11,12,21,22,31,32)
stimulus        = c(1,1,2,2,3,3)
d$prepost = mapvalues(d$testretest, from = label, to = stimulus)
d         = d[c("subject", "group", "prepost", "testretest", "meantrials", "meanreversals", "threshold")]

# write data
write_xlsx(d, "RT_cons_thresholds.xlsx")
d_testretest = d

# keep lowest threshold per subject + prepost combination (1 value for pretest and posttest per subject)
d <- merge(d, aggregate(threshold ~ subject + prepost, data = d, min))
# remove duplicates 
d <- distinct(d, subject,prepost,threshold, .keep_all=TRUE)               # use diff <- setdiff(df1, df2) to compare the data frames
                                                                                      # and check removed rows

# write data
write_xlsx(d, "RT_cons_final.xlsx")


# prepare entire dataset for plotting 
d$fgroup        = factor(d$group,   levels = c(1,2,3,4), labels = c("GGEE", "GGNE", "AC", "PC"))
d$fprepost      = factor(d$prepost, levels = c(1,2,3),     labels = c("pre", "post", "cons"))
d_pre           = subset(d, fprepost%in%c("pre"))   # add a column with pretest threshold to add as covariate
d$pretest       = d_pre$threshold[match(unlist(d$subject), d_pre$subject)]
d               = subset(d, fgroup%in%c('GGEE', 'GGNE', 'AC'))
d$fsubject      = factor(d$subject)   #first subset based on group and then factorize subject, otherwise R will still say fsubject
                                      #contains 118 lines

d_melt          = melt(d, id.var=c("fsubject", "fgroup", "fprepost"), measure.var=c("threshold"))
d_wide          = cast(fsubject + fgroup ~ fprepost , data=d_melt, mean)
head(d_wide)


# prepare testretest dataset for plotting
head(d_testretest)
label           = c(11,12,21,22,31,32)
stimulus        = c(1,2,1,2,1,2)
d_testretest$testretest    = mapvalues(d_testretest$testretest, from = label, to = stimulus)

d_testretest               = subset(d_testretest, group < 4)

d_testretest$fsubject      = factor(d_testretest$subject)
d_testretest$fgroup        = factor(d_testretest$group,   levels = c(1,2,3), labels = c("GGEE", "GGNE", "AC"))
d_testretest$fprepost      = factor(d_testretest$prepost, levels = c(1,2,3),     labels = c("pre", "post", "cons"))
d_testretest$ftestretest   = factor(d_testretest$testretest, levels = c(1,2), labels = c("test", "retest"))

d_melt_testretest          = melt(d_testretest, id.var=c("fsubject", "fgroup", "fprepost", "ftestretest"), measure.var=c("threshold"))
d_wide_testretest          = cast(fsubject + fgroup + fprepost ~ ftestretest , data=d_melt_testretest, mean)
head(d_wide_testretest)

pre               = subset(d_testretest, fprepost == "pre")
pre_melt          = melt(pre, id.var=c("fsubject", "fgroup", "ftestretest"), measure.var=c("threshold"))
pre_wide          = cast(fsubject + fgroup ~ ftestretest , data=pre_melt, mean)
head(pre_wide)

post              = subset(d_testretest, fprepost == "post")
post_melt         = melt(post, id.var=c("fsubject", "fgroup", "ftestretest"), measure.var=c("threshold"))
post_wide         = cast(fsubject + fgroup ~ ftestretest , data=post_melt, mean)
head(post_wide)

cons              = subset(d_testretest, fprepost == "cons")
cons_melt         = melt(cons, id.var=c("fsubject", "fgroup", "ftestretest"), measure.var=c("threshold"))
cons_wide         = cast(fsubject + fgroup ~ ftestretest , data=cons_melt, mean)
head(cons_wide)



# descriptives ------

#'## Descriptives
# prepare data for descriptives
d_melt                  <- melt(d, id.var=c("fsubject", "fgroup", "fprepost"), measure.var=c("threshold"))
d_thresholds            <- cast(fsubject + fgroup ~ fprepost , data = d_melt, fun.aggregate=mean)
colnames(d_thresholds)  <- c("subject", "group", "pretest", "posttest", "consolidation")
head(d_thresholds) 

#'Descriptives median & IQR                                  # non-normal data so median + IQR instead of mean + sd
descriptives            <- cast(d_melt, fgroup*fprepost ~ ., fun.aggregate=c(median,IQR))
names(descriptives)[3]  <- "median"
names(descriptives)[4]  <- "IQR"
descriptives

#' Descriptives numbers of subjects                                 
numbers            <- cast(d_melt, fgroup*fprepost ~ ., fun.aggregate=length)
numbers


# plots ------

#'## Plots

#' boxplot
boxplot         = ggplot(d, aes(x=fgroup, y=threshold, fill=fprepost)) + 
                  geom_boxplot() + theme_classic()
dotplot         = boxplot + geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(0.75), dotsize=0.5, binwidth=15) +
                  labs(title="Intervention effect on rise time task",x="group", y = "threshold (ms)", fill="test phase") +
                  theme(text = element_text(size=30))

tiff("interventioneffect_dotplot_cons.tiff",width=800,height=800)                 #save figure in wd
print(dotplot)
dev.off()

#' boxplot with individual subject lines (not jittered)
boxplotNJ         = ggplot(d, aes(x=fprepost, y=threshold, fill=fprepost)) + 
                    geom_boxplot(outlier.shape = NA) + theme_classic() + facet_wrap(. ~ fgroup, strip.position="bottom")   #outlier shape is NA because we already have individual points
dotplotNJ         = boxplotNJ + geom_point(pch = 21, position = position_dodge(0.75)) 
lineplotNJ        = dotplotNJ + geom_line(aes(group=fsubject), position=position_dodge(0), alpha=0.5) +   #alpha makes the lines more see through
                    labs(title="Rise time discrimination",x="group", y = "threshold (ms)", fill="test phase") +
                    scale_fill_discrete(labels = c("pretest", "posttest", "consolidation test")) +   #change legend labels
                    theme(text = element_text(size=30),
                          plot.title = element_text(hjust = 0.5),   #center title
                          axis.ticks.x=element_blank(),   #we made the plot with test phase on the x axis and facetted by group, but changed the aes
                          axis.text.x=element_blank(),    #so that group seems to be on the x axis (hence we had to remove the test phase x axis labels and ticks)
                          legend.position='bottom')

tiff("boxplot_individualsubjectsNJ_cons.tiff",width=800,height=800)                 #save figure in wd
print(lineplotNJ)
dev.off()

#' boxplot with individual subject lines (jittered)
pd = ggplot2::position_jitterdodge(dodge.width = 0.75, jitter.width = 0.3, seed = 1)
boxplotJ         = ggplot(d, aes(x=fprepost, y=threshold, fill=fprepost)) + 
  geom_boxplot(outlier.shape = NA) + theme_classic() + facet_wrap(. ~ fgroup, strip.position="bottom")   #outlier shape is NA because we already have individual points
dotplotJ         = boxplotJ + geom_point(aes(group=fsubject), pch = 21, position=pd) 
lineplotJ        = dotplotJ + geom_line(aes(group=fsubject), position=pd, alpha=0.5) +   #alpha makes the lines more see through   
  labs(title="Rise time discrimination",x="group", y = "threshold (ms)", fill="test phase") +   
  scale_fill_discrete(labels = c("pretest", "posttest", "consolidation test")) +   #change legend labels                  
  theme(text = element_text(size=30),
        plot.title = element_text(hjust = 0.5),   #center title
        axis.ticks.x=element_blank(),    #we made the plot with test phase on the x axis and facetted by group, but changed the aes
        axis.text.x=element_blank(),     #so that group seems to be on the x axis (hence we had to remove the test phase x axis labels and ticks)
        legend.position='bottom')

tiff("boxplot_individualsubjectsJ_cons.tiff",width=800,height=800)                 #save figure in wd
print(lineplotJ)
dev.off()

#' raincloud plot
source("R_rainclouds.R")
library(cowplot)
 
raincloud      = ggplot(d,aes(x=fprepost,y=threshold, fill = fprepost, colour = fprepost))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE) +
    ylab('threshold (ms)')+xlab('group')+theme_classic()+facet_wrap(~fgroup,strip.position="bottom") +
    theme(text = element_text(size=30),
        plot.title = element_text(hjust = 0.5),   #center title
        axis.ticks.x=element_blank(),    #we made the plot with test phase on the x axis and facetted by group, but changed the aes
        axis.text.x=element_blank(),     #so that group seems to be on the x axis (hence we had to remove the test phase x axis labels and ticks)
        legend.position='bottom')
# add points, lines, boxplots, ...

p9 <- ggplot(simdat,aes(x=group,y=score, fill = group, colour = group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = TRUE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(group)+0.25, y = score),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  ylab('Score')+xlab('Group')+coord_flip()+theme_cowplot()+guides(fill = FALSE, colour = FALSE) + facet_wrap(~gr2)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 9: Complex Raincloud Plots with Facet Wrap")
ggsave(filename=file.path('figs', 'tutorial_R', '9facetplot.png'), width = w, height = h)
p9


#' correlation plot test retest
plot(d_wide_testretest$test, d_wide_testretest$retest, xlab="Test", ylab="Retest", main="Test-retest reliability")
plot(pre_wide$test, pre_wide$retest, xlab="Test", ylab="Retest", main="Test-retest reliability pre-test")
plot(post_wide$test, post_wide$retest, xlab="Test", ylab="Retest", main="Test-retest reliability post-test")
plot(cons_wide$test, cons_wide$retest, xlab="Test", ylab="Retest", main="Test-retest reliability consolidation test")

# normality check ------

fitQQ <- lm(threshold ~ fprepost+fgroup, data=d)
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
ks.test(fit_residuals, "pnorm", mean=mean(fit_residuals), sd=sd(fit_residuals))
  # normality assumption violated

# normality check with log transform
fitQQlog <- lm(logthreshold ~ fprepost+fgroup, data=d)
qqPlot(fitQQlog, main="QQ Plot")
fit_residuals_log <- residuals(object = fitQQlog)
ks.test(fit_residuals_log, "pnorm", mean=mean(fit_residuals_log), sd=sd(fit_residuals_log))
  # normality assumption not violated


# check homogeneity of variances with log transform
leveneTest(threshold ~ fgroup*fprepost, data = d)
  # homogeneity of variances assumption violated

# check equality of distributions for permutation testing
sm.density.compare(d$threshold, d$fgroup)
  # distributions appear to be quite equal


# analyses ------------

### robust lmm

options(scipen = 999)

## 
anova_merMod<-function(model,rand,w=NULL,seed=round(runif(1,0,100),0),nsim=10000){
  data<-model@frame
  if(!is.null(w)){
    data<-data[,-grep("(weights)",names(data))]
  }
  
  resp<-names(model.frame(model))[1]
  #generate a list of reduced model formula
  fs<-list()
  fs[[1]]<-as.formula(paste(resp,"~ 1 +",rand))
  nb_terms<-length(attr(terms(model),"term.labels"))
  if(nb_terms>1){
    for(i in 1:nb_terms){
      tmp<-c(attr(terms(model),"term.labels")[1:i],rand)
      fs[[i+1]]<-reformulate(tmp,response=resp)
    }      
  }
  
  #fit the reduced model to the data
  
  fam<-family(model)[1]$family
  if(fam=="gaussian"){
    m_fit<-lapply(fs,function(x) lmer(x,data,REML=FALSE))
  } else if(fam=="binomial"){
    m_fit<-lapply(fs,function(x) glmer(x,data,family=fam,weights=w))
  }  else{
    m_fit<-lapply(fs,function(x) glmer(x,data,family=fam))
  }
  
  #compare nested model with one another and get LRT values (ie increase in the likelihood of the models as parameters are added)
  tab_out<-NULL
  
  for(i in 1:(length(m_fit)-1)){
    comp<-PBmodcomp(m_fit[[i+1]],m_fit[[i]],seed=seed,nsim=nsim)    
    term_added<-attr(terms(m_fit[[i+1]]),"term.labels")[length(attr(terms(m_fit[[i+1]]),"term.labels"))]
    #here are reported the bootstrapped p-values, ie not assuming any parametric distribution like chi-square to the LRT values generated under the null model
    #these p-values represent the number of time the simulated LRT value (under null model) are larger than the observe one
    tmp<-data.frame(term=term_added,LRT=comp$test$stat[1],p_value=comp$test$p.value[2])
    tab_out<-rbind(tab_out,tmp)
    print(paste("Variable ",term_added," tested",sep=""))
  }  
  print(paste("Seed set to:",seed))
  return(tab_out)  
}
##


rfit <- rlmer(threshold ~ fgroup*fprepost + (1|fsubject), data = d)

summary(rfit)

getME(rfit, "w_e") # all robustness weights for the residuals
getME(rfit, "w_b") # all robustness weights for the random effects

plot(rfit) # QQ plot etc. that also indicate weights

anova_merMod(model=rfit,rand="(1|fsubject)")        # uses below function that wraps around the PBmodcomp function to compute bootstrapped p-values 
                                            # for each term in a model by sequentially adding them
  # main effect time: .0009
  # interaction effect group x time: 0.044


# post-hoc on main effect time
d_pre   = subset(d, fprepost%in%c("pretest"))
mean(d_pre$threshold, 0.2)  # 200.64
median(d_pre$threshold) # 189
d_post  = subset(d, fprepost%in%c("posttest"))
mean(d_post$threshold, 0.2) # 107.33
median(d_post$threshold)  # 95
d_cons  = subset(d, fprepost%in%c("consolidation"))
mean(d_cons$threshold, 0.2) # 66.36
median(d_cons$threshold)  # 60

pre_vs_post    = wilcox.test(d_wide$pretest, d_wide$posttest, paired=TRUE, alternative="greater")$p.value        # holm = < .001
post_vs_cons   = wilcox.test(d_wide$posttest, d_wide$consolidation, paired=TRUE, alternative="greater")$p.value  # holm = < .001
pre_vs_cons    = wilcox.test(d_wide$pretest, d_wide$consolidation, paired=TRUE, alternative="greater")$p.value   # holm = < .001

posthoctime           = c("pre_vs_post", "post_vs_cons", "pre_vs_cons")
posthoctime           = as.data.frame(posthoctime)
names(posthoctime)[1] = "comparison"
posthoctime$pvalues   =  p.adjust(c(pre_vs_post, post_vs_cons, pre_vs_cons), method="holm")

posthoctime


# post-hoc test on interaction group x prepost #
# are there differences between pretest, posttest and consolidation test for the 3 groups separately? 
d_ph <- dcast(d, fsubject ~ fgroup + fprepost, value.var="threshold", fun.aggregate = mean)
head(d_ph)
# this format is needed for post-hoc test
# post hoc test by performing wilcox test on all conditions of interest

GGEEpre_vs_GGEEpost  = wilcox.test(d_ph$GG_EE_pretest,                           
                                  d_ph$GG_EE_posttest,                           
                                  paired=TRUE, alternative="greater")$p.value     # GGEEpre_vs_GGEEpost: .0005 V
GGEEpost_vs_GGEEcons = wilcox.test(d_ph$GG_EE_posttest,                            
                                  d_ph$GG_EE_consolidation,                           
                                  paired=TRUE, alternative="greater")$p.value     # GGEEpost_vs_GGEEcons: 0.35
GGEEpre_vs_GGEEcons  = wilcox.test(d_ph$GG_EE_pretest,                            
                                   d_ph$GG_EE_consolidation,                           
                                   paired=TRUE, alternative="greater")$p.value    # GGEEpre_vs_GGEEcons: .0002 V
GGNEpre_vs_GGNEpost  = wilcox.test(d_ph$GG_NE_pretest,                                                    
                                  d_ph$GG_NE_posttest,                           
                                  paired=TRUE, alternative="greater")$p.value     # GGNEpre_vs_GGNEpost: 0.058 
GGNEpost_vs_GGNEcons = wilcox.test(d_ph$GG_NE_posttest,                            
                                   d_ph$GG_NE_consolidation,                           
                                   paired=TRUE, alternative="greater")$p.value    # GGNEpost_vs_GGNEcons: 0.048 V
GGNEpre_vs_GGNEcons  = wilcox.test(d_ph$GG_NE_pretest,                            
                                   d_ph$GG_NE_consolidation,                           
                                   paired=TRUE, alternative="greater")$p.value    # GGNEpre_vs_GGNEcons: 0.0003 V
ACpre_vs_ACpost      = wilcox.test(d_ph$Active_Control_pretest,                   
                                  d_ph$Active_Control_posttest,                 
                                  paired=TRUE, alternative="greater")$p.value     # ACpre_vs_ACpost: 0.35
ACpost_vs_ACcons     = wilcox.test(d_ph$Active_Control_posttest,                   
                                  d_ph$Active_Control_consolidation,                 
                                  paired=TRUE, alternative="greater")$p.value     # ACpost_vs_ACcons: 0.005 V
ACpre_vs_ACcons      = wilcox.test(d_ph$Active_Control_pretest,                   
                                   d_ph$Active_Control_consolidation,                 
                                   paired=TRUE, alternative="greater")$p.value    # ACpre_vs_ACcons: 0.001 V

posthocinteraction         = c("GGEEpre_vs_GGEEpost", "GGEEpost_vs_GGEEcons", "GGEEpre_vs_GGEEcons",
                                 "GGNEpre_vs_GGNEpost", "GGNEpost_vs_GGNEcons", "GGNEpre_vs_GGNEcons",
                                 "ACpre_vs_ACpost", "ACpost_vs_ACcons", "ACpre_vs_ACcons")
posthocinteraction         = as.data.frame(posthocinteraction)
names(posthocinteraction)[1] = "comparison"
posthocinteraction$pvalues =  p.adjust(c(GGEEpre_vs_GGEEpost, GGEEpost_vs_GGEEcons, GGEEpre_vs_GGEEcons,
                                         GGNEpre_vs_GGNEpost, GGNEpost_vs_GGNEcons, GGNEpre_vs_GGNEcons,
                                         ACpre_vs_ACpost, ACpost_vs_ACcons, ACpre_vs_ACcons), method="holm")
posthocinteraction

# check descriptives for medians
# GG_EE	pretest	278.0	
# GG_EE	posttest	81.0	
# GG_EE	consolidation	84.5	
# GG_NE	pretest	119.0	
# GG_NE	posttest	91.5	
# GG_NE	consolidation	62.0	
# Active_Control	pretest	175.0	
# Active_Control	posttest	139.5	
# Active_Control	consolidation	55.0


# are there differences between the groups at pre, post and consolidation separatly? NO
# order data for Kruskal.test (order by group)
d_ordered = d[order(d$fgroup),]                     # data need to be ordered this way for Kruskal

d_pre            = subset(d_ordered, (fprepost%in%c("pretest")))
groupdiff_pre    = kruskal.test(threshold ~ fgroup, data=d_pre)$p.value         # 0.48 (holm)

d_post           = subset(d_ordered, (fprepost%in%c("posttest")))
groupdiff_post   = kruskal.test(threshold ~ fgroup, data=d_post)$p.value        # 0.48 (holm)

d_cons           = subset(d_ordered, (fprepost%in%c("consolidation")))
groupdiff_cons   = kruskal.test(threshold ~ fgroup, data=d_cons)$p.value        # 0.48 (holm)

p.adjust(c(groupdiff_pre, groupdiff_post, groupdiff_cons), method="holm")


# same analysis but with pretest value added as covariate in the analyses

rfitcov <- rlmer(threshold ~ fgroup*fprepost+pretest + (1|fsubject), data = d) # robust mixed model with rlmer (robustlmm package)

summary(rfitcov)

getME(rfitcov, "w_e") # all robustness weights for the residuals
getME(rfitcov, "w_b") # all robustness weights for the random effects

plot(rfitcov) # QQ plot etc. that also indicate weights

set.seed(123)
anova_merMod(model=rfitcov,rand="(1|fsubject)") # uses below function that wraps around the PBmodcomp function to compute bootstrapped p-values 
                                               # for each term in a model by sequentially adding them

# main effect time: .0009
# interaction effect group x time: .037
# covariate: .0009

compare(rfit, rfitcov, show.rho.functions = FALSE)

d$rpred   = predict(rfit, type = "response") # this gives you the predicted values of the model


# analyses EE ------------------

# prepare data for correlation analysis

d_wide$diff         = d_wide$pre - d_wide$post
colnames(d_wide)    = c("subject", "group", "pre", "post", "diff") 

EE                  = read_excel("EE_exposure_Femke_correct.xlsx")
d_wide$EE_hours     = EE$SessionTime_Stories_Hours[match(unlist(d_wide$subject), EE$iCode)]   
colnames(d_wide)    = c("subject", "group", "pre", "post", "diff", "hoursEE") 

d_EE                = subset(d_wide, group%in%c("GG_EE"))

corrEE              = as.data.frame(d_EE[c("hoursEE", "diff")]) # needs to be data frame so corr recognizes column names to use


# assumptions: violated so better opt vor spearman of robust correlation

# absence of outliers in either variable
boxplot(corrEE$diff)
boxplot(corrEE$hoursEE) # outliers -> violated
boxplot(corrEE$accuracyEE) # outliers -> violated

# plots

setwd("C:/Users/u0125029/Documents/3. Onderzoek/3. Post-test 2019/5. Analyse/RT_task/Plots")

tiff("corr_hoursEE.tiff",width=400,height=400)                 #save figure in wd
plot(corrEE$hoursEE, corrEE$diff,  main="pretest - posttest threshold vs. hours played EE", xlab="Hours played EE", ylab="pretest - posttest threshold (ms)")
abline(lm(diff ~ hoursEE, data=corrEE))
dev.off()

# correlation analyses

cor.test(corrEE$diff, corrEE$hoursEE, method="spearman")    # spearman is robust, pearson isn't

winall(corrEE)  # robust correlation coefficients (Mair & Wilcox, 2019)

#round(cor(corr, method = c("spearman"), use="complete.obs"), 2)
#rquery.cormat(corr)
#chart.Correlation(corr, histogram=TRUE, header=TRUE)

# analyses GG -------------------

head(d_wide)

GG                  = read.csv("GG_exposure.csv")
d_wide$GG_hours     = GG$HoursPlayed[match(unlist(d_wide$subject), GG$iCode)]   
d_wide$GG_levels    = GG$LevelsPlayed[match(unlist(d_wide$subject), GG$iCode)]
d_wide$GG_progress  = GG$GameProgress[match(unlist(d_wide$subject), GG$iCode)]
d_wide$GG_accuracy  = GG$Accuracy[match(unlist(d_wide$subject), GG$iCode)]
colnames(d_wide)    = c("subject", "group", "pre", "post", "diff", "hoursEE", "accuracyEE", "hoursGG", "levelsGG", "progressGG", "accuracyGG") 

d_GG                = subset(d_wide, group%in%c("GG_EE", "GG_NE"))

corrGG              = as.data.frame(d_GG[c("hoursGG", "levelsGG", "progressGG", "accuracyGG", "diff")]) # needs to be data frame so corr recognizes column names to use

# assumptions: violated so better opt for spearman or robust correlation

boxplot(corrGG$diff) # outliers -> violated

# plots

setwd("C:/Users/u0125029/Documents/3. Onderzoek/3. Post-test 2019/5. Analyse/RT_task/Plots")

tiff("corr_hoursGG.tiff",width=400,height=400)                 #save figure in wd
plot(corrGG$hoursGG, corrGG$diff,  main="pretest - posttest threshold vs. hours played GG", xlab="Hours played GG", ylab="pretest - posttest threshold (ms)")
abline(lm(diff ~ hoursGG, data=corrGG))
dev.off()

tiff("corr_levelsGG.tiff",width=400,height=400)
plot(corrGG$levelsGG, corrGG$diff,  main="pretest - posttest threshold vs. levels GG", xlab="levels GG", ylab="pretest - posttest threshold (ms)")
abline(lm(diff ~ levelsGG, data=corrGG))
dev.off()

tiff("corr_progressGG.tiff",width=400,height=400)
plot(corrGG$progressGG, corrGG$diff,  main="pretest - posttest threshold vs. progress GG", xlab="progress GG", ylab="pretest - posttest threshold (ms)")
abline(lm(diff ~ progressGG, data=corrGG))
dev.off()

tiff("corr_accuracyGG.tiff",width=400,height=400)
plot(corrGG$accuracyGG, corrGG$diff,  main="pretest - posttest threshold vs. accuracy GG", xlab="Accuracy GG", ylab="pretest - posttest threshold (ms)")
abline(lm(diff ~ accuracyGG, data=corrGG))
dev.off()

# correlation analyses

cor.test(corrGG$diff, corrGG$hoursGG, method="spearman")    # spearman is robust, pearson isn't
cor.test(corrGG$diff, corrGG$levelsGG, method="spearman")
cor.test(corrGG$diff, corrGG$progressGG, method="spearman")
cor.test(corrGG$diff, corrGG$accuracyGG, method="spearman") 

winall(corrGG)  # robust correlation coefficients (Mair & Wilcox, 2019)

# alternative analyses (non-optimal)  --------------------
### linear mixed effects

fit             = lmer(logthreshold  ~ fgroup*fprepost + (1|subject), data=d)

summary(fit) 

Anova(fit, type = "III", test.statistic = "F")
# lme4 documentation: Anova provides a wrapper for Kenward-Roger-corrected tests using pbkrtest
# Kenward-Roger approximation, so indeed a good way to get p-values for mixed models

ph              = emmeans(fit, specs = pairwise  ~ fgroup:fprepost, adjust = "bonferroni")
ph$contrasts

### robust anova (but can't handle missing data)

bwtrim(threshold ~ fgroup*fprepost, id = subject, data = d_aov)
sppbb(threshold ~ fgroup*fprepost, id = subject, data = d_aov)
sppba(threshold ~ fgroup*fprepost, id = subject, data = d_aov)
sppbi(threshold ~ fgroup*fprepost, id = subject, data = d_aov)

# plot with trimmed means and trimmed se's
d_aov.agg            = with(d_aov, aggregate(threshold, list(fgroup = fgroup, fprepost = fprepost), mean, trim = 0.20))
d_aov.agg$tse        = as.vector(with(d_aov, by(threshold, list(fgroup = fgroup, fprepost = fprepost), trimse, tr = 0.20)))

gp                   = ggplot(d_aov.agg, aes(x = fgroup, y = x, colour = fprepost, group = fprepost)) + ylab("threshold")
gp + geom_line() + geom_point(aes(shape = fprepost), size = 3) + 
  geom_errorbar(aes(ymax = x + tse, ymin = x - tse), width = 0.1) 



### permutation tests

# order data for Kruskal.test (order by group)
d_ordered = d[order(d$fgroup),]                     # data need to be ordered this way for Kruskal
kruskal.test(threshold ~ fgroup, data =  d_ordered)                     # kruskal: when factor contains more than two levels
# one-way analysis group: no sign difference between groups 

d_pre = subset(d_ordered, (fprepost%in%c("pretest")))
kruskal.test(threshold ~ fgroup, data=d_pre)

d_post = subset(d_ordered, (fprepost%in%c("posttest")))
kruskal.test(threshold ~ fgroup, data=d_post)


wilcox.test(d_wide$pretest, d_wide$posttest, 
            alternative ="greater", paired = TRUE)                                # wilcoxon:  when comparing two conditions with paired observations
# mann-whitney: two conditions with independent observations
# one-way analysis test phase: sign difference between pretest and posttest
# Ha not possible in terms of mean, median, ... because of non-parametric test
# Kids are more likely to have lower RT thresholds in the post-test compared
# to the pre-test (course non-parametric testing p. 29)


# using lmPerm library

Permutation <- aovp(threshold ~ fgroup*fprepost + Error(fsubject/fprepost), data=d_aov, perm="Exact")
summary(Permutation)



d_ph <- dcast(d, fsubject ~ fgroup + fprepost, value.var="threshold")
head(d_ph)
# this format is needed for post-hoc test
# post hoc test by performing wilcox test on all conditions of interest

GGEEpre_vs_GGEEpost = wilcox.test(d_ph$GG_EE_pretest,                            # 0.0002313328  (holm & hochberg)
                                  d_ph$GG_EE_posttest,                           # 0.0002313328  (bonferroni)
                                  paired=TRUE, alternative="greater")$p.value                       
GGNEpre_vs_GGNEpost = wilcox.test(d_ph$GG_NE_pretest,                            # 0.0387299202  (holm & hochberg)                         
                                  d_ph$GG_NE_posttest,                           # 0.0580948803  (bonferroni)
                                  paired=TRUE, alternative="greater")$p.value              
ACpre_vs_ACpost     = wilcox.test(d_ph$Active_Control_pretest,                  # 0.2077287478   (holm & hochberg)
                                  d_ph$Active_Control_posttest,                 # 0.6231862435   (bonferroni)
                                  paired=TRUE, alternative="greater")$p.value              
# important: cannot compute exact p-values because of ties (all cases) and zeroes (case 2 & 3)

p.adjust(c(GGEEpre_vs_GGEEpost, GGNEpre_vs_GGNEpost, ACpre_vs_ACpost), method="holm")


# test-retest reliability

cor.test(d_wide_testretest$test, d_wide_testretest$retest, method=c("pearson"))
cor.test(pre_wide$test, pre_wide$retest, method=c("pearson"))
cor.test(post_wide$test, post_wide$retest, method=c("pearson"))



# other options -----------

# PLOT INCLUDING ALL DATA #
# prepare data
plot.median.iqr            <- cast(d_melt, fgroup*fprepost ~ ., c(median,quantile25,quantile75))
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
d_aov_ordered2 = d_aov[order(d_aov$fprepost, d_aov$subject),]
wilcox.test(threshold~fprepost, data=d_aov_ordered2,
            alternative ="greater", paired = TRUE) 
# for this option (long data) the data need to be ordered so that the first observation where fprepost = pretest is paired to the first
# observation where fprepost = posttest (this is the case in both d_aov and d_aov_ordered2)
# for wide format we don't need to order in a specific way because we have all thesholds on one line

# sukkelruimte ------

boxplot(value~fprepost:fgroup, data=d_melt)$out         # get actual outlier values

# assign outlier values into a vector
outliers <- boxplot(value~fprepost:fgroup, data=d_melt, plot=FALSE)$out
# check the results
print(outliers)
# First you need find in which rows the outliers are
d[which(d$threshold %in% outliers),]



# ARTool

# ART needs subject as first column and DV as last column with IV's in between
d_ART <- d[,c('subject','fgroup','fprepost','threshold')]
head(d_ART)

m           <- art(threshold ~ fgroup * fprepost + (1|subject), data=d_ART)
            # (1|subject):              anova run as lmer
            # Error(fsubject/fprepost): anova run as RM anova
summary(m)  # F values of ANOVAs on aligned responses not of interest are not all approx. 0: ART may not be appropriate
anova(m)
