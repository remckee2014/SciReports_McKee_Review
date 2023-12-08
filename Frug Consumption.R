##Frug Efficiency Models with Extra Transformations as examples 
##For review in Scientific Reports Fall 2023

# #clear environment
remove(list=ls())

#library
library(tidyverse)
library(lme4)
library(car)
library(ggplot2)
library(cowplot)
library(dplyr)
library(emmeans)
library(glmmTMB)

##Read in data 
frug_dat<-read.csv("June2022/FINAL_FOR_POST/Ecofunction/2023_Results/SciReports_McKee_Review/frug.eff.csv",row=1,header=T)%>%mutate(prop.consumed2=
  case_when(prop.consumed==0~0.001,
            prop.consumed==1~0.999,
            prop.consumed>0&prop.consumed<1~prop.consumed) ##add values for beta regression
)

table(frug_dat$mesoPres)
##MANUSCRIPT RESULTS
#Proportion of Fruit Remaining with fruit as additive effect,site as random effect

#Additive model: Mixed linear model 
consumed.model<-lme4::lmer(data=frug_dat,  formula= (prop.consumed)~1+mesoPres+ Fruit+ (1|Site))
Anova(consumed.model)
emmeans(consumed.model, list(pairwise ~ Fruit), adjust = "tukey")

#Test normality 
shapiro.test(resid(consumed.model)) ## Does not violate assumption of normality 

##Examine Interaction 
interact.model<-lme4::lmer(data=frug_dat,  formula= (prop.consumed)~1+mesoPres* Fruit+ (1|Site))
summary(interact.model)
Anova(interact.model)

#Test normality 
shapiro.test(resid(interact.model))
hist(resid(interact.model))

#Summary of data 
frug_sum_con<-frug_dat%>%
  group_by(mesoPres, Fruit)%>%
  summarize(mean=mean(prop.consumed),sd=sd(prop.consumed))%>%mutate(lower=(mean-sd), upper=(mean+sd))


##SUPPLEMENTAL RESULTS


##Alternative approaches


#Arc Sin Transformation 
consumed.model.arcsin<-lme4::lmer(data=frug_dat,  formula= asin(prop.consumed)~1+mesoPres+ Fruit+ (1|Site))
summary(consumed.model.arcsin)
Anova(consumed.model.arcsin) ##Not much different than the results from untransformed. Still far from significant 

##Examine Interaction 
interact.model.asin<-lme4::lmer(data=frug_dat,  formula= asin(prop.consumed)~1+mesoPres* Fruit+ (1|Site))
summary(interact.model.asin)
Anova(interact.model.asin)



##Beta regression 
breg<-glmmTMB(data=frug_dat, family=beta_family(link="logit"), formula= (prop.consumed2)~1+mesoPres+ Fruit+(1|Site))
summary(breg)


breg.int<-glmmTMB(data=frug_dat, family=beta_family(link="logit"), formula= (prop.consumed2)~1+mesoPres* Fruit+ (1|Site))
summary(breg.int)
