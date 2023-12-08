##Survival Analysis 
##Updated: October25, 2023


##Fit survival models with marginal modeling of clustered data 

################


# #clear environment
remove(list=ls())


#Load Packages

library(dplyr) 
library(survival) #For KM
library(coxme) #For Cox models with mixed effects
library(survminer)

#Read in scavenging data 
scav.data<-read.csv("June2022/FINAL_FOR_POST/Ecofunction/2023_Results/SciReports_McKee_Review/scav.data.csv")%>%
  mutate(Site=as.factor(Site), mesoPres=as.factor(mesoPres))

##Some Summary Data
total.by.group<-scav.data%>%
  group_by(mesoPres)%>%
  summarize(total=n())

##Time to first scavenging event
arrive.summary<-scav.data%>%
  group_by(mesoPres,arrive.censor)%>%
  summarize(count=n())%>%
  left_join(total.by.group)%>%
  mutate(percent=count/total)

##Time to Total Consumption/Decomposition of a Carcass
consum.summary<-scav.data%>%
  group_by(mesoPres,con.censor)%>%
  summarize(count=n())%>%
  left_join(total.by.group)%>%
  mutate(percent=count/total)



##################
#Scavenging
##################

#############################################
#SCAVENGING: TIME TO FIRST SCAVENGING EVENT 
################################################

#Construct a KM curve to visualize the general patterns 

##KM Curves split based on whether mesomammals were present in passive surveys 
KM.scav.arrival <- survfit(Surv(Time2FirstVisit, arrive.censor) ~ mesoPres, data=scav.data)
KM.scav.arrival

##Plot the KM Curve 
KM.A<-ggsurvplot(KM.scav.arrival, xlab="Hours since station placement", ylab="Prob. carcass unvisited", xlim = c(0,180),  conf.int = T, legend.title= "", legend.labs=c("Detected", "Not detected"),  break.x.by = 50)
KM.A


#Cox regression 
#########################
scav.t2f<-coxph(Surv(Time2FirstVisit,  arrive.censor)~mesoPres+cluster(Site), data=scav.data)
scav.t2f

##Checking assumptions 
cox.zph(scav.t2f)#Not significant 
z1<-ggcoxzph(cox.zph(scav.t2f)) #p>0.05

#Plot survival curves from simple model 
nd<-with(scav.data, data.frame(mesoPres=c("Detected", "Not Detected")))
fit1<-survfit(scav.t2f, newdata=nd)
fit1
a<-ggsurvplot(fit1, data=scav.data, xlab="Hours since station placement", ylab="Prob. carcass unvisited", xlim = c(0,180),  conf.int = T, legend.title= "", legend.labs=c("Detected", "Not detected"),  break.x.by = 50)

##Extract just the plot to fix this last aesthic 
a$plot<-a$plot+theme(legend.text = element_text(size = 14, color = "black",))

a


#############################################
#SCAVENGING: TIME TO COMPLETE CONSUMPTION 
#############################################
## Scavenging Time to Consumption KM
KM.scav.consumption<- survfit(Surv(Time2Con, con.censor) ~ mesoPres, data=scav.data)
KM.scav.consumption

##make a plot 
KM.B<-ggsurvplot(KM.scav.consumption, legend="none", ylab="Prob. carcass remains", xlab="Hours since station placement",conf.int = T, xlim=c(0,180))
KM.B


#Simplest model--NO Random Effects 
scav.con<-coxph(Surv(Time2Con, con.censor)~mesoPres+cluster(Site), data=scav.data)
scav.con


##Checking assumptions 
cox.zph(scav.con)#Not significant 
z2<-ggcoxzph(cox.zph(scav.con))#p>0.05

fit2<-survfit(scav.con, newdata=nd)
nd
fit2
b<-ggsurvplot(fit2, data=scav.data, xlab="Hours since station placement", ylab="Prob. carcass remains", xlim = c(0,180),  conf.int = T,legend="none",  break.x.by = 50)
b


##################
#Frugivory #Time to first frugivory event (arrival of first frugivore)
##################
frug.data<-read.csv("June2022/FINAL_FOR_POST/Ecofunction/2023_Results/SciReports_McKee_Review/frug.data.csv")

#For arrival, frugivores were counted as arriving if we viewed them consuming any of the fruits as it was usually difficult to tell exactly which one they were eating. 

##data with just one fruit type represented to avoid repeats 
single.fruit<-frug.data%>%
  filter(Fruit=="Cocoplum")


##Some Summary Data
total.by.group.frug<-single.fruit%>%
  group_by(mesoPres)%>%
  summarize(total=n())

##Time to first scavenging event
arrive.summary.frug<-single.fruit%>%
  group_by(mesoPres,arrive.censor)%>%
  summarize(count=n())%>%
  left_join(total.by.group.frug)%>%
  mutate(percent=count/total)

## Frugivory Time to Arrival 
KM.frug.arrival <- survfit(Surv(Time2FirstVisit, arrive.censor) ~ mesoPres, data=single.fruit)
KM.frug.arrival

KM.C<-ggsurvplot(KM.frug.arrival, xlab="Hours since station placement", ylab="Prob. fruit unvisited", xlim = c(0,120),  conf.int = T, legend="none")
KM.C

frug.T2F<-coxph(Surv(Time2FirstVisit, arrive.censor) ~ mesoPres + cluster(Site), data=single.fruit)
frug.T2F
#z=1.35, p=0.182
#no differences from mesomammal presence
cox.zph(frug.T2F) #p=0.77
z3<-ggcoxzph(cox.zph(frug.T2F))




nd2<-with(single.fruit, data.frame(mesoPres=c("Detected", "Not Detected")))
fit3<-survfit(frug.T2F, newdata=nd2)
fit3
c<-ggsurvplot(fit3, data=single.fruit, xlab="Hours since station placement", ylab="Prob. fruit unvisited", xlim = c(0,180),  conf.int = T, legend="none",  break.x.by = 50)

##Survival Panel
library(cowplot)
library(png)
library(patchwork)
#These have to be created with the survival analysis code
a.plot<-a$plot ##Extract only the plot so it can be put into panel
b.plot<-b$plot
c.plot<-c$plot


tiff("June2022/FINAL_FOR_POST/Oct 2023 Submission/Figures/survival.fig.tiff",
     width=5,
     height=8,
     units="in",
     res=900,
     compression="lzw")
plot_grid(a.plot, b.plot, c.plot, ncol=1, nrow=3,labels=c("a", "b","c"), align = "v", rel_heights = c(.38,.31, .31), scale=0.93)
dev.off()
# 


tiff("June2022/FINAL_FOR_POST/Oct 2023 Submission/Figures/zph1.tiff",
     width=5,
     height=4,
     units="in",
     res=900,
     compression="lzw")
z1
dev.off()
# 


tiff("June2022/FINAL_FOR_POST/Oct 2023 Submission/Figures/zph2.tiff",
     width=5,
     height=4,
     units="in",
     res=900,
     compression="lzw")
z2
dev.off3

tiff("June2022/FINAL_FOR_POST/Oct 2023 Submission/Figures/zph3.tiff",
     width=5,
     height=4,
     units="in",
     res=900,
     compression="lzw")
z3
dev.off()
