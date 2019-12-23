#Adaptive Validation Example Code
#Created By: Lindsay Collin
# 12 Dec 2019
#Goal: illustrate how to implement the AV substudy design for prospectively collected validation data
#Estimate PPV and NPV for exposure misclassification
#
#Stopping criteria: 
# 1. Efficacy reached if lower CI bound above 0.60
# 2. Futility reached if upper CI bound below 0.60
#######################################################

#load packages
library(tidyverse)
library(binom)


#read in dataset
VAL2<-read.sas7bdat("strong_y_new.sas7bdat")

#Set value to indicate who has been validated
VAL2$T<-0
summary(val)

#recode Gender
VAL2$gen<-ifelse(VAL2$GENDER=='M',0,1)


#Calculate overall PPV and NPV
xtabs(~gen+natal_sexX, data=val)
dat1 <- as.table(matrix(c(204,48,23,260), nrow = 2, byrow = TRUE))
colnames(dat1) <- c("Dis+","Dis-")
rownames(dat1) <- c("Test+","Test-")
rval1 <- epi.tests(dat1, conf.level = 0.95)
print(rval1); summary(rval1)



#Set seed
set.seed(12345)

#subset first 5 observations to fill each cell

#randomly select 5 obs from each gender

#subset by study time
subset(VAL2, count==5)
VAL3<-subset(VAL2, study_time<=54)
by_dia1<- VAL3 %>% group_by (gen)

samp2<-sample_n(by_dia1, 5)


#subset datasets for PPV and NPV

table(samp2$gen, samp2$natal_sexX)


#identify validation set in original dataset

VAL2$T<-ifelse(VAL2$study_id %in% samp2$study_id,1,VAL2$T)


#create next dataset

subset(VAL2, count==10) 
#study time 117


VAL4<-subset(VAL2, study_time<=117)

#eliminate those from risk set who have already been validated

VAL4<-subset(VAL4, T==0)

#randomly select 5 obs from each gender 

by_dia2<- VAL4 %>% group_by (gen)

samp3<-sample_n(by_dia2, 5)

table(samp3$gen, samp3$natal_sexX)


#identify validation set in original dataset

VAL2$T<-ifelse(VAL2$study_id %in% samp3$study_id,2,VAL2$T)
table(VAL2$T)

subset(VAL2, count==15)

VAL6<-subset(VAL2, study_time<=165)


#eliminate those from risk set who have already been validated

VAL7<-subset(VAL6, T==0)

#randomly select 5 obs from each gender

by_dia3<- VAL7 %>% group_by (gen)

samp3<-sample_n(by_dia3, 5)

table(samp3$gen, samp3$natal_sexX)

#identify validation set in original dataset

VAL2$T<-ifelse(VAL2$study_id %in% samp3$study_id,3,VAL2$T)

subset(val, count==20)

VAL8<-subset(VAL2, study_time<=224)


#eliminate those from risk set who have already been validated
VAL8<-subset(VAL8, T==0)

#randomly select 5 obs from each gender and inc_diabetes

by_dia4<- VAL8 %>% group_by (gen)

samp4<-sample_n(by_dia4, 5)

table(samp4$gen, samp4$natal_sexX)


#identify validation set in original dataset

VAL2$T<-ifelse(VAL2$study_id %in% samp4$study_id,4,VAL2$T)

#CONTINUE the above steps as needed

###################################################

#Bayes binomial models

##################################################


#PPV
binom.bayes(x=3, n=5, conf.level=0.95, type="central", 
                  prior.shape1=1, prior.shape2 = 1)

binom.bayes(x=3, n=5, conf.level=0.95, type="central", 
                  prior.shape1=4, prior.shape2 = 3)

binom.bayes(x=2, n=5, conf.level=0.95, type="central", 
                  prior.shape1=7, prior.shape2 = 5)

binom.bayes(x=4, n=5, conf.level=0.95, type="central", 
                  prior.shape1=9, prior.shape2 = 8)

binom.bayes(x=4, n=5, conf.level=0.95, type="central", 
            prior.shape1=13, prior.shape2 = 9)


#NPV 
binom.bayes(x=4, n=5, conf.level=0.95, type="central", 
                  prior.shape1=1, prior.shape2 = 1)
binom.bayes(x=2, n=5, conf.level=0.95, type="central", 
                  prior.shape1=5, prior.shape2 = 2)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
                  prior.shape1=7, prior.shape2 = 5)
binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
                  prior.shape1=12, prior.shape2 = 5)

binom.bayes(x=4, n=5, conf.level=0.95, type="central", 
            prior.shape1=17, prior.shape2 = 5)


#Plots

p<-ggplot(data=val10,
          mapping=aes(x=NV, y=Post.Mean,
                      ymin=Post.Lower, ymax=Post.Upper,
                      group=Measure, color=Measure))+
  geom_pointrange(position=position_dodge2(width = 10), size=0.5)+
  scale_x_continuous(breaks=seq(50,150,50), limits=c(0,160))+
  scale_y_continuous(breaks=seq(0.00,1.00,0.25), limits=c(0,1))+
  scale_color_discrete(name="Measure", labels=c("PPV","NPV"),
                       limits=c("PPV","NPV"))+
  labs(x=" ", y="  ", color="Measure")+
  geom_hline(aes(yintercept=0.6, linetype="dashed"),colour="black", size=1.1, data=threshold)+
  scale_linetype_manual(name="Threshold", values=c(2,2),labels=c(" 0.60 "))+
  ggtitle("Adult Subcohort")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size=1.1),
                     plot.title=element_text(size=14, face="bold", hjust=.5),
                     axis.title.x=element_text(size=12, face="bold"),
                     axis.title.y = element_text(size=12,face="bold"),
                     axis.text = element_text(face="bold", size=10),
                     legend.title = element_text(face="bold",size=10))
