####################################################################
#Fulton COVID imputation
#and bias analysis
#Author: Lindsay Collin
#Date: 10 July 2020
#Task: validate imputation of race/ethnicity using BSIG
#Task: use validation data for QBA to bias-adjust estimates of
#incidence, hospitalization, and mortality accounting for missing
#race/ethnicity
####################################################################
#load packages


library(wru)
library(tidycensus)
library(dplyr)
library(tidyverse)
library(janitor)
library(binom)
library(openxlsx)
library(MCMCpack)
library(tidyverse)
library(matrixStats)

#######---------------------------------BISG-----------------------------###

#load Georgia/Fulton Data (not shown)
#block information (subset those with geocoding data)

Fulton_BISG_short2_G<-Fulton_BISG_short2 %>% filter(block!="NA") #19091

#surname and census blockgroup
Fulton_pred_raceeth<-predict_race(voter.file = Fulton_BISG_short2_G,
                                  census.surname = TRUE, 
                                  surname.only = FALSE, 
                                  surname.year = 2010,
                                  census.geo = "block", 
                                  census.data = FULTON_ACSObj2018) 

#surname only for those with missing geocode
Fulton_BISG_short2_S<-subset(Fulton_BISG_short2, is.na(tract)) #546

Fulton_pred_raceeth_S<-predict_race(voter.file = Fulton_BISG_short2_S, 
                                    census.surname = TRUE, 
                                    surname.only = TRUE, 
                                    surname.year = 2010) 



Fulton_pred<-rbind(Fulton_pred_raceeth,Fulton_pred_raceeth_S)


## Most probable Race/Ethnicity 
Fulton_pred1<-Fulton_pred %>%
  rownames_to_column() %>%
  gather(pred_R, max_pred, pred.whi:pred.oth) %>%
  group_by(rowname) %>%
  filter(rank(-max_pred)==1)


##mere back to original dataset


Fulton_full_pred_aug<-left_join(AUG_prep, Fulton_pred1, by=c("pui"="ID"))
#######----------------------------------QBA-----------------------------####


#rename
Fulton_Aug2020full_pred<-Fulton_full_pred_aug

#subset to those who did not have reported race/ethnicity
BA<-subset(Fulton_Aug2020full_pred, race.miss==1)

#Each PPV follows a dirichlet distribution


#number of iterations
SIMS<-100000

#original dataset (TRUTH)
R_A<-260
R_B<-6964
R_H<-1617
R_O<-515
R_W<-3152

#ACS denominators

d_A<-69987
d_H<- 74328
d_B<-445992
d_O<-4238+1612+206
d_W<-406755


#infection race, complete case
qbinom(p=c(0.025,0.50,0.975),d_A,R_A/d_A)/d_A*10000
qbinom(p=c(0.025,0.50,0.975),d_B,R_B/d_B)/d_B*10000
qbinom(p=c(0.025,0.50,0.975),d_H,R_H/d_H)/d_H*10000
qbinom(p=c(0.025,0.50,0.975),d_O,R_O/d_O)/d_O*10000
qbinom(p=c(0.025,0.50,0.975),d_W,R_W/d_W)/d_W*10000


#hospitalization proportion, complete case
qbinom(p=c(0.025,0.50,0.975),260,25/260)/260*100
qbinom(p=c(0.025,0.50,0.975),1617,214/1617)/1617*100
qbinom(p=c(0.025,0.50,0.975),6964,1195/6964)/6964*100
qbinom(p=c(0.025,0.50,0.975),3152,312/3152)/3152*100
qbinom(p=c(0.025,0.50,0.975),515,30/515)/515*100

#mortality proportion, complete case
qbinom(p=c(0.025,0.50,0.975),260,5/260)/260*100
qbinom(p=c(0.025,0.50,0.975),1617,15/1617)/1617*100
qbinom(p=c(0.025,0.50,0.975),6964,320/6964)/6964*100
qbinom(p=c(0.025,0.50,0.975),3152,112/3152)/3152*100
qbinom(p=c(0.025,0.50,0.975),515,4/515)/515*100


#incidence difference (binomial proportion and variance)
i_W=R_W/d_W
v_w=(i_W*(1-i_W))/d_W
i_A=R_A/d_A
v_A=(i_A*(1-i_A))/d_A
i_H=R_H/d_H
v_H=(i_H*(1-i_H))/d_H
i_B=R_B/d_B
v_B=(i_B*(1-i_B))/d_B
i_O=R_O/d_O
v_O=(i_O*(1-i_O))/d_O

#incidence difference and 95%CI
rd_A=(i_A-i_W)
lowerA=(rd_A-1.96*sqrt(v_w+v_A))*10000
upperA=(rd_A+1.96*sqrt(v_w+v_A))*10000

rd_H=(i_H-i_W)
lowerH=(rd_H-1.96*sqrt(v_w+v_H))*10000
upperH=(rd_H+1.96*sqrt(v_w+v_H))*10000

rd_B=(i_B-i_W)
lowerB=(rd_B-1.96*sqrt(v_w+v_B))*10000
upperB=(rd_B+1.96*sqrt(v_w+v_B))*10000

rd_O=(i_O-i_W)
lowerO=(rd_O-1.96*sqrt(v_w+v_O))*10000
upperO=(rd_O+1.96*sqrt(v_w+v_O))*10000


#predicted race_eth
r_A<-178
r_B<-2296
r_H<-1034
r_O<-17
r_W<-3632

#set seed--Mamba mentality
set.seed(24) 

#Assign dirichlet distribution for PPV for each race/ethnicity category
#based on comparison of imputation compared with reported race/ethnicity

PPV_A<-rdirichlet(SIMS,c(145,13,16,12,28))
PPV_B<-rdirichlet(SIMS,c(16,5118,77,132,192))
PPV_H<-rdirichlet(SIMS,c(15,68,1288,68,103))
PPV_O<-rdirichlet(SIMS,c(4,11,6,1,2))
PPV_W<-rdirichlet(SIMS,c(80,1754,230,302,2827))

#95%CI
probs=c(0.025, 0.50, 0.975)
colQuantiles(PPV_A,probs=probs)
colQuantiles(PPV_B,probs=probs)
colQuantiles(PPV_H,probs=probs)
colQuantiles(PPV_O,probs=probs)
colQuantiles(PPV_W,probs=probs)



#Bias-adjustement, probabilitistically move imputed counts to expected truth
A<-r_A*PPV_A[,1]+PPV_B[,1]*r_B+r_H*PPV_H[,1]+PPV_O[,1]*r_O+PPV_W[,1]*r_W
B<-r_A*PPV_A[,2]+PPV_B[,2]*r_B+r_H*PPV_H[,2]+PPV_O[,2]*r_O+PPV_W[,2]*r_W
H<-r_A*PPV_A[,3]+PPV_B[,3]*r_B+r_H*PPV_H[,3]+PPV_O[,3]*r_O+PPV_W[,3]*r_W
O<-r_A*PPV_A[,4]+PPV_B[,4]*r_B+r_H*PPV_H[,4]+PPV_O[,4]*r_O+PPV_W[,4]*r_W
W<-r_A*PPV_A[,5]+PPV_B[,5]*r_B+r_H*PPV_H[,5]+PPV_O[,5]*r_O+PPV_W[,5]*r_W
W<-1082-A-B-H-O #remainder

#add back in the recorded race/ethnicity
R_A1<-R_A+A
R_B1<-R_B+B
R_H1<-R_H+H
R_O1<-R_O+O
R_W1<-R_W+W

#take quantiles of the counts (incorporates uncertainity)
quantile(R_A1,c(0.025,0.50,0.975))
quantile(R_B1,c(0.025,0.50,0.975))
quantile(R_H1,c(0.025,0.50,0.975))
quantile(R_O1,c(0.025,0.50,0.975))
quantile(R_W1,c(0.025,0.50,0.975))

#bias adjusted incidence
I.A<-R_A1/d_A
I.B<-R_B1/d_B
I.H<-R_H1/d_H
I.O<-R_O1/d_O
I.W<-R_W1/d_W

#variance of incidence for disparities measure
v.A=(I.A*(1-I.A))/d_A
v.H=(I.H*(1-I.H))/d_H
v.B=(I.B*(1-I.B))/d_B
v.W=(I.W*(1-I.W))/d_W
v.O=(I.O*(1-I.O))/d_O

#Quantiles of the binomial distribution
I.A2<-rbinom(SIMS,d_A,I.A)
I.B2<-rbinom(SIMS,d_B,I.B)
I.H2<-rbinom(SIMS,d_H,I.H)
I.O2<-rbinom(SIMS,d_O,I.O)
I.W2<-rbinom(SIMS,d_W,I.W)

pctA<-quantile(I.A2, c(0.025, 0.5, 0.975))/d_A*10000
pctB<-quantile(I.B2, c(0.025, 0.5, 0.975))/d_B*10000
pctH<-quantile(I.H2, c(0.025, 0.5, 0.975))/d_H*10000
pctO<-quantile(I.O2, c(0.025, 0.5, 0.975))/d_O*10000
pctW<-quantile(I.W2, c(0.025, 0.5, 0.975))/d_W*10000

print(pctA)
print(pctH)
print(pctB)
print(pctW)
print(pctO)

#Absolute Difference
#add back in random error: bootstrap approx
rannorm <- rnorm(SIMS, mean=0, sd=1)

I.dA<-I.A-I.W
I.dAe<-(I.dA-rannorm*(sqrt(v.A+v.W)))

I.dH<-I.H-I.W
I.dHe<-(I.dH-rannorm*(sqrt(v.H+v.W)))

I.dB<-I.B-I.W
I.dBe<-(I.dB-rannorm*(sqrt(v.B+v.W)))

I.dO<-I.O-I.W
I.dOe<-(I.dO-rannorm*(sqrt(v.O+v.W)))

quantile(I.dA, probs=c(0.025, 0.5, 0.975))*10000
quantile(I.dH, probs=c(0.025, 0.5, 0.975))*10000
quantile(I.dB, probs=c(0.025, 0.5, 0.975))*10000
quantile(I.dO, probs=c(0.025, 0.5, 0.975))*10000


### by hospitalization and mortality proportions

h_A<-25
h_H<-214
h_B<-1195
h_O<-4
h_W<-112

h_rA<-h_A/R_A1
h_rB<-h_B/R_B1
h_rH<-h_H/R_H1
h_rO<-h_O/R_O1
h_rW<-h_W/R_W1

#boot approx: reincoporate random error
rannorm <- rnorm(SIMS, mean=0, sd=1)

var.ha<-(h_rA*(1-h_rA))/R_A1
h_rA2<-h_rA-rannorm*sqrt(var.ha)
pctA1<-quantile(h_rA2, c(0.025, 0.5, 0.975))*100
pctA1
var.hb<-(h_rB*(1-h_rB))/R_B1
h_rB2<-h_rB-rannorm*sqrt(var.hb)
pctB1<-quantile(h_rB2, c(0.025, 0.5, 0.975))*100
pctB1

var.hH<-(h_rH*(1-h_rH))/R_H1
h_rH2<-h_rH-rannorm*sqrt(var.hH)
pctH1<-quantile(h_rH2, c(0.025, 0.5, 0.975))*100
pctH1

var.hO<-(h_rO*(1-h_rO))/R_O1
h_rO2<-h_rO-rannorm*sqrt(var.hO)
pctO1<-quantile(h_rO2, c(0.025, 0.5, 0.975))*100
pctO1

var.hW<-(h_rW*(1-h_rW))/R_W1
h_rW2<-h_rW-rannorm*sqrt(var.hW)
pctW1<-quantile(h_rW2, c(0.025, 0.5, 0.975))*100
pctW1

### mortality

d_A<-5
d_H<-15
d_B<-320
d_O<-4
d_W<-112

d_rA<-d_A/R_A1
d_rB<-d_B/R_B1
d_rH<-d_H/R_H1
d_rO<-d_O/R_O1
d_rW<-d_W/R_W1

#boot approx
rannorm <- rnorm(SIMS, mean=0, sd=1)

var.da<-(d_rA*(1-d_rA))/R_A1
d_rA2<-d_rA-rannorm*sqrt(var.da)
pctA2<-quantile(d_rA2, c(0.025, 0.5, 0.975))*100
pctA2
var.db<-(d_rB*(1-d_rB))/R_B1
d_rB2<-d_rB-rannorm*sqrt(var.db)
pctB2<-quantile(d_rB2, c(0.025, 0.5, 0.975))*100
pctB2

var.dH<-(d_rH*(1-d_rH))/R_H1
d_rH2<-d_rH-rannorm*sqrt(var.dH)
pctH2<-quantile(d_rH2, c(0.025, 0.5, 0.975))*100
pctH2

var.dO<-(d_rO*(1-d_rO))/R_O1
d_rO2<-d_rO-rannorm*sqrt(var.dO)
pctO2<-quantile(d_rO2, c(0.025, 0.5, 0.975))*100
pctO2

var.dW<-(d_rW*(1-d_rW))/R_W1
d_rW2<-d_rW-rannorm*sqrt(var.dW)
pctW2<-quantile(d_rW2, c(0.025, 0.5, 0.975))*100
pctW2

