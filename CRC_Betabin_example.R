#Adaptive Validation CRC-- beta-binomial models
#Example Code
#Author: Lindsay Collin: lindsay.jane.collin@emory.edu
#Date: December 23, 2019

##########################################
#stopping criteria
#NPV/PPV: threshold=0.80, precision=0.15
#Se/SP: threshold=0.90, precision=0.08
##########################################


#Bayes binomial model--needs function/package
library(binom)

#AV Balanced 5: PPV

binom.bayes(x=4, n=5, conf.level=0.95, type="central", 
                  prior.shape1=1, prior.shape2 = 1)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
                  prior.shape1=5, prior.shape2 = 2)

binom.bayes(x=4, n=5, conf.level=0.95, type="central", 
                  prior.shape1=10, prior.shape2 =2)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
                  prior.shape1=14, prior.shape2 = 3)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
                  prior.shape1=19, prior.shape2 = 3)


#NPV 

binom.bayes(x=4, n=5, conf.level=0.95, type="central", 
            prior.shape1=1, prior.shape2 = 1)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
            prior.shape1=5, prior.shape2 = 2)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
            prior.shape1=10, prior.shape2 = 2)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
            prior.shape1=15, prior.shape2 =2)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
            prior.shape1=20, prior.shape2 = 2)



#######################################
#PPV--10-per-cell

binom.bayes(x=9, n=10, conf.level=0.95, type="central", 
                  prior.shape1=1, prior.shape2 = 1)

binom.bayes(x=9, n=10, conf.level=0.95, type="central", 
                  prior.shape1=10, prior.shape2 = 2)

binom.bayes(x=9, n=10, conf.level=0.95, type="central", 
                  prior.shape1=19, prior.shape2 = 3)

binom.bayes(x=9, n=10, conf.level=0.95, type="central", 
                  prior.shape1=28, prior.shape2 =4)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
                  prior.shape1=37, prior.shape2 = 5)

binom.bayes(x=9, n=10, conf.level=0.95, type="central", 
                  prior.shape1=47, prior.shape2 = 5)

binom.bayes(x=5, n=10, conf.level=0.95, type="central", 
                  prior.shape1=56, prior.shape2 = 6)


#NPV--- 10 beta
binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=1, prior.shape2 = 1)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=11, prior.shape2 = 1)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=21, prior.shape2 = 1)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=31, prior.shape2 = 1)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=41, prior.shape2 = 1)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=51, prior.shape2 = 1)

binom.bayes(x=9, n=10, conf.level=0.95, type="central", 
            prior.shape1=61, prior.shape2 = 1)


#SE-- Balanced 5-per-cell
binom.bayes(x=4, n=5, conf.level=0.95, type="central", 
            prior.shape1=1, prior.shape2 = 1)

binom.bayes(x=4, n=5, conf.level=0.95, type="central", 
            prior.shape1=5, prior.shape2 = 2)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
            prior.shape1=9, prior.shape2 = 3)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
            prior.shape1=14, prior.shape2 = 3)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
            prior.shape1=19, prior.shape2 = 3)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
            prior.shape1=24, prior.shape2 = 3)



################################################
#Sp-- 5-per-cell

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
            prior.shape1=1, prior.shape2 = 1)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
            prior.shape1=6, prior.shape2 = 1)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
            prior.shape1=11, prior.shape2 = 1)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
            prior.shape1=16, prior.shape2 = 1)

binom.bayes(x=4, n=5, conf.level=0.95, type="central", 
            prior.shape1=21, prior.shape2 = 1)

binom.bayes(x=5, n=5, conf.level=0.95, type="central", 
            prior.shape1=25, prior.shape2 = 2)



#######################################
#Se-- 10-per-cell

binom.bayes(x=8, n=10, conf.level=0.95, type="central", 
            prior.shape1=1, prior.shape2 = 1)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=9, prior.shape2 = 3)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=19, prior.shape2 = 3)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=29, prior.shape2 = 3)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=39, prior.shape2 = 3)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=49, prior.shape2 = 3)

binom.bayes(x=2, n=3, conf.level=0.95, type="central", 
            prior.shape1=59, prior.shape2 = 3)


#SP--- 10-per-cell beta
binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=1, prior.shape2 = 1)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=11, prior.shape2 = 1)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=21, prior.shape2 = 1)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=31, prior.shape2 =1)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=41, prior.shape2 = 1)

binom.bayes(x=10, n=10, conf.level=0.95, type="central", 
            prior.shape1=51, prior.shape2 = 1)

binom.bayes(x=9, n=10, conf.level=0.95, type="central", 
            prior.shape1=61, prior.shape2 = 1)
