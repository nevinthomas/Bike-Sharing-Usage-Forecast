setwd("C:/Users/eunbe/Google Drive/20Spring/Regression/RegressionProject")
ntadata <- read.table(file = "nta.csv", sep=",",header=TRUE);

##Variables
#startn: the number of rides starting at each nta
#endn: the number of rides starting at each nta
#totaln: startn + endn
#diffn: startn-endn
#area: how large each nta is (square miles)
#midage, teenage, older: the number of people
#low_, mid_, high_income: percentage of ppl belongs to each category * total # of population

####Total n####

#Basic model : Using all the variables
totn.m.full<-lm(totaln ~ area + midage + teenage + older + low_income + mid_income + high_income,data=ntadata)
summary(totn.m.full)

##Backward elimination: a backward stepwise regression
totn.backward.step <-stepAIC(totn.m.full, direction = "backward")
summary(totn.backward.step)

##Bidirectional Elimination
totn.both.step <- stepAIC(totn.m.full, direction = "both")
summary(totn.both.step)

##Forward selection: a forward stepwisre regression
totn.forward.step <- stepAIC(totn.m.full, direction = "forward")
summary(totn.forward.step)

#### final model ####
m.final<- totn.backward.step

#Assumption check
plot(m.final)

#Y transformation
boxcox(m.final)
boxcox(m.final, lambda=seq(0,1,0.01))
##As lambda = .2, ~ close to zero. Not going to transform Y.


####diff n: start n - end n #### which city has more starts?

#Basic model : Using all the variables
dfn.m.full<-lm(diffn ~ area + midage + teenage + older + low_income + mid_income + high_income,data=ntadata)
summary(dfn.m.full)

##Backward elimination: a backward stepwise regression
dfn.backward.step <-stepAIC(dfn.m.full, direction = "backward")
summary(dfn.backward.step)

##Bidirectional Elimination
dfn.both.step <- stepAIC(dfn.m.full, direction = "both")
summary(dfn.both.step)

##Forward selection: a forward stepwisre regression
#dfn.forward.step <- stepAIC(dfn.m.full, direction = "forward")
#summary(dfn.forward.step)


----Appendix (FYI)
####start n####

#Basic model : Using all the variables
stn.m.full<-lm(startn ~ area + midage + teenage + older + low_income + mid_income + high_income,data=ntadata)
summary(stn.m.full)

##Backward elimination: a backward stepwise regression
stn.backward.step <-stepAIC(stn.m.full, direction = "backward")
summary(stn.backward.step)

##Bidirectional Elimination
stn.both.step <- stepAIC(stn.m.full, direction = "both")
summary(stn.both.step)

##Forward selection: a forward stepwisre regression
#stn.forward.step <- stepAIC(stn.m.full, direction = "forward")
#summary(stn.forward.step)

####end n####

#Basic model : Using all the variables
edn.m.full<-lm(endn ~ area + midage + teenage + older + low_income + mid_income + high_income,data=ntadata)
summary(edn.m.full)

##Backward elimination: a backward stepwise regression
edn.backward.step <-stepAIC(edn.m.full, direction = "backward")
summary(edn.backward.step)

##Bidirectional Elimination
edn.both.step <- stepAIC(edn.m.full, direction = "both")
summary(edn.both.step)

##Forward selection: a forward stepwisre regression
#edn.forward.step <- stepAIC(edn.m.full, direction = "forward")
#summary(edn.forward.step)

##I excluded separate analyses for start and end because they show similar patterns
#summary(stn.both.step)
#summary(edn.both.step)
