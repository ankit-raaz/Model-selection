# install libcurl4-openssl-dev  (when tidyverse was not installing)
# install.packages("tidyverse")
# install.packages("devtools")
# devtools::install_github("tidyverse/tidyverse")
library(ecospace)
library(vegan)
library(tidyverse)
library(FD, quietly = TRUE)
library(randomForest)
library(rpart)
library(rfUtilities)

data("KWTraits")
nchar<-18
char.state<-c(2,7,3,3,2,2,5,5,2,5,2,2,5,2,5,5,3,3)

char.type<-c("numeric", "ord.num", "numeric", "numeric", "numeric", "numeric",
             "ord.num", "ord.num", "numeric", "ord.num", "numeric", "numeric", "ord.num",
             "numeric", "ord.num", "numeric", "numeric", "numeric")

char.names <- c("Reproduction", "Size", "Substrate composition", "Substrate
consistency", "Supported", "Attached", "Mobility", "Absolute tier", "Absolute
microhabitat", "Relative tier", "Relative microhabitat", "Absolute food
microhabitat", "Absolute food tier", "Relative food microhabitat", "Relative
food tier", "Feeding habit", "Diet", "Food condition")

state.names <- c("SEXL", "ASEX", "BVOL", "BIOT", "LITH", "FLUD", "HARD", "SOFT",
                 "INSB", "SPRT", "SSUP", "ATTD", "FRLV", "MOBL", "ABST", "AABS", "IABS", "RLST",
                 "AREL", "IREL", "FAAB", "FIAB", "FAST", "FARL", "FIRL", "FRST", "AMBT", "FILT",
                 "ATTF", "MASS", "RAPT", "AUTO", "MICR", "CARN", "INCP", "PART", "BULK")

ecospace<- create_ecospace(nchar,char.state,char.type,char.names,
                           state.names,constraint = 2,
                           weight.file = KWTraits)


Exp0.5<- expansion(Sseed=5, Smax=100, ecospace=ecospace, strength=0.5)
 # HERE IS THE PROBLEM 
#'[Here the output gives : Error in expansion(Sseed = 5, Smax = 100, ecospace = ecospace, strength = 0.5) : 
#'[unused arguments (Sseed = 5, Smax = 100, ecospace = ecospace, strength = 0.5)]
# 
Expansion1<-expansion(Sseed=5, Smax=100, ecospace=ecospace)  

# The expansion model is not loading here, other than expansion everything works fine.

#'[you can go ahead and can run the could below to get the results of randomForest

Neutral<- neutral(Sseed=5, Smax=100, ecospace=ecospace)
PartR0.5<- partitioning(Sseed = 5, Smax = 100, ecospace=ecospace, rule="relaxed", strength = 0.5 )
PartR1<-partitioning(Sseed = 5, Smax = 100, ecospace=ecospace, rule="relaxed", strength = 1 )
PartS0.5<-partitioning(Sseed = 5, Smax = 100, ecospace=ecospace, rule="strict", strength = 0.5 )
PartS1<-partitioning(Sseed = 5, Smax = 100, ecospace=ecospace, rule="strict", strength = 1 )
Redund0.5<-redundancy(Sseed=5, Smax = 100,ecospace=ecospace,strength = 0.5) 
Redund1<-redundancy(Sseed=5, Smax = 100,ecospace=ecospace,strength = 1)



# In the codes below will give warning that the you did not specify a parameter value. Param will be left empty.
#'[ Thats  not a problem because I have assigned the parameter already in the codes above it. ]
neut<- calc_metrics(samples = Neutral, Smax =100,
                    Model = "Neutral")

PR0.5<- calc_metrics(samples = PartR0.5, Smax =100,
                     Model = "Partitioning-0.5")
PR1<-calc_metrics(samples = Neutral, Smax =100,
                  Model = "partitioning")
PS0.5<- calc_metrics(samples = PartS0.5, Smax =100,
                     Model = "partitioning")
PS1<- calc_metrics(samples = PartS1, Smax =100,
                   Model = "partitioning")
Red0.5<- calc_metrics(samples = Redund0.5, Smax =100,
                      Model = "redundancy")
Red1<- calc_metrics(samples = Redund1, Smax =100,
                    Model = "redundancy")

stat_data<- rbind(neut,PR0.5,PR1,PS0.5,PS1,Red0.5,Red1)
stat_data<- subset( stat_data, select = -Param)
stat_data<- subset( stat_data, select = -S)
library(dplyr)
stat_data <- stat_data %>% relocate(Model, .after = qual.FRic)
str(stat_data)
stat_data$Model <- as.factor(stat_data$Model)
tree<-rpart(Model~., method="class", control = rpart.control(cp=0, minsplit = 1),
            data=stat_data)
?randomForest
par(mfrow=c(1,5), xpd=NA)
plot(tree)
text(tree,use.n = T)


set.seed(187)
rf.mod<-randomForest(Model~., ntree=50, mtry=1, data=stat_data)
plot(rf.mod)
rf.mod

varImpPlot(rf.mod)

