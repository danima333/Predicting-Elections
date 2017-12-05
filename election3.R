
election <- read.csv('cleanElections.csv', stringsAsFactors = F)

eNames <- read.csv('Enames.csv')
names(election)
eNames

#create 2016 subset for testing
sub16 <- election[,!names(election) %in% c('mfgEmp12', 'DrugFR12','Obese12','Pov12',
                                           'medInc12','incomePercent12','pop12','intMig12','RorD08','mfgEmp12', 'DrugFR08','Obese08','Pov08',
                                           'medInc08','incomePercent08','pop08','intMig08','empQuoMfg12','empQuoTtl2012',
                                           'EmpTtl2012','WageMfg2012','WageTtl2012','County.State','Dem12','Rep08','Rep12','Total08','Total12','emp08','civLab08',
                                           'emp12','domMig08','unemp08','enempR08','percentDem12','unempR08','Dem08','unemp12','domMig12','civLab12',
                                           'netMig12','netMig08','unempR12','popCHG08','popCHG12','percentDem08')]

names(sub16) <- names(eNames)
year <- rep('2016',nrow(sub16))
sub16 <- cbind(sub16,year)

# Convert variables to numeric
sub16[,10:73] <- sapply(sub16[,10:73], as.numeric)
sub16$DrugFR<- sub16[,"DrugFR"]/100
sub16$unempR<- sub16[,"unempR"]/100

## DEALING WITH NAs
# Option 1: Remove NAs
# sub16 <- na.omit(sub16)

# Option 2: Impute NA values with mean
if (!'imputeR' %in% installed.packages()){
  install.packages('imputeR')
}
library(imputeR)
sub16[sapply(sub16, is.numeric)] = guess(sub16[sapply(sub16, is.numeric)], type='mean')


#2012 subset
sub12 <- election[,!names(election) %in% c('mfgEmp16', 'DrugFR16','Obese16','Pov16',
                                           'medInc16','incomePercent16','pop16','intMig16','RorD16','mfgEmp16', 'DrugFR08','Obese08','Pov08',
                                           'medInc08','incomePercent08','pop08','intMig08','empQuoMfg16','empQuoTtl2016',
                                           'EmpTtl2016','WageMfg2016','WageTtl2016','County.State','Dem16','Rep08','Rep16','Total08','Total16','emp08','civLab08',
                                           'emp16','domMig08','unemp08','enempR08','percentDem16','unempR08','Dem08','unemp16','domMig16','civLab16',
                                           'netMig16','netMig08','unempR16','popCHG08','popCHG16','percentDem08')]


names(sub12) <- names(eNames)
year <- rep('2012',nrow(sub12))
sub12 <- cbind(sub12,year)

# Convert variables to numeric
sub12[,10:73] <- sapply(sub12[,10:73], as.numeric)
sub12$DrugFR<- sub12[,"DrugFR"]/100
sub12$unempR<- sub12[,"unempR"]/100

## DEALING WITH NAs
# Option 1: Remove NAs
# sub12 <- na.omit(sub12)

# Option 2: Impute NA values with mean
if (!'imputeR' %in% installed.packages()){
  install.packages('imputeR')
}
library(imputeR)
sub12[sapply(sub12, is.numeric)] = guess(sub12[sapply(sub12, is.numeric)], type='mean')


#Test/Validation with even RorD 
#install.packages('caTools')
library(caTools)
help(package = 'caTools')

Y = sub16[,'RorD'] # extract labels from the data
msk = sample.split(Y, SplitRatio=8/10)
table(Y,msk)
t=sum( msk)  # number of elements in one class
f=sum(!msk)  # number of elements in the other class

validation <- sub16[msk,]
test <- sub16[!msk,]

sum(validation$RorD)/nrow(validation)
sum(test$RorD)/nrow(test)

#outcome factor
sub16$outcome <- ifelse(sub16$RorD == 1,'Dem','Rep')
sub16$outcome <- as.factor(sub16$outcome)
sub12$outcome <-ifelse(sub12$RorD ==1,'Dem','Rep')
sub12$outcome <- as.factor(sub12$outcome)
validation$outcome <- ifelse(validation$RorD == 1,'Dem','Rep')
validation$outcome <-as.factor(validation$outcome)
test$outcome <- ifelse(test$RorD == 1,'Dem','Rep')
test$outcome <- as.factor(test$outcome)

#correlation, might want to remove some variables 
cor(subset(sub12,select = -c(fips, county, outcome, RorD,state,year,region)))
names(sub12)
#add population density
sub12$popDens <- sub12$pop/sub12$landArea
sub16$popDens <- sub16$pop/sub16$landArea
validation$popDens <- validation$pop/validation$landArea
test$popDens <- test$pop/test$landArea



#PCA
#install.packages('stats')
library(stats)
sub12sub <- subset(sub12,select = c(YdiscussOppose,Ydiscuss,	YCO2limits, YCO2limitsOppose,	YtrustclimsciSST,	YtrustclimsciSSTOppose,
                                    Yregulate,	YregulateOppose,	YsupportRPS,	YsupportRPSOppose,	Yfundrenewables,	YfundrenewablesOppose, Yhappening,
                                    YhappeningOppose,	Yhuman,	YhumanOppose,	Yconsensus,	YconsensusOppose,	Yworried,	YworriedOppose,	Ypersonal,
                                    YpersonalOppose,	YharmUS,	YharmUSOppose,	Ydevharm,	YdevharmOppose,	Yfuturegen,	YfuturegenOppose,
                                    Yharmplants,	YharmplantsOppose,	Ytiming,	YtimingOppose, RorD))

logsub <- log(sub12sub)
logsub$RorD <- sub12sub$RorD

pc.RorD <- sub12sub[,'RorD']
sub12.pca <- prcomp(sub12sub, center = TRUE, scale. = TRUE) 

summary(sub12.pca)

print(sub12.pca)

plot(sub12.pca, type = 'l')
trans$rotation
# library(devtools)
# install_github("ggbiplot", "vqv")
# 
# library(ggbiplot)
# g <- ggbiplot(sub12.pca, obs.scale = 1, var.scale = 1, 
#               groups = pc.RorD, ellipse = TRUE, 
#               circle = TRUE)
# g <- g + scale_color_discrete(name = '')
# g <- g + theme(legend.direction = 'horizontal', 
#                legend.position = 'top')
# print(g)

#install.packages('caret')
#install.packages('e1071')
require(caret)
require(e1071)
trans = preProcess(subset(logsub, select = -RorD), 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC = predict(trans,subset(logsub, select = -RorD))
summary(PC)
names(PC)

#random forest
library(randomForest)
set.seed(300)

forest.vote = randomForest(RorD~   DrugFR  + Obese + medInc + popDens  +  Ydiscuss +
                             civLab + unempR + pctBlack + pctWhite + pctAsian  +
                             pctHispanic
                           ,data=sub12)

CUTOFF = .5
pred.forest = as.integer(predict(forest.vote, prob = TRUE) > CUTOFF)
table(sub12$RorD,pred.forest)

importance2 <- importance(forest.vote)
importance2 <- importance2[order(-importance2),]

importance2

#predict valdiation 2016 forest
CUTOFF = .5
pred.forest16 <- as.integer(predict(forest.vote,sub16) > CUTOFF)

#2016 validation outcomes vs. predict
table(sub16$RorD,pred.forest16)
forest16 <- table(sub16$RorD,pred.forest16)

pred.forest16 <- as.numeric(pred.forest16)
#Accuracy
(forest16[1] + forest16[4])/(forest16[1]+forest16[2]+forest16[3] + forest16[4])

#boosting
library(gbm)
set.seed(300)

boost.vote <- gbm(RorD~  mfgEmp + medInc + intMig + domMig + civLab + EmpTtl + WageMfg + WageTtl + 
                    landArea + popDens + latitude + longitude + Ydiscuss + YCO2limits + Yfundrenewables +Yhappening +
                    Yhuman +	YharmplantsOppose + Ytiming + Yconsensus +	Yworried+	Ypersonal +
                    YharmUS, data=sub12, distribution = "bernoulli", n.trees=1000)

pred.vote <- predict(boost.vote,n.trees=1000,type="response")
pred.vote <- round(pred.vote,0)
table(sub12$RorD, pred.vote)
summary(boost.vote)
#predict 2016 boosting
pred.vote16 <- predict(boost.vote,validation,n.trees=1000,type="response")
pred.vote16 <- round(pred.vote16,0)
vote16 <- table(validation$RorD,pred.vote16)

#accuracy
(vote16[1] + vote16[4]) / (vote16[1] + vote16[2] + vote16[3] + vote16[4])

#majority class proportion 2016
1- sum(validation$RorD)/nrow(validation)
1- sum(sub12$RorD)/nrow(sub12)

#boosting 2: probability (for votes by state) 
library(gbm)
set.seed(300)


boost.vote2 <- gbm(RorD~ mfgEmp + medInc + intMig + domMig + civLab + EmpTtl + WageMfg + WageTtl + 
                     landArea + popDens + latitude + longitude + Ydiscuss + YCO2limits + Yfundrenewables +Yhappening +
                     Yhuman +	YharmplantsOppose + Ytiming + Yconsensus +	Yworried+	Ypersonal +
                     YharmUS,data=sub12,distribution = "bernoulli",n.trees=1000)

pred.vote2 <- predict(boost.vote2,n.trees=1000,type="response")
table(sub12$RorD,pred.vote2)
summary(boost.vote2)

#sub16 for ROC
CUTOFF <- .4
pred.voteRR <- as.integer(predict(boost.vote2,sub16, n.trees=1000,type="response")>CUTOFF)

table(sub16$RorD,pred.voteRR)
vote16 <-  table(sub16$RorD,pred.voteRR)
(vote16[1] + vote16[4]) / (vote16[1] + vote16[2] + vote16[3] + vote16[4])

#predict 2016 boosting (for votes by state)
pred.vote16.2 <- predict(boost.vote2,sub16,n.trees=1000,type="response")
vote16.2 <- table(sub16$RorD,pred.vote16.2)
sub16v <-cbind(sub16,pred.vote16.2)

pred <- as.integer(pred.vote16.2 > .5)
conf.16 <- table(sub16$RorD, pred)
(conf.16[1] + conf.16[4])/(conf.16[1]+conf.16[2]+conf.16[3]+conf.16[4])

#for random forest..
forest16prob <- predict(forest.vote,sub16)
sub16v <- cbind(sub16,forest16prob2)
predForest <- round(forest16prob,0)
#predForestV <- as.vector(predForest[,2])
conff.16 <- table(sub16$RorD, predForest)
(conff.16[1] + conff.16[4])/(conff.16[1]+conff.16[2]+conff.16[3]+conff.16[4])

# we can't use total votes because technically we shouldnt' know total votes. determine % of pop voted in 2008, 2012..
election[,8:116] <- sapply(election[8:116], as.numeric)
election$pct2008 <- election$Total08/election$pop08
election$pct2012 <- election$Total12/election$pop12
election$avgVote <- rowMeans(subset(election, select = c(pct2008, pct2012)), na.rm = TRUE)
election$vote2016 <- round(election$pop16 * election$avgVote,0)
(election$Total16 - election$vote2016)/election$Total16

sub16v <- merge(sub16v,election[,c('fips','vote2016')], by = c('fips'))

#votes per county

#for boosted
sub16v$Dvote <- round(sub16v$vote2016*pred.vote16.2,0)
sub16v$Rvote <- sub16v$vote2016 - sub16v$Dvote

#for forest
sub16v$Dvote <- round(sub16v$vote2016*forest16prob,0)
sub16v$Rvote <- sub16v$vote2016- sub16v$Dvote
#aggregate votes by state
library("data.table")
sub16v <- as.data.table(sub16v)
setkey(sub16v,state)
sub16A <- sub16v[,list(republicanP = sum(Rvote, na.rm = TRUE),republicanA = sum(rep, na.rm = TRUE),
                       democratP = sum(Dvote, na.rm = TRUE), democratA = sum(dem, na.rm = TRUE), Obese = mean(Obese), DrugFR = mean(DrugFR), mfgEmp = mean(mfgEmp)
                       ,Pov = mean(Pov), popDens = mean(popDens), intMig = mean(intMig), civLab = mean(civLab), Ydiscuss = mean(Ydiscuss), 
                       YCO2limits = mean(YCO2limits), Yfundrenewables = mean(Yfundrenewables), Yhappening = mean(Yhappening), Yhuman = mean(Yhuman), 
                       YharmplantsOppose = mean(YharmplantsOppose), Ytiming = mean(Ytiming), Yconsensus = mean(Yconsensus), Yworried = mean(Yworried)), by = 'state']

#if rep votes > than dem votes, 0 else 1
sub16A$RorDp <- ifelse(sub16A$republicanP > sub16A$democratP, 0 ,1)  
sub16A$RorDa <- ifelse(sub16A$republicanA > sub16A$democratA, 0 ,1)  

#predicted democratic states
sum(sub16A$RorDp)
#actual democratic states
sum(sub16A$RorDa)
#incorrect states
error16 <- sub16A[sub16A$RorDa != sub16A$RorDp,]
error16[,c('state','RorDp','RorDa')]
tail(sub16A)


electoralVotes <- read.csv('electoralVotes.csv')

electoralVotes <- electoralVotes[order(electoralVotes$stateAb),]
sub16A <- cbind(sub16A,electoralVotes$EV)

#election2016 <- write.csv(sub16A, file = '2016election.csv')

#actual vs. predicted electoral votes
sub16A$actualDemEVs <- sub16A$RorDa*sub16A$V2
sub16A$predictedDemEVs <- sub16A$RorDp*sub16A$V2
sub16A$actualRepEVs <-  sub16A$V2 - sub16A$actualDemEVs
sub16A$predictedRepEVs <- sub16A$V2 - sub16A$predictedDemEVs

##these are off by about 20? 
sum(sub16A$actualDemEVs)
sum(sub16A$actualRepEVs)

sum(sub16A$predictedDemEVs)
sum(sub16A$predictedRepEVs)

###############################################
# Regression Models

# Creating Binomial Variables Back:
sub12$outcomebinomial <- NA
for (i in 1:length(sub12$outcome)) {
  if (sub12$outcome[i] == "Rep") {
    sub12$outcomebinomial[i] <- 1
  } else {
    sub12$outcomebinomial[i] <- 0
  }
}
sub16$outcomebinomial <- NA
for (i in 1:length(sub16$outcome)) {
  if (sub16$outcome[i] == "Rep") {
    sub16$outcomebinomial[i] <- 1
  } else {
    sub16$outcomebinomial[i] <- 0
  }
}

# Logistic regression (all variables not a good model):
logit.vote = glm(outcomebinomial~latitude+longitude
                 +mfgEmp+DrugFR+Obese+Pov+medInc+incomePercent+pop+intMig
                 +domMig+netMig+civLab+unemp+college+pctBlack+pctAsian
                 +pctHispanic+pctWhite+pctForeign+EmpTtl+WageMfg+WageTtl
                 +popDens+landArea+Ydiscuss+YCO2limits+YtrustclimsciSST
                 +Yregulate+YsupportRPS+Yfundrenewables+Yhappening+Yhuman
                 +Yconsensus+Yworried+Ypersonal+YharmUS+Ydevharm+Yfuturegen
                 +Yharmplants+Ytiming+year, data=sub12, family=binomial)
summary(logit.vote)

#Building a better model by eliminating non-significant variables:
#Predicting at 84%
logit.vote2 = glm(outcomebinomial~latitude
                  +DrugFR+incomePercent+pop
                  +civLab+college+pctBlack
                  +pctHispanic+pctWhite
                  +landArea+Ydiscuss
                  +YsupportRPS+Yfundrenewables+Yhuman
                  +Yconsensus+Yworried+YharmUS+Yfuturegen
                  +Ytiming, data=sub12, family=binomial)
summary(logit.vote2)

test$outcomebinomial <- NA
for (i in 1:length(test$outcome)) {
  if (test$outcome[i] == "Rep") {
    test$outcomebinomial[i] <- 1
  } else {
    test$outcomebinomial[i] <- 0
  }
}
#Testing on test set 86.3%
pred.logit2 <- predict(logit.vote2, newdata=test, type="response")
fitted.results2 <- ifelse(pred.logit2 > 0.5, 1, 0)
misClassError2 <- mean(fitted.results2 != test$outcomebinomial, na.rm=TRUE)
print(paste('Accuracy',1-misClassError2))
# Testing on sub16: Accuracy 86.44%
pred.logit2 <- predict(logit.vote2, newdata=sub16, type="response")
fitted.results2 <- ifelse(pred.logit2 > 0.5, 1, 0)
misClassError2 <- mean(fitted.results2 != sub16$outcomebinomial, na.rm=TRUE)
print(paste('Accuracy',1-misClassError2))

#ROC Plot
library(ROCR)
p <- predict(logit.vote2, newdata=sub16, type="response")
pr <- prediction(p, sub16$outcomebinomial)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# AUC is best when closer to 1 than to 0.5
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
aucconfusionMatrix(fitted.results1, cutoff = 0.5)



#Logistic Regression Model 3

logit.vote3 = glm(outcomebinomial~  WageTtl + 
                    popDens + longitude + YCO2limits + Yhuman  + Yconsensus +	Ypersonal ,data=sub12, family=binomial)
summary(logit.vote3)
pred.logit3 <- predict(logit.vote3, newdata=test, type="response")
fitted.results3 <- ifelse(pred.logit3 > 0.5, 1, 0)
misClassError3 <- mean(fitted.results3 != test$outcomebinomial, na.rm=TRUE)
print(paste('Accuracy',1-misClassError3))

#Logistic 4: Predicting with 89.77 accuracy

logit.vote4 = glm(outcomebinomial~  landArea 
                  + Yhuman 
                  + Yconsensus +	Ypersonal
                  , data=sub12, family=binomial)
summary(logit.vote4)
# Testing model on test set: accuracy 80.0%
pred.logit4 <- predict(logit.vote4, newdata=test, type="response")
fitted.results4 <- ifelse(pred.logit4 > 0.5, 1, 0)
misClassError4 <- mean(fitted.results4 != test$outcomebinomial, na.rm=TRUE)
print(paste('Accuracy',1-misClassError4))

# Testing on sub16: Accuracy 87.49%
pred.logit4 <- predict(logit.vote4, newdata=sub16, type="response")
fitted.results4 <- ifelse(pred.logit4 > 0.5, 1, 0)
misClassError4 <- mean(fitted.results4 != sub16$outcomebinomial, na.rm=TRUE)
print(paste('Accuracy',1-misClassError4))

# ROC plot
#install.packages("ROCR")
library(ROCR)
p <- predict(logit.vote4, newdata=sub16, type="response")
pr <- prediction(p, sub16$outcomebinomial)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# AUC is best when closer to 1 than to 0.5
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
aucconfusionMatrix(fitted.results1, cutoff = 0.5)

#Normalizing the data, scaling all numerical variables:
sub12Numeric <- sapply(sub12, is.numeric)
sub12N <- sub12[,sub12Numeric]
sub12N <- as.data.frame(scale(sub12N[,1:83]))
sub12N <- as.data.frame(cbind(sub12N,sub12$outcomebinomial))

#Normalizing the data, scaling all numerical variables (sub12b)
sub12Numericb <- sapply(sub12b, is.numeric)
sub12Nb <- sub12[,sub12Numericb]
sub12Nb <- as.data.frame(scale(sub12Nb[,1:83]))
sub12Nb <- as.data.frame(cbind(sub12Nb,sub12b$outcomebinomial))

# Simple tree plot:
library(tree)

tree.elec= tree(outcomebinomial~latitude+longitude
                +mfgEmp+DrugFR+Obese+Pov+medInc+incomePercent+pop+intMig
                +domMig+netMig+civLab+unemp+college+pctBlack+pctAsian
                +pctHispanic+pctWhite+pctForeign+EmpTtl+WageMfg+WageTtl
                +popDens+landArea+Ydiscuss+YCO2limits+YtrustclimsciSST
                +Yregulate+YsupportRPS+Yfundrenewables+Yhappening+Yhuman
                +Yconsensus+Yworried+Ypersonal+YharmUS+Ydevharm+Yfuturegen
                +Yharmplants+Ytiming+year
                ,data=sub12)

summary(tree.elec)

# Plot and label the tree
plot(tree.elec)
text(tree.elec,pretty=0)

# Creating a Logistic regression with the variables from the tree:

logit.vote5 = glm(outcomebinomial~ Ypersonal+WageMfg+YtrustclimsciSST+pop+intMig
                  , data=sub12, family=binomial)
summary(logit.vote5)
# Testing on test set:
pred.logit5 <- predict(logit.vote5, newdata=test, type="response")
fitted.results5 <- ifelse(pred.logit5 > 0.5, 1, 0)
misClassError5 <- mean(fitted.results5 != test$outcomebinomial, na.rm=TRUE)
print(paste('Accuracy',1-misClassError5))
# Only 90.76% accuracy

# Testing on sub16: Accuracy 89.79%
pred.logit5 <- predict(logit.vote5, newdata=sub16, type="response")
fitted.results5 <- ifelse(pred.logit5 > 0.5, 1, 0)
misClassError5 <- mean(fitted.results5 != sub16$outcomebinomial, na.rm=TRUE)
print(paste('Accuracy',1-misClassError5))

# ROC plot
#install.packages("ROCR")
library(ROCR)
p <- predict(logit.vote5, newdata=sub16, type="response")
pr <- prediction(p, sub16$outcomebinomial)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

############################################

# Random Forest 1:
set.seed(300)
forest.vote4 = randomForest(outcome~DrugFR+Obese+Pov+medInc+incomePercent+pop+civLab+emp+unempR+
                              college+pctBlack+pctAsian+pctHispanic+pctWhite+pctForeign+WageMfg+popDens+landArea+
                              + Ydiscuss + YCO2limits + Yfundrenewables + Yhuman +	YharmplantsOppose + Ytiming + Yconsensus + 
                              Yworried + Ypersonal + YharmUS ,data=sub12)
summary(forest.vote4)
importance(forest.vote4)
pred.forest4=predict(forest.vote4)
table(sub12$outcome, pred.forest4)
# Accuracy:
(713+2148)/(713+178+104+2148)

# Testing on sub16:

pred.forest2=predict(forest.vote4, newdata = sub16)
table(sub16$outcome, pred.forest2)
# Accuracy:
(494+2260)/(494+2260+372+17)

summary(forest.vote4)

# Another random forest:
plot(forest.elec)
forest.elec= randomForest(outcome~latitude+longitude
                          +mfgEmp+DrugFR+Obese+Pov+medInc+incomePercent+pop+intMig
                          +domMig+netMig+civLab+unemp+college+pctBlack+pctAsian
                          +pctHispanic+pctWhite+pctForeign+EmpTtl+WageMfg+WageTtl
                          +popDens+landArea+Ydiscuss+YCO2limits+YtrustclimsciSST
                          +Yregulate+YsupportRPS+Yfundrenewables+Yhappening+Yhuman
                          +Yconsensus+Yworried+Ypersonal+YharmUS+Ydevharm+Yfuturegen
                          +Yharmplants+Ytiming+year
                          ,data=sub12)
summary(forest.elec)
importance(forest.elec)
# Testing on sub12:
pred.elec=predict(forest.elec)
table(sub12$outcome, pred.elec)
# Accuracy on sub12: 92.01%
(727+2165)/(727+2165+87+164)
# Testing on sub16:
pred.elec16=predict(forest.elec, newdata = sub16)
table(sub16$outcome, pred.elec16)
# Accuracy on sub16: 87.52%
(494+2257)/(494+2257+17+375)

# Random Forest with variables from classification tree:
forest.elec5 <- randomForest(outcome~ Ypersonal+WageMfg+YtrustclimsciSST+pop+intMig
                             , data=sub12)
# Testing on sub12:
pred.elec5=predict(forest.elec5)
table(sub12$outcome, pred.elec5)
# Accuracy on sub12: 86.82%
(639+2090)/(639+2090+252+162)
# Testing on sub16:
pred.elec165=predict(forest.elec5, newdata = sub16)
table(sub16$outcome, pred.elec165)
# Accuracy on sub16: 88.20%
(486+2286)/(486+2286+346+25)

#Random Forest with ROC plots:
forest.elecROC= randomForest(outcome~latitude+longitude
                             +mfgEmp+DrugFR+Obese+Pov+medInc+incomePercent+pop+intMig
                             +domMig+netMig+civLab+unemp+college+pctBlack+pctAsian
                             +pctHispanic+pctWhite+pctForeign+EmpTtl+WageMfg+WageTtl
                             +popDens+landArea+Ydiscuss+YCO2limits+YtrustclimsciSST
                             +Yregulate+YsupportRPS+Yfundrenewables+Yhappening+Yhuman
                             +Yconsensus+Yworried+Ypersonal+YharmUS+Ydevharm+Yfuturegen
                             +Yharmplants+Ytiming+year
                             ,data=sub12, mtry=2, ntree=1000,
                             keep.forest=TRUE, importance=TRUE,test=sub16)

elect.pr = predict(forest.elecROC,type="prob",newdata=sub16)
elect.pred <- prediction(elect.pr, sub16$outcome)





##################################################
# NEURAL NETS (Models 1,5,6)
library(neuralnet)

##### NEURAL NET 1 ####
  vote.net <- neuralnet(RorD~  popN + mfgEmpN + medIncN + intMigN + domMigN + civLabN + EmpTtlN + WageMfgN + WageTtlN + 
                          landAreaN + popDensN + latitudeN + longitudeN + Ydiscuss + YCO2limits + Yfundrenewables +Yhappening +
                          Yhuman +	YharmplantsOppose + Ytiming + Yconsensus +	Yworried +	Ypersonal +
                          YharmUS, data=sub12, hidden= 6)
  plot(vote.net)

  # EVALUATING predictions on VALIDATION
    pred <- compute(vote.net,validation[,c("popN", "mfgEmpN", "medIncN", "intMigN", "domMigN", "civLabN", "EmpTtlN",
                                         "WageMfgN", "WageTtlN", "landAreaN", "popDensN", "latitudeN",
                                         "longitudeN", "Ydiscuss", "YCO2limits", "Yfundrenewables",
                                         "Yhappening", "Yhuman", "YharmplantsOppose", "Ytiming",
                                         "Yconsensus",	"Yworried",	"Ypersonal", "YharmUS")])
    pred = data.frame(fips=1:nrow(validation), RorDa=validation$RorD, vote.prob = pred$net.result)
    pred = pred[order(pred$vote.prob,decreasing=TRUE),]
    # create another column in the data frame pred to store the predicted cancellation outcome
    pred$vote.nn = as.numeric(pred$vote.prob>0.6)
    # confusion matrix of the prediction
    votenn <- table(pred$RorD,pred$vote.nn)
    accuracynn <- (votenn[1] + votenn[4]) / (votenn[1] + votenn[2] + votenn[3] + votenn[4])
    accuracynn 
      # hidden = 6 --- Accuracy = .8716
      # hidden = 5 --- Accuracy = .8672
      # hidden = c(5,2) --- Accuracy = .8656 
      # hidden = c(9,3) --- Accuracy = .8644
      # hidden = c(3,2) --- Accuracy = .8592
      # hidden = c(10,3) --- Accuracy = .8592
      # hidden = 4 --- Accuracy = .8592
      # hidden = c(16,10,6,4,3,2) --- Accuracy = .8437
      # hidden = c(16,6,3,2) --- Accuracy = .8425

  # EVALUATING predictions on SUB16 
    pred <- compute(vote.net,sub16[,c("popN", "mfgEmpN", "medIncN", "intMigN", "domMigN", "civLabN", "EmpTtlN",
                                         "WageMfgN", "WageTtlN", "landAreaN", "popDensN", "latitudeN",
                                         "longitudeN", "Ydiscuss", "YCO2limits", "Yfundrenewables",
                                         "Yhappening", "Yhuman", "YharmplantsOppose", "Ytiming",
                                         "Yconsensus",	"Yworried",	"Ypersonal", "YharmUS")])
    pred = data.frame(fips=1:nrow(sub16), RorDa=sub16$RorD, vote.prob = pred$net.result)
    pred = pred[order(pred$vote.prob,decreasing=TRUE),]
    # create another column in the data frame pred to store the predicted cancellation outcome
    pred$vote.nn = as.numeric(pred$vote.prob>0.6)
    # confusion matrix of the prediction
    votenn <- table(pred$RorD,pred$vote.nn)
    accuracynn <- (votenn[1] + votenn[4]) / (votenn[1] + votenn[2] + votenn[3] + votenn[4])
    accuracynn #.8816


#### NEURAL NET 5 ####
  # train the neural network
  vote.net5 <- neuralnet(RorD~ DrugFR  + Obese + medInc + popDens  +  Ydiscuss +
                           civLab + unempR + pctBlack + pctWhite + pctAsian  +
                           pctHispanic, data=sub12, hidden=3)
  plot(vote.net5)

  # EVALUATING predictions on VALIDATION
  pred5 <- compute(vote.net5,validation[,c("DrugFR", "Obese", "medInc", "popDens", "Ydiscuss",
                                             "civLab", "unempR", "pctBlack", "pctWhite",  
                                           "pctAsian", "pctHispanic")])
    # evaluate predictions
    pred5 = data.frame(fips=1:nrow(validation), RorDa=validation$RorD, vote.prob = pred5$net.result)
    pred5 = pred5[order(pred5$vote.prob,decreasing=TRUE),]
    # create another column in the data frame pred to store the predicted cancellation outcome
    pred5$vote.nn = as.numeric(pred5$vote.prob>0.5)
    # confusion matrix of the prediction
    votenn5 <- table(pred5$RorD,pred5$vote.nn)
    accuracynn5 <- (votenn5[1] + votenn5[4]) / (votenn5[1] + votenn5[2] + votenn5[3] + votenn5[4])
    accuracynn5 
    # hidden = 3 --- Accuracy = .9125

  # EVALUATING predictions on SUB16
    pred5 <- compute(vote.net5,sub16[,c("DrugFR", "Obese", "medInc", "popDens", "Ydiscuss",
                                           "civLab", "unempR", "pctBlack", "pctWhite",  
                                           "pctAsian", "pctHispanic")])
    pred5 = data.frame(fips=1:nrow(sub16), RorDa=sub16$RorD, vote.prob = pred5$net.result)
    pred5 = pred5[order(pred5$vote.prob,decreasing=TRUE),]
    # create another column in the data frame pred to store the predicted cancellation outcome
    pred5$vote.nn = as.numeric(pred5$vote.prob>0.5)
    # confusion matrix of the prediction
    votenn5 <- table(pred5$RorD,pred5$vote.nn)
    accuracynn5 <- (votenn5[1] + votenn5[4]) / (votenn5[1] + votenn5[2] + votenn5[3] + votenn5[4])
    accuracynn5 
    # Accuracy = .9154


#### NEURAL NET 6 #### 
  # train the neural network
  vote.net6 <- neuralnet(RorD~ Ypersonal+WageMfg+YtrustclimsciSST+pop+intMig, data=sub12, hidden=3)
  # EVALUATING predictions on VALIDATION
    pred6 <- compute(vote.net6,validation[,c("Ypersonal","WageMfg","YtrustclimsciSST","pop","intMig")])
    # evaluate predictions
    pred6 = data.frame(fips=1:nrow(validation), RorDa=validation$RorD, vote.prob = pred6$net.result)
    pred6 = pred6[order(pred6$vote.prob,decreasing=TRUE),]
    # create another column in the data frame pred to store the predicted cancellation outcome
    pred6$vote.nn = as.numeric(pred6$vote.prob>0.5)
    # confusion matrix of the prediction
    votenn6 <- table(pred6$RorD,pred6$vote.nn)
    accuracynn6 <- (votenn6[1] + votenn6[4]) / (votenn6[1] + votenn6[2] + votenn6[3] + votenn6[4])
    accuracynn6 
    # hidden = 3 --- Accuracy = .9077
    plot(vote.net6)
 
  # EVALUATING predictions on SUB16
    pred6 <- compute(vote.net6,sub16[,c("Ypersonal","WageMfg","YtrustclimsciSST","pop","intMig")])
    pred6 = data.frame(fips=1:nrow(sub16), RorDa=sub16$RorD, vote.prob = pred6$net.result)
    pred6 = pred6[order(pred6$vote.prob,decreasing=TRUE),]
    # create another column in the data frame pred to store the predicted cancellation outcome
    pred6$vote.nn = as.numeric(pred6$vote.prob>0.5)
    # confusion matrix of the prediction
    votenn6 <- table(pred6$RorD,pred6$vote.nn)
    accuracynn6 <- (votenn6[1] + votenn6[4]) / (votenn6[1] + votenn6[2] + votenn6[3] + votenn6[4])
    accuracynn6 
    # Accuracy = .9039
  

##################################################
#ROC curve and AUC calcs 

#install.packages('pROC')
library(pROC)
#forest.vote / pred.forest16 (CUTOFF = .5) (accuracy = .95 )  AUCsub16 = .897
#forest.vote / pred.forest16 (CUTOFF = .3) (accuracy = .88 )  AUCsub16 = .913
#boost.vote / pred.voteR (CUTOFF = .5) (accuracy = .936) AUCsub16 = .91084
#boost.vote / pred.voteR (CUTOFF = .42) (accuracy = .9 )  AUCsub16 = .924
#boost.vote2 / pred.voteRR (CUTOFF = .5) (accuracy = .9)  AUCsub16 = .901
#boost.vote2 / pred.voteRR (CUTOFF = .4) (accuracy = .867)  AUCsub16 = .892

roc_obj <- roc(sub16$RorD, pred.voteR) ## change these values for each model
auc(roc_obj)

roc_df <- data.frame(
  TPR=rev(roc_obj$sensitivities), 
  FPR=rev(1 - roc_obj$specificities))

rectangle <- function(x, y, width, height, density=12, angle=-45, ...)
  polygon(c(x,x,x+width,x+width), c(y,y+height,y+height,y), 
          density=density, angle=angle, ...)

roc_df <- transform(roc_df, 
                    dFPR = c(diff(FPR), 0),
                    dTPR = c(diff(TPR), 0))

plot(0:10/10, 0:10/10, type='n', xlab="FPR", ylab="TPR")
abline(h=0:10/10, col="lightblue")
abline(v=0:10/10, col="lightblue")

with(roc_df, {
  mapply(rectangle, x=FPR, y=0,   
         width=dFPR, height=TPR, col="green", lwd=2)
  mapply(rectangle, x=FPR, y=TPR, 
         width=dFPR, height=dTPR, col="blue", lwd=2)
  
  lines(FPR, TPR, type='b', lwd=3, col="red")
})



simple_auc <- function(TPR, FPR){
  # inputs already sorted, best scores first 
  dFPR <- c(diff(FPR), 0)
  dTPR <- c(diff(TPR), 0)
  sum(TPR * dFPR) + sum(dTPR * dFPR)/2
}

with(roc_df, simple_auc(TPR, FPR))

#AUC
rank_comparison_auc <- function(labels, scores, plot_image=TRUE, ...){
  score_order <- order(scores, decreasing=TRUE)
  labels <- as.logical(labels[score_order])
  scores <- scores[score_order]
  pos_scores <- scores[labels]
  neg_scores <- scores[!labels]
  n_pos <- sum(labels)
  n_neg <- sum(!labels)
  M <- outer(sum(labels):1, 1:sum(!labels), 
             function(i, j) (1 + sign(pos_scores[i] - neg_scores[j]))/2)
  
  AUC <- mean (M)
  if (plot_image){
    image(t(M[nrow(M):1,]), ...)
    library(pROC)
    with( roc(labels, scores),
          lines((1 + 1/n_neg)*((1 - specificities) - 0.5/n_neg), 
                (1 + 1/n_pos)*sensitivities - 0.5/n_pos, 
                col="blue", lwd=2, type='b'))
    text(0.5, 0.5, sprintf("AUC = %0.4f", AUC))
  }
  
  return(AUC)
}
rank_comparison_auc(labels=as.logical(sub16$RorD), scores=pred.voteR) ## change these values for each model 

###########################################################
##KNN
# remove all the missing values
sub12.omit=na.omit(sub12)
sub16.omit=na.omit(sub16)

# remove all the non-numeric columns
sub12.omit$county<-NULL
sub12.omit$state<-NULL
sub12.omit$region<-NULL
sub12.omit$year<-NULL
sub16.omit$state<-NULL
sub16.omit$county<-NULL
sub16.omit$region<-NULL
sub16.omit$year<-NULL

# rename columns
elec.train = sub12.omit
elec.test = sub16.omit

#x<-NA
#for(i in 1:length(elec.train)) {
#x[i]<-sum(is.na(elec.train[i]))
#  }
#x
#sum(sapply(elec.test,is.numeric))

# find k value which maximize accuracy
vk = seq(1,51,2)
accuracy = vk
for (i in 1:length(vk)){
  election.knn = knn(scale(elec.train[,sapply(elec.train,is.numeric)]),scale(elec.test[,sapply(elec.test,is.numeric)]),elec.train$outcome,k=vk[i])
  accuracy[i] = mean(elec.test$outcome==election.knn)  
}
plot(vk,accuracy,xlab='k',ylab='test accuracy',col='blue')
accuracy
max(accuracy)
# k=37

# knn function
election.knn = knn(scale(elec.train[,sapply(elec.train,is.numeric)]),scale(elec.test[,sapply(elec.test,is.numeric)]),elec.train$outcome,k=37)
election.knn
table(elec.test$outcome,election.knn)
##accuracy
(480+2426)/(480+9+201+2426)
##93.26%
