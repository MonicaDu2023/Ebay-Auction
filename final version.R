#clear workspace
rm(list=ls())

#print results
print_results=T

#prepare the package we need
library(readr)
library(tidyverse)
library(dummies)
library(Lahman)
library(modelr)
library(nnet)
library(fastDummies)
library(chron)
require("data.table") 
require("ggplot2")
require("knitr")
require("glmnet")
require("leaps")
library(randomForest)
library(cluster)
library(stargazer)
options(scipen=999)

# input data 
ebay <- read_csv("/Users/ante/Desktop/690\ final\ project/20090124_data/690.csv")

######################################################################
#initialize some useful values and add some variables we need in the later

# add id to to let operation, like merge data easier in the later
ebay$id <- 1:nrow(ebay)

# pick month up to know this auction happened what time in a year
ebay <- ebay %>%
  mutate(kaishi = substr(startdate,1,9),
         jieshu = substr(enddate,1,9))

ebay <- ebay %>%
  mutate(startd = as.Date(kaishi, "%b-%d-%y"),
         endd = as.Date(jieshu, "%b-%d-%y"))

ebay <- ebay %>%
  mutate(startmonth = substr(startd,6,7),
         endmonth = substr(endd,6,7))

# divide month to 4 seasons dummy
ebay$season1 <- 0
ebay$season1[(ebay$endmonth == '01')|(ebay$endmonth == '02')|(ebay$endmonth =='03')] <- 1
ebay$season2 <- 0
ebay$season2[(ebay$endmonth == '04')|(ebay$endmonth == '05')|(ebay$endmonth =='06')] <- 1
ebay$season3 <- 0
ebay$season3[(ebay$endmonth == '07')|(ebay$endmonth == '08')|(ebay$endmonth =='09')] <- 1
ebay$season4 <- 0
ebay$season4[(ebay$endmonth == '10')|(ebay$endmonth == '11')|(ebay$endmonth =='12')] <- 1

# get dummy variables that we need for model, trans and webpage
ebay <- cbind(ebay, as.data.frame(lm(season3 ~ model, data = ebay, x=TRUE)$x))
ebay <- select(ebay, -'(Intercept)')

ebay <- cbind(ebay, as.data.frame(lm(season3 ~ trans, data = ebay, x=TRUE)$x))
ebay <- select(ebay, -'(Intercept)')

ebay <- cbind(ebay, as.data.frame(lm(season3 ~ webpage, data = ebay, x=TRUE)$x))
ebay <- select(ebay, -'(Intercept)')

#Logarithm Value
ebay$biddy2=log(ebay$biddy2)  

# get rid of dependent linear problem between year and age
ebay <- ebay[,-c(which(colnames(ebay)=="year"))]

##########################################################################
# screen the information we need in the data set
#Dataset we will use 
data = ebay[,c(which(colnames(ebay)=="biddy2"),
               which(colnames(ebay)=="inspection"):
                 which(colnames(ebay)=="descriptionsize"),
               which(colnames(ebay)=="doors"),
               which(colnames(ebay)=="webpage"):
                 which(colnames(ebay)=="condition"),
               which(colnames(ebay)=="length"):
                 which(colnames(ebay)=="dealer"),
               which(colnames(ebay)=="season1"):
                 which(colnames(ebay)=="season3"),
               which(colnames(ebay)=="text"),
               which(colnames(ebay)=="webpageauction123"):
                 which(colnames(ebay)=="webpageebizautos")
               #which(colnames(ebay)=="model")
)]

data = data[,-c(which(colnames(data)=="condition"),
                which(colnames(data)=="relistflag"),
                which(colnames(data)=="reserve"),
                which(colnames(data)=="buyitnow"),
                which(colnames(data)=="title"),
                which(colnames(data)=="sell"),
                which(colnames(data)=="webpage")
)]

datamodel = ebay[,c(which(colnames(ebay)=="biddy2"),
                    which(colnames(ebay)=="inspection"):
                      which(colnames(ebay)=="descriptionsize"),
                    which(colnames(ebay)=="doors"),
                    which(colnames(ebay)=="webpage"):
                      which(colnames(ebay)=="condition"),
                    which(colnames(ebay)=="length"):
                      which(colnames(ebay)=="dealer"),
                    which(colnames(ebay)=="season1"):
                      which(colnames(ebay)=="season3"),
                    which(colnames(ebay)=="text"),
                    which(colnames(ebay)=="modelAccord"):
                      which(colnames(ebay)=="modelThunderbird"),
                    which(colnames(ebay)=="webpageauction123"):
                      which(colnames(ebay)=="webpageebizautos")
                    
)]

datamodel = datamodel[,-c(which(colnames(datamodel)=="condition"),
                          which(colnames(datamodel)=="relistflag"),
                          which(colnames(datamodel)=="reserve"),
                          which(colnames(datamodel)=="buyitnow"),
                          which(colnames(datamodel)=="title"),
                          which(colnames(datamodel)=="sell"),
                          which(colnames(datamodel)=="trans"),
                          which(colnames(datamodel)=="model"),
                          which(colnames(datamodel)=="webpage")
                          
)]

#the whole dataset
datafull = ebay[,c(which(colnames(ebay)=="biddy2"),
                   which(colnames(ebay)=="inspection"):
                     which(colnames(ebay)=="descriptionsize"),
                   which(colnames(ebay)=="doors"),
                   which(colnames(ebay)=="webpage"):
                     which(colnames(ebay)=="condition"),
                   which(colnames(ebay)=="length"):
                     which(colnames(ebay)=="dealer"),
                   which(colnames(ebay)=="season1"):
                     which(colnames(ebay)=="season3"),
                   which(colnames(ebay)=="text"),
                   which(colnames(ebay)=="modelAccord"):
                     which(colnames(ebay)=="modelThunderbird"),
                   which(colnames(ebay)=="ding_good"):
                     which(colnames(ebay)=="broken_pics"),
                   which(colnames(ebay)=="webpageauction123"):
                     which(colnames(ebay)=="webpageebizautos")
                   
)]

datafull = datafull[,-c(which(colnames(datafull)=="condition"),
                        which(colnames(datafull)=="relistflag"),
                        which(colnames(datafull)=="reserve"),
                        which(colnames(datafull)=="buyitnow"),
                        which(colnames(datafull)=="title"),
                        which(colnames(datafull)=="sell"),
                        which(colnames(datafull)=="trans"),
                        which(colnames(datafull)=="model"),
                        which(colnames(datafull)=="webpage")
                        
)]

#Compare Datasets we get from two front steps by do linear regression on each other seperately
z = summary(lm(data))

f = summary(lm(datamodel))

g = summary(lm(datafull))

stargazer(lm(data))

# compare R square for each other
z$r.squared

f$r.squared

g$r.squared

###########################################################################
#principle component analysis(unsupervise learning)

data1 = data[,c(which(colnames(data)=="miles"),
                which(colnames(data)=="featured"),
                which(colnames(data)=="numbids"),
                which(colnames(data)=="phone"),
                which(colnames(data)=="photos"),
                which(colnames(data)=="webpageebayhosting"),
                which(colnames(data)=="length"))]
#drop observations that contains NA
datapca= drop_na(data)

#drop varible we do prediction on and variable that might absorb other variable effect
datapca = datapca[,-c(which(colnames(datapca)=="biddy2"),
                      which(colnames(datapca)=="bookvalue"))]

#center the variables to have mean zero
pr.out =prcomp (datapca , scale =TRUE)

#contain the corresponding priciple component loading vector
pr.out$rotation

# plot two principle components
biplot (pr.out , scale =1, pc.biplot = FALSE)

#obtain variance explained by each principle component and compute proportion of it
pr.var =pr.out$sdev ^2
pve=pr.var/sum(pr.var )

#compute the cumulative sum
cumsum(pve)

#plot PVE explained by each component, as well as the cumulativ PVE
plot(pve , xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", ylim=c(0,1) ,type='b')
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="
     Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
     type='b')

####################################################################################
# subset selection 

#drop variable that might absorb other variable effect and observation that contain NA
datacv <- data[,-c(which(colnames(data)=="bookvalue"))] %>%
  drop_na()

# divide data to training data and test data randomly
set.seed (1)
train=sample (c(TRUE ,FALSE), nrow(datacv ),rep=TRUE)
test =(! train )

#perform backward stepwise selection to select best fit variables
fit.best=regsubsets(biddy2~.,data=datacv[train ,],
                    method = "backward", nvmax = 24)
plot(fit.best, scale = "adjr2")
summary(fit.best)

#make a model matrix from the test data
test.mat=model.matrix(biddy2~.,data=datacv[test ,])

#we extract the coefficients from the regfit.best and use them to form the predictions and compute test MSE
val.errors =rep(NA ,10)
for(i in 1:10){
  coefi=coef(fit.best ,id=i)
  pred=test.mat [,names(coefi)]%*% coefi
  val.errors [i]= mean((datacv$biddy2[test]-pred)^2)
}

# use smallest MSE to pick fittest model variables
which.min(val.errors)
coef(fit.best, 10)

# use the subset we get from the subset selection to do regression
set.seed(1)

train=sample (c(TRUE ,FALSE), nrow(datacv),rep=TRUE)
test =(! train )

lm2 <- (lm(biddy2~inspection+miles+featured+numbids+phone
           +photos+webpageebizautos+length+dealer, data = datacv[train,]))

# get mse
mse2 <- mse(lm2, datacv[test,])

#use the whole dataset after we handled to do regression
datar <- datacv

mod3 <- lm(biddy2~., data=datar[train,])

# get mse
mse3 <- mse(mod3, datacv[test,])

#summary regression and compare R square with each other
o = summary(mod3)
r = o$r.squared
q = summary(lm2)
h = q$r.squared
####################################################################

#Lasso and Ridge
d = cbind(data$biddy2,data1)

d = d[,-14]

d= d[complete.cases(d[,1]),]

#Drop NAs
d <- d %>%
  drop_na()


d_shuffle = d[sample(nrow(d)),]

#Using Cross Validation to pick the best tuning parameter
cv = function(lambda,ridge) {
  
  sum = 0
  for (k in 1:10) {
    
    test_indices = seq(floor(seq(1,nrow(d_shuffle),length.out = 11))[k],
                       floor(seq(1,nrow(d_shuffle),length.out = 11))[k+1],1)
    
    test_x = as.matrix(d_shuffle[test_indices,2:ncol(d_shuffle)])
    train_x = as.matrix(d_shuffle[-test_indices,2:ncol(d_shuffle)])
    
    test_y = d_shuffle[,1][test_indices]
    train_y = d_shuffle[,1][-test_indices]
    
    if (ridge) {
      fit = glmnet(train_x,train_y,alpha=0,lambda=lambda)
      sum = sum + sum((predict(fit,test_x,s=lambda) - test_y)^2)
    } else {
      fit = glmnet(train_x,train_y,alpha=1,lambda=lambda)
      sum = sum + sum((predict(fit,test_x,s=lambda) - test_y)^2)
    }
  }
  return(sum)
}

ridge_lambda = optimize(cv,c(0,1),ridge=T)$minimum #Optimal lambda for Ridge
lasso_lambda = optimize(cv,c(0,1),ridge=F)$minimum #Optimal lambda for Ridge
##########################################################

# divide data to training data and test data randomly
set.seed (1)
train=sample (c(TRUE ,FALSE), nrow(d),rep=TRUE)
test =(! train )

#Ridge regression
lambda = ridge_lambda
fit_r = glmnet(as.matrix(d[train,-1]), as.matrix(d[train,1]),alpha=0,lambda=lambda)
ridge_mse = mean((predict(fit_r,as.matrix(d[-train,-1]),s=lambda) - d[-train,1])^2)

#Lasso regression
lambda = lasso_lambda
fit_l = glmnet(as.matrix(d[train,-1]), as.matrix(d[train,1]),alpha=1,lambda=lambda)
lasso_mse = mean((predict(fit_l,as.matrix(d[-train,-1]),s=lambda) - d[-train,1])^2)

stargazer(lm2, fit_r)
############################################################

#clustering(unsurpervise learning)

#drop the observations with NA
ebaycar <- ebay %>%
  drop_na()
ebaycar$id <- 1:nrow(ebaycar)

# use car characterize to cluster
ebaycar <- ebaycar %>%
  select(inspection,miles,bookvalue,doors,age,biddy2,id)
ebaycar.cluster<- ebaycar[, 1:5]
ebay.cluster <- kmeans(ebaycar.cluster, centers = 5, iter.max = 20, nstart = 25)

#see variable we try to predict situation after we cluster
table(ebay.cluster$cluster, ebaycar$biddy2)

#aggregate cluster with original dataset
ebay.cluster$cluster <- as.factor(ebay.cluster$cluster)
aggregate(ebaycar, by = list(ebay.cluster$cluster),FUN = mean)

#repeat the process that using car characterize to cluster with different car variables
ebay.cluster2 <- ebaycar[,3]
ebay.cluster2 <- kmeans(ebay.cluster2, centers = 5, iter.max = 20, nstart = 25)
table(ebay.cluster2$cluster, ebaycar$biddy2)      
ebay.cluster2$cluster <- as.factor(ebay.cluster2$cluster)
aggregate(ebaycar, by = list(ebay.cluster2$cluster),FUN = mean)
ebaycar <- data.frame(ebaycar, ebay.cluster2$cluster)

# merge cluster id to ebay dataset and sort it
ebaygroup <- ebaycar %>%
  select(id, ebay.cluster2.cluster)
ebaytopart <-
  left_join(ebay,ebaygroup, by = "id" )
sortebaycar <- ebaycar[order(ebaytopart$ebay.cluster2.cluster),]

ebaytopart1 <- subset(sortebaycar,sortebaycar$ebay.cluster2.cluster == 1)
ebaytopart2 <- subset(sortebaycar,sortebaycar$ebay.cluster2.cluster == 2)
ebaytopart3 <- subset(sortebaycar,sortebaycar$ebay.cluster2.cluster == 3)
ebaytopart4 <- subset(sortebaycar,sortebaycar$ebay.cluster2.cluster == 4)
ebaytopart5 <- subset(sortebaycar,sortebaycar$ebay.cluster2.cluster == 5)
ebaytopart <- ebaytopart %>%
  drop_na()

#use for loop to do subset selection for each cluster sub group
for( i in levels(ebaytopart$ebay.cluster2.cluster)){
  subdata = subset(ebaytopart, ebaytopart$ebay.cluster2.cluster == i)
  
  # divide data to training data and test data randomly
  set.seed(2)
  train = sample(c(TRUE, FALSE), nrow(subdata), replace = TRUE)
  test = (!train)
  
  #perform backward stepwise selection to select best fit variables 
  fit.best.clu = regsubsets(biddy2 ~ text + inspection + featured + numbids + phone + pwrseller + photos
                            + season1 + season2 + season3 + season4 + addedinfo + descriptionsize +sellerage + webpage,
                            data = subdata[train, ], method = "backward", nvmax = 10)
  summary(fit.best.clu)
  #make a model matrix from the test data  
  test.mat = model.matrix(biddy2  ~ text + inspection + featured + numbids + phone + pwrseller + photos
                          + season1 + season2 + season3 + season4 + addedinfo + descriptionsize +sellerage + webpage,
                          data = subdata[test,])
  
  #we extract the coefficients from the regfit.best and use them to form the predictions and compute test MSE 
  val.error = rep(NA, 10)
  for(j in 1:10){
    coef = coef(fit.best.clu, id = j)
    pred = test.mat[,names(coef)]%*% coef
    val.error[j] = mean((subdata$biddy2[test] - pred) ^2)
  }
  
  # use smallest MSE to pick fittest model variables  
  print(which.min(val.error))
  m = which.min(val.error)
  summary(fit.best.clu)
  print(coef(fit.best.clu, m))
  stargazer(coef(fit.best.clu,m))
}

# do regression in subgroup with different best fit model variables we get seperately and compare each mse

#subgroup1
lm_c1 <- lm(biddy2 ~  photos, data = ebaytopart[ebaytopart$ebay.cluster2.cluster == 1,])
print(summary(lm_c1))
mse1 <- mse(lm_c1,  ebaytopart[ebaytopart$ebay.cluster2.cluster == 1,] )

#subgroup2
lm_c2 <- lm(biddy2 ~ photos + season1, 
            data = ebaytopart[ebaytopart$ebay.cluster2.cluster == 2,])
mse2 <- mse(lm_c2,ebaytopart[ebaytopart$ebay.cluster2.cluster == 2,])
print(summary(lm_c2))  

#subgroup3
lm_c3 <- lm(biddy2 ~ text + descriptionsize + inspection + featured + photos
            , data = ebaytopart[ebaytopart$ebay.cluster2.cluster == 3,])
mse3 <-mse(lm_c3,ebaytopart[ebaytopart$ebay.cluster2.cluster == 3,] )
print(summary(lm_c3))

#subgroup4
lm_c4 <- lm(biddy2 ~   featured + webpageauction123 + webpageebayhosting + inspection + phone + photos,
            data = ebaytopart[ebaytopart$ebay.cluster2.cluster == 4,])
print(summary(lm_c4))
mse4 <- mse(lm_c4,ebaytopart[ebaytopart$ebay.cluster2.cluster == 4,])

#subgroup5
lm_c5 <- lm(biddy2 ~ season1 + featured + numbids + phone + photos + inspection + 
              descriptionsize + webpageauction123 + webpagecarad + webpageebayhosting
            , data = ebaytopart[ebaytopart$ebay.cluster2.cluster == 5,])
print(summary(lm_c5))
mse5 <- mse(lm_c5,ebaytopart[ebaytopart$ebay.cluster2.cluster == 5,])

stargazer(lm_c1, lm_c2, lm_c3, lm_c4, lm_c5)

stargazer(mse1, mse2,mse3,mse4,mse5)

#########################################################################################

#random forest

set.seed(101)
dim(ebay)
ebay <- ebay %>%
  drop_na()
train = sample(1:nrow(ebay),9000)

#bagging part(predict with all variables)
ebay_forset1 <- randomForest(biddy2 ~ text + inspection + featured + numbids + phone + pwrseller + photos
                             + season1 + season2 + season3 + season4 + addedinfo + descriptionsize +sellerage
                             + bookvalue, data = ebay, subset = train)

#growing random dorest proceeds
ebay_forset2 <- randomForest(biddy2 ~ text + inspection + featured + numbids + phone + pwrseller + photos
                             + season1 + season2 + season3 + season4 + addedinfo + descriptionsize +sellerage
                             + bookvalue, data = ebay,mtry = 7, subset = train)

#plot tree and error relation
plot(ebay_forset1, log="y")
plot(ebay_forset2, log = "y")

#view importance of variables
importance <- importance(ebay_forset1)
View(ebay_forset1$importance)

# do predict using random forest with test data
pred_ebay <- predict(ebay_forset1, newdata = ebay[-train,])
pred_ebay2 <- predict(ebay_forset2, newdata = ebay[-train,])

# visualization the importance
varImportance <- data.frame(variables = row.names(importance), Importance = round(importance[,], 2))
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x= reorder(variables,Importance),
                           y = Importance, fill = Importance))+ geom_bar(stat = 'identity')+
  geom_text(aes(x = variables, y = 0.5, label = Rank),
            hjust = 0, vjust = 0.55, size = 4, colour = 'red')+
  labs(x = 'Variables') + coord_flip()

ebay.test <- ebay[-train,]
table(pred_ebay, ebay.test$biddy2)

#compare mse
mean((ebay.test$biddy2- pred_ebay)^2)
mean((ebay.test$biddy2 - pred_ebay2)^2)

################################################################################

#pick up the most commonly observed car model in the data
for(i in names(ebay)[99:115]){
  print(length(which(ebay[,i] == 1)))
}

ebayMustang <- subset(ebay, modelMustang == 1)
         
#do regression with other characterize with it to see what fit better
lmmodel <- lm(biddy2 ~  inspection + featured + numbids + phone + photos + miles 
              + length + webpageebizautos + dealer, data = ebayMustang)
summary(lmmodel)
stargazer(lmmodel)




