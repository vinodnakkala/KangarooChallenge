########## Travelers Case Competition
########## Second Submission
########## Nov 14, 2016

# TD Boost Model


library(tweedie)
library(statmod)
library(TDboost)
library(caret)
library(dplyr)
library(Matrix)
library(xgboost)


train = read.csv("C:/Users/HP/Desktop/Travelers Case Competition/Kangaroo_train.csv")
valid = read.csv("C:/Users/HP/Desktop/Travelers Case Competition/Kangaroo_valid.csv")
test = read.csv("C:/Users/HP/Desktop/Travelers Case Competition/Kangaroo_hold.csv")


train_valid = rbind(train, valid)

# Creating new variable exposure risk (exp_risk)
train_val2 = train_valid
test2 = test

# Combining certain levels of veh_body

train_val2$veh_body = as.character(train_val2$veh_body)

train_val2$veh_body = ifelse(train_val2$veh_body == "RDSTR","RARE",ifelse(train_val2$veh_body == "MCARA",
                                                                          "RARE", ifelse(train_val2$veh_body =="BUS","RARE", 
                                                                                         ifelse(train_val2$veh_body =="CONVT","RARE", train_val2$veh_body))))

train_val2$veh_body = as.factor(train_val2$veh_body)

test2$veh_body = as.character(test2$veh_body)
test2$veh_body = ifelse(test2$veh_body == "RDSTR","RARE",ifelse(test2$veh_body == "MCARA",
                                                                "RARE", ifelse(test2$veh_body =="BUS","RARE", 
                                                                               ifelse(test2$veh_body =="CONVT","RARE", test2$veh_body))))

test2$veh_body = as.factor(test2$veh_body)




# More feat enggg
# Mean loss by each veh_body type

meanclm_vehbody = aggregate(train_val2[,2], list(train_val2$veh_body), mean)

colnames(meanclm_vehbody) = c("veh_body", "meanclm_vehbody")
train_val2 = left_join(train_val2,meanclm_vehbody,by = 'veh_body')

#same with valid set
test2 = left_join(test2,meanclm_vehbody,by = 'veh_body')


# Mean loss by gender


meanclm_gender = aggregate(train_val2[,2], list(train_val2$gender), mean)

colnames(meanclm_gender) = c("gender", "meanclm_gender")
train_val2 = left_join(train_val2,meanclm_gender,by = 'gender')

#same with valid set
test2 = left_join(test2,meanclm_gender,by = 'gender')




train_val2$clm = NULL
test2$clm = NULL
train_val2$numclaims = NULL
test2$numclaims = NULL


# Model

train_val3 = train_val2
train_val3$id=NULL


train_val3$claimcst0 = ifelse(train_val3$claimcst0 >0, train_val3$claimcst0 + sd(train_val3$claimcst0), train_val3$claimcst0)


tdboost_fit <- TDboost(claimcst0 ~. , data=train_val3, distribution = list(name="EDM",alpha=1.5),cv.folds=5, n.trees=300, interaction.depth = 20)


best.iter <- TDboost.perf(tdboost_fit,method="cv")
summary(tdboost_fit,n.trees=1)   

# Prediction

View(test2)
test3 = test2[,2:10]

f.predict <- predict.TDboost(tdboost_fit, test3, best.iter,type=c("response","link"))

test3$predicted = f.predict
#View(train3)
summary(test3$predicted)

d = density(test3$predicted)
plot(d)

test3$claimcst0 = test2$claimcst0

sum(test3$predicted)

View(test3)

test3$id = test$id
## GINI calculation

source("./gini_calc.R")
#test3$split_ind = 'T'
#get.GINI(input=test3, py='predicted', y='claimcst0', filter='T', split_ind='split_ind')
submission = test3
submission = submission[,10:11]
View(submission)

submission$predicted = submission$claimcst0
submission$predicted = NULL
submission$claimcst0 = test3$predicted
submission$predicted1 = NULL

write.csv(submission,"Second_submission.csv",row.names = FALSE)



##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

library(tweedie)
library(statmod)
library(TDboost)
library(caret)
library(dplyr)
library(Matrix)
library(xgboost)
# Two Part Model

train = read.csv("C:/Users/HP/Desktop/Travelers Case Competition/Kangaroo_train.csv")
valid = read.csv("C:/Users/HP/Desktop/Travelers Case Competition/Kangaroo_valid.csv")
test = read.csv("C:/Users/HP/Desktop/Travelers Case Competition/Kangaroo_hold.csv")



train_valid = rbind(train, valid)

# Creating new variable exposure risk (exp_risk)
train_val2 = train_valid
test2 = test
train_val2$exp_risk= ifelse(train_val2$exposure > 0.75, 4, ifelse(train_val2$exposure > 0.5, 3,
                                                                  ifelse(train_val2$exposure > 0.25,2,1)))
test2$exp_risk= ifelse(test2$exposure > 0.75, 4, ifelse(test2$exposure > 0.5, 3,
                                                        ifelse(test2$exposure > 0.25,2,1)))

train_val2$exp_risk = ordered(train_val2$exp_risk)
test2$exp_risk = ordered(test2$exp_risk)


# Combining certain levels of veh_body

train_val2$veh_body = as.character(train_val2$veh_body)

train_val2$veh_body = ifelse(train_val2$veh_body == "RDSTR","RARE",ifelse(train_val2$veh_body == "MCARA",
                                                                          "RARE", ifelse(train_val2$veh_body =="BUS","RARE", 
                                                                                         ifelse(train_val2$veh_body =="CONVT","RARE", train_val2$veh_body))))

train_val2$veh_body = as.factor(train_val2$veh_body)

test2$veh_body = as.character(test2$veh_body)
test2$veh_body = ifelse(test2$veh_body == "RDSTR","RARE",ifelse(test2$veh_body == "MCARA",
                                                                "RARE", ifelse(test2$veh_body =="BUS","RARE", 
                                                                               ifelse(test2$veh_body =="CONVT","RARE", test2$veh_body))))

test2$veh_body = as.factor(test2$veh_body)
##############################################################
cat_encoding = function(data, catvar, target)
{
  outcomes <- data %>% group_by(as.factor(data[,catvar]), as.factor(data[,target])) %>% summarise (num_cust = n())
  colnames(outcomes) <- c(catvar,target,"Count")
  
  # Computing ratio of 1's to no of 1's+0's in our target variable by each level of variable veh_body
  outcomes2 = aggregate(outcomes[["Count"]], by=list(Category=outcomes[[catvar]]), FUN=sum)
  colnames(outcomes2) = c(catvar, 'no_of_zero_and_ones')
  outcomes3 = subset(outcomes, outcomes[,target] == 1)
  outcomes3 = left_join(outcomes3,outcomes2,by = catvar)
  outcomes3$ratio = outcomes3$Count/outcomes3$no_of_zero_and_ones
  outcomes3 = subset(outcomes3, select=c(catvar, "ratio"))
  
  #putting everything together
  data = left_join(data,outcomes3,by = catvar)
  
  return(data)
}

train_val2 = cat_encoding(train_val2, 'veh_body', 'clm')


# Mapping the same ratio values onto our valid set

temp = train_val2[,c('veh_body','ratio')]
temp = unique(temp)
test2= left_join(test2,temp,by = 'veh_body')
# Renaming the ratio computed

train_val2$veh_ratio = train_val2$ratio
test2$veh_ratio = test2$ratio
train_val2$ratio = NULL
test2$ratio = NULL


# Balancing dataset
testId = test2$id
test2$id = NULL
train_val2$id = NULL

#subsetting only rows with clm=0
train_zeros = subset(train_val2, train_val2[,'clm'] == 0)
nrow(train_ones)
#View(train_val_zeros)
train_ones = subset(train_val2, train_val2[,'clm'] == 1)


# Randomize our zeroes dataset
train_zeros <- train_zeros[sample(nrow(train_zeros)),]


sample_1 = train_zeros[1:3500,]
Bal_train = rbind(train_ones,sample_1)
Bal_train$claimcst0 = NULL
#nrow(Bal_train)

source("./LogisticRegression.R")

View(test2)
train_target = (Bal_train$clm)
Bal_train$clm = NULL
Bal_train$numclaims =NULL
test2$numclaims = NULL
test2$clm = NULL


model_lr <- LogisticRegression(Bal_train,train_target, test2, cv=5, metric="logloss")

#View(model_lr)
test_lr <- model_lr$test
test_lr <- data.frame("id"=testId, "predicted"=test_lr$pred_lr)

# Results

test_lr$Actuals = test$clm
test_lr$Temp = ifelse(test_lr$predicted > 0.33, 1,0)
attach(test_lr)
#View(test2)
confusionMatrix(Temp,Actuals)


# Preparing data for the linear model


test2$pred_clm = test_lr$Temp
test2$id = testId
#View(test)
test_lin = test2
# This will be our test set for our linear model

test_lin = subset(test_lin, test_lin[,'pred_clm'] == 1)

test_lin$clm = NULL

test_lin$pred_clm = NULL
testlin_id = test_lin$id

test_lin$id = NULL

# Training set for linear model

train_lin = train_val2
train_lin$claimcst0 = train_val2$claimcst0
train_lin = subset(train_lin, train_lin[,'clm'] == 1)
train_lin$clm = NULL
train_lin$numclaims = NULL


#View(train_lin)
#View(valid_lin1)

d= density(log(train_lin$claimcst0))
plot(d)
##############

##################### LINEAR MODEL ##########################

# XGBOOST MODEL

X_test = test_lin

param <- list(  objective           = "reg:linear", 
                booster             = "gblinear",
                eta                 = 0.002,   # learning rate - Number of Trees
                max_depth           = 7,      # maximum depth of a tree
                subsample           = .9,     # subsample ratio of the training instance
                colsample_bytree    = .87,    # subsample ratio of columns 
                min_child_weight    = 1,      # minimum sum of instance weight (defualt)
                scale_pos_weight    = 1       # helps convergance bc dataset is unbalanced
) 


X_train = as.data.frame(train_lin)
X_train$claimcst0 = log(X_train$claimcst0)

train_new <- sparse.model.matrix(X_train$claimcst0 ~ ., data = X_train)
dtrain <- xgb.DMatrix(data=train_new, label=X_train$claimcst0)
model_xgb <- xgb.train(params              = param, 
                       data                = dtrain, 
                       feval               = evalgini,
                       nrounds             = 50, 
                       verbose             = 1,
                       maximize            = FALSE
)

X_test$Target <- -1
testing <- sparse.model.matrix(Target ~ ., data = X_test)
preds <- predict(model_xgb, testing) 
preds = exp(preds)

summary(preds)
sum(preds)

#View(new_Valid)
test_lin$id = testlin_id
test_lin$clmAmt_preds = preds
test_lin = test_lin[,10:11]

new_Valid = left_join(test2,test_lin,by = 'id')

new_Valid$clmAmt_preds[is.na(new_Valid$clmAmt_preds)] <- 0


#View(new_Valid)


d= density(new_Valid$clmAmt_preds)
plot(d)



# Calculating GINI


source("./gini_calc.R")


#new_Valid$split_ind = 'T'
#View(valid_lin1)
#get.GINI(input=new_Valid, py='clmAmt_preds', y='claimcst0', filter='T', split_ind='split_ind')







