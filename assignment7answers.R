################################################
#
#     Text Mining for Economics & Finance
#
#            Assignment 7 answers
#
################################################

library(tidyverse)
library(quanteda)
library(randomForest)
library(glmnet)
library(xgboost)
library(rpart)
library(rpart.plot)
library(ranger)

source("kendall_acc.R")

cars=readRDS(file="data/cars.RDS") %>%
  mutate(log_miles=log(1+odometer),
         age=2021-year,
         price=round(price/1000))

cat_dat<-model.matrix(~.-1, # -1 removes the intercept
                     data=cars %>%
                       select(type,drive,transmission,
                              paint_color,size,
                              condition,fuel) %>%
                       mutate_all(~paste0("_",gsub("-","_",
                                                   gsub(" ","_",.,fixed=T),fixed=T))))

set.seed(02138)
train_split=sample(1:nrow(cars),80000)

cars_train<-cars[train_split,]
cars_test<-cars[-train_split,]

cat_train<-cat_dat[train_split,]
cat_test<-cat_dat[-train_split,]


cars_train_x<-cars_train %>% 
  select(log_miles,age) %>%
  cbind(cat_train) %>%
  as.matrix()

cars_test_x<-cars_test %>% 
  select(log_miles,age) %>%
  cbind(cat_test) %>%
  as.matrix()

cars_train_dat<-cbind(cars_train_x,
                      price=cars_train$price) %>%
  as.data.frame()

cars_test_dat<-cbind(cars_test_x,
                     price=cars_test$price) %>%
  as.data.frame()

##########################################
# LASSO Benchmark
##########################################


lasso_mod<-cv.glmnet(x=cars_train_x,y=cars_train$price)

plot(lasso_mod)

lasso_pred_test<-predict(lasso_mod,newx = cars_test_x)[,1]

kendall_acc(cars_test$price,
            lasso_pred_test)

politeness::modelPlot(lasso_mod,cars_train_x) +
  labs(y='Feature count average')



##########################################
##########################################
# classification tree
##########################################
##########################################

treemod<-rpart(price~.,cars_train_dat)

rpart.plot(treemod)

tree_pred_test<-predict(treemod,newdata=cars_test_dat)

kendall_acc(cars_test$price,
            tree_pred_test)

###################################################
###################################################
# Random Forest
###################################################
# 
# rf<-randomForest(price~.,cars_train,
#                  sampsize=10000, # observations to test
#                  mtry=5, # number of considered variables at each node
#                  ntree=100) # number of trees in forest



rf<-ranger(price~.,cars_train_dat,
           importance="impurity",
           num.trees=500) # number of trees in forest

importance(rf) %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  rename(importance=".") %>%
  mutate(variable=fct_reorder(variable,importance)) %>%
  ggplot(aes(x=variable,y=importance)) +
  geom_point() +
  theme_bw() +
  coord_flip()



rf_pred_test<-predict(rf,data=cars_test_dat)

kendall_acc(cars_test$price,
            rf_pred_test$predictions)

####################################
# xgboost
####################################

xgbMod <- xgboost(data = cars_train_x, 
                  label = cars_train$price, 
                  # max.depth = 4, 
                  # eta = .3, 
                  # nthread = 10, 
                    
                  nrounds = 1000, 
                  verbose=0)

xgb_pred_test<-predict(xgbMod, cars_test_x)

kendall_acc(cars_test$price,
            xgb_pred_test)


#######################################################
# Post-double-LASSO
#######################################################

# raw regression - lots of confounds
cars_train_dat %>%
  with(summary(lm(log_miles~age)))

pred_X<-cars_train_dat %>%
  select(-age,-log_miles) %>%
  as.matrix()

pred_D<-cars_train_dat$age

pred_Y<-cars_train$log_miles


# 1. Selection of predictors for Y
y.lasso <- cv.glmnet(x = pred_X, y = pred_Y)
coef.y.lasso <- coef(y.lasso, s = "lambda.1se")
coef.y.label <- rownames(coef.y.lasso)[as.vector(!(coef.y.lasso == 0))]
# 2. Selection of predictors for D
d.lasso <- cv.glmnet(x = pred_X, y = pred_D)
coef.d.lasso <- coef(d.lasso, s = "lambda.1se")
coef.d.label <- rownames(coef.d.lasso)[as.vector(!(coef.d.lasso == 0))]

# 3. Refit the model
coef.double.label <- union(coef.y.label, coef.d.label)
coef.double.label<-coef.double.label[coef.double.label!="(Intercept)"]

dat.double <- data.frame(Y = pred_Y, D = pred_D) %>%
  cbind(pred_X[,coef.double.label])
post.double.lasso <- lm("Y ~ .", data = dat.double)

summary(post.double.lasso)



