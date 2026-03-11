################################################
#
# Machine Learning & Text Analysis for Social Science
#
#              Assignment 7
#
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
library(SHAPforxgboost)

source("kendall_acc.R")

rests<-readRDS("data/businessset.RDS") %>%
  mutate(price=as.numeric(RestaurantsPriceRange2)) %>%
  filter(!is.na(price) & price<3)

cats=rests %>%
  pull(categories) %>%
  tolower() %>%
  str_replace_all("-","_") %>%
  str_replace_all(" & ","_") %>%
  str_replace_all(", ",",") %>%
  str_replace_all(" ,",",") %>%
  str_replace_all(" ","_") %>%
  str_replace_all(","," ") %>%
  tokens(remove_punct = T) %>%
  dfm() %>%
  dfm_trim(min_docfreq = .05,docfreq_type="prop") %>%
  as.matrix()

cleaner<-function(text){
  text=ifelse(is.na(text),"NA",text)
  text=gsub("u'","",text,fixed=T)
  text=gsub("'","",text,fixed=T)
  text=paste0("_",text)
  return(text)
}

catvars<-c("NoiseLevel","RestaurantsAttire","RestaurantsTakeOut",
           "HasTV","OutdoorSeating","Caters","RestaurantsReservations",
           "RestaurantsDelivery","GoodForKids")
pred_dat<-rests %>%
  select(price) %>%
  cbind(cats) %>%
  cbind(model.matrix(~.-1, # -1 removes the intercept
                     data=rests %>%
                       select(catvars) %>%
                       mutate_all(cleaner)))


set.seed(02138)
train_split=sample(1:nrow(pred_dat),20000)

rests_train<-pred_dat[train_split,]
rests_test<-pred_dat[-train_split,]


rests_train_x<-rests_train %>%
  select(-price) %>%
  as.matrix() %>%
  apply(1:2,as.numeric)

rests_test_x<-rests_test %>%
  select(-price) %>%
  as.matrix() %>%
  apply(1:2,as.numeric)


##########################################
# LASSO Benchmark
##########################################


lasso_mod<-cv.glmnet(x=rests_train_x,y=rests_train$price)

plot(lasso_mod)

lasso_pred_test<-predict(lasso_mod,newx = rests_test_x)[,1]

kendall_acc(rests_test$price,
            lasso_pred_test)

politeness::modelPlot(lasso_mod,rests_train_x) +
  labs(y='Feature count average')


# Ablation test - does "american" matter?

lasso_mod<-cv.glmnet(x=rests_train_x[,-4],y=rests_train$price)

plot(lasso_mod)

lasso_pred_test<-predict(lasso_mod,newx = rests_test_x[,-4])[,1]

kendall_acc(rests_test$price,
            lasso_pred_test)



# Ablation test - does "fast food" matter?

lasso_mod<-cv.glmnet(x=rests_train_x[,-17],y=rests_train$price)

plot(lasso_mod)

lasso_pred_test<-predict(lasso_mod,newx = rests_test_x[,-17])[,1]

kendall_acc(rests_test$price,
            lasso_pred_test)




# Ablation test - do reservations matter?
rests_train_nocats_x<-rests_train_x[,-(43:45)]
rests_test_nocats_x<-rests_test_x[,-(43:45)]


lasso_nocats_mod<-cv.glmnet(x=rests_train_nocats_x,y=rests_train$price)

plot(lasso_nocats_mod)

lasso_pred_nocats_test<-predict(lasso_nocats_mod,newx = rests_test_nocats_x)[,1]

kendall_acc(rests_test$price,
            lasso_pred_nocats_test)

##########################################
##########################################
# classification tree
##########################################
##########################################
treemod<-rpart(price~.,rests_train)

plot(treemod, margin = 0.2)
text(treemod, use.n = TRUE, cex = 0.8)

rpart.plot(treemod)

tree_pred_test<-predict(treemod,newdata=rests_test)

kendall_acc(rests_test$price,
            tree_pred_test)

###################################################
###################################################
# Random Forests
###################################################
# 
# rf<-randomForest(price~.,rests_train,
#                  sampsize=10000, # observations to test
#                  mtry=5, # number of considered variables at each node
#                  ntree=100) # number of trees in forest


rf<-ranger(price~.,rests_train,
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



rf_pred_test<-predict(rf,data=rests_test)

kendall_acc(rests_test$price,
            rf_pred_test$predictions)

####################################
# xgboost
####################################

xgbMod <- xgboost(data = rests_train_x, 
                  label = rests_train$price, 
                    
                  
                  # max.depth = 4, 
                  # eta = .3, 
                  # nthread = 10, 
                  nrounds = 1000, 
                  verbose=0)

xgb_pred_test<-predict(xgbMod, rests_test_x)

kendall_acc(rests_test$price,
            xgb_pred_test)

# Setting for boosting a linear model
# xgbMod <- xgboost(data = rests_train_x[,1:20], 
#                   label = rests_train$price, 
#                   booster="gblinear",
#                   nrounds = 1000, 
#                   verbose=0)

#######################################################
# SHAP for xgboost
#######################################################
# 
# Doesn't work any more, not sure why.....
# 
# xgbMod <- xgboost(data = rests_train_x[,1:10], 
#                   label = rests_train$price, 
#                   # max.depth = 4, 
#                   # eta = .3, 
#                   # nthread = 10, 
#                   nrounds = 1000, 
#                   verbose=0)
# 
# # get the values
# shap_values <- shap.values(xgb_model = xgbMod,
#                            X_train = rests_train_x[,1:10])
# shap_values$mean_shap_score
# 
# # beeswarm plots
# shap.plot.summary.wrap1(xgbMod, X = rests_train_x[,1:20])
# 
# shap_long <- shap.prep(xgb_model = xgbMod, 
#                        X_train = rests_train_x[,1:20])
# 
# # deeper plot for single feature - more useful for continuous variables
# shap.plot.dependence(data_long = shap_long, x = "nightlife") 

#######################################################
# Post-double-LASSO
#######################################################

# raw regression - lots of confounds
pred_dat %>%
  with(summary(lm(price~nightlife)))

pred_X<-pred_dat %>%
  select(-nightlife,-price) %>%
  as.matrix()

pred_D<-pred_dat$nightlife

pred_Y<-pred_dat$price


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



