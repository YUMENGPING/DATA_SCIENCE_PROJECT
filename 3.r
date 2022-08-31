

library(dplyr)
library(ggplot2)


# 1 load data

dvc <- read.csv("dvc-table-breakdown_dash.csv",fileEncoding="UTF-8-BOM")
mc <- read.csv("mc-table-breakdown_dash.csv",fileEncoding = "UTF-8-BOM")
analysis <- read.csv("analysis_monthly_eligibility_figures_cho.csv",fileEncoding = "UTF-8-BOM")
number <- read.csv("number-and-cost-of-special-items-of-service-claims-by-cho-views-analysis_number_and_cost_of_spec.csv",
                   fileEncoding = "UTF-8-BOM")
dvc$Age.Classification <- dvc$Age.Classification + mc$Age.Classification


# 2 data processing


res <-  dvc[,-c(1,5)] %>% left_join(analysis[,-1],by=c("CHO_DESC","LHO_DESC_CHO")) %>% 
  left_join(number[,-1],by=c("CHO_DESC","LHO_DESC_CHO"="LHO_DESC")) %>% na.omit(.) %>% 
  select(c(LHO_DESC_CHO,CLIENT_NORM_AGE_GROUP,GENDER,Age.Classification,FAMILY_UNITS,SERVICE_CODE,SERVICE_DESC)) 


res$LHO_DESC_CHO <- as.factor(res$LHO_DESC_CHO)
res$ CLIENT_NORM_AGE_GROUP <- as.factor(res$CLIENT_NORM_AGE_GROUP)
res$GENDER <- as.factor(res$GENDER)
res$SERVICE_CODE <- as.factor(res$SERVICE_CODE)
res$SERVICE_DESC <- as.factor(res$SERVICE_DESC)

head(res)




ggplot(res,aes(LHO_DESC_CHO,FAMILY_UNITS))+geom_boxplot()



#For different LHO_DESC_CHO, family units is a fixed value, so the family units variable is removed from the model.



ggplot(res,aes(LHO_DESC_CHO,Age.Classification))+geom_boxplot()


#It can be seen from the boxplot that when LHO_DESC_CHO is Donegal, the median, maximum and minimum values of Age.classification are higher than another LHO_DESC_CHO type.


# 3 divide the training set and the test set to train the model


idx <- sample(1:nrow(res),nrow(res)*0.8,replace = F)

train <- res[idx,]

test <- res[-idx,]

model <- glm(LHO_DESC_CHO ~ GENDER + Age.Classification  + 
               SERVICE_DESC + CLIENT_NORM_AGE_GROUP + SERVICE_CODE ,
             data = train,family=binomial(link = "logit"))





m<-summary(model)$coef

m
#The coefficient of Age.Classification is 0.42, which means that the odds of LHO_DESC_CHO equal to Cavan increases by e^0.42=152.19% for each additional unit of Age.Classification.



write.csv(m,"result.csv")






#4 模型测试集预测即混淆矩阵


library(caret)
test_pred_y <- predict(model,newdata = test,type = "response")

pred_class <- as.factor(ifelse(test_pred_y>=0.5,"Donegal","Cavan / Monaghan"))

confusionMatrix(pred_class, test$LHO_DESC_CHO)



#The accuracy of the model is 98.76%.

# 5 Model ability score, roc curve

library(ggplot2)
library(pROC)

#define object to plot and calculate AUC
rocobj <- roc(test$LHO_DESC_CHO, test_pred_y)
auc <- round(auc(test$LHO_DESC_CHO, test_pred_y),4)

#create ROC plot
ggroc(rocobj, colour = 'steelblue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))


#The auc of the model is 0.9935.