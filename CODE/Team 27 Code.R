rm(list = ls())

# Read the data
med <- read.table(file = "C://Users//nehas//Documents//GT 2023//ISYE 7406 DSML//Project//archive//medicalmalpractice.csv", sep=",", header=TRUE)
summary(med)

# check for missing values
colSums(is.na(med))
# there are no missing values 

## convert marital status and private attorney to character to match dataset description 
library('stringr')
med$Marital.Status <- as.factor(med$Marital.Status)
med$Marital.Status <- str_replace_all(med$Marital.Status,'0','Divorced')
med$Marital.Status <- str_replace_all(med$Marital.Status,'1','Single')
med$Marital.Status <- str_replace_all(med$Marital.Status,'2','Married')
med$Marital.Status <- str_replace_all(med$Marital.Status,'3','Widowed')
med$Marital.Status <- str_replace_all(med$Marital.Status,'4','Unknown')
med$Private.Attorney <- as.factor(med$Private.Attorney)
med$Private.Attorney <- str_replace_all(med$Private.Attorney,'1','Yes')
med$Private.Attorney <- str_replace_all(med$Private.Attorney,'0','No')

#however there are several unknowns under marital status and Insurance type
library(dplyr)
med %>%
  summarise_all(list(~sum(. == "Unknown")))
#since our dataset is large enough, we will remove all rows with Unknown as it will not be useful in predicting claim amount
med <- med[med$Marital.Status != "Unknown" & med$Insurance != "Unknown", ]

## take random sample of dataset 20% since its too large of a dataset
library(caret)
smp_size <- floor(0.2 * nrow(med))
set.seed(123)
smp <- sample(seq_len(nrow(med)), size = smp_size)
smp_med <- med[smp, ]

## EDA
#univariate analysis 
library(tidyverse)
summary(smp_med$Amount)
#Amount Distribution
ggplot(smp_med,aes(x = smp_med$Amount)) + geom_histogram(aes(y = after_stat(count / sum(count))), color="black", fill="light blue", binwidth = 25000) + 
  labs(title = "Claim Amount Distribution", y="Percentage", x ="Claim Amount ($)") +
  stat_bin(
    binwidth = 25000, geom = "text", color = "black",
    aes(y = after_stat(count / sum(count)), 
        label = scales::percent(after_stat(count / sum(count))), angle=90),
    vjust = 1, size=2.5, hjust=0
  )+
  scale_y_continuous(labels = scales::percent)

#distribution of age 
hist(smp_med$Age, col= "blue",breaks = 15, xlab = "Age (Years)", ylab = "Count", main= " Distribution of Age")

#Severity
ggplot(smp_med) +
  geom_bar(aes(x = Severity),fill="lavender",color="black", binwidth = 1) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  xlab("Severity")
table(smp_med$Severity)

#Private Attorney 
ggplot(smp_med) +
  geom_bar(aes(x = Private.Attorney), fill = "lavender", color="black") +
  xlab("Private Attorney") + ylab("Count")
table(smp_med$Private.Attorney)

#Marital Status
ggplot(smp_med) +
  geom_bar(aes(x = Marital.Status), fill = "lavender", color="black") +
  xlab("Marital Status") + ylab("Count")
table(smp_med$Marital.Status)

#Specialty
ggplot(smp_med) +
  geom_bar(aes(x = Specialty), fill = "lavender", color="black") +
  xlab("Physician Specialty") + ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
table(smp_med$Specialty)

#Insurance 
ggplot(smp_med) +
  geom_bar(aes(x = Insurance), fill = "lavender", color="black") +
  xlab("Insurance Type") + ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
table(smp_med$Insurance)

#Gender 
ggplot(smp_med) +
  geom_bar(aes(x = Gender), fill = "lavender", color="black") +
  xlab("Gender") + ylab("Count") 
table(smp_med$Gender)

#bivariate analysis
#claim amount vs age 
smp_med$Age_Group <- cut(smp_med$Age, breaks = seq(0, 90, by = 5), 
                         labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                                    "30-34", "35-39","40-44","45-49","50-54","55-59",
                                    "60-64", "65-69","70-74","75-79","80-84","85-89"), 
                         right = FALSE)
smp_med$Amount_Group <- cut(smp_med$Amount, 
                            breaks = c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000, 200000, 225000,950000), 
                                    labels = c("[0-25,000)", "[25,000-50,000)", "[50,000-75,000)", "[75,000-100,000)", 
                                               "[100,000-125,000)", "[125,000-150,000)","[150,000-175,000)","[175,000-200,000)",
                                               "[200,000-225,000)", "225,000+"),
                                    right = FALSE)

ggplot(smp_med) +
  geom_bar(aes(x = Age_Group, fill = Amount_Group)) +
  xlab("Age(years)") + ylab("Count")

ggplot(smp_med, aes(x = Specialty, y = Amount, fill = Private.Attorney)) +
  geom_bar(stat = "identity", position =  "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Claim Amount by Physician Specialty and Private Attorney Status")

#claim amount vs specialty 
ggplot(smp_med) +
  geom_bar(aes(x = Specialty, fill = Amount_Group)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Specialty") + ylab("Count")
#claim amouny vs private attorney 
ggplot(smp_med) +
  geom_bar(aes(x = Private.Attorney, fill = Amount_Group)) +
  xlab("Private Attorney") + ylab("Count") 
#claim amount vs gender 
ggplot(smp_med) +
  geom_bar(aes(x = Gender, fill = Amount_Group)) +
  xlab("Gender") + ylab("Count")
#claim amount vs Severity 
ggplot(smp_med) +
  geom_bar(aes(x = Severity, fill = Amount_Group)) +
  xlab("Severity") + ylab("Count")
#claim amount vs insurance type 
ggplot(smp_med) +
  geom_bar(aes(x = Insurance, fill = Amount_Group)) +
  xlab("Insurance") + ylab("Count")
#claim amount vs Marital Status type 
ggplot(smp_med) +
  geom_bar(aes(x = Marital.Status, fill = Amount_Group)) +
  xlab("Marital Status") + ylab("Count")
#Age vs Severity 
smp_med$Severity2 <- as.factor(smp_med$Severity)
ggplot(smp_med) +
  geom_bar(aes(x = Age_Group, fill = Severity2)) +
  xlab("Age") + ylab("Count")
#Specialty vs Severity 
ggplot(smp_med) +
  geom_bar(aes(x = Specialty, fill = Severity2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Specialty") + ylab("Count")

#The threshold was chosen based on mean and is later balanced since Amount is skewed 
smp_med <- smp_med[, -c(9,10,11)] #removing, Age_Group, Amount_Group and Severity2
Amount01 = I(smp_med$Amount >= 185069)
smp_med01 = data.frame(Amount01, smp_med[,-1]) 
smp_med01$Amount01 = as.numeric(smp_med01$Amount01)
table(smp_med01$Amount01)

ggplot(smp_med01) +
  geom_bar(aes(x = Amount01),fill="darkolivegreen3",color="black", binwidth = 1) +
  scale_x_continuous(breaks = c(0, 1)) +
  xlab("Low vs. High Claim Amount")

#one hot encoding dummy var
dmy <- dummyVars(" ~ .", data = smp_med01)
dmymed <- data.frame(predict(dmy, newdata = smp_med01))

# Correlation matrix 
cor_matrix <- cor(dmymed) 
library(corrplot)
corrplot(cor_matrix, method = "circle",tl.cex = 0.7)
# Top 10 correlations
cor_with_Amount01 <- cor_matrix["Amount01", ] 
cor_with_Amount01 <- cor_with_Amount01[cor_with_Amount01 != 1] 
sorted_cor_with_Amount01 <- cor_with_Amount01[order(-abs(cor_with_Amount01))] #  
top_10_cor_with_Amount01 <- head(sorted_cor_with_Amount01, 10) 
top_10_cor_with_Amount01

#train and test split 70/30
smp_size2 <- floor(0.7 * nrow(dmymed))
set.seed(123)
train_ind <- sample(seq_len(nrow(dmymed)), size = smp_size2)
medsmp_train <- dmymed[train_ind, ]
medsmp_test <- dmymed[-train_ind, ]
table(medsmp_train$Amount01)
table(medsmp_test$Amount01)

# balancing dataset 
library(ROSE)
train_balsam <- ovun.sample(Amount01 ~ ., data = medsmp_train, seed = 123, method = "both")$data
table(train_balsam$Amount01)

#variable selection using RFE method 
set.seed(123)
library(mlbench)
control <- rfeControl(functions=rfFuncs, method="cv", number=5)
results <- rfe(train_balsam[,2:35], as.factor(train_balsam$Amount01), sizes=c(1:34), rfeControl=control)
predictors(results)
# [1] "Severity"                        "Age"                             "InsurancePrivate"               
# [4] "SpecialtyCardiology"             "InsuranceMedicare.Medicaid"      "SpecialtyPediatrics"            
# [7] "SpecialtyOphthamology"           "SpecialtyResident"               "Private.AttorneyYes"            
# [10] "SpecialtyOBGYN"                  "Private.AttorneyNo"              "InsuranceWorkers.Compensation"  
# [13] "SpecialtyFamily.Practice"        "SpecialtyOrthopedic.Surgery"     "SpecialtyNeurology.Neurosurgery"
# [16] "SpecialtyRadiology"              "GenderFemale"                    "GenderMale"                     
# [19] "SpecialtyGeneral.Surgery"        "SpecialtyUrological.Surgery"     "Marital.StatusWidowed"          
# [22] "SpecialtyInternal.Medicine"      "SpecialtyThoracic.Surgery"       "SpecialtyPhysical.Medicine"     
# [25] "Marital.StatusDivorced"          "SpecialtyDermatology"            "SpecialtyAnesthesiology"        
# [28] "SpecialtyEmergency.Medicine"     "Marital.StatusMarried"           "Marital.StatusSingle"           
# [31] "SpecialtyPlastic.Surgeon"        "InsuranceNo.Insurance"          
results
plot(results, type=c("g", "o"))
results$optVariables[1:18]

varimp_data <- data.frame(feature = row.names(varImp(results))[1:18],
                          importance = varImp(results)[1:18, 1])

#plot of 18 most important variables 
ggplot(data = varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#MODELS 
#1 Random Forest
library(randomForest)
set.seed(123)
colnames(medsmp_train)
rf1 <- randomForest(as.factor(Amount01) ~., data=train_balsam, 
                    importance=TRUE)
varImpPlot(rf1)
rf.pred = predict(rf1, medsmp_test, type='class')
TestErr= mean(rf.pred !=medsmp_test$Amount01)
TestErr # 0.08601422
confusionMatrix(rf.pred, reference = as.factor(medsmp_test$Amount01), positive = "1")
table(rf.pred, medsmp_test$Amount01)

# tuning of mtry
param_grid <- expand.grid(.mtry = c(2,4,6,8,10,12,14,16))

# Set up cross-validation
control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Perform grid search with cross-validation
set.seed(123)
mtry <- train(as.factor(Amount01) ~ ., data = train_balsam, method = "rf", metric="Accuracy",
                 trControl = control, tuneGrid = param_grid, ntree=500)

# Print the best parameters
print(mtry)

# Random Forest 
# 
# 6888 samples
# 34 predictor
# 2 classes: '0', '1' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 6200, 6199, 6199, 6199, 6200, 6199, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2    0.8342061  0.6686472
# 4    0.8834224  0.7668233
# 6    0.9192808  0.8385452
# 8    0.9330729  0.8661813
# 10    0.9371359  0.8743127
# 12    0.9416377  0.8833189
# 14    0.9441057  0.8882608
# 16    0.9459935  0.8920382
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 16.

#RF with parameter tuning for mtry
set.seed(123)
rf2 <- randomForest(as.factor(Amount01) ~., data=train_balsam, ntree= 500, 
                    mtry=6, nodesize = 15, importance=TRUE)

## The testing error of this new randomForest 
rf.pred2 = predict(rf2, medsmp_test, type='class')
mean(rf.pred2 !=medsmp_test$Amount01)
# 0.08398239
table(rf.pred2, medsmp_test$Amount01)

summary(rf2)
varImpPlot(rf2)

confusionMatrix(rf.pred2, reference = as.factor(medsmp_test$Amount01), positive = "1")
cm <- confusionMatrix(rf.pred2, reference = as.factor(medsmp_test$Amount01), positive = "1")
#plot confusion matrix 
plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction")

##2 Boosting 
library(gbm)
 
gbm.spam1 <- gbm(Amount01 ~ .,data=train_balsam,
                 distribution = 'bernoulli',
                 n.trees = 1000, 
                 shrinkage = 0.01, 
                 interaction.depth = 3,
                 cv.folds = 10)
## Find the estimated optimal number of iterations
perf_gbm1 = gbm.perf(gbm.spam1, method="cv") 
perf_gbm1
## Testing Error
y2hat <- ifelse(predict(gbm.spam1,newdata = medsmp_test[,-1], n.trees=perf_gbm1, type="response") < 0.5, 0, 1)
y2hat[1:10]
mean(y2hat != medsmp_test$Amount01)
# 0.113444

#3SVM

# tuning for optimal values 
# start_time <- Sys.time()
# tune_out=tune(svm, as.factor(Amount01)~. ,data=train_balsam,
#               type = "C-classification",
#               kernel = "radial",
#               ranges = list( cost = c(0.1,1,10,100) , gamma = c(0.001,0.01,0.1,0.90)))
# end_time <- Sys.time()
# time.elapse <- (end_time - start_time)
# print(time.elapse)
# summary(tune_out)

library(e1071)
# use 18 selected variables based on random forest model 2
model_SVM = svm(Amount01 ~ Severity + Age + InsurancePrivate +
                SpecialtyCardiology + Private.AttorneyYes  + SpecialtyFamily.Practice +
                SpecialtyOBGYN + SpecialtyOphthamology + SpecialtyResident +
                SpecialtyNeurology.Neurosurgery + SpecialtyOrthopedic.Surgery+
                SpecialtyPediatrics + InsuranceMedicare.Medicaid + InsuranceWorkers.Compensation+
                GenderFemale + SpecialtyRadiology + Private.AttorneyNo + SpecialtyGeneral.Surgery,
                data = train_balsam,
                type = 'C-classification', # this is because we want to make a regression classification
                kernel = 'radial',
                cost = 100,
                gamma = 0.9)
summary(model_SVM)

#test error 
svm.pred = predict(model_SVM, medsmp_test, type='class')
mean(svm.pred !=medsmp_test$Amount01)
#0.1252963 w/o variable selection 
#0.1229258 w/ variable selection 
table(svm.pred, medsmp_test$Amount01)

#4. Logistic regression: 0.2394175
modA <- glm(Amount01 ~ Severity + Age + InsurancePrivate +
              SpecialtyCardiology + Private.AttorneyYes  + SpecialtyFamily.Practice +
              SpecialtyOBGYN + SpecialtyOphthamology + SpecialtyResident +
              SpecialtyNeurology.Neurosurgery + SpecialtyOrthopedic.Surgery+
              SpecialtyPediatrics + InsuranceMedicare.Medicaid + InsuranceWorkers.Compensation+
              GenderFemale + SpecialtyRadiology + Private.AttorneyNo + SpecialtyGeneral.Surgery, data = train_balsam);
y2hatA <- ifelse(predict(modA, medsmp_test[,-1], type="response" ) < 0.5, 0, 1)
sum(y2hatA != medsmp_test$Amount01)/length(medsmp_test$Amount01) 

#5.Linear Discriminant Analysis : 0.3121582 vs 0.2204538
library(MASS)
modB <- lda(train_balsam[,2:35], train_balsam[,1])
y2hatB <- predict(modB, medsmp_test[,-1])$class
mean( y2hatB  != medsmp_test$Amount01)

##6. Naive Bayes (with full X). Testing error = 0.6661023 vs with variable selection 0.5076194
library(e1071)
modC <- naiveBayes(as.factor(Amount01) ~ Severity + Age + InsurancePrivate +
                     SpecialtyCardiology + Private.AttorneyYes  + SpecialtyFamily.Practice +
                     SpecialtyOBGYN + SpecialtyOphthamology + SpecialtyResident +
                     SpecialtyNeurology.Neurosurgery + SpecialtyOrthopedic.Surgery+
                     SpecialtyPediatrics + InsuranceMedicare.Medicaid + InsuranceWorkers.Compensation+
                     GenderFemale + SpecialtyRadiology + Private.AttorneyNo + SpecialtyGeneral.Surgery , data = train_balsam)
y2hatC <- predict(modC, newdata = medsmp_test)
mean( y2hatC != medsmp_test$Amount01) 

#7KNN
library(class)

# Create a new data frame for the selected variables
selected_data_train <- train_balsam[, c("Amount01", "Severity", "Age", "InsurancePrivate", "SpecialtyCardiology", "SpecialtyGeneral.Surgery", "SpecialtyFamily.Practice", "SpecialtyOBGYN", "SpecialtyOphthamology", "SpecialtyResident", "SpecialtyNeurology.Neurosurgery", "SpecialtyOrthopedic.Surgery", "SpecialtyPediatrics", "InsuranceMedicare.Medicaid", "InsuranceWorkers.Compensation", "GenderFemale", "Private.AttorneyYes", "SpecialtyRadiology", "Private.AttorneyNo")]
selected_data_test <- medsmp_test[, c("Amount01", "Severity", "Age", "InsurancePrivate", "SpecialtyCardiology", "SpecialtyGeneral.Surgery", "SpecialtyFamily.Practice", "SpecialtyOBGYN", "SpecialtyOphthamology", "SpecialtyResident", "SpecialtyNeurology.Neurosurgery", "SpecialtyOrthopedic.Surgery", "SpecialtyPediatrics", "InsuranceMedicare.Medicaid", "InsuranceWorkers.Compensation", "GenderFemale", "Private.AttorneyYes", "SpecialtyRadiology", "Private.AttorneyNo")]


kkk <- c(1,3,5,7,9,11,13,15)
xnew <- selected_data_test[,-1]
testerrknn <- NULL
for(i in 1:8){
  kk <- kkk[i]
  ypred2.test <- knn(selected_data_train[,-1], xnew, selected_data_train[,1], k=kk)
  testerrknn <- cbind( testerrknn, mean(ypred2.test != selected_data_test[,1]))
}
colnames(testerrknn) <-c("KNN1","KNN3","KNN5","KNN7","KNN9","KNN11","KNN13","KNN15")
testerrknn

#cross validation 
n = dim(dmymed)[1]; ### total number of observations
n1 = 6888 #train sample
n2 = 2953 #test sample 
B= 100; ### number of loops
TEALL = NULL; ### Final TE values
set.seed(123); ### You might want to set the seed for randomization
for (b in 1:B){
  flag <- sort(sample(1:n, n1));
  train <- dmymed[flag,];
  test <- dmymed[-flag,];
  
  #1 LDA
  library(MASS)
  mod1 <- lda(train[,2:35], train[,1]); 
  ## testing error 
  pred1test <- predict(mod1,test[,-1])$class; 
  te1 <- mean(pred1test != test$Amount01)
  #2 SVM
  library(e1071)
  # use 18 selected variables based on random forest model 2
  model_SVM = svm(Amount01 ~ Severity + Age + InsurancePrivate +
                    SpecialtyCardiology + Private.AttorneyYes  + SpecialtyFamily.Practice +
                    SpecialtyOBGYN + SpecialtyOphthamology + SpecialtyResident +
                    SpecialtyNeurology.Neurosurgery + SpecialtyOrthopedic.Surgery+
                    SpecialtyPediatrics + InsuranceMedicare.Medicaid + InsuranceWorkers.Compensation+
                    GenderFemale + SpecialtyRadiology + Private.AttorneyNo + SpecialtyGeneral.Surgery,
                  data = train,
                  type = 'C-classification', # this is because we want to make a regression classification
                  kernel = 'radial',
                  cost = 100,
                  gamma = 0.9)
  svm.pred = predict(model_SVM, test, type='class')
  te2 <- mean(svm.pred !=test$Amount01)
  #3 Naive Bayes
  library(e1071)
  modC <- naiveBayes(as.factor(Amount01) ~ Severity + Age + InsurancePrivate +
                       SpecialtyCardiology + Private.AttorneyYes  + SpecialtyFamily.Practice +
                       SpecialtyOBGYN + SpecialtyOphthamology + SpecialtyResident +
                       SpecialtyNeurology.Neurosurgery + SpecialtyOrthopedic.Surgery+
                       SpecialtyPediatrics + InsuranceMedicare.Medicaid + InsuranceWorkers.Compensation+
                       GenderFemale + SpecialtyRadiology + Private.AttorneyNo + SpecialtyGeneral.Surgery , data = train)
  y2hatC <- predict(modC, newdata = test)
  te3 <- mean( y2hatC != test$Amount01)
  #4 logisitic regression
  modA <- glm(Amount01 ~ Severity + Age + InsurancePrivate +
                SpecialtyCardiology + Private.AttorneyYes  + SpecialtyFamily.Practice +
                SpecialtyOBGYN + SpecialtyOphthamology + SpecialtyResident +
                SpecialtyNeurology.Neurosurgery + SpecialtyOrthopedic.Surgery+
                SpecialtyPediatrics + InsuranceMedicare.Medicaid + InsuranceWorkers.Compensation+
                GenderFemale + SpecialtyRadiology + Private.AttorneyNo + SpecialtyGeneral.Surgery, data = train);
  y2hatA <- ifelse(predict(modA, test[,-1], type="response" ) < 0.5, 0, 1)
  te4 <- sum(y2hatA != test$Amount01)/length(test$Amount01)
  #5 KNN
  library(class)
  # Create a new data frame for the selected variables
  selected_data_train <- train[, c("Amount01", "Severity", "Age", "InsurancePrivate", "SpecialtyCardiology", "SpecialtyGeneral.Surgery", "SpecialtyFamily.Practice", "SpecialtyOBGYN", "SpecialtyOphthamology", "SpecialtyResident", "SpecialtyNeurology.Neurosurgery", "SpecialtyOrthopedic.Surgery", "SpecialtyPediatrics", "InsuranceMedicare.Medicaid", "InsuranceWorkers.Compensation", "GenderFemale", "Private.AttorneyYes", "SpecialtyRadiology", "Private.AttorneyNo")]
  selected_data_test <- test[, c("Amount01", "Severity", "Age", "InsurancePrivate", "SpecialtyCardiology", "SpecialtyGeneral.Surgery", "SpecialtyFamily.Practice", "SpecialtyOBGYN", "SpecialtyOphthamology", "SpecialtyResident", "SpecialtyNeurology.Neurosurgery", "SpecialtyOrthopedic.Surgery", "SpecialtyPediatrics", "InsuranceMedicare.Medicaid", "InsuranceWorkers.Compensation", "GenderFemale", "Private.AttorneyYes", "SpecialtyRadiology", "Private.AttorneyNo")]
  kkk <- c(1,3,5,7,9,11,13,15)
  xnew <- selected_data_test[,-1]
  testerrknn <- NULL
  for(i in 1:8){
    kk <- kkk[i]
    ypred2.test <- knn(selected_data_train[,-1], xnew, selected_data_train[,1], k=kk)
    testerrknn <- cbind( testerrknn, mean(ypred2.test != selected_data_test[,1]))
  }
  TEALL = rbind( TEALL, cbind(te1, te2, te3, te4, testerrknn));
}
dim(TEALL);
colnames(TEALL) <- c("LDA", "SVM", "Naive Bayes", "Log Reg", "KNN1","KNN3","KNN5","KNN7","KNN9","KNN11","KNN13","KNN15")
apply(TEALL, 2, mean)

# LDA         SVM Naive Bayes     Log Reg        KNN1        KNN3 
# 0.15629529  0.08826278  0.56460887  0.17478496  0.12590247  0.11837115 
# KNN5        KNN7        KNN9       KNN11       KNN13       KNN15 
# 0.11719607  0.11896715  0.12102269  0.12315611  0.12572638  0.12838808 
