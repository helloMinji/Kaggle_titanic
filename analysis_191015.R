library(dplyr)
library(ggplot2)

setwd("./kagglestudy/titanic")


titanic_train <- read.csv("./train.csv")
titanic_test <- read.csv("./test.csv")














##### 1. 데이터의 특징 #####

str(titanic_raw)
#  'data.frame':	891 obs. of  12 variables:
#    $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
#    $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
#    $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
#    $ Name       : Factor w/ 891 levels "Abbing, Mr. Anthony",..: 109 191 358 277 16 559 520 629 417 581 ...
#    $ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
#    $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
#    $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
#    $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
#    $ Ticket     : Factor w/ 681 levels "110152","110413",..: 524 597 670 50 473 276 86 396 345 133 ...
#    $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
#    $ Cabin      : Factor w/ 148 levels "","A10","A14",..: 1 83 1 57 1 1 131 1 1 1 ...
#    $ Embarked   : Factor w/ 4 levels "","C","Q","S": 4 2 4 4 4 3 4 4 4 2 ...

table(titanic_raw$Survived)

table(titanic_raw$Pclass)
table(is.na(titanic_raw$Pclass))

table(titanic_raw$Sex)
table(is.na(titanic_raw$Sex))

hist(titanic_raw$Age)
table(is.na(titanic_raw$Age))

hist(titanic_raw$SibSp)
table(is.na(titanic_raw$SibSp))

hist(titanic_raw$Parch)
table(is.na(titanic_raw$Parch))

hist(titanic_raw$Fare)
table(is.na(titanic_raw$Fare))

#test에는 NA 하나 있음
for(i in (1:418)){
  if(is.na(titanic_test$Fare[i])){
    titanic_test$Fare[i] = mean(titanic_test$Fare, na.rm = TRUE)
  }
}


table(titanic_raw$Embarked)
table(is.na(titanic_raw$Embarked))










##### 2. 시각화와 NA 처리 #####

titanic_test$Survived <- c(NA)
titanic_raw <- rbind(titanic_train, titanic_test)


## Survived
titanic_raw$Survived <- as.factor(titanic_raw$Survived)

## Pclass : factor 변환 후 plot
titanic_raw$Pclass <- as.factor(titanic_raw$Pclass)
qplot(Survived, data = titanic_raw, fill = Pclass)

## Sex : plot
qplot(Survived, data = titanic_raw, fill = Sex)

## SibSp, Parch : family로 합친 후 plot 확인할 때만 factor로 변경
titanic_raw$family <- titanic_raw$SibSp + titanic_raw$Parch
titanic_raw$family <- ifelse(titanic_raw$family == 0, 0, 1)
qplot(Survived, data = titanic_raw, fill = as.factor(family))

## Fare
boxplot(titanic_raw$Fare)$stats
# 10 단위로 나눠서 분석에 용이하게 변경, 100 이상은 10으로 통일
titanic_raw$Fare_group <- titanic_raw$Fare%/%10
titanic_raw$Fare_group <- ifelse(titanic_raw$Fare_group > 10, 10, titanic_raw$Fare_group)
titanic_raw$Fare_group <- as.factor(titanic_raw$Fare_group)
# plot
qplot(Survived, data = titanic_raw, fill = Fare_group)

## Embarked
qplot(Survived, data = titanic_raw, fill = Embarked)
titanic_raw$Embarked <- ifelse(titanic_raw$Embarked == "", "4", titanic_raw$Embarked)
titanic_raw$Embarked <- as.factor(titanic_raw$Embarked)
qplot(Survived, data = titanic_raw, fill = Embarked) # C=2,Q=3,S=4


raw <- dplyr::select(titanic_raw, -Name, -SibSp, -Parch, -Ticket, -Cabin)


## Age : plot으로 NA확인 후 처리, 분석에 용이한 Age_group으로 변수 설정
qplot(Age, data = titanic_raw)
boxplot(titanic_raw$Age)$stats

# NA처리
Age_NA <- is.na(raw$Age)
raw_na <- filter(raw, Age_NA)
raw_not_na <- filter(raw, !Age_NA)

check <- c()
for(i in (1:263)){ # train:177 test:86
  for(j in (1:1046)){ # train:714 test:332
    if ((raw_na$Pclass[i] == raw_not_na$Pclass[j])
        & (raw_na$Sex[i] == raw_not_na$Sex[j])
        & (raw_na$Embarked[i] == raw_not_na$Embarked[j])
        & (raw_na$family[i] == raw_not_na$family[j])
        & (raw_na$Fare_group[i] == raw_not_na$Fare_group[j])){
      check <- c(check, j)
      break
    }
    else{
      if(j==1046) check <- c(check, 0)
    }
  }
}
raw_na$has_same <- check

check2 <- c()
for(i in (1:263)){
  for(j in (1:1046)){
    if ((raw_na$Pclass[i] == raw_not_na$Pclass[j])
        & (raw_na$Sex[i] == raw_not_na$Sex[j])
        & (raw_na$Embarked[i] == raw_not_na$Embarked[j])
        & (raw_na$family[i] == raw_not_na$family[j])){
      check2 <- c(check2, j)
      break
    }
    else{
      if(j==1046) check2 <- c(check2, 0)
    }
  }
}
raw_na$notsame_fare <- check2

for(i in (1:263)){
  raw_na$Age[i] <- ifelse(raw_na$has_same[i] != 0, raw_not_na$Age[raw_na$has_same[i]], raw_not_na$Age[raw_na$notsame_fare[i]])
}

raw_na <- dplyr::select(raw_na, -has_same, -notsame_fare)

raw <- rbind(raw_na, raw_not_na)

# 0~20세 아래까지 어린이, 20~30은 청년, 30~40은 장년, 40~65는 중년, 65~80은 노년으로 분리
raw$Age_group <- ifelse(raw$Age < 20, 0,
                                ifelse(raw$Age < 30, 1,
                                       ifelse(raw$Age < 40, 2,
                                              ifelse(raw$Age < 65, 3, 4))))
raw$Age_group <- as.factor(raw$Age_group)
qplot(Survived, data = raw, fill = Age_group)



titanic_data <- dplyr::select(raw, -Age_group, -Fare)
titanic_data <- titanic_data[c(order(titanic_data$PassengerId)),]
rownames(titanic_data) <- NULL

rm(raw)
rm(raw_na)
rm(raw_not_na)











##### 3. 모델 구현 #####

check_NA <- is.na(titanic_data$Survived)
train_data <- filter(titanic_data, !check_NA)
test_data <- filter(titanic_data, check_NA)

## 로지스틱 회귀분석
model_logistic <- glm(Survived ~ ., data = titanic_data, family = "binomial")
summary(model_logistic) # 수행된 변수의 계수, 표준편차, p값 및 절편
# Pclass, Sex, Age, SibSp3,4 유의하다
# AIC: 658.64 (그냥 제거했을 때)

rm(back1)
back1 <- glm(Survived ~ Pclass + Sex + Age + Embarked + family, data = titanic_data, family = "binomial")
summary(back1)


anova(model_logistic, test="Chisq")
# Resid. Dev 모델의 성능이 얼마나 나아지는가. 유의한건 위와 같네.

install.packages("pscl") # pR2를 사용하려고(모델해석)
library(pscl)
pR2(model_logistic)
# 결정계수 R^2이 0.51임.(정확도) 더 높이고 싶어.


model2_logistic <- glm(Survived ~ Pclass + Sex + Age + SibSp,
                      data = titanic_data, family = "binomial")
summary(model2_logistic)
# AIC: 650.52
anova(model2_logistic, test="Chisq")
pR2(model2_logistic)
# 0.50 더 떨어졌네?


model3_logistic <- glm(Survived ~ Pclass + Sex,
                      data = titanic_data, family = "binomial")
summary(model3_logistic)
# AIC: 680.06
pR2(model3_logistic)
# 0.45

### 자유도 대비 이탈도 차이가 거의 없어야 변수를 제거해도 모델에 별 영향이 없는 것인데
### 1: 620.63, 2: 630.52, 3: 672.06 1에서 2는 큰 차이 없으므로 ok
### AIC가 가장 낮은 두번째 모델 채택


pchisq((960.90-630.52), df = (711-702), lower.tail = F)
### 9.234993e-66이므로 p-value < 0.0001이므로 적어도 하나의 설명변수는 반응변수를 예측하는데 유의하다.





## Random Forest
install.packages("randomForest")
install.packages("caret", dependencies = TRUE) # e1071 오류 생겨서 dependencies 추가함

library(MASS)
library(randomForest)
library(caret)


set.seed(10)
train_data$Survived <- as.factor(train_data$Survived)
model_rf = randomForest(Survived ~ Pclass + Sex + Age + Embarked + family + Fare_group, data = train_data, mtry = floor(sqrt(7)), ntree = 1000, proximity = T)
model_rf
## mtry는 classification이면 sqrt(변수갯수), regression이면 변수갯수/3
table(train_data$Survived, predict(model_rf)) #(row,col)
importance(model_rf) # 지니계수, 갚이 높은 변수가 클래스를 분류하는데 가장 큰 영향을 줌.

# 정확도 확인
pred_rf <- predict(model_rf)
summary(pred_rf)
train_rf <- train_data$Survived
summary(train_rf)
caret::confusionMatrix(pred_rf, train_rf) ## Accuracy : 0.8216 변수 조정 후 0.8148


set.seed(10)
model_rf2 = randomForest(Survived ~ Pclass + Sex + Age + Fare_group, data = train_data, mtry = floor(sqrt(7)),
                       ntree = 1000, proximity = T)
model_rf2
caret::confusionMatrix(predict(model_rf2), train_data$Survived) # Accuracy : 0.8202 변수 조정 후 0.8305


test_x <- test_data[c("Pclass", "Sex", "Age", "Fare_group")]
y_pred <- predict(model_rf2, test_x)

sub <- data.frame(test_data$PassengerId, y_pred)
write.csv(sub, file="submission.csv", row.names = FALSE)


## kNN

## knn은 실행전에 데이터값이 전부 숫자여야 한다
titanic_knn <- titanic_data
## level만 바꾸면 데이터는 저절로 바뀐다
levels(titanic_knn$Sex) <- c(1,2) # 1:female, 2:male
levels(titanic_knn$Embarked) <- c(2,3,4) #2:C 3:Q 4:S

knn_train <- titanic_knn[1:712,]
knn_valid <- titanic_knn[713:891,]

knn_train_x <- knn_train[,-1]
knn_train_y <- knn_train[,1]
knn_valid_x <- knn_valid[,-1]
knn_valid_y <- knn_valid[,1]

install.packages("class")
library(class)

set.seed(1000)
knn_1 <- knn(train = knn_train_x, test = knn_valid_x, cl = knn_train_y, k = 1)
knn_accuracy_1 <- sum(knn_1 == knn_valid_y) / length(knn_valid_y)
knn_accuracy_1 ## 0.6853933 after 0.698324

# 1~100까지의 k로 knn 적용 + 분류정확도 계산하기
knn_accuracy_k <- NULL
for(kk in c(1:100)){
  set.seed(1000)
  knn_k <- knn(train = knn_train_x, test = knn_valid_x, cl = knn_train_y, k = kk)
  knn_accuracy_k <- c(knn_accuracy_k, sum(knn_k == knn_valid_y) / length(knn_valid_y))
}
# 분류정확도 데이터생성
knn_valid_k <- data.frame(k=c(1:100), accuracy = knn_accuracy_k)
# 분류정확도 그래프
plot(formula = accuracy ~ k, data = knn_valid_k, type = "o", pch = 20, main = "validation - optimal k")
# 그래프에 몇 k인지 라벨링
with(knn_valid_k, text(accuracy ~ k, labels = rownames(knn_valid_k), pos = 1, cex = 0.7))

# 분류정확도는 높으면서 가장 작은 k는?
min(knn_valid_k[knn_valid_k$accuracy %in% max(knn_accuracy_k), "k"]) # 18 after 6

knn_accuracy_k # k=18일때 accuracy = 0.7808989 after k=6 acc=0.7821229





# Naive Bayesian Classifier

library(e1071)
library(caret)

titanic_train <- titanic_data[,2:7]
nb_model <- naiveBayes(Survived ~ ., data = titanic_data)
nb_model
nbpred <- predict(nb_model, titanic_train, type="class")
length(nbpred)
length(titanic_data$Survived)
caret::confusionMatrix(nbpred, titanic_data$Survived)
# Accuracy : 0.7598 after 0.7666




# SVM

library(e1071)

svm_model <- svm(Survived ~ ., data = titanic_data)
svm_model

table(predict(svm_model, titanic_train), titanic_data$Survived)
## Accuracy : (359 + 198) / 712 = 0.7823