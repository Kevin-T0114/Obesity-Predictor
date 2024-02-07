#EDA
dim(ObesityDataSet_raw_and_data_sinthetic)
summary(ObesityDataSet_raw_and_data_sinthetic)
obesityWithNumericalVariables <- ObesityDataSet_raw_and_data_sinthetic[,c("Age", "Height", "Weight", "FCVC", "NCP", "CH2O", "FAF", "TUE" )]
pc.out <- prcomp(obesityWithNumericalVariables, scale=T)
pc.out
summary(pc.out)

#KNN
#RNGkind(sample.kind = "Rounding")

set.seed(1)
obesity <- (ObesityDataSet_raw_and_data_sinthetic[,c("Height","Weight","NCP","FAF","NObeyesdad")])

#part b & c
n <- nrow(obesity)
train <- sample(1:n, 0.8*n)

x.train <- scale(obesity[train,-5])
x.test <- scale(obesity[-train,-5],
                center = attr(x.train, "scaled:center"),
                scale = attr(x.train, "scaled:scale"))

y.train <- obesity[train, "NObeyesdad"]
y.test <- obesity[-train, "NObeyesdad"]

set.seed(1)
library(class)
for(K in c(1,2,3,4,5,6,7,8,9,10)) {
  set.seed(1)
  knn.pred <- knn(train = x.train,
                  test = x.test,
                  cl = y.train$NObeyesdad,
                  k=K)
  print(mean(knn.pred != y.test$NObeyesdad))
}

#part d
set.seed(1)
obesity.scale = scale(obesity[, -ncol(obesity)])
knn.pred.opt = knn(train = obesity.scale,
                   test = obesity.scale,
                   cl = obesity$NObeyesdad, 
                   k = 1)
plot(knn.pred.opt, main = "KNN classification plot", xlab = "obesity level", ylab = "count")
summary(knn.pred.opt)
mean(knn.pred.opt != obesity$NObeyesdad)

#SVM
library(e1071)
set.seed(1)
obesity <- (ObesityDataSet_raw_and_data_sinthetic[,c("Height","Weight","NCP","FAF","NObeyesdad")])
n <- nrow(obesity)
train <- sample(1:n, 0.8*n)

#part b
set.seed(1)
ObesityClass <- tune(svm,
                     as.factor(NObeyesdad)~., data=obesity[train,],
                     kernel="radial",
                     ranges=list(cost=c(0.001,0.01,0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(ObesityClass)

#part c
set.seed(1)
train.pred <- predict(ObesityClass$best.model,obesity[train,])
mean(obesity[train,]$NObeyesdad != train.pred)

#part d
set.seed(1)
ObesityClass.obj <- svm(as.factor(NObeyesdad)~., data=obesity,
                     kernel="radial",
                     cost=100,gamma=0.5)
summary(ObesityClass.obj)
#plots
par(mfrow = c(1,1))
plot(ObesityClass.obj, obesity, Height~Weight)
plot(ObesityClass.obj, obesity, Height~NCP)
plot(ObesityClass.obj, obesity, Height~FAF)
plot(ObesityClass.obj, obesity, Weight~NCP)
plot(ObesityClass.obj, obesity, Weight~FAF)
