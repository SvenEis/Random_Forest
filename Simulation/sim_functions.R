# Data Generating Process
DataGenerator <- function(p, n, mu_vec, SIGMA, beta){
  ##
  X <- MASS::mvrnorm(n, mu_vec, SIGMA)
  ##
  pi <- (exp(X %*% beta))/(1 + exp(X %*% beta))
  Y <- as.factor(rbinom(n, 1, pi))
  ##
  MyData <- data.frame(Y, X)
  ##
  return(MyData)
}

# Add Fake Predictors
AddFakePredictors <- function(df, n_fake){
  ##
  n_obs <- nrow(df)
  ##
  mat_fake <- matrix(data = rnorm(n_obs * n_fake, mean = 0, sd = 100), nrow = n_obs, ncol = n_fake) 
  mat_fake <- data.frame(mat_fake)
  ##
  colnames(mat_fake) <- paste0("FAKE", 1:n_fake)
  ##
  return(cbind(df, mat_fake))
}

# Random Forest - default
randomForestDefault<-function(data,subset){
  rf.def.data <- randomForest(Y ~., data = data,
                              subset = subset, importance = TRUE)
  
  rf.def.pred<- predict(rf.def.data, newdata = data[-subset,])
  
  return(rf.def.pred)
}

# Random Forest - mtry tuning
randomForestTuned<-function(data,subset,Y.test){
  for (j in 1:p){
    rf.opt.data.loop <- randomForest(Y ~. , data = data,
                                     subset = subset, mtry=j, importance = TRUE)
    
    rf.opt.pred.loop <- predict(rf.opt.data.loop, newdata = data[-subset,])
    
    container.opt[j] <- 1 - (table(rf.opt.pred.loop, Y.test)[1,1] + table(rf.opt.pred.loop, Y.test)[2,2] ) /(n/2)
  }
  rf.opt.data <- randomForest(Y ~. , data = data, mtry=as.numeric(match(min(container.opt),container.opt)),
                              subset = subset, importance = TRUE)
  
  rf.opt.pred <- predict(rf.opt.data, newdata = data[-subset,])
  return(rf.opt.pred)
}

# Recursive Feature Elimination
RFEAlgo<-function(data,data.train){
  x_train<-data.train[ , ! names(data.train) %in% c("Y")]
  y_train<-data.train$Y
  
  result_rfe<-rfe(x=x_train,y=y_train,sizes=c(1:p),rfeControl=control)
  
  MyData_new<-data.frame(data$Y,data[,which(names(data) %in% predictors(result_rfe)[order(match(predictors(result_rfe),names(data)))])],row.names=NULL)
  colnames(MyData_new)[1]<-"Y"
  return(MyData_new)
}

