# Random Forest - default
randomForestDefaultAppl<-function(data,subset){
  rf.def.data <- randomForest(credit.risk ~., data = data,
                              subset = subset, importance = TRUE)
  
  rf.def.pred<- predict(rf.def.data, newdata = data[-subset,])
  
  return(rf.def.pred)
}

# Random Forest - mtry tuning
randomForestTunedAppl<-function(data,subset){
  container.opt<-c()
  for (j in 1:20){
    rf.opt.data.loop <- randomForest(credit.risk ~. , data = data,
                                     subset = subset, mtry=j, importance = TRUE)
    
    rf.opt.pred.loop <- predict(rf.opt.data.loop, newdata = data[-subset,])
    
    container.opt[j] <- 1 - (table(rf.opt.pred.loop, credit.risk.test)[1,1] + table(rf.opt.pred.loop, credit.risk.test)[2,2] ) /(sum(siz)/2)
  }
  rf.opt.data <- randomForest(credit.risk ~. , data = data, mtry=as.numeric(match(min(container.opt),container.opt)),
                              subset = subset, importance = TRUE)
  
  rf.opt.pred <- predict(rf.opt.data, newdata = data[-subset,])
  return(rf.opt.pred)
}



# Recursive Feature Elimination
RFEAlgoAppl<-function(data,data.train){
  x_train<-data.train[ , ! names(data.train) %in% c("credit.risk")]
  y_train<-data.train$credit.risk
  
  result_rfe<-rfe(x=x_train,y=y_train,sizes=c(1:20),rfeControl=control)
  
  MyData_new<-data.frame(data$credit.risk,data[,which(names(data) %in% predictors(result_rfe)[order(match(predictors(result_rfe),names(data)))])],row.names=NULL)
  colnames(MyData_new)[1]<-"credit.risk"
  return(MyData_new)
}

