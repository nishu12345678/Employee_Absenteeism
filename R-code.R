rm(list=ls())
getwd()


x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

install.packages(x)
lapply(x, require, character.only = TRUE)
install.packages("rmarkdown")
Employee_Data = read.csv("AAW.csv", header = T, na.strings = c(" ", "", "NA"))
str(Employee_Data)
class(Employee_Data)
catnames=c("ID","Reason.for.absence","Month.of.absence","Day.of.the.week","Seasons","Disciplinary.failure","Education","Son","Social.drinker","Social.smoker","Pet")
for(i in catnames){
  print(i)
  df[,i]=as.factor(df[,i])
}  
str(Employee_Data)
#Exploratory Data Analysis
Employee_Data$ID=as.factor(Employee_Data$ID)
Employee_Data$Reason.for.absence=as.factor(Employee_Data$Reason.for.absence)
Employee_Data$Month.of.absence=as.factor(Employee_Data$Month.of.absence)
Employee_Data$Day.of.the.week=as.factor(Employee_Data$Day.of.the.week)
Employee_Data$Seasons=as.factor(Employee_Data$Seasons)
Employee_Data$Disciplinary.failure=as.factor(Employee_Data$Disciplinary.failure)
Employee_Data$Education=as.factor(Employee_Data$Education)
Employee_Data$Son=as.factor(Employee_Data$Son)
Employee_Data$Social.drinker=as.factor(Employee_Data$Social.drinker)
Employee_Data$Social.smoker=as.factor(Employee_Data$Social.smoker)
Employee_Data$Pet=as.factor(Employee_Data$Pet)


for (i in 1:nrow(Employee_Data)) {
  if(Employee_Data$Absenteeism.time.in.hours[i]!=0 || is.na(Employee_Data$Absenteeism.time.in.hours[i])){
    if(Employee_Data$Reason.for.absence[i]==0 || is.na(Employee_Data$Reason.for.absence[i])){
      Employee_Data$Reason.for.absence[i]=NA
    }
      if(Employee_Data$Month.of.absence[i]==0 || is.na(Employee_Data$Month.of.absence[i])){
        Employee_Data$Month.of.absence[i]=NA
      }
  }
  
}
#Missing Value Analysis
#Impute values related to Id
Dependent_ID=c("ID","Transportation.expense","Service.time","Age","Height","Distance.from.Residence.to.Work","Education","Son","Weight","Social.smoker","Social.drinker","Pet","Body.mass.index")
Dependent_ID_Data=Employee_Data[,Dependent_ID]
Dependent_ID_Data=aggregate(.~ID,data = Dependent_ID_Data,FUN=function(e) c(x=mean(e)))
for (i in Dependent_ID) {
  for (j in (1:nrow(Employee_Data))) {
    ID=Employee_Data[j,"ID"]
    if(is.na(Employee_Data[j,i])){
      Employee_Data[j,i]=Dependent_ID_Data[ID,i]
    }
    
  }
  
}
sum(is.na(Employee_Data))
missing_index=sapply(Employee_Data,is.na)
missing_val=data.frame(apply(Employee_Data,2,function(x){sum(is.na(x))}))
nrow(Employee_Data)
#Impute values for other variables
Employee_Data=knnImputation(Employee_Data,k=7)
sum(is.na(Employee_Data))
missingValueCheck(Employee_Data)
#Outlier Analysis
class(Employee_Data)
class(Employee_Data$ID)
num_index=sapply(Employee_Data, is.numeric)
numeric_data=Employee_Data[,num_index]
num_cnames=colnames(numeric_data)
for (i in num_cnames) {
  hist(Employee_Data[,i],xlab = i,main = "_",col = (c("lightblue","darkgreen")))
  
}
num_cnames=num_cnames[num_cnames!="Absenteeism.time.in.hours"]
num_cnames
for (i in 1:length(num_cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (num_cnames[i]), x = "Absenteeism.time.in.hours"), data = subset(Employee_Data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=num_cnames[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot of responded for",num_cnames[i])))
}

# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn3,gn2,ncol=3)
gridExtra::grid.arrange(gn4,gn5,ncol=2)
gridExtra::grid.arrange(gn6,gn7,ncol=2)
df=Employee_Data
Employee_Data=df
for(i in num_cnames){
  print(i)
  val = Employee_Data[,i][Employee_Data[,i] %in% boxplot.stats(Employee_Data[,i])$out]
  print(length(val))
  
}
#Replace all outliers with NA and impute

for(i in num_cnames){
  val = Employee_Data[,i][Employee_Data[,i] %in% boxplot.stats(Employee_Data[,i])$out]
  print(length(val))
  Employee_Data[,i][Employee_Data[,i] %in% val] = NA
}
sum(is.na(Employee_Data))
Employee_Data = knnImputation(Employee_Data, k = 7)
sum(is.na(Employee_Data))
#Feature Selection
#Correlation Plot
corrgram(Employee_Data,upper.panel = panel.pie,text.panel = panel.txt,main="correlation_plot")
#As weight and body mass index are highly correlated so one variable need to be remove
#ANOVA test
cnames=colnames(Employee_Data)
for (i in cnames) {
  print(i)
  print(summary(aov(Employee_Data$Absenteeism.time.in.hours~Employee_Data[,i],Employee_Data)))
  
}
Employee_Data=subset(Employee_Data,select=-c(Weight,Education ,Service.time ,Body.mass.index ,Seasons ,Transportation.expense ,Pet ,Disciplinary.failure ,Month.of.absence ,Hit.target ,Social.drinker ,Work.load.Average.day ,Social.smoker))
Employee_Data
#Feature Scaling
  
num_index=sapply(Employee_Data, is.numeric)
numeric_data=Employee_Data[,num_index]
num_cnames=colnames(numeric_data)
num_cnames
for (i in num_cnames) {
  Employee_Data[,i]=(Employee_Data[,i]-min(Employee_Data[,i]))/(max(Employee_Data[,i])-min(Employee_Data[,i]))
  
}
hist(Employee_Data$Distance.from.Residence.to.Work)
hist(Employee_Data$Age)
hist(Employee_Data$Height)
hist(Employee_Data$Absenteeism.time.in.hours)
  #Model Development
set.seed(123)
X_index=sample(1:nrow(Employee_Data),0.8*nrow(Employee_Data))
X_train=Employee_Data[X_index,-8]
X_test=Employee_Data[-X_index,-8]
Y_train=Employee_Data[X_index,8]
Y_test=Employee_Data[-X_index,8]

train=Employee_Data[X_index,]
test=Employee_Data[-X_index,]

#Calculate RMSE
RMSE= function(y, yhat){
   sqrt(mean((y - yhat)^2))
}
#Calculate MSE
MSE = function(y, yhat){
  (mean((y - yhat)^2))
}
###########################Multiple Linear Regression###################
lm_model = lm(Absenteeism.time.in.hours ~., data = train)
summary(lm_model)
#Predict for new test cases
cat_index=sapply(Employee_Data, is.factor)
cat_data=Employee_Data[,cat_index]
cat_cnames=colnames(cat_data)
cat_cnames
for (i in cat_cnames) {
  lm_model$xlevels[[i]]=union(lm_model$xlevels[[i]],levels(X_test[[i]]))
  
}
lm_predict=predict(lm_model,newdata = X_test)
lm_predict
RMSE(lm_predict,Y_test)
MSE(lm_predict,Y_test)
######################Decesion Tree Regression#####################
DT_model=rpart(Absenteeism.time.in.hours ~.,data=train,method = "anova")
#Predict for new test cases
DT_predict=predict(DT_model,X_test)
summary(DT_model)
RMSE(DT_predict,Y_test)
MSE(DT_predict,Y_test)

#####################Random Forest###############
RF_model=randomForest(x=X_train,y=Y_train,ntree = 100)
#Predict for new test cases
RF_predict=predict(RF_model,X_test)
getTree(RF_model,1,labelVar = TRUE)
summary(RF_model)
RMSE(RF_predict,Y_test)
MSE(RF_predict,Y_test)


##########Problems#############
#Suggesting the changes
lm_model_pr=lm(Absenteeism.time.in.hours ~.,data=Employee_Data)
summary(lm_model_pr)

#Calculating Losses
p2_data=Employee_Data[,-8]
#Predict for new test cases
p2_predict=predict(RF_model,p2_data)

#convert predicted values to actual value
p2_predict=(p2_predict*120)
 #Add predicted values to the data set
p2_dataset=merge(df[,-21],p2_predict,by="row.names",all.x=TRUE)
#Calculate the total loss
loss=0
loss=0
for(i in 1:nrow(p2_dataset)){
  if (p2_dataset$Hit.target[i]!=100) 
    if(p2_dataset$Age[i]>=25 && p2_dataset$Age<=32){
      loss=loss+as.numeric(p2_dataset$Disciplinary.failure[i])*1000 +(as.numeric(p2_dataset$Education[i])+1)*500 + p2_dataset$y[i]*1000
    }else if(p2_dataset$Age[i]>=33 && p2_dataset$Age[i]<=40){
      loss=loss+as.numeric(p2_dataset$Disciplinary.failure[i]*1000) +(as.numeric(p2_dataset$Education[i])+2)*500 +p2_dataset$y[i]*1000
    }else if(p2_dataset$Age[i]>=41 && p2_dataset$Age[i]<=49){
      loss=loss+as.numeric(p2_dataset$Disciplinary.failure[i]*1000) +(as.numeric(p2_dataset$Education[i])+3)*500 +p2_dataset$y[i]*1000
    }else if(p2_dataset$Age[i]>=50 && p2_dataset$Age[i]<=60){
      loss=loss+as.numeric(p2_dataset$Disciplinary.failure[i]*1000) +(as.numeric(p2_dataset$Education[i])+4)*500 +p2_dataset$y[i]*1000
    }
    
  }
 
loss=loss/12
loss
  
  
