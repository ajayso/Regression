install.packages("Quandl")
library(Quandl)
install.packages("devtools")
library(devtools)
install_github("quandl/quandl-r")
library(TTR)
source('C:/start/ML-Regression-Analysis/functions.r')

#API Key for Quandl
Quandl.api_key("Td2oA_m_SYUdi1X9Htdi")

goldpricesAllUp =  Quandl("LBMA/GOLD")

# Calculate the ROC 
roc  = goldpricesAllUp$`USD (AM)`
yy = ROC(roc, type="discrete")*100


gpdata = data.frame(goldpricesAllUp$Date,goldpricesAllUp$`USD (AM)`,yy)
gpdata = gpdata[gpdata$goldpricesAllUp.Date > "2014-01-01",]
model = lm ( goldpricesAllUp..USD..AM..~., data = gpdata)
# linear equation 
paste('y =', coef(model)[[2]], '* x', '+', coef(model)[[1]])

# Calculate the Slope 
slopedata <- array(1:length(gpdata$goldpricesAllUp.Date))
slopedata = slope(gpdata)

# Calculate the Ratios
roc_ratio = RationRoc(gpdata)

# Calculate the Stochastic_Oscillator
so_index = 14
SOCategoric  = Stochastic_Oscillator(gpdata,so_index)
SONumeric = Stochastic_Oscillator_Numeric(gpdata,so_index)

# Try basic gaussian model in glm 

glmdataset = data.frame(gpdata, slopedata,roc_ratio,SONumeric)


trainingdataLength = 0.8 * length(glmdataset$goldpricesAllUp.Date)
testingdatalength = 0.2 * length(glmdataset$goldpricesAllUp.Date)
train <- glmdataset[1:trainingdataLength,]
#test <- xfinaldata[trainingdataLength:length(xfinaldata$goldpricesAllUp.Date),]
test <- glmdataset[trainingdataLength:length(glmdataset$goldpricesAllUp.Date),]


# Use Gaussian distribution this will require BuySell to be numeric....
lr_model <- glm(BuySellFlag ~ goldpricesAllUp..USD..AM..+ yy+ slopedata  + StochasticOscillator,data=train, family=gaussian  )
anova(lr_model, test="Chisq")
lr_predict = predict(lr_model, newdata = test, type="response")
# Calulate the Accuracy of the Model
buysellpredicted <- array(1:length(lr_predict))
for (i in 1: length(lr_predict)){
  print(i)
  index = lr_predict[i]
  if(index >0.5)
    signal = 1
  if ((index < 0.5) & (index > 0))
    signal = 0
  if ((index > -0.5) & (index < 0))
    signal = "0"
  
  if ((index < -0.5) )
    signal = -1
  
  buysellpredicted[i]= signal
  
  
}
# Get the accuracy of the GLM Model
confusion_matrix <- ftable(test$BuySellFlag, buysellpredicted)
accuracy <- sum(diag(confusion_matrix))/152*100


# Use multinom logistics regression considering we have more then 2 outcomes of categorical variable BuySell
library(nnet)
mnetdataset = data.frame(gpdata, slopedata,roc_ratio,SOCategoric)

#mnetdataset$BuySellFlag = as.factor(mnetdataset$BuySellFlag)
trainingdataLength = 0.8 * length(mnetdataset$goldpricesAllUp.Date)
testingdatalength = 0.2 * length(mnetdataset$goldpricesAllUp.Date)
train <- mnetdataset[1:trainingdataLength,]
test <- mnetdataset[trainingdataLength:length(mnetdataset$goldpricesAllUp.Date),]
mnetmodel = multinom(BuySellFlag ~ goldpricesAllUp..USD..AM..+ yy+ slopedata  + StochasticOscillator,data=train)
summary(mnetmodel)
mnetmodel_predict = predict(mnetmodel, type="probs", data.frame(test))


xx  = data.frame(mnetmodel_predict)
for (i in 1: length(mnetmodel_predict)){
  print(i)
  index = which.max(mnetmodel_predict[i,])
  if(index ==1)
    signal = "Buy"
  if(index ==2)
    signal = "Hold"
  if(index ==3)
    signal = "Sell"
  mnetmodel_predict[i,]= signal
  xx[i,]= signal
  
}
# Get the accuracy of the Multinom Model
m_confusion_matrix <- ftable(test$BuySellFlag, xx$Buy)
m_accuracy <- sum(diag(m_confusion_matrix))/152*100




