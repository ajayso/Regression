install.packages("Quandl")
library(Quandl)
install.packages("devtools")
library(devtools)
install_github("quandl/quandl-r")
library(TTR)
source('C:/start/ML-Regression-Analysis/functions.r')
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

slopedata <- array(1:length(gpdata$goldpricesAllUp.Date))
slopedata = slope(gpdata)
# Calculate the Slope 

slope = function (data){
  slopedata <- array(1:length(data$goldpricesAllUp.Date))
  
  for  (i in 1: length(data$goldpricesAllUp.Date)){
    
    
    
    if (
      (!is.na(data$yy[i])) & 
      (!is.na(data$yy[i+1]))
    )
    {  
      x1 = data$yy[i]
      x2 = data$yy[i+1]
      y1 = data[i,2]
      y2 = data[i+1,2]
      ya = y2-y1
      xa = x2-x1
      za= ya/xa
      slopedata[i+1] = za
      
    }
    
  }
  return (slopedata)
}


roc_ratio = RationRoc(gpdata)
so_index = 14
SO  = Stochastic_Oscillator(gpdata,so_index)



# Ratio of ROC


RationRoc = function (data){
  roc_ratio = array(1:length(data$goldpricesAllUp.Date))
  for  (i in 1: length(data$goldpricesAllUp.Date)){
    if (!is.na(data$yy[i])) {
      x1 = data$yy[i]
      x2 = data$yy[length(data)]
      print(x1)
      print(x2)
      xa = x1/x2
      print (xa)
      roc_ratio[i] = xa
      
    }
  }
  return (roc_ratio)
}

# Calculate the Slope 

Stochastic_Oscillator  = function (data, n ){
  
  StochasticOscillator  <- array(1:length(data$goldpricesAllUp.Date))
  BuySellFlag  <- array(1:length(data$goldpricesAllUp.Date))
  #Ln = lowest price over past n days
  goldpricedata = data$goldpricesAllUp..USD..AM..
  
  #Hn= highest price over past n days
  
  
  for  (i in 1: length(data$goldpricesAllUp.Date)){
    if (n+i <length(data$goldpricesAllUp.Date)){
      
      
      dx = n+i
      
      xdata = goldpricedata[i:dx]
      
      Ln = which.min(xdata)
      if (Ln==0){
        print("Asshole")
      }
      
      Hn = which.max(xdata)
      
      #P(x) = price on day x 
      px = data[i,2]
      # %K = (P(x) ??? Ln)/ (Hn ??? Ln) x 100%..
      pa = px -xdata[Ln]
      
      na = xdata[Hn]-xdata[Ln]
      
      ma= pa/na
      
      StochasticOscillator[i] = ma *100
      if (is.na(StochasticOscillator[i]))
        next
      if (StochasticOscillator[i] < 20)
        BuySellFlag[i]= "Buy"
      else 
        BuySellFlag[i]= "Hold"
      
      if (StochasticOscillator[i] > 80)
        BuySellFlag[i]= "Sell"
    }
    else {
      BuySellFlag[i]= "Hold"
    }
  }
  
  SX = data.frame(StochasticOscillator,BuySellFlag )
  return(SX)
}


xfinaldata = data.frame(gpdata, slopedata,roc_ratio,SO)


xfinaldata$BuySellFlag = as.factor(xfinaldata$BuySellFlag)
#xfinaldata= na.omit(xfinaldata)
trainingdataLength = 0.8 * length(xfinaldata$goldpricesAllUp.Date)
testingdatalength = 0.2 * length(xfinaldata$goldpricesAllUp.Date)
train <- xfinaldata[1:trainingdataLength,]
#test <- xfinaldata[trainingdataLength:length(xfinaldata$goldpricesAllUp.Date),]
test <- xfinaldata[606:757,]
test= na.omit(test)

# Use Gaussian distribution this will require BuySell to be numeric....
lr_model <- glm(BuySellFlag ~ goldpricesAllUp..USD..AM..+ yy+ slopedata  + StochasticOscillator,data=train, family=gaussian  )
anova(lr_model, test="Chisq")
predict(lr_model, newdata = test, type="response")


library(nnet)
model = multinom(BuySellFlag ~ goldpricesAllUp..USD..AM..+ yy+ slopedata  + StochasticOscillator,data=train)
summary(model)
predict(model, type="probs", data.frame(test))