
library(Quandl)
library(devtools)
library(TTR)
library(plotly)
library(clusterSim)
library(lubridate)



#API Key for Quandl
Quandl.api_key("Td2oA_m_SYUdi1X9Htdi")

goldpricesAllUp =  Quandl("LBMA/GOLD", type = "raw", collapse="monthly")
oilprices =    Quandl("OPEC/ORB", type = "raw", collapse="monthly")
# Merge oil and gold datasets
setOne = merge(goldpricesAllUp, oilprices, by="Date")
setOne= setOne[complete.cases(setOne), ]

#Filter the data frame to last 3 years only 
setOne <- setOne[(year(setOne$Date) >= 2010)  , ]

dataSize = dim(setOne)[1]
traindata = setOne[1:round(0.8*dataSize),]
starttestfrom = round(0.8*dataSize) +1
testdata = setOne[starttestfrom:dataSize,]

p <- plot_ly(setOne, x = setOne$`USD (AM)`, y = setOne$Value, type = 'scatter', mode = 'lines')%>%
layout(title = "Oil vs Gold Prices",  xaxis = list(title = "Gold Price"), yaxis = list (title = "Oil Price"))

oilmodel = lm(Value~`USD (PM)`, traindata)
abline(oilmodel)
paste('y =', coef(oilmodel)[[2]], '* x', '+', coef(oilmodel)[[1]])
lmPred = predict(oilmodel, testdata)


actuals_preds <- data.frame(cbind(actuals=testdata$Value, predicteds=lmPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
correlation_accuracy

# NLS
x <- traindata$`USD (AM)`
y <- traindata$Value

nformula = as.formula(y~a+b1*x+b2*z+b3*w)
library(nlstools)
z=traindata$`USD (AM)`^2
w= traindata$`USD (AM)`^3
xm<-nls(nformula,start=list(a=945,b1=-2.11,b2=-92.33,b3=-9.16))
overview(xm)
a<-coef(xm)[1]
b1<-coef(xm)[2]
b2<-coef(xm)[3]
b3<-coef(xm)[4]
plot(y~x)

lines(x,a+b1*x+b2*z+b3*w,col='green')
