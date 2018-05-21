
library(forecast)

mydata = read.csv("c:/start/ML-Regression-Analysis/gd2.csv", stringsAsFactors = FALSE, header=TRUE,sep=",");
fit = lm(USD~.,mydata)
plot(fit)



