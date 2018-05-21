


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
      # %K = (P(x) – Ln)/ (Hn – Ln) x 100%..
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

Stochastic_Oscillator_Numeric  = function (data, n ){
  
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
      # %K = (P(x) – Ln)/ (Hn – Ln) x 100%..
      pa = px -xdata[Ln]
     
      na = xdata[Hn]-xdata[Ln]
     
      ma= pa/na
      
      StochasticOscillator[i] = ma *100
      if (is.na(StochasticOscillator[i]))
        next
      if (StochasticOscillator[i] < 20)
        BuySellFlag[i]= 1
      else 
        BuySellFlag[i]= -1
      
      if (StochasticOscillator[i] > 80)
        BuySellFlag[i]= 0
    }
    else {
      BuySellFlag[i]= -1
    }
  }
  
  SX = data.frame(StochasticOscillator,BuySellFlag )
  return(SX)
}

# Function to predict multinomial logit choice model outcomes
# model = nnet class multinomial model
# newdata = data frame containing new values to predict
predictMNL <- function(model, newdata) {
  
  # Only works for neural network models
  if (is.element("nnet",class(model))) {
    # Calculate the individual and cumulative probabilities
    probs <- predict(model,newdata,"probs")
    cum.probs <- t(apply(probs,1,cumsum))
    
    # Draw random values
    vals <- runif(nrow(newdata))
    
    # Join cumulative probabilities and random draws
    tmp <- cbind(cum.probs,vals)
    
    # For each row, get choice index.
    k <- ncol(probs)
    ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
    
    # Return the values
    return(ids)
  }
}

UpdateModel <- function(model, data){
  modelupdated <- update(model, "BuySellFlag ~ goldpricesAllUp..USD..AM..+ yy+ slopedata  + StochasticOscillator", data)
  return(modelupdated)
}
