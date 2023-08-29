## Range Function:
Range <- function(x){
  range <- Max(x)-Min(x)
  return(range)
}
Range(data5)
data1 <- c(10,20,30,40,50,60,70)
Range(data1)

## Coefficient of Range Function:
Coef_of_Range <- function(x){
  Largest <- Max(x)
  Smallest <- Min(x)
  coeff_of_range <- ((Largest-Smallest)/(Largest+Smallest))*100
  return(paste(coeff_of_range,"%"))
}
Coef_of_Range(data1)

## Mean Deviation Function:

Mean_Deviation <- function(x,from="mean"){
  len <- Length(x)
  if(from=="mean" ||from==1 ){
    mean <- Arithmetic_mean(x)
    deviation <- abs(x-mean)
    mean_deviation <- Summation(deviation)/len
    return(mean_deviation)
  }
  else if(from=="median"||from==2){
    median <- Median(x)
    deviation <- abs(x-median)
    mean_deviation <- Summation(deviation)/len
    return(mean_deviation)
  }
  else if(from=="mode"||from==3){
    mode <- Mode(x)
    deviation <- abs(x-mode)
    mean_deviation <- Summation(deviation)/len
    return(mean_deviation)
  }
  else{
    return("Sorry! Please enter variable(vector) and deviation names as 'mean','median','mode', or, 1,2,3")
  }
  
}
data <- c(7,4,10,9,15,12,7,9,7)
Mean_Deviation(data,1)



## Coefficient of Mean Deviation Function:
Coef_of_Mean_Deviation <- function(x,from="mean"){
  if(from=="mean" || from==1){
    coeff <-(Mean_Deviation(x,"mean")/Arithmetic_mean(x))*100
    return(coeff)
  }
  else if(from=="median" || from==2){
    coeff <- (Mean_Deviation(x,"median")/Median(x))*100
    return(coeff)
  }
  else if(from=="mode" || from==3){
    coeff <- (Mean_Deviation(x,"mode")/Mode(x))*100
    return(coeff)
  }
  else{
    return("Sorry! Please enter variable(vector) and deviation names as 'mean','median','mode', or, 1,2,3")
  }
}
Coef_of_Mean_Deviation(data,"mode")

## Inter Quartile Range:
IQR <- function(x){
  Q1 <- Quartiles(x,1)
  Q3 <- Quartiles(x,3)
  return(Q3-Q1)
}
IQR(data)

## Quartile Deviation:
Quartile_Deviation <- function(x){
  Q1 <- Quartiles(x,1)
  Q3 <- Quartiles(x,3)
  quartile_deviation <- (Q3-Q1)/2
  return(round(quartile_deviation,3))
}
Quartile_Deviation(data)

## Coefficient of Quartiles Deviation:
Coef_of_Quart_Deviation <- function(x){
  Q1 <- Quartiles(x,1)
  Q3 <- Quartiles(x,3)
  coef_quart_deviation <- ((Q3-Q1)/(Q3+Q1))*100
  return(round(coef_quart_deviation,3))
}
Coef_of_Quart_Deviation(data)

## Sample_Variance Function
Sample_Variance <- function(x){
  len <- length(x)
  sum <- 0 ; ssx <-0
  for(i in 1:len){
    ssx <- ssx + x[i]^2
    sum <- sum + x[i]
  }
  mean <- (1/(len-1))*(ssx-(sum^2)/len)
  return(mean)
}
Sample_Variance(data2)

CV<- function(x){
  cv <- (sqrt(Sample_Variance(x))/ Arithmetic_mean(x))*100
  return(cv)
}
CV(c(4.9,4.1,4.4,3.3,4.6,4.8))
