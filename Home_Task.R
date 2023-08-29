### Home Work of  this "22-08-2023" day ####
## Write the function of following terms:
#1. Arithmetic, Geometric and Harmonic Mean
#2. Sample Variance
#3. Moments, Skewness, Kurtosis

data1 <- c(42.5,67.5,92.5,117.5,142.5,167.5,192.5)
freq1 <- c(3,4,6,9,12,11,5)


## Function of Arithmetic mean##
AR <- function(x,f=rep(1,times=length(x))){
  len <- length(x)
  sum <- 0; n <- 0
  for(i in 1:len){
    sum <- sum + x[i]*f[i]
    n <- n+f[i]
  }
  mean <- round(sum/n,3)
  return(mean)
}
AR(data1,freq1)



## Function of Geometric mean ##
GM1 <- function(x,f=rep(1,times=length(x))){
  len <- length(x)
  product <- 1; n <- 0
  for(i in 1:len){
    product <- product*(x[i]^f[i])
    n <- n+f[i]
  }
  mean <- round(product^(1/n),3)
  return(mean)
}
GM1(data1,freq1)


## Another Way:
GM2 <- function(x,f=rep(1,times=length(x))){
  len <- length(x)
  sum <- 0; n<- 0
  for(i in 1:len){
    sum <- sum + f[i]*log(x[i],10)
    n <- n + f[i]
  }
  mean <- 10^(sum/n)
  return(round(mean,3))
}
GM2(data1,freq1)



### Function of Harmonic Mean ###
HM1<- function(x,f=rep(1,times=length(x))){
  len <- length(x)
  sum <- 0; n <- 0
  for(i in 1:len){
    sum <- sum + f[i]/x[i]
    n<- n + f[i]
  }
  aver <- sum/n
  mean <- round(1/aver,3)
  return(mean)
}
HM1(data1,freq1)



## Another Way:
HM2 <- function(x,f=rep(1,times=length(x))){
  len <- length(x)
  sum <- 0; n <- 0
  for(i in 1:len){
    sum <- sum + f[i]/x[i]
    n <- n + f[i]
  }
  mean <- round(n/sum,3)
  return(mean)
}
HM2(data1,freq1)



### Function of Sample Variance ###
data2 <- c(42.5,67.5,92.5,117.5, 142.5, 167.5,192.5)
freq2 <- c(3,4,6,9,12,11,5)

Sample_var <- function(x,f=rep(1,times=length(x))){
  len <- length(x)
  n <- 0 ; sum <- 0 ; ssx <-0
  for(i in 1:len){
    ssx <- ssx + f[i]*(x[i]^2)
    sum <- sum + f[i]*x[i]
    n <- n + f[i]
  }
  mean <- (1/(n-1))*(ssx-(sum^2)/n)
  return(round(mean,3))
}
Sample_var(data2,freq2)



### Function of Moments about origin##
data3 <- c(25,35,45,55,65,75)
freq3 <- c(10,22,42,15,6,5)

Moment_Origin <- function(x,r,f= rep(1,times=length(x))){
  len <- length(x)
  sum <- 0; n <- 0
  for(i in 1:len){
    sum <- sum + f[i]*(x[i]^r)
    n <- n + f[i]
  }
  moment <- round(sum/n,3)
  return(moment)
}
Moment_Origin(data3,3,freq3)



## Function of Central Moment##
Central_Moment <- function(x,r,f=rep(1,times=length(x))){
  len <- length(x)
  sum <- 0; n <- 0
  for(i in 1:len){
    sum <- sum + f[i]*x[i]
    n <- n + f[i]
  }
  x_bar <- sum/n
  ssx <- 0
  for(i in 1:len){
    ssx <- ssx + f[i]*((x[i]-x_bar)^r)
  }
  moment <- round(ssx/n,5)
  
  return(moment)
}
Central_Moment(data3,4,freq3)



### Function of Skewness ###
data4 <- c(0,18,19,20,21,22,23,24,25,26,27)
freq4 <- c(1,1,2,1,8,4,5,5,3,1,1)

Skewness1 <- function(x,f=rep(1,times=length(x))){
  mu3 <- Central_Moment(x,3,f)
  mu2 <- Central_Moment(x,2,f)
  skew<- mu3/(mu2^(3/2))
  return(round(skew,3))
}
Skewness1(data4,freq4)



### Another Way
Skewness2 <- function(x,f=rep(1,times=length(x))){
  len <- length(x)
  sum <- 0; n <- 0
  for(i in 1:len){
    sum <- sum + f[i]*x[i]
    n <- n + f[i]
  }
  x_bar <- sum/n
  ssx3 <- 0; ssx2 <- 0
  for(i in 1:len){
    ssx3 <- ssx3 + f[i]*((x[i]-x_bar)^3)
    ssx2 <- ssx2 + f[i]*((x[i]-x_bar)^2)
  }
  mu3 <- ssx3/n; mu2 <- ssx2/n
  skew <- mu3/(mu2^(3/2))
  return(round(skew,3))
}
Skewness2(data4,freq4)



### Function of Kurtosis ###
Kurtoris <- function(x,f=rep(1,times=length(x))){
  mu4 <- Central_Moment(x,4,f)
  mu2 <- Central_Moment(x,2,f)
  kurto <- round(mu4/(mu2^2),3)
  return(kurto-3)
}
Kurtoris(data4,freq4)

### Another Way
Kurtosis2 <- function(x,f=rep(1,times=length(x))){
  len <- length(x)
  sum <- 0; n <- 0
  for(i in 1:len){
    sum <- sum + f[i]*x[i]
    n <- n + f[i]
  }
  x_bar <- sum/n
  ssx4 <- 0; ssx2 <- 0
  for(i in 1:len){
    ssx4 <- ssx4 + f[i]*((x[i]-x_bar)^4)
    ssx2 <- ssx2 + f[i]*((x[i]-x_bar)^2)
  }
  mu4 <- ssx4/n; mu2 <- ssx2/n
  kurto <- round(mu4/(mu2^2),3)
  return(kurto-3)
}
Kurtosis2(data4,freq4)



#### Function of Correlation ####
x1 <- c(112, 116, 103, 116, 98, 118, 112, 104, 111, 105)
y1 <- c(65, 69, 60, 68, 56, 72, 60, 53, 64, 62)

Corr <- function(x,y){
  len_x <- length(x); len_y <- length(y)
  if(len_x==len_y){
    sum_x <- 0; sum_y <- 0; ssx<- 0; ssy <-0; spxy <- 0
    for(i in 1:len_x){
      ssx <- ssx + x[i]^2
      ssy <- ssy + y[i]^2
      spxy <- spxy + x[i]*y[i]
      sum_x <- sum_x +x[i]
      sum_y <- sum_y +y[i]
    }
    numerator <- spxy-((sum_x*sum_y)/len_x)
    denominator <- sqrt((ssx-(sum_x^2)/len_x)*(ssy-(sum_y^2)/len_y))
    corr <- round(numerator/denominator, 3)
    return(corr)
  }
  else{
    return("Sorry! Length of x and y doesn't match.")
  }
}
Corr(x1,y1)




#### Function of Regression ###
x2 <- c(56,42,36,47,49,42,60,72,63,55)
y2 <- c(147,125,118,128,145,140,155,160,149,150)

Reg <- function(x,y){
  if(length(x)==length(y)){
    spxy <- 0; ssx <- 0; sum_x <- 0; sum_y <- 0; n <- length(x)
    for(i in 1:n){
      spxy <- spxy + x[i]*y[i]
      ssx <- ssx + x[i]^2
      sum_x <- sum_x + x[i]
      sum_y <- sum_y + y[i]
    }
    num <- spxy - ((sum_x*sum_y)/n)
    deno <- ssx - ((sum_x^2)/n)
    coeff <- round(num/deno, 3)
    inter <-round((sum_y/n)- coeff*(sum_x/n),3)
    result <- list(Intercept.=inter,Coefficient.=coeff)
    return(result)
  }
  else{
    return("Sorry! length of x and y doesn't equal.")
  }
}
Reg(x2,y2)



## Factorial program
Factorial <- function(n){
  if(n>0){
    fact <- 1
    for(i in 1:n){
      fact <- fact*i
    }
    return(fact)
  }
  else if(n==0){
    return(1)
  }
  else{
    print("Sorry! Factorial does not exits for this number.")
  }
}
Factorial(5)

## Permutation & Combination er program:
Permutation <- function(n,r){
  if(n>=r){
    npr <- Factorial(n)/Factorial(n-r)
    return(npr)
  }
  else{
    return("Sorry! Permutation can't be determine because r is larger than n.")
  }
}
Permutation(4,4)

Combination <- function(n,r){
  if(n>=r){
    ncr <- Factorial(n)/(Factorial(r)*Factorial(n-r))
    return(ncr)
  }
  else{
    return("Sorry! Permutation can't be determine because r is larger than n.")
  }
}
Combination(5,0)
