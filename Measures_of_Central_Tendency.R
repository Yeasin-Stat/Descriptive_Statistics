## Length Function:
Length <- function(x){
  count <- 0
  for(i in x){
    count <- count+1
  }
  return(count)
}
data5 <- c(4.9,4.1,4.4,3.3,4.6,4.8)
Length(data5)
#Maximum Function:
Max <- function(x){
  len <- Length(x)
  max <- x[1]
  for(i in 1:len){
    if(x[i]>max){
      max <- x[i]
    }
  }
  return(max)
}
Max(data5)

#Minimum Function:
Min <- function(x){
  len <- Length(x)
  min <- x[1]
  for(i in 1:len){
    if(x[i]<min){
      min <- x[i]
    }
  }
  return(min)
}
Min(data5)

## Summation Function:
Summation <- function(x){
  sum <- 0
  len <- Length(x)
  for (i in 1:len){
    sum <- sum + x[i]
  }
  return(sum)
}
Summation(data5)


### Measures of Central Tendency or Location:###

## Arithmetic Mean Function:
Arithmetic_mean <- function(x){
  sum <- 0
  len <- Length(x)
  for (i in 1:len){
    sum <- sum + x[i]
  }
  aver <- sum/len
  return(aver)
}
Arithmetic_mean(data5)

## First function to find Geometric Mean
Geometric_mean1 <- function(x){
  len <- Length(x)
  product <- 1
  for(i in 1:len){
    product <- product * x[i] 
  }
  mean <- product^(1/len)
  return(mean)
}
data1 <- c(2,4,8,12,16,24)
Geometric_mean1(data1)


## Second function to find Geometric Mean
Geometric_mean2 <- function(x){
  len <- Length(x)
  log_gm <- Summation(log(x,10))/len
  geom_mean <- 10^log_gm
  return(geom_mean)
}
Geometric_mean2(data1)

## Harmonic mean 1st function:
Harmonic_mean1 <- function(x){
  len <- Length(x)
  summ <- 0
  for(i in 1:len){
    summ <- summ + 1/x[i]
  }
  aver <- summ/len
  har_mean <- 1/aver
  return(har_mean)
}
data2 <- c(125, 130, 75, 10,45, 5, 0.50, 0.40, 500, 150)
Harmonic_mean1(data2)

## Harmonic mean 2nd function : 
Harmonic_mean2 <- function(x){
  len <- Length(x)
  summ <- Summation(1/x)
  har_mean <- len/summ
  return(har_mean)
}
Harmonic_mean2(data2)

## Weighted Arithmetic Mean:
Weighted_mean <- function(x,w){
  weighted_sum <- 0
  len <- Lenghth(x)
  total_wight <- Summation(w)
  for(i in 1:len){
    weighted_sum <-weighted_sum+ x[i]*w[i]
  }
  weighted_mean <- weighted_sum/total_wight
  return(weighted_mean)
}
Grade<- c(4,3.75,3.75,4,4,4,4,4,4,4)
Credit <-c(3,2,3,3,3,2,1.5,1.5,1,2)
Weighted_mean(Grade, Credit)


## Median Function:

Median <- function(x){
  sorted_x <- sort(x)
  len <- Length(sorted_x)
  if(len%%2==0){
    median <- (sorted_x[len/2] + sorted_x[(len/2)+1])/2
  }
  else{
    median <- sorted_x[(len+1)/2]
  }
  return(median)
}
data3 <- c(3,5,2,8,4,10,14,16,24,74)
Median(data3)

## Mode Function:
Mode <- function(x){
  unique_x <- unique(x)
  table <- tabulate(match(x,unique_x))
  mode <- unique_x[table==max(table)]
  return(mode)
}
dat <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
Mode(dat)

## Quartiles Function:
Quartiles <- function(x,i){
  sort_x <- sort(x)
  len <- Lenghth(x)
  j <- (i*len)/4
  if(j==round(j)){
    quartile <- (sort_x[j] + sort_x[j+1])/2
  }
  else{
    quartile <- sort_x[ceiling(j)] 
  }
  return(quartile)  
}

data4 <- c(7850, 7950, 8050,7880, 7755, 7710, 7890, 8130, 7940, 8325, 7920, 7880)
Quartiles(data4,1)
Median(data4)

## Deciles Function:
Deciles <- function(x,i){
  sort_x <- sort(x)
  len <- Length(x)
  j <- (i*len)/10
  if(j==round(j)){
    decile <- (sort_x[j] + sort_x[j+1])/2
  }
  else{
    decile <- sort_x[ceiling(j)] 
  }
  return(decile)
}
data4 <- c(7850, 7950, 8050,7880, 7755, 7710, 7890, 8130, 7940, 8325, 7920, 7880)
Deciles(data4,1)


## Percentiles Function:
Perceltiles <- function(x,i){
  sort_x <- sort(x)
  len <- Length(x)
  j <- (i*len)/100
  if(j==round(j)){
    percentile <- (sort_x[j] + sort_x[j+1])/2
  }
  else{
    percentile <- sort_x[ceiling(j)] 
  }
  return(percentile)
}
Perceltiles(data4,80)

## Others Measures of Average:
## Quadratic Mean Function:

Quadratic_mean <- function(x){
  len <- Length(x)
  sum <- 0
  for(i in 1:len){
    sum <- sum + x[i]^2
  }
  mean <- sqrt(sum/len)
  return(mean)
}
y <- c(1,3,4,5,7)
Quadratic_mean(y)

## Trimean Function:
Trimean <- function(x){
  Q1 <- Quartiles(x,1)
  Q2 <- Quartiles(x,2)
  Q3 <- Quartiles(x,3)
  tri_mean <- (Q1+(2*Q2)+Q3)/4
  return(tri_mean)
}
Trimean(data4)

## Trimmed Mean Function:
Trimmed_mean <- function(x,trim){
  lower_trim <- Perceltiles(x,trim)
  upper_trim <- Perceltiles(x,(100-trim))
  len <- Length(x)
  new_data <- NULL
  for(i in 1:len){
    if(x[i]> lower_trim & x[i] < upper_trim){
      new_data <- c(new_data,x[i])
    }
  }
  trimmed_mean <- Arithmetic_mean(new_data)
  return(trimmed_mean)
}
data6 <- c(32,37,47,43,36,42,38,43,86,26)
Trimmed_mean(data6,10)

### 5 number Summary Function:
Summary <- function(x){
  min <- Min(x)
  Q1 <- Quartiles(x,1)
  median <- Median(x)
  Q3 <- Quartiles(x,3)
  max <- Max(x)
  summary <- data.frame(Min=min, Q1=Q1,Med=median,
                        Q3=Q3, Max=max)
  print(summary,row.names=FALSE)
}
x <- c(12, 18, 10, 15, 8, 11, 19, 17, 20, 15, 18)
Summary(x)

