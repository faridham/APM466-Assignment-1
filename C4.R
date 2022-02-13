
rm(list=ls())

library(readxl)
Bonds <- read_excel("~/Desktop/UofT/2021-22/WINTER 2022/APM466/Assignment 1/Data/Bonds?.xlsx", 
                    range = "A2:P13")

Bonds$ttm10 <- as.numeric(as.Date(Bonds$Maturity) - as.Date(as.character("2022/01/10"), format="%Y/%m/%d"))/365.25
Bonds$ttm11 <- as.numeric(as.Date(Bonds$Maturity) - as.Date(as.character("2022/01/11"), format="%Y/%m/%d"))/365.25
Bonds$ttm12 <- as.numeric(as.Date(Bonds$Maturity) - as.Date(as.character("2022/01/12"), format="%Y/%m/%d"))/365.25
Bonds$ttm13 <- as.numeric(as.Date(Bonds$Maturity) - as.Date(as.character("2022/01/13"), format="%Y/%m/%d"))/365.25
Bonds$ttm14 <- as.numeric(as.Date(Bonds$Maturity) - as.Date(as.character("2022/01/14"), format="%Y/%m/%d"))/365.25
Bonds$ttm17 <- as.numeric(as.Date(Bonds$Maturity) - as.Date(as.character("2022/01/17"), format="%Y/%m/%d"))/365.25
Bonds$ttm18 <- as.numeric(as.Date(Bonds$Maturity) - as.Date(as.character("2022/01/18"), format="%Y/%m/%d"))/365.25
Bonds$ttm19 <- as.numeric(as.Date(Bonds$Maturity) - as.Date(as.character("2022/01/19"), format="%Y/%m/%d"))/365.25
Bonds$ttm20 <- as.numeric(as.Date(Bonds$Maturity) - as.Date(as.character("2022/01/20"), format="%Y/%m/%d"))/365.25
Bonds$ttm21 <- as.numeric(as.Date(Bonds$Maturity) - as.Date(as.character("2022/01/21"), format="%Y/%m/%d"))/365.25

## 4a

ytmdata <- data.frame(matrix(, nrow = 11, ncol = 10))

for(i in 7:16){
  ytmdata[1, i-6] <- -log(Bonds[1,i]/(100+100*Bonds$Coupon[1]))/Bonds[1, i+10]
  for(j in 2:11){
    p_i <- Bonds$Coupon[j]*100
    f <- function(r){
      sum1 <- 0
      for(k in 1:(j-1)){
        sum1 <- sum1 - exp(-r*Bonds[k, i+10])
      }
      z = Bonds[j,i] + p_i*sum1 - (100+p_i)*exp(-r*Bonds[j, i+10])
    }
    df <- function(r){
      sum2 <- 0
      for(k in 1:(j-1)){
        sum2 <- sum2 + Bonds[k,i+10]*exp(-r*Bonds[k,i+10])
      }
      z = p_i*sum2 + (100+p_i)*Bonds[j,i+10]*exp(-r*Bonds[j,i+10])
    }
    x1 <- 0.1
    n <- 1
    while(n<10){
      n <- n+1
      y1 <- f(x1)
      y2 <- df(x1)
      x2 <- x1 - y1/y2
      x1 <- x2
    }
    ytmdata[j, i-6] <- x1
  }
}


colors = c("black", "blue", "red", "green", "yellow", "purple", "grey", "brown", "cornsilk", "cyan")

xaxis <- seq(0, 5, by = 0.5)
plot(xaxis, ytmdata$X1, type = "l", ylim = c(0,0.06), xlab="Maturity (in Years)", ylab="Yield To Maturity", col=colors[1], main="Yield Curve")
for(i in 2:10){
  lines(xaxis, ytmdata[1:11, i], col=colors[i])
}
legend(4, 0.025, cex=0.38, legend=c("10-Jan", "11-Jan", "12-Jan", "13-Jan", "14-Jan", "17-Jan", "18-Jan", "19-Jan", "20-Jan", "21-Jan"), col=colors, lty=1:1)

## 4b

for(i in 7:16){
  Bonds[1 , i+20] <- -log(Bonds[1 , i]/(100+100*Bonds[1,2]))/Bonds[1, i+10]
}

for(j in 2:11){
  for(i in 7:16){
    p_i <- Bonds$Coupon[j]*100
    sum <- 0
    for(k in 1:(j-1)){
      sum <- sum - exp(-Bonds[k, i+20]*Bonds[k, i+10])
    }
    Bonds[j, i+20] <- -log((Bonds[j, i] + p_i*sum)/(100+p_i))/Bonds[j, i+10]
  }
}

yields <- data.frame(matrix(, nrow = 11, ncol = 10))

for(i in 1:11){
  for(j in 1:10){
    yields[i,j] <- Bonds[i, j+26]
  }
}

plot(xaxis, yields[1:11, 1], ylim = c(0,0.06), type ="l", xlab = "Maturity (in Years)", ylab="Spot Rate", col=colors[1], main="Spot Curve")
for(i in 2:10){
  lines(xaxis, yields[1:11, i], col=colors[i])
}
legend(4, 0.025, cex=0.38, legend=c("10-Jan", "11-Jan", "12-Jan", "13-Jan", "14-Jan", "17-Jan", "18-Jan", "19-Jan", "20-Jan", "21-Jan"), col=colors, lty=1:1)

## 4c

forward <- data.frame(matrix(, nrow = 9, ncol = 11))

for(j in 1:9){
  for(i in 1:10){
    forward[j,i] <- (yields[j+2,i]*Bonds[j+2,i+16] - yields[j,i]*Bonds[j,i+16])/(Bonds[j+2,i+16]-Bonds[j,i+16])
  }
}

plot(xaxis[2:10], forward$X1, ylim = c(0,0.08), type="l", xlab="Maturity (in Years)", ylab="1 Year Forward Rate", col=colors[1], main="Forward Curve")
for(i in 2:9){
  lines(xaxis[2:10], forward[1:9,i], col=colors[i])
}
legend(4, 0.028, cex=0.38, legend=c("10-Jan", "11-Jan", "12-Jan", "13-Jan", "14-Jan", "17-Jan", "18-Jan", "19-Jan", "20-Jan", "21-Jan"), col=colors, lty=1:1)


## 5

logyield <- data.frame(matrix(, nrow = 5, ncol = 9))

for(i in 1:5){
  for(j in 1:9){
    logyield[i,j] <- log(ytmdata[((2*i)+1),j+1]/ytmdata[((2*i)+1),j])
  }
}
ly_cov <- cov(t(logyield))

logforward <- data.frame(matrix(, nrow = 5, ncol = 9))

for(i in 1:5){
  for(j in 1:9){
    logforward[i,j] <- log(forward[2*i-1,j+1]/forward[2*i-1,j])
  }
}

lf_cov <- cov(t(logforward))  

## 6

eigen(ly_cov)
eigen(lf_cov)


