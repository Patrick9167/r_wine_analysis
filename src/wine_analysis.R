#Hierarchical model
install.packages("ggplot2")
df1 <- read.csv("https://www.scss.tcd.ie/~arwhite/Teaching/CS7DS3/school_compare_2.csv")

df1$school <- factor(df1$school)
dim(df1)

head(df1)
library(ggplot2)
ggplot(df1) + geom_boxplot(aes(school, score, fill = school)) + geom_jitter(aes(school, score, shape = school))

##### WINE REVIES ASSIGNMENT
setwd("~/TCD 5th Year/ASM/asm/assign_main")
setwd("~/Documents/fifth/asm/assign_main")
wr <- read.csv("wine-reviews/winemag-data_first150k.csv")

# isolate chile/chard and sauv/saf
sb_sa<-wr[wr$variety=="Sauvignon Blanc",]
sb_sa<-sb_sa[sb_sa$country=="South Africa",]
c_c<-wr[wr$variety=="Chardonnay",]
c_c<-c_c[c_c$country=="Chile",]

## ASSUMPTION: only pure chardonnay and sauvignon blanc, no variations
# all chile/chard and sauv/saf wines (all prices)
x<-rbind(sb_sa,c_c)
x<-x[,c(2,5,6,10)]
x<-x[complete.cases(x),]

ggplot(x)+geom_boxplot(aes(country, points, fill=country)) + geom_jitter(aes(country, points, fill=country))

# 15 euro
x_15 <- x[x$price==15,]
head(x_15)
dim(x_15)
x_15
ggplot(x_15, na.rm=TRUE)+geom_boxplot(aes(country, points, fill=country),na.rm=TRUE) + geom_jitter(aes(country, points, fill=country), na.rm=TRUE)

mean((c_c[c_c$price==15,])$points, na.rm=TRUE)
tapply(x_15$points, x_15$variety, mean)
#Sauvignon blanc = 86.36842
# 15 euro = 86.812
#Chardonnay = 85.24601
#15 euro = 85.0727

tapply(x_15$points, x_15$country, median, na.rm=TRUE)
#Sauvignon blanc = 86
# 15 euro = 87
#Chardonnay = 85
#15 euro = 85

tapply(x_15$points, x_15$country, sd)
#Sauvignon blanc = 1.931054
# 15 euro = 1.817834
#Chardonnay = 2.295566
#15 euro = 2.774584

# T test
t.test(points~country, data=x_15, var.equal = TRUE)

## Bayesian testing

compare_2_gibbs <- function(y, ind, mu0=mean(x_15$points), tau0 = 1/25, del0 = 0, gamma0 = 1/25,
                            a0 = 1, b0 = 10, maxiter = 5000)
  
{
  y1 <- y[ind == "Sauvignon Blanc"]
  y2 <- y[ind == "Chardonnay"]
  n1 <- length(y1)
  n2 <- length(y2)
  ##### starting values
  mu <- (mean(y1) + mean(y2)) / 2
  del <- (mean(y1) - mean(y2)) / 2
  mat_store <- matrix(0, nrow = maxiter, ncol = 3)
  #####
  ##### Gibbs sampler
  an <- a0 + (n1 + n2)/2
  for(s in 1 : maxiter)
  {
    ##update tau
    bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2))
    tau <- rgamma(1, an, bn)
    ##
    ##update mu
    taun <- tau0 + tau * (n1 + n2)
    mun <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / taun
    mu <- rnorm(1, mun, sqrt(1/taun))
    ##
    ##update del
    gamman <- tau0 + tau*(n1 + n2)
    deln <- ( del0 * tau0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamman
    del<-rnorm(1, deln, sqrt(1/gamman))
    ##
    ## store parameter values
    mat_store[s, ] <- c(mu, del, tau)
  }
  colnames(mat_store) <- c("mu", "del", "tau")
  return(mat_store)
}
install.packages("MCMCpack")
library(MCMCpack)
fit<- compare_2_gibbs(x_15$points, as.factor(x_15$variety))

plot(as.mcmc(fit))

mu <- fit[,1]
sigma <- 1/sqrt(fit[,3])
d <- fit[,2]

y1_sim <-rnorm(5000, mu + d, sd = sigma)
y2_sim <-rnorm(5000, mu - d, sd = sigma)

mean(y1_sim>y2_sim) # Probability a Sav Blanc will be better than Chard


m <- mean(part1_data$points)
mean((part1_data$points - m) ^ 2)

############################~~~TODO~~~~~~~~#############################################


# Italian wine reviews
library(plyr)

# Charcteristics of whole datset
mean(wr$points) ## 87.88842
median(wr$points) ## 88
sd(wr$points) ## 3.222

#### Italian and under 20
ita_wine <- wr[wr$country=="Italy",]
ita_u20 <- ita_wine[ita_wine$price<=20,]
ita_u20_nonull<-ita_u20[complete.cases(ita_u20),]

italian.wines <- subset(wr, price <= 20 & country == 'Italy')
italian.wines <- italian.wines[!table(italian.wines$region_1)[italian.wines$region_1] < 4,]
italian.wines <- data.frame(region <- italian.wines$region_1, points <- italian.wines$points)
italian.wines <- italian.wines[complete.cases(italian.wines), ]
italian.wines <- italian.wines[!(italian.wines$region == ""), ]
colnames(italian.wines) <- c('region', 'points')
italian.wines$region <- droplevels(italian.wines$region)

## Get unique regions and the # of reviews
out<-matrix(ncol=2,nrow=276)
colnames(out)<-c("region","reviews")
i<-1
for(regs in unique(ita_u20_nonull$region_1)){
  if(sum(ldply(ita_u20_nonull$region_1, function(c) sum(c==regs)))>=100){
    out[i,1]<-regs
    out[i,2]<-as.numeric(sum(ldply(ita_u20_nonull$region_1, function(c) sum(c==regs))))
  }
  i<-i+1
}
out
unique_regions<-out[complete.cases(out),]
unique_regions

## Get average points and prices for the unique regions
i<-1
out2<-matrix(ncol=2,nrow=nrow(unique_regions))
colnames(out2)<-c("points","price")
for(regions in unique_regions[,1]){
  sub<-x[x$region_1==regions,]
  out2[i,1]=as.numeric(mean(sub$points))
  out2[i,2]=as.numeric(mean(sub$price))
  i<-i+1
}

## Bind to dataframe
d<-cbind(unique_regions,out2)
d_df<-as.data.frame(d)
d_df
## z = all rows in original dataset that are from a unique region
z<-subset(x, region_1 %in% unique_regions)

## Get points and region
test<-z[c(5,8)]
unique(test$region_1)

## display boxplot and jitter with overall mean
ggplot(d_df) + geom_boxplot(aes("Wines",as.numeric(points), fill="red"),  show.legend = FALSE) + geom_jitter(aes("Wines",as.numeric(points)), show.legend = FALSE) + geom_hline(yintercept=mean(wr$points),color="blue", size=1) 

abv_avg <- d_df[as.numeric(d_df$points)>=mean(wr$points),]

dim(abv_avg)


####################### GIBBS SAMPLE #############################################


compare_m_gibbs <- function(y, ind, mu0 = mean(wr$points), tau0 = 1/400,
                            a0 = 1/2, b0 = 15, alpha0 =1/2, beta0 = 15, maxiter = 5000)
{
  ### starting values
  m <- nlevels(ind)
  ybar <- theta <- tapply(y, ind, mean)
  tau_w <- 1/mean(tapply(y, ind, var)) ##within group precision
  mu <- mean(theta)
  tau_b <-var(theta) ##between group precision (maybe 1 over sqrt of theta)
  n_m <- tapply(y, ind, length)
  alphan <- alpha0 + sum(n_m)/2
  ###
  ### setup MCMC
  theta_mat <- matrix(0, nrow=maxiter, ncol=m)
  mat_store <- matrix(0, nrow=maxiter, ncol=3)
  ###
  ### MCMC algorithm
  
  for(s in 1:maxiter)
  {
    # sample new values of the thetas
    for(j in 1:m)
    {
      taun <- n_m[j] * tau_w + tau_b
      thetan <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / taun
      theta[j]<-rnorm(1, thetan, 1/sqrt(taun))
    }
    #sample new value of tau_w
    ss <- 0
    for(j in levels(ind)){
      ss <- ss + sum((y[ind == j] - theta[j])^2)
    }
    betan <- beta0 + ss/2
    tau_w <- rgamma(1, alphan, betan)
    #sample a new value of mu
    taum <- m * tau_b + tau0
    mum <- (mean(theta) * m * tau_b + mu0 * tau0) / taum
 #   print(mean(theta))
    mu <- rnorm(1, mum, 1/ sqrt(taum))
    # sample a new value of tau_b
    am <- a0 + m/2
    bm <- b0 + sum((theta - mu)^2) / 2
    tau_b <- rgamma(1, am, bm)
    #store results
    theta_mat[s,] <- theta
    mat_store[s, ] <- c(mu, tau_w, tau_b)
    
  }
  colnames(mat_store) <- c("mu", "tau_w", "tau_b")
  return(list(params = mat_store, theta = theta_mat))
}

fit2 <- compare_m_gibbs(test$points, test$region_1)
fit2 <- compare_m_gibbs(as.numeric(d_df$points), d_df$region)
fit2 <- compare_m_gibbs(italian.wines$points, italian.wines$region) ## Do not run this unless you're reaedy to wait for a long time


apply(fit2$params, 2, mean)

apply(fit2$params, 2, sd)

mean(1/sqrt(fit2$params[, 2]))

sd(1/sqrt(fit2$params[, 2]))

mean(1/sqrt(fit2$params[, 3]))

sd(1/sqrt(fit2$params[, 3]))


theta_hat <- apply(fit2$theta, 2, mean) ## get basic posterior summary
names(theta_hat) <- 1:100 ## keep track of different schools
sort(theta_hat, decreasing = TRUE) ## which schools did best and worst?


#a=trust data
#b=deviation



############################
############################
############################
############################
####### question 2 #########
###### clustering  #########
############################
############################
############################
### US wines - points and price
install.packages("mclust")
library(mclust)

wr
usa <- wr[wr$country=="US",]
usa
usa_pp <- usa[,c(5,6)]
usa_pp_nonull <- usa_pp[complete.cases(usa_pp),]
usa_trimmed <- usa_pp_nonull[usa_pp_nonull$price<400,]
dim(usa_trimmed)
plot(usa_trimmed)

# Form clusters and summaries
fit<-Mclust(usa_trimmed)
plot(fit, what="classification")
plot(fit, what = "uncertainty")
head(fit$classification) ## optimal assignment of z
fit$parameters$mean

# BIC information for G1:9
plot(fit, what="BIC")
fit$BIC

# G 10:19
fit_more_g<- Mclust(usa_trimmed, G=10:19)
plot(fit_more_g, what = "classification")
plot(fit_more_g,what="BIC")
fit_more_g$BIC  
fit_more_g$parameters$mean

# G 1:25
fit_25<- Mclust(usa_trimmed, G=1:25)
plot(fit_25, what = "classification")
plot(fit_25,what="BIC")
fit_25$BIC
fit_25$parameters$mean

# G 1:20
fit<-Mclust(usa_trimmed, prior = priorControl(), G=1:20)
plot(fit, what = "classification")
plot(fit,what="BIC")
fit$BIC
fit$parameters$mean

### selecting different groupings
fit2<- Mclust(usa_trimmed, G=2, modelNames="VVE")
fit_more_g$BIC
table(fit_more_g$classification, fit2$classification)
plot(fit2, what = "uncertainty")




library(ggplot2)
# library(cowplot)
library(dplyr)
install.packages("sigmoid")
library(sigmoid)
line = function(x) {
  1 / (1 + exp(-x))
}

x <- seq(-5, 5, 0.01)

plot(x, relu(x), col='black')
abline(h=0.0, lty=2)
abline(v=0, lty=2)
