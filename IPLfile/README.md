# IPL
https://github.com/Jaan-17/IPL/blob/main/IPL%20Ball-by-Ball%202008-2020.csv
IPL Ball-by-Ball 2008-2020.csv
df = read.csv("IPL Ball-by-Ball 2008-2020.csv",header = TRUE)
View(df)
names(df)
dim(df)

## Wickets are of discrete distribution.
## There are Poisson, uniform, geometric negative binomial
## subsetting the total wickets and bowlers
wkt = df %>%
  group_by(bowler, id)%>%
  summarize(wkt = sum(is_wicket))
unique(wkt$bowler)
dim(wkt)
## assigned bowler is TA Boult
boult = wkt[wkt$bowler == 'TA Boult',]
boult
## Plotting the histogram
hist(boult$wkt,freq=F,main="TA Boult")
m=mean(boult$wkt);std=sd(boult$wkt);
m;std
sum(boult$wkt)
lines(density(boult$wkt),col="blue")

# For discrete data
## Let the null hypothesis,Ho = The wickets of the bowler fits Poisson distribution
##  The alternative hypothesis,H1 = The wickets doesnot follow Poisson distribution 
library(vcd) 
gf.boult <- goodfit(boult$wkt, type = "poisson", par = NULL)
summary(gf.boult)
plot(gf.boult, main="Boult wicket distribution")
## The result P value is greater than 0.05 , hence we accept the null hpyothesis, thus the wickets fits poisson distribution


## Subsetting the batsman runs and batsman names
library(dplyr)
totalrun  = df %>%
  group_by(batsman, id)%>%
  summarize(runs = sum (batsman_runs))
View(totalrun)
unique(totalrun$batsman)

## the taken batsman for the analysis MP Stoinis
MP = totalrun[totalrun$batsman == 'MP Stoinis',]
MP
unique(MP$runs)
sum(MP$runs)
mean(MP$runs);sd(MP$runs)
hist(MP$runs, 10)
library(fitdistrplus)
descdist(MP$runs)
descdist(MP$runs , discrete = FALSE, boot=1000)
plotdist(MP$runs, histo = TRUE, demp = TRUE)

## Various continous distributions are normal, gamma, beta, weibull, lognormal, logistic,etc..

## Ho = the runs fits lognormal distributions
## H1 = The runs does not fits lognormal distributions
dis1<-fitdistr(MP$runs+.001,"Lognormal",lower=0.001)
dis1$estimate
ks.test(MP$runs,"plnorm",meanlog=1.716550 ,sdlog= 2.999861)
## the resultant p value is 0.004097 < 0.05 hence we reject the Ho. ie that doesnot fit in lognormal distribution.

## Ho = the runs fits exponential distributions
## H1 = The runs does not fits exponential distributions
dis2<-fitdistr(MP$runs+0.001,"Exponential",lower=0.001)
did2$estimate
ks.test(MP$runs,"exp")
## the resultant p value is 2.2e-16 < 0.05 hence we reject the Ho. ie that doesnot fit in exponential distribution.

## Ho = the runs fits gamma distributions
## H1 = The runs does not fits gamma distributions
dis3<-fitdistr(MP$runs+0.00001,"gamma",lower=0.001)
dis3$estimate
ks.test(MP$runs+0.00001,"gamma",shape=0.43802526  ,rate= 0.01979701 )
## here also the distribution doesnot fit the gamma distribution

## Ho = the runs fits weibull distributions
## H1 = The runs does not fits weibull distributions
dis4<-fitdistr(MP$runs+0.00001,"weibull",lower=0.001)
dis4$estimate
ks.test(MP$runs,"pweibull",  shape=0.5372375 ,scale=15.2324984 )
## the resultant p value is 0.03446 < 0.05 hence we reject the Ho. ie the data doesnot fit in weibull distribution.

## Ho = the runs fits normal distributions
## H1 = The runs does not fits normal distributions
dis5<-fitdistr(MP$runs+0.00001,"normal",lower=0.001)
dis5$estimate
mpnorm = rnorm(1000,  mean=19.64287 ,sd= 17.15035)
plot(density(mpnorm))
ks.test(MP$runs,"pnorm",mean=19.64287,sd= 17.15035 )
## the resultant p value is 0.3384 > 0.05 hence we accept the Ho. 
## the data does fit in the normal distribution.1e the runs of the batsma is a normal distribution.








