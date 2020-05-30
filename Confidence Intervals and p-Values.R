# Geom Smooth Confidence Interval #
data("nhtemp")
df<-data.frame(year=as.numeric(time(nhtemp)),temperature=as.numeric(nhtemp))
library(ggplot2)
ggplot(aes(year,temperature),data=df)+
  geom_point()+
  geom_smooth()+
  ggtitle("Average Yearly Temperature in New Haven")

# Monte Carlo simulation of confidence intervals
p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations with 0 and 1
X_hat <- mean(X); X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N); SE_hat    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean

# Solving for  z  with qnorm
z <- qnorm(0.995);z    # calculate z value for 99.5% confidence interval (Pval=0.005)
pnorm(qnorm(0.995))    
pnorm(qnorm(1-0.995))    
pnorm(z) - pnorm(-z)    

# Monte Carlo Simulation
library(dplyr)
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

# Confidence interval for the spread with sample size of 25
N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N) #includes 0, means that we have small sample size so we need to add sample size

### Example 1
library(dplyr)
library(dslabs)
data("polls_us_election_2016")
polls <- filter(polls_us_election_2016, enddate>="2016-10-31" & state=="U.S." )
nrow(polls)
N<-polls$samplesize[1]; N
X_hat<-polls$rawpoll_clinton[1]/100
X_hat
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat
z <- qnorm(0.975); z
ci <- c(X_hat - z*se_hat, X_hat + z*se_hat)
ci

### Example 2 Pollster results for p
head(polls)
z <- qnorm(0.975); z
X_hat=polls$rawpoll_clinton/100
se_hat= sqrt(X_hat*(1-X_hat)/polls$samplesize)
lower = X_hat - z*se_hat
upper = X_hat + z*se_hat
polls <- mutate(polls, X_hat, se_hat, lower, upper)
pollster_results <- select(polls, .data$pollster, .data$enddate, .data$X_hat, .data$se_hat, .data$lower, .data$upper)
head(pollster_results)

### Example 3 Comparing to actual results - p
library(dplyr)
p=0.482
hit <-rep(0,nrow(pollster_results))
hit[(pollster_results$upper-p)>0 & (p-pollster_results$lower)>0]<-1
pollster_results<-mutate(pollster_results, hit)
avg_hit <- summarize(pollster_results,mean=mean(hit)); avg_hit
head(pollster_results)

### Example 4 Confidence interval for d
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") 
mutate(polls,d_hat=rawpoll_clinton/100-rawpoll_trump/100)
N <- polls$samplesize[1]
N
d_hat <- polls$rawpoll_clinton[1]/100-polls$rawpoll_trump[1]/100
d_hat
p<-(d_hat+1)/2
X_hat<-p
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)
se_hat
z<-qnorm(0.975)
lower<-2*X_hat-1-z*se_hat
upper<-2*X_hat-1+z*se_hat
ci <- c(lower,upper)
ci

### Example 5 Pollster results for d
z <- qnorm(0.975); z
d_hat <- polls$rawpoll_clinton/100 - polls$rawpoll_trump/100
X_hat=(d_hat+1)/2
se_hat= 2*sqrt(X_hat*(1-X_hat)/polls$samplesize)
lower = d_hat - z*se_hat
upper = d_hat + z*se_hat
polls <- mutate(polls, d_hat, se_hat, lower, upper)
pollster_results <- select(polls, .data$pollster, .data$enddate, .data$d_hat, .data$lower, .data$upper)

### Example 6 Comparing to actual results - d
d <- 0.021
hit <-rep(0,nrow(pollster_results))
hit[(pollster_results$upper-d)>0 & (d-pollster_results$lower)>0]<-1
pollster_results<-mutate(pollster_results, hit)
avg_hit <- summarize(pollster_results,mean=mean(hit)); avg_hit

### Example 7 Comparing to actual results by pollster
d <- 0.021
d_hat <- polls$rawpoll_clinton/100 - polls$rawpoll_trump/100
errors <- d_hat - d
polls2 <- mutate(polls,d, d_hat, errors)
ggplot(aes(x = pollster, y = errors ), data=polls2) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Example 8
d <- 0.021
d_hat <- polls$rawpoll_clinton/100 - polls$rawpoll_trump/100
errors <- d_hat-d
mutate(polls,errors) %>% group_by(pollster) %>% filter(n() >= 5) %>% ggplot(aes(x = pollster, y = errors )) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
