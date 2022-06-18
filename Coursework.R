setwd("D:\\Users\\Joaquim\\Downloads\\Stats")
load(file="killersandmotives.Rdata")

createsample(36)

mysample

boxplot(mysample$AgeFirstKill)
boxplot(mysample$AgeLastKill)

nrow(mysample)

sum(mysample$AgeFirstKill==99999)
nrow(mysample[!mysample$AgeFirstKill==99999,])

sum(is.na(mysample$Motive))
nrow(mysample[!is.na(mysample$Motive),])

sum((mysample$YearBorn+mysample$AgeFirstKill)<1900)
nrow(mysample[!(mysample$YearBorn+mysample$AgeFirstKill)<1900,])

mysample1<-mysample[!mysample$AgeFirstKill==99999,]
mysample2<-mysample1[!is.na(mysample1$Motive),]
mysample3<-mysample2[(mysample2$YearBorn+mysample2$AgeFirstKill)>=1900,]

mysample3$CareerDuration<-(mysample3$AgeLastKill-mysample3$AgeFirstKill)

library('moments')
boxplot(mysample3$AgeFirstKill)
title(xlab = "Age At First Kill", line = 0.8) 
title(ylab = "Age", line = 2.1) 
skewness(mysample3$AgeFirstKill)

boxplot(mysample3$AgeLastKill)
title(xlab = "Age At Last Kill", line = 0.8) 
title(ylab = "Age", line = 2.1) 
skewness(mysample3$AgeLastKill)

boxplot(mysample3$CareerDuration)
title(xlab = "Career Duration", line = 0.8) 
title(ylab = "Number  of  Years", line = 2.2) 
skewness(mysample3$CareerDuration)

#Relationship between the variables
plot(mysample3$CareerDuration,mysample3$AgeFirstKill,xlab='Career Duration',
     ylab='Age At First Kill',col='blue')
cor(mysample3$CareerDuration,mysample3$AgeFirstKill)

plot(mysample3$CareerDuration,mysample3$AgeLastKill,xlab='Career Duration',
     ylab='Age At Last Kill',col='blue')
cor(mysample3$CareerDuration,mysample3$AgeLastKill)

first_kill_u=mean(mysample3$AgeFirstKill)
first_kill_sd=sd(mysample3$AgeFirstKill)
plot(hist(mysample3$AgeFirstKill,breaks='FD'),main="Age at First Kill Normal Distribution"
     ,xlab='Age at First Kill',freq=FALSE,col='grey')

last_kill_u=mean(mysample3$AgeLastKill)
last_kill_sd=sd(mysample3$AgeLastKill)
plot(hist(mysample3$AgeLastKill,breaks='FD'),main="Age at Last Kill Normal Distribution"
     ,xlab='Age at Last Kill',freq=FALSE,col='grey')

career_dur_u=mean(mysample3$CareerDuration)
career_dur_sd=sd(mysample3$CareerDuration)
plot(hist(mysample3$CareerDuration),main="Career Duration Normal Distribution"
     ,xlab='Career Duration in Years',freq=FALSE,col='grey')

#Normal distribution
plot(hist(mysample3$AgeFirstKill,breaks='FD'),main="Age at First Kill Normal Distribution"
     ,xlab='Age at First Kill',freq=FALSE,border='blue')
first_kill<-seq(from=min(mysample3$AgeFirstKill),
                to=max(mysample3$AgeFirstKill),by=0.1)
lines(first_kill,dnorm(first_kill,first_kill_u,first_kill_sd),col='red')

plot(hist(mysample3$AgeLastKill,breaks='FD'),main="Age at Last Kill Normal Distribution"
     ,xlab='Age at Last Kill',freq=FALSE,border='blue')
last_kill<-seq(from=min(mysample3$AgeLastKill),
                to=max(mysample3$AgeLastKill),by=0.1)
lines(last_kill,dnorm(last_kill,last_kill_u,last_kill_sd),col='red')

#plot(hist(mysample3$CareerDuration),main="Career Duration Normal Distribution"
#     ,xlab='Career Duration in Years',freq=FALSE,border='blue')
#career_dur<-seq(from=min(mysample3$CareerDuration),
#               to=max(mysample3$CareerDuration),by=0.1)
#lines(career_dur,dnorm(career_dur,career_dur_u,career_dur_sd),col='red')

#Exponential distribution
#first_kill_lambda=1/first_kill_u
#plot(hist(mysample3$AgeFirstKill),main="Age at First Kill Exponential Distribution"
#     ,xlab='Age in Years',freq=FALSE)
#first_kill<-seq(from=min(mysample3$AgeFirstKill),
#                to=max(mysample3$AgeFirstKill),by=0.1)
#lines(first_kill,dexp(first_kill,first_kill_lambda,log=FALSE))

#last_kill_lambda=1/last_kill_u
#last_kill_sd=sd(mysample3$AgeLastKill)
#plot(hist(mysample3$AgeLastKill),main="Age at Last Kill Exponential Distribution"
#     ,xlab='Age in Years',freq=FALSE)
#last_kill<-seq(from=min(mysample3$AgeLastKill),
#               to=max(mysample3$AgeLastKill),by=0.1)
#lines(last_kill,dexp(last_kill,last_kill_lambda))

career_dur_lambda=1/career_dur_u
plot(hist(mysample3$CareerDuration),main="Career Duration Exponential Distribution"
     ,xlab='Career Duration in Years',freq=FALSE,border = 'blue')
career_dur<-seq(from=min(mysample3$CareerDuration),
                to=max(mysample3$CareerDuration),by=0.1)
lines(career_dur,dexp(career_dur,career_dur_lambda),col='red')


summary(mysample3[mysample3$Motive=='Angel of Death',]$AgeFirstKill)
summary(mysample3[mysample3$Motive=='Robbery or financial gain',]$AgeFirstKill)
summary(mysample3[mysample3$Motive=='Unknown',]$AgeFirstKill)

#Fn1 <- ecdf(mysample3[mysample3$Motive=='Angel of Death',]$AgeFirstKill)

# Estimates of mu and sigma:
mu1 <- mean(mysample3[mysample3$Motive=='Angel of Death',]$AgeFirstKill)
sigma1 <- sd(mysample3[mysample3$Motive=='Angel of Death',]$AgeFirstKill)

# CDF of a normal distribution with the 
# estimated parameters:

#G1 <- function(x){
#        return(pnorm(x, mean = mu1, sd = sigma1))
#}

#plot(Fn1, verticals = TRUE, pch = NA)
#x1 <- 1:100
#lines(x1, G1(x1), col = "red")

#ks.test(x=mysample3[mysample3$Motive=='Angel of Death',]$AgeFirstKill,
#        y='pnorm',mean = mu1, sd = sigma1)

qqnorm(mysample3[mysample3$Motive=='Angel of Death',]$AgeFirstKill)
qqline(mysample3[mysample3$Motive=='Angel of Death',]$AgeFirstKill,col='blue')

#Fn2 <- ecdf(mysample3[mysample3$Motive=='Robbery or financial gain',]$AgeFirstKill)

# Estimates of mu and sigma:
mu2 <- mean(mysample3[mysample3$Motive=='Robbery or financial gain',]$AgeFirstKill)
sigma2 <- sd(mysample3[mysample3$Motive=='Robbery or financial gain',]$AgeFirstKill)

# CDF of a normal distribution with the 
# estimated parameters:

#G2 <- function(x){
#        return(pnorm(x, mean = mu2, sd = sigma2))
#}

#plot(Fn2, verticals = TRUE, pch = NA)
#x2 <- 1:100
#lines(x2, G2(x2), col = "red")

#ks.test(x=mysample3[mysample3$Motive=='Robbery or financial gain',]$AgeFirstKill,
#        y='pnorm',mean=mu2,sd=sigma2)

qqnorm(mysample3[mysample3$Motive=='Robbery or financial gain',]$AgeFirstKill)
qqline(mysample3[mysample3$Motive=='Robbery or financial gain',]$AgeFirstKill,col='blue')


#Fn3 <- ecdf(mysample3[mysample3$Motive=='Unknown',]$AgeFirstKill)

# Estimates of mu and sigma:
mu3 <- mean(mysample3[mysample3$Motive=='Unknown',]$AgeFirstKill)
sigma3 <- sd(mysample3[mysample3$Motive=='Unknown',]$AgeFirstKill)

# CDF of a normal distribution with the 
# estimated parameters:

#G3 <- function(x){
#        return(pnorm(x, mean = mu3, sd = sigma3))
#}

#plot(Fn3, verticals = TRUE, pch = NA)
#x3 <- 1:100
#lines(x3, G3(x3), col = "red")
#ks.test(x=mysample3[mysample3$Motive=='Unknown',]$AgeFirstKill,y='pnorm',mean = mu3, sd = sigma3)

qqnorm(mysample3[mysample3$Motive=='Unknown',]$AgeFirstKill)
qqline(mysample3[mysample3$Motive=='Unknown',]$AgeFirstKill,col='blue')

z1=(mu1-27)/(sqrt((sigma1^2)/23))
p1=2*pnorm(-z1)
LCI1=mu1-(1.96*sqrt((sigma1^2)/23))
UCI1=mu1+(1.96*sqrt((sigma1^2)/23))
t.test(x=mysample3[mysample3$Motive=='Angel of Death',]$AgeFirstKill,mu=27)

z2=(mu2-27)/(sqrt((sigma2^2)/510))
p2=2*pnorm(-z2)
LCI2=mu2-(1.96*sqrt((sigma2^2)/510))
UCI2=mu2+(1.96*sqrt((sigma2^2)/510))
t.test(x=mysample3[mysample3$Motive=='Robbery or financial gain',]$AgeFirstKill,mu=27)

z3=(mu3-27)/(sqrt((sigma3^2)/77))
p3=2*pnorm(-z3)
LCI3=mu3-(1.96*sqrt((sigma3^2)/77))
UCI3=mu3+(1.96*sqrt((sigma3^2)/77))
t.test(x=mysample3[mysample3$Motive=='Unknown',]$AgeFirstKill,mu=27)

# Analysis labels for the left side:

analysis = c("Angel of Death", 
             "Robbery or Financial Gain", 
             "Unknown")

# Results of each test (estimated mean, 
# upper CI limit, lower CI limit, p-value):

estimate  =  c(32.35, 29.46, 29.97)             
upper     =  c(36.11,30.23,32.18)
lower     =  c(28.58,28.68,27.76)
pval      =  c(0.007,5.99e-10,0.008)

# Set the margin widths:

par(mar = c(6,6,1,6))

# Create an empty plot of a suitable 
# size (considering the width of your
# confidence intervals):

plot(x = 0,                                  # One point at (0,0).
     xlim = c(20,45), ylim=c(0, 5),        # Axis limits.
     type = "n", xaxt = "n", yaxt="n",       # No points, no axes drawn.
     xlab = NULL, ylab= NULL, ann = FALSE,   # No axis labels or numbers.
     bty="n")                                # No box.

# Add a horizontal (side = 1) axis:
axis(side = 1, cex.axis = 1) 
# Add an axis label 4 lines below the axis:
mtext("Estimated difference in means of Age at First Kill for different motives 
      compared to the null hypothesis (blue-mean age 27), with 95% confidence interval", 
      side = 1, line = 4) 

# Add some grid lines, preferably lined up
# with the numbers on the horizontal axis:
for(i in c(25,30,35,40)){
        lines(c(i, i), c(0, 4), lty = 2, col = "gray53")}
lines(c(27,27), c(0, 4), lty = 2, col = "blue")
# Add labels for each analysis on the left (side = 2) 
# at vertical heights of 1, 2, and 3
verticalpos = 1:3
mtext(text = analysis,  at = verticalpos, 
      side = 2, line = 5, outer = FALSE, las = 1, adj = 0)

# Plot the four point estimates (centres 
# of the CIs for each analysis): 
points(estimate, verticalpos, pch = 16)

# Plot the four interval estimates:

for(i in 1:4 ){
        lines(c(lower[i], upper[i]), c(verticalpos[i], verticalpos[i]))
        lines(c(lower[i], lower[i]), c(verticalpos[i] + 0.2, verticalpos[i] - 0.2))
        lines(c(upper[i], upper[i]), c(verticalpos[i] + 0.2, verticalpos[i] - 0.2))}

# Now we add numerical results on the right (side = 4), but we 
# need to put them into a nice form first. Note that
# paste() merges words and numbers, and formatC()
# allows us to control the number of decimal places.
est <- formatC(estimate, format='f', digits = 2)
P <- formatC(pval , format = 'f', digits = 3) 
pval <- paste("p =", P)    # Type pval to see what this does.
L <- formatC(lower, format = 'f', digits = 2)
U <- formatC(upper, format = 'f', digits = 2)
interval <- paste("(", L, ", ", U, "),", sep = "")   # Type interval to check.
# Putting it all together:
results <- paste(est, interval, pval)
# Add to the plot:
mtext(text = results, at = verticalpos, 
      side = 4, line = 6, outer = FALSE, las = 1, adj = 1)
box("inner")


t.test(x=mysample3[mysample3$Motive=='Angel of Death',]$AgeFirstKill,
       y=mysample3[mysample3$Motive=='Robbery or financial gain',]$AgeFirstKill,
       var.equal=TRUE)
t.test(x=mysample3[mysample3$Motive=='Angel of Death',]$AgeFirstKill,
       y=mysample3[mysample3$Motive=='Unknown',]$AgeFirstKill)
t.test(x=mysample3[mysample3$Motive=='Robbery or financial gain',]$AgeFirstKill,
       y=mysample3[mysample3$Motive=='Unknown',]$AgeFirstKill)

# Analysis labels for the left side:

analysis = c("Angel of Death -
Robbery or Financial Gain", 
             "Angel of Death - Unknown", 
             "Robbery or Financial 
Gain - Unknown")

# Results of each test (estimated mean, 
# upper CI limit, lower CI limit, p-value):

estimate  =  c(2.89,2.37,-0.52)             
upper     =  c(6.40,6.69,1.86)
lower     =  c(-0.86,-1.94,-2.89)
pval      =  c(0.1304,0.2732,0.666)

# Set the margin widths:

par(mar = c(8,8,1,8))

# Create an empty plot of a suitable 
# size (considering the width of your
# confidence intervals):

plot(x = 0,                                  # One point at (0,0).
     xlim = c(-4,8), ylim=c(0, 5),        # Axis limits.
     type = "n", xaxt = "n", yaxt="n",       # No points, no axes drawn.
     xlab = NULL, ylab= NULL, ann = FALSE,   # No axis labels or numbers.
     bty="n")                                # No box.

# Add a horizontal (side = 1) axis:
axis(side = 1, cex.axis = 1) 
# Add an axis label 4 lines below the axis:
mtext("Estimated difference in means of Age at First Kill between different motives 
      compared to the null hypothesis (blue- mean difference 0), with 95% confidence interval", 
      side = 1, line = 4) 

# Add some grid lines, preferably lined up
# with the numbers on the horizontal axis:
for(i in c(-2,2,6)){
        lines(c(i, i), c(0, 4), lty = 2, col = "gray53")}
lines(c(0,0), c(0, 4), lty = 2, col = "blue")
# Add labels for each analysis on the left (side = 2) 
# at vertical heights of 1, 2, and 3
verticalpos = 1:3
mtext(text = analysis,  at = verticalpos, 
      side = 2, line = 6, outer = FALSE, las = 1, adj = 0)

# Plot the four point estimates (centres 
# of the CIs for each analysis): 
points(estimate, verticalpos, pch = 16)

# Plot the four interval estimates:

for(i in 1:4 ){
        lines(c(lower[i], upper[i]), c(verticalpos[i], verticalpos[i]))
        lines(c(lower[i], lower[i]), c(verticalpos[i] + 0.2, verticalpos[i] - 0.2))
        lines(c(upper[i], upper[i]), c(verticalpos[i] + 0.2, verticalpos[i] - 0.2))}

# Now we add numerical results on the right (side = 4), but we 
# need to put them into a nice form first. Note that
# paste() merges words and numbers, and formatC()
# allows us to control the number of decimal places.
est <- formatC(estimate, format='f', digits = 2)
P <- formatC(pval , format = 'f', digits = 3) 
pval <- paste("p =", P)    # Type pval to see what this does.
L <- formatC(lower, format = 'f', digits = 2)
U <- formatC(upper, format = 'f', digits = 2)
interval <- paste("(", L, ", ", U, "),", sep = "")   # Type interval to check.
# Putting it all together:
results <- paste(est, interval, pval)
# Add to the plot:
mtext(text = results, at = verticalpos, 
      side = 4, line = 7.5, outer = FALSE, las = 1, adj = 1)
box("inner")




