source("pnets.r")

emig <- read.table("electromig.txt", header=TRUE)

par(mfrow=c(1,2))
bplot <- boxplot(emig, main="Boxplot", col="darkgrey")
stripchart(emig, method="jitter", vertical=TRUE,
  pch=22, bg="darkgrey", main="Dotplot")

par(mfrow=c(2,2))
dat <- emig$HOURS
net.normal(dat, CO.L=2, xlab="Hours")
net.lognormal(dat, CO.L=2, xlab="Hours")
net.weibull(dat, CO.L=2, xlab="Hours")
net.logis(dat, CO.L=2, xlab="Hours")

emigr <- setdiff(emig$HOURS, bplot$out)
summary(emigr)

par(mfrow=c(1,2))
boxplot(emigr, main="Boxplot", col="darkgrey")
stripchart(emigr, method="jitter", vertical=TRUE,
  pch=22, bg="darkgrey", main="Dotplot")

par(mfrow=c(2,2))
dat <- emigr
net.normal(dat, CO.L=2, xlab="Hours")
net.lognormal(dat, CO.L=2, xlab="Hours")
net.weibull(dat, CO.L=2, xlab="Hours")
net.logis(dat, CO.L=2, xlab="Hours")
