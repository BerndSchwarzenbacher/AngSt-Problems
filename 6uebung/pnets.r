# c/ W. Gurker für AMStat16

"net.normal" <-
function (DATA,CO.P=1,CO.L=1,ml.line='y',xlab='Data') {
 # CO.P -- color points
 # CO.L -- color line
 # ml.line -- draw ml-line?
 # value: ml estimates
 m = mean(DATA)
 s = sd(DATA)*sqrt((length(DATA)-1)/length(DATA))
 plot(sort(DATA),qnorm(((1:length(DATA))-0.5)/length(DATA)),
      pch=19,cex=1,axes=FALSE,xlab=xlab,ylab='Probability',
      main='Normal Probability Plot',col=CO.P)
 if (ml.line == 'y') {abline(-m/s,1/s,lwd=1.5,col=CO.L)}
 a = par('xaxp'); b = par('yaxp')
 delta = (a[2]-a[1])/a[3]
 tl.x = delta/5
 tic.x = seq(a[1]-delta,a[2]+delta,by=tl.x)
 axis(1,tic.x,labels=FALSE,tck=-0.01)
 for (i in 1:length(tic.x)) {
      abline(v=tic.x[i],lwd=0.5)}
 u = c(0.001,0.0015,0.002,0.003,0.004,0.005,0.0075,0.01,
      0.015,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,
      0.12,0.14,0.16,0.18,0.2,0.25,0.3,0.35,0.4,0.45)
 u.red = c(0.001,0.002,0.005,0.01,0.02,0.05,0.1,0.2,0.3,0.4)
 quant.y.red = c(u.red,0.5,1-u.red[length(u.red):1])
 quant.y = c(u,0.5,1-u[length(u):1])
 lab.y = c('.001','','.002','','','.005','','.01','','.02','','','.05',
           '','','','','.1','','','','','.2','','.3','','.4','','.5',
           '','.6','','.7','','.8','','','','','.9','','','','','.95',
           '','','.98','','.99','','.995','','','.998','','.999')
 tic.y = qnorm(quant.y)
 tic.y.red = qnorm(quant.y.red)
 for (j in 1:length(tic.y)) {
      abline(h=tic.y[j],lwd=0.5)}
 abline(h=-1,lty=2,col='red')
 abline(h=1,lty=2,col='red')
 axis(1)
 axis(2,tic.y.red,labels=FALSE,tick=TRUE,cex.axis=0.8,tck=-0.01)
 axis(2,tic.y,labels=lab.y,las=TRUE,tick=TRUE,cex.axis=0.8,tck=-0.005)
 axis(4,c(-3:3),tick=TRUE,las=TRUE,cex.axis=0.8)
 box()
 mtext(paste('ML - estimates: mhat =',round(m,3),'  shat =',round(s,3)),
       side=3,cex=0.8)
 c(m,s)
}

"net.lognormal" <-
function (DATA,CO.P=1,CO.L=1,ml.line='y',xlab='Data') {
 # CO.P -- color points
 # CO.L -- color line
 # ml.line -- draw ml-line?
 # value: ml estimates
 m = mean(log(DATA))
 s = sd(log(DATA))*sqrt((length(DATA)-1)/length(DATA))
 m10 = mean(log10(DATA))
 s10 = sd(log10(DATA))*sqrt((length(DATA)-1)/length(DATA))
 plot(sort(DATA),qnorm(((1:length(DATA))-0.5)/length(DATA)),
      pch=19,cex=1,axes=FALSE,xlab=xlab,ylab='Probability',
      main='Log-Normal Probability Plot',col=CO.P,
      log='x')
 if (ml.line == 'y') {abline(-m10/s10,1/s10,lwd=1.5,col=CO.L)}
 a = axTicks(1); a.diff = diff(a)
 tic.x = c()
 for (i in 2:length(a)) {
      tic.x = c(tic.x,seq(a[i-1],a[i],by=a.diff[i-1]/10))}
 tic.x = c(seq(a[1]-a.diff[1],a[1],by=a.diff[1]/10),tic.x,
           seq(a[length(a)],a[length(a)]+a.diff[length(a)-1],
           by=a.diff[length(a)-1]/10))
 axis(1,tic.x,labels=FALSE,tck=-0.01)
 for (i in 1:length(tic.x)) {
      abline(v=tic.x[i],lwd=0.5)}
 u = c(0.001,0.0015,0.002,0.003,0.004,0.005,0.0075,0.01,
      0.015,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,
      0.12,0.14,0.16,0.18,0.2,0.25,0.3,0.35,0.4,0.45)
 u.red = c(0.001,0.002,0.005,0.01,0.02,0.05,0.1,0.2,0.3,0.4)
 quant.y.red = c(u.red,0.5,1-u.red[length(u.red):1])
 quant.y = c(u,0.5,1-u[length(u):1])
 lab.y = c('.001','','.002','','','.005','','.01','','.02','','','.05',
           '','','','','.1','','','','','.2','','.3','','.4','','.5',
           '','.6','','.7','','.8','','','','','.9','','','','','.95',
           '','','.98','','.99','','.995','','','.998','','.999')
 tic.y = qnorm(quant.y)
 tic.y.red = qnorm(quant.y.red)
 for (j in 1:length(tic.y)) {
      abline(h=tic.y[j],lwd=0.5)}
 abline(h=-1,lty=2,col='red')
 abline(h=1,lty=2,col='red')
 axis(1)
 axis(2,tic.y.red,labels=FALSE,tick=TRUE,cex.axis=0.8,tck=-0.01)
 axis(2,tic.y,labels=lab.y,las=TRUE,tick=TRUE,cex.axis=0.8,tck=-0.005)
 axis(4,c(-3:3),tick=TRUE,las=TRUE,cex.axis=0.8)
 box()
 mtext(paste('ML - estimates: mhat =',round(m,3),'  shat =',round(s,3)),
       side=3,cex=0.8)
 c(m,s)
}

"net.weibull" <-
function (DATA,CO.P=1,CO.L=1,ml.line='y',xlab='Data') {
 # CO.P -- color points
 # CO.L -- color line
 # ml.line -- draw ml-line?
 # value: ml estimates
 # ml-fit: weibull
 library(survival)
 wei.ml = survreg(Surv(DATA)~1,dist="weibull")
 tau = exp(wei.ml$coefficients)
 beta = 1/wei.ml$scale
 plot(sort(DATA),log(-log(1-(1:length(DATA)-0.5)/length(DATA))),
      pch=19,cex=1,axes=FALSE,xlab=xlab,ylab='Probability',
      main='Weibull Probability Plot',col=CO.P,
      log='x')
 if (ml.line == 'y') {abline(-log(tau)*beta,log(10)*beta,lwd=1.5,col=CO.L)}
 a = axTicks(1); a.diff = diff(a)
 tic.x = c()
 for (i in 2:length(a)) {
      tic.x = c(tic.x,seq(a[i-1],a[i],by=a.diff[i-1]/10))}
 tic.x = c(seq(a[1]-a.diff[1],a[1],by=a.diff[1]/10),tic.x,
           seq(a[length(a)],a[length(a)]+a.diff[length(a)-1],
           by=a.diff[length(a)-1]/10))
 axis(1,tic.x,labels=FALSE,tck=-0.01)
 for (i in 1:length(tic.x)) {
      abline(v=tic.x[i],lwd=0.5)}
 u = c(0.001,0.0015,0.002,0.003,0.004,0.005,0.0075,0.01,
      0.015,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,
      0.12,0.14,0.16,0.18,0.2,0.25,0.3,0.35,0.4,0.45)
 u.red = c(0.001,0.002,0.005,0.01,0.02,0.05,0.1,0.2,0.3,0.4)
 quant.y.red = c(u.red,0.5,1-u.red[length(u.red):1])
 quant.y = c(u,0.5,1-u[length(u):1])
 lab.y = c('.001','','.002','','','.005','','.01','','.02','','','.05',
           '','','','','.1','','','','','.2','','.3','','.4','','.5',
           '','.6','','.7','','.8','','','','','.9','','','','','.95',
           '','','.98','','.99','','.995','','','.998','','.999')
 tic.y = log(-log(1-quant.y))
 tic.y.red = log(-log(1-quant.y.red))
 for (j in 1:length(tic.y)) {
      abline(h=tic.y[j],lwd=0.5)}
 abline(h=0,lty=2,col='red')
 axis(1,cex.axis=0.8)
 axis(2,tic.y.red,labels=FALSE,tick=TRUE,cex.axis=0.8,tck=-0.01)
 axis(2,tic.y,labels=lab.y,las=TRUE,tick=TRUE,cex.axis=0.7,tck=-0.005)
 axis(4,c(-10:10),tick=TRUE,las=TRUE,cex.axis=0.8)
 box()
 mtext(paste('ML - estimates: etahat =',round(tau,3),'  betahat =',round(beta,3)),
       side=3,cex=0.8)
 c(tau,beta)
}

"net.logis" <-
function (DATA,CO.P=1,CO.L=1,ml.line='y',xlab='Data') {
 # CO.P -- color points
 # CO.L -- color line
 # ml.line -- draw ml-line?
 # value: ml estimates
 library(survival)
 logis.ml = survreg(Surv(DATA)~1,dist="logistic")
 m <- logis.ml$coefficients
 s <- logis.ml$scale
 plot(sort(DATA),qlogis(((1:length(DATA))-0.5)/length(DATA)),
      pch=19,cex=1,axes=FALSE,xlab=xlab,ylab='Probability',
      main='Logistic Probability Plot',col=CO.P)
 if (ml.line == 'y') {abline(-m/s,1/s,lwd=1.5,col=CO.L)}
 a = par('xaxp'); b = par('yaxp')
 delta = (a[2]-a[1])/a[3]
 tl.x = delta/5
 tic.x = seq(a[1]-delta,a[2]+delta,by=tl.x)
 axis(1,tic.x,labels=FALSE,tck=-0.01)
 for (i in 1:length(tic.x)) {
      abline(v=tic.x[i],lwd=0.5)}
 u = c(0.001,0.0015,0.002,0.003,0.004,0.005,0.0075,0.01,
      0.015,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,
      0.12,0.14,0.16,0.18,0.2,0.25,0.3,0.35,0.4,0.45)
 u.red = c(0.001,0.002,0.005,0.01,0.02,0.05,0.1,0.2,0.3,0.4)
 quant.y.red = c(u.red,0.5,1-u.red[length(u.red):1])
 quant.y = c(u,0.5,1-u[length(u):1])
 lab.y = c('.001','','.002','','','.005','','.01','','.02','','','.05',
           '','','','','.1','','','','','.2','','.3','','.4','','.5',
           '','.6','','.7','','.8','','','','','.9','','','','','.95',
           '','','.98','','.99','','.995','','','.998','','.999')
 tic.y = qlogis(quant.y)
 tic.y.red = qlogis(quant.y.red)
 for (j in 1:length(tic.y)) {
      abline(h=tic.y[j],lwd=0.5)}
 abline(h=-1,lty=2,col='red')
 abline(h=1,lty=2,col='red')
 axis(1)
 axis(2,tic.y.red,labels=FALSE,tick=TRUE,cex.axis=0.8,tck=-0.01)
 axis(2,tic.y,labels=lab.y,las=TRUE,tick=TRUE,cex.axis=0.8,tck=-0.005)
 axis(4,c(-6:6),tick=TRUE,las=TRUE,cex.axis=0.8)
 box()
 mtext(paste('ML - estimates: xihat =',round(m,3),'  deltahat =',round(s,3)),
       side=3,cex=0.8)
 c(m,s)
}
