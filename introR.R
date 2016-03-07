#zuweisung mit <-
x <- c(1,2,3)
is(x)
is.numeric(x)
is.factor(x)
class(x)
class(TRUE)
class(sin)
1:10
seq(from=1, to=10, by=2)
seq(1,10,2)
rep(1:3,10)

#Hilfe mit ?
?matrix

y<- numeric(4)

y[2]<-999

y[1] <- "True"

m<-matrix(1:6,ncol=3,byrow=TRUE)

length(y)
length(m)

dim(m)

dim(m)[2]
ncol(m)
m[1,2]
m[1,]

z<-as.numeric(m)

#workspace
ls()
rm(z)
ls()
rm(list=ls())

x<- rep(1:4,3)
x
min(x)
max(x)
mean(x)
var(x)
sd(x)

install.packages('ggplot2')
require('ggplot2')

#working directory
getwd()

setwd("C:/user/...")
getwd()

#header erste zeile variablenname
data<-read.table('beginner.txt', header=TRUE, sep=";")
#read.csv fuer csv files
#alternativ
#tools->import dataset->from text file/from url

data(mtcars)
?mtcars
str(mtcars)
summary(mtcars)
is(mtcars)

head(mtcars)
tail(mtcars)

head(mtcars,1)
tail(mtcars,1)

colnames(mtcars)
names(mtcars)
rownames(mtcars)

nrow(mtcars)
ncol(mtcars)
dim(mtcars)

is.factor(mtcars$cyl)
#einteilen in kategorie
cyl.fact<-as.factor(mtcars$cyl)
is.factor(cyl.fact)

plot(mtcars$cyl)
plot(cyl.fact)
levels(cyl.fact)
summary(cyl.fact)
table(mtcars$cyl)

#autos mit 8 zylinder
mtcars[mtcars$cyl == 8,]

mt2<-mtcars[mtcars$wt < mean(mtcars$wt), c("mpg", "cyl","wt")]
mt2

rowMeans(mt2)
colSums(mt2)
apply(mt2,2,sum)
#1 zeilen-, 2 spaltenweise

m<-matrix(1:6,ncol=3,byrow=TRUE)
colnames(m) <-c("a","b","c")
m

apply(mt2,1,function(x) x+2)

#transponieren
t(mt2)


#logische operatoren
#<,>, <=, >=, ==, !=, &, |

#zw 6 und 8 zylinder
# %in% entspricht "Element von"
mtcars[mtcars$wt <mean(mtcars$wt) & mtcars$cyl %in% c(6,8),]

#plot
t <- seq(0,2*pi,pi/20)
length(t)
plot(t, sin(t))

plot(t, sin(t), pch=7)
plot(t,sin(t),pch=20,cex=4)

plot(t,sin(t), main="Sinuskurve", xlab="t", ylab="sin(t)",col="red", lwd=2)
plot(t,sin(t), main="Sinuskurve", xlab="t", ylab="sin(t)",col="orange", lwd=2, type="l")
plot(t,sin(t), main="Sinuskurve", xlab="t", ylab="sin(t)",col="blue", lwd=2, type="l", lty=2)

#lines
plot(t,sin(t), col="blue", type="l")
lines(t,cos(t), col="red")

plot(t,sin(t), main="Trigonometrische Fkt", xlab="t", ylab="", col="blue", type="l", ylim=c(-1,1))
lines(t,cos(t),col="red")
abline(h=0,lty=3)

legend("bottomleft", legend=c("sin(t)","cos(t)"), lty=1,col=c("blue", "red"), cex=1.2)

#Statistik
#dichtefkt

#mean= erwartungswert
#sd = standardabweichung
dnorm(1, mean=100, sd=50)#dichtefkt einer Normalverteilung
pnorm(0.4,mean=3, sd=5)#wert der VF der Normalverteilng
qnorm(0.975)#quantil
rnorm(100)#random

set.seed(1)
x<-rnorm(1000)

#kennzahlen
mean(x)
sd(x)
var(x)
median(x)

#histogramm und boxplot
hist(x)
hist(x,freq=F)#dichte
lines(density(x))
boxplot(x,main="Boxplot von 1000 N(0,1)-verteilten Zahlen")

#Schleifen
n<-0; z<-0
while(n<5){
  z<-z+n
  n<-n+1
}
z; n

y<-numeric(5)
for(j in 1:5){
  y[j]<-j+500
}
y

#was in der letzten zeile steht wird ausgegeben
myfunction <-function(x){
  if(x> 0.5) k<- 500
  else k<- 20
  a<- x+k
  b<-x*k
  #return (c(a,b))
  c(a,b)
}

xy<-myfunction(rnorm(1))
xy


myfunction <-function(x){
  if(x> 0.5) k<- 500
  else k<- 20
  a<- x+k
  b<-x*k
  list(A=a,B=b)
}

xy<-myfunction(rnorm(1))
xy$A

#negative indizes sind wohldefiniert
#anstatt dass zeile i ausgegeben wird, wird zeile i ignoriert
m<-matrix(1:9,ncol=3,byrow=TRUE)
m
m[-1,]
m[-2:-1,-1]
m[-2:-1,1]
#mit 0 wird der Typ augegeben
m
m[0,0]
m[0,]
m[,0]

#fun :)
# T = TRUE und F=FALSE als Abkuerzungen standardmaessig
T <-F
T
