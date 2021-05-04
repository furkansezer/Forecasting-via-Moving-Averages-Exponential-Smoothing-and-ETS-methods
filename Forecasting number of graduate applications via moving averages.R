#Forecast next 4 years of graduate applications using moving averages method.

grad<-read.table("graduate.txt")
grad_ts<-ts(grad,frequency = 1)
grad_ts

plot(grad_ts,ylab="Avg number of graduate app")

o5<-ma(grad_ts,order=5,centre=F)

o5[74]<-o5[73]
o5[75]<-o5[73]
x<-c(o5,298.08,298.08)
lines(x,col="red")

o8<-ma(grad_ts,order=8,centre = T)
o8
o8[72]<-o8[71]
o8[73]<-o8[71]
o8[74]<-o8[71]
o8[75]<-o8[71]

plot(grad_ts,ylab="Avg number of graduate app")
y<-c(o8,279.6631,279.6631,279.6631,279.6631)
lines(y,col="red")

errors_5<-grad_ts[5:75]-o5[5:75]
sum(errors_5^2)

errors_8<-grad_ts[5:75]-o8[5:75]
sum(errors_8^2)

accuracy(forecast(o8[5:75],h=4))
accuracy(forecast(o5[5:75],h=4))