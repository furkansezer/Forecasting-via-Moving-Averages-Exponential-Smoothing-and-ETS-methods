#Forecast paperback book sales using exponential smoothing with seasonality (Holt-Winters method)
book<-read.table("books.txt",header = TRUE)
book_p_ts<-ts(book$Paperback)
plot(book_p_ts)

book_ts
plot(book_ts)
acf(book$Hardcover)

ses_paperback<-HoltWinters(book$Paperback,alpha =0.2,beta = F,gamma = F)
ses_paperback
fitted(ses_paperback)
ses_paperback$SSE
predict(ses_paperback,prediction.interval = 0.95)
forecasted<-predict(ses_paperback,n.ahead = 4)
forecasted

ses_paperback1<-HoltWinters(book$Paperback,alpha =0.6,beta = F,gamma = F)
ses_paperback1
fitted(ses_paperback1)
ses_paperback1$SSE
predict(ses_paperback1,prediction.interval = 0.95)
forecasted1<-predict(ses_paperback1,n.ahead = 4)
forecasted1

ses_paperback2<-HoltWinters(book$Paperback,alpha =0.9,beta = F,gamma = F)
ses_paperback2
fitted(ses_paperback2)
ses_paperback2$SSE
predict(ses_paperback2,prediction.interval = 0.95)
forecasted2<-predict(ses_paperback2,n.ahead = 4)
forecasted2


ses_paperback3<-HoltWinters(book$Paperback,alpha =0.4,beta = F,gamma = F)
ses_paperback3
fitted(ses_paperback3)
ses_paperback3$SSE
predict(ses_paperback3,prediction.interval = 0.95)
forecasted3<-predict(ses_paperback3,n.ahead = 4)
forecasted3

ses_paperback4<-HoltWinters(book$Paperback,alpha =0.75,beta = F,gamma = F)
ses_paperback4
fitted(ses_paperback4)
ses_paperback4$SSE
predict(ses_paperback4,prediction.interval = 0.95)
forecasted4<-predict(ses_paperback4,n.ahead = 4)
forecasted4

#Forecasting hardcover book sales using exponential smoothing with seasonality (Holt-Winters)


plot(ses_paperback,forecasted)
lines(fitted(ses_paperback)[,1],col=2)
plot(ses_paperback4,forecasted4)
plot(ses_paperback2,forecasted2)

opti_paper<-HoltWinters(book$Paperback,beta = F,gamma = F)
opti_paper
forecast_paper<-predict(opti_paper,n.ahead=4)
forecast_paper
opti_paper$SSE



ses_hardcover<-HoltWinters(book$Hardcover,alpha =0.2,beta = F,gamma = F)
ses_hardcover
fitted(ses_hardcover)
ses_hardcover$SSE
predict(ses_hardcover,prediction.interval = 0.95)
forecasted<-predict(ses_hardcover,n.ahead = 4)
forecasted

ses_hardcover1<-HoltWinters(book$Hardcover,alpha =0.6,beta = F,gamma = F)
ses_hardcover1
fitted(ses_hardcover1)
ses_hardcover1$SSE
predict(ses_hardcover1,prediction.interval = 0.95)
forecasted1<-predict(ses_hardcover1,n.ahead = 4)
forecasted1

ses_hardcover2<-HoltWinters(book$Hardcover,alpha =0.9,beta = F,gamma = F)
ses_hardcover2
fitted(ses_hardcover2)
ses_hardcover2$SSE
predict(ses_hardcover2,prediction.interval = 0.95)
forecasted2<-predict(ses_hardcover2,n.ahead = 4)
forecasted2

ses_hardcover3<-HoltWinters(book$Hardcover,alpha =0.4,beta = F,gamma = F)
ses_hardcover3
fitted(ses_hardcover3)
ses_hardcover3$SSE
predict(ses_hardcover3,prediction.interval = 0.95)
forecasted3<-predict(ses_hardcover3,n.ahead = 4)
forecasted3

ses_hardcover4<-HoltWinters(book$Hardcover,alpha =0.75,beta = F,gamma = F)
ses_hardcover4
fitted(ses_hardcover4)
ses_hardcover4$SSE
predict(ses_hardcover4,prediction.interval = 0.95)
forecasted4<-predict(ses_hardcover4,n.ahead = 4)
forecasted4

#QUE 2 PART C

plot(ses_hardcover,forecasted)
plot(ses_hardcover1,forecasted1)
plot(ses_hardcover2,forecasted2)

opti<-HoltWinters(book$Hardcover,beta = F,gamma = F)
opti
opti$SSE
forecast_opti<-forecast(opti,n.ahead=4)
forecast_opti
plot(forecast_opti)
predict(opti,prediction.interval = 0.95)

#Forecast paperback book sales using Holt's linear method


linear_paper<-HoltWinters(book$Paperback,gamma=F)
linear_paper
linear_paper$SSE
linear_hard<-HoltWinters(book$Hardcover,gamma=F)
linear_hard
linear_hard$SSE

predict(linear_paper,n.ahead=4)
predict(linear_hard,n.ahead=4)

accuracy(forecast(linear_paper,h=10))
accuracy(forecast(opti_paper,h=10))
accuracy(forecast(linear_hard,h=4))
accuracy(forecast(opti,h=4))

plot(forecast(linear_paper,h=4))
plot(forecast(opti_paper,h=4))
predict(linear_paper,prediction.interval=0.95)
predict(linear_hard,prediction.interval=0.95)
predict(opti_paper,prediction.interval = 0.95)
115.0282^2