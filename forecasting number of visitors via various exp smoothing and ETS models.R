#Forecasting the number of visitors using the monthly Australian short-term overseas visitors data via
#i. Multiplicative Holt-Winters' method
#ii. Additive Holt-Winters' method
#iii. ETS model with damped trend
#iv. Optimal ETS model

visit<-read.table("visitors.txt")
visit_ts<-ts(visit,frequency=12,start = c(1985,5),end=c(2005,4))
k<-decompose(visit_ts,type = "mult")
plot(k)
acf((visit_ts))
plot(visit_ts)
visit_ts

visit_mul<-HoltWinters(visit_ts,seasonal = "mult")
predict(visit_mul,n.ahead = 24)
forecast(visit_mul,h=24)

visit_add<-HoltWinters(visit_ts,seasonal = "add")
add_vi_f<-predict(visit_add,n.ahead = 24)
visit_add
forecast(visit_add,h=24)
plot(visit_add,add_vi_f)
ets_damped<-ets(visit_ts,damped = TRUE)

forecast(ets_damped,h=24)
plot(f_ets_damped)
optimal_ets<-ets(visit_ts)
optimal_ets
forecast(optimal_ets)


visit_mul$SSE

sseofpaper<-c(ses_paperback$SSE, ses_paperback3$SSE,ses_paperback1$SSE,ses_paperback4$SSE,ses_paperback2$SSE)
alphasofpaper<-c(0.2,0.4,0.6,0.75,0.9)
plot(alphasofpaper,sseofpaper,col="blue",pch=16)

sseofhard<-c(ses_hardcover$SSE, ses_hardcover3$SSE,ses_hardcover1$SSE,ses_hardcover4$SSE,ses_hardcover2$SSE)
alphasofhard<-c(0.2,0.4,0.6,0.75,0.9)
plot(alphasofhard,sseofhard,col="blue",pch=16)



accuracy(forecast(ets_damped,h=2))
accuracy(forecast(optimal_ets,h=2))
accuracy(forecast(visit_add),h=2)
accuracy(forecast(visit_mul),h=2)
