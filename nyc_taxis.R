library(ggplot2)
library(forecast)
library(scales)
library(MASS)

## read in and add columns
nyc_taxi_data<-read.csv("f:/nyc_code/total.csv", header=T, as.is=T)
names(nyc_taxi_data)

nyc_taxi_data$date<-as.Date(nyc_taxi_data$date)
nyc_taxi_data$year<-format(nyc_taxi_data$date,"%Y")
nyc_taxi_data$week<-as.Date(format(nyc_taxi_data$date+1,"%Y-%W-1"),"%Y-%W-%u")-1

## remove the week starting 12/28/2014 and greater than 12/24/2017
nyc_taxi_data<-nyc_taxi_data[which(!(is.na(nyc_taxi_data$week) | (nyc_taxi_data$week > "2017-12-25" |  nyc_taxi_data$week < "2015-01-01")  )) ,]



#summarize by week

nyc_taxi_agg1<-aggregate(data=nyc_taxi_data, rides ~ week, FUN=sum) 

#plot by week

ggplot(data=nyc_taxi_agg1, aes(x=week, y=rides)) +
  geom_line(size=1.15) + 
  geom_point()+ scale_y_continuous(labels = scales::comma)+
  ggtitle("NYC Weekly Taxi Rides")+
  xlab("Week")+ylab("Rides") 

summary(nyc_taxi_agg1[which(nyc_taxi_agg1$week>'2016-12-26'), ])

#summarize by week and service type

nyc_taxi_agg2<-aggregate(data=nyc_taxi_data, rides ~ week+ type, FUN=sum) 

#plot by week

ggplot(data=nyc_taxi_agg2, aes(x=week, y=rides, color=type)) +
  geom_line(size=1.15) + 
  scale_color_manual(values=c("red", "green" , "gold3"),name = "Service Type")+
  geom_point()+ scale_y_continuous(labels = scales::comma)+
  ggtitle("NYC Weekly Taxi Rides by Service Type")+
  xlab("Week")+ylab("Rides")
  


#summarize by week and vendor

nyc_taxi_agg3<-aggregate(data=nyc_taxi_data[which(nyc_taxi_data$type %in% c("green","yellow")),], rides ~ week+ vendor, FUN=sum) 

#plot by week

ggplot(data=nyc_taxi_agg3, aes(x=week, y=rides, color=vendor)) +
  geom_line(size=1.15) + 
  geom_point()+ scale_y_continuous(labels = scales::comma)+
  ggtitle("NYC Weekly Taxi Rides by Vendor")+
  xlab("Week")+ylab("Rides")


##  run a basic linear regression

nyc_taxi_agg4<-aggregate(data=nyc_taxi_data, rides ~ date+ week + year + type , FUN=sum) 


mod1.glm<-glm(data=nyc_taxi_agg4, rides ~ date+ week + year + type + type*date  -1 )

mod1.step <- stepAIC(mod1.glm, trace = FALSE)
summary(mod1.step)
qqnorm((mod1.step$residuals-mean(mod1.step$residuals))/sd(mod1.step$residuals), pch = 1, frame = FALSE)
qqline((mod1.step$residuals-mean(mod1.step$residuals))/sd(mod1.step$residuals), col = "steelblue", lwd = 2)
plot(nyc_taxi_agg4$date , (mod1.step$residuals-mean(mod1.step$residuals))/sd(mod1.step$residuals),
     main="Date vs Standardized Residuals for Linear Regression Model" , xlab="Date", ylab= "Standardized Residuals")
abline(0,0)
abline(3,0,col="red")
abline(-3,0,col="red")

#significant devianation from normal indicates another issue in the residuals

acf(mod1.step$residuals)
pacf(mod1.step$residuals)

# acf and pacf plots Strongly suggests time series piece to the model

nyc_taxi_agg4$cab<-0
nyc_taxi_agg4$cab[which(nyc_taxi_agg4$type=="yellow")]<- 1
nyc_taxi_agg4$cab[which(nyc_taxi_agg4$type=="green")]<-0
nyc_taxi_agg4$cab[which(nyc_taxi_agg4$type=="fhv")]<- -1

out.arima<-auto.arima(nyc_taxi_agg4$rides, xreg=c(as.matrix(nyc_taxi_agg4$cab)))
out.arima
pacf(out.arima$residuals)


out.arima2<-arima(nyc_taxi_agg4$rides, xreg=cbind(nyc_taxi_agg4$cab), c(6,0,0),
                  seasonal = list(order = c(4,0,4), period=7))
out.arima2
acf(out.arima2$residuals)
pacf(out.arima2$residuals)

qqnorm((out.arima2$residuals-mean(out.arima2$residuals))/sd(out.arima2$residuals), pch = 1, frame = FALSE)
qqline((out.arima2$residuals-mean(out.arima2$residuals))/sd(out.arima2$residuals), col = "steelblue", lwd = 2)
plot(nyc_taxi_agg4$date , (out.arima2$residuals-mean(out.arima2$residuals))/sd(out.arima2$residuals),
     main="Date vs Standardized Residuals for Time Series + Cab Type Model" , xlab="Date", ylab="Standardized Residuals")
abline(0,0)
abline(3,0,col="red")
abline(-3,0,col="red")

checkresiduals(out.arima2)
checkresiduals(mod1.step)


yellowcab<-nyc_taxi_agg4[which(nyc_taxi_agg4$type=="yellow"),]
greencab<-nyc_taxi_agg4[which(nyc_taxi_agg4$type=="green"),]
fhvcab<-nyc_taxi_agg4[which(nyc_taxi_agg4$type=="fhv"),]

plot(yellowcab$date,yellowcab$rides)

### Yellow Cab ARIMA Modeling
plot(yellowcab$date,yellowcab$rides)

yellowcab.arima<-arima(yellowcab$rides, c(8,0,2), 
                       seasonal = list(order = c(4,0,4), period=7)) 
pacf(yellowcab.arima$residuals)
checkresiduals(yellowcab.arima)

### Yellow Cab 2018 180 day prediction

yellowpreds<-predict(yellowcab.arima, n.ahead = 180)

upperyellow<-yellowpreds$pred + 1.96*yellowpreds$se
loweryellow<-yellowpreds$pred - 1.96*yellowpreds$se

yellowpreds2<-yellowpreds$pred[1:180]

pred_dates<-seq(as.Date("2018-01-01"), by = 1, length.out = 180)                             


plotting_data<-c()
plotting_data<-cbind.data.frame(upperyellow,as.Date(pred_dates))
plotting_data<-cbind.data.frame(plotting_data,loweryellow)
plotting_data<-cbind.data.frame(plotting_data,yellowpreds2)
names(plotting_data)[2]="dates"

plotting_data$week<-as.Date(format(plotting_data$dates+1,"%Y-%W-1"),"%Y-%W-%u")-1
plotting_data2<-aggregate(data=plotting_data, cbind(upperyellow, loweryellow, yellowpreds2) ~ week, FUN=sum)



ggplot(plotting_data2, aes(x = week)) + 
  geom_line(aes(y = upperyellow), colour="red", size=1) + 
  geom_line(aes(y = yellowpreds2), colour = "gold3", size=1) + 
  geom_line(aes(y = loweryellow), colour="red", size=1) + 
  ylab(label="Rides") + 
  xlab("Weekly")+ scale_y_continuous(labels = scales::comma)+
  ggtitle("Weekly Forecast for 2018's First Six Months - Yellow Cab Taxis")



plot(greencab$date,greencab$rides)
acf(greencab$rides)
pacf(greencab$rides)
### Green Cab ARIMA Modeling
plot(greencab$date,greencab$rides)

auto.arima(greencab$rides)


greencab.arima<-arima(greencab$rides, c(6,0,0), 
                       seasonal = list(order = c(2,0,3), period=7)) 
pacf(greencab.arima$residuals)
checkresiduals(greencab.arima)


### green Cab 2018 180 day prediction

greenpreds<-predict(greencab.arima, n.ahead = 180)

uppergreen<-greenpreds$pred + 1.96*greenpreds$se
lowergreen<-greenpreds$pred - 1.96*greenpreds$se

greenpreds2<-greenpreds$pred[1:180]

pred_dates<-seq(as.Date("2018-01-01"), by = 1, length.out = 180)                             


plotting_data<-c()
plotting_data<-cbind.data.frame(uppergreen,as.Date(pred_dates))
plotting_data<-cbind.data.frame(plotting_data,lowergreen)
plotting_data<-cbind.data.frame(plotting_data,greenpreds2)
names(plotting_data)[2]="dates"

plotting_data$week<-as.Date(format(plotting_data$dates+1,"%Y-%W-1"),"%Y-%W-%u")-1
plotting_data2<-aggregate(data=plotting_data, cbind(uppergreen, lowergreen, greenpreds2) ~ week, FUN=sum)

                          
                          
ggplot(plotting_data2, aes(x = week)) + 
  geom_line(aes(y = uppergreen), colour="red", size=1) + 
  geom_line(aes(y = greenpreds2), colour = "green", size=1) + 
  geom_line(aes(y = lowergreen), colour="red", size=1) + 
  ylab(label="Rides") + 
  xlab("Weekly")+ scale_y_continuous(labels = scales::comma)+
  ggtitle("Weekly Forecast for 2018's First Six Months - Green Cab Taxis")


### FHV ARIMA MODELING
plot(fhvcab$date,fhvcab$rides)
fhvcab$outliers<-0; fhvcab$outliers[which(fhvcab$rides<=200000 & fhvcab$date >='2016-01-01' )] <-1
fhvcab$outliers[which(fhvcab$rides<=300000 & fhvcab$date >='2017-01-01' )] <-1

fhvcab.arima<-arima(fhvcab$rides, c(6,0,0), xreg = c(fhvcab$outliers), 
                    seasonal = list(order = c(4,0,4), period=7))
pacf(fhvcab.arima$residuals)
checkresiduals(fhvcab.arima)

### FHV Predicting 180 into 2018
fhvoutliers<- rep(0,180)
fhvpreds<-predict(fhvcab.arima,newxreg =  fhvoutliers, n.ahead = 180)
upperfhv<-fhvpreds$pred + 1.96*fhvpreds$se
lowerfhv<-fhvpreds$pred - 1.96*fhvpreds$se
fhvpreds2<-fhvpreds$pred[1:180]
pred_dates<-seq(as.Date("2018-01-01"), by = 1, length.out = 180)                             
names(plotting_data)[2]="dates"

plotting_data<-c()
plotting_data<-cbind.data.frame(upperfhv,as.Date(pred_dates))
plotting_data<-cbind.data.frame(plotting_data,lowerfhv)
plotting_data<-cbind.data.frame(plotting_data,fhvpreds2)
names(plotting_data)[2]="dates"

plotting_data$week<-as.Date(format(plotting_data$dates+1,"%Y-%W-1"),"%Y-%W-%u")-1
plotting_data2<-aggregate(data=plotting_data, cbind(upperfhv, lowerfhv, fhvpreds2) ~ week, FUN=sum)



ggplot(plotting_data2, aes(x = week)) + 
  geom_line(aes(y = upperfhv), colour="red", size=1) + 
  geom_line(aes(y = fhvpreds2), colour = "black", size=1) + 
  geom_line(aes(y = lowerfhv), colour="red", size=1) + 
  ylab(label="Rides") + 
  xlab("Weekly")+ scale_y_continuous(labels = scales::comma)+
  ggtitle("Weekly Forecast for 2018's First Six Months - FHV")

qqnorm((out.arima2$residuals-mean(out.arima2$residuals))/sd(out.arima2$residuals), pch = 1, frame = FALSE)
qqline((out.arima2$residuals-mean(out.arima2$residuals))/sd(out.arima2$residuals), col = "steelblue", lwd = 2)
plot(nyc_taxi_agg4$date , (out.arima2$residuals-mean(out.arima2$residuals))/sd(out.arima2$residuals),
     main="Date vs Standardized Residuals for Time Series + Cab Type Model" , xlab="Date", ylab="Standardized Residuals")
abline(0,0)
abline(3,0,col="red")
abline(-3,0,col="red")

        
