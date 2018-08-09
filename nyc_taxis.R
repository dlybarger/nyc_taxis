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


out.arima2<-arima(nyc_taxi_agg4$rides, xreg=cbind(nyc_taxi_agg4$cab), c(3,0,5),
                  seasonal = list(order = c(6,0,6), period=7))
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

        