library(ggplot2)
library(cowplot)
library(car)
library(forecast)
library(tseries)
require(graphics)

setwd("~/Documents/My R/Time Series/Case Study")
rawdata <- read.csv("Global Superstore.csv")
rawdata$Order.Date <-strptime(rawdata$Order.Date,format='%d-%m-%Y')
rawdata$Ship.Date <- strptime(rawdata$Ship.Date,format='%d-%m-%Y')
sum(is.na(rawdata))
colnames(rawdata)[apply(is.na(rawdata), 2, any)]

rawdata$Segment <- as.factor(rawdata$Segment)
levels(rawdata$Segment)

rawdata$Market <- factor(rawdata$Market)
levels(rawdata$Market)

rawdata$Category <- factor(rawdata$Category)
levels(rawdata$Category)

rawdata$Sub.Category <- factor(rawdata$Sub.Category)
levels(rawdata$Sub.Category)


rawdata$Month <- strftime(rawdata$Order.Date, "%m")
rawdata$Year <- strftime(rawdata$Order.Date, "%Y")
monthly_agg <- aggregate(cbind(Sales,Quantity, Profit) ~ Market + Segment + Month + Year, rawdata, FUN = sum)
monthly_agg$timeframe <- as.POSIXct(paste(monthly_agg$Year, monthly_agg$Month,'01', sep = "-"))


Africa_Consumer <- subset(monthly_agg,  Market == "Africa" & Segment == "Consumer")[,c(8,5,6,7)]
Africa_Corporate <- subset(monthly_agg,  Market == "Africa" & Segment == "Corporate")[,c(8,5,6,7)]
Africa_HomeOffice <- subset(monthly_agg,  Market == "Africa" & Segment == "Home Office")[,c(8,5,6,7)]

APAC_Consumer <- subset(monthly_agg,  Market == "APAC" & Segment == "Consumer")[,c(8,5,6,7)]
APAC_Corporate <- subset(monthly_agg,  Market == "APAC" & Segment == "Corporate")[,c(8,5,6,7)]
APAC_HomeOffice <- subset(monthly_agg,  Market == "APAC" & Segment == "Home Office")[,c(8,5,6,7)]

Canada_Consumer <- subset(monthly_agg,  Market == "Canada" & Segment == "Consumer")[,c(8,5,6,7)]
Canada_Corporate <- subset(monthly_agg,  Market == "Canada" & Segment == "Corporate")[,c(8,5,6,7)]
Canada_HomeOffice <- subset(monthly_agg,  Market == "Canada" & Segment == "Home Office")[,c(8,5,6,7)]

EMEA_Consumer <- subset(monthly_agg,  Market == "EMEA" & Segment == "Consumer")[,c(8,5,6,7)]
EMEA_Corporate <- subset(monthly_agg,  Market == "EMEA" & Segment == "Corporate")[,c(8,5,6,7)]
EMEA_HomeOffice <- subset(monthly_agg,  Market == "EMEA" & Segment == "Home Office")[,c(8,5,6,7)]

EU_Consumer <- subset(monthly_agg,  Market == "EU" & Segment == "Consumer")[,c(8,5,6,7)]
EU_Corporate <- subset(monthly_agg,  Market == "EU" & Segment == "Corporate")[,c(8,5,6,7)]
EU_HomeOffice <- subset(monthly_agg,  Market == "EU" & Segment == "Home Office")[,c(8,5,6,7)]

LATAM_Consumer <- subset(monthly_agg,  Market == "LATAM" & Segment == "Consumer")[,c(8,5,6,7)]
LATAM_Corporate <- subset(monthly_agg,  Market == "LATAM" & Segment == "Corporate")[,c(8,5,6,7)]
LATAM_HomeOffice <- subset(monthly_agg,  Market == "LATAM" & Segment == "Home Office")[,c(8,5,6,7)]

US_Consumer <- subset(monthly_agg,  Market == "US" & Segment == "Consumer")[,c(8,5,6,7)]
US_Corporate <- subset(monthly_agg,  Market == "US" & Segment == "Corporate")[,c(8,5,6,7)]
US_HomeOffice <- subset(monthly_agg,  Market == "US" & Segment == "Home Office")[,c(8,5,6,7)]

Market_levels <- levels(monthly_agg$Market)
Segment_levels <- levels(monthly_agg$Segment)

cv_matrix <- data.frame(Market.Segment = character(),CV = numeric(), stringsAsFactors = FALSE)
total_segments <- length(Market_levels) * length(Segment_levels)

x <- 0
for (i in 1:length(Market_levels)){
 for (j in 1:length(Segment_levels)){
       x <- x + 1
        cv_matrix[x,1] <- paste(Market_levels[i], gsub(" ", "", Segment_levels[j], fixed = TRUE),sep = "_")
        cv_matrix[(x),2] <-  sd(get(cv_matrix[x,1])$Profit) / mean(get(cv_matrix[x,1])$Profit) 
       }
    }
cv_matrix$Market.Segment  <- factor(cv_matrix$Market.Segment, levels = cv_matrix[order(cv_matrix$CV), "Market.Segment"])

#------------------------------------------------------------------------------------------#
theme1 <- theme_minimal()
theme1 <- theme1 + theme(axis.text.x = element_text(angle = 90))

plot_new <- ggplot(cv_matrix, aes(x = Market.Segment, y = CV, fill = CV)) 
plot_new <- plot_new + geom_bar(stat = "identity") + theme1 + coord_flip() 
plot_new
ggsave("plot1.jpeg")




plot5 <- ggplot(monthly_agg, aes(x = Profit, color = Market))
plot5 <- plot5 + geom_density() + theme1 + scale_color_brewer(palette="Dark2")
plot5

plot6 <- ggplot(monthly_agg, aes(x = Profit, color = Segment))
plot6 <- plot6 + geom_density() + theme1 + scale_color_brewer(palette="Dark2")
plot6

plot7 <- ggplot(monthly_agg, aes(x = Sales, color = Market))
plot7 <- plot7 + geom_density() + theme1 + scale_color_brewer(palette="Dark2")
plot7

plot8 <- ggplot(monthly_agg, aes(x = Sales, color = Segment)) 
plot8 <- plot8 + geom_density() + theme1 + scale_color_brewer(palette="Dark2")
plot8

plot9 <- ggplot(monthly_agg, aes(x = Segment, y= Profit, fill = Segment))
plot9 <- plot9 + geom_boxplot() + theme1
plot9 + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) 
ggsave("plot9.jpeg")

plot9_1 <- ggplot(monthly_agg, aes(x = Market , y = Profit ,fill = Market)) 
plot9_1 <- plot9_1 + geom_boxplot() + theme1
plot9_1 + scale_fill_brewer(palette="Dark2") + facet_grid(Segment ~ .)
ggsave("plot9_1.jpeg")

plot9_2 <- ggplot(monthly_agg, aes(x = Market,y = Quantity, fill = Market)) 
plot9_2 <- plot9_2 + geom_boxplot() + theme1
plot9_2 + scale_fill_brewer(palette="Dark2") +facet_grid(Segment ~ .)
ggsave("plot9_2.jpeg")

plot9_3 <- ggplot(monthly_agg, aes(x = Market,y = Sales, fill = Market)) 
plot9_3 <- plot9_3 + geom_boxplot() + theme1
plot9_3 + scale_fill_brewer(palette="Dark2") +facet_grid(Segment ~ .)
ggsave("plot9_3.jpeg")

#####
plot10 <- ggplot(monthly_agg, aes(x = Market, y= Sales, color = Market))
plot10 <- plot10 + geom_line(size = 2)  + theme1 + scale_color_brewer(palette="Dark2")

plot11 <- ggplot(monthly_agg, aes(x = Market, y= Profit, color = Profit))
plot11 <- plot11 + geom_line(size = 2)  + theme1 + scale_colour_gradient(low="red", high="green")

plot12 <- ggplot(monthly_agg, aes(x = Segment, y= Sales, color = Segment))
plot12 <- plot12 + geom_line(size = 2)  + theme1 + scale_color_brewer(palette="Dark2")

plot13 <- ggplot(monthly_agg, aes(x = Segment, y= Profit, color = Profit))
plot13 <- plot13 + geom_line(size = 2)  + theme1 + scale_colour_gradient(low="red", high="green")
plot_grid(plot10, plot12,plot11, plot13)
ggsave("plotgrid1.jpeg")

plot1 <- ggplot(EU_Consumer, aes(x= timeframe, y=Profit )) + geom_line() + theme1
plot2 <- ggplot(EU_Consumer, aes(x= timeframe, y=Sales )) + geom_line() + theme1
plot3 <- ggplot(APAC_Consumer, aes(x= timeframe, y=Profit )) + geom_line() + theme1
plot4 <- ggplot(APAC_Consumer, aes(x= timeframe, y=Sales )) + geom_line() + theme1
plot_grid(plot1,plot2,plot3,plot4)
ggsave("plotgrid2.jpeg")
#------------------------------------------------------------------------------------------#
#Classical Decomposition EU Consumer Sales
timevals_in <- c(1:42)
timevals_out <- c(43:48) 
ylab <- c("EU Consumer Sales Time Series")
xlab <- c("Months from Jan 2011")
main_title <- c("Time Series Plot of Market: EU , Segment: Consumer, Data: Sales")
sub        <- c("Sales of company Global Mart, Jan 2011 to June 2014")
timeser <- ts( EU_Consumer[1:42,2])
#tiff("Name1.tiff",  width = 8, height = 6, units = 'in', res = 300)
plot(timeser, xlab = xlab, ylab = ylab , main = main_title ,sub = sub ) 
w <-.6
smoothedseries <- filter(timeser, 
                         filter=rep((0.2+(1/(.5*w+1))),(0.2+(.5*w+1))), 
                         method='convolution', sides=2)
#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,.5,-.5)) {
  smoothedseries[i] <- smoothedseries[i+.5] - diff
}
#Smoothing right end of the time series
n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-.5]
for (i in seq(n-w+.5, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}
lines(smoothedseries, col="blue", lwd=2)
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

lmfit <- lm(Sales ~ sin(4*Month) * poly(Month,3) + cos(0.5*Month) * (poly(Month,2) +  ( Month)),
            data=smootheddf)

global_pred <- predict(lmfit,data.frame(Month=timevals_in))
summary(global_pred)
lines( global_pred, col='red', lwd=2) 
legend(x = "topleft", y=0.9, legend=c("Train Prediction", "Smoothed Series", "Actual data"), col=c("red", "blue","Black"), lty=1:1, cex=0.5)

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
kpss.test(resi)

outdata <- EU_Consumer[43:48,2]
outdata
global_pred_out <- predict(lmfit,data.frame(Month = timevals_out))
global_pred_out
fcast <- global_pred_out

MAPE_class_dec <- accuracy(fcast,outdata)[5]
MAPE_class_dec

#Plot the predictions along with original values
total_timeser <-  ts(EU_Consumer$Sales)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))

plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")
#------------------------------------------------------------------------------------------#
#Auto Arima EU Sunsumer Sales TS
autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
resi_auto_arima <- timeser - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata)[5]
MAPE_auto_arima
#------------------------------------------------------------------------------------------#
#Classical Decomposition EU Consumer Quantity
timeser <- ts(EU_Consumer[1:42,3])
ylab <- c("EU Consumer Quantity Time Series")
xlab <- c("Months from Jan 2011")
main_title <- c("Time Series Plot of Market: EU , Segment: Consumer, Data: Quantity")
sub        <- c("Quanity of company Global Mart, Jan 2011 to June 2014")
plot(timeser, xlab = xlab, ylab = ylab , main = main_title ,sub = sub ) 
w <-.6
smoothedseries <- filter(timeser , 
                         filter=rep(0.2+(1/(.5*w+1)),0.2+((.5*w+1))), 
                         method='convolution', sides=1)

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,.5,-.5)) {
  smoothedseries[i] <- smoothedseries[i+.5] - diff
}

#Smoothing right end of the time series

n <- length(timeser )
diff <- smoothedseries[n-w] - smoothedseries[n-w-.5]
for (i in seq(n-w+.5, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

lines(smoothedseries, col="blue", lwd=2)

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

lmfit <- lm(Quantity ~ sin(4*Month) * poly(Month,3) + cos(.6155*Month) * (poly(Month,3) +  ( Month)),
            data=smootheddf)

length(lmfit$coefficients) > lmfit$rank
lmfit$coefficients
global_pred <- predict(lmfit,data.frame(Month=timevals_in))
summary(global_pred)
lines( global_pred, col='red', lwd=2)
legend(x = "topleft", y=0.9, legend=c("Train Prediction", "Smoothed Series", "Actual data"), col=c("red", "blue","Black"), lty=1:1, cex=0.5)
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")

acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
kpss.test(resi)

outdata <- EU_Consumer[43:48,3]
global_pred_out <- predict(lmfit,data.frame(Month = timevals_out))
global_pred_out
fcast <- global_pred_out

MAPE_class_dec <- accuracy(fcast,outdata)[5]
MAPE_class_dec
#Plot the predictions along with original values
total_timeser <-  ts(EU_Consumer$Quantity)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")
#------------------------------------------------------------------------------------------#
#Auto Arima EU Consumer Quantity
autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
resi_auto_arima <- timeser - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata)[5]
MAPE_auto_arima
#------------------------------------------------------------------------------------------#
#Classical Decomposition APAC Consumer Sales
timeser <- ts(APAC_Consumer[1:42,2])
ylab <- c("APAC Consumer Sales Time Series")
xlab <- c("Months from Jan 2011")
main_title <- c("Time Series Plot of Market: APAC , Segment: Consumer, Data: Sales")
sub        <- c("Quanity of company Global Mart, Jan 2011 to June 2014")
plot(timeser, xlab = xlab, ylab = ylab , main = main_title ,sub = sub ) 
w <-.6
smoothedseries <- filter(timeser , 
                         filter=rep(0.2+(1/(.5*w+1)),0.2+((.5*w+1))), 
                         method='convolution', sides=1)
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,.5,-.5)) {
  smoothedseries[i] <- smoothedseries[i+.5] - diff
}
#Smoothing right end of the time series
n <- length(ts_EU_Consumer_Quantity )
diff <- smoothedseries[n-w] - smoothedseries[n-w-.5]
for (i in seq(n-w+.5, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

lines(smoothedseries, col="blue", lwd=2)

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

lmfit <- lm(Sales ~ sin(5*Month) * poly(Month,2) + cos(.5*Month) * (poly(Month,3) + ( Month)),
            data=smootheddf)

length(lmfit$coefficients) > lmfit$rank
lmfit$coefficients
global_pred <- predict(lmfit,data.frame(Month=timevals_in))
summary(global_pred)
lines( global_pred, col='red', lwd=2)
legend(x = "topleft", y=0.9, legend=c("Train Prediction", "Smoothed Series", "Actual data"), col=c("red", "blue","Black"), lty=1:1, cex=0.5)
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")

acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
kpss.test(resi)

outdata <- APAC_Consumer[43:48,2]
outdata
global_pred_out <- predict(lmfit,data.frame(Month = timevals_out))
global_pred_out
fcast <- global_pred_out
MAPE_class_dec <- accuracy(fcast,outdata)[5]
MAPE_class_dec
#Plot the predictions along with original values
total_timeser <-  ts(APAC_Consumer$Sales)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")
#------------------------------------------------------------------------------------------#
#Auto Arima EU Sunsumer Sales TS
autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
resi_auto_arima <- timeser - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata)[5]
MAPE_auto_arima
#------------------------------------------------------------------------------------------#
#Classical Decomposition EU Consumer Quantity
timeser <- ts(APAC_Consumer[1:42,3])
ylab <- c("APAC Consumer Quantity Time Series")
xlab <- c("Months from Jan 2011")
main_title <- c("Time Series Plot of Market: APAC , Segment: Consumer, Data: Quantity")
sub        <- c("Quanity of company Global Mart, Jan 2011 to June 2014")
plot(timeser, xlab = xlab, ylab = ylab , main = main_title ,sub = sub ) 
w <-.6
smoothedseries <- filter(timeser , 
                         filter=rep(0.2+(1/(.5*w+1)),0.2+((.5*w+1))), 
                         method='convolution', sides=1)

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,.5,-.5)) {
  smoothedseries[i] <- smoothedseries[i+.5] - diff
}

#Smoothing right end of the time series

n <- length(timeser )
diff <- smoothedseries[n-w] - smoothedseries[n-w-.5]
for (i in seq(n-w+.5, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

lines(smoothedseries, col="blue", lwd=2)

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

lmfit <- lm(Quantity ~ sin(4*Month) * poly(Month,3) + cos(.61*Month) * (poly(Month,2) +  ( Month)),
            data=smootheddf)

length(lmfit$coefficients) > lmfit$rank
lmfit$coefficients
global_pred <- predict(lmfit,data.frame(Month=timevals_in))
summary(global_pred)
lines( global_pred, col='red', lwd=2)
legend(x = "topleft", y=0.9, legend=c("Train Prediction", "Smoothed Series", "Actual data"), col=c("red", "blue","Black"), lty=1:1, cex=0.5)
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")

acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
kpss.test(resi)

outdata <- EU_Consumer[43:48,3]
global_pred_out <- predict(lmfit,data.frame(Month = timevals_out))
global_pred_out
fcast <- global_pred_out

MAPE_class_dec <- accuracy(fcast,outdata)[5]
MAPE_class_dec
#Plot the predictions along with original values
total_timeser <-  ts(EU_Consumer$Quantity)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

#------------------------------------------------------------------------------------------#
#Auto Arima EU Sunsumer Sales TS
autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
resi_auto_arima <- timeser - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata)[5]
MAPE_auto_arima


