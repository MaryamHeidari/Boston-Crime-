#setwd("/Users/findlee/Desktop/Master's Degree 2018:2019/Summer 2019/ALY6040 90650 Data Mining Applications/final") 
#crime <- read.csv("crime.csv", stringsAsFactors = FALSE)
df <- read.csv(file.choose(), stringsAsFactors = FALSE)
head(df, n=2)
#####EDA
#Select necessary variables
df <- subset(df, select = c(OFFENSE_DESCRIPTION,OFFENSE_CODE_GROUP,
                            DISTRICT, OCCURRED_ON_DATE,OCCURRED_ON_DATE,
                            YEAR,MONTH,DAY_OF_WEEK,HOUR,
                            UCR_PART, STREET))
str(df)
nrow(df)
ncol(df)
#check the missing values
any(is.na(df))

install.packages("dplyr")
library(dplyr)
df <- filter(df, UCR_PART != "") #remove blank in UCR_PART
df <- filter(df,  DISTRICT != "") #remove blank in DISTRICT
#Removing duplicates
data <- unique(df)
nrow(data)

#EDA
install.packages("ggplot2")
library(ggplot2)

#bar plot of crime categories
ggplot(data = filter(data, UCR_PART == "Part One")) +
  geom_bar(mapping = aes(x = OFFENSE_CODE_GROUP)) +
  xlab("")+
  ggtitle("Bar plot of Part 1 - Highly Serious Crimes")+
  coord_flip()

ggplot(data = filter(data, UCR_PART == "Part Two")) +
  geom_bar(mapping = aes(x = OFFENSE_CODE_GROUP)) +
  xlab("")+
  ggtitle("Bar plot of Part 2 - Serious Crimes")+
  coord_flip()

ggplot(data = filter(data, UCR_PART == "Part Three")) +
  geom_bar(mapping = aes(x = OFFENSE_CODE_GROUP)) +
  xlab("")+
  ggtitle("Bar plot of Part 3 - Less Serious Crimes")+
  coord_flip()

ggplot(data = filter(data, UCR_PART == "Other")) +
  geom_bar(mapping = aes(x = OFFENSE_CODE_GROUP)) +
  xlab("")+
  ggtitle("Bar plot of Other Crimes")+
  coord_flip()
#bar plot of number of crime base on Districts
ggplot(data = data) +
  geom_bar(mapping = aes(x = data$DISTRICT)) +
  xlab("")
  
#######CLUSTRING 
data1<- data[,c("OFFENSE_CODE_GROUP","STREET","YEAR","MONTH","DAY_OF_WEEK","HOUR")]
install.packages("klaR")
library(klaR)
result <- kmodes(data1, modes = 4)
attributes(result)
result$size
table(data$UCR_PART,result$cluster)
table(data1$STREET,result$cluster)
table(data1$YEAR,result$cluster)
table(data1$MONTH,result$cluster)
table(data1$DAY_OF_WEEK,result$cluster)
table(data1$HOUR,result$cluster)

# library(forecast)
##### TIME SERIES
install.packages("forecast")
library('forecast')
install.packages("tseries")
library('tseries')
tb<-table(data$OFFENSE_CODE_GROUP)
tdf<-as.data.frame(tb)
tdf$Var1 <- NULL

tscrime<- ts(data = tdf,
             frequency=12,  #for monthly
             start = 2015, end = 2019) #data is arranged from 1st quarter 1950

plot.ts(tscrime) 
#decomp <- stl(tscrime, s.window= "periodic")
decomp <- decompose(tscrime)
plot(decomp) 

#adjust seasonal effect
deseasonal<- seasadj(decomp)
plot(deseasonal) #plot the seasonal effect

auto.arima(deseasonal, seasonal=FALSE) 
##predict values after 1990 based on ARIMA model
a = arima(deseasonal, order=c(0,1,1))  #fit ARIMA model
fcast <- forecast(a, h= 60)       #forecast the model
plot(forecast(fcast))         
