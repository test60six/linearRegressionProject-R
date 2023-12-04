#install package dependencies 
install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrgram")
install.packages("corrplot")
install.packages("ggthemes")
install.packages("caTools")

library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)
#import data 
df<- read.csv("bikeshare.csv")
#check the head
head(df)

#predict the counts


#explore the data 

ggplot(df,aes(temp,count)) + geom_point(alpha=0.2, aes(color=temp)) + theme_bw()

#Plot count versus datetime as a scatterplot with a color gradient based on temperature.
df$datetime <- as.POSIXct(df$datetime)


#plot data of count vs hour based on temperature on working days 
pl <- ggplot(filter(df,workingday==1),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()

#same plot based on non working days 
pl <- ggplot(filter(df,workingday==0),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.8)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()


#What is the correlation between temp and count?
cor(df[,c("temp","count")])


#explore season data 
ggplot(df,aes(factor(season),count)) + geom_boxplot(aes(color=factor(season))) +theme_bw()

#create hour column from date time column
time.stamp <- df$datetime[4]
format(time.stamp, "%H")

df$hour <- sapply(df$datetime,function(x){format(x,"%H")})

#view head
head(df)

#plot count vs hour with scale based on temperature 
pl <- ggplot(filter(df,workingday==1),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()

#building the linear model

temp.model <- lm(count~temp,df)

#summary of temp model
summary(temp.model)

#How many bike rentals would we predict if the temperature was 25 degrees Celsius

# Method 1
6.0462 + 9.17*25

# Method 2
temp.test <- data.frame(temp=c(25))
predict(temp.model,temp.test)

#change hour to numeric value
df$hour <- sapply(df$hour,as.numeric)

#model and summary 
model <- lm(count ~ . -casual - registered -datetime -atemp,df )
summary(model)


