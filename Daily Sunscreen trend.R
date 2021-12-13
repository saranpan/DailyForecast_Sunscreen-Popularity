#install.packages('lubridate')
library(forecast);
library(tseries);
library(glue);
library(lubridate)

setwd("C:/Users/Wallik/Desktop/Sunscreen Trend project")

df <- read.csv('daily_sunscreen.csv',
               header=TRUE,sep = '')

# Size
nrow(df) # = 4748 
ncol(df) # = 3

attach(df)

#start=c(2019,1,13); end=c(2021,12,5)

df.ts = ts(est_hits , 
           start= decimal_date(as.Date("2008-12-01")),freq=365)

plot(df.ts,main ='Daily "Sunscreen" (Dec.2008 - Dec.2021) ') 

plot(aggregate(df.ts,FUN='mean'),main ='Yearly "Sunscreen" (Dec.2008 - Dec.2021) ')

boxplot(df.ts ~ cycle(df.ts),main ='Boxplot for each day (Dec.2008 - Dec.2021)')
