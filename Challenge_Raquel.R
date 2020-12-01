setwd("C:/Users/Novo/Desktop/Desafio 4 intelligence")
rm(list=ls())
graphics.off()


########################case1#################################


install.packages("forecast")
library(forecast)

install.packages("timeSeries")
library(timeSeries)
library(timeDate)
library(graphics)
library(dplyr)
library(labelled)

tfp <- read.csv(file = "TFP.csv", sep = ",")
head(tfp)

###### Análise para USA
subdata_tfp_USA <- tfp %>%
  filter(isocode == "USA")

copy_labels(tfp, subdata_tfp_USA)

subdata_tfp_USA <- ts(subdata_tfp_USA, star=1950, freq=1)

# Verificação do gráfico da série 

ts.plot(subdata_tfp_USA[,3],  main = "TPF USA")

# Estatísticas e testes estatísticos para séries de tempo

summary(subdata_tfp_USA[,3]) #estatística descritiva USA
sd(subdata_tfp_USA[,3], na.rm = FALSE) #Desvio Padrão USA



###### Análise para CAN

subdata_tfp_CAN <- tfp %>%
  filter(isocode == "CAN")

copy_labels(tfp, subdata_tfp_CAN)

subdata_tfp_CAN <- ts(subdata_tfp_CAN, star=1950, freq=1)

# Verificação do gráfico da série 

ts.plot(subdata_tfp_CAN[,3],  main = "TPF CAN")

# Estatísticas e testes estatísticos para séries de tempo

summary(subdata_tfp_CAN[,3]) #estatística descritiva CAN
sd(subdata_tfp_CAN[,3], na.rm = FALSE) #Desvio Padrão CAN



###### Análise para MEX

subdata_tfp_MEX <- tfp %>%
  filter(isocode == "MEX")

copy_labels(tfp, subdata_tfp_MEX)

subdata_tfp_MEX <- ts(subdata_tfp_MEX, star=1950, freq=1)

# Verificação do gráfico da série 

ts.plot(subdata_tfp_MEX[,3],  main = "TPF MEX")

# Estatísticas e testes estatísticos para séries de tempo

summary(subdata_tfp_MEX[,3]) #estatística descritiva MEX
sd(subdata_tfp_MEX[,3], na.rm = FALSE) #Desvio Padrão MEX


#previsão 10 anos USA 
library(forecast)
library(tidyverse)

subdata_tfp_USA[,3] %>%
  ets() %>%
  forecast(h = 10) %>%
  autoplot(main = "Forecast USA")


#previsão 10 anos CAN
subdata_tfp_CAN[,3] %>%
  ets() %>%
  forecast(h = 10) %>%
  autoplot(main = "Forecast CAN")


#previsão 10 anos MEX
subdata_tfp_MEX[,3] %>%
  ets() %>%
  forecast(h = 10) %>%
  autoplot(main = "Forecast MEX")

#### Can you think about another feature that could be helpful in explaining TFP series? Explain.

library(pwt8)
View(pwt8.0)


########################### CASE 2 #################################

rm(list=ls())
graphics.off()

library(dplyr)

install.packages("ggplot2")

library(ggplot2)

install.packages("labelled")
library(labelled)

comex <- read.csv(file = "data_comexstat.csv", sep = ",")
View(comex)
head(comex)

# Show the evolution of total monthly and total annual exports from Brazil (all states and to everywhere) of 'soybeans', 'soybean oil' and 'soybean meal'

library(dplyr)

#soybeans by year 
subdata_soybeans <- comex %>% 
  filter(type %in% c("Export")) %>%
  filter(product =="soybeans")

copy_labels(comex, subdata_soybeans)

subdata_soybeans$date <- as.Date(subdata_soybeans$date)

aggregate_tons <- aggregate(subdata_soybeans$tons, by=list(subdata_soybeans$date),sum)


subdata_soybeans_TS = ts(aggregate_tons, start = 1997, end=2019, frequency = 1)

plot(subdata_soybeans_TS[,2], main= "Total soybeans exports by year", ylab = "TOTAL EXPORT")


#soybeans by month
subdata_soybeans <- comex %>% 
  filter(type %in% c("Export")) %>%
  filter(product =="soybeans")

copy_labels(comex, subdata_soybeans)

subdata_soybeans$date <- as.Date(subdata_soybeans$date)

aggregate_tons <- aggregate(subdata_soybeans$tons, by=list(subdata_soybeans$date),sum)


subdata_soybeans_TS = ts(aggregate_tons, start = 1997, end=2019, frequency = 12)

plot(subdata_soybeans_TS[,2], main = "Total soybeans exports by month", ylab = "TOTAL EXPORT")




#beansoil by year

subdata_soybeansoil <- comex %>% 
  filter(type %in% c("Export")) %>%
  filter(product =="soybean_oil")

copy_labels(comex, subdata_soybeansoil)

subdata_soybeansoil$date <- as.Date(subdata_soybeansoil$date)

aggregate_tons_beansoil <- aggregate(subdata_soybeansoil$tons, by=list(subdata_soybeansoil$date),sum)


subdata_soybeansoil_TS = ts(aggregate_tons_beansoil, start = 1997, end=2019, frequency = 1)

plot(subdata_soybeansoil_TS[,2], main = "Total soybean oil export by year", ylab = "TOTAL EXPORT")



#beans oil by month


subdata_soybeansoil <- comex %>% 
  filter(type %in% c("Export")) %>%
  filter(product =="soybean_oil")

copy_labels(comex, subdata_soybeansoil)

subdata_soybeansoil$date <- as.Date(subdata_soybeansoil$date)

aggregate_tons_beansoil <- aggregate(subdata_soybeansoil$tons, by=list(subdata_soybeansoil$date),sum)


subdata_soybeansoil_TS = ts(aggregate_tons_beansoil, start = 1997, end=2019, frequency = 12)


plot(subdata_soybeansoil_TS[,2], main = "Total soybean oil export by month", ylab = "TOTAL EXPORT")




#beans meal by year

subdata_soybeansmeal <- comex %>% 
  filter(type %in% c("Export")) %>%
  filter(product =="soybean_meal")

copy_labels(comex, subdata_soybeansmeal)

subdata_soybeansmeal$date <- as.Date(subdata_soybeansmeal$date)

aggregate_tons_beansmeal <- aggregate(subdata_soybeansmeal$tons, by=list(subdata_soybeansmeal$date),sum)


subdata_soybeansmeal_TS = ts(aggregate_tons_beansmeal, start = 1997, end=2019, frequency = 1)


plot(subdata_soybeansmeal_TS[,2], main = "Total soybean meal export by year", ylab = "TOTAL EXPORT")



#beans meal by month


subdata_soybeansmeal <- comex %>% 
  filter(type %in% c("Export")) %>%
  filter(product =="soybean_meal")

copy_labels(comex, subdata_soybeansmeal)

subdata_soybeansmeal$date <- as.Date(subdata_soybeansmeal$date)

aggregate_tons_beansmeal <- aggregate(subdata_soybeansmeal$tons, by=list(subdata_soybeansmeal$date),sum)


subdata_soybeansmeal_TS = ts(aggregate_tons_beansmeal, start = 1997, end=2019, frequency = 12)


plot(subdata_soybeansmeal_TS[,2], main = "Total soybean meal export by month", ylab = "TOTAL EXPORT")





### What are the 3 most important products exported by Brazil in the last 5 years?


subdata_export <- comex %>% 
  filter(type %in% c("Export")) 

copy_labels(comex, subdata_export)


subdata_export$date <- as.Date(subdata_export$date)

library(lubridate)
subdata_export_2015_2019 <- mutate(subdata_export, year = year(date))
subdata_export_2015_2019 <- subdata_export_2015_2019[!(subdata_export_2015_2019$year <= 2014),]


install.packages("ggplot2")
library(ggplot2)

g <- ggplot(subdata_export_2015_2019, aes(product))

g + geom_bar()

ggplot(subdata_export_2015_2019) + geom_bar(aes(y = product))


#### What are the main routes through which Brazil have been exporting 'corn' in the last few years? 

#corn

library(dplyr)
subdata_export_corn <- comex %>% 
  filter(type %in% c("Export")) %>%
  filter(product == "corn")

library(labelled)    
copy_labels(comex, subdata_export_corn)


subdata_export_corn$date <- as.Date(subdata_export_corn$date)

library(lubridate)
subdata_export_corn_2015_2019 <- mutate(subdata_export_corn, year = year(date))
subdata_export_corn_2015_2019 <- subdata_export_corn_2015_2019[!(subdata_export_corn_2015_2019$year <= 2014),]

install.packages("ggplot2")
library(ggplot2)


ggplot(subdata_export_corn_2015_2019) + geom_bar(aes( y= route))


### Are there differences in the relative importance of routes depending on the product?

library(dplyr)

subdata_export_route <- comex %>% 
  filter(type %in% c("Export"))

library(labelled)    
copy_labels(comex, subdata_export_route)


subdata_export_route$date <- as.Date(subdata_export_route$date)

library(lubridate)
subdata_export_route_2015_2019 <- mutate(subdata_export_route, year = year(date))
subdata_export_route_2015_2019 <- subdata_export_route_2015_2019[!(subdata_export_route_2015_2019$year <= 2014),]

install.packages("ggplot2")
library(ggplot2)

g <- ggplot(subdata_export_route_2015_2019, aes(product))

g + geom_bar(aes(fill= route))



#### Which countries have been the most important trade partners for Brazil in terms of 'corn' and 'sugar' in the last 3 years?

#corn


subdata_export_corn <- comex %>% 
  filter(type %in% c("Export")) %>%
  filter(product == "corn")

copy_labels(comex, subdata_export_corn)


subdata_export_corn$date <- as.Date(subdata_export_corn$date)

library(lubridate)
subdata_export_corn_2017_2019 <- mutate(subdata_export_corn, year = year(date))
subdata_export_corn_2017_2019 <- subdata_export_corn_2017_2019[!(subdata_export_corn_2017_2019$year <= 2016),]

aggregate_export_corn_contry <- aggregate(subdata_export_corn_2017_2019$tons, list(country = subdata_export_corn_2017_2019$country), sum)
aggregate_export_corn_contry <- aggregate_export_corn_contry[!(aggregate_export_corn_contry$x <= 5000000),]

install.packages("ggplot2")
library(ggplot2)

ggplot(aggregate_export_corn_contry, aes(x = country, y = x)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "parceiros de exportação de milho",
       x = "País",
       y = "exportações")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#sugar

subdata_export_sugar <- comex %>% 
  filter(type %in% c("Export")) %>%
  filter(product == "sugar")

copy_labels(comex, subdata_export_sugar)


subdata_export_sugar$date <- as.Date(subdata_export_sugar$date)

library(lubridate)
subdata_export_sugar_2017_2019 <- mutate(subdata_export_sugar, year = year(date))
subdata_export_sugar_2017_2019 <- subdata_export_sugar_2017_2019[!(subdata_export_sugar_2017_2019$year <= 2016),]

aggregate_export_sugar_contry <- aggregate(subdata_export_sugar_2017_2019$tons, list(country = subdata_export_sugar_2017_2019$country), sum)
aggregate_export_sugar_contry <- aggregate_export_sugar_contry[!(aggregate_export_sugar_contry$x <= 1000000),]

install.packages("ggplot2")
library(ggplot2)

ggplot(aggregate_export_sugar_contry, aes(x = country, y = x)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "parceiros de exportação de açucar",
       x = "País",
       y = "exportações")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





#### For each of the products in the dataset, show the 5 most important states in terms of exports?

#wheat
subdata_wheat <- comex %>% 
  filter(type %in% c("Export")) %>%
  filter(product =="wheat")


copy_labels(comex, subdata_wheat)

ggplot(subdata_wheat) + geom_bar(aes(y= state)) + 
  theme_classic()

#corn
subdata_corn <- comex %>% 
  filter(type %in% c("Export")) %>%
  filter(product =="corn")


copy_labels(comex, subdata_corn)

ggplot(subdata_corn) + geom_bar(aes(y= state)) + 
  theme_classic()


#soybeans
subdata_soybeans <- comex %>% 
  filter(type %in% c("Export")) %>%
  filter(product =="soybeans")


copy_labels(comex, subdata_soybeans)

ggplot(subdata_soybeans) + geom_bar(aes(y= state)) + 
  theme_classic()




#soybean_meal
subdata_soybean_meal <- comex %>% 
  filter(type %in% c("Export")) %>%
  filter(product =="soybean_meal")


copy_labels(comex, subdata_soybean_meal)

ggplot(subdata_soybean_meal) + geom_bar(aes(y= state)) + 
  theme_classic()



#soybean_oil
subdata_soybeansoil <- comex %>% 
  filter(type %in% c("Export")) %>%
  filter(product =="soybean_oil")


copy_labels(comex, subdata_soybeansoil)


ggplot(subdata_soybeansoil) + geom_bar(aes(y= state)) + 
  theme_classic()



#sugar
subdata_sugar <- comex %>% 
  filter(type %in% c("Export")) %>%
  filter(product =="sugar")


copy_labels(comex, subdata_sugar)


ggplot(subdata_sugar) + geom_bar(aes(y= state)) + 
  theme_classic()




#### Question: What should be the total brazilian soybeans, soybean_meal, and corn export forecasts, in tons, for the next 11 years (2020-2030)? We're mostly interested in the annual forecast.


