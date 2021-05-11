#Import Library
library(readxl)
library(agricolae)
library(car)

#Data
Data = read_xlsx('D:/Portofolio/Faktorial RAL.xlsx', sheet = 'Sheet2')
View(Data)
str(Data)
Data$Temperature = as.factor(Data$Temperature)
Data$Waktu = as.factor(Data$Waktu)
str(Data)

#Anova
#fac.RAL=aov(Umur~Temperature*Waktu, data = Data)
fac.RAL = aov(Umur~Temperature+Waktu+Temperature:Waktu, data = Data)
summary(fac.RAL)

#Uji Lanjut
## HSD Test
TukeyHSD(fac.RAL)

#Checking Assumption
Resid = residuals(object = fac.RAL)
Data.Resid = data.frame(Data, Resid)
View(Data.Resid)

##Normality Test
plot(fac.RAL,2)
shapiro.test(Resid)

##Homogenity Test
plot(fac.RAL,1)
###Temperature
bartlett.test(Resid~Temperature, data = Data)
###Waktu
bartlett.test(Resid~Waktu, data = Data)
###TemperatureWaktu
TemperatureWaktu = interaction(Data$Temperature,Data$Waktu)
bartlett.test(Resid~TemperatureWaktu, data = Data)
