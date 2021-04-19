#Data
##Perlakuan
perlakuan1 = rep('A',3)
perlakuan2 = rep('B',3)
perlakuan3 = rep('C',3)
perlakuan4 = rep('D',3)
Perlakuan = c(perlakuan1, perlakuan2, perlakuan3, perlakuan4)
Perlakuan = factor(Perlakuan)
##Kelompok
Kelompok = rep(1:3,4)
Kelompok = factor(Kelompok)
##Respon
Respon = c(9.3, 9.4, 9.6,
           9.4, 9.3, 9.8,
           9.2, 9.4, 9.5,
           9.7, 9.6, 10.0
          )

Data = data.frame(Perlakuan, Kelompok, Respon)
View(Data)

#Model
RAKL = aov(Respon~Perlakuan+Kelompok, data = Data)
summary(RAKL)

#Uji Lanjut
##1. Duncan Test
library(agricolae)
###Terhadap Perlakuan
duncan.test(RAKL, 'Perlakuan', alpha = 0.05, console = TRUE)
###Terhadap Kelompok
duncan.test(RAKL, 'Kelompok', alpha = 0.05, console = TRUE)
##2. HSD Test
TukeyHSD(RAKL)

#Checking Assumption
Resid = residuals(object = RAKL)
##Normality Test
plot(RAKL,2)
shapiro.test(Resid)
##Homogenity Test
plot(RAKL,1)
library(car)
###Perlakuan
leveneTest(Respon~Perlakuan, data = Data)
###Kelompok
leveneTest(Respon~Kelompok, data = Data)

