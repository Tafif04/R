#Data
##Perlakuan
perlakuan1 = rep('A',8)
perlakuan2 = rep('B',8)
perlakuan3 = rep('C',8)
perlakuan4 = rep('D',8)
Perlakuan = c(perlakuan1, perlakuan2, perlakuan3, perlakuan4)
Perlakuan = factor(Perlakuan)
##Respon
Respon = c(230, 225, 223, 216, 229, 201, 205, 193,
           187, 223, 214, 192, 222, 208, 198, 217,
           177, 201, 178, 170, 207, 182, 178, 195,
           129, 139, 160, 149, 122, 146, 166, 169
          )

Data = data.frame(Perlakuan, Respon)
View(Data)

#Model Anova
RAL = aov(Respon~Perlakuan, data = Data)
summary(RAL)

#Uji Lanjut
##1. Duncan Test
library(agricolae)
duncan.test(RAL, 'Perlakuan', alpha = 0.05, console = TRUE)
##2. HSD Test
TukeyHSD(RAL)

#Checking Assumption
Resid = residuals(object = RAL)
##Normality Test
plot(RAL,2)
shapiro.test(Resid)
##Homogenity Test
plot(RAL,1)
library(car)
leveneTest(Respon~Perlakuan, data = Data)
