#Import Stock Price from Yahoo Finance
#Close Price
library(quantmod)
saham = function(code, from, to){
  price = getSymbols(code,auto.assign=FALSE,from=from,to=to)
  close.price = Cl(price)
  return(close.price)
}

#Example: TLKM.JK, BBRI.JK, ICBP.JK
TLKM = saham(code = 'TLKM.JK', from = "2021-01-01", to = "2021-03-11")
BBRI = saham(code = 'BBRI.JK', from = "2021-01-01", to = "2021-03-11")
ICBP = saham(code = 'ICBP.JK', from = "2021-01-01", to = "2021-03-11")
Portofolio = data.frame(TLKM,BBRI,ICBP)
View(Portofolio)

#Return Geometric
Return = function (x)
{
  n = length(x)
  hasil = vector(length=(n-1))
  for (i in 2:n){
    hasil[i-1]= log(x[i]/x[i-1])
  }
  return(round(hasil,4))
}

#Return Aset Tunggal
R.TLKM = Return(Portofolio$TLKM.JK.Close)
R.BBRI = Return(Portofolio$BBRI.JK.Close)
R.ICBP = Return(Portofolio$ICBP.JK.Close)
R.Asset = data.frame(R.TLKM,R.BBRI,R.ICBP)
View(R.Asset)

#Pembobot
Weight = function(x){
  varcov = cov(x)
  invarcov = solve(varcov)
  identitas = matrix(1,nrow(varcov),1)
  tin = t(identitas)
  a = invarcov%*%identitas
  b = tin%*%a
  c = 1/b
  weight = a%*%c
  return(weight)
}

Pembobot = Weight(R.Asset) 
Pembobot

#Return Portofolio
R.Portofolio = (R.TLKM*Pembobot[1])+(R.BBRI*Pembobot[2])+(R.ICBP*Pembobot[3])
R.Portofolio

#HSVaR
HSVaR = function(r,hp,modal,alpha){
  sort = sort(r)
  Alpha = alpha
  Per = Alpha*(length(r)+1)
  i = round(Per,0)
  a = i-Per
  if (a < 0){
    Vol = sort[i]-(a*(sort[i+1]-sort[i]))
  } else {
    Vol = sort[i]-(a*(sort[i]-sort[i-1]))
  }
  Hp = hp
  Mod = modal
  VaR = Vol*Mod*sqrt(Hp)
  hsvar = abs((VaR/Mod)*100)
  cat('------------Perhitungan VaR Menggunakan Historical Simulation----------- \n')
  cat('Alpha               :', alpha,'\n')
  cat('Persentil ke-alpha  :', Per,'\n')
  cat('Nilai Persentil     :',Vol,'\n')
  cat('Holding Periode     :',Hp,'Hari','\n')
  cat('Modal               :',Mod,'\n')
  cat('Value at Risk       :',VaR,'\n')
  cat('Value at Risk (%)   :',hsvar,'%','\n')
  cat('------------------------------------------------------------------------')
}

HSVaR(R.Portofolio, hp = 1, modal = 1000000, alpha = 0.05)