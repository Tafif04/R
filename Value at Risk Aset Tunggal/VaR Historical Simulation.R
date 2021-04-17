#Import Stock Price from Yahoo Finance
#Close Price
library(quantmod)
saham = function(code, from, to){
  price = getSymbols(code,auto.assign=FALSE,from=from,to=to)
  close.price = Cl(price)
  return(close.price)
}

#Example TLKM.JK (Telkom Indonesia (Persero) Tbk)
TLKM = saham(code = 'TLKM.JK', from = "2021-01-31", to = "2021-03-27")
TLKM = as.data.frame(TLKM)


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
R.TLKM = Return(TLKM$TLKM.JK.Close)
R.TLKM

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

HSVaR(R.TLKM, hp = 1, modal = 1000000, alpha = 0.05)
