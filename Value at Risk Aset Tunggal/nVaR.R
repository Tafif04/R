#Import Stock Price from Yahoo Finance
#Close Price
library(quantmod)
saham = function(code, from, to){
  price = getSymbols(code,auto.assign=FALSE,from=from,to=to)
  close.price = Cl(price)
  return(close.price)
}

#Example TLKM.JK (Telkom Indonesia (Persero) Tbk)
TLKM = saham(code = 'TLKM.JK', from = "2020-01-31", to = "2020-02-27")
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

#nVar
nVaR = function(r,hp,modal,alpha){
  M = mean(r)
  V = var(r)
  Vol = sqrt(V)
  Hp = hp
  Mod = modal
  Z = qnorm(alpha)
  VaR = -(Z)*sqrt(Hp)*Vol*Mod
  nvar = (VaR/Mod)*100
  Saham = saham
  cat('------------Perhitungan VaR Pendekatan Distribusi Normal----------- \n')
  cat('Means             :',M,'\n')
  cat('Variance          :',V,'\n')
  cat('Volatilitas       :',Vol,'\n')
  cat('Holding Periode   :',Hp,'Hari','\n')
  cat('Modal             :',Mod,'\n')
  cat('Alpha             :',alpha,'\n')
  cat('Value at Risk     :',VaR,'\n')
  cat('Value at Risk (%) :',nvar,'%','\n')
  cat('------------------------------------------------------------------- \n')
}

nVaR(R.TLKM, hp = 1, modal = 1000000, alpha = 0.05)
