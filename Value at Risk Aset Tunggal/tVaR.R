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

tVaR = function(r,hp,modal,alpha){
  M = mean(r)
  V = var(r)
  Vol = sqrt(V)
  Hp = hp
  Mod = modal
  v = length(r)-1
  t = qt(alpha,v)
  VaR = -(t)*sqrt(Hp)*sqrt((v-2)/v)*Vol*Mod
  tvar = (VaR/Mod)*100
  cat('------------Perhitungan VaR Pendekatan Distribusi t----------- \n')
  cat('Means              :',M,'\n')
  cat('Variance           :',V,'\n')
  cat('Volatilitas        :',Vol,'\n')
  cat('Holding Periode    :',Hp,'Hari','\n')
  cat('Modal              :',Mod,'\n')
  cat('v                  :',v,'\n')
  cat('Alpha              :', alpha,'\n')
  cat('t                  :',t,'\n')
  cat('Value at Risk      :',VaR,'\n')
  cat('Value at Risk (%)  :',tvar,'%','\n')
  cat('----------------------------------------------------------------')
}

tVaR(R.TLKM, hp = 1, modal = 1000000, alpha = 0.05)
