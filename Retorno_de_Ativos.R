## Dados da B3

library(quantmod)
library(fBasics)
library(ggplot2)
library(scales)
library(BatchGetSymbols)
library(GetTDData)
library(dplyr)
library(magrittr)
library(tidyr)
library(tibble)

first.date = as.Date('2008-01-01')
last.date = as.Date('2020-02-22')
my.assets = c('^BVSP','PETR4.SA', 'VALE3.SA', 'ITUB4.SA')
my.l = BatchGetSymbols(tickers = my.assets,
                       first.date = first.date,
                       last.date = last.date)

ggplot(my.l$df.tickers, aes(x = ref.date, y = price.close))+
  geom_line()+facet_wrap(~ticker, scales = 'free_y')

bvsp = my.l$df.tickers |>
  filter(ticker == "^BVSP") |>
  select(ref.date,
         "close_bvsp" = price.close)

itub4 = my.l$df.tickers |>
  filter(ticker == "ITUB4.SA") |>
  select(ref.date,
         "close_itub" = price.close)

data = inner_join(bvsp, itub4, by = "ref.date")

ggplot(data, aes(x=close_bvsp, y=close_itub))+
  geom_point()


## Dados do Tesouro Direto

install.packages("GetTDData")
library(GetTDData)
ls("package:GetTDData")
help(package = "GetTDData")
get_td_names()
td_get('NTN-B')

# Primeiro, vamos atribuir o resultado de td_get('NTN-B') a uma variável

dados_ntnb <- td_get('NTN-B')

# Agora, vamos filtrar para obter apenas os dados da NTN-B 150545

ntnb45 <- dados_ntnb |>
  dplyr::filter(asset_code == "NTN-B 150545")

# Visualizar as primeiras linhas do resultado

head(ntnb45)

library(ggplot2)
library(scales)
ls("package:ggplot2")

ggplot(ntnb45, aes(x = ref_date, y = yield_bid*100)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Evolução do Yield da NTN-B 150545",
       x = "Data",
       y = "Yield (%)")

## Retornos simples diários da 3M, de janeiro de 2001 a setembro de 2011

url = 'https://faculty.chicagobooth.edu/-/media/faculty/ruey-s-tsay/teaching/introts/ch1data.zip'
download.file(url, destfile = 'ch1data.zip')
unzip('ch1data.zip', list=TRUE)


mmm = unzip('ch1data.zip', files='d-mmm-0111.txt')
mmm = read.table(mmm, header=T)
tail(mmm)

## Histograma

hist(mmm[,2], nclass=30, main='Histograma dos retornos simples da 3M',
     xlab='')

## Densidade

# Obter a densidade

d1=density(mmm[,2]) 

# Intervalo dos retornos da 3M

range(mmm[,2]) 

# Criar uma sequência que incrementa em 0.001.

x=seq(-.1,.1,.001) 
y1=dnorm(x,mean(mmm[,2]),stdev(mmm[,2]))
plot(d1$x,d1$y,xlab='rtn',ylab='density',type='l')
lines(x,y1,lty=2)


## Bar Chart da Apple

getSymbols("AAPL",from="2017-06-05", source='yahoo')

# Locate open, high, low, and close prices

X=AAPL[,1:4] 

xx=cbind(as.numeric(X[,1]),as.numeric(X[,2]),as.numeric(X[,3]),as.numeric(X[,4]))

source('https://faculty.chicagobooth.edu/-/media/faculty/ruey-s-tsay/teaching/introts/ohlc.r')

ohlc_plot(xx,xl="days",yl="price",title="Apple Stock")

getSymbols("AAPL", source='yahoo')

x1=as.numeric(AAPL$AAPL.Close)

source('https://faculty.chicagobooth.edu/-/media/faculty/ruey-s-tsay/teaching/introts/ma.r')

ma(x1,21)

# Usando o arquivo temp

data = unzip('ch1data.zip', files='m-ibmsp-2611.txt')
data = read.table('m-ibmsp-2611.txt', header=T)
tail(data)

# Transforma em log retornos

ibm=log(data$ibm+1) 
sp=log(data$sp+1)

# Criar indexador de tempo

tdx=c(1:nrow(data))/12+1926 
par(mfcol=c(1,2))
plot(tdx,ibm,xlab='year',ylab='lrtn',type='l')
title(main='(a) IBM returns')

# X-axis first.

plot(tdx,sp,xlab='year',ylab='lrtn',type='l') 
title(main='(b) SP index')

# Correlação amostral

cor(ibm,sp) 

# Obter a reta de regressão

m1=lm(ibm~sp) 

# Scatter plot

plot(sp,ibm,cex=0.8) 

# Add the linear regression line

abline(m1, col='red')

## Retorno de Ativos

getSymbols('AAPL', src='yahoo', auto.assign = T)

tail(AAPL[,4])

AAPL.rtn = diff(log(AAPL$AAPL.Close))

plot(AAPL.rtn)

## Volatilidade Implícita

getSymbols('VIXCLS', src='FRED')

vix = VIXCLS[complete.cases(VIXCLS),]
vix = window(vix, start="2016-01-01")
plot(vix)
