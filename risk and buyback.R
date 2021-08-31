########program to analyse risk with buyback programs ####################
##############################################################
#---------------importing repurchase data##########
#------------import from file -------------
library(readxl)
buybacks_2010 <- read_excel("C:/Users/foued/OneDrive/risk and buyback/buyback on Newyork Exchange/buybacks 2010.xlsx")
buybacks_2011 <- read_excel("C:/Users/Foued/OneDrive/risk and buyback/buyback on Newyork Exchange/buybacks 2011.xlsx")
buybacks_2012 <- read_excel("C:/Users/Foued/OneDrive/risk and buyback/buyback on Newyork Exchange/buybacks 2012.xlsx")
buybacks_2013 <- read_excel("C:/Users/Foued/OneDrive/risk and buyback/buyback on Newyork Exchange/buybacks 2013.xlsx")
buybacks_2014 <- read_excel("C:/Users/Foued/OneDrive/risk and buyback/buyback on Newyork Exchange/buybacks 2014.xlsx")
buybacks_2015 <- read_excel("C:/Users/Foued/OneDrive/risk and buyback/buyback on Newyork Exchange/buybacks 2015.xlsx")
buybacks_2016 <- read_excel("C:/Users/Foued/OneDrive/risk and buyback/buyback on Newyork Exchange/buybacks 2016.xlsx")
buybacks_2017 <- read_excel("C:/Users/Foued/OneDrive/risk and buyback/buyback on Newyork Exchange/buybacks 2017.xlsx")
buybacks_2018 <- read_excel("C:/Users/Foued/OneDrive/risk and buyback/buyback on Newyork Exchange/buybacks 2018.xlsx")
buybacks_2019 <- read_excel("C:/Users/Foued/OneDrive/risk and buyback/buyback on Newyork Exchange/buybacks 2019.xlsx")

colnames(buybacks_2010)=c("firms","rachat")
colnames(buybacks_2011)=c("firms","rachat")
colnames(buybacks_2012)=c("firms","rachat")
colnames(buybacks_2013)=c("firms","rachat")
colnames(buybacks_2014)=c("firms","rachat")
colnames(buybacks_2015)=c("firms","rachat")
colnames(buybacks_2016)=c("firms","rachat")
colnames(buybacks_2017)=c("firms","rachat")
colnames(buybacks_2018)=c("firms","rachat")
colnames(buybacks_2019)=c("firms","rachat")


#made as dataframe 
#----------------------------------
buybacks_2010=as.data.frame(buybacks_2010)
buybacks_2012=as.data.frame(buybacks_2012)
buybacks_2013=as.data.frame(buybacks_2013)
buybacks_2014=as.data.frame(buybacks_2014)
buybacks_2015=as.data.frame(buybacks_2015)
buybacks_2016=as.data.frame(buybacks_2016)
buybacks_2017=as.data.frame(buybacks_2017)
buybacks_2018=as.data.frame(buybacks_2018)
buybacks_2019=as.data.frame(buybacks_2019)
####subset 2010 with repurchase 
##------------------------------
buybacks_2010$year=2010
buybacks_2011$year=2011
buybacks_2012$year=2012
buybacks_2013$year=2013
buybacks_2014$year=2014
buybacks_2015$year=2015
buybacks_2016$year=2016
buybacks_2017$year=2017
buybacks_2018$year=2018
buybacks_2019$year=2019
t=do.call("rbind", list(buybacks_2010
                        ,buybacks_2011,buybacks_2013,
                        buybacks_2014,buybacks_2015,
                        buybacks_2016,buybacks_2017,
                        buybacks_2018,buybacks_2019))

wrep=subset(t,t$rachat>0)
wnrep=subset(t,t$rachat==0)
###prepare quotesymbol#################
##"__________----------------
library(stringr)
library(readxl)
setwd("C:/Users/foha2/OneDrive/risk and buyback")
#*****must set working directory before***********************
quotesymbols=read_excel("quotesymbols.xlsx")
quotesymbols$quote=str_sub(quotesymbols$Quote,1,-3)
#------import  from quantmod############### 
##-------------------------------
library(quantmod)
start_date <- Sys.Date()-3650
end_date <- Sys.Date()
quotes=quotesymbols$quote
quotes=list(quotes)
quotes=na.omit(quotes)
######import from yahoo METHOD 1--------
for (i in quotes){
  quote[i]=getSymbols(i,src = "yahoo", from=start_date,to=end_date, auto.assign = TRUE)
}
library(zoo)
library(xts)
#################import data METHOD 2#################

envt <- new.env()

getSymbols(quotes,env=envt,from=start_date, to=end_date)
ETF_Adj_Data <- do.call(merge, eapply(envt, Ad))


##############PREPARE DATA ##############
quotesymbols$Quote=NULL
datawrep=merge(wrep,quotesymbols)
datawnrep=merge(wnrep,quotesymbols)

###return calculation########--------
########################
library(zoo)
library(xts)
Monthly_Adj_Data <- ETF_Adj_Data[endpoints(ETF_Adj_Data,'months')]
monthlyreturn=diff(log(Monthly_Adj_Data))
##d####elete NA  value and columns 
monthlyreturn=monthlyreturn[-1,]
monthlyreturn[, -which(sapply(monthlyreturn, function(x) sum(is.na(x)))==nrow(monthlyreturn))]
monthlyreturn <-na.fill(monthlyreturn, fill = 0.00)

###############descriptive statistics ####################
Treturn=apply(monthlyreturn,2,mean) # mean return by firms
Treturnrow<-apply(monthlyreturn,1,mean) #mean return by date

#######################export to excel
data=as.data.frame(monthlyreturn)
library(writexl)
write_xlsx(data,"donnees.xlsx")  ###convertir en excel
write.zoo(monthlyreturn,file="export.csv",
          index.name="okay",row.names=FALSE,
          col.names=TRUE,sep=",")##  convertir en cvs
#----------------trasform data as dataframe----------------
firmR<-as.data.frame(Treturn)
dateR<-as.data.frame(Treturnrow)


#******sytemic risk********************************
#*****************prepare dataset**********************************
library(SystemicR)
library(zoo)
library(xts)
risk<-data.frame(date=index(monthlyreturn), coredata(monthlyreturn))
risk$date<- format(risk$date,"%d/%m/%Y")
library(dplyr)
risk<-rename(risk,Date=date)
risk$Date<-as.factor(risk$Date)
#**********analyse risks*************************4
f_CoVaR_Delta_CoVaR_i_q(risk)








