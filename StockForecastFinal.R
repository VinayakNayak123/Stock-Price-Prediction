#Load the libraries here
library(TTR)
library(tseries)
library(forecast)
library(dplyr)




setwd('')
getwd()


initialdf<-read.csv('HackathonRound1.csv',header=TRUE,stringsAsFactors = FALSE)
initialdf <- initialdf[!(initialdf$Share.Names=="Share18" & initialdf$Date < "2015-07-08"),] 
updatedset<-read.csv('DataUpdate_Hackathon.csv',header=TRUE,stringsAsFactors = FALSE)
stockdf<-bind_rows(initialdf,updatedset)

stockdf$Date <- as.Date(stockdf$Date,
                        format = "%d-%b-%y")

Share_list=c("Share1",	"Share2",	"Share3",	"Share4",	"Share5",	"Share6",	"Share7",	"Share8",	
             "Share9",	"Share10",	"Share11",	"Share12",	"Share13",		"Share14","Share15",	
             "Share16",	"Share17",	"Share18",	"Share19",	"Share20",	"Share21",	"Share22",	
             "Share23",	"Share24",	"Share25",	"Share26",	"Share27",	"Share28",	"Share29",	
             "Share30",	"Share31",	"Share32",	"Share33",	"Share34",	"Share35",	"Share36",	
             "Share37",	"Share38",	"Share39",	"Share40",	"Share41",	"Share42",	"Share43",	
             "Share44",	"Share45",	"Share46",	"Share47",	"Share48",	"Share49",	"Share50"
)



getforecastclosePrice=function(Sharedf){
  Share1df_sorted <- Sharedf[with(Sharedf, order(Sharedf$Share.Names,Sharedf$Date)),] 
  Share1df_sorted$cnt_ma = ma(Share1df_sorted$Close.Price, order=7) 
  count_ma = ts(na.omit(Share1df_sorted$cnt_ma), frequency=7)
  decomp = stl(count_ma, s.window="periodic")
  deseasonal_cnt <- seasadj(decomp)
  
  
  #Evaluate and Iterate
  set.seed(100)
  #fit<-auto.arima(deseasonal_cnt,stepwise=TRUE, approximation=FALSE,lambda = 0,trace = TRUE)
  fit<-auto.arima(deseasonal_cnt, d=NA, D=NA, max.p=5, max.q=5,
             max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
             start.p=2, start.q=2, start.P=1, start.Q=1,
             stationary=FALSE, seasonal=TRUE,
             ic=c("aicc", "aic", "bic"), stepwise=TRUE, trace=FALSE,
             truncate=NULL, xreg=NULL,
             test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
             allowdrift=TRUE, allowmean=TRUE, lambda=NULL, biasadj=FALSE,
             parallel=FALSE, num.cores=2)
  
  fcast <- forecast(fit, h=4)
  upperlist<-fcast$mean
  Share_Price_close1<-upperlist[3]
  Share_Price_close2<-upperlist[4]
  return (list(Share_Price_close1,Share_Price_close2)) 
}




getforecastOpenPrice=function(Sharedf){
  Share1df_sorted <- Sharedf[with(Sharedf, order(Sharedf$Share.Names,Sharedf$Date)),] 
  Share1df_sorted$cnt_ma = ma(Share1df_sorted$Open.Price, order=7) 
  count_ma = ts(na.omit(Share1df_sorted$cnt_ma), frequency=7)
  decomp = stl(count_ma, s.window="periodic")
  deseasonal_cnt <- seasadj(decomp)
  
  #Evaluate and Iterate
  set.seed(100)
  #fit<-auto.arima(deseasonal_cnt,stepwise=TRUE, approximation=FALSE,lambda = 0,trace = TRUE)
  fit<-auto.arima(deseasonal_cnt, d=NA, D=NA, max.p=5, max.q=5,
                  max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                  start.p=2, start.q=2, start.P=1, start.Q=1,
                  stationary=FALSE, seasonal=TRUE,
                  ic=c("aicc", "aic", "bic"), stepwise=TRUE, trace=FALSE,
                  truncate=NULL, xreg=NULL,
                  test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
                  allowdrift=TRUE, allowmean=TRUE, lambda=NULL, biasadj=FALSE,
                  parallel=FALSE, num.cores=2)
  fcast <- forecast(fit, h=4)
  upperlist<-fcast$mean
  Share_Price_open1<-upperlist[3]
  Share_Price_open2<-upperlist[4]
  return (list(Share_Price_open1,Share_Price_open2)) 
}



#For close price
Close_price_df <- data.frame(x = numeric(), stringsAsFactors = FALSE)
for (stkname in Share_list){
  Sharedf<-stockdf[stockdf$Share.Names==stkname,]
  Share_close_price=getforecastclosePrice(Sharedf)
  print(stkname)
  print(Share_close_price[1])
  print(Share_close_price[2])
  if(stkname=="Share36"){
    Share_close_price[1]<-0
    Share_close_price[2]<-0
  }
  df_temp<-rbind(Close_price_df,Share_close_price[1])
  df_temp2<-rbind(df_temp,Share_close_price[2])
  Close_price_df<-df_temp2
}


#For open price
Open_price_df <- data.frame(x = numeric(), stringsAsFactors = FALSE)
for (stkname in Share_list){
  Sharedf<-stockdf[stockdf$Share.Names==stkname,]
  Share_close_price=getforecastOpenPrice(Sharedf)
  print(stkname)
  print(Share_close_price[1])
  print(Share_close_price[2])
  if(stkname=="Share36"){
    Share_close_price[1]<-0
    Share_close_price[2]<-0
  }
  df_temp<-rbind(Open_price_df,Share_close_price[1])
  df_temp2<-rbind(df_temp,Share_close_price[2])
  Open_price_df<-df_temp2
}

Close_price_df
Open_price_df


# Giving headers to our dataframe
colheading_opn<-c("OpenPriceDF")
names(Open_price_df)<-colheading_opn

colheading_close<-c("ClosePriceDF")
names(Close_price_df)<-colheading_close

submitdf<-read.csv('hackathon_Sample_Submission.csv')
head(submitdf)
submitdf$Open.<-Open_price_df$OpenPriceDF
submitdf$Close<-Close_price_df$ClosePriceDF
submitdf
write.csv(submitdf,file ="SampleHACK.csv",row.names = FALSE)


