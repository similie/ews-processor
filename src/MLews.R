##############################################
# Developed and manteined by Decatab Pte Ltd #
# for addtional informaton please contact    #
# info@decatab.com                           #
##############################################


library(DBI)
library(RPostgreSQL)
library(xgboost)
library(data.table)
library(caret)
library(missRanger)
library(lubridate)
library(padr)
library(zoo)
#load the enviroment
load("/src/Docker.RData")

#CONNECT TO DB
pg<-dbDriver("PostgreSQL")
onedb <-dbConnect(pg, dbname = Sys.getenv("Dbname"), 
                host =Sys.getenv("Host"),
                port = Sys.getenv("Port"), 
                user = Sys.getenv("User"),
                password = Sys.getenv("Password"))

FinalPrediction<-"noRisk"

repeat {
  
#GET THE DATA FROM THE DATABASE
sensorData<-dbGetQuery(onedb, 'SELECT
	s.local_name,
	A.DATE,
	A.temperature,
	A.humidity,
	A.soil_moisture,
	A.precipitation 
FROM
	"assets"."all_weather"
	A JOIN "public"."station" s ON ( A.station = s.ID ) 
WHERE
	A.station IN ( 5, 2, 1 ) 
	AND A."date" > CURRENT_DATE - INTERVAL "3 months" 
ORDER BY
	A."station",
	A."date”;')

#Calculate Mean by day
sensorData<-lapply(split(sensorData,sensorData$local_name),function(x){
  setDT(x)
  #x$Date<-as.POSIXct(x$date, orders="%d/%m/%y %H:%M:%S")
  x$Date<-as_date(parse_date_time(gsub('.{3}$', '', x$date), orders = "%d/%m/%y %H:%M:%S"))
  x$Date<-as_date(x$Date)
  x$date<-NULL
  x$local_name<-NULL
  setorder(x,Date)
  x<-x[,lapply(.SD,mean,na.rm=T),by=c("Date")]

})

#Setting colnames
setnames(sensorData$`Ainaro Vila`,2:dim(sensorData$`Ainaro Vila`)[2],paste0(names(sensorData$`Ainaro Vila`)[2:dim(sensorData$`Ainaro Vila`)[2]],"_","S1"))
setnames(sensorData$Aitutu,2:dim(sensorData$Aitutu)[2],paste0(names(sensorData$Aitutu)[2:dim(sensorData$Aitutu)[2]],"_","S2"))
setnames(sensorData$`Cassa Villa`,2:dim(sensorData$`Cassa Villa`)[2],paste0(names(sensorData$`Cassa Villa`)[2:dim(sensorData$`Cassa Villa`)[2]],"_","S3"))



#MERGE THE THREE STATIONS
sensorData<-Reduce((function() {
  function(x, y) {
    d = merge(x, y, all = T, by = 'Date')
  }})(), sensorData)
setorder(sensorData,"Date")

#Merge with the satelite data
sensorData<-merge(sensorData,Docker_preds, all = T, by = 'Date')
#Keep the last 3 months
sensorData<-sensorData[Date%between% c(Sys.Date()-89 , Sys.Date())]
sensorData = padr::pad(sensorData)
#Missing imputation
sensorData_full<- missRanger(sensorData[,-c("Date"),with=F], pmm.k = 30,splitrule = "extratrees", num.trees = 100,seed = 123)
sensorData_full$Date<-sensorData$Date


#MEAN OF ALL THE STASTIONS
allRain<-paste0("precipitation_S",1:3)
allTemp<-paste0("temperature_S",1:3)
allHum<-paste0("humidity_S",1:3)
allMoist<-paste0("soil_moisture_S",1:3)
sensorData_full[, AvgRain := rowMeans(.SD), .SDcols = allRain] 
sensorData_full[, AvgTemp := rowMeans(.SD), .SDcols = allTemp]
sensorData_full[, AvgHum := rowMeans(.SD), .SDcols = allHum]
sensorData_full[, AvgMoist := rowMeans(.SD), .SDcols = allMoist]
sensorData_full<-sensorData_full[,-c(allRain,allTemp,allHum,allMoist),with=F]
names(sensorData_full)
#Defining Rolls
SumCols<-c("AvgRain")
MeanCols<-c("AvgTemp","AvgHum","AvgMoist","surfMoist" ,"subMoist")
#Rollsum
sensorData_full[ , paste0("last1D",SumCols) := lapply(.SD, function(x) rollsumr(x,k = 1, fill = NA)), .SDcols = SumCols]
sensorData_full[ , paste0("last3D",SumCols) := lapply(.SD, function(x) rollsumr(x,k = 3, fill = NA)), .SDcols = SumCols]
sensorData_full[ , paste0("last1W",SumCols) := lapply(.SD, function(x) rollsumr(x,k = 7, fill = NA)), .SDcols = SumCols]
sensorData_full[ , paste0("last1M",SumCols) := lapply(.SD, function(x) rollsumr(x,k = 30, fill = NA)), .SDcols = SumCols]
sensorData_full[ , paste0("last3M",SumCols) := lapply(.SD, function(x) rollsumr(x,k = 90, fill = NA)), .SDcols = SumCols]

#Rollmean
sensorData_full[ , paste0("last1D",MeanCols) := lapply(.SD, function(x) rollmeanr(x,k = 1, fill = NA)), .SDcols = MeanCols]
sensorData_full[ , paste0("last3D",MeanCols) := lapply(.SD, function(x) rollmeanr(x,k = 3, fill = NA)), .SDcols = MeanCols]
sensorData_full[ , paste0("last1W",MeanCols) := lapply(.SD, function(x) rollmeanr(x,k = 7, fill = NA)), .SDcols = MeanCols]
sensorData_full[ , paste0("last1M",MeanCols) := lapply(.SD, function(x) rollmeanr(x,k = 30, fill = NA)), .SDcols = MeanCols]
sensorData_full[ , paste0("last3M",MeanCols) := lapply(.SD, function(x) rollmeanr(x,k = 90, fill = NA)), .SDcols = MeanCols]
sensorData_full$Month<-month(sensorData_full$Date)
sensorData_full$Date<-NULL

names(sensorData_full)
anyNA(sensorData_full)
sensorData_full<-na.omit(sensorData_full)
dim(sensorData_full)
setcolorder(sensorData_full,"Month")
names(sensorData_full)
#final data
currentData<-sensorData_full
names(currentData)
setcolorder(currentData,colOrder)                

setDF(currentData)
#apply the transformation
csData<<- predict(prepocessObj, currentData[,-c(1)])
#add Month
currentData<<-cbind(csData,"Month"=currentData$Month)
# predict and return result
testPredGBXProb <<-predict(xgb_model, currentData[,predictors], type = "prob")
FinalPrediction<<- factor( ifelse(testPredGBXProb[,"Risk"] > threshold,  "Risk", "noRisk") )
finalResult<-data.frame("timeStamp"=Sys.time(),"probEvent"=round(testPredGBXProb[,"Risk"],3),"Label"=FinalPrediction)

print(finalResult)

#SAVE THE RESULTS IN THE TABLE
dbWriteTable(onedb,'output_table',finalResult, row.names=FALSE,append=TRUE)

  if (FinalPrediction =="Risk"){
    Sys.sleep(3600)}#one hour
  else{Sys.sleep(3600*6)}#six hours
}
