require('zoo')
require('xts')
require('e1071')
require('randomForest')
require('IDPmisc')


go <- function(){
    
    #Yearly
    c13 <- read.csv("cansim-0800013.csv", skip=4, nrows=5, na.strings = c("NA", "x", "F"))
    colnames(c13) <- c("Date", "all.13", "sb.13")
    c13$Date <- as.Date(as.yearmon(paste(c13$Date, "1"), "%Y %m"))

    #Quarterly
    c22 <- read.csv("cansim-0800022.csv", skip=4, nrows=43, na.strings = c("NA", "x", "F"))
    c22 <- c22[-1,]
    colnames(c22) <- c("Date", "all.22", "sb.22")
    c22$Date <- quarter.to.date(c22$Date)

    #Monthly
    c09 <- read.csv("cansim-0800009.csv", skip=4, nrows=214, na.strings = c("NA", "x", "F"))
    c09 <- c09[-1,] #Remove Row 1
    colnames(c09) <- c("Date", "all.09", "sb.09")
    c09$Date<-as.Date(as.yearmon(c09$Date, format="%b-%Y"))

    #Weekly
    trend <- read.csv("report.csv", skip=4, nrow=570)
    colnames(trend)[1] <- "Date"
    trend$Date <- week.to.date(trend$Date)

    #Convert to XTS
    trend.xts <- as.xts(trend[,-1], order.by = trend$Date)
    c09.xts <-   as.xts(c09[,-1], order.by = c09$Date)
    c22.xts <-   as.xts(c22[,-1], order.by = c22$Date)
    c13.xts <-   as.xts(c13[,-1], order.by = c13$Date)
    
    #But merging these does not work well
    t9.xts <-merge(trend.xts, c09.xts)

    #Convert the trends to weekly, works much better
    mon.trend.xts <- to.monthly(trend.xts, indexAt='yearmon', droptime=TRUE, OHLC=FALSE)
    mt9.xts <- merge(mon.trend.xts, c09.xts)


    #Only Jan2004 to Dec2007 well populated, build the model for these years
    smt9.xts <- mt9.xts["2003-12/2007-11"]
    
    #Predict the sales for 2008 based on 2004-2007
    # "2007-12/2008-11" because the selector seems to be off by one
    future.xts <- mt9.xts["2007-12/2008-11"][,1:(length(names(mt9.xts))-1)]

    par(mfrow=c(row=1,col=3))
    model.and.plot('sb.09', smt9.xts, future.xts, "lm", NULL)
    #model.and.plot('sb.09', smt9.xts, future.xts, "svm", "gamma=0.1")
    #model.and.plot('sb.09', smt9.xts, future.xts, "randomForest", "ntree=50")

 }

model.and.plot <- function(independent, series, future, modeller, param){
    
    eval(parse(text=paste0("model<-", modeller, '(', independent, "~., series, ", param, ')')))
    fitted <- predict(model,series)
    fitted <- as.xts(fitted, order.by=index(series))

    future.independent <- predict(model, future)
    future.independent <- as.xts(future.independent, order.by = index(future))
    fitted.and.future <- c(fitted, future.independent)
    plot(fitted.and.future, main=paste0(modeller, " ", param), ylab=independent, ylim=c(0, max(fitted)))
    eval(parse(text=paste0("points(series$",independent,", type='p', pch=16)")))
    summary(model)
}



quarter.to.date <- function(Quarters){
    DatesList <- sapply(Quarters, toString)
    DatesDF <- data.frame(t(data.frame(strsplit(DatesList, " "))))
    DatesDF$Month <- (as.numeric(data.frame(t(
        data.frame(strsplit(toString(DatesDF[,1]), "Q"))))[,2])-1)*3+1
    Dates <- as.Date(as.yearmon(paste(DatesDF[,2],DatesDF[,3]), "%Y %m"))
    Dates
}


week.to.date <- function(week){
    
     as.Date(data.frame(t(data.frame(strsplit(sapply(week, toString), " "))))[,1], "%Y-%m-%d")
}
