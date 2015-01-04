require('zoo')
require('xts')
require('e1071')
require('randomForest')
require('IDPmisc')


go <- function(){
    
    ##Yearly
    c13 <- read.csv("cansim-0800013.csv", skip=4, nrows=5, na.strings = c("NA", "x", "F"))
    colnames(c13) <- c("Date", "all.13", "sb.13")
    c13$Date <- as.Date(as.yearmon(paste(c13$Date, "1"), "%Y %m"))

    ##Quarterly
    c22 <- read.csv("cansim-0800022.csv", skip=4, nrows=43, na.strings = c("NA", "x", "F"))
    c22 <- c22[-1,]
    colnames(c22) <- c("Date", "all.22", "sb.22")
    c22$Date <- quarter.to.date(c22$Date)

    ##Monthly
    c09 <- read.csv("cansim-0800009.csv", skip=4, nrows=214, na.strings = c("NA", "x", "F"))
    c09 <- c09[-1,] #Remove Row 1
    colnames(c09) <- c("Date", "all.09", "sb.09")
    c09$Date<-as.Date(as.yearmon(c09$Date, format="%b-%Y"))

    ##Weekly
    latest.report <- choose.newest.file("~/Downloads", "report*.csv")

    trend <- read.csv(latest.report, skip=4, nrow=570)
    colnames(trend)[1] <- "Date"
    trend$Date <- week.to.date(trend$Date)

    ##Convert to XTS
    trend.xts <- as.xts(trend[,-1], order.by = trend$Date)
    c09.xts <-   as.xts(c09[,-1], order.by = c09$Date)
    c22.xts <-   as.xts(c22[,-1], order.by = c22$Date)
    c13.xts <-   as.xts(c13[,-1], order.by = c13$Date)
    
    ##But merging these directly does not work well
    t9.xts <-merge(trend.xts, c09.xts)

    ##Convert the trends to weekly and then merge, works much better
    mon.trend.xts <- to.monthly(trend.xts, indexAt='yearmon', droptime=TRUE, OHLC=FALSE)
    mt9.xts <- merge(mon.trend.xts, c09.xts)
    #We're going to use only c09 and trends for our models.  

    ##Only Jan2004 to Dec2007 are well populated in c09, so build the model for these years
    ## The xts selector seems to be off by one
    smt9.xts <- mt9.xts["2003-12/2007-11"]
    
    ##Predict the sales for 2008 based on sales for 2004-2007
    future.xts <- mt9.xts["2007-12/2008-11"][,1:(length(names(mt9.xts))-1)]

    ##Predict the sales for 2008 based on sales for 2004-2007
    par(mfrow=c(row=1,col=3)) ## plot three side-by-side
    model.and.plot('sb.09', smt9.xts, future.xts, "lm", NULL) #linear regression
    model.and.plot('sb.09', smt9.xts, future.xts, "svm", "gamma=0.1") #suport vector machine
    model.and.plot('sb.09', smt9.xts, future.xts, "randomForest", "ntree=50") 

 }

model.and.plot <- function(dependent, series, future, modeller, param){
    #Model the dependent variable in the time series, and predict its value in the future 
    eval(parse(text=paste0("observed <- series$", dependent)))
    eval(parse(text=paste0("model<-", modeller, '(', dependent, "~., series, ", param, ')')))
    fitted <- predict(model,series)
    fitted <- as.xts(fitted, order.by=index(series))

    future.dependent <- predict(model, future)
    future.dependent <- as.xts(future.dependent, order.by = index(future))
    max.future.dependent <- future.dependent[which.max(future.dependent)]
    fitted.and.future <- c(fitted, future.dependent)

    rsq <- 1 - sum((observed-fitted)^2) / sum((observed-mean(observed))^2)
    label <- c(paste0(modeller, " ", param),
               paste0("Max Future ", prettyNum(big.mark=",", round(digits=0,max.future.dependent))),
               paste0("R.Sq = ", round(digits=2,rsq)))
    plot(fitted.and.future, main=label, ylab=dependent, ylim=c(0, max(observed)))
    points(observed,, type='p', pch=16)
    points(max.future.dependent, type='p', pch=5)
}


#Convert quarters Q1, Q2, ... to months 01, 04, 07, ...
quarter.to.date <- function(Quarters){
    DatesList <- sapply(Quarters, toString)
    DatesDF <- data.frame(t(data.frame(strsplit(DatesList, " "))))
    DatesDF$Month <- (as.numeric(data.frame(t(
        data.frame(strsplit(toString(DatesDF[,1]), "Q"))))[,2])-1)*3+1
    Dates <- as.Date(as.yearmon(paste(DatesDF[,2],DatesDF[,3]), "%Y %m"))
    Dates
}

##Convert "2004-02-15 - 2004-02-21" to "2004-02-15"
week.to.date <- function(week){
     as.Date(data.frame(t(data.frame(strsplit(sapply(week, toString), " "))))[,1], "%Y-%m-%d")
}

##
choose.newest.file <- function(download.directory, pattern){
    names <- list.files(download.directory,  glob2rx(pattern), full.names=TRUE)
    if(length(names)==1)
        names
    else 
       names[which.max(file.info(names)$mtime)]
}
