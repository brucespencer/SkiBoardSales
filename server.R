require(shiny)
require(lubridate)
source("predict.sales.R")

shinyServer(function(input, output) {

    latest.report <- choose.newest.file("~/Downloads", "report*.csv")

    output$salesPlot <- renderPlot({
        
       #Monthly
       c09 <- read.csv("cansim-0800009.csv", skip=4, nrows=214, na.strings = c("NA", "x", "F"))
       c09 <- c09[-1,] #Remove Row 1
       colnames(c09) <- c("Date", "all.09", "sb.09")
       c09$Date<-as.Date(as.yearmon(c09$Date, format="%b-%Y"))
       
       #Weekly
       trend <- read.csv(latest.report, skip=4, nrow=570)
       colnames(trend)[1] <- "Date"
       trend$Date <- week.to.date(trend$Date)
       
       #Convert to XTS
       trend.xts <- as.xts(trend[,-1], order.by = trend$Date)
       c09.xts <-   as.xts(c09[,-1], order.by = c09$Date)
       #Convert the trends to weekly, works much better
       mon.trend.xts <- to.monthly(trend.xts, indexAt='yearmon', droptime=TRUE, OHLC=FALSE)
       mt9.xts <- merge(mon.trend.xts, c09.xts)
       
       #Only Jan2004 to Dec2007 well populated in Cansim, build the model for these years
       smt9.xts <- mt9.xts["2003-12/2007-11"]
       
       #Predict the sales for 2008 based on 2004-2007
       select.string <- paste0("2007-12/",format(as.Date(as.yearmon("2007-12"))+
                           months(input$npredictions), format="%Y-%m"))
       future.xts <- mt9.xts[select.string][,1:(length(names(mt9.xts))-1)]
       
       #par(mfrow=c(row=1,col=3))
       par(mfrow=c(row=1,col=2))
       model.and.plot('sb.09', smt9.xts, future.xts, "lm", NULL)
       #model.and.plot('sb.09', smt9.xts, future.xts, "svm", "gamma=0.1")
       model.and.plot('sb.09', smt9.xts, future.xts, "randomForest", "ntree=50")
    })

    searchterms <- read.csv(latest.report, skip=4, nrow=1)

    output$SearchTerms  <-  renderText({paste("Search Terms: ", paste(collapse=" ",  names(searchterms[-1])))})    

})                                                                                                                    



