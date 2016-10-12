library(quantmod)

notFound <- c()
getStock <- function (exchange, time.range) {
  require(TTR)
  tickersList <- TTR::stockSymbols()
  tickersList <- tickersList[,c("Symbol", "Name", "MarketCap", "Exchange")]
  colnames(tickersList) = c("symbol", "name", "marketCap", "exchange")
  write.csv(tickersList, file="symbols.csv")
  
  symbols <- tickersList$symbol[(tickersList$exchange == exchange)]
  if (!file.exists(exchange)) dir.create(exchange)
  for(symbol in symbols) {
    tryCatch({
      symbolDataName <- getSymbols(symbol)
      data <- get(symbolDataName)
      data <- as.data.frame(data[time.range])
      colnames(data) <- c("open", "high" , "low", "close", "volume", "adjusted")
      data <- cbind(data.frame(date=rownames(data)), data)
      file.name <- paste(symbol, "csv", sep=".")
      write.csv(data, file=paste(exchange, file.name, sep="/"), row.names=FALSE)
      cat(exchange, "-", symbol, "complete", "\n")
    }, error=function(e) {
      print(e)
      notFound <<- c(notFound, symbol)
    }, warning=function(e) {
      print(e)
      notFound <<- c(notFound, symbol)
    })
  }
  write.csv(notFound, file="notFound.csv")
}

getStock("NYSE", "2015::")
getStock("NASDAQ", "2015::")