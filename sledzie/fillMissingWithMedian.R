# ---- fillMissing ----

fillMissingWithMedian <- function(data, dfToProcess, attributeName, attributesToFilter){
  #inicjalizacja pustego filtra
  emptyFilter <- rep(FALSE, nrow(data))
  
  
  #iteracja rekord po rekordzie
  for(i in 1:nrow(dfToProcess)){
    row <- dfToProcess[i,]
    filter <- emptyFilter
    #nakładanie kolejnych filtrów do znalezienia podobnej grupy
    for(j in 1:length(attributesToFilter)){
      attr <- attributesToFilter[j]
      filter <- filter | data[attr] == row[attr][1,1]
    }
    similarGroups <- data[filter, ]
    #przypisanie mediany
    dfToProcess[i,][attributeName] <- median(as.numeric(unlist(similarGroups[attributeName])), na.rm = TRUE)
  }
  return(dfToProcess)
}

fillMissingSST <- function(data, dfToProcess){
  for(i in 1:nrow(dfToProcess)){
    row <- dfToProcess[i,]
    similarGroups <- data[
        data$totaln == row$totaln &
        data$sal == row$sal &
        data$xmonth == row$xmonth &
        data$nao == row$nao, ]
    #przypisanie mediany
    dfToProcess[i,]$sst <- median(as.numeric(unlist(similarGroups$sst)), na.rm = TRUE)
  }
  return(dfToProcess)
}