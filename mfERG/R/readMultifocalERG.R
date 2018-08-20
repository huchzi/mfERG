
readMultifocalERG <- function(pathname)
{

  con <- file(pathname, "r")
  resultFile <- readLines(con, encoding = "latin1")
  close(con)
  
  resultFile <- sapply(resultFile, function(x) sub(";$", ";;", x))
  resultFile[1] <- sub(";$", "", resultFile[1])
  results <- strsplit(resultFile, ";")
  
  header = list()
  firstCol <- sapply(results, function(x) x[1])
  
  for(i in c("Patient", "Examination", "Stimulation", "Derivation", "Analysis")) {
    
    rowSearch <- which(firstCol == i)
    
    headerAdd <- c(results[[rowSearch]], recursive = T)
    headerNames <- c(results[[rowSearch - 1]], recursive = T)
    
    headerAdd <- as.list(setNames(headerAdd, headerNames))
    
    header <- c(header, headerAdd)
    
    
  }
  
  extractNumeric <- function(x) {
    as.numeric(gsub(",", ".", x))
  }
  
  header$`Curve Length [s]` <- extractNumeric(header$`Curve Length [s]`)
  header$`Sample Freq [Hz]` <- extractNumeric(header$`Sample Freq [Hz]`)
  
  frames <- round(header$`Curve Length [s]` * header$`Sample Freq [Hz]`)
  
  secondCol <- sapply(results, function(x) x[2])
  
  rowSearch <- which(secondCol == "Area [deg^2]")
  
  summaryTab <- do.call("rbind.data.frame", results[rowSearch + 1:71])
  
  names(summaryTab) <-results[[rowSearch]]
  
  rownames(summaryTab) <- as.character(summaryTab[, 1])
  
  summaryTab <- summaryTab[, -1]
  
  for (i in 1:9)
    summaryTab[, i] <- as.numeric(gsub(",", ".", as.character(summaryTab[, i])))
  
  rowSearch <- grep("^Sum Curves", firstCol)
  
  sumCurves <- do.call("rbind.data.frame", results[rowSearch + 1 + 1:frames])
  
  names(sumCurves) <-results[[rowSearch + 1]]
  
  rownames(sumCurves) <- as.character(sumCurves[, 1])
  
  sumCurves <- sumCurves[, -c(1, 12)]
  
  for (i in 1:10)
    sumCurves[, i] <- as.numeric(gsub(",", ".", as.character(sumCurves[, i])))
  
  rowSearch <- grep("^Segment Curves", firstCol)
  
  segCurves <- do.call("rbind.data.frame", results[rowSearch + 1 + 1:frames])
  
  names(segCurves) <-results[[rowSearch + 1]]
  
  rownames(segCurves) <- as.character(segCurves[, 1])
  
  segCurves <- segCurves[, -c(1, 63)]
  
  for (i in 1:61)
    segCurves[, i] <- as.numeric(gsub(",", ".", as.character(segCurves[, i])))
  
  mfERG <- list()
  mfERG$header <- header
  mfERG$frames <- frames
  mfERG$frameDuration <- header$`Curve Length [s]` / frames
  mfERG$summary <- summaryTab
  mfERG$sumCurves <- sumCurves
  mfERG$segments <- segCurves
    
  class(mfERG) <- c(class(mfERG), "mfERG")
  
  return(mfERG)
}

plot.mfERG <- function(mfERG) {
  
  test <- qplot(x = 1:mfERG$frames * mfERG$frameDuration, 
                mfERG$sumCurves$Sum, geom = "line")
  
  test <- test + 
    geom_vline(aes(xintercept = mfERG$summary["Sum", "Peak Time P1 [s]"]))
  
  print(test)
  
}