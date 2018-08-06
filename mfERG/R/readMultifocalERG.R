
readMultifocalERG <- function(pathname)
{
  Tabelle <- list()

    Tab <- read.csv2(pathname, skip = 3, nrows = 2)
    Tabelle$date <- as.Date(Tab[1, 1], "%d.%m.%Y")
    Tab <- read.csv2(pathname, skip = 8, nrows = 2)
    Tabelle$eye <- as.character(Tab[1, 2])
    Tab <- read.csv2(pathname,
                     skip = 14,
                     nrows = 72,
                     header = F)
    Tabelle$values <- Tab[-1, 1:10]
    tabNames <- unlist(Tab[1, -11])
    tabNames <- gsub("\\[.*\\]", "", tabNames)
    tabNames <- gsub("[\\.| ]", "", tabNames)
    tabNames[1] <- "Fields"
    names(Tabelle$values) <- tabNames
    
    Curves <- read.csv2(pathname,
                        skip = 176,
                        header = T)
    Tabelle$curves <- Curves[, -63]
    
  return(Tabelle)
}