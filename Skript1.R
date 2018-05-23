files <- list.files(".", "*.csv")
Tabelle <- list()
daten <- c()
eye <- c()
for (i in 1:length(files)) {
  Tab <- read.csv2(files[i], skip = 3, nrows = 2)
  daten <- c(daten, as.Date(Tab[1, 1], "%d.%m.%Y"))
  Tab <- read.csv2(files[i], skip = 8, nrows = 2)
  test <- Tab
  eye <- c(eye, as.character(Tab[1, 2]))
  Tab <- read.csv2(files[i], skip = 14, nrows = 71)
  Tabelle[i] <- list(Tab[11:71, ])
}

all_data <- c()
for (i in 1:61) {
  liste <- c()
  for (j in 1:4) {
    liste <- c(liste, Tabelle[[j]][i, 7])
  }
  model <- lm(liste ~ daten)
  tab <- data.frame(x = daten, 
                    y = liste, 
                    slope = coef(model)[[2]],
                    pvalue = pf(summary(model)$fstatistic[1], summary(model)$fstatistic[2],
                                 summary(model)$fstatistic[3], lower.tail = FALSE),
                    seg = i,
                    eye = eye)
  all_data <- rbind(all_data, tab)
}

for (i in 1:51) {
  test <- lm(y ~ x, data = subset(all_data, seg == i))
  if(coef(summary(test))[2, 4] < 0.05) print(coef(test))
}

liste <- c()
liste <- c(liste, as.double(gsub(",","",Tabelle1[31, 6])))
liste <- c(liste, as.double(gsub(",","",Tabelle2[31, 6])))
liste <- c(liste, as.double(gsub(",","",Tabelle3[31, 6])))
liste <- c(liste, as.double(gsub(",","",Tabelle4[31, 6])))

liste2 <- c()
liste2 <- c(liste2, as.double(gsub(",","",Tabelle1[32, 6])))
liste2 <- c(liste2, as.double(gsub(",","",Tabelle2[32, 6])))
liste2 <- c(liste2, as.double(gsub(",","",Tabelle3[32, 6])))
liste2 <- c(liste2, as.double(gsub(",","",Tabelle4[32, 6])))
