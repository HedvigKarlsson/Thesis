set.seed(1)

#Här läses datamaterialet in.
data <- read.csv2("/home/hedka083/Documents/C-uppsats/databas.csv")

#Läser in data förutom rad 1 till 61 samt variablerna 8:81.
data <- data[62:nrow(data), 8:81]
colnames(data)[ncol(data)] <- "Kon"
#Tar bort variablerna: Q1, Q2, Q4, Q5.
data <- subset(data, select = -c(Q1, Q1_Open, 
                                   Q2_01_Open, 
                                   Q2_02_Open, 
                                   Q2_03_Open,
                                   Q4_Open,
                                   Q5_Open))

#Tar bort variablerna Q13-Q18, 21, 25(Kön).
data <- subset(data, select = -c(Q131,Q132,Q133, Q134, Q135, Q136, Q137, Q138, Q139, Q13_09_Open,
                                   Q141, Q142, Q143, Q144, Q145, Q146, Q147, Q148, Q149, Q1410, Q1411,
                                   Q1412, Q14_12_Open,
                                   Q15,
                                   Q161, Q162, Q163, Q164, Q165, Q166, Q167, 
                                   Q17, Q18, Q21, Q25,Kon))


#Här väljs variablerna:Q3, Q6 till Q9 samt Q11 & Q12 ut.
df_ordinal <- subset(data, select = c(Q3_1,Q6_1,Q7_1,
                               Q8_1,
                               Q8_2,
                               Q8_3,
                               Q8_4,
                               Q8_5,
                               Q8_6,
                               Q8_7,
                               Q8_8,
                               Q8_9,
                               Q8_10,
                               Q8_11,
                               Q8_12,
                               Q8_13,
                               Q8_14,
                               Q9_1,
                               Q11,
                               Q12))


#Sätt alla 99999997 samt "8" i variabel Q11 och Q12 till missade NA.
na_999 <- function(x) {
  missing <- which(x == 99999997 | x == 8)
  x[missing] <- NA
  x
}
df_na <- data.frame(sapply(df_ordinal, na_999))

#Funktionen för imputeringen. Simple random imputation mha fördelning enligt gelman artikeln.
random.imp <- function(a) {
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample(a.obs, n.missing, replace=TRUE)
  imputed
}



# datasetet imputeras mha simple random imputation
df_ordinalimp <- data.frame(sapply(df_na, random.imp))

#Berarbeta dem ordinala till ordinala i r f
df_ordinalimp <- data.frame(lapply(df_ordinalimp, factor, ordered = TRUE))

#Q11 och Q12 har omvända skalor från negativ till positiv
df_ordinalimp$Q11 <- factor(df_ordinalimp$Q11, 
                            levels = rev(levels(df_ordinalimp$Q11)))
df_ordinalimp$Q12 <- factor(df_ordinalimp$Q12, 
                            levels = rev(levels(df_ordinalimp$Q12)))                           

#Bearbetar dem nominala frågorna i r, gör om till faktorer.
df_nominala <- subset(data, select = c(Q20,
                                       Q22,
                                       Q23,
                                       Q24,
                                       Q26,
                                       Q27))
df_nominala <- data.frame(lapply(df_nominala, factor))


missing_alder <- which(data$Q19 == 1)
data$Q19[missing_alder] <- NA
data$Q19 <- random.imp(data$Q19)


data$Q19 <- cut(data$Q19, 
                      breaks=c(1, 8, 28, 48, 68, 74))


data_imputerat <- cbind(df_ordinalimp, "Q19" = data$Q19, df_nominala)

#sparar datasetet till en fil.
write.csv(data_imputerat, "slumpimputerat.csv")
