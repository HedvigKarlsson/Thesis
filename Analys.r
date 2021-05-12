library(lavaan)
source("imputering.R")#Här hämtas data från en annan R-fil.

#Endast faktor analys körs på variablerna Q8_1 till Q9 (Varumarke)
df_varumarke <- data_imputerat[,4:18]

#Här skapas en faktoranalysmodell
model1 <- ' funktionalitet  =~ tryggt + bekvamt + lita + anvandning
            omtanke =~ bemotande + lyhordhet + engagemang
            ansvar =~ miljo + skattemedel + mangfald
            intelligens =~ samhallutveckling + problemlosande + utveckling
            enkelhet =~ enkel + smidighet'
fit<-cfa(model = model1,
         data = df_varumarke, 
         ordered = c("tryggt","bekvamt","lita", "anvandning",
                     "bemotande","lyhordhet", "engagemang",
                     "miljo", "skattemedel", "mangfald",
                     "samhallutveckling", "problemlosande", "utveckling",
                     "enkel", "smidighet"),
         estimator="DWLS",test="Satorra.Bentler")

#Standard Xhi2_DWLS?. 806.231 = 
#Robust Chi2 = 1929.390
#Scaling correction factor = 0.418 = 806.231/1929.390

summary(fit, fit.measures=TRUE, standardized = TRUE)
#If fit.measures = TRUE, a second section is
#printed containing the test statistic of the baseline model (where all observed variables are

#Här skapas "scores" utifrån faktoranalysmodellen, som sedan läggs in som variabel i kommande logistiska regressionsmodell.
scores_pred <- lavPredict(fit)
#rite.csv(scores, "scores.csv")
scores <- read.csv("scores.csv")[,-1]


#The weight matrix (the 'W' in WLS) is the inverse of the asymptotic
#variance matrix of the sample statistics (thresholds, polychorics). See
#muthen (1984) for the theory.

#WLSM only affects the test, using the mean-adjustment only (aka Satorra
#Bentler)
# 
# 1) estimator="DWLS"
# 2) se="robust"
# 3) test="satorra.bentler"


polycorr <- inspect(fit, "sampstat")$cov #De polykoriska korrelationerna
inv_weightmat <-inspect(fit, "wls.v") #Den tillhörande asmyptotiska kovariansmatrisen.
implied_mat <- inspect(fit, "implied")$cov #model implicerade matrix.
corrlatent <- inspect(fit, "cor.lv") #The model-implied correlation matrix of the latent variables.
inspect(fit,what="std")$lambda #laddningarna
lavInspect(fit, "cor.lv") #Korrelationerna mellan faktorerna, The model-implied correlation matrix of the latent variables.


library("semPlot")
semPaths(fit, "std", thresholds = F, intercepts = F, fade=FALSE,
         sizeLat = 7,
         sizeLat2 = 5,
         curvePivot = F,
         edge.label.cex =1,
         fixedStyle = TRUE,
         layout = "tree"
         )



###Ordinal logistisk regression
library(MASS)
library(lmtest)
df_logistisk <- cbind(data_imputerat[,-c(4:18)], scores)
#Anger default kontraster
#options(contrasts = c("contr.treatment", "contr.poly"))

df_logistisk$fortroende
poTest(logit)
#Eftersom Frågorna hur ofta du med SL samt hur ofta åker du med bil ej är jämnt avstånd och således inte polynomiala kontraster.
contrasts(df_logistisk$oftaSL, ) <- contr.treatment
contrasts(df_logistisk$oftaBil) <- contr.treatment

#Ej ha med alla upp itl ^9? kontraster.
contrasts(df_logistisk$rekommendera,4) <- contr.poly

logit <- polr(fortroende ~., data=df_logistisk, Hess = TRUE)
mlnom <- multinom(fortroende ~., data=df_logistisk)
logitnoll <- polr(fortroende ~1, data=df_logistisk, Hess = TRUE)

#Proportioenlla odds assumption.
M1 <- logLik(logit)
M2 <- logLik(mlnom)
(G <- -2*(M1[1] - M2[1]))
pchisq(G,3,lower.tail = FALSE)


# brant test.
poTest(logit)

coeftest(logit)
coefs <- coeftest(logit)[,]
logLik(logit)

#Likelihood ratio test
anova(logitnoll, logit)
library("lmtest")
lrtest(logit, logitnoll)
# pchisq(46.612, 30, lower.tail = FALSE)


# library(usdm)
# vif(df_logistisk)
