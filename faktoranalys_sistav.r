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


polycorr <- inspect(fit, "sampstat")$cov #De polykoriska korrelationerna, r i dwls?
inv_weightmat <-inspect(fit, "wls.v") #Den tillhörande asmyptotiska kovariansmatrisen, inversa viktmatrisen#w
implied_mat <- inspect(fit, "implied")$cov #model implicerade matrix , p_hat bestående av en vektor?
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
#Eftersom Frågorna hur ofta du med SL samt hur ofta åker du med bil ej är jämnt avsåtnd och således inte polynomiala kontraster, osäker denna
#anta dem nominala, osäker..?
contrasts(df_logistisk$oftaSL, ) <- contr.treatment
contrasts(df_logistisk$oftaBil) <- contr.treatment

#Ej ha med alla upp itl ^9? kontraster. är det rätt, ja dem är ortogonalt oberoende..?
contrasts(df_logistisk$rekommendera,4) <- contr.poly

logit <- polr(fortroende ~., data=df_logistisk, Hess = TRUE)
mlnom <- multinom(fortroende ~., data=df_logistisk)
logitnoll <- polr(fortroende ~1, data=df_logistisk, Hess = TRUE)

#Proportioenlla odds assumption. med likehliodo ratio test mot mul enligt veneblaes and riples kap 7.3 måst hämta källa
M1 <- logLik(logit)
M2 <- logLik(mlnom)
(G <- -2*(M1[1] - M2[1]))
pchisq(G,3,lower.tail = FALSE)
#samma värde som


#Eller köra brant test. källa hosmer.
poTest(logit)
#Samtliga höga chi2 o låga p värden? Alltså ej uppfylls.lr?

coeftest(logit)
coefs <- coeftest(logit)[,]
logLik(logit)



#Dock ger det att intelliganse faktor ej heller blir signifkkant., därför behålls den i modellen?

#modell med endast konstant för att få 0 modellen?
# logit_noll <- polr(fortroende ~1, data=df_logistisk, Hess = TRUE)
# summary(logit_noll)
# # 13430.27 största modell.
# coeftest(logit_noll)
# logLik(logit_noll)

#Ett hypotestest H0 uttrycks som: ”Den reducerade modellen är
#statistiskt bättre.” Om p litet <0.05? Förkastas H_0. Modell bör ej reduceras?

#Likelihood ratio test
anova(logitnoll, logit)
library("lmtest")
lrtest(logit, logitnoll)
# pchisq(46.612, 30, lower.tail = FALSE)


# library(usdm)
# vif(df_logistisk)
