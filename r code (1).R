Variable --> Raw Input --> Adjusted Input
lnassets --> $4,200,000,000 --> 4200 --> 8.34283980427146
prepack --> no --> 0
preneg --> no --> 0
lnsales --> $4,196,600,000 --> 22.157540510584869
xyearfiled --> 2001 --> 2001
shop --> shop (aka yes) --> 1

library(survey)
data <- read.table("evaluationdata.tab", sep = "\t", header = T)
design31 <- svydesign(id = ~casenumber, weights = ~pweight, data = data)
model31 <- svyglm(lnfeeexpord ~ lnassets + prepack + preneg + lnsales + xyearfiled + shop, design = design31)
tempdata <- as.data.frame(t(c(8.34283980427146, 0, 0, 22.157540510584869, 2001, 1)))
colnames(tempdata) <- c("lnassets", "prepack", "preneg", "lnsales", "xyearfiled", "shop")
newdata1 <- with(data, tempdata)
pred <- predict(model31, newdata = newdata1, se.fit = T)
efit <- pred[1]
lfit <- efit - 1.44 * sqrt(attr(pred, "var")) / .97
hfit <- efit + 1.44 * sqrt(attr(pred, "var")) / .97
list(efit = efit, hfit = hfit, lfit = lfit)

actual (R):
lfit --> 17.78565 --> $52,991,919
efit --> 17.93218 --> $61,354,556
hfit --> 18.07872 --> $71,037,609

actual (Stata):
lfit --> 17.78468 --> $52,940,542
efit --> 17.9324  --> $61,368,056
hfit --> 18.08011 --> $71,136,420

expected (website):
lfit --> $20,418,487
efit --> $61,354,822
hfit --> $184,363,035

Variable --> Raw Input --> Adjusted Input
lnroles ---> 15 ---> 2.70805020110221
lnassets ---> $4,200,000,000 ---> 4200 ---> 8.34283980427146
lndaysin ---> 737 ---> 6.602587892189337
yearconfirmed ---> 2003 ---> 2003
lnemployees ---> 14700 ---> 9.595602772766828
shop ---> shop (aka yes) ---> 1
saleall ---> yes ---> 1

library(survey)
data <- read.table("evaluationdata.tab", sep = "\t", header = T)
design34 <- svydesign(id = ~casenumber, weights = ~pweight, data = data)
model34 <- svyglm(lnfeeexpord ~ lnroles + lnassets + lndaysin + yearconfirmed + lnemployees + shop + saleall, design = design34)
tempdata <- as.data.frame(t(c(2.70805020110221, 8.34283980427146, 6.602587892189337, 2003, 9.595602772766828, 1, 1)))
colnames(tempdata) <- c("lnroles", "lnassets", "lndaysin", "yearconfirmed", "lnemployees", "shop", "saleall")
newdata1 <- with(data, tempdata)
pred <- predict(model34, newdata = newdata1, se.fit = T)
efit <- pred[1]
lfit <- efit - 1.44 * sqrt(attr(pred, "var")) / .97
hfit <- efit + 1.44 * sqrt(attr(pred, "var")) / .97
list(efit = efit, hfit = hfit, lfit = lfit)

actual (R):
lfit --> 17.44751 --> $37,788,296
efit --> 17.59955 --> $43,993,392
hfit --> 17.75159 --> $51,217,406

expected (website):
lfit --> $20,365,868
efit --> $43,993,467
hfit --> $95,032,785



library(survey)
#library(lmtest)
#library(sandwich)

setwd ("G:/dropbox/Dropbox/LopuckiBookFinalData/survivalcalculator-master/do files (Stata)/do files (Stata)")
data <- read.table("eval and pred data.tab", sep="\t", header = T)

#Table 3.1 Model

design31 <- svydesign(id=~casenumber, weights = ~pweight, data=data)
model31 <-svyglm(lnfeeexpord ~ lnassets + prepack + preneg + lnsales + xyearfiled + shop, design=design31 )

#Fake data for out-of-sample testing

#tempdata <- as.data.frame(t(c(8, 0, 0, 20, 2000, 1)))
tempdata <- as.data.frame(t(c(22.15, 0, 0, 22.15, 2001, 1)))
#colnames(tempdata) <- c("lnassets", "prepack", "preneg", "lnsales", "xyearfiled", "shop")
colnames(tempdata) <- c("lnassets", "prepack", "preneg", "lnsales", "xyearfiled", "shop")

#pred[1] is the predicted point estimate.  Getting the SE out of the object is a bit messy.  1.44 is the multiplier for
#85pct CI.  Note that I've calculated upper and lower bounds of CI as a vector, although this may not be totally necessary.

newdata1 <-with(data,tempdata)
pred<-predict(model31, newdata = newdata1, se.fit=T)
efit <- pred[1]
lfit <- efit - 1.44*sqrt(attr(pred, "var"))/.97
hfit <- efit + 1.44*sqrt(attr(pred, "var"))/.97
list(efit=efit, hfit=hfit, lfit=lfit)

#coin85 <-c(pred[1] - 1.44*sqrt(attr(pred, "var"))/.97, pred[1]+1.44*sqrt(attr(pred, "var"))/.97)
#coin95 <-c(pred[1] - 1.96*sqrt(attr(pred, "var"))/.97, pred[1]+1.96*sqrt(attr(pred, "var"))/.97)

#Table 3.4 Model
design34 <- svydesign(id=~casenumber, weights = ~pweight, data=data)
model34 <-svyglm(lnfeeexpord ~ lnroles + lnassets + lndaysin + yearconfirmed + lnemployees + shop + saleall, design=design)

#Fake data for out-of-sample testing.

#tempdata <- as.data.frame(t(c(2, 7, 6, 2000, 8, 1, 1)))
#colnames(tempdata) <- c("lnroles", "lnassets", "lndaysin", "yearconfirmed", "lnemployees", "shop", "saleall")

newdata1 <-with(data,tempdata)
pred<-predict(model34, newdata = newdata1, se.fit=T)
fit <-pred[1]
lfit <-fit - 1.44*sqrt(attr(pred, "var"))/.97
hfit <-fit + 1.44*sqrt(attr(pred, "var"))/.97
list(efit=efit, hfit=hfit, lfit=lfit)

#coin85 <-c(pred[1] - 1.44*sqrt(attr(pred, "var"))/.97, pred[1]+1.44*sqrt(attr(pred, "var"))/.97)
#coin95 <-c(pred[1] - 1.96*sqrt(attr(pred, "var"))/.97, pred[1]+1.96*sqrt(attr(pred, "var"))/.97)

#Code to test out robust std errors in R.  Ignore since it does not work correctly with pweights.

#testmodel <-lm(lnfeeexpord ~ lnassets + prepack + preneg + lnsales + xyearfiled + shop, data = data)
#robuststuff <- coeftest(testmodel, vcov = vcovHC(testmodel, type="HC1"))
               