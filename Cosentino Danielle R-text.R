#Final Project
lyv_orig = read.csv("LYV_orig.csv")
lyv_orig = lyv_orig[-1,]
lyv_orig$Date= as.Date(lyv_orig$Date, format = "%m/%d/%y")

#1b
    #returns
plot(lyv_orig$Date, lyv_orig$Close, main="Closing Daily Prices of LYV, Danielle Cosentino", xlab ="Years", ylab = "Closing Daily Prices", cex=.15)
plot(lyv_orig$Date, lyv_orig$LYVret, main="Daily Returns of LYV, Danielle Cosentino", xlab = "Years", ylab = "Daily Returns", cex=.15)
hist(lyv_orig$Close, breaks=100, main="Histogram of Closing Prices of LYV, Danielle Cosentino")
hist(lyv_orig$LYVret, breaks=100, main="Histogram of Closing Prices of LYV, Danielle Cosentino")

    #risk
plot(lyv_orig$Date, lyv_orig$LYVrisk, main="Range of LYV, Danielle Cosentino", xlab ="Years", ylab = "Ranges", cex=.15)
plot(lyv_orig$Date, lyv_orig$LnLYVrisk, main="Log of Range of LYV, Danielle Cosentino",xlab= "Years", ylab = "ln(Range)", cex=.15)
hist(lyv_orig$LYVrisk, breaks=100, main="Histogram of range of LYV, Danielle Cosentino")
hist(lyv_orig$LnLYVrisk, breaks=100, main="Histogram of log of range of LYV, Danielle Cosentino")

#1d
ret_regr = read.csv("LYVret_Regr.csv")
risk_regr = read.csv("LYVrisk_Regr.csv")

pairs(ret_regr[2:4], cex=.15, main="Matrix Plot of Lagged Daily Returns of LYV, Danielle Cosentino")
pairs(risk_regr[2:4], cex=.15, main="Matrix Plot of Lagged Risk of LYV, Danielle Cosentino")

#2a - Smoothing spline
library(splines)
    #Daily returns data
plot(lyv_orig$Date, lyv_orig$LYVret, cex=.5, main="Smoothing Spline of Daily Returns, Danielle Cosentino", xlab="Years", ylab="Daily Returns" )
fit1= smooth.spline(lyv_orig$Date, lyv_orig$LYVret,cv=TRUE)
lines(fit1, col="blue",lwd=2)

    #Risk data
plot(lyv_orig$Date, lyv_orig$LnLYVrisk, cex=.5, main="Smoothing Spline of Risk, Danielle Cosentino", xlab="Years", ylab="ln(Range)")
fit2= smooth.spline(lyv_orig$Date, lyv_orig$LnLYVrisk, cv=TRUE)
lines(fit2, col="blue",lwd=2)

#2b - taking a sample of 100 from a continuous time period
lyv_orig2010 = subset(lyv_orig, Date>=as.Date('2010-01-01') & Date<=as.Date('2010-12-31'))
dim(lyv_orig2010)
    #Plot 2010 daily return data
plot(lyv_orig2010$Date, lyv_orig2010$LYVret, cex=.5, main="Smoothing Spline of Daily Returns in 2010, Danielle Cosentino", xlab="Months", ylab="Daily Returns" )
fit3= smooth.spline(lyv_orig2010$Date, lyv_orig2010$LYVret,cv=TRUE)
lines(fit3, col="blue",lwd=2)

    #Plot 2010 risk data
plot(lyv_orig2010$Date, lyv_orig2010$LYVrisk, cex=.5, main="Smoothing Spline of Risk in 2010, Danielle Cosentino", xlab="Months", ylab="ln(Range)" )
fit4= smooth.spline(lyv_orig2010$Date, lyv_orig2010$LYVrisk,cv=TRUE)
lines(fit4, col="blue",lwd=2)

#2c - experimenting with paramter df
#creating an additional subset of data from 2020-present
tail(lyv_orig)
lyv_orig2020 = subset(lyv_orig, Date>=as.Date('2020-01-01') & Date<=as.Date('2023-04-20'))

#Full data
    #Returns
plot(lyv_orig$Date, lyv_orig$LYVret, cex=.5, main="Smoothing Spline of Daily Returns, Danielle Cosentino", xlab="Years", ylab="Daily Returns" )
fit1= smooth.spline(lyv_orig$Date, lyv_orig$LYVret,cv=TRUE)
fit1$df #2 df
fit2 = smooth.spline(lyv_orig$Date, lyv_orig$LYVret, df=10)
fit3 = smooth.spline(lyv_orig$Date, lyv_orig$LYVret, df=20)
fit4 = smooth.spline(lyv_orig$Date, lyv_orig$LYVret, df=30)
lines(fit1, col="blue",lwd=2)
lines(fit2,col="red",lwd=2)
lines(fit3, col="green", lwd=2)
lines(fit4, col="orange", lwd=2)
legend("topright", legend=c("30 DF","20 DF","10 DF", "2 DF"), col=c("orange","green", "red", "blue"), lty=1, lwd=2,cex=.8)

    #Risk
plot(lyv_orig$Date, lyv_orig$LnLYVrisk, cex=.5, main="Smoothing Spline of Risk, Danielle Cosentino", xlab="Years", ylab="ln(Range)")
fit1= smooth.spline(lyv_orig$Date, lyv_orig$LnLYVrisk, cv=TRUE)
fit1$df #177 df
fit2 = smooth.spline(lyv_orig$Date, lyv_orig$LnLYVrisk, df=100)
fit3 = smooth.spline(lyv_orig$Date, lyv_orig$LnLYVrisk, df=80)
fit4 = smooth.spline(lyv_orig$Date, lyv_orig$LnLYVrisk, df=50)
lines(fit1, col="blue",lwd=2)
lines(fit2, col="red",lwd=2)
lines(fit3, col="green",lwd=2)
lines(fit4, col="orange",lwd=2)
legend("topleft", legend=c("50 DF","80 DF","100 DF", "177 DF"), col=c("orange","green", "red", "blue"), lty=1, lwd=2,cex=.8)

#2010 data
    #Returns
plot(lyv_orig2010$Date, lyv_orig2010$LYVret, cex=.5, main="Smoothing Spline of Daily Returns in 2010, Danielle Cosentino", xlab="Months", ylab="Daily Returns" )
fit1 = smooth.spline(lyv_orig2010$Date, lyv_orig2010$LYVret,cv=TRUE)
fit1$df #3 df
fit2 = smooth.spline(lyv_orig2010$Date, lyv_orig2010$LYVret, df=1)
fit3 = smooth.spline(lyv_orig2010$Date, lyv_orig2010$LYVret, df=10)
fit4 = smooth.spline(lyv_orig2010$Date, lyv_orig2010$LYVret, df=20)
lines(fit1, col="blue",lwd=2)
lines(fit2, col="red", lwd=2)
lines(fit3, col="green", lwd=2)
lines(fit4, col="orange", lwd=2)
legend("topright", legend=c("20 DF","10 DF","1 DF", "3 DF"), col=c("orange","green", "red", "blue"), lty=1, lwd=2,cex=.8)

    #Risk
plot(lyv_orig2010$Date, lyv_orig2010$LYVrisk, cex=.5, main="Smoothing Spline of Risk in 2010, Danielle Cosentino", xlab="Months", ylab="ln(Range)" )
fit1= smooth.spline(lyv_orig2010$Date, lyv_orig2010$LYVrisk,cv=TRUE)
fit1$df #28 df
fit2 = smooth.spline(lyv_orig2010$Date, lyv_orig2010$LYVrisk,df=60)
fit3 = smooth.spline(lyv_orig2010$Date, lyv_orig2010$LYVrisk, df=100)
fit4 = smooth.spline(lyv_orig2010$Date, lyv_orig2010$LYVrisk, df=5)
lines(fit3, col="green", lwd=2)
lines(fit2, col="red", lwd=2)
lines(fit1, col="blue",lwd=2)
lines(fit4, col="orange", lwd=2)
legend("topleft", legend=c("5 DF","100 DF","60 DF", "28 DF"), col=c("orange","green", "red", "blue"), lty=1, lwd=2,cex=.8)

#2020-Present data
    #Returns
plot(lyv_orig2020$Date, lyv_orig2020$LYVret, cex=.5, main="Smoothing Spline of Daily Returns from 2020-2023, Danielle Cosentino", xlab="Months", ylab="Daily Returns" )
fit1 = smooth.spline(lyv_orig2020$Date, lyv_orig2020$LYVret,cv=TRUE)
fit1$df #2 df
fit2 = smooth.spline(lyv_orig2020$Date, lyv_orig2020$LYVret, df=5)
fit3 = smooth.spline(lyv_orig2020$Date, lyv_orig2020$LYVret, df=10)
fit4 = smooth.spline(lyv_orig2020$Date, lyv_orig2020$LYVret, df=30)
lines(fit4, col="orange", lwd=2)
lines(fit3, col="green", lwd=2)
lines(fit2, col="red", lwd=2)
lines(fit1, col="blue",lwd=2)
legend("topright", legend=c("30 DF","10 DF","5 DF", "2 DF"), col=c("orange","green", "red", "blue"), lty=1, lwd=2,cex=.8)

    #Risk
plot(lyv_orig2020$Date, lyv_orig2020$LYVrisk, cex=.5, main="Smoothing Spline of Risk from 2020-2023, Danielle Cosentino", xlab="Months", ylab="ln(Range)" )
fit1= smooth.spline(lyv_orig2020$Date, lyv_orig2020$LYVrisk,cv=TRUE)
fit1$df #67 df
fit2 = smooth.spline(lyv_orig2020$Date, lyv_orig2020$LYVrisk,df=100)
fit3 = smooth.spline(lyv_orig2020$Date, lyv_orig2020$LYVrisk, df=30)
fit4 = smooth.spline(lyv_orig2020$Date, lyv_orig2020$LYVrisk, df=5)
lines(fit2, col="red", lwd=2)
lines(fit1, col="blue",lwd=2)
lines(fit3, col="green", lwd=2)
lines(fit4, col="orange", lwd=2)
legend("topleft", legend=c("5 DF","30 DF","100 DF", "67 DF"), col=c("orange","green", "red", "blue"), lty=1, lwd=2,cex=.8)

#3a
#open classification data set
ret_class = read.csv("LYVret_class.csv")
risk_class = read.csv("LYVrisk_class.csv")

#need to convert last column to factor for the classification data:
ret_class$LYVret = as.factor(ret_class$LYVret)
risk_class$LYVrisk = as.factor(risk_class$LYVrisk)

#take random sample of n=400
ret_regr_rand = ret_regr[sample(4351,400),]
risk_regr_rand = risk_regr[sample(4351,400),]

ret_class_rand = ret_class[sample(4351,400),]
risk_class_rand = risk_class[sample(4351,400),]

library(tree)
#RETURN CLASSIFICATION DATA
tree.ret_class = tree(LYVret~.-Date, ret_class_rand)
summary(tree.ret_class)
plot(tree.ret_class)
text(tree.ret_class, pretty=0)
tree.ret_class

	#estimating error
set.seed(12)
train = sample(1:nrow(ret_class_rand), 200)
ret_class.test = ret_class_rand[-train,]
LYVret.test = ret_class_rand$LYVret[-train]
tree.ret_class = tree(LYVret~.-Date, ret_class_rand, subset=train)
summary(tree.ret_class)
plot(tree.ret_class)
text(tree.ret_class, pretty=0, cex=.5)
tree.ret_class
tree.pred = predict(tree.ret_class, ret_class.test, type="class")
table(tree.pred, LYVret.test)

	#cross-validation for pruning
cv.ret_class = cv.tree(tree.ret_class, FUN=prune.misclass)
names(cv.ret_class)
cv.ret_class	#dev=6

	#prune tree to get 6 nodes
prune.ret_class = prune.misclass(tree.ret_class, best=6)
plot(prune.ret_class)
text(prune.ret_class, pretty=0)

	#estimating test error on pruned tree
tree.pred = predict(prune.ret_class, ret_class.test, type="class")
table(tree.pred, LYVret.test)

#RISK CLASSIFICATION DATA
    #estimating error
set.seed(14)
train = sample(1:nrow(risk_class_rand), 200)
risk_class.test = risk_class_rand[-train,]
LYVrisk.test = risk_class_rand$LYVrisk[-train]
tree.risk_class = tree(LYVrisk~.-Date, risk_class_rand, subset=train)
summary(tree.risk_class)
plot(tree.risk_class)
text(tree.risk_class, pretty=0,cex=.5)
tree.pred2 = predict(tree.risk_class, risk_class.test, type="class")
table(tree.pred2, LYVrisk.test)

    #cross-validation for pruning
set.seed(15)
cv.risk_class = cv.tree(tree.risk_class, FUN=prune.misclass)
names(cv.risk_class)
cv.risk_class   #dev=2

    #prune tree to get 5 nodes
prune.risk_class = prune.misclass(tree.risk_class, best=2)
plot(prune.risk_class)
text(prune.risk_class, pretty=0)

    #estimating test error on pruned tree
tree.pred3 = predict(prune.risk_class, risk_class.test, type="class")
table(tree.pred3, LYVrisk.test)

#RETURN REGRESSION DATA
    #training data and plot
set.seed(75)
train = sample(1:nrow(ret_regr_rand), nrow(ret_regr_rand)/2)
tree.ret_regr = tree(LYVret~.-Date, ret_regr_rand, subset=train)
summary(tree.ret_regr)
plot(tree.ret_regr)
text(tree.ret_regr, pretty=0)

    #cross-validation for pruning
cv.ret_regr = cv.tree(tree.ret_regr)
plot(cv.ret_regr$size, cv.ret_regr$dev, type="b", main="CV Error Rate of Regression Returns, Danielle Cosentino")
cv.ret_regr     #dev=5

    #prune tree to get 5 nodes
prune.ret_regr = prune.tree(tree.ret_regr, best=7)
plot(prune.ret_regr)
text(prune.ret_regr, pretty=0)

    #use unpruned tree to make predictions on test set
yhat = predict(tree.ret_regr, newdata=ret_regr_rand[-train,])
ret_regr.test = ret_regr_rand[-train, "LYVret"]
plot(yhat, ret_regr.test, main="Predictions on Test Returns, Danielle Cosentino")
abline(0,1)
mean((yhat-ret_regr.test)^2)

#RISK REGRESSION DATA
    #training data and plot
set.seed(76)
train = sample(1:nrow(risk_regr_rand), nrow(risk_regr_rand)/2)
tree.risk_regr = tree(LYVrisk~.-Date, risk_regr_rand, subset=train)
summary(tree.risk_regr)
plot(tree.risk_regr)
text(tree.risk_regr, pretty=0)

    #cross-validation for pruning
cv.risk_regr = cv.tree(tree.risk_regr)
plot(cv.risk_regr$size, cv.risk_regr$dev, type="b")
cv.risk_regr #dev=6, same as original tree

    #use unpruned tree to make predictions on test set
yhat = predict(tree.risk_regr, newdata=risk_regr_rand[-train,])
risk_regr.test = risk_regr_rand[-train,"LYVrisk"]
plot(yhat, risk_regr.test, main="Predictions on Test Risk, Danielle Cosentino")
abline(0,1)
mean((yhat-risk_regr.test)^2)

#4a Tuberculosis data
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
tb.countries = read.csv("TB_burden_countries_2023-05-13.csv")
tb.est = read.csv("MDR_RR_TB_burden_estimates_2023-05-13.csv")
tb.exp = read.csv("TB_expenditure_utilisation_2023-05-13.csv")
tb.surv = read.csv("TB_dr_surveillance_2023-05-13.csv")
tb.contact = read.csv("TB_contact_tpt_2023-05-13.csv")

#for contact data: prevtx_data_available==60, remove NAs
tb.contact60 = tb.contact[tb.contact$prevtx_data_available==60,]
tb.contact60 <- tb.contact60 %>% drop_na(prevtx_data_available)
tb.contact60 <- tb.contact60[,c('iso3','year','newinc_con_prevtx')]

#merge all four datasets by columns iso3 and year
tb.join <- tb.countries %>% inner_join(tb.est, by=c('iso3','year'))
tb.join <- tb.join %>% inner_join(tb.exp, by=c('iso3', 'year'))
tb.join <- tb.join %>% inner_join(tb.surv, by=c('iso3','year'))
tb.join <- tb.join %>% inner_join(tb.contact60, by=c('iso3', 'year'))

tb.clean = tb.join[,c("country.x","iso3","g_whoregion.x","year","e_pop_num","e_inc_num","e_inc_tbhiv_num","e_rr_pct_ret","e_inc_rr_num","exp_lab","pulm_labconf_new","pulm_labconf_ret","rr_new","rr_rel","mdr_new","mdr_ret","newinc_con_prevtx","e_mort_num")]

tb.clean = tb.clean[!is.na(tb.clean$g_whoregion.x),] #removing any columns with missing iso3 code
tb.clean$g_whoregion.x = as.factor(tb.clean$g_whoregion.x) #creating factor levels
tb.clean$iso3 <- tb.clean$iso3 %>% as.factor()
tb.clean[is.na(tb.clean)] <-0 #replace any missing values with 0

#remove outliers
q1 = quantile(tb.clean$e_mort_num, .25)
q3 = quantile(tb.clean$e_mort_num, .75)
iqr = IQR(tb.clean$e_mort_num)
tb.clean = subset(tb.clean, tb.clean$e_mort_num>(q1-1.5*iqr) & tb.clean$e_mort_num<(q3+1.5*iqr))

#y-var is e_mort_num
pairs(tb.clean[,6:18], cex=.5) #scatterplot matrix
#correlation
cor(tb.clean[,6:18])

#mortality per year based on region
tb.clean%>% group_by(g_whoregion.x, year) %>% summarize(Sum=sum(e_mort_num)) %>% ggplot(aes(x=year,y=Sum, group=g_whoregion.x,color=g_whoregion.x))+geom_smooth(se=FALSE) +labs(title="Tuberculosis Deaths per Region from 2017-2021, Danielle Cosentino",x="Years",y="Number of Deaths",colour="Region")

#lab expenditure
ggplot(tb.clean, aes(year, exp_lab, fill=g_whoregion.x)) +geom_col() +labs(title="Lab Expenditure in U.S. Dollars from 2017-2021,Danielle Cosentino", x="Years", y="Lab Expenditure (U.S.$)", fill="Region") +scale_y_continuous(labels = function(x) format(x, scientific=FALSE))

#mapping number of deaths per 100k on a world map
tb.countries2017 = subset(tb.countries, year>=2017))
tbcountries2017[is.na(tb.countries2017)] <-0
plot_geo() %>% add_trace(z=~tb.countries2017$e_mort_100k, text=tb.countries2017$country, span=I(0),locations=tb.countries2017$iso3, locationmode='ISO-3') %>% rangeslider(tb.countries2017$e_mort_100k[0], tb.countries2017$e_mort_100k[200]) %>%colorbar(title="Mortality per 100k",limits=c(0,150)) %>%layout(title="Tuberculosis Deaths per 100k, Danielle Cosentino")

#4b
tb.regr = tb.clean[,c("e_inc_num","e_inc_tbhiv_num","e_rr_pct_ret","e_inc_rr_num","exp_lab","pulm_labconf_new","pulm_labconf_ret","rr_new","rr_rel","mdr_new","mdr_ret","newinc_con_prevtx","e_mort_num")]
set.seed(25)
train = sample(1:nrow(tb.regr), floor(nrow(tb.regr)/2))
tree.tb = tree(e_mort_num~., tb.regr, subset=train)
summary(tree.tb)
plot(tree.tb)
text(tree.tb, pretty=0)

cv.tb = cv.tree(tree.tb)
plot(cv.tb$size, cv.tb$dev, type="b")
cv.tb #dev=7, same as original tree

yhat = predict(tree.tb, newdata=tb.regr[-train,])
tb.test = tb.regr[-train,"e_mort_num"]
plot(yhat, tb.test, main="Predictions on Test Tuberculosis Data, Danielle Cosentino")
abline(0,1)
mean((yhat-tb.test)^2) #mse=406,769.7

#4c (i) Least squares regression
lm.fit = lm(e_mort_num~., data=tb.regr, subset=train)
summary(lm.fit)

lm.pred = predict(lm.fit, newdata=tb.regr[-train,])
mean((lm.pred-tb.test)^2) #mse=1,209,498

#4c (ii) Ridge regression on out-of-sample data
library(glmnet)
x=model.matrix(e_mort_num~.,tb.regr)[,-1]
y=tb.regr$e_mort_num

ridge.mod = glmnet(x[train,],y[train], alpha=0, thresh=1e-12)

#choose lambda using c-v
cv.out = cv.glmnet(x[train,], y[train],alpha=0)
(bestlam=cv.out$lambda.min) #bestlam=164.2223
plot(cv.out, main="MSE of Ridge Regression, Danielle Cosentino")

ridge.pred = predict(ridge.mod, s=bestlam, newx=x[-train,])
mean((ridge.pred-y[-train])^2) #mse=836,682.8

out=glmnet(x,y,alpha=0)
(ridge.coef = predict(out, type="coefficients", s=bestlam)[1:12,]) #ridge model uses all 11 x-vars

#4c (iii) lasso regression
lasso.mod = glmnet(x[train,],y[train],alpha=1)
cv.out2 = cv.glmnet(x[train,],y[train], alpha=1)
plot(cv.out2, main="MSE of Lasso Regression, Danielle Cosentino")
(bestlam2 = cv.out2$lambda.min) #bestlam = 8.970277

lasso.pred=predict(lasso.mod, s=bestlam2, newx=x[-train,])
mean((lasso.pred-y[-train])^2) #mse=896,367.4

out2 = glmnet(x,y,alpha=1)
(lasso.coef = predict(out2, type="coefficients",s=bestlam2)[1:12,])
lasso.coef[lasso.coef!=0] #lasso model with best lambda  has 10 x-variables
