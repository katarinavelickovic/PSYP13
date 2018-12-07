###########################################################
#                                                         #
#                   Assignment 1                          #
#                                                         #
###########################################################

#########
#loading packages
########

library(psych)
library(car)
library(lmtest)
library(lm.beta)

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_1.csv")
View(data_sample_1)

###########################################################
#                                                         #
#                 Coding errors                           #
#                                                         #
###########################################################

#exploring age
describe(data_sample_1$age)
hist(data_sample_1$age, breaks = 20)

data_sample_1_cleaned = data_sample_1 #creating a copy
data_sample_1_cleaned = data_sample_1_cleaned[-which(data_sample_1_cleaned[,"age"] == "222"),] # excluding the invalid value
hist(data_sample_1_cleaned$age, breaks = 20)

#exploring anxiety
describe(data_sample_1_cleaned$STAI_trait)
hist(data_sample_1_cleaned$STAI_trait, breaks = 20)

#exploring sex
table(data_sample_1_cleaned$sex) 
data_sample_1_cleaned[,"sex"][data_sample_1_cleaned[,"sex"] == "female "] = "female" # just in case
data_sample_1_cleaned[,"sex"][data_sample_1_cleaned[,"sex"] == "male "] = "male" # just in case
table(data_sample_1_cleaned$sex)
factor(data_sample_1_cleaned$sex)

#exploring pain catastrophizing
hist(data_sample_1_cleaned$pain_cat, breaks = 20) 

#exploring mindfulness
describe(data_sample_1_cleaned$mindfulness)
hist(data_sample_1_cleaned$mindfulness, breaks = 20)
data_sample_1_cleaned = data_sample_1_cleaned[-which(data_sample_1_cleaned[,"mindfulness"] < "1"),] # excluded values lower than 1

#exploring cortisol measures

hist(data_sample_1_cleaned$cortisol_serum, breaks = 20)
hist(data_sample_1_cleaned$cortisol_saliva, breaks = 20)

plot(cortisol_saliva ~ cortisol_serum, data = data_sample_1_cleaned) #just checking to see how these measures are associated


###########################################################
#                                                         #
#                Checking for outliers                    #
#                                                         #
###########################################################

#building the models
mod1 <- lm(pain ~ age+sex, data = data_sample_1_cleaned)
mod2 <- lm(pain ~ age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva, data = data_sample_1_cleaned)

#plotting

plot(data_sample_1_cleaned$pain ~ data_sample_1_cleaned$age)
abline(lm(pain ~ age, data = data_sample_1_cleaned))

plot(data_sample_1_cleaned$pain ~ data_sample_1_cleaned$STAI_trait)
abline(lm(pain ~ STAI_trait, data = data_sample_1_cleaned))

plot(data_sample_1_cleaned$pain ~ data_sample_1_cleaned$pain_cat)
abline(lm(pain ~ pain_cat, data = data_sample_1_cleaned))

plot(data_sample_1_cleaned$pain ~ data_sample_1_cleaned$mindfulness)
abline(lm(pain ~ mindfulness, data = data_sample_1_cleaned))

plot(data_sample_1_cleaned$pain ~ data_sample_1_cleaned$cortisol_serum)
abline(lm(pain ~ cortisol_serum, data = data_sample_1_cleaned))

plot(data_sample_1_cleaned$pain ~ data_sample_1_cleaned$cortisol_saliva)
abline(lm(pain ~ cortisol_saliva, data = data_sample_1_cleaned))

plot(mod2, which = 5)
plot(mod2, which = 4)

# tried to exclude the observation with highest Cook's distance, but no diff in coefficients.

mod2_71 <- lm(pain ~ age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva, data = data_sample_1_cleaned, subset=-71)
mod2
mod2_71

###########################################################
#                                                         #
#                Checking assumptions                     #
#                                                         #
###########################################################

#1.Normality of residuals

#qq plots
plot(mod2, which = 2)

#histograms of residuals
hist(residuals(mod2), breaks = 20)

describe(residuals(mod2))

#2.linearity assumption

#checking the "big picture" linearity by checking the relationship between fitted and observed values
yhat.2 <- fitted.values( object = mod2)
plot( x = yhat.2,
        y = data_sample_1_cleaned$pain,
        xlab = "Fitted Values",
        ylab = "Observed Values"
        )

#relationship between fitted values and residuals
residualPlots( model = mod2 )

#3.homoscedascity
plot(mod2, which=3)

ncvTest(mod2)

bptest(mod2)

#4.multicolinearity
cor(data_sample_1_cleaned$cortisol_saliva, data_sample_1_cleaned$cortisol_serum)
vif(mod=mod2)

#decided to remove cortisol_saliva from the model, based on previous findings and high multicolinearity

mod3 <- lm(pain ~ age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum, data = data_sample_1_cleaned)
mod3
summary(mod3)

confint(mod3)
lm.beta(mod3)

anova(mod1,mod3)
AIC(mod1,mod3)

###########################################################
#                                                         #
#                  Assignment 2                           #
#                                                         #
###########################################################

#exploring weight
describe(data_sample_1_cleaned$weight)
hist(data_sample_1_cleaned$weight, breaks = 20)

###########################################################
#                                                         #
#                Checking for outliers                    #
#                                                         #
###########################################################

#building the models
model1 <- lm(pain ~ age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum, data = data_sample_1_cleaned)
model2 <- lm(pain ~ age+sex+weight+STAI_trait+pain_cat+mindfulness+cortisol_serum, data = data_sample_1_cleaned)
step(object=model2, direction="backward")


plot(model2, which = 5)
plot(model2, which = 4)


###########################################################
#                                                         #
#                Checking assumptions                     #
#                                                         #
###########################################################

#1.Normality of residuals

#qq plots
plot(model2, which = 2)

#histograms of residuals
hist(residuals(model2), breaks = 20)

describe(residuals(model2))

#2.linearity assumption

#checking the "big picture" linearity by checking the relationship between fitted and observed values
yhat.2 <- fitted.values( object = model2)
plot( x = yhat.2,
      y = data_sample_1_cleaned$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values"
)

#relationship between fitted values and residuals
residualPlots( model = model2 )

#3.homoscedascity
plot(model2, which=3)

ncvTest(model2)

bptest(model2)

#4.multicolinearity
vif(mod=model2)

#building the models

theory_model <- lm(pain ~ age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum, data = data_sample_1_cleaned)
backward_model <- lm(pain ~ age+sex+pain_cat+mindfulness+cortisol_serum, data = data_sample_1_cleaned)

summary(backward_model)
confint(backward_model)
lm.beta(backward_model)

AIC(backward_model, theory_model)
anova(backward_model, theory_model)

#running on new data

data_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_2.csv")
View(data_sample_2)

#exploring age
describe(data_sample_2$age)
hist(data_sample_2$age, breaks = 20)

#exploring anxiety
describe(data_sample_2$STAI_trait)
hist(data_sample_2$STAI_trait, breaks = 20)

#exploring sex
table(data_sample_2$sex) 
factor(data_sample_2$sex)

#exploring pain catastrophizing
describe(data_sample_2$pain_cat)
hist(data_sample_2$pain_cat, breaks = 20)

#exploring mindfulness
describe(data_sample_2$mindfulness)
hist(data_sample_2$mindfulness, breaks = 20)
data_sample_2_cleaned=data_sample_2
data_sample_2_cleaned = data_sample_2_cleaned[-which(data_sample_2_cleaned[,"mindfulness"] < "1"),] # excluded values lower than 1

#exploring cortisol measures

hist(data_sample_2_cleaned$cortisol_serum, breaks = 20)
hist(data_sample_2_cleaned$cortisol_saliva, breaks = 20)

predicted_pain = predict(theory_model, newdata = data_sample_2_cleaned)
cbind(data_sample_2_cleaned, predicted_pain)
predicted_pain

predicted_pain = predict(backward_model, newdata = data_sample_2_cleaned)
cbind(data_sample_2_cleaned, predicted_pain)
predicted_pain

RSS1 = sum((data_sample_2_cleaned$pain - predict(theory_model))^2)
TSS1

RSS2 = sum((data_sample_2_cleaned$pain - predict(backward_model))^2)
TSS2

###########################################################
#                                                         #
#                  Assignment 3                           #
#                                                         #
###########################################################

library(psych) # for describe
library(ggplot2) # for ggplot
library(cAIC4) # for cAIC
library(r2glmm) # for r2beta
library(reshape2) # for melt
library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer
library(influence.ME) # for influence
library(lattice) # for qqmath

source("GraphPlots.R")

data3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_3.csv")
View(data3)

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

#assigning ID and sex as factors
data3$ID = factor(data3$ID)
data3$sex = factor(data3$sex)

# descriptives, checking the data
describe(data3)
table(data3[, "sex"])
hist(data3$pain1,breaks = 20)
hist(data3$pain2,breaks = 20)
hist(data3$pain3,breaks = 20)
hist(data3$pain4,breaks = 20)
hist(data3$STAI_trait,breaks = 20)
hist(data3$pain_cat,breaks = 20)
hist(data3$mindfulness,breaks = 20)
hist(data3$cortisol_serum,breaks = 20)

#identifying repeated measures
repeated_variables = c("pain1", "pain2", "pain3", "pain4")
cor(data3[, repeated_variables])

#changing the format from wide to long, rearranging the data set
data3long = melt(data3, measure.vars = repeated_variables,
                 variable.name = "time", value.name = "pain")
data3long = data3long[order(data3long[, "ID"]),]

data3long$time = as.numeric(data3long$time)

#building the models
mod_int = lmer(pain ~ age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+time+(1 | ID), data = data3long)
mod_slope = lmer(pain ~ age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+time + (time | ID), data = data3long)

data3long_withpreds = data3long
data3long_withpreds$pred_int = predict(mod_int)
data3long_withpreds$pred_slope = predict(mod_slope)

#visualizing actual observations and predictions in order to compare models
ggplot(data3long_withpreds, aes(y = pain, x = time,
                                group = ID)) + geom_point(size = 3) + geom_line(color = "red",
                                                                                aes(y = pred_int, x = time)) + facet_wrap(~ID, ncol = 5)

ggplot(data3long_withpreds, aes(y = pain, x = time,
                                group = ID)) + geom_point(size = 3) + geom_line(color = "red",
                                                                                aes(y = pred_slope, x = time)) + facet_wrap(~ID, ncol = 5)

#It seems like the slope model fits the data slightly better, however:
cAIC(mod_int)$caic
cAIC(mod_slope)$caic
anova(mod_int,mod_slope)

#quadratic term of time:
mod_int_quad = lmer(pain ~ age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+time+I(time^2)+(1 | ID), data = data3long)

data3long_withpreds$pred_int_quad = predict(mod_int_quad)

plot_quad = ggplot(data3long_withpreds, aes(y = pain,
                                            x = time, group = ID)) + geom_point(size = 3) + geom_line(color = "red",
                                                                                                      aes(y = pred_int_quad, x = time)) + facet_wrap(~ID, ncol = 5)

plot_quad

cAIC(mod_int)$caic
cAIC(mod_int_quad)$caic

anova(mod_int, mod_int_quad)

data3long_centered_time = data3long
data3long_centered_time$time_centered = data3long_centered_time$time -
  mean(data3long_centered_time$time)

mod_int_quad = lmer(pain ~ time_centered + I(time_centered^2)+age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum + (1 | ID), data = data3long_centered_time)
cAIC(mod_int_quad)$caic

#marginal R squared
r2beta(mod_int_quad, method = "nsj", data = data3long_centered_time)
?r2beta

#cAIC
cAIC(mod_int_quad)$caic

#model coefficients
summary(mod_int_quad)

# Confidence intervals for the coefficients
confint(mod_int_quad)

# standardized Betas
stdCoef.merMod(mod_int_quad)

#model diagnostics

#influential outliers
influence_observation = influence(mod_int_quad, obs = T)$alt.fixed
influence_group = influence(mod_int_quad, group = "ID")$alt.fixed

pred_names = colnames(influence_group)

par(mfrow = c(1, length(pred_names)))
for (i in 1:length(pred_names)) {
  boxplot(influence_observation[, pred_names[i]], main = pred_names[i])
}

for (i in 1:length(pred_names)) {
  boxplot(influence_group[, pred_names[i]], main = pred_names[i])
}

#normality of residuals
resid<-residuals(mod_int_quad)

openGraph()
qqnorm(resid)
shapiro.test( resid ) 

#linearity

openGraph()
plot(mod_int_quad, arg = "pearson")

data3long_with_resid = data3long_centered_time
data3long_with_resid$resid = residuals(mod_int_quad)

openGraph()
plot(resid ~ time_centered, data = data3long_with_resid)
openGraph()
plot(resid ~ age, data = data3long_with_resid)
openGraph()
plot(resid ~ sex, data = data3long_with_resid)
openGraph()
plot(resid ~ mindfulness, data = data3long_with_resid)
openGraph()
plot(resid ~ STAI_trait, data = data3long_with_resid)
openGraph()
plot(resid ~ pain_cat, data = data3long_with_resid)
openGraph()
plot(resid ~ cortisol_serum, data = data3long_with_resid)

#homoscedacity
plot(mod_int_quad, arg = "pearson")
homosced_mod = lm(data3long_with_resid$resid^2 ~ data3long_with_resid$ID)
summary(homosced_mod)

# caluclate interquartile range within each cluster
IQR_of_residuals_by_participant = sapply(split(data3long_with_resid,
                                               f = data3long_with_resid$ID), function(x) IQR(x$resid))
# rank ordering them
rank = rank(IQR_of_residuals_by_participant)
# adding rank to the dataframe containing the residuals
data3long_with_resid$rank = rep(rank, each = length(repeated_variables))
# creating a vector of participant IDs ordered based on the
# rank, this will be used as labels
IDforplot = unique(data3long_with_resid$ID[order(data3long_with_resid$rank)])

ggplot(data3long_with_resid, aes(y = resid, x = factor(rank),labels = ID)) + geom_boxplot() + scale_x_discrete(labels = IDforplot) +coord_flip()

#multicolinearity
pairs.panels(data3long_centered_time[, c("time_centered", "age", "sex", "mindfulness", "STAI_trait", "pain_cat", "cortisol_serum")], col = "red",
             lm = T)
