data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

library(tidyverse)
library(psych)
library(dplyr)

DS1 = data_sample_1

View(DS1)

# ID_34 - STAI_trait and ID_88 - pain coding errors --> exclude them, change sex variable for data analysis

DS1 = DS1 %>%
  mutate(sex = factor(sex))
levels(DS1$sex)

DS1_clean = DS1[-c(34, 88), ]

DS1_clean %>%
  summary(DS1_clean)

pain_mod1 = lm(pain ~ age + sex, data = DS1_clean)
summary(pain_mod1)
summary(pain_mod1)$adj.r.squared

pain_mod2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = DS1_clean)
summary(pain_mod2)
summary(pain_mod2)$adj.r.squared

AIC(pain_mod1)
AIC(pain_mod2)

# difference bigger than 2, accept smaller model which is model 2

anova(pain_mod1, pain_mod2)

cooks.distance(pain_mod2)

4 / 158

pain_mod2 %>%
  plot(which = 4)

pain_mod2 %>%
  plot(which = 5)

# many cases with cook's distance higher than 0.025 but all >1

# now check normality, linearity, homoscedasticity, multicollinearity

# normality

pain_mod2 %>%
  plot(which = 2)

describe(residuals(pain_mod2))

residuals_pain_mod2 = enframe(residuals(pain_mod2))
residuals_pain_mod2 %>%
  ggplot() + aes(x = value) + geom_histogram()

# normality is given with skew and kurtosis between -1 and 1, skew is -0.15 and kurtosis is -0.03

library(car)

# linearity

pain_mod2 %>%
  residualPlots()

# linearity assumption holds true with all tests being non significant

# homoscedasticity

pain_mod2 %>%
  plot(which = 3)

pain_mod2 %>%
  ncvTest()

library(lmtest)

pain_mod2 %>%
  bptest()

# p value bigger than 0.05 therefore no heterostadicity

pain_mod2 %>%
  vif()

# there is multicolinarity for both cortisol measures, all other are under 3, could exclude saliva 

pain_mod_final = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = DS1_clean)

#### RERUN TESTS FOR FINAL MODEL

cooks.distance(pain_mod_final)

4 / 158

pain_mod_final %>%
  plot(which = 4)

pain_mod_final %>%
  plot(which = 5)

# a few cases with cook's distance higher than 0.025 but all >1

# now check normality, linearity, homoscedasticity, multicollinearity

# normality

pain_mod_final %>%
  plot(which = 2)

describe(residuals(pain_mod_final))

residuals_pain_mod_final = enframe(residuals(pain_mod_final))
residuals_pain_mod_final %>%
  ggplot() + aes(x = value) + geom_histogram()

# normality is given with skew and kurtosis between -1 and 1, skew is -0.18 and kurtosis is 0.02

# linearity

pain_mod_final %>%
  residualPlots()

# linearity assumption holds true with all tests being non significant

# homoscedasticity

pain_mod_final %>%
  plot(which = 3)

pain_mod_final %>%
  ncvTest()

pain_mod_final %>%
  bptest()

# p values bigger than 0.05 therefore no heterostadicity

pain_mod_final %>%
  vif()

# no multicolinarity anymore



##### FINISH RERUN


pain_mod_final %>%
  summary()

library(lm.beta)

confint(pain_mod_final)

lm.beta(pain_mod_final)

anova(pain_mod_final)

# regression equation für pain_mod_final without SALIVA

Y = 1.473851 + -0.040406 * age + 0.155581 * sex + -0.009472 * STAI_trait + 0.111333 * pain_cat + -0.277517 * mindfulness + 0.567650 * cortisol_serum



pain_mod2 %>%
  summary()

####### regression equation for pain_mod2

Y = -0.007629 + -0.020333 * age + 0.150553 * sex + -0.024244 * STAI_trait + 0.136053 * pain_cat + -0.247153 * mindfulness + 0.173495 * cortisol_serum + 0.489155 * cortisol_saliva

pain_mod1 %>%
  summary()

lm.beta(pain_mod1)

anova(pain_mod1, pain_mod_final)

# 

AIC(pain_mod1)
AIC(pain_mod2)
AIC(pain_mod_final)

# mod 1 574.1267 ; finalmod 479.2624
# difference >2, accept smaller model which is finalmod

library(sjPlot)

tab_model(pain_mod1, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

tab_model(pain_mod_final, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

##### part 2

library(MASS)

pain_mod_bw1 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = DS1_clean)

# data and model diagnostics

pain_mod_bw1 %>%
  summary()

anova(pain_mod_bw1)

# cooks distance

pain_mod_bw1 %>%
  plot(which = 4)

pain_mod_bw1 %>%
  plot(which = 5)

# cooks distance all under 0.05, all except around 10 under 0.025

# outliers are 47, 85, 86

# normality

pain_mod_bw1 %>%
  plot(which = 2)

describe(residuals(pain_mod_bw1))

residuals_pain_mod_bw1 = enframe(residuals(pain_mod_bw1))
residuals_pain_mod_bw1 %>%
  ggplot() + aes(x = value) + geom_histogram()

pain_mod_bw1 %>%
  plot(which = 2)

# skew and kurtosis is between -1 and 1 therefore normality is NOT violated, 
# residuals model shows that all cases are pretty close

# lineraity

pain_mod_bw1 %>%
  residualPlots()

# no significance therefore lineraity assumption holds true

# homoscedasticity 

pain_mod_bw1 %>%
  plot(which = 3)

pain_mod_bw1 %>%
  ncvTest()

# p value bigger than 0.05 therefore assumption of homoscedasticity holds true 

# multicollinarity

pain_mod_bw1 %>%
  vif()

# all under 3 therefore no problematic multicollinarity 

bwr_pain =  step(pain_mod_bw1, direction = "backward")

# age, mindfulness, cortisol_serum, pain_cat 

backward_model = lm(pain ~ age + mindfulness + cortisol_serum + pain_cat, data = DS1_clean)

backward_model %>%
  summary()

tab_model(backward_model, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)


anova(backward_model)


## THEORY BASED MODEL CHANGED


theory_based_model = pain_mod_final

theory_based_model %>%
  summary()

anova(theory_based_model)

AIC(backward_model)
AIC(pain_mod_bw1)

# difference more than 2, backward model smaller 

AIC(backward_model)
AIC(theory_based_model)

# difference more than 2, backward model is smaller


DS2 = read.csv("https://tinyurl.com/87v6emky")

DS2 = DS2 %>%
  mutate(sex = factor(sex))
levels(DS2$sex)


# compare predicted values with actual pain ratings, which model is better 

predictionBM = predict(backward_model, DS2)
predictionBM

predictionTBM = predict(theory_based_model, DS2)

residuals_BM = sum((DS2[, "pain"] - predictionBM)^2)

residuals_TBM = sum((DS2[, "pain"] - predictionTBM)^2)

residuals_BM

# 249.5637

residuals_TBM

# 243.8192

# backwards model higher so more error, theory based model makes less error when predicting 

summary(backward_model)$adj.r.squared
# 0.5073085

summary(theory_based_model)$adj.r.squared
# 0.504055

# the theory based model explains less of the variance 


# regression equation of backwardmodel

backward_model %>%
  summary()

Y = 1.27627 + -0.04116 * age + -0.26852 * mindfulness + 0.53383 * cortisol_serum + 0.11359 * pain_cat



# part 3

DS3 = read.csv("https://tinyurl.com/b385chpu")
view(DS3)

# woman instead of female and income, ID25 woman, ID2 income  

DS3_clean = DS3 %>%
  mutate(sex=replace(sex,sex=="woman", "female"))

DS3_clean = DS3_clean %>%
  mutate(sex = factor(sex))
levels(DS3_clean$sex)

DS4 = read.csv("https://tinyurl.com/4f8thztv")

library(lme4)
library(lmerTest)

linear_mixed_model = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = DS3_clean)

# compare two models

linear_mixed_model %>%
  summary()

tab_model(linear_mixed_model, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)


stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"] 
  se <- se.fixef * sdx/sdy 
  return(data.frame(stdcoef = sc, stdse = se))
  }

stdCoef.merMod(linear_mixed_model)

pain_mod_final%>%
  summary()

# same significant effects, 
# 

summary(linear_mixed_model)

anova(linear_mixed_model)
anova(pain_mod_final)

library(MuMIn)

r.squaredGLMM(linear_mixed_model)

# RsqMarginal ist 0.3852492 ; RsqConditional ist 0.0.4632079 --> marginal für fixed pred. , cond. für fixed + random 

predictionDF4 = predict(linear_mixed_model, DS4, allow.new.levels = TRUE)
  

residualsDF4 = sum((DS4[, "pain"] - predictionDF4)^2)
residualsDF4


RSS = sum((DS4$pain - predictionDF4)^2)
RSS

TSS = sum((DS4$pain - predict(linear_mixed_model))^2)
TSS

VarianceModel4 = 1 - (RSS/TSS)  
VarianceModel4

# Varianz is 0.5417238

# Varianz higher than RsqM and RsqC for linear mixed model 


new_linear_mixed_model = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = DS3_clean)

new_linear_mixed_model %>%
  summary()

predictionDF3 = predict(new_linear_mixed_model, DS3_clean, allow.new.levels = TRUE)

DS3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 1) + geom_line(color = "red",
                                                       aes(y = predictionDF3, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)

