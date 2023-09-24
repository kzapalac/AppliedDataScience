#### Results for Group Project ####

## install (if needed) and load required packages
# install.packages("tidyverse")
library(tidyverse)
# install.packages("naniar")
library(naniar)
# install.packages("misty")
library(misty)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("reghelper")
library(reghelper)
# install.packages("psych")
library(psych)
# install.packages("lme4")
library(lme4)
# install.packages("plyr")
library(plyr)
# install.packages("Hmisc")
library(Hmisc)
# install.packages("sjPlot")
library(sjPlot)
# install.packages("reshape2")
library(reshape2)
# install.packages("lmerTest")
library(lmerTest)
#install.packages("Hmisc")
library(Hmisc)

# import and view data
couplemoods <- read.csv("~/OneDrive/Documents/School/Year 3/AppliedDataScience/GroupProject/Data/CoupleMobileSensingProjectData.csv")

# select only the important variables (need to add confounding variables to this)
couplemoods <- select(couplemoods, ID, P1scl, P2scl, P1AllWe, P1AllYou, P1AllPosemo, P1AllNegemo, P1Close, P1ConsumeCaffiene, P1ConsumeAlcohol, P1PhysicalActivity,
                      P2AllWe, P2AllYou, P2AllPosemo, P2AllNegemo, P2Close, P2ConsumeCaffiene, P2ConsumeAlcohol, P2PhysicalActivity, Together, Interacting)
str(couplemoods)
head(couplemoods)

## replace missing data codes 999, 444, and 888 with NA for any variables you plan to use
# then delete missing data 
couplemoods = couplemoods %>% replace_with_na(replace = 
                                                list(P1scl = c(999, 444, 888), P2scl = c(999,444, 888), 
                                                     P1AllWe = c(999,444, 888), P1AllYou = c(999,444, 888), P1AllPosemo= c(999,444, 888), P1AllNegemo = c(999,444, 888), 
                                                     P1ConsumeCaffiene = c(999,444, 888), P1ConsumeAlcohol = c(999,444, 888), P1PhysicalActivity = c(999,444, 888),
                                                     P2AllWe = c(999,444, 888), P2AllYou = c(999,444, 888), P2AllPosemo= c(999,444, 888), P2AllNegemo = c(999,444, 888),
                                                     P2ConsumeCaffiene = c(999,444, 888), P2ConsumeAlcohol = c(999,444, 888), P2PhysicalActivity = c(999,444, 888),
                                                     P1Close = c(999,444, 888), P2Close = c(999,444, 888),
                                                     Together = c(999,444, 888), Interacting = c(999,444, 888)))


### Descriptive Statistics #################################################
# only key variables
couplemoods_desc <- select(couplemoods, ID, P1scl, P2scl, P1AllWe, P1AllYou, P1AllPosemo, P1AllNegemo, P1Close,
                           P2AllWe, P2AllYou, P2AllPosemo, P2AllNegemo, P2Close)
# correlation table with mean, sd, min, and max
corM <- cor(x=couplemoods_desc, use='complete.obs') # create correlation table
corMdf <- as.data.frame(corM)
# correlation table with p_values
df <- rcorr(as.matrix(couplemoods_desc), type='pearson')
print_this <- data.frame(df$r) %>% round(digits=3)
clipr::write_clip(print_this)

# other descriptive statist to add to the table
means <- colMeans(couplemoods_desc, na.rm=T)
sds <- sapply(couplemoods_desc, sd, na.rm=T)
mins <- sapply(couplemoods_desc, min, na.rm=T)
maxs <- sapply(couplemoods_desc, max, na.rm=T)
validValsPerCol <- 1560 - colSums(is.na(couplemoods_desc))
propValid <- validValsPerCol/1560
nullValsPerCol <- colSums(is.na(couplemoods_desc))
corMdf <- add_column(corMdf, mean = means, .before="ID")
corMdf <- add_column(corMdf, sd = sds, .after="mean")
corMdf <- add_column(corMdf, min = mins, .after="sd")
corMdf <- add_column(corMdf, max= maxs, .after="min")
corMdf <- add_column(corMdf, missing = nullValsPerCol, .after="max")
corMdf <- add_column(corMdf, valid_proportion = propValid, .after="missing")
corMdf <- round(corMdf, digits=2)
clipr::write_clip(corMdf) # copy the table to clipboard to easily transfer to paper and presentation


### Potential confounding variables ##########################################
# Potential confounds for the outcomes of H1a, H2, H3, and H4 (outcome=P2scl for all of these)
## Tested confounds: caffeine, alcohol, physical activity, together, and interacting
## center the necessary variables (don't need to center dichotomous variables: caffiene, alochol, together)
couplemoods$P1PhysicalActivityCWC <- center(couplemoods$P1PhysicalActivity, type='CWC', cluster=couplemoods$ID)
couplemoods$P2PhysicalActivityCWC <- center(couplemoods$P2PhysicalActivity, type='CWC', cluster=couplemoods$ID)
couplemoods$InteractingCWC <- center(couplemoods$Interacting, type='CWC', cluster=couplemoods$ID)
## create the model for P1
confoundmodel1a <- lmer(formula = P1scl ~ 1 + P1ConsumeCaffiene + P1ConsumeAlcohol + P1PhysicalActivityCWC + Together + InteractingCWC
                        + (1 | ID),
                        data=couplemoods,
                        na.action=na.exclude)
summary(confoundmodel1a)
##### p-value
coefs <-data.frame(coef(summary(confoundmodel1a)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
# create the model for P2
confoundmodel1b <- lmer(formula = P2scl ~ 1 + P2ConsumeCaffiene + P2ConsumeAlcohol + P2PhysicalActivityCWC + Together + InteractingCWC
                        + (1 | ID),
                        data=couplemoods,
                        na.action=na.exclude)
summary(confoundmodel1b)
##### p-value
coefs <-data.frame(coef(summary(confoundmodel1b)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
# confounds for H1b (outcome=P2wordusage for all of these)
## Tested confounds: together and interacting
### partner 1
#### we
confoundmodel2a <- lmer(formula = P1AllWe ~ 1 + Together + InteractingCWC + 
                          + (1 | ID),
                        data=couplemoods,
                        na.action=na.exclude)
summary(confoundmodel2a)
##### p-value
coefs <-data.frame(coef(summary(confoundmodel2a)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
#### you
confoundmodel2b <- lmer(formula = P1AllYou ~ 1 + Together + InteractingCWC + 
                          + (1 | ID),
                        data=couplemoods,
                        na.action=na.exclude)
summary(confoundmodel2b)
##### p-value
coefs <-data.frame(coef(summary(confoundmodel2b)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
#### Posemo
confoundmodel2c <- lmer(formula = P1AllPosemo ~ 1 + Together + InteractingCWC + 
                          + (1 | ID),
                        data=couplemoods,
                        na.action=na.exclude)
summary(confoundmodel2c)
###### p-value
coefs <-data.frame(coef(summary(confoundmodel2c)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
#### Negemo
confoundmodel2d <- lmer(formula = P1AllNegemo ~ 1 + Together + InteractingCWC + 
                          + (1 | ID),
                        data=couplemoods,
                        na.action=na.exclude)
summary(confoundmodel2d)
##### p-value
coefs <-data.frame(coef(summary(confoundmodel2d)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
### partner 2
#### We
confoundmodel22a <- lmer(formula = P2AllWe ~ 1 + Together + InteractingCWC + 
                           + (1 | ID),
                         data=couplemoods,
                         na.action=na.exclude)
summary(confoundmodel22a)
###### p-value
coefs <-data.frame(coef(summary(confoundmodel22a)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
### You
confoundmodel22b <- lmer(formula = P2AllYou ~ 1 + Together + InteractingCWC + 
                           + (1 | ID),
                         data=couplemoods,
                         na.action=na.exclude)
summary(confoundmodel22b)
##### p-value
coefs <-data.frame(coef(summary(confoundmodel22b)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
### Posemo
confoundmodel22c <- lmer(formula = P2AllPosemo ~ 1 + Together + InteractingCWC + 
                           + (1 | ID),
                         data=couplemoods,
                         na.action=na.exclude)
summary(confoundmodel22c)
##### p-value
coefs <-data.frame(coef(summary(confoundmodel22c)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
### Negemo
confoundmodel22d <- lmer(formula = P2AllNegemo ~ 1 + Together + InteractingCWC + 
                           + (1 | ID),
                         data=couplemoods,
                         na.action=na.exclude)
summary(confoundmodel22d)
##### p-value
coefs <-data.frame(coef(summary(confoundmodel22d)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)



### Dfs for each hypothesis ####################################################
# Create separate dfs for each hypothesis to retain as much data as possible
## select the important variables for each hypothesis and delete the rows with missing data
# H1a
couplemoodsH1a <-  couplemoods %>% select(ID, P1scl, P2scl, P1PhysicalActivity)
couplemoodsH1a <- na.omit(couplemoodsH1a)
# H1b
couplemoodsH1b <- couplemoods %>% select(ID, P1AllWe, P1AllYou, P1AllPosemo, P1AllNegemo, P2AllWe, P2AllYou, P2AllPosemo, P2AllNegemo, Interacting)
couplemoodsH1b <- na.omit(couplemoodsH1b)
# H2
couplemoodsH2 <- couplemoods %>% select(ID, P2scl, P1scl, P1AllWe, P1AllYou, P1AllPosemo, P1AllNegemo, P1PhysicalActivity)
couplemoodsH2 <- na.omit(couplemoodsH2)
# H3
couplemoodsH3 <-  couplemoods %>% select(ID, P1scl, P2scl, P1AllWe, P1Close, P1PhysicalActivity)
couplemoodsH3 <- na.omit(couplemoodsH3)
# H4
couplemoodsH4 <-  couplemoods %>% select(ID, P1scl, P2scl, P1AllYou, P1Close, P1PhysicalActivity)
couplemoodsH4 <- na.omit(couplemoodsH4)


# use partner 1 to predict partner 2 for all the models below
# CWC predictors and moderators (only partner 1) for each hypothesis
# H1a
couplemoodsH1a$P1sclCWC <- center(couplemoodsH1a$P1scl, type='CWC', cluster=couplemoodsH1a$ID)
couplemoodsH1a$P1PhysicalActivityCWC <- center(couplemoodsH1a$P1PhysicalActivity, type='CWC', cluster=couplemoodsH1a$ID)
# H1b
couplemoodsH1b$P1AllWeCWC <- center(couplemoodsH1b$P1AllWe, type='CWC', cluster=couplemoodsH1b$ID)
couplemoodsH1b$P1AllYouCWC <- center(couplemoodsH1b$P1AllYou, type='CWC', cluster=couplemoodsH1b$ID)
couplemoodsH1b$P1AllPosemoCWC <- center(couplemoodsH1b$P1AllPosemo, type='CWC', cluster=couplemoodsH1b$ID)
couplemoodsH1b$P1AllNegemoCWC <- center(couplemoodsH1b$P1AllNegemo, type='CWC', cluster=couplemoodsH1b$ID)
couplemoodsH1b$InteractingCWC <- center(couplemoodsH1b$Interacting, type='CWC', cluster=couplemoodsH1b$ID)
# H2
couplemoodsH2$P1sclCWC <- center(couplemoodsH2$P1scl, type='CWC', cluster=couplemoodsH2$ID)
couplemoodsH2$P1AllWeCWC <- center(couplemoodsH2$P1AllWe, type='CWC', cluster=couplemoodsH2$ID)
couplemoodsH2$P1AllYouCWC <- center(couplemoodsH2$P1AllYou, type='CWC', cluster=couplemoodsH2$ID)
couplemoodsH2$P1AllPosemoCWC <- center(couplemoodsH2$P1AllPosemo, type='CWC', cluster=couplemoodsH2$ID)
couplemoodsH2$P1AllNegemoCWC <- center(couplemoodsH2$P1AllNegemo, type='CWC', cluster=couplemoodsH2$ID)
couplemoodsH2$P1PhysicalActivityCWC <- center(couplemoodsH2$P1PhysicalActivity, type='CWC', cluster=couplemoodsH2$ID)
# H3
couplemoodsH3$P1sclCWC <- center(couplemoodsH3$P1scl, type='CWC', cluster=couplemoodsH3$ID)
couplemoodsH3$P1AllWeCWC <- center(couplemoodsH3$P1AllWe, type='CWC', cluster=couplemoodsH3$ID)
couplemoodsH3$P1CloseCWC <- center(couplemoodsH3$P1Close, type='CWC', cluster=couplemoodsH3$ID)
couplemoodsH3$P1PhysicalActivityCWC <- center(couplemoodsH3$P1PhysicalActivity, type='CWC', cluster=couplemoodsH3$ID)
# H4
couplemoodsH4$P1sclCWC <- center(couplemoodsH4$P1scl, type='CWC', cluster=couplemoodsH4$ID)
couplemoodsH4$P1AllYouCWC <- center(couplemoodsH4$P1AllYou, type='CWC', cluster=couplemoodsH4$ID)
couplemoodsH4$P1CloseCWC <- center(couplemoodsH4$P1Close, type='CWC', cluster=couplemoodsH4$ID)
couplemoodsH4$P1PhysicalActivityCWC <- center(couplemoodsH4$P1PhysicalActivity, type='CWC', cluster=couplemoodsH4$ID)




### Hypothesis 1 ##########################################################
# Couples will be more similar physiologically and linguistically over the course of a day than would be expected of strangers
# H1a physiological linkage
model1a <- lmer(formula = P2scl ~ 1 + P1sclCWC + P1PhysicalActivityCWC + (1 | ID),
                data=couplemoodsH1a,
                na.action=na.exclude)
summary(model1a)
## p-value
coefs <-data.frame(coef(summary(model1a)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
## plot your result
ggplot(couplemoodsH1a, aes(x = P1sclCWC, y = P2scl)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  ggtitle("Physiological Linkage in Couples on an Hourly Basis") +
  theme(plot.title = element_text(hjust = 0.5))

# H1b linguistic similarities
## We
model1b1 <- lmer(formula = P2AllWe ~ 1 + P1AllWeCWC + (1 | ID),
                 data=couplemoodsH1b,
                 na.action=na.exclude)
summary(model1b1)
### p-value
coefs <-data.frame(coef(summary(model1b1)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
### plot your result
ggplot(couplemoodsH1b, aes(x = P1AllWeCWC, y = P2AllWe)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  ggtitle("Hourly Usage of We in Couples") +
  theme(plot.title = element_text(hjust = 0.5))
## You
model1b2 <- lmer(formula = P2AllYou ~ 1 + P1AllYouCWC + InteractingCWC + (1 | ID),
                 data=couplemoodsH1b,
                 na.action=na.exclude)
summary(model1b2)
### p-value
coefs <-data.frame(coef(summary(model1b2)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
### plot your result
ggplot(couplemoodsH1b, aes(x = P1AllYouCWC, y = P2AllYou)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "green") +
  ggtitle("Hourly Usage of You in Couples") +
  theme(plot.title = element_text(hjust = 0.5))
## Posemo
model1b3 <- lmer(formula = P2AllPosemo ~ 1 + P1AllPosemoCWC + (1 | ID),
                 data=couplemoodsH1b,
                 na.action=na.exclude)
summary(model1b3)
### p-value
coefs <-data.frame(coef(summary(model1b3)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
### plot your result
ggplot(couplemoodsH1b, aes(x = P1AllPosemoCWC, y = P2AllPosemo)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "orange") +
  ggtitle("Hourly Usage of Positive Emotion Words in Couples") +
  theme(plot.title = element_text(hjust = 0.5))
# Negemo
model1b4 <- lmer(formula = P2AllNegemo ~ 1 + P1AllNegemoCWC + (1 | ID),
                 data=couplemoodsH1b,
                 na.action=na.exclude)
summary(model1b4)
### p-value
coefs <-data.frame(coef(summary(model1b4)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
## plot your result
ggplot(couplemoodsH1b, aes(x = P1AllNegemoCWC, y = P2AllNegemo)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "purple") + 
  ggtitle("Hourly Usage of Negative Emotion Words in Couples") +
  theme(plot.title = element_text(hjust = 0.5))


### Hypothesis 2 ###########################################################
# The strength of hourly physiological linkage in couples changes depending on the linguistic context
## We
model2a <- lmer(formula = P2scl ~ 1 + P1sclCWC + P1AllWeCWC + 
                  P1sclCWC:P1AllWeCWC + 
                  P1PhysicalActivityCWC + 
                  (1 | ID),
                data=couplemoodsH2,
                na.action=na.exclude)
summary(model2a)
### p-value
coefs <-data.frame(coef(summary(model2a)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
### plot your result
### use this to get the means and standard deviations for plotting the interaction
psych::describe(couplemoodsH2$P1sclCWC) # take the mean plus and minus 1 SD and insert below for levels
psych::describe(couplemoodsH2$P1AllWeCWC) # take the mean plus and minus 1 SD and insert below for levels
### extract the simple slopes
if (require(lme4, quietly=TRUE)) {
  model2a <- lmer(P2scl ~ P1sclCWC * P1AllWeCWC + (1 | ID), data = couplemoodsH2)
  summary(model2a)
  simple_slopes(model2a)
  simple_slopes(model2a,
                levels=list(P1sclCWC = c(-4.21, 4.21, 'sstest'),
                            P1AllWeCWC = c(-1.06, 1.06, 'sstest')))  
}
### now plot
if (require(lme4, quietly=TRUE)) {
  model2a <- lmer(P2scl ~ P1sclCWC * P1AllWeCWC + (1 | ID), data = couplemoodsH2)
  graph_model(model2a, y = P2scl, x = P1sclCWC, lines = P1AllWeCWC)
}
# You
model2b <- lmer(formula = P2scl ~ 1 + P1sclCWC + P1AllYouCWC + 
                  P1sclCWC:P1AllYouCWC + 
                  P1PhysicalActivityCWC +
                  (1 | ID),
                data=couplemoodsH2,
                na.action=na.exclude)
summary(model2b)
### p-value
coefs <-data.frame(coef(summary(model2b)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
### plot your result
### use this to get the means and standard deviations for plotting the interaction
psych::describe(couplemoodsH2$P1sclCWC) # take the mean plus and minus 1 SD and insert below for levels
psych::describe(couplemoodsH2$P1AllYouCWC) # take the mean plus and minus 1 SD and insert below for levels
### extract the simple slopes
if (require(lme4, quietly=TRUE)) {
  model2b <- lmer(P2scl ~ P1sclCWC * P1AllYouCWC + (1 | ID), data = couplemoodsH2)
  summary(model2b)
  simple_slopes(model2b)
  simple_slopes(model2b,
                levels=list(P1sclCWC = c(-4.21, 4.21, 'sstest'),
                            P1AllYouCWC = c(-2.07, 2.07, 'sstest')))  
}
### now plot
if (require(lme4, quietly=TRUE)) {
  model2b <- lmer(P2scl ~ P1sclCWC * P1AllYouCWC + (1 | ID), data = couplemoodsH2)
  graph_model(model2b, y = P2scl, x = P1sclCWC, lines = P1AllYouCWC)
}
# Posemo
model2c <- lmer(formula = P2scl ~ 1 + P1sclCWC + P1AllPosemoCWC + 
                  P1sclCWC:P1AllPosemoCWC + 
                  P1PhysicalActivityCWC +
                  (1 | ID),
                data=couplemoodsH2,
                na.action=na.exclude)
summary(model2c)
### p-value
coefs <-data.frame(coef(summary(model2c)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
### plot your result
### use this to get the means and standard deviations for plotting the interaction
psych::describe(couplemoodsH2$P1sclCWC) # take the mean plus and minus 1 SD and insert below for levels
psych::describe(couplemoodsH2$P1AllPosemoCWC) # take the mean plus and minus 1 SD and insert below for levels
### extract the simple slopes
if (require(lme4, quietly=TRUE)) {
  model2c <- lmer(P2scl ~ P1sclCWC * P1AllPosemoCWC + (1 | ID), data = couplemoodsH2)
  summary(model2c)
  simple_slopes(model2c)
  simple_slopes(model2c,
                levels=list(P1sclCWC = c(-4.21, 4.21, 'sstest'),
                            P1AllPosemoCWC = c(-1.88, 1.88, 'sstest')))  
}
### now plot
if (require(lme4, quietly=TRUE)) {
  model2c <- lmer(P2scl ~ P1sclCWC * P1AllPosemoCWC + (1 | ID), data = couplemoodsH2)
  graph_model(model2c, y = P2scl, x = P1sclCWC, lines = P1AllPosemoCWC)
}
# Negemo
model2d <- lmer(formula = P2scl ~ 1 + P1sclCWC + P1AllNegemoCWC + 
                  P1sclCWC:P1AllNegemoCWC + 
                  P1PhysicalActivityCWC +
                  (1 | ID),
                data=couplemoodsH2,
                na.action=na.exclude)
summary(model2d)
### p-value
coefs <-data.frame(coef(summary(model2d)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
### plot your result
### use this to get the means and standard deviations for plotting the interaction
psych::describe(couplemoodsH2$P1sclCWC) # take the mean plus and minus 1 SD and insert below for levels
psych::describe(couplemoodsH2$P1AllNegemoCWC) # take the mean plus and minus 1 SD and insert below for levels
### extract the simple slopes
if (require(lme4, quietly=TRUE)) {
  model2d <- lmer(P2scl ~ P1sclCWC * P1AllNegemoCWC + (1 | ID), data = couplemoodsH2)
  summary(model2d)
  simple_slopes(model2d)
  simple_slopes(model2d,
                levels=list(P1sclCWC = c(-4.21, 4.21, 'sstest'),
                            P1AllNegemoCWC = c(-1.33, 1.33, 'sstest')))  
}
### now plot
if (require(lme4, quietly=TRUE)) {
  model2d <- lmer(P2scl ~ P1sclCWC * P1AllNegemoCWC + (1 | ID), data = couplemoodsH2)
  graph_model(model2d, y = P2scl, x = P1sclCWC, lines = P1AllNegemoCWC)
}



### Hypothesis 3 ###########################################################
# When couples feel a sense of togetherness, conceptualized as a higher hourly usage of we and feelings of closeness, then the strength of hourly physiological linkage will increase
## test the association between a level-1 predictor, level-1 moderator, and a level-1 outcome
model3 <- lmer(formula = P2scl ~ 1 + P1sclCWC + P1AllWeCWC + P1CloseCWC +
                 P1sclCWC:P1AllWeCWC + P1sclCWC:P1CloseCWC + P1AllWeCWC:P1CloseCWC +
                 P1sclCWC:P1AllWeCWC:P1CloseCWC + 
                 P1PhysicalActivityCWC  +
                 (1 | ID),
               data=couplemoodsH3,
               na.action=na.exclude)
summary(model3)
### p-value
coefs <-data.frame(coef(summary(model3)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
### plot your result
# use this to get the means and standard deviations for plotting the interaction
psych::describe(couplemoodsH3$P1sclCWC) # take the mean plus and minus 1 SD and insert below for levels
psych::describe(couplemoodsH3$P1AllWeCWC) # take the mean plus and minus 1 SD and insert below for levels
psych::describe(couplemoodsH3$P1CloseCWC) # take the mean plus and minus 1 SD and insert below for levels
# extract the simple slopes
if (require(lme4, quietly=TRUE)) {
  model3 <- lmer(P2scl ~ P1sclCWC * P1AllWeCWC * P1CloseCWC + (1 | ID), data = couplemoodsH3)
  summary(model3)
  simple_slopes(model3)
  simple_slopes(model3,
                levels=list(P1sclCWC = c(-4.08, 4.08, 'sstest'),
                            P1AllWeCWC = c(-1.06, 1.06, 'sstest'),
                            P1CloseCWC = c(-20.2,20.2, 'sstest')))  
}
# now plot
plot_model(model3, type = "int", mdrt.values=c("meansd"))



### Hypothesis 4 ##########################################################
# When couples use “you” more frequently and feel less close during a given hour, then the strength of physiological linkage will increase
## test the association between a level-1 predictor, level-1 moderator, and a level-1 outcome
model4 <- lmer(formula = P2scl ~ 1 + P1sclCWC + P1AllYouCWC + P1CloseCWC +
                 P1sclCWC:P1AllYouCWC + P1sclCWC:P1CloseCWC + P1AllYouCWC:P1CloseCWC +
                 P1sclCWC:P1AllYouCWC:P1CloseCWC + 
                 P1PhysicalActivityCWC +
                 (1 | ID),
               data=couplemoodsH4,
               na.action=na.exclude)
summary(model4)
### p-value
coefs <-data.frame(coef(summary(model4)))
coefs$p.z <- 2*(1-pnorm(abs(coefs$t.value)))
round(coefs, digits=3)
## plot your result
# use this to get the means and standard deviations for plotting the interaction
psych::describe(couplemoodsH4$P1sclCWC) # take the mean plus and minus 1 SD and insert below for levels
psych::describe(couplemoodsH4$P1AllYouCWC) # take the mean plus and minus 1 SD and insert below for levels
psych::describe(couplemoodsH4$P1CloseCWC) # take the mean plus and minus 1 SD and insert below for levels

# extract the simple slopes
if (require(lme4, quietly=TRUE)) {
  model4 <- lmer(P2scl ~ P1sclCWC * P1AllYouCWC * P1CloseCWC + (1 | ID), data = couplemoodsH4)
  summary(model4)
  simple_slopes(model4)
  simple_slopes(model4,
                levels=list(P1sclCWC = c(-4.08, 4.08, 'sstest'),
                            P1AllYouCWC = c(-2.05, 2.05, 'sstest'),
                            P1CloseCWC = c(-20.2,20.2, 'sstest')))  
}
# now plot
plot_model(model4, type = "int", mdrt.values=c("meansd"))
