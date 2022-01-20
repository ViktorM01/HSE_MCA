library(haven)
library(tidyverse)
library(sjlabelled)
library(stargazer)
library(ggcorrplot)
library(lme4)
library(pglm)
library(aod)
library(lmtest)
library(car)
library(InformationValue)
library(caret)
library(ResourceSelection)
library(memisc)
library(broom)
library(pscl)
library(glm.predict)
library(ggpubr)
library(nlme)
library(psych)
library(arm)
library(multilevel)
library(lattice)
library(sjPlot)
library(influence.ME)
library(ggplot2)
library(lmerTest)
library(glmmTMB)
library(influence.ME)


#
data <- read_dta(file.choose())
data_1 <- dplyr::select(data, wkhtot, wkhct, stflife, mbtru, cntry, gndr, agea, ctzcntr)
data_1[data_1$mbtru == 7 | data_1$mbtru == 8 | data_1$mbtru == 9 |
         data_1$ctzcntr == 7 | data_1$ctzcntr == 8 | data_1$ctzcntr == 9 |
         data_1 == 666 | data_1 == 777 | data_1 == 888 | data_1 == 999|
         data_1 == 55 |  data_1 == 77 | data_1 == 88 | data_1 == 99 |
       data_1 == 5555 | data_1 == 7777 | data_1 == 8888 | data_1 == 9999] <- NA

unique(data_1$cntry, incomparables = FALSE)
unique(data_1$wkhtot, incomparables = FALSE)
unique(data_1$stflife, incomparables = FALSE)

union <- c(26.3, 50.3, 18.0, 14.9, 11.5, 16.5, 66.5, 4.3, 13.6, 60.3, 8.8, 23.4, 35, 7.9, 24.5, 91.8,
           34.4, 7.1, 11.9, 16.4, 49.2, 12.7, 15.3, 65.6, 20.4, 10.7)
names(union) <- c("AT", "BE", "BG", "CH", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GB", "HR", "HU", "IE", "IS",
                         "IT", "LT", "LV", "NL", "NO", "PL", "PT", "SE", "SI", "SK")

data_2_1 <- cbind(names(union),union)
colnames(data_2_1) <- c("cntry","union")
data_1 <- merge(data_2_1, data_1, by = "cntry")

data_1 <- na.omit(data_1)
unique(data_1)
summary(data_1)
sd(data_1)

res <- cor.test(data_1$wkhtot, data_1$wkhct, 
                method = "pearson")
res

multi.hist(data_1[, sapply(data_1, is.double)])

data_1$wkhtot <- log(as.numeric(data_1$wkhtot)+1)
data_1$wkhct <- log(as.numeric(data_1$wkhct)+1)

data_1$stflife <- as.numeric(data_1$stflife)
data_1$mbtru <- as.factor(data_1$mbtru)
data_1$agea <- (data_1$agea - mean(data_1$agea))/sd(data_1$agea)
data_1$ctzcntr <- as.factor(data_1$ctzcntr)
data_1$gndr <- as.factor(data_1$gndr)
data_1$union <- as.numeric(data_1$union)

describe(data_1)

multi.hist(data_1[, sapply(data_1, is.double)])

model_0 <- lmer(stflife ~ 1 + (1|cntry), REML = TRUE, data = data_1)
summary(null)
0.03478/(0.09313+0.03478) 

ggplot(data_1, aes(x=wkhtot, y=stflife, color=cntry)) +
  geom_smooth(method=lm, se=FALSE)

#Модели

model_1 <- lmer(stflife ~ wkhct + mbtru + agea + ctzcntr + gndr + (1|cntry), REML = FALSE, data = data_1)
anova(model_0, model_1)
summary(model_1)

model_2 <- lmer(stflife ~ wkhct + mbtru + agea + ctzcntr + gndr + (1 + wkhct|cntry), REML = FALSE, data = data_1)
anova(model_1, model_2)
summary(model_2)

model_3 <- lmer(stflife ~ wkhct + mbtru + agea + ctzcntr + gndr + union + (1|cntry), REML = FALSE, data = data_1)
anova(model_2, model_3)
anova(model_1, model_3)
summary(model_3)

model_4 <- lmer(stflife ~ wkhct + mbtru + agea + ctzcntr + gndr + union + (1 + wkhct|cntry), REML = FALSE, data = data_1)
anova(model_3, model_4)
anova(model_1, model_4)
summary(model_4)

model_5 <- lmer(stflife ~ wkhct + mbtru + ctzcntr + gndr + union + wkhct:union + (1 + wkhct|cntry), REML = FALSE, data = data_1)
anova(model_1, model_5)
anova(model_3, model_5)
summary(model_5)

#Проверка на устойчивость. Переоценим модель, откинув наблюдения с экстраординарным количеством часов

data_4 <- dplyr::select(data, wkhtot, wkhct, stflife, mbtru, cntry, gndr, agea, ctzcntr)
data_4[data_4$mbtru == 7 | data_4$mbtru == 8 | data_4$mbtru == 9 |
         data_4$ctzcntr == 7 | data_4$ctzcntr == 8 | data_4$ctzcntr == 9 |
         data_4 == 666 | data_4 == 777 | data_4 == 888 | data_4 == 999|
         data_4 == 55 |  data_4 == 77 | data_4 == 88 | data_4 == 99 |
         data_4 == 5555 | data_4 == 7777 | data_4 == 8888 | data_4 == 9999] <- NA

unique(data_4$cntry, incomparables = FALSE)
unique(data_4$wkhtot, incomparables = FALSE)
unique(data_4$stflife, incomparables = FALSE)

data_4 <- merge(data_2, data_4, by = "cntry")
data_4 <- merge(data_2_1, data_4, by = "cntry")
data_4 <- na.omit(data_4)

data_4 <- data_4[!data_4$wkhtot > 60,]
data_4 <- data_4[!data_4$wkhct > 60,]
data_4$wkhtot <- log(as.numeric(data_4$wkhtot)+1)
data_4$wkhct <- log(as.numeric(data_4$wkhct)+1)

data_4$stflife <- as.numeric(data_4$stflife)
data_4$mbtru <- as.factor(data_4$mbtru)
data_4$agea <- (data_4$agea - mean(data_4$agea))/sd(data_4$agea)
data_4$ctzcntr <- as.factor(data_4$ctzcntr)
data_4$gndr <- as.factor(data_4$gndr)
data_4$union <- as.numeric(data_4$union)

model_0a <- lmer(stflife ~ 1 + (1|cntry), REML = TRUE, data = data_4)
summary(null)

model_1a <- lmer(stflife ~ wkhct + mbtru + agea + ctzcntr + gndr + (1|cntry), REML = FALSE, data = data_4)
anova(model_0a, model_1a)
summary(model_1a)

model_2a <- lmer(stflife ~ wkhct + mbtru + agea + ctzcntr + gndr + (1 + wkhct|cntry), REML = FALSE, data = data_4)
anova(model_1a, model_2a)
summary(model_2a)

model_3a <- lmer(stflife ~ wkhct + mbtru + agea + ctzcntr + gndr + union + (1|cntry), REML = FALSE, data = data_4)
anova(model_2a, model_3a)
anova(model_1a, model_3a)
summary(model_3a)

model_4a <- lmer(stflife ~ wkhct + mbtru + agea + ctzcntr + gndr + union + (1 + wkhct|cntry), REML = FALSE, data = data_4)
anova(model_3a, model_4a)
summary(model_4a)

model_1b <- lmer(stflife ~ wkhtot + mbtru + agea + ctzcntr + gndr + (1|cntry), REML = FALSE, data = data_4)
anova(model_0a, model_1b)
summary(model_1a)

model_2b <- lmer(stflife ~ wkhtot + mbtru + agea + ctzcntr + gndr + (1 + wkhtot|cntry), REML = FALSE, data = data_4)
anova(model_1b, model_2b)
summary(model_2b)

model_3b <- lmer(stflife ~ wkhtot + mbtru + agea + ctzcntr + gndr + union + (1|cntry), REML = FALSE, data = data_4)
anova(model_2b, model_3b)
anova(model_1b, model_3b)
summary(model_3b)

model_4b <- lmer(stflife ~ wkhct + mbtru + agea + ctzcntr + gndr + union + (1 + wkhct|cntry), REML = FALSE, data = data_4)
anova(model_3a, model_4b)
summary(model_4b)

#Диагностики. Проверка на влиятельные наблюдения
model_3c <- lmer(stflife ~ wkhct + agea + mbtru + ctzcntr + gndr + union + (1|cntry), data = data_1)
summary(model_3c)

estex.model_3c <- influence(model_3c, "cntry")
dfbetas(estex.model_3c)

plot(estex.model_3c,
     which="dfbetas",
     xlab = "DFbetaS",
     ylab = "cntry")

cooks.distance(estex.model_3c, sort=TRUE)
plot(estex.model_3c, which="cook",
     cutoff=.15, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="cntry")

model_3cc <- exclude.influence(model_3c, "cntry", c("IE", "BG", "LT"))
summary(model_3cc)

model_3ca <- lmer(stflife ~ wkhct + mbtru + ctzcntr + gndr + union + (1|cntry), REML = FALSE, data = data_1)
anova(model_2, model_3)
anova(model_1, model_3)
summary(model_3ca)
