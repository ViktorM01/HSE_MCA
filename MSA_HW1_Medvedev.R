install.packages("haven")
install.packages("plm")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lmtest")
install.packages("sandwich")

library(haven)
library(plm)
library(ggplot2)
library(dplyr)
library(lmtest)
library(sandwich)

panel<-read_dta("MSA_hw1.dta")
head(panel)
attach(panel)

ols_1 <- plm(lncrime ~ lnpolice + lndensity + nonwhite, data = panel, model="pooling")
summary(ols_1)

ols_2 <- lm(data = panel, lncrime ~ lnpolice + lndensity + nonwhite)
summary(ols_2)

county_base <- relevel(factor(county), ref ="1")
LSDV <- lm(lncrime~lnpolice + lndensity + nonwhite + county_base, data = panel)
summary(LSDV)

LSDV_transformed <- plm(lncrime~lnpolice + lndensity + nonwhite + county_base,
                        data = panel, index=c("county", "year"), model="within")
summary(LSDV_transformed)

pFtest(LSDV_transformed, ols)

re <- plm(lncrime~lnpolice + lndensity + nonwhite, 
          data = panel, index=c("county", "year"), model="random")
summary(re)

plmtest(ols, type=c("bp"))
phtest(LSDV_transformed, re) 

tyear <- relevel(factor(year), ref ="81")
LSDV_t <- lm(lncrime~lnpolice + lndensity + nonwhite + tyear)
summary(LSDV_t)

fe_twoways <- plm(lncrime~lnpolice + lndensity + nonwhite, data = panel, index=c("county", "year"), effect = "twoways", model = "within")
summary(fe_twoways)
pFtest(fe_twoways, LSDV_transformed)

pFtest(fe_twoways, LSDV_transformed)
bptest(fe_twoways) 

coeftest(fe_twoways, vcov = vcovHC, type = "HC3")

LSDV_twoways <- lm(lncrime~lnpolice + lndensity + nonwhite + as.factor(county) + as.factor(year), data = panel) 
y_pred <- LSDV_twoways$fitted 
panel_pred <- data.frame(panel, y_pred)
summary(LSDV_twoways)
summary(y_pred) 

merged <- panel_pred %>% group_by(county)%>% summarize(., cor(lncrime, y_pred))%>% merge(panel_pred, ., by="county")
head(merged)

merged$new <- ifelse(abs(merged$`cor(lncrime, y_pred)`)<0.3,1,0)
fe_twoways_2 <- plm(lncrime~lnpolice + lndensity + nonwhite, merged[merged$new == 0,], index=c("county", "year"), effect = "twoways")
coeftest(fe_twoways_2, vcov = vcovHC, type = "HC3")

a <- group_by(panel, county) %>%
  do(data.frame(beta = coef(lm( lncrime ~ lndensity, data = .))[2]))
a

var <- summarize(group_by(panel, county), var(lndensity))
m <- as.data.frame(merge(a, var, by ="county"))

m$coef <- m$beta*(m$`var(lndensity)`/sum(m$`var(lndensity)`))
sum(m$coef)

plm(data = panel, lncrime ~ lndensity, index=c("county", "year"))$coefficients
