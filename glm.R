## REFERÊNCIA: https://stats.stackexchange.com/questions/308811/interpret-glm-model-in-test-of-difference-in-proportions

library(readxl)
library(multcomp)
library(plyr)

dados <- read_excel("Computers.xlsx", col_types = c("skip", "skip", "skip", "skip", "skip", "skip", "text", "skip", "text", "skip"))
dados$cd <- as.factor(dados$cd); dados$premium <- as.factor(dados$premium)
table(dados)

dados$cd  <- revalue(dados$cd , c("yes"=1))
dados$cd  <- revalue(dados$cd , c("no"=0))

model <- glm(cd ~ premium, family = "binomial", data = dados)
summary(model)

exp(coef(model)) #odds ratio (pesquisar sobre a necessidade)
exp(confint(model)) # IC

compare <- glht(model, linfct = mcp(premium = "Tukey"))
summary(compare)