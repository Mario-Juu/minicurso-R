### Configurar pasta de trabalho e Importar "Computers.xlsx" (renomeando para "dados")
# install.packages("editData")    #Instala o Addin para editar os dados no próprio RStudio (Não está mais funcionando)

################### Estatística descritiva #########################
library(pastecs)
library(xtable)

desc <- stat.desc(dados)   # Geral
desc_price <- stat.desc(dados[,2])    # Apenas para "Price"
desc_price <- format(desc_price, scientific = FALSE, digits=2, nsmall=2)

### Gráficos
# Colunas (premium)
cont <- table(dados$premium)
barplot(cont, main="Contagem de ocorrências 'premium' no dataset 'Computers'", xlab="Marca 'premium'? (IBM, COMPAQ)", ylab="Frequência")

setEPS()             # Exportar vetorialmente (útil para LaTeX)
postscript("barplot.eps")
barplot(cont, main="Contagem de ocorrências 'premium' no dataset 'Computers'", xlab="Marca 'premium'? (IBM, COMPAQ)", ylab="Frequência")
dev.off()

# Pizza (premium)
pie(cont, labels = c("No", "Yes"), main="Contagem de ocorrências 'premium' no dataset 'Computers'")

# Dispersão 
plot(dados$hd, dados$price, main="Diagrama de Dispersão", xlab="HD (Mb)", ylab="Preço (USD)", pch=19)
plot(dados$ram, dados$price, main="Diagrama de Dispersão", xlab="RAM (Mb)", ylab="Preço (USD)", pch=19)
plot(dados$speed, dados$price, main="Diagrama de Dispersão", xlab="Speed (mHz)", ylab="Preço (USD)", pch=19)

### Distribuição de frequências para a variável "Price"
sturges <- 1+3.3*log10(nrow(dados[,2]))
range <- max(dados$price)-min(dados$price)
amp <- range/sturges
amp5 <- 5*round(amp/5)       # Garante que a amplitude fique múltipla de 5
xmin <- 5*floor(min(dados$price)/5)       # Garante que o menor limite inferior fique abaixo do mínimo amostral
xmax <- 5*round(max(dados$price)/5)
limites <- seq(xmin, xmax+amp5, by=amp5)
cortes_aux <- cut(dados$price, limites, right=FALSE, dig.lab=4)
price_freq <- as.data.frame(cbind(table(cortes_aux)))   #Data frame facilita a manipulação

for(i in 1:nrow(price_freq)) {
  price_freq[i,2] <- round(price_freq[i,1]/sum(price_freq[,1]), digits = 4)
  price_freq[i,3] <- price_freq[i,2]*100
}

price_freq[1,4] <- price_freq[1,1] 
price_freq[1,5] <- price_freq[1,2]
price_freq[1,6] <- price_freq[1,3]

for(i in 2:nrow(price_freq)) {
  price_freq[i,4] <- price_freq[i,1]+price_freq[i-1,4]
  price_freq[i,5] <- round(price_freq[i,4]/sum(price_freq[,1]), digits = 4)
  price_freq[i,6] <- price_freq[i,5]*100
}

colnames(price_freq) <- c("f","f_rel","f_rel_%","Fac","Fac_rel","Fac_rel_%")

# Exportar para LaTeX
print(xtable(price_freq, type = "latex"), sanitize.rownames.function = function(x) paste0("{",x,"}"), file = "Price_Dist_Freq.tex")

### Histograma e Normalidade
# Para a distribuição de frequência
par(las=2); par(mar=c(5,8,4,2))
barplot(price_freq$f, names.arg=rownames(price_freq), horiz=FALSE, cex.names=0.8,
        main="Histograma para a distribuição de frequencias", ylab="Frequência absoluta")

# Para os dados brutos
hist(dados$price, main="Histograma para os dados brutos", xlab="Preço (USD)", ylab="Frequência absoluta")

# Distribuição Normal
hist(dados$price, prob=TRUE)
curve(dnorm(x, mean=mean(dados$price), sd=sd(dados$price)), add=TRUE)

### Boxplot
boxplot(dados[,2],data=dados, main="Boxplot - Preço dos Computadores", xlab="Preço", ylab="USD", outline=FALSE, horizontal=FALSE)

###################### Inferência ##########################

### Intervalos de Confiança
t.test(dados$price, alternative="two.sided", conf.level = 0.95)$conf.int

### Comparação entre os preços dos PCs "premium" e "não premium". São diferentes?
# Exploração inicial (boxplots)
boxplot(dados$price ~ dados$premium, main="Preços Premium vs Não Premium", xlab="Premium", ylab="USD", outline=FALSE)

# Teste t
var.test(dados$price ~ dados$premium)
t.test(dados$price ~ dados$premium, var.equal=FALSE)

### Regressão
# Simples (HD vs Preço)
# Analisando a relação com diagrama de dispersão
scatter.smooth(x=dados$hd, y=dados$price)
r <- cor(x=dados$hd, y=dados$price)
reg <- lm(dados$price ~ dados$hd)
summary(reg)

# Múltipla
reg_multi <- lm(dados$price ~ dados$speed + dados$hd + dados$ram + dados$screen)
summary(reg_multi)

#Eficiência relativa
summary(reg_multi)$r.squared/summary(reg)$r.squared

# Exemplo de predição - 1º dado do conjunto de dados
ref <- dados$price[1]
pred_simples <- predict(reg, dados)[1]
pred_multi <- predict(reg_multi, dados)[1]

##################### Predição ############################
### Random Forest
library(caret)
library(randomForest) 

dados <- as.data.frame(dados)      #Tipagem para random_index
set.seed(sample(1:1000000, 1))
random_index <- createDataPartition(dados[,2], p = 0.7, list = FALSE)
dados_treino <- dados[random_index,]; table(dados_treino$premium)
dados_teste <- dados[-random_index,]; table(dados_teste$premium)
rf_model <- randomForest(factor(premium) ~ ., data=dados_treino, controls = cforest_unbiased(ntree=1000, mtr=2))     #Dá (dava) erro

dados_treino$multi <- factor(dados_treino$multi); dados_treino$cd <- factor(dados_treino$cd)     #O erro é por causa de variáveis que deveriam ser do tipo factor
dados_teste$multi <- factor(dados_teste$multi); dados_teste$cd <- factor(dados_teste$cd)

rf_model <- randomForest(factor(premium) ~ ., data=dados_treino, controls = cforest_unbiased(ntree=1000, mtr=2)) 
rf_pred <- predict(rf_model, dados_teste)
rf_tab_acuracia <- table(rf_pred, dados_teste$premium)
rf_acuracia <- 1-((rf_tab_acuracia[2,1] + rf_tab_acuracia[1,2])/nrow(dados_teste))
paste("Acurácia da classificação (Random Forest):", round(rf_acuracia,2))

### k-Nearest Neighbors (Com exemplo de paralelismo?)
library(parallel)
library(doSNOW)
library(e1071)

start_time <- Sys.time()
knn_model <- train(factor(premium) ~ ., data=dados_treino, method = "knn", trControl = trainControl(method="repeatedcv", repeats = 3), tuneLength = 20)
end_time <- Sys.time()
end_time - start_time

clusters <- makeCluster(detectCores()); registerDoSNOW(clusters)
start_time <- Sys.time()
knn_model <- train(factor(premium) ~ ., data=dados_treino, method = "knn", trControl = trainControl(method="repeatedcv", repeats = 3), tuneLength = 20)
end_time <- Sys.time()
end_time - start_time

unregister <- function(clusters) {       # Função para desconectar os clusters
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

unregister(); rm(clusters)
  
knn_pred <- predict(knn_model, dados_teste)
knn_tab_acuracia <- table(knn_pred, dados_teste$premium)
knn_acuracia <- 1-((knn_tab_acuracia[2,1] + knn_tab_acuracia[1,2])/nrow(dados_teste))
paste("Acurácia da classificação(k-Nearest Neighbors):", round(knn_acuracia,2))

############### Análise componentes principais ############
library(FactoMineR)

princ_comp <- PCA(dados[-c(1,7:9)], scale.unit=TRUE, ncp=4, graph=TRUE)
source('graph.var.paolo.R')
graph.var.mod(princ_comp, title="Principal Component Analysis", new.plot = FALSE, cex=1.5, cex.lab=1.5)

############### Objetos e uso da memória #########################
library(gdata)

Memory <- ll()
Memory[,2] <- round((Memory[,2]/1024),2)
colnames(Memory) <- c("Class", "Mb")
Memory
