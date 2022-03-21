#
library(readxl)
dados <- read_excel("dados.xlsx")
dados$Bolsonaro18 <- 100*dados$Bolsonaro18
dados$Neves14 <- 100*dados$Neves14
summary(dados)
dt <- subset(dados, select=c(Bolsonaro18, Neves14, Serra10, Alckmin06,Serra02))
matcor <- cor(dt)
print(matcor, digits = 2)
require(corrplot)
corrplot(matcor, method="circle")
corrplot(matcor, method="number", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)
plot(dados$Serra02, dados$log_pop)
cor.test(dados$Serra02, dados$log_pop)
plot(dados$Alckmin06, dados$log_pop)
cor.test(dados$Alckmin06, dados$log_pop)
plot(dados$Serra10, dados$log_pop)
cor.test(dados$Serra10, dados$log_pop)
plot(dados$Neves14, dados$log_pop)
cor.test(dados$Neves14, dados$log_pop)
plot(dados$Bolsonaro18, dados$log_pop)
cor.test(dados$Bolsonaro18, dados$log_pop)
plot(dados$Serra02, dados$Alckmin06)
plot(dados$Serra02, dados$Serra10)
plot(dados$Serra02, dados$Neves14)
plot(dados$Serra02, dados$Bolsonaro18)
plot(dados$Alckmin06, dados$Serra10)
plot(dados$Alckmin06, dados$Neves14)
plot(dados$Alckmin06, dados$Bolsonaro18)
cor.test(dados$Alckmin06, dados$Serra10)#
plot(dados$Serra10, dados$Neves14)
plot(dados$Serra10, dados$Bolsonaro18)
plot(dados$Neves14, dados$Bolsonaro18)
cor.test(dados$Neves14, dados$Bolsonaro18)#
dados$ranking <- dados$Neves14 + dados$Bolsonaro18 + dados$Serra02 + dados$Alckmin06 + dados$Serra10
hist(dados$ranking)
cor(dados$ranking, dados$log_pop)
#normalizar ranking
library(caret)
epa32 <- preProcess(dados[,c(8)], method = c("range"))
norm2 <- predict(epa32,dados[,c(8)])
summary(norm2)
cor(dados$ranking, norm2$ranking)
dados$ranking <- norm2$ranking
rm(epa32, norm2)
# os mais antipetistas da história 
library(tidyverse)
library(knitr)
library(kableExtra)
b5 <- dados %>% 
  dplyr::select(CIDADE, ranking) %>% 
  arrange(desc(ranking))
b5 %>%
  kbl(caption = "Cidades mais antipetistas 2002-2020") %>%
  kable_classic(full_width = F, html_font = "Garamond")
# ranking dando maior peso para eleições mais recentes

dados$ranking_ponderado <- (5*dados$Bolsonaro18)+(4*dados$Neves14)+(3*dados$Serra10)+
  (2*dados$Alckmin06)+(dados$Serra02)
summary(dados$ranking_ponderado)
epa32 <- preProcess(dados[,c(9)], method = c("range"))
norm2 <- predict(epa32,dados[,c(9)])
summary(norm2)
cor(dados$ranking_ponderado, norm2$ranking_ponderado)
dados$ranking_ponderado <- norm2$ranking_ponderado
rm(epa32, norm2)
b6 <- dados %>% 
  dplyr::select(CIDADE, ranking_ponderado) %>% 
  arrange(desc(ranking_ponderado))
b6 %>%
  kbl(caption = "Cidades mais antipetistas 2002-2020") %>%
  kable_classic(full_width = F, html_font = "Garamond")

cor(dados$ranking, dados$ranking_ponderado)#muito forte nao necessita usar o ponderado
plot(dados$ranking, dados$ranking_ponderado)
plot(dados$ranking, dados$log_pop)
plot(dados$ranking_ponderado, dados$log_pop)


cor.test(dados$ranking_ponderado, dados$log_pop)

# maior q 50%
dados$pleito02 <- dados$Serra02 > 50
table(dados$pleito02)
dados$pleito06 <- dados$Alckmin06 > 50
table(dados$pleito06)
dados$pleito10 <- dados$Serra10 > 50
table(dados$pleito10)
dados$pleito14 <- dados$Neves14 > 50
table(dados$pleito14)
dados$pleito18 <- dados$Bolsonaro18 > 50
table(dados$pleito18)
dt2 <- dados
dt2
dt2 <- dt2 %>% 
  pivot_longer(
    cols = Neves14:Serra10, # as colunas desse intervalo
    names_to = "pleito", # terão seus nomes armazenados nessa nova coluna
    names_prefix = "pleito_", # pegar apenas os nomes que vem depois de 'ano_'
    values_to = "votacao") # e os seus valores armazenados nessa nova coluna


dt2
dt2 <-subset(dt2,select=c(CIDADE,log_pop,ranking,votacao,pleito))
dt2
summary(dt2)
hist(dt2$votacao)
cor(dt2$votacao, dt2$log_pop)
library(scales)
dt3 <- dt2
dt2$maiorq50 <- dt2$votacao>50
dt3$votacao <- dt3$votacao/100
means <- aggregate(votacao ~  pleito, dt3, mean)
ggplot(dt3, aes(x=pleito, y=votacao, fill=pleito)) + 
  geom_boxplot() +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3, show.legend=FALSE) + 
  geom_text(data = means, aes(label = percent(votacao, accuracy=0.1), y = votacao-0.02))

# as tres mais e as tres menos
b5 %>%
  kbl(caption = "Cidades mais antipetistas 2002-2020") %>%
  kable_classic(full_width = F, html_font = "Garamond")

dt4 <- subset(dt2, CIDADE == "BRACO DO TROMBUDO"|CIDADE =="RIO DO SUL"|
                CIDADE == "TAIO"|CIDADE =="JOSE BOITEUX"| CIDADE == "DONA EMMA"|
                CIDADE =="VITOR MEIRELES", select = c(CIDADE, votacao,pleito))
dt4
ord <- c("BRACO DO TROMBUDO","RIO DO SUL","TAIO","JOSE BOITEUX","VITOR MEIRELES","DONA EMMA")
f1 <- factor(dt4$CIDADE, levels =ord)
bar <- ggplot(dt4, aes(f1, votacao, fill=pleito))
bar + geom_bar(stat = "identity") + xlab("As 3 mais e as 3 menos antipetistas 2002-2018") + ylab("Score antipetista")


# reg
dt2$pleito <- as.factor(dt2$pleito)
dt2$pleito <- relevel(dt2$pleito, "Bolsonaro18")
model1 <- lm(votacao ~ pleito, data=dt2)
summary(model1)
obj <- model1
model2 <- lm(votacao ~ pleito + log_pop,data= dt2)
summary(model2)
library(coefplot) # to edit - https://cran.r-project.org/web/packages/coefplot/coefplot.pdf
# esse tb https://rdrr.io/cran/fixest/man/coefplot.html
coefplot.lm(model2, intercept = FALSE)
theCI2 <- coefplot:::buildModelCI(model2)
coefplot::buildPlottingPloty.default(theCI2)          
theCI2 # para ver o que gera
The <- theCI2[-1, ]
The
The <- The[-5, ]# sem log_pop
The
obj2 <- The
coefplot(The, interactive=TRUE, color='red',zeroColor = "blue")
coefplot(The, interactive=FALSE, color='red',zeroColor = "blue")
coefplot(model2, interactive=TRUE, intercept=FALSE,color='green')
coefplot(model2, interactive=FALSE, intercept=FALSE,color='green')
summary(model2)

#só os casos menor que 50
dt5 <- subset(dt2, maiorq50==FALSE, select=c(CIDADE,log_pop,ranking,votacao,pleito,maiorq50))
table(dt5$maiorq50)
plot(dt5$pleito,xlab="Eleição",ylab="N",main="Antipetismo menos de 50% dos votos no turno2")
#COMPARAÇÃO POR PLEITO MAIOR QUE 50
ordi <- c("Serra02","Alckmin06","Serra10","Neves14","Bolsonaro18")
f2 <- factor(dt2$pleito, levels =ordi)
dt2$maiorq50 <-as.factor(dt2$maiorq50)
levels(dt2$maiorq50) <- c("Menos que 50%", "Mais que 50%")
levels(dt2$maiorq50)
g <- ggplot(dt2) + aes(x=f2,fill=maiorq50) + geom_bar(position = "fill") 
g <- g + xlab("") + ylab("Proporção") + scale_fill_discrete(name="Mais que a metade?")
g
table(dados$pleito02)#paraconferir _ serra 02 foi mal mesmo
mean(dados$Serra02)#paraconferir
#comparador de médias
bar.m <- ggplot(dt2, aes(f2,votacao, fill=pleito))
bar.m + geom_bar(stat = "summary", fun = "mean",fill = "steelblue") + scale_fill_discrete(name="Pleito") +
  xlab("Ano")+ ylab("média Alto Vale") + stat_summary(aes(label=round(..y.., 2)),
                                                      fun = mean, geom = "text", size=4, vjust=6, color = "black") +
  labs(title="Médias de votação Antipetista",subtitle = "Cidades Alto Vale 2002-2018",
       caption = "Fonte: TSE") + theme_bw() + theme(text = element_text(size = 10)) + stat_summary(fun.data = mean_se, geom="errorbar", width = 0.5)

#
28/7#4 gráficos de 7 cidades cada divididas em quartis populacionais
dt2$quartil<- ntile(dt2$log_pop, 4)
dt2$quartil <- as.factor(dt2$quartil)
levels(dt2$quartil)
dados$quartil<- ntile(dados$log_pop, 4)
dados$quartil <- as.factor(dados$quartil)
levels(dados$quartil)


b21 <- dados %>% 
  dplyr::select(CIDADE,quartil) %>% 
  arrange(quartil)
b21 %>%
  kbl(caption = "Cidades mais antipetistas 2002-2020") %>%
  kable_classic(full_width = F, html_font = "Garamond")
#quartil1
dt21 <- subset(dt2, quartil == 1, select = c(CIDADE, votacao,pleito))
dt21$votacao <- dt21$votacao/100
means <- aggregate(votacao ~  CIDADE, dt21, mean)
ggplot(dt21, aes(x=CIDADE, y=votacao, fill=CIDADE)) + 
  geom_boxplot() +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3, show.legend=FALSE) + labs(title="Votação antipetista nas cidades do quartil 1 populacional",
                                                           subtitle="Alto Vale Segundo turno 2002-2018")+
  geom_text(data = means, aes(label = percent(votacao, accuracy=0.1), y = votacao-0.02))
#quartil2
dt22 <- subset(dt2, quartil == 2, select = c(CIDADE, votacao,pleito))
dt22$votacao <- dt22$votacao/100
means <- aggregate(votacao ~  CIDADE, dt22, mean)
ggplot(dt22, aes(x=CIDADE, y=votacao, fill=CIDADE)) + 
  geom_boxplot() +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3, show.legend=FALSE) + labs(title="Votação antipetista nas cidades do quartil 2 populacional",
                                                           subtitle="Alto Vale Segundo turno 2002-2018")+
  geom_text(data = means, aes(label = percent(votacao, accuracy=0.1), y = votacao-0.02))
#quartil3
dt23 <- subset(dt2, quartil == 3, select = c(CIDADE, votacao,pleito))
dt23$votacao <- dt23$votacao/100
means <- aggregate(votacao ~  CIDADE, dt23, mean)
ggplot(dt23, aes(x=CIDADE, y=votacao, fill=CIDADE)) + 
  geom_boxplot() +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3, show.legend=FALSE) + labs(title="Votação antipetista nas cidades do quartil 3 populacional",
                                                           subtitle="Alto Vale Segundo turno 2002-2018")+
  geom_text(data = means, aes(label = percent(votacao, accuracy=0.1), y = votacao-0.02))

votacao <- dt2$votacao
log_pop <- dt2$log_pop
plot(votacao,log_pop)
model <-lm(votacao ~ h2222)
summary(model)
h2222 <- log_pop*log_pop
plot(votacao, h2222)
rm(model)
model <- lm(votacao ~ pleito + log_pop + pleito*log_pop, data = dt2)
summary(model)
rm(model)

#quartil3
dt24 <- subset(dt2, quartil == 4, select = c(CIDADE, votacao,pleito))
dt24$votacao <- dt24$votacao/100
means <- aggregate(votacao ~  CIDADE, dt24, mean)
ggplot(dt24, aes(x=CIDADE, y=votacao, fill=CIDADE)) + 
  geom_boxplot() +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3, show.legend=FALSE) + labs(title="Votação antipetista nas cidades mais populosas",
                                                           subtitle="Alto Vale Segundo turno 2002-2018")+
  geom_text(data = means, aes(label = percent(votacao, accuracy=0.1), y = votacao-0.02))

#dif
dados$dif06 <- dados$Alckmin06 - dados$Serra02
summary(dados$dif06)
dados$dif10 <- dados$Serra10 - dados$Alckmin06
summary(dados$dif10)
dados$dif14 <- dados$Neves14 - dados$Serra10
summary(dados$dif14)
dados$dif18 <- dados$Bolsonaro18 - dados$Neves14
summary(dados$dif18)

dt33 <- subset(dados, select=c(dif06, dif10, dif14, dif18))
matcor <- cor(dt33)
print(matcor, digits = 2)
require(corrplot)
corrplot(matcor, method="circle")
corrplot(matcor, method="number", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)
#fraco
plot(dados$dif14, dados$dif18)
plot(dados$dif10, dados$dif14)
#fraco


dt2
summary(dt2$log_pop)
summary(dados$log_pop)


by(dt2$votacao, dt2$quartil, mean)

y6y <- ggplot(dt2, aes(quartil, votacao))
y6y + geom_boxplot() +  xlab("quartil de população") +
  ylab("votação antipetista 2002-2018")

# dt 24 - as 7 mais populosas
dt24
summary(dt24)
dt24 <- subset(dt2, quartil == 4, select = c(CIDADE, votacao,pleito,log_pop,ranking))
dt24
plot(dt24$votacao, dt24$log_pop)
plot(dados$Bolsonaro18, dados$log_pop)
plot(dt24$ranking, dt24$log_pop)

#...

#opa outlier
plot(dt2$ranking, dt2$log_pop)#ops
#opa retirar braço e rio do sul
dt556 <-dados
dt556 <- dt556[-c(5),] # rm braço
dt556 <- dt556[-c(20),] # rm rsl
print(dt556) 
epa32 <- preProcess(dt556[,c(8)], method = c("range"))
norm2 <- predict(epa32,dt556[,c(8)])
summary(norm2)
cor(dt556$ranking, norm2$ranking)
dt556$ranking <- norm2$ranking
rm(epa32, norm2)
dt558 <- dt556 %>% 
  pivot_longer(
    cols = Neves14:Serra10, # as colunas desse intervalo
    names_to = "pleito", # terão seus nomes armazenados nessa nova coluna
    names_prefix = "pleito_", # pegar apenas os nomes que vem depois de 'ano_'
    values_to = "votacao") # e os seus valores armazenados nessa nova coluna


summary(dt558$pleito)
table(dt558$pleito)
dt558 <-subset(dt558,select=c(CIDADE,log_pop,ranking,votacao,pleito))
print(dt558)
plot(dt2$ranking, dt2$log_pop)#ops
plot(dt558$ranking, dt558$log_pop)

h6 <- lm(ranking ~ log_pop, data=dt558)
summary(h6)
h7 <- lm(ranking~log_pop + CIDADE, data=dt24)
summary(h7)
coefplot(h7,intercept=FALSE)
coefplot(h6)


# outra coisa
table(dt2$quartil)
model_3555 <- lm(votacao ~quartil, data=dt2)
summary(model_3555)
coefplot(model_3555, interactive=TRUE, intercept=FALSE,color='red')
# outra coisa
dt2$rm <- as.numeric(dt2$quartil)
dt2$quartil4 <- dt2$rm > 3
table(dt2$quartil4)
model_3555_acasadela <- lm(votacao ~quartil4, data=dt2)
summary(model_3555_acasadela)
coefplot(model_3555_acasadela, interactive=TRUE, intercept=FALSE,color='blue')
library(sjPlot)
tab_model(model_3555_acasadela)


#repetir outras coisas no modelo sem outliers

dt558$quartil<- ntile(dt558$log_pop, 4)
dt558$quartil <- as.factor(dt558$quartil)
levels(dt558$quartil)
model_3555_b <- lm(votacao ~quartil, data=dt558)
summary(model_3555_b)
coefplot(model_3555_b, interactive=TRUE, intercept=FALSE,color='red')
#
dt558$rm <- as.numeric(dt558$quartil)
dt558$quartil4 <- dt558$rm > 3
table(dt558$quartil4)
model_3555_acasadela_b <- lm(votacao ~quartil4, data=dt558)
summary(model_3555_acasadela_b)
coefplot(model_3555_acasadela_b, interactive=TRUE, intercept=FALSE,color='blue')
tab_model(model_3555_acasadela_b)

# mais dados
dt666 <- subset(dados, select=c(CIDADE, ranking, log_pop,Serra02,Neves14,Bolsonaro18,Alckmin06,Serra10,quartil))
dt666
write.csv(dt666,"D:/ATUALIZA_PASTA_d/A Nova pasta/Antipetismo no Alto Vale 02_18/maisdados.csv", row.names = FALSE)
# lá nesse arquivo(codificado em xls), adicionarei mais dados de a\nálises anteriores de meu site
library(readxl)
maisdados <- read_excel("maisdados.xls")
rm(b21,b6,bar,bar.m,dados,dt,dt2,dt21,dt22,dt23,dt24,dt3,dt4,dt5,dt556,dt558,dt666,g,h6,h7,matcor,means)
rm(b5,dt33,model_3555,model_3555_acasadela,model_3555_b,model_3555_acasadela_b,model1,model2,The,theCI2,y6y)
rm(f1,f2,h2222,log_pop,ord,ordi,votacao)
dados <- maisdados
rm(maisdados)
dt2 <- dados
dt2
dt2 <- dt2 %>% 
  pivot_longer(
    cols = Serra02:Serra10, # as colunas desse intervalo
    names_to = "pleito", # terão seus nomes armazenados nessa nova coluna
    names_prefix = "pleito_", # pegar apenas os nomes que vem depois de 'ano_'
    values_to = "votacao") # e os seus valores armazenados nessa nova coluna


dt2
summary(dt2)
dt2$votacao <- as.numeric(dt2$votacao)
dt2$log_pop <- as.numeric(dt2$log_pop)
dt2$ranking <- as.numeric(dt2$ranking)
dt2$pleito <- as.factor(dt2$pleito)
dt2$pleito <- relevel(dt2$pleito, "Bolsonaro18")
summary(dt2)
#normalizar
epa32 <- preProcess(dt2[,c(14)], method = c("range"))
norm2 <- predict(epa32,dt2[,c(14)])
summary(norm2)
cor(dt2$votacao, norm2$votacao)#pra conferir
dt2$dep <- norm2$votacao
rm(epa32, norm2)
boxplot(dt2$votacao)
boxplot(dt2$dep)
dt2$dep <- 100*dt2$dep
boxplot(dt2$dep)
summary(dt2$dep)
hist(dt2$dep)
#modelos
model_pol <- lm(dep ~ Lula_2006+pleito, data=dt2)
summary(model_pol)
model_demog <- lm(dep ~ log_pop +idh_mun+rural_2010+evangelico+idosos+superior_comp, data=dt2)
summary(model_demog)
modelo_full <- lm(dep~Lula_2006+pleito+log_pop +idh_mun+rural_2010+evangelico+idosos+superior_comp, data=dt2)
summary(modelo_full)
library(huxtable)
huxreg(model_pol, model_demog, modelo_full)
theCI2 <- coefplot:::buildModelCI(modelo_full)
coefplot::buildPlottingPloty.default(theCI2)          
theCI2 # para ver o que gera
coefplot(modelo_full, interactive=TRUE, intercept=FALSE,color='green')
coefplot(modelo_full, interactive=FALSE, intercept=FALSE,color='green')
coefplot(obj, interactive=TRUE, intercept=FALSE,color="red")


# neves e Bolsonaro 1000 vezes

# simulacao
dados <- read_excel("dados.xlsx")
dados$Bolsonaro18 <- 100*dados$Bolsonaro18
dados$Neves14 <- 100*dados$Neves14
dados$Bolsonaro18
sd(dados$Bolsonaro18)
bols <- rnorm(n = 1000, mean = c(81.46, 87.19, 83.61, 82.96, 87.73, 69.20,
                                  72.89, 82.77, 76.64, 83.94, 67.84, 83.67, 83.32, 75.26,
                                  81.98, 82.16, 83.80, 78.69, 77.97, 75.96, 82.94, 79.00, 71.60,
                                  84.00, 83.44, 75.58, 68.89, 77.21), sd = 5.523)
dados$Neves14
sd(dados$Neves14)
neves <- rnorm(n = 1000, mean = c(59.86, 64.61, 66.43, 60.46, 74.42, 53.53, 55.27, 66.50, 49.90, 65.93, 57.24,
                                  65.62, 66.65, 58.02, 57.04, 65.31, 64.19, 63.84, 65.95, 57.61, 73.26, 60.32,
                                  58.78, 68.15, 67.71, 48.77, 48.33, 54.94), sd = 6.08)
hist(dados$Bolsonaro18)
hist(dados$Neves14)
cor(dados$Neves14, dados$Bolsonaro18)
hist(bols)
hist(neves)
Bolsonaro <- dados$Bolsonaro18
Neves <- dados$Neves14
dt <- data.frame(neves,bols)
dt8 <-data.frame(Neves,Bolsonaro)
cor(Neves,Bolsonaro)
cor(neves,bols)
cor.test(Neves,Bolsonaro)
cor.test(neves,bols)
summary(dt)
summary(dt8)
dt$Neves <- dt$neves
mod1 <- lm(Bolsonaro ~ Neves, data=dt8)
mod2 <- lm(bols ~ Neves, data=dt)
huxreg(mod1, mod2)

plot(Neves, Bolsonaro)
plot(neves, bols)


dis <- ggplot(dt, aes(bols, Neves))
dis<- dis + geom_point()
dis
dis + geom_smooth(method = lm)
dis + geom_smooth(method = lm, se = F)

dis2 <- ggplot(dt8, aes(Bolsonaro,Neves))
dis2<- dis2 + geom_point()
dis2
dis2 + geom_smooth(method = lm)
dis2 + geom_smooth(method = lm, se = F)


# comparador de modelos - mod 1 mod 2 modelo_full, modelo_demog, modelo_pol, obj
#install.packages("AICcmodavg")
library(AICcmodavg)
models <- list(mod1,mod2)
aictab(cand.set=models)#tem q ter as mesmsas variáveis 
models <- list(modelo_full,model_demog,model_pol)
aictab(cand.set=models)


# comparados ntile Neves e Bolsonaro (real e fictítico)

dt8$d2014 <- ntile(dt8$Neves, 3)
dt8$d2014  <- as.factor(dt8$d2014)
levels(dt8$d2014)
dt8$d2018 <- ntile(dt8$Bolsonaro, 2)
dt8$d2018  <- as.factor(dt8$d2018)
levels(dt8$d2018)
dt$d2014 <- ntile(dt$Neves, 3)
dt$d2014  <- as.factor(dt$d2014)
levels(dt$d2014)
dt$d2018 <- ntile(dt$bols, 2)
dt$d2018  <- as.factor(dt$d2018)
levels(dt$d2018)



#normal
# TESTE GAMMA
#exemplo abaixo
#tab <- table(Lapop2019$ConfSTF, Lapop2019$ed)
library(vcdExtra)
#chisq.test(tab) # para o p-valor rodar o qui quadrado
#GKgamma(tab) # cuidado o gamma superestima
tab1 <- table(dt8$d2014, dt8$d2018)
tab1  
options(scipen = 999)
chisq.test(tab1)  
GKgamma(tab1)
by(dt8$Bolsonaro, dt8$d2014, mean)
g <- ggplot(dt8, aes(d2014, Bolsonaro))
g + geom_boxplot() +  xlab("Bolso") +
  ylab("niveis Neves real")

#fictício
tab_ficcao <- table(dt$d2014, dt$d2018)
tab_ficcao  
chisq.test(tab1)  
GKgamma(tab1)


by(dt$bols, dt$d2014, mean)
h <- ggplot(dt, aes(d2014, bols))
h + geom_boxplot() +  xlab("Bolso") +
  ylab("niveis Neves ficção")

# comparar modelo_pol2 (sem Lula)com dados todos rnorm
summary(model_pol)
model_pol2 <- lm(dep ~ pleito, data = dt2)
summary(model_pol2)
neves #já
bols #já
#falta os outros três
dados$Serra02
sd(dados$Serra02)
serra02 <- rnorm(n = 1000, mean = c(48.140, 46.830, 39.150, 38.460, 61.340, 39.850, 39.820, 45.290, 53.850, 42.330,
                                    47.970, 43.210, 43.050, 39.720, 47.050, 43.300, 37.280, 43.890,
                                    43.270, 36.670, 50.580, 42.300, 29.880, 45.495, 48.833, 43.500, 37.390,
                                    42.620), sd = 6.005)
dados$Serra10
sd(dados$Serra10)
serra10 <- rnorm(n = 1000, mean = c(50.97, 52.47, 60.85, 43.76, 57.57, 50.60, 40.20, 50.43, 47.59, 54.33, 42.45, 56.83,
                                    45.63, 54.61, 52.40, 48.85, 44.03, 38.84, 56.52, 53.35, 61.26, 53.88, 40.74, 59.48,
                                    55.35, 46.16, 52.38, 41.05), sd = 6.5208)
dados$Alckmin06
sd(dados$Alckmin06)
alckimin <- rnorm(n = 1000, mean = c(60.40, 64.12, 72.93, 54.16, 72.29, 51.34, 50.68, 58.02, 63.74, 61.22, 52.56, 56.82,
                                    59.10, 60.62, 61.05, 53.86, 58.62, 53.62, 66.23, 58.83, 63.28, 66.21, 61.08,
                                    68.57, 67.06, 61.59, 53.24, 57.11), sd = 5.984)
hist(serra02)
hist(dados$Serra02)
hist(serra10)
hist(dados$Serra10)
hist(alckimin)
hist(dados$Alckmin06)


ficcao <- data.frame(bols,neves,alckimin,serra02,serra10)
ficcao$ranking <- ficcao$bols + ficcao$neves + ficcao$alckimin + ficcao$serra02 + ficcao$serra10
hist(ficcao$ranking)
print(ficcao)
ranking <- ficcao$ranking
Bolsonaro18 <- bols
Neves14 <- neves
Alckmin06 <- alckimin
Serra02 <- serra02
Serra10 <- serra10
ficcao <- data.frame(Bolsonaro18,Neves14,Alckmin06,Serra02,Serra10,ranking)
dados2 <- ficcao

ficcao <- ficcao %>% 
  pivot_longer(
    cols = Bolsonaro18:Serra10, # as colunas desse intervalo
    names_to = "pleito", # terão seus nomes armazenados nessa nova coluna
    names_prefix = "pleito_", # pegar apenas os nomes que vem depois de 'ano_'
    values_to = "votacao") # e os seus valores armazenados nessa nova coluna

#normalizar ranking,c(14)]
library(caret)
epa32 <- preProcess(ficcao[,c(1)], method = c("range"))
norm2 <- predict(epa32,ficcao[,c(1)])
summary(norm2)
cor(ficcao$ranking, norm2$ranking)
ficcao$ranking <- 100*norm2$ranking
rm(epa32, norm2)

#summary(model_pol2)
summary(model_pol2)
ficcao$pleito <- as.factor(ficcao$pleito)
ficcao$pleito <- relevel(ficcao$pleito, "Bolsonaro18")
model_ficcional <- lm(ranking ~ pleito, data=ficcao)
summary(model_ficcional)


coefplot(model_ficcional, intercept=FALSE, interactive = TRUE)
coefplot(model_pol2, intercept=FALSE, interactive = TRUE)

summary(dt2)
summary(ficcao)

by(dt2$dep, dt2$pleito, mean)
by(ficcao$ranking, ficcao$pleito, mean)
mean(Alckmin06)
mean(Serra10)


# BIC
# simulacro

var01 <- rnorm(n = 5000, mean = c(48.140, 46.830, 39.150, 38.460, 61.340, 39.850, 39.820, 45.290, 53.850, 42.330,
                                  47.970, 43.210, 43.050, 39.720, 47.050, 43.300, 37.280, 43.890,
                                  43.270, 36.670, 50.580, 42.300, 29.880, 45.495, 48.833, 43.500, 37.390,
                                  42.620), sd = 6.005)
dep <- rnorm(n=5000, mean = c(0.9,1,1.1), sd=0.07)
hist(var01)
hist(dep)
var02 <-rnorm(n=5000, mean=c(25,50,89,89.1),sd=6)
var03 <- ficcao$pleito
hist(var02)
cor.test(var01,var02)
cor.test(var01, dep)
cor.test(var02, dep)
regressor <-rnorm(n=5000, mean=c(49.8,50.8,51.1), sd=0.4)
hist(regressor)
cor(regressor, dep)
cor.test(regressor, var02)
data <- data.frame(dep, regressor, var01, var02,var03)
model1 <- lm(dep ~ regressor)
summary(model1)
model2 <- lm(dep ~ regressor + var01 + var02, data=data)
summary(model2)
model3 <- lm(dep ~ regressor + var01 + var02 + var03, dat=data)
summary(model3)
model4 <- lm(dep ~ regressor + var01+var02+var03+regressor*var03, data = data)
summary(model4)
huxreg(model1, model2, model3, model4)
models <- list(model1,model2, model3, model4)
aictab(cand.set=models)#tem q ter as mesmsas variáveis 


plot(dep~regressor)
plot(dep~var02)
plot(dep~var01)
plot(regressor~var02)
by(data$var01, data$var03, mean)
by(data$var02, data$var03, mean)
by(data$regressor, data$var03, mean)
by(data$dep, data$var03, mean)


library(BAS)
model<-bas.lm(dep~.,data=data, prior="ZS-null", modelprior = uniform(), method = "MCMC")
plot(model, which=1, add.smooth=F)
#2. cumulative probability
plot(model, which=2)

#3. model dimension plot
plot(model, which=2)
#4.PIP
plot(model, which = 4, ask=FALSE, caption="", sub.caption="")
#model rank
image(model, rotate = F)
summary(model)
summary(data)
predicao<-data.frame(var01=43.96,var02=63.22,regressor=50.56, var03="Serra02")

pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values
#var01min
predicao<-data.frame(var01=18.12,var02=63.22,regressor=50.56, var03="Serra02")
pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#var01min + var02max
predicao<-data.frame(var01=18.12,var02=110.449,regressor=50.56,var03="Serra02")
pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

sd(regressor)
50.56+0.7
#agora mexer no regressor (1 sd para cima)
predicao<-data.frame(var01=18.12,var02=110.449,regressor=51.26,var03="Serra02")
pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#agora mexer no regressor (2 sd para cima)
50.56+1.4
predicao<-data.frame(var01=18.12,var02=110.449,regressor=51.96,var03="Neves14")
pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values


#agora mexer no regressor (3 sd para cima)
50.56+(0.7*3)
predicao<-data.frame(var01=18.12,var02=110.449,regressor=52.66,var03="Neves14")
pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

# reals
coefplot(modelo_full, intercept=F,interactive=TRUE)
summary(modelo_full)
dt <- subset(dt2, select=c(dep,pleito,superior_comp,evangelico,idosos,rural_2010,log_pop))

model<-bas.lm(dep~.,data=dt, prior="ZS-null", modelprior = uniform(), method = "MCMC")
plot(model, which=1, add.smooth=F)
#2. cumulative probability
plot(model, which=2)

#3. model dimension plot
plot(model, which=2)
#4.PIP
plot(model, which = 4, ask=FALSE, caption="", sub.caption="")
#model rank
image(model, rotate = F)
summary(model)
#PostProbs - só rural, pleito e superior comp-    0.40 boa chance 40% de ser o melhor modelo)
modelopost <- lm(dep ~ pleito + superior_comp + rural_2010, data = dt)
summary(modelopost)
huxreg(modelo_full, modelopost)
huxreg(modelopost)
cor(dt$superior_comp, dt$rural_2010)
plot(dt$superior_comp, dt$rural_2010)
#muito bom
coefplot(modelopost, interactive=TRUE, intercept=FALSE)

#
# coeficiente padronizado ( qual é fator o mais importante?)
# install.packages("lm.beta")
library(lm.beta)
modelo2x <- lm.beta(modelopost)
tab_model(modelo2x, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")


#predição
dt <- subset(dt, select=c(dep,pleito,superior_comp,rural_2010))

model<-bas.lm(dep~.,data=dt, prior="ZS-null", modelprior = uniform(), method = "MCMC")
plot(model, which=1, add.smooth=F)
#2. cumulative probability
plot(model, which=2)

#3. model dimension plot
plot(model, which=2)
#4.PIP
plot(model, which = 4, ask=FALSE, caption="", sub.caption="")
#model rank
image(model, rotate = F)
summary(model)

#manter max de superior e min de rural e Bolsonaro18
max(dt$superior_comp)
min(dt$rural_2010)
predicao<-data.frame(superior_comp=13.45,rural_2010=7.21,pleito="Bolsonaro18")
pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values             
Bolsonaro <- pred$fit
Bolsonaro
# manter constante variáveis demográficas Serra02
predicao<-data.frame(superior_comp=13.45,rural_2010=7.21,pleito="Serra02")
pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values   
Serra02 <- pred$fit
# manter constante variáveis demográficas Alckmin06
predicao<-data.frame(superior_comp=13.45,rural_2010=7.21,pleito="Alckmin06")
pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values   
Alckmin06 <- pred$fit 
# manter constante variáveis demográficas Serra10
predicao<-data.frame(superior_comp=13.45,rural_2010=7.21,pleito="Serra10")
pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values   
Serra10 <- pred$fit 
# manter constante variáveis demográficas Neves14
predicao<-data.frame(superior_comp=13.45,rural_2010=7.21,pleito="Neves14")
pred = predict(model, newdata = predicao, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values
Neves14 <- pred$fit 
#na previsão caaiu de 67 para 51,73 (entre min47,78 e max66)
# https://sites.google.com/view/gregoriosilva/curtas/previs%C3%A3o-bolsonaro-alto-vale-2022
#  regra de 3
previnormal = (100*51.73)/82.42
previ_min = (100*47.78)/82.42
previ_max = (100*66)/82.42
previ_max
previ_min
previnormal
# entre 57,97 e 80.07
x <- c(previ_max, previ_min, previnormal)
sd(x)
var01 <- rnorm(n = 5000, mean = c(62.76), sd = 11.62)
hist(var01)
summary(var01)

by(dt2$dep, dt2$pleito, mean)
by(dt2$dep, dt2$pleito, median)
by(dt2$dep, dt2$pleito, sd)
var02 <- rnorm(n = 5000, mean = c(85.5), sd = 9.54)
hist(var02)
summary(var02)

# essa média de dt2$pleito Boslonaro equivaleu à 82.5% no sergundo turno
# já a previsão que fizemos nessa métrica similar equival a x

# max 
# 89,61 está para 82,5, assim como previ_max está para x
max <- (82.5*previ_max)/89.61
# meio
# 89,61 está para 82,5, assim como previnormal está para x
meio <- (82.5*previnormal)/89.61
# min 
min <- (82.5*previ_min)/89.61
# 89,61 está para 82,5, assim como previ_min está para x

max
meio
min
b <- runif(n=10000, min=53.37, max=73.72)
summary(b)
sd(b)
b <- rnorm(n=10000, mean=63.62,sd=5.83)
hist(b, xlab="Previsão",ylab="",main="Intervalo Bolsonaro Alto Vale Segundo Turno em 21 de Março")
boxplot(b)
summary(b)
b2 <- data.frame(b)
b3 <- ggplot(b2, aes(b))
b3 <- b3 + geom_histogram(binwidth=1)
b3 + labs(title = "Previsão Bolsonaro segundo turno Alto Vale",
          subtitle = "Estimativas simuladas",
          x = "", y = "Previsões", caption = "Previsão com dados disponíveis até 21 de Março de 2022")
library(RColorBrewer)
b3 + scale_fill_brewer(palette="Set3")+ labs(title = "Previsão Bolsonaro segundo turno Alto Vale",
          subtitle = "Estimativas simuladas",
          x = "", y = "Previsões", 
          caption = "Previsão com dados disponíveis até 21 de Março de 2022")+theme_blank()
  
b2 <- a2
a <- b
a <- data.frame(a)
n <- 1000
samp <- sample_n(a, n)
library(statsr)
library(dplyr)
samp %>%
  dplyr::summarise (mean_samp60 = mean(a), med_s60 = median(a),
             se_s60 = sd(a))
#confidence 95%
z_star_95 <- qnorm(0.975)
z_star_95
samp %>%
  dplyr::summarise(lower = mean(a) - z_star_95 * (sd(a) / sqrt(n)),
            upper = mean(a) + z_star_95 * (sd(a) / sqrt(n)))

# confidence levels
params <- a %>%
  dplyr::summarise(mu = mean(a))

samp %>%
  dplyr::summarise(samp_min = min(a), samp1_max = max(a))



ci <- a %>%
  rep_sample_n(size = n, reps = 50, replace = TRUE) %>%
  dplyr::summarise(lower = mean(a) - z_star_95 * (sd(a) / sqrt(n)),
            upper = mean(a) + z_star_95 * (sd(a) / sqrt(n)))
ci %>%
  slice(1:5) # só pra ver
ci <- ci %>%
  mutate(capture_mu = ifelse(lower < params$mu & upper > params$mu, "yes", "no"))
ci %>%
  slice(1:20) # só pra ver

ci_data <- data.frame(ci_id = c(1:50, 1:50),
                      ci_bounds = c(ci$lower, ci$upper),
                      capture_mu = c(ci$capture_mu, ci$capture_mu))
ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id, 
                           group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params$mu, color = "darkgray") # draw vertical line

#
