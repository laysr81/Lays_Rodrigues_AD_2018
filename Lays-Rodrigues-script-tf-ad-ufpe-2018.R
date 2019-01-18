install.packages("dyplr")
library(ggplot2)
Lays_Rodrigues_tf_ad_ufpe_2018 <- read_dta("Lays-Rodrigues-tf-ad-ufpe-2018.dta")
Lapop17 <- Lays_Rodrigues_tf_ad_ufpe_2018

####Transformção das repostas 1 e 2 para 0 e 1
Brasil17 <- Lapop17 %>%
  mutate( w14a = ifelse( w14a == "1",0,1))
counts <- table(Brasil17$w14a, Brasil17$q5a) 

### Gráfico 1
counts <- table(Brasil17$w14a, Brasil17$q5a)
barplot(counts, main="Gráfico 1 - Apoio à interrupção",
        xlab="Frequência", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

####Gráfico 2 

counts <- table(Brasil17$w14a, Brasil17$q1)
barplot(counts, main="Gráfico 2 - Apoio à interrupção",
        xlab="Gênero", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


###Gráfico 3

counts <- table(Brasil17$w14a, Brasil17$ed)
barplot(counts, main="Gráfico 3 - Apoio à interrupção",
        xlab="tempo em inst. de ensino", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

### Gráfico 4
counts <- table(Brasil17$w14a, Brasil17$l1)
barplot(counts, main="Gráfico 4 - Apoio à interrupção",
        xlab="Ideologia política", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

###Logit - Bivariada e cáculo de odd-ratios 

logit1 <- glm(formula = w14a ~ q5a, family = "binomial"(link= "logit"), data= Brasil17)
glm(formula = w14a ~ q5a, family = "binomial"(link="logit"), data= Brasil17)
summary(logit1)

confint.default(logit1) ##cálculo dos intervalos de confiança do modelo ajustado
###razões de chance 
exp(cbind(OR = coef(logit1), confint.default(logit1)))

###Logit - Multivariada e cálculo de odd-ratios

logit2 <- glm(formula = w14a ~ q5a + l1 + ed + q1, family = "binomial"(link= "logit"), data= Brasil17)
glm(formula = w14a ~ q5a + ed + l1 + q1, family = "binomial"(link="logit"), data= Brasil17)
summary(logit2)
confint.default(logit2) ##cálculo dos intervalos de confiança do modelo ajustado
exp(cbind(OR = coef(logit2), confint.default(logit2))) ###razões de chance






