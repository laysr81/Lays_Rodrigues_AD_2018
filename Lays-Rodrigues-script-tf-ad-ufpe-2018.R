install.packages("dyplr")
library(ggplot2)
Lays_Rodrigues_tf_ad_ufpe_2018 <- read_dta("Lays-Rodrigues-tf-ad-ufpe-2018.dta")
Lapop17 <- Lays_Rodrigues_tf_ad_ufpe_2018

####Transform��o das repostas 1 e 2 para 0 e 1
Brasil17 <- Lapop17 %>%
  mutate( w14a = ifelse( w14a == "1",0,1))
counts <- table(Brasil17$w14a, Brasil17$q5a) 

### Gr�fico 1
counts <- table(Brasil17$w14a, Brasil17$q5a)
barplot(counts, main="Gr�fico 1 - Apoio � interrup��o",
        xlab="Frequ�ncia", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

####Gr�fico 2 

counts <- table(Brasil17$w14a, Brasil17$q1)
barplot(counts, main="Gr�fico 2 - Apoio � interrup��o",
        xlab="G�nero", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


###Gr�fico 3

counts <- table(Brasil17$w14a, Brasil17$ed)
barplot(counts, main="Gr�fico 3 - Apoio � interrup��o",
        xlab="tempo em inst. de ensino", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

### Gr�fico 4
counts <- table(Brasil17$w14a, Brasil17$l1)
barplot(counts, main="Gr�fico 4 - Apoio � interrup��o",
        xlab="Ideologia pol�tica", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

###Logit - Bivariada e c�culo de odd-ratios 

logit1 <- glm(formula = w14a ~ q5a, family = "binomial"(link= "logit"), data= Brasil17)
glm(formula = w14a ~ q5a, family = "binomial"(link="logit"), data= Brasil17)
summary(logit1)

confint.default(logit1) ##c�lculo dos intervalos de confian�a do modelo ajustado
###raz�es de chance 
exp(cbind(OR = coef(logit1), confint.default(logit1)))

###Logit - Multivariada e c�lculo de odd-ratios

logit2 <- glm(formula = w14a ~ q5a + l1 + ed + q1, family = "binomial"(link= "logit"), data= Brasil17)
glm(formula = w14a ~ q5a + ed + l1 + q1, family = "binomial"(link="logit"), data= Brasil17)
summary(logit2)
confint.default(logit2) ##c�lculo dos intervalos de confian�a do modelo ajustado
exp(cbind(OR = coef(logit2), confint.default(logit2))) ###raz�es de chance






