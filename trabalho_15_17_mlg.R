rm(list=ls())
gc(reset=TRUE)


setwd("C:/Users/fsbmat/Desktop/Trabalho_MLG/Homicidios")

require(gdata)
require(foreign)
require(ggplot2)
require(MASS)
library(betareg)
require(Epi)
library(car)
library(mlbench)
library(effects)
library(np)
library(biglm)
cat("\014")


#update.packages(checkBuilt=TRUE)

dados=read.table("15_17.txt",header=TRUE)
attach(dados)
#S=sexo, R=Raça/cor, ESC=Escolaridade, I=Idade, Y=1: Morrer assassinado e 0: morrer de outra forma
dados=transform(R=factor(R),ESC=factor(ESC),S=factor(S),dados)
head(dados)

#Estatística Descritiva dos dados
summary(dados)

#Gráfico que mostra por idade a proporção do número de pessoas assassinadas em relação ao número de 
#pessoas que morreram em 2013 no estado de Minas Gerais, note que a partir dos dez anos há um aumento 
#linear e rápido até que chega-se a um pico entre os 15 e 18 anos e depois o número de homicídios volta
# a descrescer.


a=table(I,Y)
prop=a[,2]/(a[,1]+a[,2])
Idade <- seq(15, 17, 1)
plot(Idade,prop,ylab="Proporção", main="Proporção por idade de pessoas assassinadas em 2013 a partir dos dez anos em
     relação as pessoas que morreram em Minas Gerais")
lines(Idade,prop)



#Note que as mortes por assassinato superam as mortes por outras causas

bp=barplot(table(Y,I), beside=T, leg=c("Morte por outra causa", "Homicidío" ),
           args.legend = list(x = "topleft",bty = "n"),ylim=c(0, 2000), ylab="Quantidade", xlab="Idade", main="Número de pessoas que morreram em 2013
           no Estado de Minas Gerais aos 15,16 e 17 anos")
text(bp, table(Y,I)+100, table(Y,I))



#Outros gráficos interessantes para analise

bp=barplot(table(Y,S),ylim=c(0, 3500), beside=T,leg=c("Morte por outra causa", "Homicidío" ),
           ylab="Quantidade", xlab="",names.arg = c("masculino","feminino"), main="Número de pessoas que morreram em 2013
           no Estado de Minas Gerais por sexo")
text(bp, table(Y,S)+150, table(Y,S))

bp=barplot(table(Y,R),args.legend = list(x = "topleft",bty = "n"), ylim=c(0, 2500), beside=T,leg=c("Morte por outra causa", "Homicidío" ),
           ylab="Quantidade", xlab="Raça/Cor",names.arg = c("Branco","Negro","Pardo","Indígena"), main="Número de pessoas que morreram em 2013
           no Estado de Minas Gerais por Raça/Cor")
text(bp, table(Y,R)+150, table(Y,R))

bp=barplot(table(Y,ESC), args.legend = list(x = "topleft",bty = "n"), ylim=c(0, 2500), beside=T,leg=c("Morte por outra causa", "Homicidío" ),
           ylab="Quantidade", xlab="Escolaridade",names.arg = c("Nenhum","1 a 3","4 a 7","8 a 11","12 ou mais"), main="Número de pessoas que morreram em 2013 no
           Estado de Minas Gerais por anos de estudo")
text(bp, table(Y,ESC)+150, table(Y,ESC))

#A função aggregate() é usada para encontrar o número de sucessos e o número de fracassos para cada "cenário":
w <- aggregate(Y ~ S+R+ESC+I, data=dados,FUN=sum)
n <- aggregate(Y ~ S+R+ESC+I,data=dados,FUN=length)
success=w$Y; failure=n$Y
w.n <- data.frame(S=w$S,R=w$R,ESC=w$ESC,I=w$I, success=w$Y, failure=n$Y , proportion = round(w$Y/n$Y,6))
head(w.n)


#Número de pessoas assassinadas por idade
plot(w.n[,4],w.n[,5],ylab="Quantidade",xlab="Idade", main="Quantidade de pessoas assassinadas por Idade
     em Minas Gerais no ano de 2013 
     a partir dos dez anos de idade")

#Box-plot da Proporção de pessoas assassinadas por Raça/Cor
plot(w.n[,2],w.n[,7],ylab="Proporção de assassinato",xlab="Raça/Cor",names=c("Branco","Negro","Pardo","Indígena"), main="Box-plot das pessoas assassinadas em Minas Gerais
     no ano de 2013 por Raça/Cor")

#Box-plot da Proporção de pessoas assassinadas por Escolaridade
plot(w.n[,3],w.n[,7],ylab="Proporção",xlab="Escolaridade", names=c("Nenhum","1 a 3","4 a 7","8 a 11","12 ou mais"), main="Box-plot das  pessoas assassinadas em Minas Gerais
     no ano de 2013 por Escolaridade")

#Box-plot da Proporção de pessoas assassinadas por Sexo
bx=plot(w.n[,1],w.n[,7],ylab="Proporção",xlab="", names= c("masculino","feminino"), main="Box-plot das  pessoas assassinadas em Minas Gerais
        no ano de 2013 por Sexo")


#Análise visual do impacto das covariáveis em Y

Y1=factor(Y)
layout(matrix(1:2, ncol = 2))
cdplot(Y1 ~ I, data = dados)
cdplot(Y1 ~ R, data = dados)
cdplot(Y1 ~ ESC, data = dados)
cdplot(Y1 ~ S, data = dados)

####################################
#Possíveis modelos
###################################



#mod1<- glm(formula = Y ~ 1, family=binomial(link=logit), data = dados)
#mod2<- glm(formula = Y ~ S, family=binomial(link=logit), data = dados)
#mod3<- glm(formula = Y ~ R, family=binomial(link=logit), data = dados)
#mod4<- glm(formula = Y ~ ESC, family=binomial(link=logit), data = dados)
#mod5<- glm(formula = Y ~ I, family=binomial(link=logit), data = dados)
#mod6<- glm(formula = Y ~ S+R, family=binomial(link=logit), data = dados)
#mod7<- glm(formula = Y ~ S+ESC, family=binomial(link=logit), data = dados)
#mod8<- glm(formula = Y ~ S+I, family=binomial(link=logit), data = dados)
#mod9<- glm(formula = Y ~ R+ESC, family=binomial(link=logit), data = dados)
#mod10<- glm(formula = Y ~ R+I, family=binomial(link=logit), data = dados)
#mod11<- glm(formula = Y ~ ESC+I, family=binomial(link=logit), data = dados)
#mod12<- glm(formula = Y ~ S+R+ESC, family=binomial(link=logit), data = dados)
#mod13<- glm(formula = Y ~ S+R+I, family=binomial(link=logit), data = dados)
#mod14<- glm(formula = Y ~ R+ESC+I, family=binomial(link=logit), data = dados)
#mod15<- glm(formula = Y ~ S+R+ESC+I, family=binomial(link=logit), data = dados)
#mod16<- glm(formula = Y ~ S+R+ESC+I+I*S, family=binomial(link=logit), data = dados)
#mod17<- glm(formula = Y ~ S+R+ESC+I+I*R, family=binomial(link=logit), data = dados)
#mod18<- glm(formula = Y ~ S+R+ESC+I+I*ESC, family=binomial(link=logit), data = dados)
#mod19<- glm(formula = Y ~ S+R+ESC+I+I*S+I*R, family=binomial(link=logit), data = dados)
#mod20<- glm(formula = Y ~ S+R+ESC+I+I*S+I*ESC, family=binomial(link=logit), data = dados)
#mod21<- glm(formula = Y ~ S+R+ESC+I+I*R+I*ESC, family=binomial(link=logit), data = dados)
#mod22<- glm(formula = Y ~ S+R+ESC+I+I*S*R, family=binomial(link=logit), data = dados)
#mod23<- glm(formula = Y ~ S+R+ESC+I+I*S*ESC, family=binomial(link=logit), data = dados)
#mod24<- glm(formula = Y ~ S+R+ESC+I+S*R, family=binomial(link=logit), data = dados)
#mod25<- glm(formula = Y ~ S+R+ESC+I+S*R*ESC, family=binomial(link=logit), data = dados)

#Teste para comparação dos modelos
#anova(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,mod15,mod16,mod17,mod18,mod19,mod20,mod21,mod22,mod23,mod24,mod25,test="Chisq")

#Por este teste foi possível avaliar os diversos modelos acima, foram significativos os
#modelos mod2 ao mod15

#anova(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,mod15,test="Chisq")

#O melhor modelo que possui deviance mais próxima do número de graus de liberdade é:
#Model 15: Y ~ S + R + ESC + I

#Vejamos métodos de seleção de covariáveis

#Pacote e códigos para escolha do melhor modelo

library (glmulti)
search.1.aicc<-glmulti(y=Y ~.,data=dados, fitfunction="glm",level=1,method="h",crit="aicc",
                       family=binomial(link="logit"))
print(search.1.aicc)
aa<-weightable(search.1.aicc)
cbind(model=aa[1:5,1],round(aa[1:5,2:3],digits=3))

#Por meio do código acima, o melhor modelo foi "Y ~ 1 + S + R + ESC + I"


#Por meio do método de seleção abaixo podemos tentar achar um melhor modelo
#Foward selection
empty.mod<-glm(formula = Y ~ 1, family=binomial(link=logit), data = dados)
full.mod<-glm(formula = Y ~ ., family=binomial(link=logit), data = dados)
forw.sel<-step(object=empty.mod, scope=list(upper=full.mod), direction="forward", k=log(nrow(dados)), trace=TRUE)

#Melhor modelo pelo foward: Y ~ ESC + S + R + I

#Backward selection
back.sel<-step(object=full.mod,scope=list(lower=empty.mod), direction="backward",k=(nrow(dados)),trace = TRUE )

#Melhor modelo pelo backward: Y ~ S 

##Stepwise selection
step(glm(formula = Y ~ ., family=binomial(link=logit), data = dados), direction = "both")

#Melhor modelo Stepwise: Y ~ S + R + ESC + I

#Outro método de seleção individual

search.1.bic<-glmulti(y=Y ~., data=dados, fitfunction="glm", level=1, method="h", crit="bic", family=binomial(link="logit"))
head(weightable(search.1.bic))
plot(search.1.bic,type="w")
#Coeficientes do melhor modelo pela função glmulti
parms<- coef(search.1.bic)
# Renaming columns to fit in book output display
colnames(parms)<-c("Estimate","Variance","n.Models","Probability","95%CI+/-")
round(parms,digits=3)
parms.ord<-parms[order(parms[,4],decreasing=TRUE),]
ci.parms<-cbind(lower=parms.ord[,1]-parms.ord[,5],upper=parms.ord[,1]+parms.ord[,5])
round(cbind(parms.ord[,1],ci.parms),digits = 3)

#Melhor modelo:  Y ~ 1 + S + R + ESC + I

#Razão de chances
round(exp(cbind(OR=parms.ord[,1], ci.parms))[-1,],digits=2)



#Teste para comparação dos modelos
#anova(mod9,modglm,modfow,modbac,modStep,test="Chisq")

#Melhor modelo pela deviance residual e número de graus de liberdade é modfow

#Modind deve ser analisado separadamente por ser uma análise individual, porém, ele é o mesmo modelo modfow
#compare as analises abaixo para confirmar
#anova(modind,test="Chisq")
#anova(modfow,test="Chisq")

#Vamos analisar algumas interações

#mod1 <- glm(formula = success / failure ~ ESC + S + R + I+I:S, weights=failure , family=binomial(link=logit), data = w.n)
#mod2 <- glm(formula = success / failure ~ ESC + S + R + I+I:ESC, weights=failure , family=binomial(link=logit), data = w.n)
#mod3 <- glm(formula = success / failure ~ ESC + S + R + I+I:S+I:ESC, weights=failure , family=binomial(link=logit), data = w.n)
#mod4 <- glm(formula = success / failure ~ ESC + S + R + I+I:ESC:S, weights=failure , family=binomial(link=logit), data = w.n)
#anova(mod1,mod2,mod3,mod4,modfow,test="Chisq")
#Nenhum foi significativo, logo não iremos analisar interação

#Bestfit pelos métodos acima foi o modelo minimal success / failure ~ ESC + S + R + I


bestfit=glm(formula = Y ~ S+R+ESC+I, family=binomial(link=logit), data = dados)
anova(bestfit,test="Chisq")
summary(bestfit)

##########################################################################################
#H0: O modelo ajustado é adequado
#H1: O modelo ajustado não é adequado

p_valor=pchisq(deviance(bestfit),bestfit$df.residual,lower.tail = F);p_valor

#rejeita-se H0 o ajuste não é adequado 

library(ResourceSelection)
#Teste de Hosmer e Lemeshow
#H0: O modelo está bem ajustado
#H1: O modelo não está bem ajustado
hoslem.test(bestfit$y, fitted(bestfit))

#testes de diagnósticos
library(LogisticDx)
dx(bestfit)
#gof(bestfit)

rm("LogisticDx")

OR(bestfit)
attributes(OR(bestfit))

plot(bestfit)
sig(bestfit)
sig(bestfit, test="coef")
sig(bestfit, test="var")
ss(bestfit, coeff="ESC2")

library(pROC)

roc(bestfit$y,bestfit$fitted.values,plot=T,ci=T,identity=TRUE,print.auc=TRUE)



############################################
#Resíduos
############################################

pi.hat<-predict(bestfit,type="response")
p.res<-residuals(bestfit,type="pearson")
s.res<-rstandard(bestfit,type="pearson")
lin.pred<-bestfit$linear.predictors
#w.n<-data.frame(w.n,pi.hat,p.res,s.res,lin.pred)
#head(w.n)

plot(pi.hat)
plot(p.res)
plot(lin.pred)

source("glmDiagnostics.R")
save.diag<-glmInflDiag(mod.fit=bestfit,print.output=TRUE,which.plots=1:2)

#Por meio destes gráficos identificamos observações influentes na análise e resolvemos
#Analisa-las, retirando-as do modelo. Vamos retirar todas com |Cooks.D|>=0.2 e maior h, ou seja, primeiro
#a observação 75





#Veja que as observações retiradas influenciaram muito o modelo

summary(bestfit)



#Residuos padronizados contra resíduos estimados pelo modelo
par(mfrow=c(1,1))
pred<-predict(bestfit)
stand.resid<-rstandard(model=bestfit, type="pearson")
plot(x=pred,y=stand.resid, xlab="Estimated logit ( success )", ylab="Standardized Pearson residuals", 
     main="Standardized residuals vs. Estimated logit", ylim=c(-6,6))
abline(h=c(qnorm(0.995),0,qnorm(0.005)), lty="dotted",col="red")

#Resíduo de Pearson
rp=residuals(bestfit,type="pearson")

#H0:o modelo ajustado está correto
#Ha:Não H0
1-pchisq(sum(rp^2),bestfit$df.residu)
#O modelo está correto


par(mfrow=c(1,1))
plot(predict(bestfit),residuals(bestfit),col=c("blue","red")[1+Y])
abline(h=0,lty=2,col="grey")
lines(lowess(predict(bestfit),residuals(bestfit)),col="black",lwd=2)


par(mfrow=c(1,1))
plot(predict(bestfit),residuals(bestfit))
abline(h=0,lty=2,col="grey")

plot(bestfit$linear.predictors,residuals(bestfit, type="pearson"))
plot(residuals(bestfit,type="pearson"),ylab="Resíduos de Pearson")
identify(1:length(Y), residuals(bestfit,type="pearson"))
plot(residuals(bestfit,type="deviance"),ylab="Resíduos Deviance")
identify(1:length(Y), residuals(bestfit,type="deviance"))

hist(residuals(bestfit))


#Distribuição normal os resíduos?
hist(residuals(bestfit), breaks=seq(min(residuals(bestfit)),max(residuals(bestfit))+1,by=.5),
     xlim=c(-4,4), probability=T, col='light blue')
points(density(residuals(bestfit), bw=.5), type='l', lwd=3, col='red')


#Distribuição normal os resíduos?
#hist(bestfit$residuals, breaks=seq(min(bestfit$residuals),max(bestfit$residuals)+1,by=.5),
#     xlim=c(-10,10), probability=T, col='light blue')
#points(density(bestfit$residuals, bw=.5), type='l', lwd=3, col='red')


#Para análise e identificação de outiliers
#Ver se o modelo muda muito ao retirar este dado
outlierTest(bestfit)

#linhas=c(1503)
#dados=dados[-linhas,]
#bestfit=glm(formula = Y ~ S+R+ESC+I, family=binomial(link=logit), data = dados)
#save.diag<-glmInflDiag(mod.fit=bestfit,print.output=TRUE,which.plots=1:2)

#Gráfico de resíduos versus ajuste
plot(bestfit, which=1)
#Q-q plot
plot(bestfit, which=2)
#Valor padronizado dos resíduos
plot(bestfit, which=3)
#Distância de Cook's
plot(bestfit, which=4)
#Leverage
plot(bestfit, which=5)
#Distância de Cook's vs Leverage
plot(bestfit, which=6)
#Matriz hat
#plot(hat(Y), type='h')



boxplot(bestfit$fitted.values~bestfit$y)

library(car)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(bestfit)
# plot studentized residuals vs. fitted values
spreadLevelPlot(bestfit)
leveragePlots(bestfit)




linhas=c(1208)
dados=dados[-linhas,]
bestfit=glm(formula = Y ~ S+R+ESC+I, family=binomial(link=logit), data = dados)
save.diag<-glmInflDiag(mod.fit=bestfit,print.output=TRUE,which.plots=1:2)

linhas=c(1208)
dados=dados[-linhas,]
bestfit=glm(formula = Y ~ S+R+ESC+I, family=binomial(link=logit), data = dados)

linhas=c(1730)
dados=dados[-linhas,]
bestfit=glm(formula = Y ~ S+R+ESC+I, family=binomial(link=logit), data = dados)

#De outra forma os mesmos gráficos
op <- par(mfrow=c(2,2))
plot(bestfit,ask=F)
par(op)


#Gráfico dos resíduos do modelo considerando as covariáveis separadamente
plot(bestfit$residuals,col=unique(I), ylab="Resíduos",main="Covariável I")
plot(bestfit$residuals,col=unique(R), ylab="Resíduos",main="Covariável R")
plot(bestfit$residuals,col=unique(ESC), ylab="Resíduos",main="Covariável ESC")
plot(bestfit$residuals,col=unique(S), ylab="Resíduos",main="Covariável S")

summary(bestfit$fitted.values) 

#Gráfico de envelope da binomial, demora bastante para rodar

fit.model=bestfit
source("envel_bino.txt")

#Odds Ratio

exp(coef(bestfit))
exp(cbind(coef(bestfit)))
#Intervalo de confiança - demora muito
#tab=cbind(Par=coef(bestfit),OR=exp(coef(bestfit)), confint(bestfit))

library(xtable)
#colnames(tab) <- c( "Parâmetros","Razão de Chances", "2.5\%","97.5\%")
#rownames(tab) <- c("S2", "R2", "R4","R5","ESC2","ESC3","ESC4","ESC5","I")
#xtable(tab)
#Perfil da pessoa assassinada no ano de 2013 em Minas Gerais
#ESC: Escolaridade
#I: Idade
#R: Raça/cor
#S: Sexo

#Todos são solteiros


#R1:Ser de cor branca - Valor de Referência
#R2: A chance de um indivíduo negro ser assassinado é 90% maior que a chance de um indivíduo branco
#R3:Ser amarelo - não pessoas nessa classe
#R4:A chance de um indivíduo pardo ser assassinado é 72% maior que a chance de um indivíduo branco
#R5:A chance de um indivíduo indígena ser assassinado é 75% menor que a chance de um indivíduo branco
#ESC1:Ter de nenhum estudo - valor referência 
#ESC2:Ter de 1 a 3 anos de estudo não significativo
#ESC3:Ter de 4 a 7 anos de estudo não foi significativa
#ESC4:A chance de um indivíduo que tem de 8 a 11 anos de estudo ser assassinado é 58% menor que a chance de um indivíduo com nenhum estudo
#ESC5:A chance de um indivíduo que tem 12 ou mais anos de estudo ser assassinado é 88% menor que a chance de um indivíduo com nenhum estudo
#S1:Sexo masculino - valor de referência
#S2:A chance de um indivíduo do sexo feminino ser assassinado é 52% menor que a chance de um indivíduo do sexo masculino
summary(bestfit)

S2=1
R2=0
R4=1
R5=0
ESC2=0
ESC3=0  
ESC4=0
ESC5=1
I=17

hat_pi = exp(-3.54342-0.71483*(S2)+0.64436*(R2)+0.54040*(R4)-1.38578*(R5)+0.33147*(ESC2)+0.09528*(ESC3)-0.87388*(ESC4)-2.14116*(ESC5)+0.22666*I)/(1+exp(-3.54342-0.71483*(S2)+0.64436*(R2)+0.54040*(R4)-1.38578*(R5)+0.33147*(ESC2)+0.09528*(ESC3)-0.87388*(ESC4)-2.14116*(ESC5)+0.22666*I))
hat_pi


plot(fitted(bestfit)~ESC+R)
table(dados$Y, predict(bestfit) > 0.5)

0.09687408+0.22666*0.09687408

