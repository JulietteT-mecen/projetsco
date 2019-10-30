# Import libraries and set working directory
library(data.table)
library(stargazer)
library(corrplot)
library(ggplot2)
library(RColorBrewer)
library(lmtest)
library(car)
library(miscTools)
setwd("~/FAC/M2/Econometrics/projetsco")

# Import and clean data
data=fread("Pommes.csv",dec=",")
data=data[,N:=NULL]
data$adv=as.factor(data$adv)

#####

# Fonction générique pour estimer la production

estimation=function(data,var_expliquee,vars_explicatives,forme_fonctionelle){
  if (forme_fonctionelle=="lineaire"){
    formule=paste(var_expliquee,"~ ",sep=" ")
    compteur=0
    for(i in vars_explicatives){
      compteur=compteur+1
      if (compteur==1){
        formule=paste(formule,i,sep=" ")
      }else{
        formule=paste(formule,"+",i,sep=" ")
      }
    }
    rm(compteur,i)
    formule=as.formula(formule)
    lmlineaire=lm(formule, data=data)
    return(lmlineaire)
  }else if (forme_fonctionelle=="cd"){
    formule=paste("log(",var_expliquee,") ~ ",sep="")
    compteur=0
    for(i in vars_explicatives){
      compteur=compteur+1
      if (compteur==1){
        formule=paste(formule," log(",i,") ",sep="")
      }else{
        formule=paste(formule," +"," log(",i,") ",sep="")
      }
    }
    rm(compteur,i)
    formule=as.formula(formule)
    lmcd=lm(formule, data=data)
    return(lmcd)
  }else if (forme_fonctionelle=="translog"){
    formule=paste("log(",var_expliquee,") ~ (",sep="")
    compteur=0
    for(i in vars_explicatives){
      compteur=compteur+1
      if (compteur==1){
        formule=paste(formule," log(",i,") ",sep="")
      }else{
        formule=paste(formule," +"," log(",i,") ",sep="")
      }
    }
    rm(compteur,i)
    formule=paste(formule,")^2",sep="")
    for (i in vars_explicatives){
      formule=paste(formule,"+I(log(",i,")^2)",sep="")
    }
    formule=as.formula(formule)
    lmtranslog=lm(formule, data=data)
    return(lmtranslog)
  }
}
#####

# Statistiques descriptives 

### Histogrammes des productivités moyennes des 3 facteurs de prod
data=data[,pmcap:=qOut/vCap]
data=data[,pmlab:=qOut/vLab]
data=data[,pmmat:=qOut/vMat]
ggplot(data, aes(x=pmcap)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
ggplot(data, aes(x=pmlab)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
ggplot(data, aes(x=pmmat)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

### Corrélations entre les 3 facteurs de prod
datacor=data[,.(vLab,vCap,vMat,qOut)]
M=cor(datacor)
corrplot(M,diag=F,type = "lower",addCoef.col = "white", method="shade",
         col=brewer.pal(n=8, name="PuOr"))

### Graph des correlations deux à deux 
ggplot(data, aes(x=pmcap, y=pmlab)) + 
  geom_point()+
  geom_smooth(method=lm)
ggplot(data, aes(x=pmcap, y=pmmat)) + 
  geom_point()+
  geom_smooth(method=lm)
ggplot(data, aes(x=pmmat, y=pmlab)) + 
  geom_point()+
  geom_smooth(method=lm)

### Graph par rapport a qOut
ggplot(data, aes(x=pmcap, y=qOut)) + 
  geom_point()+
  geom_smooth(method=lm)
ggplot(data, aes(x=pmlab, y=qOut)) + 
  geom_point()+
  geom_smooth(method=lm)
ggplot(data, aes(x=pmmat, y=qOut)) + 
  geom_point()+
  geom_smooth(method=lm)

### Calcul des quantités
data[,qCap:=vCap/pCap]
data[,qLab:=vLab/pLab]
data[,qMat:=vMat/pMat]

### Calcul du coût total et des coûts variables
data[,cost:=vCap+vLab+vMat]
all.equal(data$cost, with( data, pCap * qCap + pLab * qLab + pMat * qMat )) 
data[,vcost:=vLab+vMat]

### Calcul des indices Paasche, Laspeyre et Fisher
data[,XP:=(vCap+vLab+vMat) / (mean(qCap)*pCap + mean(qLab)*pLab + mean(qMat)*pMat)]
data[,XL:=(qCap*mean(pCap) + qLab*mean(pLab) + qMat*mean(pMat)) /
       (mean(qCap)*mean(pCap) + mean(qLab)*mean(pLab) + mean(qMat)*mean(pMat))]
data[,XF:=sqrt(XP*XL)]

### Indice de productivité globale des facteurs
data[,tfp:=qOut/XF]
ggplot(data, aes(x=tfp)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
ggplot(data, aes(x=tfp, y=qOut)) + 
  geom_point()+
  geom_smooth(method=lm)
ggplot(data, aes(x=tfp, y=qCap)) + 
  geom_point()+
  geom_smooth(method=lm)
ggplot(data, aes(x=pgf, y=qLab)) + 
  geom_point()+
  geom_smooth(method=lm)
ggplot(data, aes(x=tfp, y=qMat)) + 
  geom_point()+
  geom_smooth(method=lm)

### Boxplot
p=ggplot(data, aes(x=adv, y=tfp,color=adv)) + 
  geom_boxplot()
p+scale_color_manual(values=c("#999999", "#E69F00"))

#####

# Estimation des fonctions de production
varlist=c("qCap","qLab","qMat")

## Fonction linéaire 
# lmlineaire=lm( qOut ~ qCap + qLab + qMat, data = data)
lmlineaire=estimation(data, var_expliquee = "qOut", 
                      vars_explicatives = varlist, forme_fonctionelle = "lineaire")
summary(lmlineaire)
compPlot(data$qOut, fitted(lmlineaire))
sum(fitted(lmlineaire) < 0 )
coef(lmlineaire)

### Elasticités
data[,eps_cap:=coef(lmlineaire)["qCap"]*qCap/qOut]
data[,eps_lab:=coef(lmlineaire)["qLab"]*qLab/qOut]
data[,eps_mat:=coef(lmlineaire)["qMat"]*qMat/qOut]
colMeans(data[, c("eps_cap","eps_lab","eps_mat"), with=FALSE])
ggplot(data, aes(x=eps_cap)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
ggplot(data, aes(x=eps_lab)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
ggplot(data, aes(x=eps_mat)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

data[,eps_cap_fit:=coef(lmlineaire)["qCap"]*qCap/fitted(lmlineaire)]
data[,eps_lab_fit:=coef(lmlineaire)["qLab"]*qLab/fitted(lmlineaire)]
data[,eps_mat_fit:=coef(lmlineaire)["qMat"]*qMat/fitted(lmlineaire)]
colMeans(data[, c("eps_cap_fit","eps_lab_fit","eps_mat_fit"), with=FALSE])
ggplot(data, aes(x=eps_cap_fit)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
ggplot(data, aes(x=eps_lab_fit)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
ggplot(data, aes(x=eps_mat_fit)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

data[,eps_echelle:= eps_cap+eps_lab+eps_mat]
data[,eps_echelle_fit:= eps_cap_fit+eps_lab_fit+eps_mat_fit]
colMeans(data[, c("eps_echelle","eps_echelle_fit"), with=FALSE])
ggplot(data, aes(x=eps_echelle)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
ggplot(data, aes(x=eps_echelle_fit)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
ggplot(data, aes(x=eps_echelle, y=XF)) + 
  geom_point()

### Taux marginal de substitution technique
tmst_CapLab=-coef(lmlineaire)["qLab"] / coef(lmlineaire)["qCap"]
tmst_CapMat=-coef(lmlineaire)["qMat"] / coef(lmlineaire)["qCap"]
tmst_LabCap=-coef(lmlineaire)["qCap"] / coef(lmlineaire)["qLab"]
tmst_LabMat=-coef(lmlineaire)["qMat"] / coef(lmlineaire)["qLab"]
tmst_MatCap=-coef(lmlineaire)["qCap"] / coef(lmlineaire)["qMat"]
tmst_MatLab=-coef(lmlineaire)["qLab"] / coef(lmlineaire)["qMat"]

### Taux Marginal relatif de substitution technique 
data[,tmrst_CapLab:=-eps_lab/eps_cap]
data[,tmrst_CapMat:=-eps_mat/eps_cap]
data[,tmrst_LabCap:=-eps_cap/eps_lab]
data[,tmrst_LabMat:=-eps_mat/eps_lab]
data[,tmrst_MatCap:=-eps_cap/eps_mat]
data[,tmrst_MatLab:=-eps_lab/eps_mat]
colMeans(data[, c("tmrst_CapLab","tmrst_MatLab"), with=FALSE])

## Fonction Cobb-Douglas
# lmcobbdouglas=lm(log(qOut)~log(qCap)+log(qLab)+log(qMat),data=data)
lmcobbdouglas=estimation(data, var_expliquee = "qOut", 
                      vars_explicatives = varlist, forme_fonctionelle = "cd")
stargazer(lmcobbdouglas,type="text")

# Intervalle des rendements d'echelle
shapiro.test(residuals(lmcobbdouglas))
vcov(lmcobbdouglas)
est=sum(lmcobbdouglas$coefficients[-1])
dESCD=c( 0, 1, 1, 1 )
varESCD=t(dESCD) %*% vcov(lmcobbdouglas) %*% dESCD
seESCD=sqrt( varESCD )
est+1.96*seESCD
est-1.96*seESCD
# 1 n'appartient pas à l'intervalle

# Estimer la translog
# lmtranslog=lm(log(qOut)~(log(qCap)+log(qLab)+log(qMat))^2+I(log(qCap)^2)+I(log(qLab)^2)+I(log(qMat)^2),
           # data=data)
lmtranslog=estimation(data, var_expliquee = "qOut", 
                      vars_explicatives = varlist, forme_fonctionelle = "translog")
stargazer(lmtranslog,type="text")
lmtranslog$coefficients
vif(lmtranslog)
# Il y a une très grande colinéarité entre les paramètres (ce qui explique leur non significativité)

# Test CD vs translog
anova(lmcobbdouglas,lmtranslog)
stargazer(lmcobbdouglas,type="text")
summary(lmcobbdouglas)$r.squared
length(lmcobbdouglas$residuals)
lmcobbdouglas$df.residual
length(lmcobbdouglas$coefficients)-1

# Fonction pour faire le test CD vs translog
f_test=function(contraint,non_contraint,seuil){
  rcontraint=summary(contraint)$r.squared
  rnoncontraint=summary(non_contraint)$r.squared
  Q=length(non_contraint$coefficients)-length(contraint$coefficients)
  resultat=((rnoncontraint-rcontraint)/(1-rnoncontraint))*(non_contraint$df.residual/Q)
  alpha=1-seuil
  F_theo=qf(alpha,Q,non_contraint$df.residual)
  if (resultat>F_theo){
    print(paste(round(resultat,3),"est strictement supérieur à la statistique théorique de Fisher, on rejette l'hypothèse nulle",sep=" "))
  }else {
    print(paste(round(resultat,3),"est inférieur à la statistique théorique de Fisher, on ne peux pas rejeter l'hypothèse nulle",sep=" "))
  }
  
}
f_test(lmcobbdouglas,lmtranslog,0.05)

## Fonction CES
lmces_CapLab=lm(log(qCap/qLab) ~ log(pLab/pCap), data=data)
lmces_CapMat=lm(log(qCap/qMat) ~ log(pMat/pCap), data=data)
lmces_LabCap=lm(log(qLab/qCap) ~ log(pCap/pLab), data=data)
lmces_LabMat=lm(log(qLab/qMat) ~ log(pMat/pLab), data=data)
lmces_MatCap=lm(log(qMat/qCap) ~ log(pCap/pMat), data=data)
lmces_MatLab=lm(log(qMat/qLab) ~ log(pLab/pMat), data=data)
ces_list=c("lmces_CapLab","lmces_CapMat","lmces_LabCap","lmces_LabMat","lmces_MatCap","lmces_MatLab")
for (i in ces_list){
  stargazer(get(i),type="text")
}
rm(i)

#####

# Fonctions de coût

## Cobb Douglas
reg_cout_CD=lm( log( cost ) ~ log( pCap ) + log( pLab ) + log( pMat ) + log( qOut ), data = data )
stargazer(reg_cout_CD, type='text')

## Cobb Douglass de court terme
reg_cout_CD_short_run<-lm(log(qOut)~log(qLab)+log(qMat)+log(qCap), data=data)
stargazer(reg_cout_CD_short_run, type="text")