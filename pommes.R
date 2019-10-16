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

## Fonction linéaire 
lmlineaire=lm( qOut ~ qCap + qLab + qMat, data = data)
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
ggplot(data, aes(x=eps_echelle, y=X)) + 
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
lmcobbdouglas=lm(log(qOut)~log(qCap)+log(qLab)+log(qMat),data=data)
stargazer(lmcobbdouglas,type="text")



# Intervalle des rendements d'echelle
shapiro.test(residuals(lm1))
vcov(lm1)
est=sum(lm1$coefficients[-1])
dESCD=c( 0, 1, 1, 1 )
varESCD=t(dESCD) %*% vcov(lm1) %*% dESCD
seESCD=sqrt( varESCD )
est+1.96*seESCD
est-1.96*seESCD
# 1 n'appartient pas à l'intervalle

# Estimer la translog
lm2=lm(log(qOut)~(log(qCap)+log(qLab)+log(qMat))^2+I(log(qCap)^2)+I(log(qLab)^2)+I(log(qMat)^2),
          data=data)
stargazer(lm2,type="text")
lm2$coefficients
vif(lm2)
# Il y a une très grande colinéarité entre les paramètres (ce qui explique leur non significativité)


# Boucle calcul elasticité 
function_list=c("lineaire","cobbdouglas","quadratic","translog")
test=c("lineaire","cobbdouglas")
lmlineaire=lm( qOut ~ qCap + qLab + qMat, data = data)
lmcobbdouglas=lm(log(qOut)~log(qCap)+log(qLab)+log(qMat),data=data)
assign(x=paste("table_",i,sep=""),value=subset(A, ID == i))
reg=get("lmcobbdouglas")
coef(reg)
coef(lmcobbdouglas)
for (i in test){
  print(i)
  reg=get(paste("lm",i,sep=""))
  
}
