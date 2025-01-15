
# aktivace knihovny s popisnymi statistikami
library(DescTools)

# Nacteni vestavene databaze Boston
library(MASS)
data(Boston)
  # faktory ovlivnujici cenu nemovitosti na predmesti Bostonu
?Boston
  # co popisuji jednotlive promenne

# kategoricke promenne databaze Boston
tax.k <- Boston$tax %/% 100
land.z <- Boston$zn %/% 10
Boston.kat <- data.frame(cbind("chas"=Boston$chas, "rad" = Boston$rad, tax.k, land.z))
  # databaze kategorickych promennych

##########################
### robustni popisne statistiky ciselne promenne

# pracujte s promennou dis (vzdalenost k pracovnim centrum)
dis <- Boston$dis
hist(dis,col="skyblue",border="darkblue",freq=F,main="Histogram",xlab="Vzdalenost v km",ylab="Hustota")
  # nesymetricke rozdeleni s kandidaty na odlehle hodnoty

# Jak toto ovlivni charakteristiky polohy
mean(dis)
median(dis)
mean(dis,trim=0.1)
  # useknuty prumer, z kazde strany usekne 10% hodnot
  # kolik procent hodnot je dobre useknout?
sum(dis>=9)/length(dis)
mean(dis, trim = 0.02)
HuberM(dis)  
  library(asbio)
  huber.mu(dis)
     # Huberuv M-estimator (maximalne verohodny odhad stredni hodnoty)

# charakteristiky variability
sd(dis)
IQR(dis)
MAD(dis)

# kvartilovy koeficient sikmosti (Bowley)
Q1<-quantile(dis,0.25)
Q3<-quantile(dis,0.75)
(Q3+Q1-2*median(dis))/(Q3-Q1)
# porovnejte s klasickou sikmosti
Skew(dis)

# oktilovy koeficient spicatosti (Moorse)
Q18<-quantile(dis,1/8)
Q28<-quantile(dis,2/8)
Q38<-quantile(dis,3/8)
Q48<-quantile(dis,4/8)
Q58<-quantile(dis,5/8)
Q68<-quantile(dis,6/8)
Q78<-quantile(dis,7/8)
((Q78-Q58)+(Q38-Q18))/(Q68-Q28)
# porovnejte s klasickou spicatosti
Kurt(dis)

# Vyzkousejte pro promennou rm a nox
rm <- Boston$rm
hist(rm,col="skyblue",border="darkblue",freq=F,main="Histogram",xlab="počet pokojů",ylab="Hustota")
boxplot(rm, col = "mistyrose",border = "darkred", main="Krabicový graf")
boxplot(rm, col = "mistyrose",border = "darkred", main="Krabicový graf", range = 3)
mean(rm)
median(rm)
mean(rm, trim = 0.1)
sum(rm<=4.5)/length(rm)
mean(rm, trim = 0.01)
HuberM(rm)
sd(rm)
IQR(rm)
MAD(rm)
Kurt(rm)

nox <- Boston$nox
hist(nox,col="skyblue",border="darkblue",freq=F,main="Histogram",xlab="oxidy dusíku",ylab="Hustota")
boxplot(nox, col = "mistyrose",border = "darkred", main="Krabicový graf")
mean(nox)
median(nox)
mean(nox, trim = 0.1)
sum(nox>=0.75)/length(nox)
mean(nox, trim = 0.05)
HuberM(nox)
sd(nox)
IQR(nox)
MAD(nox)
##########################
### Vztah dvou promennych

## Pro popsani vztahu dvou kategorickych promennych se pouziva kontingencni tabulka
# Souvisi spolu dane a pristupnost k dalnicim? (promenne tax.k a rad)

prom2<-Boston.kat$rad
prom3<-Boston.kat$tax.k
(tab<-table(prom2,prom3))
# mozna lepe videt z relativnich cetnosti
addmargins(prop.table(tab))
addmargins(prop.table(tab,1))
addmargins(prop.table(tab,2))
  # ktery typ relativnich cetnosti se pro popis zavislosti hodi nejvic?
  # Jak byste vztah popsali?
plot(as.factor(prom2)~as.factor(prom3),col=2:7)
plot(as.factor(prom3)~as.factor(prom2),col=2:7)
# Souvisi spolu dane a pozice u reky? (tax.k, chas)
prom3<-Boston.kat$chas
prom2<-Boston.kat$tax.k
(tab<-table(prom2,prom3))
# mozna lepe videt z relativnich cetnosti
addmargins(prop.table(tab))
addmargins(prop.table(tab,1))
addmargins(prop.table(tab,2))
plot(as.factor(prom2)~as.factor(prom3),col=2:7)
plot(as.factor(prom3)~as.factor(prom2),col=2:7)


data(bacteria)
bac <- bacteria$y
trt <- bacteria$trt
plot(bac~trt, col=2:3)
(tab<-table(bac,trt))
addmargins(prop.table(tab,1))
# A co dane a rezidencni zony? (tax.k, land.z)

#############################
## Vztah dvou ciselnych promennych popisujeme pomoci
#   bodoveho (rozptyloveho) grafu, korelacniho koeficientu, korelacni tabulky

# Jaky je vztah mezi promennymi rm a age?
prom1 <- Boston$age
prom2 <- Boston$rm
plot(prom1 ~ prom2,pch=19,main="Rozptylovy graf",xlab="Rm",ylab="Age")
  # Co z grafu vidite?

cov(prom1,prom2,use="complete.obs")
# co Vam rika kovariance
cor(prom1,prom2,use="complete.obs")
# a co korelace

# A co je korelacni tabulka?
prom1.c <- factor(ifelse(prom1<=20, '<20', ifelse(prom1<=40, '20-40', 
                                                  ifelse(prom1<=60, '40-60', ifelse(prom1<=80, '60-80', '>80')))),
                  levels=c("<20","20-40","40-60","60-80",">80")) 
prom2.c <- factor(ifelse(prom2<=5, '<5', ifelse(prom2<=6, '5-6', 
                                                  ifelse(prom2<=7, '6-7', ifelse(prom2<=80, '7-8', '>8')))),
                  levels=c("<5","5-6","6-7","7-8",">8"))
table(prom1.c,prom2.c)

# Popiste souvislost rm a medv (bez korelacni tabulky)
prom1 <- Boston$medv
prom2 <- Boston$rm
plot(prom1 ~ prom2,pch=19,main="Rozptylovy graf",xlab="Rm",ylab="Medv")
# Co z grafu vidite?

cov(prom1,prom2,use="complete.obs")
# co Vam rika kovariance
cor(prom1,prom2,use="complete.obs")
# Najdete promenne s nejsilnejsim vztahem
# pouzijte pouze ciselne promenne a ykuste na ne pouzit prikazy pairs a cor na vsechny dohromady
ciselne <- data.frame(Boston$crim, Boston$nox, Boston$rm, Boston$lstat, Boston$medv)
cor(ciselne)
pairs(ciselne)

#############################
## Normalita dat - QQ plot

# Ohodnotte normalitu promenne rm
# vyska
rm <- Stulong$rm

# histogram - hodi se ho kreslit v "hustote"
hist(rm,col="skyblue",border="darkblue",main="Histogram",ylab="Hustota",
     xlab="rm", freq=F)
# Ma tvar Gaussovy krivky?
curve(dnorm(x,mean(rm),sd(rm)),from=min(rm),to=max(rm), add=T,col=2)
# prikresleni hustoty odpovidajiciho normalniho rozdeleni 

# Sikmost, spicatost
Skew(rm)
Kurt(rm)
# jsou nulove?

# Pravdepodobnostni graf
PlotQQ(rm,pch=19,cex=0.5)
qqnorm(rm,pch=19, cex=0.5);qqline(rm,distribution=qnorm,col=2,lwd=2)
# graf bez pouziti knihovny DescTools
# jak cist pravdepodobnostni graf
# vyska ma priblizne normalni rozdeleni

# Maji promenne nox a medv normalni rozdeleni?

################################
### Testovani hypotez
# Test normality
# napr. Shapiro-Wilkuv test

## Ma rm normalni rozdeleni?
# Testovane hypotezy
# nulova hypoteza H0: data maji normalni rozdeleni
# jen jedna moznost
# alternativni hypoteza H1: data nemaji normalni rozdeleni
# vice moznosti (vsechna ostatni rozdeleni)

# test
shapiro.test(rm)
# statisticke testy obecne nefunguji pri velkem poctu pozorovani
# nejlepe funguji na vzorku cca 100 hodnot

# vyhodnoceni testu
# p-value <= alpha (= 0.05) -> zamitam H0, plati H1
# p-value > alpha (= 0.05) -> nezamitam H0 

# Interpretace testu
# kdyz nezamitam H0 -> data maji priblizne normalni rozdeleni
# kdyz zamitam H0 -> data nemaji normalni rozdeleni

## Ma normalni rozdeleni nox, medv?
## A co vyber z normalniho rozdeleni
vyb <- rnorm(200)


