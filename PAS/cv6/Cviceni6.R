
# nactete databazi Policie.RData

library(DescTools)
library(TeachingDemos)
# aktivuje knihovny

#######################
### Hodnoceni normality
# Graficke i ciselne testy normality

# Ohodnotte normalitu promenne vaha
vaha <- Policie$weight

# histogram
hist(vaha, main="Histogram",xlab="Vaha v kg",ylab="Absolutni cetnosti",
    col="skyblue",border="darkblue")
  # s hustotou normalniho rozdeleni 
  hist(vaha ,main="Histogram",xlab="Vaha v kg",ylab="Hustota",
    col="skyblue",border="darkblue",freq=F)
    # je treba vykreslit histogram prepocteny na hustotu
  lines(x<-seq(50,140,by=0.2),dnorm(x,mean(vaha),sd(vaha)),col=2)

# Graficky test: Q-Q plot - Quantile Comparison plot
PlotQQ(vaha, pch=19)
Skew(vaha)
Kurt(vaha)
    # body lezi priblizne na primce, data maji priblizne normalni rozdeleni

# Pro "rozumne" velka data (od 50 do cca 300 pozorovani)
#   je mozne vypocitat i ciselny test normality
# Testujeme
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
shapiro.test(vaha)
  # p-hodnota 0.2045 > alfa 0.05 => H0 nezamitame
  # data maji priblizne normalni rozdeleni
  LillieTest(vaha)
  AndersonDarlingTest(vaha,"pnorm",mean=mean(vaha),sd=sd(vaha))
     # testu existuje vice
  
## Otestujte normalitu promenne fat (procento tuku), a promenne pulse (puls).
  tuk <- Policie$fat
  
  hist(tuk, main="Histogram",xlab="Procento tuku",ylab="Absolutni cetnosti",
       col="skyblue",border="darkblue")
  
  PlotQQ(tuk, pch=19)
  shapiro.test(tuk)
  
  puls <- Policie$pulse
  
  hist(puls, main="Histogram",xlab="Puls bpm",ylab="Absolutni cetnosti",
       col="skyblue",border="darkblue")
  
  PlotQQ(puls, pch=19)
  shapiro.test(puls)
#############################
### Vztah dvou ciselnych promennych 

## Jaky je vztah mezi vahou a vyskou?
# Graficky pomoci bodoveho grafu
vaha <- Policie$weight
vyska <- Policie$height
plot(vaha ~ vyska, pch=19,main="Rozptylovy graf",xlab="Vyska v cm",ylab="Vaha v kg")
  # Co z grafu vidite?
  abline(lm(vaha ~ vyska), col=2, lwd=2)
    # je mozne prikreslit i primku linearni zavislosti
  
# Korelacni koeficient
cor(vaha, vyska, use="complete.obs")
  # co o zavislosti vite?

## Souvisi spolu pulse a vaha?
puls <- Policie$pulse

# Rozptylovy graf
plot(puls ~ vaha, pch=19,main="Rozptylovy graf",xlab="Vaha v kg",ylab="Puls bpm")
  abline(lm(puls ~ vaha), col=2, lwd=2)

# Korelacni koeficient
cor(puls, vaha, use="complete.obs")
  # je tato zavislost vyznamna?

## Test o vyznamnosti korelacniho koeficientu (test o linearni zavislosti)
# Testovane hypotezy
#   H0: promenne spolu linearne nesouvisi (cor = 0) vs. H1: promenne spolu souvisi (cor <> 0)  
cor.test(puls, vaha)
  # p-hodnota 0.0862 > alfa 0.05 => H0 nezamitam
  # Na hladine vyznamnosti 5% se neprokazala souvislost mezi vahou a pulsem.
  #   A co na hladine vyznamnosti 10%?

# Predpokladem pouziti Pearsonova korelacniho koeficientu je 
#   normalita obou promennych - mame? 

## Souvisi spolu vyska a procento tuku?
plot(vyska ~ tuk, pch=19,main="Rozptylovy graf",xlab="Procento tuku",ylab="Výška v cm")
abline(lm(vyska ~ tuk), col=2, lwd=2)
cor.test(vyska,tuk)
cor.test(vyska,tuk, method = "spearman")
## Souvisi spolu reakcni doba a procento tuku?
reakce <- Policie$react
plot(reakce ~ tuk, pch=19,main="Rozptylovy graf",xlab="Procento tuku",ylab="Reakční doba")
abline(lm(reakce ~ tuk), col=2, lwd=2)
cor.test(reakce,tuk)
cor.test(reakce,tuk, method = "spearman")
#####################################
### Priklady na pravdepodobnost

### Diskretni rozdeleni
## Binomicke rozdeleni: n - pocet pokusu, p - pst uspechu
# pbinom(k,n,p) - distribucni funkce
# dbinom(k,n,p) - pravdepodobnostni funcke

n <- 13
p <- 120/375
dbinom(6, n, p)

# Pravdepodobnostni funkce k prikladu 2
plot(0:13,dbinom(0:13,13,120/375),col="green3",type="h", 
     main="Pravdepodobnostni funkce Bi(13,0.32)",
     xlab="Pocet uspechu",ylab="p(x)")
points(0:13,dbinom(0:13,13,120/375),col="green3",pch=19)

# Distribucni funkce k prikladu 2
plot(pbinom(0:13,13,120/375), type = "s", col="purple",
     main = "Distribucni funkce Bi(13,0.32)",
     xlab = "Pocet uspechu", ylab = "F(x)")
# a)
1 - pbinom(6, n, p)
sum(dbinom(7:13, n, p))

# b)
pbinom(2, n, p)

# c) 
dbinom(0, n, p)

# d)
dbinom(13, n, p)
# Priklady 1, 7

## Hypergeometricke rozdeleni: w - pocet bilych kouli v osudi, b - pocet cernych kouli v osudi
#   n - pocet kouli tazenych z osudi
# phyper(k,w,b,n) - distribucni funkce
# dhyper(k,w,b,n) - pravdepodobnostni funcke

# Pravdepodobnostni funkce k prikladu 5
plot(5:20,dhyper(5:20,35,15,20),col="green3",type="h", 
     main="Pravdepodobnostni funkce Hy(35,15,20)",
     xlab="Pocet uspechu",ylab="p(x)")
points(5:20,dhyper(5:20,35,15,20),col="green3",pch=19)

# Distribucni funkce k prikladu 5
plot(5:20,phyper(5:20,35,15,20), type = "s", col="purple",
     main = "Distribucni funkce Hy(35,15,20)",
     xlab = "Pocet uspechu", ylab = "F(x)")
# a)
dhyper(15, 35, 15, 20)
# Priklady 16, 21


## Geometricke rozdeleni: p - pravdepodobnost uspechu, k - pocet neuspechu pred prvnim uspechem
# pgeom(k,p) - distribucni funkce
# dgeom(k,p) - pravdepodobnostni funcke

# Pravdepodobnostni funkce k prikladu 3
plot(0:10,dgeom(0:10,0.65),col="green3",type="h", 
     main="Pravdepodobnostni funkce Hy(0.65)",
     xlab="Pocet uspechu",ylab="p(x)")
points(0:10,dgeom(0:10,0.65),col="green3",pch=19)

# Distribucni funkce k prikladu 3
plot(0:10,pgeom(0:10,0.65), type = "s", col="purple",
     main = "Distribucni funkce Ge(0.65)",
     xlab = "Pocet uspechu", ylab = "F(x)")

# Priklady 4

## Poissonovo rozdeleni: lambda - stredni hodnota
# ppois(k,lambda) - distribucni funkce
# dpois(k,lambda) - pravdepodobnostni funcke

# Pravdepodobnostni funkce k prikladu 6
plot(0:15,dpois(0:15,5),col="green3",type="h", 
     main="Pravdepodobnostni funkce Po(5)",
     xlab="Pocet uspechu",ylab="p(x)")
points(0:15,dpois(0:15,5),col="green3",pch=19)

# Distribucni funkce k prikladu 6
plot(0:15,ppois(0:15,5), type = "s", col="purple",
     main = "Distribucni funkce Po(5)",
     xlab = "Pocet uspechu", ylab = "F(x)")

# Priklady 14

## Negativni binomicke rozdeleni: n - pocet uspechu, p - pst uspechu
# pnbinom(k,n,p) - distribucni funkce
# dnbinom(k,n,p) - pravdepodobnostni funcke


