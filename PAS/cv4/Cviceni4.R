# Nacteni databaze Stulong.RData
# oprava nazvu promennych v souboru
names(Stulong)<-c("ID","vyska","vaha","syst1","syst2","chlst","vino","cukr","bmi",
                  "vek","KOURrisk","Skupina","VekK")

# aktivace knihovny s popisnymi statistikami
library(DescTools)

##########################
### Hledani modusu
## Spojita promenna

# pracujte s vyskou jako se spojitou promennou a hledejte modus
vyska <- Stulong$vyska
# histogram - budeme prokladat hustotu, je treba ji tedy uvazovat i na y-ove ose
hist(vyska,col="skyblue",border="darkblue",main="Histogram",ylab="Hustota",
     xlab="Vyska v cm",freq=F)
  # mozno pouzit i vice sloupcu
  hist(vyska,col="skyblue",border="darkblue",main="Histogram",ylab="Hustota",
     xlab="Vyska v cm",breaks=15,freq=F)

# prolozime jadrovy odhad hustoty
(jadro <- density(vyska))
lines(jadro,col=2)
# chceme hladsi funkci? musime zmenit bandwidth
(jadro <- density(vyska,bw=3))
lines(jadro,col=4)
  # jaky bandwidth se Vam jevi jako optimalni?

# kde ma odhad hustoty maximum?
jadro$x[which.max(jadro$y)]

# najdete modus pro spojitou promennou bmi
bmi <- Stulong$bmi
hist(bmi,col="skyblue",border="darkblue",main="Histogram",ylab="Hustota",
     xlab="BMI",freq=F)
(jadro <- density(bmi))
lines(jadro,col=2)
jadro$x[which.max(jadro$y)]
# pracujte s vahou jako se spojitou promennou a najdete modus
vaha <- Stulong$vaha
hist(vaha,col="skyblue",border="darkblue",main="Histogram",ylab="Hustota",
     xlab="Váha v kg",freq=F)
(jadro <- density(vaha,bw=4))
lines(jadro,col=3)
jadro$x[which.max(jadro$y)]
##############################
### Hledani modusu
## Diskretni promenna s mnoha kategoriemi

# Uvazujme vek ako diskretni promennou
vek <- Stulong$vek
# histogram - nyni muze byt v cetnostech
hist(vek,col="skyblue",border="darkblue",main="Histogram",ylab="Absolutni cetnosti",
     xlab="Vek v rocich")
  # Kolik bude modu? Ve kterem intervalu je modus?
  # podle vzorce modus = A + h*d0/(d0+d1)
hist(vek, plot=F)
  # pokud si nenecham histogram nakreslit, ale vypsat ...
meze<-hist(vek, plot=F)$breaks
pocty<-hist(vek, plot=F)$counts
m<-which.max(pocty)
  # kolikaty sloupec je nejvyssi
(A<-meze[m])
(h<-meze[2]-meze[1])
(d0<-pocty[m]-pocty[m-1])
(d1<-pocty[m]-pocty[m+1])
(modus<- A + h*d0/(d0+d1))
  # modus podle vzorce

# Urcete modus pro prvni hodnotu systolickeho tlaku (syst1)
tlak <- Stulong$syst1
hist(tlak,col="skyblue",border="darkblue",main="Histogram",ylab="Absolutni cetnosti",
     xlab="Systolicky tlak")
hist(tlak, plot=F)
meze<-hist(tlak, plot=F)$breaks
pocty<-hist(tlak, plot=F)$counts
m<-which.max(pocty)
(A<-meze[m])
(h<-meze[2]-meze[1])
(d0<-pocty[m]-pocty[m-1])
(d1<-pocty[m]-pocty[m+1])
(modus<- A + h*d0/(d0+d1))
# Urcete modus pro cukr
  # pokud sousedni sloupec k tomu nejvyssimu chybi, bere se jeho cetnost jako 0
cukr <- Stulong$cukr
hist(cukr,col="skyblue",border="darkblue",main="Histogram",ylab="Absolutni cetnosti",
     xlab="cukr")
hist(cukr, plot=F)
meze<-hist(cukr, plot=F)$breaks
pocty<-hist(cukr, plot=F)$counts
m<-which.max(pocty)
(A<-meze[m])
(h<-meze[2]-meze[1])
(d0<-pocty[m]-0)
(d1<-pocty[m]-pocty[m+1])
(modus<- A + h*d0/(d0+d1))

##########################
## Odlehle hodnoty

# Ma promenna vyska odlehle hodnoty
vyska <- Stulong$vyska
# Je promenna symetricka nebo sesikmena? 
#   A je jeji rozdeleni jednovrcholove nebo vicevrcholove?
hist(vyska,col="skyblue",border="darkblue",main="Histogram",ylab="Absolutni cetnosti",
     xlab="Vyska v cm",breaks=15)
# A co na odlehle hodnoty rika boxplot?
boxplot(vyska,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Vyska v cm")
  # Vidime adepty na odlehle hodnoty
  # Podle jakeho pravidla boxplot odlehle hodnoty hleda?
# Zprisnime pravidlo
boxplot(vyska,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Vyska v cm", range=3)
  # Do prisnejsiho pravidla se vejdou vsechny hodnoty

# Jake mame hranice pro odlehle hodnoty?
# 1.5 nasobek mezikvartiloveho rozpeti - adept na odlehle pozorovani
(DM <- quantile(vyska,0.25) - 1.5*IQR(vyska))
vyska[vyska<DM]
  # dolni adepti na odlehle pozorovani
(HM<-quantile(vyska,0.75) + 1.5*IQR(vyska))
vyska[vyska>HM]
  # horni adepti na odlehle pozorovani

# 3 nasobek mezikvartiloveho rozpeti - odlehla pozorovani
(DM<-quantile(vyska,0.25) - 3*IQR(vyska))
vyska[vyska<DM]
  # dolni odlehla pozorovani
(HM<-quantile(vyska,0.75) + 3*IQR(vyska))
vyska[vyska>HM]
  # horni odlehla pozorovani

# 3 nasobek smerodatne odchylky - adepti na odlehle pozorovani
(DM<-mean(vyska) - 3*sd(vyska))
vyska[vyska<DM]
  # dolni adepti na odlehle pozorovani
(HM<-mean(vyska) + 3*sd(vyska))
vyska[vyska>HM]
  # horni adepti na odlehle pozorovani

# 4 nasobek smerodatne odchylky - odlehle pozorovani
(DM<-mean(vyska) - 4*sd(vyska))
vyska[vyska<DM]
  # dolni odlehla pozorovani
(HM<-mean(vyska) + 4*sd(vyska))
vyska[vyska>HM]
  # horni odlehla pozorovani

# A jak jsou na tom odlehla pozorovani pro vahu?
vaha <- Stulong$vaha
# Je promenna symetricka nebo sesikmena? 
#   A je jeji rozdeleni jednovrcholove nebo vicevrcholove?
hist(vaha,col="skyblue",border="darkblue",main="Histogram",ylab="Absolutni cetnosti",
     xlab="Vaha v kg",breaks=15)
# A co na odlehle hodnoty rika boxplot?
boxplot(vaha,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Vaha v kg")
# dolni adepti na odlehle pozorovani
(HM<-quantile(vaha,0.75) + 1.5*IQR(vaha))
vaha[vaha>HM]
# dolni odlehla pozorovani
(HM<-quantile(vaha,0.75) + 3*IQR(vaha))
vaha[vaha>HM]
# dolni adepti na odlehle pozorovani
(HM<-mean(vaha) + 3*sd(vaha))
vaha[vaha>HM]
# dolni odlehla pozorovani
(HM<-mean(vaha) + 4*sd(vaha))
vaha[vaha>HM]
# A co prvni hodnota systolickeho tlaku?
syst <- Stulong$syst1
hist(syst,col="skyblue",border="darkblue",main="Histogram",ylab="Absolutni cetnosti",
     xlab="Systolicky tlak")
  # zde uz je sesikmeni vyraznejsi, zkusime symetrizujici transformaci
  # pro kladna data lze pouzit prirozeny logaritmus
  # pro data, kde jsou i zaporne hodnoty, je transformace sqrt((x-min)/(max-min))
ln.syst <- log(syst)
hist(ln.syst,col="skyblue",border="darkblue",main="Histogram",ylab="Absolutni cetnosti",
     xlab="Logaritmus systolickeho tlak")
  # pomohlo to? Odlehle hodnoty se hledaji u symetrizovane promenne

boxplot(syst,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Systolicky tlak")
  # krabicovy graf pro puvodni promennou
  # pokud chci dosahnout tykadlem na vsechny samostatne body,
  #   patricne navysim parametr range
  boxplot(syst,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Systolicky tlak",range=10)
boxplot(ln.syst,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Logaritmus systolickeho tlaku")
(HM<-quantile(ln.syst,0.75) + 1.5*IQR(ln.syst))
exp(HM)
(HM<-quantile(ln.syst,0.75) + 3*IQR(ln.syst))
exp(HM)
(HM<-mean(ln.syst) + 3*sd(ln.syst))
exp(HM)
(HM<-mean(ln.syst) + 4*sd(ln.syst))
exp(HM)
exp(ln.syst[ln.syst>HM])

  # krabicovy graf pro transformovanou promennou
  # mame kandiday na odlehle hodnoty

# A co pro cukr?
cukr <- Stulong$cukr
hist(cukr,col="skyblue",border="darkblue",main="Histogram",ylab="Absolutni cetnosti",
     xlab="cukr")
cukr.t <- sqrt((cukr-min(cukr))/(max(cukr)-min(cukr)))
hist(cukr.t,col="skyblue",border="darkblue",main="Histogram",ylab="Absolutni cetnosti",
     xlab="Transformovaný cukr")
boxplot(cukr.t,col="yellow",border="orange3",main="Krabicovy graf",
        ylab="Transformovaný cukr",range=3)
#########################
## Jsou data normalne rozlozena?
# vyska
vyska <- Stulong$vyska
  
# histogram - hodi se ho kreslit v "hustote"
hist(vyska,col="skyblue",border="darkblue",main="Histogram",ylab="Hustota",
     xlab="Vyska v cm", freq=F)
  # Ma tvar Gaussovy krivky?
  curve(dnorm(x,mean(vyska),sd(vyska)),from=min(vyska),to=max(vyska), add=T,col=2)
    # prikresleni hustoty odpovidajiciho normalniho rozdeleni 
  
# Sikmost, spicatost
Skew(vyska)
Kurt(vyska)
  # jsou nulove?

# Pravdepodobnostni graf
PlotQQ(vyska,pch=19,cex=0.5)
  qqnorm(vyska,pch=19, cex=0.5);qqline(vyska,distribution=qnorm,col=2,lwd=2)
    # graf bez pouziti knihovny DescTools
  # jak cist pravdepodobnostni graf
  # vyska ma priblizne normalni rozdeleni

# Ma bmi normalni rozdeleni?
  bmi <- Stulong$bmi
  
  # histogram - hodi se ho kreslit v "hustote"
  hist(bmi,col="skyblue",border="darkblue",main="Histogram",ylab="Hustota",
       xlab="bmi", freq=F)
  # Ma tvar Gaussovy krivky?
  curve(dnorm(x,mean(bmi),sd(bmi)),from=min(bmi),to=max(bmi), add=T,col=2)
  # prikresleni hustoty odpovidajiciho normalniho rozdeleni 
  
  # Sikmost, spicatost
  Skew(bmi)
  Kurt(bmi)
  # jsou nulove?
  
  # Pravdepodobnostni graf
  PlotQQ(bmi,pch=19,cex=0.5)
# Ma prvni hodnota systolickeho tlaku normalni rozdeleni?
  tlak <- Stulong$syst1
  
  # histogram - hodi se ho kreslit v "hustote"
  hist(tlak,col="skyblue",border="darkblue",main="Histogram",ylab="Hustota",
       xlab="tlak", freq=F)
  # Ma tvar Gaussovy krivky?
  curve(dnorm(x,mean(tlak),sd(tlak)),from=min(tlak),to=max(tlak), add=T,col=2)
  # prikresleni hustoty odpovidajiciho normalniho rozdeleni 
  
  # Sikmost, spicatost
  Skew(tlak)
  Kurt(tlak)
  # jsou nulove?
  
  # Pravdepodobnostni graf
  PlotQQ(tlak,pch=19,cex=0.5)
################################
### Testovani hypotez
# Test normality
# napr. Shapiro-Wilkuv test
  
## Ma vyska normalni rozdeleni?
# Testovane hypotezy
# nulova hypoteza H0: data maji normalni rozdeleni
  # jen jedna moznost
# alternativni hypoteza H1: data nemaji normalni rozdeleni
  # vice moznosti (vsechna ostatni rozdeleni)

# test
shapiro.test(sample(vyska,100))
  # statisticke testy obecne nefunguji pri velkem poctu pozorovani
  # nejlepe funguji na vzorku cca 100 hodnot
  
# vyhodnoceni testu
  # p-value <= alpha (= 0.05) -> zamitam H0, plati H1
  # p-value > alpha (= 0.05) -> nezamitam H0 
  
# Interpretace testu
  # kdyz nezamitam H0 -> data maji priblizne normalni rozdeleni
  # kdyz zamitam H0 -> data nemaji normalni rozdeleni
  
## Ma normalni rozdeleni BMI, vaha?