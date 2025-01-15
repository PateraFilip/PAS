### Testovani hypotez
# Pred testem musime stanovit
#   testovane hypotezy: nulovou (H0) a alternativni (H1)
#   hladinu vyznamnosti (nejcasteji alfa = 0.05)
# Vyhodnoceni pomoci p-hodnoty
#   p-hodnota <= alfa => H0 zamitame, plati H1
#   p-hodnota > alfa => H0 nezamitame

### Chi-kvadrat test nezavislosti dvou kategorickych promennych

## Souvisi spolu barva vlasu a pohlavi?
# testovane hypotezy
#   H0: barva vlasu a pohlavi spolu nesouvisi
#   H1: barva vlasu a pohlavi spolu souvisi
(tab <- HairEyeColor[,1,])
  # tabulka absolutnich cetnosti

# prikaz pro vypocet chi-kvadrat testu je chisq.test
chisq.test(tab)
  # zakladni volani testu
  # test porovnava pozorovane a ocekavane cetnosti
chisq.test(tab)$obs
chisq.test(tab)$exp
  # predpokladem chi-kvadrat testu je, ze vsechny ocekavane cetnosti jsou vetsi nez 5

# neni-li splnen predpoklad, pouziva se Fisheruv exaktni test
fisher.test(tab)
  # p-hodnota testu vysla 0.9174 > alfa => nezamitame H0
  # souvislost mezi barvou vlasu a pohlavim se neprokazala

## Souvisi spolu barva vlasu a barva oci u muzu?
(tab <- HairEyeColor[,,1])
chisq.test(tab)
fisher.test(tab)

## Souvisi spolu pocet valcu a pocet rychlosti u osobnich vozu?
data("mtcars")
auta <- mtcars
valce <- auta$cyl
rychlosti <- auta$gear
  # nacteni dat

# vstupem do chi-kvadrat testu muze byt tabulka absolutnich cetnosti, nebo primo dvojice promennych
chisq.test(valce, rychlosti)
fisher.test(valce, rychlosti)
plot(as.factor(valce)~as.factor(rychlosti), col=2:4, main="Souvislost poctu valcu a poctu rychlosti")
## Souvisi spolu typ prevodovky a pocet rychlosti?
prevod <- auta$am
rychlosti <- auta$gear

###############################
### Vypocty pravdepodobnosti

### Diskretni rozdeleni
## Binomicke rozdeleni: n - pocet pokusu, p - pst uspechu
# pbinom(k,n,p) - distribucni funkce
# dbinom(k,n,p) - pravdepodobnostni funcke
#cv 10
1-pbinom(34, 100, 0.25)
#cv 7a
1-pbinom(1, 100, 0.035)
#cv 7b
sum(dbinom(3:5, 100, 0.035))
pbinom(5, 100, 0.035)-pbinom(2, 100, 0.035)

## Hypergeometricke rozdeleni: w - pocet bilych kouli v osudi, b - pocet cernych kouli v osudi
#   n - pocet kouli tazenych z osudi
# phyper(k,w,b,n) - distribucni funkce
# dhyper(k,w,b,n) - pravdepodobnostni funcke
# cv 10
1 - phyper(34, 100, 300, 100) #nelze
#cv 16
1-phyper(2, 15, 10, 5)

 ## Geometricke rozdeleni: p - pravdepodobnost uspechu
# pgeom(k,p) - distribucni funkce
# dgeom(k,p) - pravdepodobnostni funcke
#cv 4a
dgeom(5, 0.2)
#cv 4b
1-pgeom(4, 0.2)
#cv 4c
pgeom(4, 0.2)
#cv 4d
pgeom(5, 0.2)-pgeom(1, 0.2)

## Poissonovo rozdeleni: lambda - stredni hodnota
# ppois(k,lambda) - distribucni funkce
# dpois(k,lambda) - pravdepodobnostni funcke
#cv 6a
dpois(5, 5)
#cv 6b
1-ppois(4, 5)

### Spojita rozdeleni
## Normalni rozdeleni: mu - stredni hodnota, sigma - smerodatna odchylka
# pnorm(x,mu,sigma) - distribucni funkce
# qnorm(p,mu, sigma) - kvantilova funkce

# hustota - vyska dospelych muzu
curve(dnorm(x,180,7),from=150,to=210, main="Hustota N(180, 49)",col="red",ylab="Hustota")
# distribucni funkce - vyska dospelych muzu
curve(pnorm(x,180,7),from=150,to=210, main="Distribucni funkce N(180, 49)",col="purple",ylab="Hustota")
# pravdepodobnost, ze nahodne vybrany muz bude mensi nez 170 cm
oldpar <- par(mfrow=c(1,2))
curve(dnorm(x,180,7),from=150,to=210, main="Hustota N(180, 49)")
lines(c(0,170),c(0,0),lwd=3,col="green") 
xx <- seq(150,170,length.out=101)
polygon(c(150,xx,170),c(0,dnorm(xx,180,7),0),col="green")
# umime-li zmerit velikost zelene plochy, mame pst :)

curve(pnorm(x,180,7),from=150,to=210, main="Distr. fce N(180, 49)")
lines(c(170,170),c(0,pnorm(170,180,7)),col="green",lty=2,lwd=2)
lines(c(0,170),c(pnorm(170,180,7),pnorm(170,180,7)),col="green",lty=2,lwd=2)
# hodnotu lze vycist z distribucni funkce
par(oldpar)


## Lognormalni rozdeleni: mu - stredni hodnota, sigma - smerodatna odchylka
# plnorm(x,mu,sigma) - distribucni funkce
# qlnorm(p,mu,sigma) - kvantilova funkce

## Exponencialni rozdeleni: int - intenzita
# pexp(x,int) - distribucni funkce
# qexp(p,int) - kvantilova funkce
