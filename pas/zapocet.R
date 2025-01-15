library(readxl)
library(DescTools)
data <- read_excel("./prij.xlsx")


summary(data)

sorted_ss3 <- sort(data$ss3)

Skew(data$celprij)
Skew(data$matprij)
Kurt(data$celprij)
Kurt(data$matprij)

#2)
# V?po?et medi?nu
median_value <- median(sorted_ss3)
summary(sorted_ss3)

# Indexy pro prvn? a posledn? hodnotu v medi?nov?m intervalu
first_index <- max(which(sorted_ss3 <= median_value))
last_index <- min(which(sorted_ss3 >= median_value))

# Medi?nov? interval
median_interval <- sorted_ss3[first_index:last_index]

# Meze intervalu
lower_limit <- min(median_interval)
upper_limit <- max(median_interval)

# V?pis intervalu a mezn?ch hodnot
cat("Medi?nov? interval:", median_interval, "\n")
cat("Doln? mez:", lower_limit, "\n")
cat("Horn? mez:", upper_limit, "\n")

#3)
#Modus
zemprij <- data$zemprij

hist(zemprij, main="Histogram zemprij", xlab="Hodnota", ylab="Frekvence", col="lightblue", border="black")

# P?id?n? m???ky
grid()

# V?po?et m?dy
mode <- as.numeric(names(table(zemprij))[which.max(table(zemprij))])

# V?pis m?dy
cat("Modus:", mode, "\n")

#4)

tabulka <- table(data$Pohlavi, data$Obor)

barplot(tabulka, beside = TRUE, col = c("lightblue", "lightgreen"), legend = TRUE)

chi_square_test <- chisq.test(kont_tab)
print(chi_square_test)

#5)

correlation_coefficient <- cor(data$ss3, data$celprij)
cat("Korela?n? koeficient:", correlation_coefficient, "\n")

#6)

kvartily1 <- quantile(data$matzem, probs = c(0.25, 0.5, 0.75))
kvartily2 <- quantile(data$geol, probs = c(0.25, 0.5, 0.75))

print(kvartily1)
print(kvartily2)

summary(data$matzem)
summary(data$geol)

#7)

hist(data$ss3, main="Histogram", xlab="Hodnota")
qqnorm(data$ss3)
qqline(data$ss3)
print(shapiro.test(data$ss3))

#8) 

result <- t.test(data$celprij)
ci <- result$conf.int
print(ci)

#9)

lst <- c()

for(i in data$Pohlavi){
  if (i == "m")
    i = 1
  if (i == "z")
    i = 0
}

print(data$Pohlavi)

MeanCI(data$celprij)

#9)

i = 1
for (i in 1:length(data$Pohlavi))
{
  if (data$Pohlavi[i] == 'm')
  {
    data$Pohlavi[i] <- 1
  }
  if(data$Pohlavi[i] == 'z')
  {
    data$Pohlavi[i] <- 2
  }
}

print(data$Pohlavi)

correlation_coefficient_2 <- cor(as.numeric(data$Pohlavi), data$celprij)
cat("Korela?n? koeficient:", correlation_coefficient_2, "\n")


prom1 <- data$celprij
prom2 <- data$Pohlavi
tapply(prom1,prom2,MeanCI)
MeanDiffCI(prom1~prom2)
#10)
# A)
# P(x=12)
dbinom(12,15,0.93) # 0.06532823

# B)
# P(x>=8) -> 1-P(x<8) -> 1-P(x<=7)
1-pbinom(7,10,0.93) # 0.9716579

#11)
# A)
# P(x<170)
pnorm(170,180,7) # 0.07656373

# B)
# P(x>195) -> 1-P(x<=195)
1-pnorm(195,180,7) # 0.01606229

# C)
# P(x>195) -> 1-P(x<=195)
# P(x<170)
(1-pnorm(195,180,7)) * (pnorm(170,180,7)) # 0.001229788

tabule <- table(data$Pohlavi, data$celprij)



mean(data$celprij)
sd(data$celprij)

confint(data$celprij, level = 0.95)