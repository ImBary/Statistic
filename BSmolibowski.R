
#policja
#ggplot2
library(ggplot2)
library(fitdistrplus)
library(moments)
ggplot(data=pce_d,aes(Data,Zamkniecie))+
  geom_point(colour="blue")


hist(pce_d$Zamkniecie, prob = TRUE, xlab = "zamkniecie")

#plotdist(pce_d$Zamkniecie, histo = TRUE, demp = TRUE)

#2
#odchylenie standardowe

skewness(pce_d$Zamkniecie) #skosnosc 0.7 mowi nam ze prawe ramie jest wydluzone
sd(pce_d$Zamkniecie)#odchylenie standardowe 
kurtosis(pce_d$Zamkniecie) #kurtoza > 3 wskazuje na spora intestywnosc wartosci skrajnych
mean(pce_d$Zamkniecie)#srednia
var(pce_d$Zamkniecie)#var

# kurtoza okresla intensywnosc wystyepowania wartosci skrajnych (w ogonach)
# skosnosc to miara symetrii/asymetrii | minusowa - lewo | dodatnia - prawo
# odchylenie stadardowe rozproszenie zbioru danych wedlug sredniej


#3

fg <- fitdist(pce_d$Zamkniecie, "norm")#dopasowanie gestesci normalnej
fln <- fitdist(pce_d$Zamkniecie, "lnorm")#dopasowanie gestosci log
fw <- fitdist(pce_d$Zamkniecie, "gamma")#dopasowanie gestosci gamma
par(mfrow = c(1, 1))#rozmiar
plot.legend <- c("gamma", "lognormal", "norm")#nazwy plotow
denscomp(list(fw, fln, fg), legendtext = plot.legend)#tworzy comp na gestosci
qqcomp(list(fw, fln, fg), legendtext = plot.legend)#tworzy comp z kE i kT
cdfcomp(list(fw, fln, fg), legendtext = plot.legend)#tworzy comp na dystrybuancie
#kt - kwantyl teoretyczny | kE - kwantyl empiryczny

#4

# Dopasowanie rozkładu Normalnego
fit_norm <- fitdist(pce_d$Zamkniecie, "norm")
summary(fit_norm)
test_norm <- gofstat(fit_norm, fitnames = "norm")

# Dopasowanie rozkładu Log-Normalnego
fit_lnorm <- fitdist(pce_d$Zamkniecie, "lnorm")
summary(fit_lnorm)
test_lnorm <- gofstat(fit_lnorm, fitnames = "lnorm")

# Dopasowanie rozkładu Gamma
fit_gamma <- fitdist(pce_d$Zamkniecie, "gamma")
summary(fit_gamma)
test_gamma <- gofstat(fit_gamma, fitnames = "gamma")

# Wyświetlenie wyników testów
test_norm
test_lnorm
test_gamma

#                                  norm   |   lnorm    |  gamma
#Kolmogorov-Smirnov statistic   0.1012138 | 0.09105258 | 0.09436892
#Cramer-von Mises statistic     0.3689378 | 0.25513733 | 0.28957664
#Anderson-Darling statistic     2.3711022 | 1.66370275 | 1.87978109
#==
#Akaike's Information Criterion 474.6259  | 465.7726   | 468.4648
#Bayesian Information Criterion 481.6447  | 472.7914   | 475.4836

#co pasuje?
#w naszym przypadku zdecydowanie 5/0 lnorm pasuje najbardziej (wszedzie najmniejsze wartosci)

#opis
#Kolmogorov - Smirnov sprawdza w najdalsza odleglolsc w JEDNYM PUNKCIE 
#______________________
#Cramer - von mises
#oblicza dystrybuante w kazdym punkcie z probki danych pozniej oblicza dystrubuante dla
#podanej funkcji (gamma,lnorm,norm) w naszym przypadku 
# pozniej liczy kwadrat roznicy tych dystrybuant
#_______________________
#Andreson - Darling 
#suma kwadratow roznicy pomiedzy teoretyczna dystrybuanta a empiryczna 

#5
# testowanie rownosci rozkladow
#generuje dane z rozkladow gamma i lnorm i porównujemy jak sie roznia poprzez test KS
# Pobranie parametrów dopasowanych rozkładów

params_lnorm <- fit_lnorm$estimate
hist(params_lnorm,prob=TRUE)
params_lnorm
dn_ln <-  ks.test(pce_d$Zamkniecie,plnorm,params_lnorm[1],params_lnorm[2],exact=TRUE)$statistic

dn_ln
#generujemy probe licznosci n=10 z rozkladu lnorm
#sa to nasze dane z nieznanego rozkladu
N <- 10000
n <- length(pce_d$Zamkniecie); n

Dln <- c()

for (i in 1:N) { 
   
  Yln <- rlnorm(n,params_lnorm[1],params_lnorm[2])
  
  Dln[i] <-  ks.test(Yln,plnorm, params_lnorm[1],params_lnorm[2],exact=TRUE)$statistic
}

dn_ln <-  ks.test(pce_d,plnorm,params_lnorm[1],params_lnorm[2],exact=TRUE)$statistic
dn_ln
#wyniki z punktow 1.2 na histogramie
par(mfrow=c(1,1))
hist(Dln,prob=T)
points(dn_ln,0,pch=19,col=2)


#3. Obliczamy p-value.
p_value_ln <- length(Dln[Dln>dn_ln])/N; p_value_ln
p_value_ln

#4. Przyjmujemy poziom istotnosci alpha=0.05
alpha <- 0.05
p_value_ln <= alpha

# hipoteze odrzucamy bo p_value_ln jest mniejsza od poziomu istotnosci
