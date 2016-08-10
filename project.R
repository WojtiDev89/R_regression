cenyAutIII2012 = read.table("http://tofesi.mimuw.edu.pl/~cogito/smarterpoland/cenyAutIII2012/cenyAutIII2012.csv
", sep=";",dec=",", header=T, na.strings = "", stringsAsFactors=FALSE)

auta.df <- data.frame(cenyAutIII2012)
#auta.df <- subset(auta.df, Marka == "Acura" & Brutto.netto == "brutto" & is.na(Pojazd.uszkodzony) == TRUE & Waluta == "PLN" & Kraj.aktualnej.rejestracji == "Polska")
auta.df <- subset(auta.df, Marka == "Volkswagen" & Model == "Passat" & Wersja == "B6" & is.na(Pojazd.uszkodzony) == TRUE & Brutto.netto == "brutto" & Waluta == "PLN" & 
 (Nadwozie == "Kombi" | Nadwozie == "Sedan / Limuzyna") & Liczba.drzwi == "4/5" & is.na(Wyposazenie.dodatkowe) == FALSE & is.na(Informacje.dodatkowe) == FALSE & Skrzynia.biegow != "polautomatyczna / sekwencyjna" & Kraj.aktualnej.rejestracji == "Polska")
auta<-auta.df
wiersze <- dim(auta)[1]
summary(auta)

#rm(list = ls())

uzupelniaj = FALSE
uzupelniaj.wyp = TRUE
uzupelniaj.inf.dod = TRUE
normalizuj = TRUE

library(stringr)
library(MASS)


##############################################     Przygotowanie danych      ###################################


############################## Adres

Adres<-auta[,"Adres"]
Adres<-as.character(Adres)
Adres<-str_sub(Adres,1,1)

okregi <- c("okreg.warszawski","okreg.olsztyñski","okreg.lubelski","okreg.krakowski","okreg.katowicki",
"okreg.wroclawski","okreg.poznanski","okreg.szczecinski","okreg.gdanski","okreg.lodzki")

indeks <- 0;

najwiecej <- names(sort(-table(Adres)))[1]
najwiecej <- as.numeric(najwiecej)

for (i in 1:wiersze)
{

	
	if(is.na(Adres[i]) == FALSE)
		{
			indeks <- as.numeric(Adres[i])
			Adres[i] <- okregi[indeks+1]
		}

	if(is.na(Adres[i]) == TRUE & uzupelniaj == TRUE)
		{
			 Adres[i] <- okregi[najwiecej+1] 
		}

}

####################### Wyposazenie dodatkowe

wyp.dod = c("ABS"," ASR"," EDS"," ESP"," alufelgi"," autoalarm"," bagaznik na dach"," blokada dyferencjalu"," blokada skrzyni biegow"," centralny zamek",
 " czujnik deszczu", " czujnik parkowania"," el. lusterka"," el. szyby"," hak",
 " immobiliser"," instalacja gazowa"," kierownica wielofunkcyjna"," klatka"," klimatyzacja"," komputer",
 " ksenony", " niezalezne ogrzewanie"," pod. przednia szyba"," podgrzewane fotele",
 " poduszka powietrzna",  " przyciemniane szyby" , " radio / CD" , " reg. wysokosc podwozia" ," skorzana tapicerka",
 " system nawigacji", " szyberdach", " lwiatla przeciwmglowe",  " tempomat" , " welurowa tapicerka",
 " wspomaganie kierownicy"," 4x4")


wyposazenie <- auta[,"Wyposazenie.dodatkowe"]
Wyposazenie.dodatkowe <- matrix(ncol=37,nrow=wiersze)
colnames(Wyposazenie.dodatkowe) = wyp.dod

if(uzupelniaj.wyp == TRUE) {Wyposazenie.dodatkowe[] <- FALSE}

a<-list();

for (i in 1:wiersze) 
{
	a<-strsplit(as.character(wyposazenie[i]),",")[[1]]
	if(length(a) > 1) 
	{
		for(k in 1:length(wyp.dod))
		{
	
			for (j in 1:length(a))
			{

			if(a[j]==wyp.dod[k]) {Wyposazenie.dodatkowe[i,k]=TRUE}

			}
	
		}
	}
}

nazwy = c("ABS","ASR","EDS","ESP","alufelgi","autoalarm","bagaznik.na.dach","blokada.dyferencjalu","blokada.skrzyni.biegow","centralny.zamek",
 "czujnik.deszczu", "czujnik.parkowania","el.lusterka","el.szyby","hak",
 "immobiliser","instalacja.gazowa","kierownica.wielofunkcyjna","klatka","klimatyzacja","komputer",
 "ksenony", "niezalezne.ogrzewanie","pod.przednia.szyba","podgrzewane fotele",
 "poduszka.powietrzna",  "przyciemniane.szyby" , "radio.CD" , "reg.wysokosc.podwozia" ,"skorzana.tapicerka",
 "system.nawigacji", "szyberdach", "swiatla.przeciwmglowe",  "tempomat" , "welurowa.tapicerka",
 "wspomaganie.kierownicy","4x4")

colnames(Wyposazenie.dodatkowe) = nazwy


##################### informacje dodatkowe


inf.dod = c("bezwypadkowy","dla niepelnosprawnych","garazowany","homologacja na ciezarowe","pierwszy wlasciciel","serwisowany w ASO","tuning","zabytkowy")

wiersze <- dim(auta)[1]

informacje <- auta[,"Informacje.dodatkowe"]
Informacje.dodatkowe <- matrix(ncol=8,nrow=wiersze)
colnames(Informacje.dodatkowe) = inf.dod
if(uzupelniaj.inf.dod == TRUE) {Informacje.dodatkowe[] <- FALSE}


b<-list();


for (i in 1:wiersze)  
{
	b<-strsplit(as.character(informacje[i]),", ")[[1]]
	if(length(b) > 1) 
	{
		for(k in 1:length(inf.dod))
		{
	
			for (j in 1:length(b))
			{

			if(b[j]==inf.dod[k]) {Informacje.dodatkowe[i,k]=TRUE}

			}
	
		}
	}

}


######################### Kraj.pochodzenia

Kraj.pochodzenia<-auta[,"Kraj.pochodzenia"]

if(uzupelniaj == TRUE) {

najwiecej <- names(sort(-table(Kraj.pochodzenia)))[1]

Kraj.pochodzenia[is.na(Kraj.pochodzenia)==TRUE] <- najwiecej

				}

Kraj.pochodzenia <- as.factor(Kraj.pochodzenia)



######################## KM

KM <-as.numeric(auta[,"KM"])

if(uzupelniaj == TRUE) {

srednia <- mean(KM[is.na(KM)==FALSE])
KM[is.na(KM)==TRUE] <- floor(srednia)

				}

if(normalizuj == TRUE) {


}
######################## Pojemnosc.skokowa

Pojemnosc.skokowa <-as.numeric(auta[,"Pojemnosc.skokowa"])
srednia <- mean(Pojemnosc.skokowa[is.na(Pojemnosc.skokowa)==FALSE])

if(uzupelniaj == TRUE) {
Pojemnosc.skokowa[is.na(Pojemnosc.skokowa)==TRUE] <- floor(srednia)
				}

Pojemnosc.skokowa <- Pojemnosc.skokowa[] /100
#Pojemnosc.skokowa <- Pojemnosc.skokowa[is.na(Pojemnosc.skokowa)==FALSE] /100
Pojemnosc.skokowa <- round(Pojemnosc.skokowa[]) *100 
#Pojemnosc.skokowa <- round(Pojemnosc.skokowa[is.na(Pojemnosc.skokowa)==FALSE]) *100 

#Pojemnosc.skokowa <- as.factor(Pojemnosc.skokowa)


######################## Nadwozie

Nadwozie<-auta[,"Nadwozie"]

if(uzupelniaj == TRUE) {

indeks <- 0;

najwiecej <- names(sort(-table(Nadwozie)))[1]

Nadwozie[is.na(Nadwozie)==TRUE] <- najwiecej
				}

Nadwozie <- as.factor(Nadwozie)


######################## Liczba.drzwi

Liczba.drzwi<-auta[,"Liczba.drzwi"]

if(uzupelniaj == TRUE) {

indeks <- 0;

najwiecej <- names(sort(-table(Liczba.drzwi)))[1]

Liczba.drzwi[is.na(Liczba.drzwi)==TRUE] <- najwiecej
				}

Liczba.drzwi <- as.factor(Liczba.drzwi)


####################### Cena.w.PLN

Cena.w.PLN <- auta[,"Cena.w.PLN"]
Cena.w.PLN <- as.numeric(Cena.w.PLN)


######################## Przebieg.w.km

Przebieg.w.km <-as.numeric(auta[,"Przebieg.w.km"])

if(uzupelniaj == TRUE) {

srednia <- mean(Przebieg.w.km[is.na(Przebieg.w.km)==FALSE])
Przebieg.w.km[is.na(Przebieg.w.km)==TRUE] <- floor(srednia)
				}

####################### Rodzaj.paliwa

Rodzaj.paliwa <- as.factor(auta[,"Rodzaj.paliwa"])

if(uzupelniaj == TRUE) {
indeks <- 0;

najwiecej <- names(sort(-table(Rodzaj.paliwa)))[1]

Rodzaj.paliwa[is.na(Rodzaj.paliwa)==TRUE] <- najwiecej
				}

####################### Rok.produkcji

Rok.produkcji <- as.numeric(auta[,"Rok.produkcji"])

if(uzupelniaj == TRUE) {
indeks <- 0;

najwiecej <- names(sort(-table(Rok.produkcji)))[1]

Rok.produkcji[is.na(Rok.produkcji)==TRUE] <- najwiecej
				}

######################## Kolor

Kolor<-auta[,"Kolor"]

if(uzupelniaj == TRUE) {

najwiecej <- names(sort(-table(Kolor)))[1]

Kolor[is.na(Kolor)==TRUE] <- najwiecej

				}

Kolor <- as.factor(Kolor)


######################## Skrzynia.biegow

Skrzynia.biegow<-auta[,"Skrzynia.biegow"]

if(uzupelniaj == TRUE) {

najwiecej <- names(sort(-table(Skrzynia.biegow)))[1]

Skrzynia.biegow[is.na(Skrzynia.biegow)==TRUE] <- najwiecej

				}

Skrzynia.biegow <- as.factor(Skrzynia.biegow)

######################## Marka

Marka<-auta[,"Marka"]

######################## Model

Model<-auta[,"Model"]

######################## Wersja

Wersja<-auta[,"Wersja"]









auta.dane <- data.frame(Cena.w.PLN,KM,Marka,Model,Wersja,Nadwozie,Pojemnosc.skokowa,Przebieg.w.km,
Rodzaj.paliwa,Rok.produkcji,Kolor,Kraj.pochodzenia,Skrzynia.biegow,Wyposazenie.dodatkowe,
Informacje.dodatkowe,Adres)

summary(auta.dane)
str(auta.dane)

#Usuniêcie niepe³nych przyk³adów
auta.dane <- auta.dane[complete.cases(auta.dane), ]

#Usuniêcie zbednych atrybutów
auta.dane <- subset(auta.dane, select = -c(zabytkowy,tuning,dla.niepelnosprawnych, reg.wysokosc.podwozia,
poduszka.powietrzna,klimatyzacja,klatka,instalacja.gazowa,el.szyby,el.lusterka,centralny.zamek,
blokada.dyferencjalu,ABS,wspomaganie.kierownicy))

layout(matrix(1:4,2,2))
#plot(auta.dane[,"Cena.w.PLN"])
hist(auta.dane[,"Cena.w.PLN"],breaks=seq(0,140000,by=1400))
hist(auta.dane[,"Pojemnosc.skokowa"],breaks=seq(0,4000,by=40))
hist(auta.dane[,"KM"],breaks=seq(0,300,by=3))
hist(auta.dane[,"Przebieg.w.km"],breaks=seq(0,300000,by=3000))

auta.dane <- subset(auta.dane, Cena.w.PLN < 100000 & Przebieg.w.km > 5000 & (Rodzaj.paliwa == "benzyna" | Rodzaj.paliwa =="olej napedowy (diesel)" ) )
auta.dane[,"Rodzaj.paliwa"] <- as.factor(as.character(auta.dane[,"Rodzaj.paliwa"]))
summary(auta.dane)
str(auta.dane)

splitdf <- function(dataframe, seed=NULL) {
	if (!is.null(seed)) set.seed(seed)
	index <- 1:nrow(dataframe)
	trainindex <- sample(index, trunc(length(index)/2))
	trainset <- dataframe[trainindex, ]
	testset <- dataframe[-trainindex, ]
	list(trainset=trainset,testset=testset)
}




##########################################          Regresja          ###########################################


fit<-lm(Cena.w.PLN~KM+Nadwozie+Pojemnosc.skokowa+Przebieg.w.km+
Rodzaj.paliwa+Rok.produkcji+Kolor+Kraj.pochodzenia+Skrzynia.biegow+ASR+EDS+ESP+
alufelgi+autoalarm+bagaznik.na.dach+blokada.skrzyni.biegow+
czujnik.deszczu+czujnik.parkowania+hak+
immobiliser+kierownica.wielofunkcyjna+komputer+
ksenony+niezalezne.ogrzewanie+pod.przednia.szyba+podgrzewane.fotele+przyciemniane.szyby+radio.CD+skorzana.tapicerka+
system.nawigacji+szyberdach+swiatla.przeciwmglowe+tempomat+welurowa.tapicerka+X4x4+bezwypadkowy+garazowany+
homologacja.na.ciezarowe+pierwszy.wlasciciel+serwisowany.w.ASO+Adres,data=auta.dane)

summary(fit)
layout(matrix(1:4,2,2))
plot(fit)

cat ("Press [enter] to continue")
line <- readline()

auta.dane2 <- subset(auta.dane, Cena.w.PLN < 50000)

fit2<-lm(Cena.w.PLN~KM+Nadwozie+Pojemnosc.skokowa+Przebieg.w.km+
Rodzaj.paliwa+Rok.produkcji+Kolor+Kraj.pochodzenia+Skrzynia.biegow+ASR+EDS+ESP+
alufelgi+autoalarm+bagaznik.na.dach+blokada.skrzyni.biegow+
czujnik.deszczu+czujnik.parkowania+hak+
immobiliser+kierownica.wielofunkcyjna+komputer+
ksenony+niezalezne.ogrzewanie+pod.przednia.szyba+podgrzewane.fotele+przyciemniane.szyby+radio.CD+skorzana.tapicerka+
system.nawigacji+szyberdach+swiatla.przeciwmglowe+tempomat+welurowa.tapicerka+X4x4+bezwypadkowy+garazowany+
homologacja.na.ciezarowe+pierwszy.wlasciciel+serwisowany.w.ASO+Adres,data=auta.dane2)

summary(fit2)
layout(matrix(1:4,2,2))
plot(fit2)


# selekcja atrybutow

step <- stepAIC(fit, direction="both")
step$anova # display results

cat ("Press [enter] to continue")
line <- readline()

step2 <- stepAIC(fit2, direction="both")
step2$anova # display results


fit.atryb<-lm(Cena.w.PLN ~ Nadwozie + Przebieg.w.km + Rodzaj.paliwa + Rok.produkcji + 
    Skrzynia.biegow + EDS + alufelgi + kierownica.wielofunkcyjna + 
    ksenony + skorzana.tapicerka + system.nawigacji + bezwypadkowy + 
    pierwszy.wlasciciel, data=auta.dane)


fit.atryb2<-lm(Cena.w.PLN ~ Nadwozie + Przebieg.w.km + Rodzaj.paliwa + Rok.produkcji + 
    Kolor + Kraj.pochodzenia + Skrzynia.biegow + alufelgi + autoalarm + 
    czujnik.deszczu + niezalezne.ogrzewanie + radio.CD + system.nawigacji + 
    szyberdach + tempomat + X4x4 + bezwypadkowy + garazowany,data=auta.dane2)


summary(fit.atryb)
layout(matrix(1:4,2,2))
plot(fit.atryb)

cat ("Press [enter] to continue")
line <- readline()

summary(fit.atryb2)
layout(matrix(1:4,2,2))
plot(fit.atryb2)


###### Podzia³ danych 80/20 trenuj¹cy/testuj¹cy

indexes = sample(1:nrow(auta.dane), size=0.2*nrow(auta.dane))
testing = auta.dane[indexes,]
training = auta.dane[-indexes,]


fit.atryb<-lm(Cena.w.PLN ~ Nadwozie + Przebieg.w.km + Rodzaj.paliwa + Rok.produkcji + 
    Skrzynia.biegow + EDS + alufelgi + kierownica.wielofunkcyjna + 
    ksenony + skorzana.tapicerka + system.nawigacji + bezwypadkowy + 
    pierwszy.wlasciciel, data=training)


fit.atryb2<-lm(Cena.w.PLN ~ Nadwozie + Przebieg.w.km + Rodzaj.paliwa + Rok.produkcji + 
    Kolor + Kraj.pochodzenia + Skrzynia.biegow + alufelgi + autoalarm + 
    czujnik.deszczu + niezalezne.ogrzewanie + radio.CD + system.nawigacji + 
    szyberdach + tempomat + X4x4 + bezwypadkowy + garazowany,data=training)

cat ("Press [enter] to continue")
line <- readline()

summary(fit.atryb)
summary(fit.atryb2)

pred<-predict(fit.atryb,testing)
pred2<-predict(fit.atryb2,testing)
#summary(pred)
#str(pred)

blad<-0
blad=testing[,"Cena.w.PLN"]-pred[]
mse=sqrt(mean((testing[,"Cena.w.PLN"]-pred[])^2))
sqrt(var(blad))
mean(blad)
layout(matrix(1:2,1,2))
plot(testing[,"Cena.w.PLN"],blad,main="B³¹d w zale¿noœci od wartoœci")

summary(blad)
hist(blad,breaks=seq(-20000,25000,by=1000))

cat ("Press [enter] to continue")
line <- readline()

blad2<-0
blad2=testing[,"Cena.w.PLN"]-pred2[]
mse2=sqrt(mean((testing[,"Cena.w.PLN"]-pred2[])^2))
sqrt(var(blad2))
mean(blad2)
layout(matrix(1:2,1,2))
plot(testing[,"Cena.w.PLN"],blad2,main="B³¹d w zale¿noœci od wartoœci")
summary(blad2)
hist(blad2,breaks=seq(-20000,25000,by=1000))











