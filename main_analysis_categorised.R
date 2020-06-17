source("/home/bsuuv/Opinnot/RUOVA/Tutkimus/tutkimusanalyysi_apufunktioita.R")

#Luetaan data muuttujaan
data <- formatRUOVAData("/home/bsuuv/Opinnot/RUOVA/Tutkimus/tutkimusdata.xlsx", luvut = TRUE)

data <- yhdistaKuvat(data)

#Kategorisoidaan terveellisyys
data$Terveellinen[data$Terveellinen >= 0.67] <- "Terveellinen"
data$Terveellinen[data$Terveellinen < 0.67] <- "Epäterveellinen"
data$Terveellinen <- factor(data$Terveellinen, levels = c("Terveellinen", "Epäterveellinen"))

#Kategorisoidaan makeus
data$Makea[data$Makea >= 0.5] <- "Makea"
data$Makea[data$Makea < 0.5] <- "Suolainen"
data$Makea <- factor(data$Makea, levels = c("Makea", "Suolainen"))

#Kategorisoidaan täyttävyys
data$Tayttava[data$Tayttava >= 0.5] <- "Täyttävä"
data$Tayttava[data$Tayttava < 0.5] <- "Ei-täyttävä"
data$Tayttava <- factor(data$Tayttava, levels = c("Täyttävä", "Ei-täyttävä"))

#Kategorisoidaan ikä
data$Ika[data$Ika < 30 & data$Ika >= 24] <- "24-29"
data$Ika[data$Ika < 24 & data$Ika >= 21] <- "21-23"
data$Ika[data$Ika < 21 & data$Ika >= 18] <- "18-20"
data$Ika <- factor(data$Ika, levels = c("30+", "24-29", "21-23", "18-20"))

#Kategorisoidaan kylläisyys
data$Kyllaisyys[data$Kyllaisyys > 5] <- "Kylläinen"
data$Kyllaisyys[data$Kyllaisyys <= 5] <- "Ei-kylläinen"
data$Kyllaisyys <- factor(data$Kyllaisyys, levels = c("Kylläinen", "Ei-kylläinen"))

#Kategorisoidaan univaje
data$uniPoikkeama[data$uniPoikkeama >= 0] <- "Hyvin nukkunut"
data$uniPoikkeama[data$uniPoikkeama < 0] <- "Huonosti nukkunut"
data$uniPoikkeama <- factor(data$uniPoikkeama, levels = c("Hyvin nukkunut", "Huonosti nukkunut"))

#Tunnesyöminen
data$tunnePisteet[data$tunnePisteet > 50] <- "Tunnesyöjä"
data$tunnePisteet[data$tunnePisteet < 50] <- "Ei-tunnesyöjä"

sink("/home/bsuuv/Opinnot/RUOVA/Tutkimus/paa_analyysi_kategorisoitu.txt")

#Naiset
cat("\nTerveellisyys\n")
khiiKuvat(data$Terveellinen)

cat("\nMakeus\n")
khiiKuvat(data$Makea)

cat("\nTäyttävyys\n")
khiiKuvat(data$Tayttava)


#Miehet
cat("\nTerveellisyys\n")
khiiKuvat(data$Terveellinen, sukup = "M")

cat("\nMakeus\n")
khiiKuvat(data$Makea, sukup = "M")

cat("\nTäyttävyys\n")
khiiKuvat(data$Tayttava, sukup = "M")

#Tunnesyöminen
cat("\nTunnesyöminen ja terveellisyys\n")
CrossTable(data$tunnePisteet, data$Terveellinen, chisq = T)
cat("\nTunnesyöminen ja makeus\n")
CrossTable(data$tunnePisteet, data$Makea, chisq = T)
cat("\nTunnesyöminen ja täyttävyys\n")
CrossTable(data$tunnePisteet, data$Tayttava, chisq = T)
sink()
