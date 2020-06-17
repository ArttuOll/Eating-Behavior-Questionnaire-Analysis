source("/home/bsuuv/Opinnot/RUOVA/Tutkimus/tutkimusanalyysi_apufunktioita.R")

data <- formatRUOVAData("/home/bsuuv/Opinnot/RUOVA/Tutkimus/tutkimusdata.xlsx", luvut = TRUE)

data <- yhdistaKuvat(data)

#Kategorisoidaan vastemuuttuja logistista regressiota varten
data$Terveellinen[data$Terveellinen >= 0.66] <- 1
data$Terveellinen[data$Terveellinen < 0.66] <- 0
data$Makea[data$Makea >= 0.5] <- 1
data$Makea[data$Makea < 0.5] <- 0
data$Tayttava[data$Tayttava >= 0.5] <- 1
data$Tayttava[data$Tayttava < 0.5] <- 0

leveneTest(data$uniPoikkeama, group = data$Terveellinen)
leveneTest(data$Ika, group = data$Terveellinen)
leveneTest(data$Kyllaisyys, group = data$Terveellinen)

sink("/home/bsuuv/Opinnot/RUOVA/Tutkimus/paa_analyysi.txt")

#Terveellisyys
cat("\nTerveellisyys\n")
malli1Terv <- glm(data$Terveellinen ~ data$uniPoikkeama, family = "binomial", data = data)
summary(malli1Terv)

cat("\ne potenssiin estimaatti on:\n")
exp(malli1Terv$coefficients)

malli2Terv <- glm(data$Terveellinen ~ data$uniPoikkeama + data$Ika + data$Kyllaisyys, family = "binomial", data = data)
summary(malli2Terv)

cat("\ne potenssiin estimaatti on:\n")
exp(malli2Terv$coefficients)

#Kuvaa mallin ennustamien jäännöksien eroa havaituista arvoista
plot(predict(malli1Terv), rstandard(malli1Terv))
abline(h = 0)
plot(predict(malli2Terv), rstandard(malli2Terv))
abline(h = 0)

#Kuvaa selittävän ja vastemuuttujan suhdetta
plot(data$Terveellinen ~ data$uniPoikkeama)
abline(v = c(0, 5, -5))

#Makeus
cat("\nMakeus\n")
malli1Mak <- glm(data$Makea ~ data$uniPoikkeama, family = "binomial", data = data)
summary(malli1Mak)

cat("\ne potenssiin estimaatti on:\n")
exp(malli1Mak$coefficients)

malli2Mak <- glm(data$Makea ~ data$uniPoikkeama + data$Ika + data$Kyllaisyys, family = "binomial", data = data)
summary(malli2Mak)

cat("\ne potenssiin estimaatti on:\n")
exp(malli2Mak$coefficients)

#Kuvaa mallin ennustamien jäännöksien eroa havaituista arvoista
plot(predict(malli1Mak), rstandard(malli1Mak))
abline(h = 0)
plot(predict(malli2Mak), rstandard(malli2Mak))
abline(h = 0)

#Kuvaa selittävän ja vastemuuttujan suhdetta
plot(data$Makea ~ data$uniPoikkeama)
abline(v = c(0, 5, -5))

#Täyttävyys
cat("\nTäyttävyys\n")
malli1Tay <- glm(data$Tayttava ~ data$uniPoikkeama, family = "binomial", data = data)
summary(malli1Tay)

cat("\ne potenssiin estimaatti on:\n")
exp(malli1Tay$coefficients)

malli2Tay <- glm(data$Tayttava ~ data$uniPoikkeama + data$Ika + data$Kyllaisyys, family = "binomial", data = data)
summary(malli2Tay)

cat("\ne potenssiin estimaatti on:\n")
exp(malli2Tay$coefficients)

#Kuvaa mallin ennustamien jäännöksien eroa havaituista arvoista
plot(predict(malli1Tay), rstandard(malli1Tay))
abline(h = 0)
plot(predict(malli2Tay), rstandard(malli2Tay))
abline(h = 0)

#Kuvaa selittävän ja vastemuuttujan suhdetta
plot(data$Tayttava ~ data$uniPoikkeama)
abline(v = c(0, 5, -5))

sink()
