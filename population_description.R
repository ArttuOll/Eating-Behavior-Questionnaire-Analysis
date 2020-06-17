source("/home/bsuuv/Opinnot/RUOVA/Tutkimus/tutkimusanalyysi_apufunktioita.R")

#Luetaan data muuttujaan
data <- formatRUOVAData("/home/bsuuv/Opinnot/RUOVA/Tutkimus/tutkimusdata.xlsx")

#Naisten ja miesten lukumäärät
cat("\nLukumäärät sukupuolittain\n")
summary(data$Sukupuoli)
print()

#Iän tunnuslukuja
cat("\nIkä, kaikki\n")
medianMeanSd(data$Ika)

cat("\nIkä, sukupuolittain\n")
sukupuolierot(data$Ika)

#Kylläisyyden tunnuslukuja

cat("\nKylläisyys, kaikki\n")
medianMeanSd(data$Kyllaisyys)

cat("\nKylläisyys, sukupuolittain\n")
sukupuolierot(data$Kyllaisyys)

#Virkeyden tunnuslukuja
cat("\nVirkeys, kaikki\n")
medianMeanSd(data$Virkeys)

cat("\nVirkeys, sukupuolittain\n")
sukupuolierot(data$Virkeys)

#Tunnusluvut unen tarpeelle yleensä
cat("\nUnen tarve yleensä, kaikki\n")
medianMeanSd(data$Uni_yleensa)

#Tunnusluvut viimeyön unen määrän poikkeamalle normaalista
cat("\nViimeyön poikkeama tavallisesta, kaikki\n")
medianMeanSd(data$uniPoikkeama)

sukupuolierot(data$uniPoikkeama)

