#Funktio datan muokkaamiselle analysoitavaan muotoon

formatRUOVAData <- function(path, luvut = FALSE, rajaa = TRUE) {
  #Luetaan data .xlsx-tiedostosta
  data <- read_xlsx(path = path)
  
  tunnedata <- read_excel("/home/bsuuv/Opinnot/RUOVA/Tutkimus/tutkimusdata_tunnesyominen.xlsx")
  
  data <- cbind(data, tunnePisteet = tunnedata$`Tunnesyömisen pisteet`)
  
  #Poistetaan epaolennaiset sarakkeet
  data <- data[,c(-24, -1, -2, -3, -4, -5)]
  
  #Muutetaan sarakkeiden nimet järkevimmiksi
  colnames(data)[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)] <- c("Sukupuoli", "Ika", "Kyllaisyys", "Kuvat1", "Kuvat2", "Kuvat3", "Kuvat4", "Kuvat5", "Kuvat6", "Kuvat7", "Kuvat8", "Kuvat9",
                                                                                        "Kun_huolissani", "Kun_alakuloinen", "Kun_yksinainen", "Virkeys", "Uni_viime", "Uni_yleensa")
  
  #Tutkimuspopulaation rajaus
  if (rajaa) {
    data <- data[data$Ika < 30 & data$Sukupuoli == "Nainen", ]
  }
  
  #Luokitellaan kuvat kategorioiksi, jos luvut = F, numeroiksi, jos luvut = T
  #Terveellinen = 1, Makea = 1, Täyttävä = 1
  if (luvut == F) {
    data$Kuvat1[data$Kuvat1 == "Oikea"] <- "Terveellinen"
    data$Kuvat1[data$Kuvat1 == "Vasen"] <- "Epäterveellinen"
    data$Kuvat2[data$Kuvat2 == "Oikea"] <- "Makea"
    data$Kuvat2[data$Kuvat2 == "Vasen"] <- "Suolainen"
    data$Kuvat3[data$Kuvat3 == "Oikea"] <- "Epäterveellinen"
    data$Kuvat3[data$Kuvat3 == "Vasen"] <- "Terveellinen"
    data$Kuvat4[data$Kuvat4 == "Oikea"] <- "Ei-täyttävä"
    data$Kuvat4[data$Kuvat4 == "Vasen"] <- "Täyttävä"
    data$Kuvat5[data$Kuvat5 == "Oikea"] <- "Suolainen"
    data$Kuvat5[data$Kuvat5 == "Vasen"] <- "Makea"
    data$Kuvat6[data$Kuvat6 == "Oikea"] <- "Suolainen"
    data$Kuvat6[data$Kuvat6 == "Vasen"] <- "Makea"
    data$Kuvat7[data$Kuvat7 == "Oikea"] <- "Ei-täyttävä"
    data$Kuvat7[data$Kuvat7 == "Vasen"] <- "Täyttävä"
    data$Kuvat8[data$Kuvat8 == "Oikea"] <- "Epäterveellinen"
    data$Kuvat8[data$Kuvat8 == "Vasen"] <- "Terveellinen"
    data$Kuvat9[data$Kuvat9 == "Oikea"] <- "Suolainen"
    data$Kuvat9[data$Kuvat9 == "Vasen"] <- "Makea"
  }
  else {
    data$Kuvat1[data$Kuvat1 == "Oikea"] <- 1
    data$Kuvat1[data$Kuvat1 == "Vasen"] <- 0
    data$Kuvat2[data$Kuvat2 == "Oikea"] <- 1
    data$Kuvat2[data$Kuvat2 == "Vasen"] <- 0
    data$Kuvat3[data$Kuvat3 == "Oikea"] <- 0
    data$Kuvat3[data$Kuvat3 == "Vasen"] <- 1
    data$Kuvat4[data$Kuvat4 == "Oikea"] <- 0
    data$Kuvat4[data$Kuvat4 == "Vasen"] <- 1
    data$Kuvat5[data$Kuvat5 == "Oikea"] <- 0
    data$Kuvat5[data$Kuvat5 == "Vasen"] <- 1
    data$Kuvat6[data$Kuvat6 == "Oikea"] <- 0
    data$Kuvat6[data$Kuvat6 == "Vasen"] <- 1
    data$Kuvat7[data$Kuvat7 == "Oikea"] <- 0
    data$Kuvat7[data$Kuvat7 == "Vasen"] <- 1
    data$Kuvat8[data$Kuvat8 == "Oikea"] <- 0
    data$Kuvat8[data$Kuvat8 == "Vasen"] <- 1
    data$Kuvat9[data$Kuvat9 == "Oikea"] <- 0
    data$Kuvat9[data$Kuvat9 == "Vasen"] <- 1
  }
  
  #Muutetaan numeeriset muuttujat R:n silmissä numeerisiksi 
  data$Uni_viime <- as.numeric(data$Uni_viime)
  data$Uni_yleensa <- as.numeric(data$Uni_yleensa)
  data$Ika <- as.numeric(data$Ika)
  
  #Luodaan uusi sarake viime yön unen määrän poikkeamalle yleisestä
  data$uniPoikkeama = data$Uni_viime - data$Uni_yleensa
  
  #Muutetaan kategoriset muuttujat R:n silmissä kategorisiksi
  data$Sukupuoli = factor(data$Sukupuoli, levels = c("Nainen", "Mies"), ordered = TRUE)
  data$Kun_huolissani = factor(data$Kun_huolissani, levels = c("Ei ollenkaan totta", "Enimmäkseen ei totta", "Enimmäkseen totta", "Täysin totta"))
  data$Kun_alakuloinen = factor(data$Kun_alakuloinen, levels = c("Ei ollenkaan totta", "Enimmäkseen ei totta", "Enimmäkseen totta", "Täysin totta"))
  data$Kun_yksinainen = factor(data$Kun_yksinainen, levels = c("Ei ollenkaan totta", "Enimmäkseen ei totta", "Enimmäkseen totta", "Täysin totta"))
  
  if (luvut == F) {
    data$Kuvat1 = factor(data$Kuvat1, levels = c("Terveellinen", "Epäterveellinen"))
    data$Kuvat2 = factor(data$Kuvat2, levels = c("Makea", "Suolainen"))
    data$Kuvat3 = factor(data$Kuvat3, levels = c("Terveellinen", "Epäterveellinen"))
    data$Kuvat4 = factor(data$Kuvat4, levels = c("Ei-täyttävä", "Täyttävä"))
    data$Kuvat5 = factor(data$Kuvat5, levels = c("Suolainen", "Makea"))
    data$Kuvat6 = factor(data$Kuvat6, levels = c("Suolainen", "Makea"))
    data$Kuvat7 = factor(data$Kuvat7, levels = c("Ei-täyttävä", "Täyttävä"))
    data$Kuvat8 = factor(data$Kuvat8, levels = c("Terveellinen", "Epäterveellinen"))
    data$Kuvat9 = factor(data$Kuvat9, levels = c("Suolainen", "Makea"))
  } else {
    data$Kuvat1 <- as.numeric(data$Kuvat1)
    data$Kuvat2 <- as.numeric(data$Kuvat2)
    data$Kuvat3 <- as.numeric(data$Kuvat3)
    data$Kuvat4 <- as.numeric(data$Kuvat4)
    data$Kuvat5 <- as.numeric(data$Kuvat5)
    data$Kuvat6 <- as.numeric(data$Kuvat6)
    data$Kuvat7 <- as.numeric(data$Kuvat7)
    data$Kuvat8 <- as.numeric(data$Kuvat8)
    data$Kuvat9 <- as.numeric(data$Kuvat9)
  }
  
  return(data)
}

#Funktio perus tunnuslukujen laskemiselle
medianMeanSd <- function(data) {
  cat("\nMedian: ")
  cat(round(median(data), 2))
  
  cat("\nMean: ")
  cat(round(mean(data), 2))
  
  cat("\nStandard deviation: ")
  cat(round(sd(data), 2))
}

#Funktio, joka tutkii sukupuolieroja annetun muuttujan suhteen
sukupuolierot <- function(muuttuja) {
  cat("Ovatko varianssit yhtäsuuria?\n")
  levene <- leveneTest(muuttuja, group = data$Sukupuoli)
  
  if(levene$`Pr(>F)`[1] < 0.05) {
    cat("Varianssit eivät ole yhtäsuuria ryhmien välillä, käytetään Welchin t-testiä\n")
    welch <- t.test(muuttuja[data$Sukupuoli == "Mies"], muuttuja[data$Sukupuoli == "Nainen"], var.equal = FALSE)
    
    if(welch$p.value < 0.05) {
      cat("\nMuuttujan arvo oli merkitsevästi eroava sukupuolten välillä.\n", "P = ", welch$p.value)
      cat("\nMiehet: ", round(welch$estimate[1], 2), "Naiset: ", round(welch$estimate[2], 2))
    } else {
      cat("\nMuuttujan arvot eivät eronneet merkitsevästi sukupuolien välillä")
      cat("\nMiehet: ", round(welch$estimate[1], 2), "Naiset: ", round(welch$estimate[2], 2))
    }
    
  } else {
    cat("Varianssit ovat yhtäsuuria, käytetään Studentin t-testiä\n")
    student <- t.test(data$Kyllaisyys[data$Sukupuoli == "Mies"], data$Kyllaisyys[data$Sukupuoli == "Nainen"], var.equal = TRUE)
    
    if(student$p.value < 0.05) {
      cat("\nMuuttujan ", muuttuja, "arvo oli merkitsevästi eroava sukupuolten välillä.\n", "P = ", student$p.value)
      cat("\nMiesten keski-kylläisyys: ", round(student$estimate[1], 2), "Naisten keski-kylläisyys: ", round(student$estimate[2], 2))
    } else {
      cat("\nMuuttujan arvot eivät eronneet merkitsevästi sukupuolien välillä")
      cat("\nMiesten keski-kylläisyys: ", round(student$estimate[1], 2), "Naisten keski-kylläisyys: ", round(student$estimate[2], 2))
    }
  }
}

yhdistaKuvat <- function(data) {
  #Yhdistetään kuvien vastaukset kategorioiden perusteella
  data$Terveellinen <- apply(data.frame(data$Kuvat1, data$Kuvat3, data$Kuvat8), 1, mean)
  data$Terveellinen <- round(data$Terveellinen, 2)
  data$Makea <- apply(data.frame(data$Kuvat2, data$Kuvat5, data$Kuvat6, data$Kuvat9), 1, mean)
  data$Makea <- round(data$Makea, 2)
  data$Tayttava <- apply(data.frame(data$Kuvat4, data$Kuvat7), 1, mean)
  data$Tayttava <- round(data$Tayttava, 2)
  
  #Poistetaan kuvien sarakkeet tarpeettomina
  data <- data[,c(-4, -5, -6, -7, -8, -9, -10, -11, -12)]
  
  return(data)
} 

khiiKuvat <- function(muuttuja, sukup = "N") {
  
  if (sukup == "N") {
    cat("\nSeuraavat tulokset koskevat vain alle 30 vuotiaita naisia!\n")
    
    #Khii-neliötesti ikä
    cat("\nIkä\n")
    print(CrossTable(data$Ika[data$Sukupuoli == "Nainen" & data$Ika != "30+"], muuttuja[data$Sukupuoli == "Nainen" & data$Ika != "30+"],
                     prop.chisq = FALSE, chisq = TRUE, prop.t = FALSE))
    
    #Khii-neliötesti kyllaisyys
    cat("\nKylläisyys\n")
    print(CrossTable(data$Kyllaisyys[data$Sukupuoli == "Nainen" & data$Ika != "30+"], muuttuja[data$Sukupuoli == "Nainen" & data$Ika != "30+"],
                     prop.chisq = FALSE, chisq = TRUE, prop.t = FALSE))
    
    #Khii-neliötesti univaje
    cat("\nUnen poikkeama\n")
    print(CrossTable(data$uniPoikkeama[data$Sukupuoli == "Nainen" & data$Ika != "30+"], muuttuja[data$Sukupuoli == "Nainen" & data$Ika != "30+"],
                     prop.chisq = FALSE, chisq = TRUE, prop.t = FALSE))  
  } else if (sukup == "M") {
    cat("\nSeuraavat tulokset koskevat vain alle 30 vuotiaita miehiä!\n")
    
    #Khii-neliötesti ikä
    cat("\nIkä\n")
    print(CrossTable(data$Ika[data$Sukupuoli == "Mies"], muuttuja[data$Sukupuoli == "Mies"],
                     prop.chisq = FALSE, chisq = TRUE, prop.t = FALSE))
    
    #Khii-neliötesti kyllaisyys
    cat("\nKylläisyys\n")
    print(CrossTable(data$Kyllaisyys[data$Sukupuoli == "Mies"], muuttuja[data$Sukupuoli == "Mies"],
                     prop.chisq = FALSE, chisq = TRUE, prop.t = FALSE))
    
    #Khii-neliötesti univaje
    cat("\nUnen poikkeama\n")
    print(CrossTable(data$uniPoikkeama[data$Sukupuoli == "Mies"], muuttuja[data$Sukupuoli == "Mies"],
                     prop.chisq = FALSE, chisq = TRUE, prop.t = FALSE))
  }
  
}
