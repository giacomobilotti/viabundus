#### Load some of the climate proxies from NOAA ####

library(readxl)
library(tidyverse)
library(ggplot2)

climdir <- file.path('data', 'raw_data', 'NOAA')

#### PAGES 2k ####
# Dataset from "palaeo-reconstruction"
# Tab 2: PAGES 2k Network regional reconstruction series with uncertainties
# All temperatures are anomalies relative to 1961-1990 reference period in °C

pages  <- read_excel(file.path(climdir, 'reconstruction', "PAGES2k-DatabaseS2-Regional-Temperature-Reconstructions.xlsx"), 
           sheet = "PAGES2k recon's - annual")

pages <- pages[,grepl('Europe', colnames(pages))]
colnames(pages) <- c('Year', 'T', 'min', 'max')
pages <- pages[-1,]
pages <- pages |>
  mutate(across(everything(), as.numeric))  

## 30 years avg
pages30  <- read_excel(file.path(climdir, 'reconstruction', 'PAGES2k-DatabaseS2-Regional-Temperature-Reconstructions.xlsx'), 
    sheet = "PAGES 2k - 30 yr")

pages30 <- pages30[,grepl('Mid-point|Europe', colnames(pages30))]
pages30 <- pages30[,-3]
colnames(pages30) <- c('Year', 'T', 'SD')

## Plot
plot(pages$Year, pages$T, type = "n",
     xlab = "Year CE", ylab = "Temperature anomaly (°C)",
     main = "European Temperature reconstruction (PAGES 2k)",
     xlim = c(1200, 1800))

# Add shaded uncertainty band
polygon(c(pages$Year, rev(pages$Year)),
        c(pages$min, rev(pages$max)),
        col = adjustcolor("skyblue", alpha.f = 0.4),
        border = NA)

# Add mean line
lines(pages$Year, pages$T, col = 'grey25')
lines(pages30$Year, pages30$T, col = 'darkred', lwd = 1.5)
# Add grid for readability
grid()
box()

#### Cunningham et al 2013 ####
## Palaeo Record
# 1000 Year Composite Sea Surface Temperature Record from the North Atlantic
#  cunningham2013-nena-comp.txt: NE North Atlantic composite SST reconstruction from all proxy records in this study.
# reconstructed sea-surface temperature (SST) anomalies relative to the modern climatological baseline 

Cunningham <- read.table(file.path(climdir, 'cunningham2013', 'cunningham2013-nena-comp.txt'),
                         sep = "\t", comment.char = "#")
# header argument does not work properly, add manually
colnames(Cunningham) <- Cunningham[1,]
Cunningham <- Cunningham[-1,]
Cunningham <- Cunningham |>
  mutate(across(everything(), as.numeric))  

plot(x = 1201:1799, y = rev(Cunningham$`SST-anom.recon`[Cunningham$age_AD > 1200 & Cunningham$age_AD < 1800]),
     type = 'l', lwd = 2, col = 'darkblue', main = 'North Atlantic SST anomalies (Cunningham et al. 2013)',
     xlab = 'Year', ylab = 'Anomaly')

#### Xu et al 2024 ####
# North Atlantic-European Sector Jet Stream (EU JSL) Reconstruction Data from 1300-2004 CE

Xu <- read.table(file.path(climdir, 'reconstruction', 'xu2024', 
                           'xu2024-eu_jsl.txt'), header = TRUE,
                         sep = "\t", comment.char = "#") |>
  subset(Year <= 1800)
# the second column (Ins_EU_JSL) refers to the period when it was directly measured and not reconstructed

# 30-year moving average (centred)
Xu30 <- stats::filter(Xu$recon_EU_JSL, rep(1/30, 30), sides = 2)

## Plot
plot(Xu$Year, Xu$recon_EU_JSL, type = "n",
     xlab = "Year CE", ylab = "JS position",
     main = "N Atlantic-EU Jet Stream reconstruction Jul-Aug (Xu et al 2024)",
     xlim = c(1300, 1800))

# Add shaded uncertainty band
polygon(c(Xu$Year, rev(Xu$Year)),
        c(Xu$recon_EU_JSL_lower, rev(Xu$recon_EU_JSL_upper)),
        col = adjustcolor("lightgrey", alpha.f = 0.4),
        border = NA)

lines(Xu$Year, Xu$recon_EU_JSL, col = "blue", lwd = .8)
lines(Xu$Year, Xu30, col = "darkred", lwd = 2)

#### Büntgen et al. (2021) ####
# self-calibrating Palmer Drought Severity Index
# positive values indicate relatively wet summers, negative dry
scpdsi <- read.table(file.path(climdir, "treering", "reconstructions", "buntgen2021scpdsi-noaa.txt"), 
                     header = TRUE, 
                     comment.char = "#", 
                     sep = "\t")

head(scpdsi)

## Plot 
plot(scpdsi$Year, scpdsi$scPDSI, type = "l", xlim = c(1200, 1800),
     xlab = "Year CE", ylab = "scPDSI", lwd = .75,
     main = "Central Europe Summer Drought Reconstruction (Büntgen et al. 2021)")
# Polygon for uncertainty in sky blue
polygon(c(scpdsi$Year, rev(scpdsi$Year)),
        c(scpdsi$scPDSI., rev(scpdsi$scPDSI..1)),
        col = rgb(135/255, 206/255, 235/255, 0.5), border = NA)  # skyblue with 30% transparency

lines(scpdsi$Year, scpdsi$scPDSI50yr, col = "red", lwd = 2)

# Legend
legend("topright", legend = c("Annual", "50-yr smooth", "Uncertainty"),
       col = c("black", "red", "grey"), lty = c(1,1,2), lwd = c(1,2,1))

# Reference line
abline(h = 0, col = "steelblue", lty = 2, lwd = 2)

#### Büntgen, U., et al.  2011 #####
## Central Europe 2500 Year Tree Ring Summer Climate Reconstructions 
# Based on Oak trees treering separating between NE France, SE Germany and NE Germany
# Independent reconstructions for DE and CH are also included
## Combined T and Prec data
buentgen11 <- read_excel(
  file.path(climdir, "treering", "reconstructions", "buentgen2011europe.xls"), 
  sheet = "Fig.4 Recons", skip = 6) |>
  subset(Year <= 1800)
buentgen11 <- buentgen11[,-6]

names(buentgen11) <- c("Year", "Precipitation", "P_lo", "P_up", "P_Ind_DE",
                       "Summer Temperature", "T_lo", "T_up", "T_Ind_DE")


## Plot 
plot(buentgen11$Year, buentgen11$Precipitation, type = "l", xlim = c(1200, 1750),
     xlab = "Year CE", ylab = "Precipitation", lwd = .75,
     main = "Central Europe Precipitation Reconstruction (Büntgen et al. 2011)")
# Polygon for uncertainty in sky blue
polygon(c(buentgen11$Year, rev(buentgen11$Year)),
        c(buentgen11$P_lo, rev(buentgen11$P_up)),
        col = rgb(135/255, 206/255, 235/255, 0.5), border = NA)  # skyblue with 30% transparency

Bu30 <- stats::filter(buentgen11$Precipitation, rep(1/30, 30), sides = 2)

lines(buentgen11$Year, Bu30, col = "red", lwd = 2)

plot(buentgen11$Year, buentgen11$`Summer Temperature`, type = "l", xlim = c(1200, 1750),
     xlab = "Year CE", ylab = "Summer Temperature", lwd = .75,
     main = "Central Europe Summer T Reconstruction (Büntgen et al. 2011)")
# Polygon for uncertainty in sky blue
polygon(c(buentgen11$Year, rev(buentgen11$Year)),
        c(buentgen11$T_lo, rev(buentgen11$T_up)),
        col = rgb(135/255, 206/255, 235/255, 0.5), border = NA)  # skyblue with 30% transparency

Bu30b <- stats::filter(buentgen11$`Summer Temperature`, rep(1/30, 30), sides = 2)

lines(buentgen11$Year, Bu30b, col = "orange", lwd = 2)

### Now the regional series

buent2011reg <- read_excel(
  file.path(climdir, "treering", "reconstructions", "buentgen2011europe.xls"),
  sheet = "Fig S4 Oak Extremes", skip = 6) |>
  subset(year...1 > 1199)

nef30 <- stats::filter(buent2011reg$France_20sp_corrected, rep(1/30, 30), sides = 2)
neg30 <- stats::filter(buent2011reg$Brand_20sp_corrected, rep(1/30, 30), sides = 2)
seg30 <- stats::filter(buent2011reg$Bayern_20sp_corrected, rep(1/30, 30), sides = 2)

par(mfrow = c(3,1))
plot(buent2011reg$year...1, buent2011reg$France_20sp_corrected, type = "l", col = "grey",
     xlim = c(1300, 1800), xlab = "Year CE", ylab = "Anomaly (STDV)", main = "NEF Anomalies")
lines(buent2011reg$year...1, nef30, col = "orange", lwd = 2)
segments(buent2011reg$year...6, 0,
         buent2011reg$year...6, buent2011reg$`france-bayern extremes_2 1.5stdev`,
         col = "darkorange", lwd = 3)
segments(buent2011reg$year...6, 0,
         buent2011reg$year...6, buent2011reg$`france-brand extremes_2 1.5stdev`,
         col = "darkviolet", lwd = 3)

# Legend
legend("topleft", legend = c("Annual", "30-yr avg", "Extremes NEF-SEG", "Extremes NEF-NEG"),
       col = c("grey", "orange", "darkorange", "darkviolet"), lty = 1, lwd = 2)


plot(buent2011reg$year...1, buent2011reg$Bayern_20sp_corrected, col = "lightblue",
      xlim = c(1300, 1800), type = 'l', xlab = "Year CE", ylab = "Anomaly (STDV)", main = "SEG Anomalies")
lines(buent2011reg$year...1, seg30, col = "skyblue", lwd = 2)
segments(buent2011reg$year...6, 0,
         buent2011reg$year...6, buent2011reg$`france-bayern extremes_2 1.5stdev`,
         col = "darkorange", lwd = 3)
segments(buent2011reg$year...6, 0,
         buent2011reg$year...6, buent2011reg$`bayern-brand extremes_2 1.5stdev`,
         col = "steelblue", lwd = 3)

# Legend
legend("topleft", legend = c("Annual", "30-yr avg", "Extremes NEF-SEG", "Extremes NEF-NEG"),
       col = c("lightblue", "skyblue", "darkorange", "steelblue"), lty = 1, lwd = 2)


plot(buent2011reg$year...1, buent2011reg$Brand_20sp_corrected, col = "lightgreen",
     xlim = c(1300, 1800), type = 'l', xlab = "Year CE", ylab = "Anomaly (STDV)", main = "NEG Anomalies")
lines(buent2011reg$year...1, neg30, col = "darkgreen", lwd = 2)
segments(buent2011reg$year...6, 0,
         buent2011reg$year...6, buent2011reg$`france-brand extremes_2 1.5stdev`,
         col = "darkviolet", lwd = 3)
segments(buent2011reg$year...6, 0,
         buent2011reg$year...6, buent2011reg$`bayern-brand extremes_2 1.5stdev`,
         col = "steelblue", lwd = 3)

# Legend
legend("topleft", legend = c("Annual", "30-yr avg", "Extremes NEF-SEG", "Extremes NEF-NEG"),
       col = c("lightgreen", "darkgreen", "darkviolet", "steelblue"), lty = 1, lwd = 2)

#### Linderholm et al 2015 ####
# Fennoscandia 900 Year Summer Temperature Reconstruction from tree rings

Scandi15 <- read.table(
  file.path(climdir, 'treering', 'reconstructions', 'fennoscandia2015temp.txt'),
  header = TRUE, sep = '\t', comment.char = "#"
) |>
  subset(age_CE < 1801)

## Plot 
# 30-year moving average (centred)
Scandi30 <- stats::filter(Scandi15$temp, rep(1/30, 30), sides = 2)

## Plot
plot(Scandi15$age_CE, Scandi15$temp, type = "n",
     xlab = "Year CE", ylab = "Anomaly",
     main = "Reconstructed average Fennoscandian JJA temperature anomalies (Linderholm et al 2015)",
     xlim = c(1300, 1750))

# Add shaded uncertainty band
polygon(c(Scandi15$age_CE, rev(Scandi15$age_CE)),
        c(Scandi15$temp., rev(Scandi15$temp..1)),
        col = adjustcolor("skyblue", alpha.f = 0.4),
        border = NA)

lines(Scandi15$age_CE, Scandi15$temp, col = "steelblue", lty = 2, lwd = .5)
lines(Scandi15$age_CE, Scandi30, col = "navy", lwd = 2)
abline(h = 0, lty = 2, col = "darkred")
legend(
  "bottomleft", legend = c("Annual", "30-yr avg", "CI (+/- RMSE error)"),
  col = c("navy", "steelblue", "skyblue"), lty = 1, lwd = 2
)

#### Buntgen et al 2013 ####
# Eastern Europe 1000 Year May-June Temperature Reconstruction
# Trees used in the study come from Tatra mountain in Slovakia

EastEU13 <- read.table(
  file.path(climdir, 'treering', 'reconstructions', 'tatra2013temp-noaa.txt'),
  header = TRUE, 
  comment.char = "#"
)

# 30-year moving average (centred)
EE30 <- stats::filter(EastEU13$tempanom.MJ, rep(1/30, 30), sides = 2)


## Plot
plot(EastEU13$age_AD, EastEU13$tempanom.MJ, type = "l",
     xlab = "Year CE", ylab = "Anomaly",
     main = "Reconstructed average MJA temperature anomalies in Tatra area (Büntgen et al 2013)",
     xlim = c(1300, 1750))
## Using simple calibration error (+/- 1 RMSE)
lines(EastEU13$age_AD, EastEU13$tempanom.MJ - 0.5, col = "grey90")
lines(EastEU13$age_AD, EastEU13$tempanom.MJ + 0.5, col = "grey90")
lines(EastEU13$age_AD, EE30, col = "navy", lwd = 2)
abline(h = 0, lty = 2, col = "darkred")

#### 
