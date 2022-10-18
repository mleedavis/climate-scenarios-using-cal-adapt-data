
################################################################################
# Total Annual precipitation
# Calculates model means for historic (1960-1991) and mid term (2035-2064)
# rcp45 & rcp85 scenarios
#
# Generates boxplots of model means grouped by scenario
#
################################################################################

library("tidyverse")
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(gt) #for creating nice tables
library(hrbrthemes)
library(viridis)
library(here)

#Set Working Directory
here()

################################################################################
#READ DATA
################################################################################

histPrecip <- read.csv(here("Data", "precip_rcp45.csv"), skip=7)
rcp45Precip <- read.csv(here("Data", "precip_rcp45.csv"), skip=7)
rcp85Precip <- read.csv(here("Data", "precip_rcp85.csv"), skip=7)

################################################################################
# Historic mung--extract HIST from here matches 85 hist
################################################################################

#Drop modeled min and max fields
histPrecip <- select(histPrecip, -(ModeledRCP4.5RangeMin:ModeledRCP4.5RangeMax))

#Filter 2035-2064 data
histPrecip <-  filter(histPrecip, year %in% c(1960:1991))
head(histPrecip)

#Convert to long form
histPrecip <- histPrecip %>%
  gather(Observed : MIROC5, key=model, value=precip )
histPrecip <- mutate(histPrecip, scenario="hist")
head(histPrecip)

################################################################################
# RCP45 mung
################################################################################

#Drop modeled min and max fields
rcp45Precip <- select(rcp45Precip, -(ModeledRCP4.5RangeMin:Observed))

#Filter 2035-2064 data
rcp45Precip <-  filter(rcp45Precip, year %in% c(2035:2064))
head(rcp45Precip)

#Convert to long form
rcp45Precip <- rcp45Precip %>%
  gather(ACCESS1.0 : MIROC5, key=model, value=precip )
rcp45Precip <- mutate(rcp45Precip, scenario="rcp45")
head(rcp45Precip)

################################################################################
# RCP85 mung
################################################################################

#Drop modeled min and max fields
rcp85Precip <- select(rcp85Precip, -(ModeledRCP8.5RangeMin:Observed))

#Filter 2035-2064 data
rcp85Precip <-  filter(rcp85Precip, year %in% c(2035:2064))
head(rcp85Precip)

#Convert to long form
rcp85Precip <- rcp85Precip %>%
  gather(ACCESS1.0 : MIROC5, key=model, value=precip )
rcp85Precip <- mutate(rcp85Precip, scenario="rcp85")
head(rcp85Precip)

################################################################################
#Tie data together
Precip <-rbind(histPrecip, rcp45Precip, rcp85Precip)
head(Precip)
################################################################################

################################################################################
# Convert inches to centimeters
################################################################################
Precip$precipcm <- (Precip$precip)*2.54
head(Precip)

################################################################################
# Calculate group means
################################################################################
datP <- Precip %>%
  group_by(scenario, model) %>%
  summarize(meanCM = mean(precipcm),   # calculates the mean
            minVal = min(precipcm),
            maxVal = max(precipcm))   # calculates the standard deviation

#write intermediate long & wide data
write.csv(Precip, here("Output", "precipLong.csv"))
write.csv(datP, here("Output", "precip.csv"))

table_precip <- datP %>%
  gt() %>%
  tab_header (
    title=" Annual Mean Precipitation, cm"
  )  %>%
  cols_label(
    meanCM = "Annual Mean Precipitation",
    minVal = "Minimum of Annual Mean Precipitation",
    maxVal = "Maximum of Annual Mean Precipitation",
  )

table_precip

#table_precip %>%
  #gtsave("table_precip.rtf")
gtsave(table_precip, here("Output", "table_precip.rtf"))
################################################################################
# Boxplot
################################################################################
datP%>%
  ggplot( aes(x=scenario, y=meanCM)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=1.2, alpha=1.9, width=0.15) +
  #geom_text(aes(label=model), hjust = -0.3) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  #ggtitle("Forecast Mean Annual Maximum Temperature C, Nevada 2035-2065 by Model \n
  #       LOCA statistically downscaled CMIP5 data") +
  xlab("")+
  ylab("Mean precipitation, cm")


ggsave(here("Output", "PrecipBoxplots.png"), width = 10,height=8,units='in',dpi=300)

################################################################################
# Calculate mean and max/min of all models for period/scenario
################################################################################
head(Precip)
datHist <- filter(Precip, model == 'Observed' & scenario == 'hist')
datHist <- datHist %>%
  group_by(scenario) %>%
  summarize(meanCM = mean(precipcm),   # calculates the mean
            minVal = min(precipcm),
            maxVal = max(precipcm))  # calculates min and max values
datHist

head(Precip)
datFuture <- filter(Precip, scenario != 'hist', )
head(datFuture)
datFuture <-  datFuture %>%
  group_by(scenario) %>%
  summarize(meanCM = mean(precipcm),   # calculates the mean
            minVal = min(precipcm),
            maxVal = max(precipcm))  # calculates min and max values
datFuture



