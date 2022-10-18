
################################################################################
#
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

histTmax <- read.csv(here("Data", "tasMax_rcp45.csv"), skip=7)
rcp45Tmax <- read.csv(here("Data", "tasMax_rcp45.csv"), skip=7)
rcp85Tmax <- read.csv(here("Data", "tasMax_rcp85.csv"), skip=7)

################################################################################
# Historic mung--extract HIST from here matches 85 hist
################################################################################

#Drop modeled min and max fields
histTmax <- select(histTmax, -(ModeledRCP4.5RangeMin:ModeledRCP4.5RangeMax))

#Filter 2035-2064 data
histTmax <-  filter(histTmax, year %in% c(1960:1991))
head(histTmax)

#Convert to long form
histTmax <- histTmax %>%
  gather(Observed : MIROC5, key=model, value=temp )
histTmax <- mutate(histTmax, scenario="hist")
head(histTmax)

################################################################################
# RCP45 mung
################################################################################

#Drop modeled min and max fields
rcp45Tmax <- select(rcp45Tmax, -(ModeledRCP4.5RangeMin:Observed))

#Filter 2035-2064 data
rcp45Tmax <-  filter(rcp45Tmax, year %in% c(2035:2064))
head(rcp45Tmax)

#Convert to long form
rcp45Tmax <- rcp45Tmax %>%
  gather(ACCESS1.0 : MIROC5, key=model, value=temp )
rcp45Tmax <- mutate(rcp45Tmax, scenario="rcp45")
head(rcp45Tmax)

################################################################################
# RCP85 mung
################################################################################

#Drop modeled min and max fields
rcp85Tmax <- select(rcp85Tmax, -(ModeledRCP8.5RangeMin:Observed))

#Filter 2035-2064 data
rcp85Tmax <-  filter(rcp85Tmax, year %in% c(2035:2064))
head(rcp85Tmax)

#Convert to long form
rcp85Tmax <- rcp85Tmax %>%
  gather(ACCESS1.0 : MIROC5, key=model, value=temp )
rcp85Tmax <- mutate(rcp85Tmax, scenario="rcp85")
head(rcp85Tmax)

################################################################################
#Tie data together
Tmax <-rbind(histTmax, rcp45Tmax, rcp85Tmax)
head(Tmax)
################################################################################

################################################################################
# Convert F to C
################################################################################
Tmax$tempC <- (Tmax$temp-32)*(5/9)
head(Tmax)
################################################################################
# Calculate group means for each period and model
################################################################################
datC <- Tmax %>%
  group_by(scenario, model) %>%
  summarize(m = mean(tempC),   # calculates the mean
            minVal = min(tempC),
            maxVal = max(tempC))   # calculates min and max values
head(datC)

#write intermediate long & wide data
write.csv(Tmax, here("Output", "maxTempLong.csv"))
write.csv(datC, here("Output", "maxTemp.csv"))

table_Tmax <- datC %>%
  gt() %>%
  tab_header (
    title=" Maximum Annual Mean Temperature, Celsius"
  )  %>%
  cols_label(
    m = "Mean Annual Max Temperature",
    minVal = "Minimum of Annual Av Max Temperature",
    maxVal = "Maximum of Annual Av Max Temperature",
  )

table_Tmax

table_Tmax %>%
  gtsave("Output/table_Tmax.rtf")

################################################################################
# Boxplot
################################################################################
datC%>%
  ggplot( aes(x=scenario, y=m)) +
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
  ylab("Mean maximum temp, C")


ggsave(here("Output", "TmaxBoxplots.png"), width = 10,height=8,units='in',dpi=300)

################################################################################
# Calculate mean and max/min of all models for period/scenario
################################################################################
head(Tmax)
datHist <- filter(Tmax, model == 'Observed' & scenario == 'hist')
datHist <- datHist %>%
  group_by(scenario) %>%
  summarize(mean = mean(tempC),   # calculates the mean
            minVal = min(tempC),
            maxVal = max(tempC))   # calculates min and max values
datHist


head(Tmax)
datFuture <- filter(Tmax, scenario != 'hist', )
  head(datFuture)
datFuture <-  datFuture %>%
  group_by(scenario) %>%
  summarize(mean = mean(tempC),   # calculates the mean
            minVal = min(tempC),
            maxVal = max(tempC))   # calculates min and max values
datFuture

