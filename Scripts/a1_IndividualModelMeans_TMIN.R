
################################################################################
# TMIN
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

histTmin <- read.csv(here("Data", "tasMin_rcp45.csv"), skip=7)
rcp45Tmin <- read.csv(here("Data", "tasMin_rcp45.csv"), skip=7)
rcp85Tmin <- read.csv(here("Data", "tasMin_rcp85.csv"), skip=7)

################################################################################
# Historic mung--extract HIST from here matches 85 hist
################################################################################

#Drop modeled min and max fields
histTmin <- select(histTmin, -(ModeledRCP4.5RangeMin:ModeledRCP4.5RangeMax))

#Filter 2035-2064 data
histTmin <-  filter(histTmin, year %in% c(1960:1991))
head(histTmin)

#Convert to long form
histTmin <- histTmin %>%
  gather(Observed : MIROC5, key=model, value=temp )
histTmin <- mutate(histTmin, scenario="hist")
head(histTmin)

################################################################################
# RCP45 mung
################################################################################

#Drop modeled min and max fields
rcp45Tmin <- select(rcp45Tmin, -(ModeledRCP4.5RangeMin:Observed))

#Filter 2035-2064 data
rcp45Tmin <-  filter(rcp45Tmin, year %in% c(2035:2064))
head(rcp45Tmin)

#Convert to long form
rcp45Tmin <- rcp45Tmin %>%
  gather(ACCESS1.0 : MIROC5, key=model, value=temp )
rcp45Tmin <- mutate(rcp45Tmin, scenario="rcp45")
head(rcp45Tmin)

################################################################################
# RCP85 mung
################################################################################

#Drop modeled min and max fields
rcp85Tmin <- select(rcp85Tmin, -(ModeledRCP8.5RangeMin:Observed))

#Filter 2035-2064 data
rcp85Tmin <-  filter(rcp85Tmin, year %in% c(2035:2064))
head(rcp85Tmin)

#Convert to long form
rcp85Tmin <- rcp85Tmin %>%
  gather(ACCESS1.0 : MIROC5, key=model, value=temp )
rcp85Tmin <- mutate(rcp85Tmin, scenario="rcp85")
head(rcp85Tmin)

################################################################################
#Tie data together
Tmin <-rbind(histTmin, rcp45Tmin, rcp85Tmin)
head(Tmin)
################################################################################

################################################################################
# Convert F to C
################################################################################
Tmin$tempC <- (Tmin$temp-32)*(5/9)
head(Tmin)
################################################################################
# Calculate group means
################################################################################
datC <- Tmin %>%
  group_by(scenario, model) %>%
  summarize(m = mean(tempC),   # calculates the mean
            minVal = min(tempC),
            maxVal = max(tempC))   # calculates the standard deviation

#write intermediate long & wide data
write.csv(Tmin, here("Output", "minTempLong.csv"))
write.csv(datC, here("Output", "minTemp.csv"))

table_Tmin <- datC %>%
  gt() %>%
  tab_header (
    title=" Minimum Annual Mean Temperature, Celsius"
  )  %>%
  cols_label(
    m = "Mean Annual Minimum Temperature",
    minVal = "Minimum of Annual Av Minimum Temperature",
    maxVal = "Maximum of Annual Av Minimum Temperature",
  )

table_Tmin

#table_Tmin %>%
 #gtsave("table_Tmin.rtf")
gtsave(table_Tmin, here("Output", "table_Tmin.rtf"))

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
  ylab("Mean minimum temp, C")


ggsave(here("Output", "TminBoxplots.png"), width = 10,height=8,units='in',dpi=300)


################################################################################
# Calculate mean and max/min of all models for period/scenario
################################################################################
head(Tmin)
datHist <- filter(Tmin, model == 'Observed' & scenario == 'hist')
datHist <- datHist %>%
  group_by(scenario) %>%
  summarize(mean = mean(tempC),   # calculates the mean
            minVal = min(tempC),
            maxVal = max(tempC))   # calculates min and max values
datHist


head(Tmin)
datFuture <- filter(Tmin, scenario != 'hist', )
head(datFuture)
datFuture <-  datFuture %>%
  group_by(scenario) %>%
  summarize(mean = mean(tempC),   # calculates the mean
            minVal = min(tempC),
            maxVal = max(tempC))   # calculates min and max values
datFuture
