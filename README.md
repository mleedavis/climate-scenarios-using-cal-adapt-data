# climate-scenarios-using-cal-adapt-data
Generate GCM level box-plots and tabular output for LOCA CMIP5 historic, RCP 4.5 and RCP 8.5 scenarios from annual average data served up through the CalAdapt program (https://cal-adapt.org/tools/annual-averages)

Annual averages for any area within the California and Nevada program area of CalAdapt may be analyzed by downloading csv data from the above link; add data to your "Data" folder after cloning to use relative paths via here()

By default this code calculates model means using a historic baseline of 1960-1991 and mid-term future of 2035-2064 to match timelines used by the NatureServe Habitat Climate Change Vulnerability Index (HCCVI) program (https://www.natureserve.org/ccvi-ecosystems)

Code blocks are verbose for Annual Maximum Temperature, Annual Minimum Temperature, and Annual Accumulated Precipitation. 

Model Spread for Nevada Maximum Annual Temperature Historic (1991-1960) and Future (2035-2064) Scenarios: !["Maximum Annual Temperature Historic (1960-1991) vs Future (2035-2064)"](images/TmaxBoxplots.jpg)

