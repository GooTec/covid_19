#install.packages('tidyverse')
library('tidyverse')
library(dplyr)

setwd("/Users/82103/Documents/Biostat/200829_data")


# calculate days and standardized Government index
gov_info = read.csv("KOR_Government_info.csv") # raw_input
Date = as.character(gov_info$Date)
date = as.Date(Date, format='%Y%m%d')
days = as.numeric(date - date[1])+1
gov_info = mutate(gov_info, Government = Raw_Government/3*100, days)
# head(gov_info)
# write.csv(gov_info, "KOR_Government_info_days.csv")

########### input csv ###############
#data = read.csv("KOR_additive_measure_200826.csv")
data = read.csv("KOR_multiplicative_measure_200826.csv")
#####################################
latest_days = data$Days[length(data$Days)]
ref_days = c(gov_info$days,latest_days)
ref_days
Government = c()
for(i in 1:(length(ref_days)-1)){
  print(i)
  interval = ref_days[i+1]-ref_days[i]
  print(interval)
  if(interval == 0){interval = 1}
  Government = c(Government,rep(gov_info$Government[i],interval))
}
Government
data$Government = Government[1:length(data$Days)]
#write.csv(data, "KOR_additive_measure_200823.csv")
write.csv(data, "KOR_multiplicative_measure_200823.csv")




