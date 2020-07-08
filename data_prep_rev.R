# load(file="data_prep.RData")

######################################### summary #########################################
# result data
# data_add -> additive measure policy data
# data_mult -> multiplicative measure policy data


# install.packages("geepack")
# install.packages("xlsx")
setwd("/Users/taehyun/Desktop/trajectory analysis_rev1")
library(geepack)
library(dplyr)
library(tibble)
library(zoo)
library(stringr)

# load data
orig_df <- read.csv("OxCGRT_latest.csv", header = FALSE, stringsAsFactors = FALSE)
country_list <- read.csv("CountryNameList.csv", stringsAsFactors = FALSE)
policy_summary <- read.csv("policySummary.csv", stringsAsFactors = FALSE)

# setting column names
colnames(orig_df) <- orig_df[1, ]
colnames(orig_df) <- gsub(" ","_",colnames(orig_df))
colnames(orig_df) <- gsub("/","_",colnames(orig_df))
rownames(policy_summary) <- policy_summary$PolicyName

# adding country code to country list
country_list <- merge(country_list, unique(select(orig_df, CountryName, CountryCode)), by = "CountryName")

# subset countries which are used
df <- orig_df %>% filter(orig_df$CountryName %in% country_list$CountryName)
df$Date <- strptime(as.character(df$Date), "%Y%m%d")

# remove unnecessary columns
df <- df[, setdiff(colnames(df), c("CountryName", "Date", "ConfirmedDeaths", "StringencyIndex", "StringencyIndexForDisplay", "StringencyLegacyIndex", "StringencyLegacyIndexForDisplay", "GovernmentResponseIndex", "GovernmentResponseIndexForDisplay", "ContainmentHealthIndex", "ContainmentHealthIndexForDisplay", "EconomicSupportIndex", "EconomicSupportIndexForDisplay", "M1_Wildcard"))]

policy_summary$PolicyName = gsub("\\/", "_", policy_summary$PolicyName)
rownames(policy_summary) = gsub("\\/", "_", rownames(policy_summary))

non_flag_col <- names(df) %in% c(policy_summary[policy_summary$Type != "Flag", "PolicyName"], "ConfirmedCases")
data <- c()

for (each_country in unique(df$CountryCode)) {
  # subset each country data
  temp_data <- df[df$CountryCode == each_country, ]
  temp_data[, "ConfirmedCases"] <- as.numeric(temp_data[, "ConfirmedCases"])
  
  # setting initial na values to 0
  temp_data[1, non_flag_col][which(is.na(temp_data[1, non_flag_col]) | temp_data[1, non_flag_col] == "")] = 0
  temp_data[, non_flag_col] = apply(temp_data[,non_flag_col], 2, function(x) x = na.locf(as.numeric(x)))
  temp_data[, "ConfirmedCases"] = na.locf(temp_data[, "ConfirmedCases"])
  
  # if cumulative sum is less than previous day, replace value with average previous and following data
  if (sum(diff(temp_data[, "ConfirmedCases"]) < 0) > 0) {
    odd_idx <- which((diff(temp_data[, "ConfirmedCases"]) < 0) %in% TRUE) + 1
    temp_data[odd_idx, "ConfirmedCases"] <- as.integer((temp_data[odd_idx - 1, "ConfirmedCases"] + temp_data[odd_idx + 1, "ConfirmedCases"]) / 2)
  }
  
  temp_data$DailyConfirmed =  c(0, diff(temp_data[, "ConfirmedCases"]))
  
  # subset with first day data
  first_day <- match(TRUE, temp_data$ConfirmedCases > 0)
  temp_data <- temp_data[first_day:(nrow(temp_data) - 1),]
  temp_data$Days = 1:nrow(temp_data)
  data <- rbind(data, temp_data)
}

# additive policy flag measurement
data_add <- data

# multiplicative policy flag measurement
data_mult <- data

# formatting each variables
for (i in 1:ncol(data)) {
  if (colnames(data)[i] %in% policy_summary$PolicyName) {
    each_policy <- colnames(data)[i]
    # when policy is ordinal with flag
    if (policy_summary[each_policy, "Type"] == "Ordinal" & policy_summary[each_policy, "hasFlag"] == TRUE) {
      max_level <- policy_summary[each_policy,"max"]
      nona <- !(is.na(data[,i]))
      
      # (curr_level + flag(if given)) / (max_level + flag(if given))
      data_add[nona, i] <- 
        apply(cbind(as.numeric(data_add[nona, i]), as.numeric(data_add[nona, i + 1])), 1, function(x) sum(x[1], x[2], na.rm = T)) / 
        (max_level + !(is.na(as.numeric(data_add[nona, i + 1]))))
      
      # (curr_level / max_level) * (1 + flag(if given))
      data_mult[nona, i] <- 
        apply(cbind((as.numeric(data_mult[nona, i]) / max_level), as.numeric(data_mult[nona, i + 1])), 1, function(x) prod(x[1], (1 + x[2]), na.rm = T))
    }
    
    # when policy is ordinal without flag
    else if (policy_summary[each_policy, "Type"] == "Ordinal" & policy_summary[each_policy, "hasFlag"] == FALSE) {
      max_level <- policy_summary[each_policy,"max"]
      nona <- !(is.na(data[,i]))
      data_add[nona, i] <- 
        (as.numeric(data_add[nona, i])) / (max_level)
      data_mult[nona, i] <- 
        (as.numeric(data_mult[nona, i])) / (max_level)
    }
    
    # when policy is numerical
    else if (policy_summary[each_policy,"Type"] == "USD") {
      nona <- !(is.na(data[,i]))
      data_add[,i] <- as.numeric(data_add[,i])
      data_add[!(is.na(data_add[,i])) & data_add[,i] > 0, i] <- log10(data_add[!(is.na(data_add[,i])) & data_add[,i] > 0, i])
      data_mult[,i] <- as.numeric(data_mult[,i])
      data_mult[!(is.na(data_mult[,i])) & data_mult[,i] > 0, i] <- log10(data_mult[!(is.na(data_mult[,i])) & data_mult[,i] > 0, i])
    }
  }
}

# Excluding ECU Data
data_add = data_add[which(data_add$CountryCode != "ECU"),]
data_mult = data_mult[which(data_mult$CountryCode != "ECU"),]


setwd("/Users/taehyun/Desktop/covid_19/")
# Merging Population Info.
population_df = read.csv("populations.csv", header = F)
colnames(population_df) = c("Code", "Population")

data_add = left_join(data_add, population_df, by = c("CountryCode" = "Code"))  
data_add$Population = as.numeric(as.character(data_add$Population))
data_mult = left_join(data_mult, population_df, by = c("CountryCode" = "Code"))  
data_mult$Population = as.numeric(as.character(data_mult$Population))

# Drop unused level(ECU)
data_add$CountryCode = factor(data_add$CountryCode) 
data_mult$CountryCode = factor(data_mult$CountryCode)

# Drop Flag Columns
data_add = data_add[,!str_detect(colnames(data_add), "Flag")] 
data_mult = data_mult[,!str_detect(colnames(data_mult), "Flag")]

write.csv(data_add, "data_additive_measure.csv", row.names = F)
write.csv(data_mult, "data_multiplicative_measure.csv", row.names = F)

# save.image(file = "data_prep.RData")