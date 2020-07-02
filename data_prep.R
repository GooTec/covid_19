# install.packages("geepack")
# install.packages("xlsx")

library(geepack)
library(dplyr)
library(tibble)
library(xlsx)

setwd("~/Downloads/trajectory analysis/")

# load data
orig_df <- read.csv("OxCGRT_latest.csv", header = FALSE, stringsAsFactors = FALSE)
country_list <- read.csv("CountryNameList.csv", stringsAsFactors = FALSE)
policy_summary <- read.csv("policySummary.csv", stringsAsFactors = FALSE)

# setting names
colnames(orig_df) <- orig_df[1, ]
rownames(policy_summary) <- policy_summary$PolicyName

# adding country code to country name
country_list <- merge(country_list, unique(select(orig_df, CountryName, CountryCode)), by = "CountryName")

# subset countries which are used
df <- orig_df %>% filter(orig_df$CountryName %in% country_list$CountryName)
df$Date <- strptime(as.character(df$Date), "%Y%m%d")
df <- add_column(df, Days = (difftime(df$Date, min(df$Date), units="days") + 1), .after = "Date")

# remove unnecessary columns
df <- df[, setdiff(colnames(df), c("CountryName", "Date", "ConfirmedDeaths", "StringencyIndex", "StringencyIndexForDisplay", "StringencyLegacyIndex", "StringencyLegacyIndexForDisplay", "GovernmentResponseIndex", "GovernmentResponseIndexForDisplay", "ContainmentHealthIndex", "ContainmentHealthIndexForDisplay", "EconomicSupportIndex", "EconomicSupportIndexForDisplay", "M1_Wildcard"))]

# formatting each variables
for (i in 1:ncol(df)) {
  if (colnames(df)[i] %in% policy_summary$PolicyName) {
    each_policy <- colnames(df)[i]
    if (policy_summary[each_policy, "Type"] == "Ordinal" & 
        policy_summary[each_policy, "hasFlag"] == TRUE) {
      max_level <- policy_summary[each_policy,"max"]
      nona <- !(is.na(df[,i]))
      df[nona, i] <- 
        (as.numeric(df[nona, i]) + as.numeric(df[nona, i + 1])) / 
        (max_level + !(is.na(as.numeric(df[nona, i + 1]))))
    }
    else if (policy_summary[each_policy, "Type"] == "Ordinal" & 
             policy_summary[each_policy, "hasFlag"] == FALSE) {
      max_level <- policy_summary[each_policy,"max"]
      nona <- !(is.na(df[,i]))
      df[nona, i] <- 
        (as.numeric(df[nona, i])) / (max_level)
    }
    else if (policy_summary[each_policy,"Type"] == "USD") {
      nona <- !(is.na(df[,i]))
      df[,i] <- as.numeric(df[,i])
      df[nona & df[,i] > 0, i] <- log10(as.numeric(df[nona & df[,i] > 0, i]))
    }
  }
}

# listing into data with each country
data <- list()
for (each_country in unique(df$CountryCode)) {
  temp_data <- df[df$CountryCode == each_country, !grepl("Flag", colnames(df))]
  temp_data[, "ConfirmedCases"] <- as.numeric(temp_data[, "ConfirmedCases"])
  temp_data$DailyConfirmed <- temp_data[, "ConfirmedCases"] - c(0, temp_data[1:nrow(temp_data) - 1,"ConfirmedCases"])
  data <- append(data, list(temp_data))
  names(data)[length(data)] <- each_country
}

