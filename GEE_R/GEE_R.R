library(geepack)
library(dplyr)

setwd("/Users/taehyun/Desktop/covid_19")

######################################
# Note. 2020.07.08
# Using Offset log(Populations) in GEE
# Using data_add & data_mult
# Except ECU
#
# DailyConfirmed ~ Variable, Using Offset = log(Population)
# DailyConfirmed ~ Days + Variable, Using Offset = log(Population)
# DailyConfirmed ~ Days + log(Days) + Variable, Using Offset = log(Population)
# Cov Structure : Ind, Ex, AR1
######################################

# Loading Data 
data_add = read.csv("data_additive_measure.csv")
data_mult = read.csv("data_multiplicative_measure.csv")

# Select Policy Variables & Data
variables = colnames(data_add)[2:18]
names_vec = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "E1", "E2", "E3", "E4", "H1", "H2", "H3", "H4", "H5")

# data = data_add
data = data_mult

# DailyConfirmed ~ Variable, Using Offset = log(Population)
# 1) Independence Structure (All fitted)
Ind_res_list = list()

for (Variable in variables){
  sub_data = data[, c('DailyConfirmed', 'CountryCode', 'Population', Variable)]
  print(paste(Variable, "Start"))
  mf = formula(DailyConfirmed ~ .- CountryCode - Population)
  geeInd <- geeglm(mf, id=CountryCode, data=sub_data, offset = log(Population), family=poisson, corstr="ind")
  Ind_res_list = append(Ind_res_list, list(geeInd))
}
names(Ind_res_list) = names_vec

# 2) Exchangeable Structure (All fitted except C8(Add) / C4(Mult))
Exc_res_list = list()
for (Variable in variables){
  if (Variable %in% c("C4_Restrictions_on_gatherings")) {
    Exc_res_list = append(Exc_res_list, list(NA))
    next
  }  
  sub_data = data[, c('DailyConfirmed', 'CountryCode', 'Population', Variable)]
  print(paste(Variable, "Start"))
  mf = formula(DailyConfirmed ~ .- CountryCode - Population)
  geeExc <- geeglm(mf, id=CountryCode, data=sub_data, offset = log(Population), family=poisson, corstr="ex")
  Exc_res_list = append(Exc_res_list, list(geeExc))
}
names(Exc_res_list) = names_vec

# 3) AR1 Structure (All fitted except C3, C4(Add))
AR_res_list = list()
for (Variable in variables){
  #if (Variable %in% c("C3_Cancel_public_events", "C4_Restrictions_on_gatherings")) {
  #  AR_res_list = append(AR_res_list, NA)
  #  next
  #}  
  sub_data = data[, c('DailyConfirmed', 'CountryCode', 'Population', Variable)]
  print(paste(Variable, "Start"))
  mf = formula(DailyConfirmed ~ .- CountryCode - Population)
  geeAR <- geeglm(mf, id=CountryCode, data=sub_data, offset = log(Population), family=poisson, corstr="ar1")
  AR_res_list = append(AR_res_list, list(geeAR))
}

names(AR_res_list) = names_vec

# Results
res_Ind_matrix = matrix(nrow = 17, ncol = 4)
colnames(res_Ind_matrix) = c("Intercept", "Pval", "Coefficient", "Pval")
rownames(res_Ind_matrix) = variables
res_Exc_matrix = res_Ind_matrix
res_AR_matrix =  res_Ind_matrix

for (i in 1:17) {
  if (sum(is.na(Ind_res_list[[i]]))) {
    res_Ind_matrix[i, 1:4] = rep(NA, 4)
  } else {
    res_Ind_matrix[i, 1:4] = c(summary(Ind_res_list[[i]])$coefficients$Estimate[1],
                               summary(Ind_res_list[[i]])$coefficients$Pr[1],
                               summary(Ind_res_list[[i]])$coefficients$Estimate[2],
                               summary(Ind_res_list[[i]])$coefficients$Pr[2]) 
  }
}

for (i in 1:17) {
  if (sum(is.na(Exc_res_list[[i]]))) {
    res_Exc_matrix[i, 1:4] = rep(NA, 4)
  } else {
    res_Exc_matrix[i, 1:4] = c(summary(Exc_res_list[[i]])$coefficients$Estimate[1],
                               summary(Exc_res_list[[i]])$coefficients$Pr[1],
                               summary(Exc_res_list[[i]])$coefficients$Estimate[2],
                               summary(Exc_res_list[[i]])$coefficients$Pr[2]) 
  }
}

for (i in 1:17) {
  if (sum(is.na(AR_res_list[[i]]))) {
    res_AR_matrix[i, 1:4] = rep(NA, 4)
  } else {
    res_AR_matrix[i, 1:4] = c(summary(AR_res_list[[i]])$coefficients$Estimate[1],
                              summary(AR_res_list[[i]])$coefficients$Pr[1],
                              summary(AR_res_list[[i]])$coefficients$Estimate[2],
                              summary(AR_res_list[[i]])$coefficients$Pr[2]) 
  }
}

res_Ind_matrix
res_Exc_matrix
res_AR_matrix
write.csv( res_Ind_matrix, "Mult_GEE_Ind_result_R.csv")
write.csv( res_Exc_matrix, "Mult_GEE_Exc_result_R.csv")
write.csv( res_AR_matrix, "Mult_GEE_AR_result_R.csv")

# DailyConfirmed ~ Days + Variable, Using Offset
# 1) Independence Structure (All fitted)
Days_Ind_res_list = list()
for (Variable in variables){
  sub_data = data[, c('DailyConfirmed', 'Days', 'CountryCode', 'Population', Variable)]
  print(paste(Variable, "Start"))
  mf = formula(DailyConfirmed ~ .- CountryCode - Population)
  geeInd <- geeglm(mf, id=CountryCode, data=sub_data, offset = log(Population), family=poisson, corstr="ind")
  Days_Ind_res_list = append(Days_Ind_res_list, list(geeInd))
}
names(Days_Ind_res_list) = names_vec

# 2) Exchangeable Structure (All fitted except C5, C6, C8, E3 (Add) / C5, E3 (Mult))
Days_Exc_res_list = list()
for (Variable in variables){
  if (Variable %in% c("C5_Close_public_transport", "E3_Fiscal_measures")) {
    Days_Exc_res_list = append(Days_Exc_res_list, list(NA))
    next
  }  
  sub_data = data[, c('DailyConfirmed', 'Days', 'CountryCode', 'Population', Variable)]
  print(paste(Variable, "Start"))
  mf = formula(DailyConfirmed ~ .- CountryCode - Population)
  geeExc <- geeglm(mf, id=CountryCode, data=sub_data, offset = log(Population), family=poisson, corstr="ex")
  Days_Exc_res_list = append(Days_Exc_res_list, list(geeExc))
}
names(Days_Exc_res_list) = names_vec

# 3) AR1 Structure (All fitted except C4, C7 (Add) / C3 (Mult))
Days_AR_res_list = list()
for (Variable in variables){
  if (Variable %in% c("C3_Cancel_public_events")) {
    Days_AR_res_list = append(Days_AR_res_list, list(NA))
    next
  }  
  sub_data = data[, c('DailyConfirmed', 'Days', 'CountryCode', 'Population', Variable)]
  print(paste(Variable, "Start"))
  mf = formula(DailyConfirmed ~ .- CountryCode - Population)
  geeAR <- geeglm(mf, id=CountryCode, data=sub_data, offset = log(Population), family=poisson, corstr="ar1")
  Days_AR_res_list = append(Days_AR_res_list, list(geeAR))
}
names(Days_AR_res_list) = names_vec

# Results
res_Days_Ind_matrix = matrix(nrow = 17, ncol = 6)
colnames(res_Days_Ind_matrix) = c("Intercept", "Pval", "Days", "Pval", "Coefficient", "Pval")
rownames(res_Days_Ind_matrix) = variables
res_Days_Exc_matrix = res_Days_Ind_matrix
res_Days_AR_matrix =  res_Days_Ind_matrix

for (i in 1:17) {
  if (sum(is.na(Days_Ind_res_list[[i]]))) {
    res_Days_Ind_matrix[i, 1:6] = rep(NA, 6)
  } else {
    res_Days_Ind_matrix[i, 1:6] = c(summary(Days_Ind_res_list[[i]])$coefficients$Estimate[1],
                                    summary(Days_Ind_res_list[[i]])$coefficients$Pr[1],
                                    summary(Days_Ind_res_list[[i]])$coefficients$Estimate[2],
                                    summary(Days_Ind_res_list[[i]])$coefficients$Pr[2],
                                    summary(Days_Ind_res_list[[i]])$coefficients$Estimate[3],
                                    summary(Days_Ind_res_list[[i]])$coefficients$Pr[3]) 
  }
}

for (i in 1:17) {
  if (sum(is.na(Days_Exc_res_list[[i]]))) {
    res_Days_Exc_matrix[i, 1:6] = rep(NA, 6)
  } else {
    res_Days_Exc_matrix[i, 1:6] = c(summary(Days_Exc_res_list[[i]])$coefficients$Estimate[1],
                                    summary(Days_Exc_res_list[[i]])$coefficients$Pr[1],
                                    summary(Days_Exc_res_list[[i]])$coefficients$Estimate[2],
                                    summary(Days_Exc_res_list[[i]])$coefficients$Pr[2],
                                    summary(Days_Exc_res_list[[i]])$coefficients$Estimate[3],
                                    summary(Days_Exc_res_list[[i]])$coefficients$Pr[3])
  }  
}

for (i in 1:17) {
  if (sum(is.na(Days_AR_res_list[[i]]))) {
    res_Days_AR_matrix[i, 1:6] = rep(NA, 6)
  } else {
    res_Days_AR_matrix[i, 1:6] = c(summary(Days_AR_res_list[[i]])$coefficients$Estimate[1],
                                   summary(Days_AR_res_list[[i]])$coefficients$Pr[1],
                                   summary(Days_AR_res_list[[i]])$coefficients$Estimate[2],
                                   summary(Days_AR_res_list[[i]])$coefficients$Pr[2],
                                   summary(Days_AR_res_list[[i]])$coefficients$Estimate[3],
                                   summary(Days_AR_res_list[[i]])$coefficients$Pr[3]) 
  }
}

res_Days_Ind_matrix
res_Days_Exc_matrix
res_Days_AR_matrix
write.csv( res_Days_Ind_matrix, "Mult_Days_GEE_Ind_result_R.csv")
write.csv( res_Days_Exc_matrix, "Mult_Days_GEE_Exc_result_R.csv")
write.csv( res_Days_AR_matrix, "Mult_Days_GEE_AR_result_R.csv")

# DailyConfirmed ~ Days + log(Days) + Variable, Using Offset = log(popultaion size)
# 1) Independence Structure (All fitted)
Days_log_Ind_res_list = list()
for (Variable in variables){
  #if (Variable %in% c("C1_School_closing")) {
  #  Days_AR_res_list = append(Days_AR_res_list, list(NA))
  #  next
  #}  
  sub_data = data[, c('DailyConfirmed', 'Days', 'CountryCode', 'Population', Variable)]
  sub_data$logDays = log(sub_data$Days)
  print(paste(Variable, "Start"))
  mf = formula(DailyConfirmed ~ .-CountryCode - Population)
  geeInd <- geeglm(mf, id=CountryCode, data=sub_data, offset=log(Population), family=poisson, corstr="ind")
  Days_log_Ind_res_list = append(Days_log_Ind_res_list, list(geeInd))
}
names(Days_log_Ind_res_list) = names_vec

# 2) Exchangeable Structure (All fitted except C1, C2, C3, C4, C5, ...)
if (FALSE) {
  Days_log_Exc_res_list = list()
  for (Variable in variables){
    #if (Variable %in% c("C1_School_closing", "C2_Workplace_closing", "C3_Cancel_public_events", "C4_Restrictions_on_gatherings")) {
    #  Days_log_Exc_res_list = append(Days_log_Exc_res_list, list(NA))
    #  next
    #}
    if (Variable %in% c("C1_School_closing")) {
      Days_log_Exc_res_list = append(Days_log_Exc_res_list, list(NA))
      next
    }
    sub_data = data[, c('DailyConfirmed', 'Days', 'CountryCode', 'Population', Variable)]
    sub_data$logDays = log(sub_data$Days)
    print(paste(Variable, "Start"))
    mf = formula(DailyConfirmed ~ .-CountryCode - Population)
    geeExc <- geeglm(mf, id=CountryCode, data=sub_data, offset=log(Population), family=poisson, corstr="ex")
    Days_log_Exc_res_list = append(Days_log_Exc_res_list, list(geeExc))
  }
  names(Days_log_Exc_res_list) = names_vec
}

# 3) AR1 Structure (All fitted except C1, C2, C3, ...)
if (FALSE) {
  Days_log_AR_res_list = list()
  for (Variable in variables){
    if (Variable %in% c("C1_School_closing", "C2_Workplace_closing", "C3_Cancel_public_events", "C4_Restrictions_on_gatherings")) {
      Days_log_AR_res_list = append(Days_log_AR_res_list, list(NA))
      next
    }
    sub_data = data[, c('DailyConfirmed', 'Days', 'CountryCode', 'Population', Variable)]
    sub_data$logDays = log(sub_data$Days)
    print(paste(Variable, "Start"))
    mf = formula(DailyConfirmed ~ .-CountryCode - Population)
    geeAR <- geeglm(mf, id=CountryCode, data=sub_data, offset=log(Population), family=poisson, corstr="ar1")
    Days_log_AR_res_list = append(Days_log_AR_res_list, list(geeAR))
  }
  names(Days_log_AR_res_list) = names_vec
}

# Results
res_Days_log_Ind_matrix = matrix(nrow = 17, ncol = 8)
colnames(res_Days_log_Ind_matrix) = c("Intercept", "Pval", "Days", "Pval", "logDays", "Pval", "Coefficient", "Pval")
rownames(res_Days_log_Ind_matrix) = variables
res_Days_log_Exc_matrix = res_Days_log_Ind_matrix
res_Days_log_AR_matrix = res_Days_log_Ind_matrix

for (i in 1:17) {
  if (sum(is.na(Days_log_Ind_res_list[[i]]))) {
    res_Days_log_Ind_matrix[i, 1:8] = rep(NA, 8)
  } else {
    res_Days_log_Ind_matrix[i, 1:8] = c(summary(Days_log_Ind_res_list[[i]])$coefficients$Estimate[1],
                                        summary(Days_log_Ind_res_list[[i]])$coefficients$Pr[1],
                                        summary(Days_log_Ind_res_list[[i]])$coefficients$Estimate[2],
                                        summary(Days_log_Ind_res_list[[i]])$coefficients$Pr[2],
                                        summary(Days_log_Ind_res_list[[i]])$coefficients$Estimate[4],
                                        summary(Days_log_Ind_res_list[[i]])$coefficients$Pr[4], 
                                        summary(Days_log_Ind_res_list[[i]])$coefficients$Estimate[3],
                                        summary(Days_log_Ind_res_list[[i]])$coefficients$Pr[3]) 
  }
}

if (FALSE) {
  for (i in 1:17) {
    if (sum(is.na(Days_log_Exc_res_list[[i]]))) {
      res_Days_log_Exc_matrix[i, 1:8] = rep(NA, 8)
    } else {
      res_Days_log_Exc_matrix[i, 1:8] = c(summary(Days_log_Exc_res_list[[i]])$coefficients$Estimate[1],
                                          summary(Days_log_Exc_res_list[[i]])$coefficients$Pr[1],
                                          summary(Days_log_Exc_res_list[[i]])$coefficients$Estimate[2],
                                          summary(Days_log_Exc_res_list[[i]])$coefficients$Pr[2],
                                          summary(Days_log_Exc_res_list[[i]])$coefficients$Estimate[4],
                                          summary(Days_log_Exc_res_list[[i]])$coefficients$Pr[4], 
                                          summary(Days_log_Exc_res_list[[i]])$coefficients$Estimate[3],
                                          summary(Days_log_Exc_res_list[[i]])$coefficients$Pr[3]) 
    }
  }
  
  for (i in 1:17) {
    if (sum(is.na(Days_log_AR_res_list[[i]]))) {
      res_Days_log_AR_matrix[i, 1:8] = rep(NA, 8)
    } else {
      res_Days_log_AR_matrix[i, 1:8] = c(summary(Days_log_AR_res_list[[i]])$coefficients$Estimate[1],
                                         summary(Days_log_AR_res_list[[i]])$coefficients$Pr[1],
                                         summary(Days_log_AR_res_list[[i]])$coefficients$Estimate[2],
                                         summary(Days_log_AR_res_list[[i]])$coefficients$Pr[2],
                                         summary(Days_log_AR_res_list[[i]])$coefficients$Estimate[4],
                                         summary(Days_log_AR_res_list[[i]])$coefficients$Pr[4], 
                                         summary(Days_log_AR_res_list[[i]])$coefficients$Estimate[3],
                                         summary(Days_log_AR_res_list[[i]])$coefficients$Pr[3]) 
    }
  }
}

res_Days_log_Ind_matrix
# res_Days_log_Exc_matrix
# res_Days_log_AR_matrix

write.csv( res_Days_log_Ind_matrix, "Mult_logdays_GEE_Ind_result_R.csv")
# write.csv( res_Days_log_Ind_matrix, "Mult_logdays_GEE_Exc_result_R.csv")
# write.csv( res_Days_log_Ind_matrix, "Mult_logdays_GEE_AR_result_R.csv")

# Save Images
# save.image(Ind_res_list, Exc_res_list, AR_res_list, Days_Ind_res_list, Days_Exc_res_list, Days_AR_res_list, Days_log_Ind_res_list, file="res_list.RData") 