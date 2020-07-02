##-------------------------------------- load library --------------------------------------##
library(dplyr)
library(ggplot2)
library(nls2)

calculate_mse <- function(y, y_hat){
  mse = sum((y-y_hat)^2)/length(y)
  
  return(mse)
}
##------------------------------------------------------------------------------------------## 

# setwd('/data/member/twgoo/COVID-19/')
input_df = read.csv("https://query.data.world/s/umemrwc5l4krte4tywzenip3jz4vrn", header=TRUE, stringsAsFactors=FALSE);


selected_df = select(input_df, c('Case_Type', 'Cases','Difference', 'Date', 'Country_Region', 'Province_State'))
selected_df$Date =  as.Date(selected_df$Date ,format = "%m/%d/%y")  
preprocessed_df = mutate(selected_df, Days_after_First_Case= as.integer(Date - as.Date('2020-01-22'))) %>% 
  arrange(Country_Region, Case_Type, Days_after_First_Case)

confirmed_df = filter(preprocessed_df, Case_Type=='Confirmed') %>% 
  group_by(Country_Region, Date, Days_after_First_Case)  %>%
  summarise(Cases = sum(Cases),Difference = sum(Difference))

# data_dir = '/data/member/twgoo/COVID-19/'
# write.csv(confirmed_df, paste(data_dir,'COVID_19_Confirmed.csv'))
countries = unique(confirmed_df$Country_Region)
length(countries)
for( country in countries){
  print(country)
  confirmed_df_country = filter(confirmed_df, Country_Region== country)
  
  first_conf_day_country = min(confirmed_df_country$Days_after_First_Case[which(confirmed_df_country$Cases > 0)])
  first_conf_date_country = confirmed_df_country$Date[confirmed_df_country$Days_after_First_Case == first_conf_day_country]
  final_conf_date_country = "2020-04-19"
  final_conf_day_country = confirmed_df_country$Days_after_First_Case[confirmed_df_country$Date == final_conf_date_country]
  print(paste("For fitting model, data from ", first_conf_date_country, " to ", final_conf_date_country, "will be used"))
  
  confirmed_df_country = mutate(confirmed_df_country, Days_after_Start = Days_after_First_Case - first_conf_day_country)
  train_df_country = confirmed_df_country[confirmed_df_country$Days_after_First_Case >= first_conf_day_country & confirmed_df_country$Days_after_First_Case <= final_conf_day_country ,]
  subset_df_country = confirmed_df_country[confirmed_df_country$Days_after_First_Case >= first_conf_day_country, ]
  test_df_country = confirmed_df_country[confirmed_df_country$Days_after_First_Case > final_conf_day_country, ]
  
  logit_init_country = NULL
  bert_init_country = NULL
  gomp_init_country = NULL
  
  alg = "plinear-random"
  
  set.seed(777)
  start_logit_country = data.frame(a = c(0,max(subset_df_country$Cases)), b = c(-10,10), c = c(-10,10)) 
  try(logit_init_country <- nls2(Cases ~ a / (1 + exp(b-(c*Days_after_Start))), data = subset_df_country, 
                                 algorithm = alg, start = start_logit_country, control = nls.control(maxiter = 1000)))
  set.seed(777)
  start_bert_country =  data.frame(a = c(0,max(subset_df_country$Cases)), b = c(-10,10), c = c(-10,10))
  try(bert_init_country <- nls2(Cases ~ a * ((1- exp(-b* Days_after_Start))^c), data = subset_df_country, 
                                algorithm = alg, start = start_bert_country, control = nls.control(maxiter = 1000)))
  set.seed(777)
  start_gomp_country = data.frame(a = c(0,max(subset_df_country$Cases)), b = c(-100,100), c = c(-100,100)) # initial values interval: a = 0 ~ maximum cases, b = 0 ~ 100, c = 0 ~ 1
  try(gomp_init_country <- nls2(Cases ~ a * exp(b * (-1) * exp((-1) * c * Days_after_Start)), data = subset_df_country, 
                                algorithm = alg, start = start_gomp_country, control = nls.control(maxiter = 1000)))
  
  
  coef(logit_init_country)
  # 1.148241e+04 4.887964e+00 1.094904e-01
  coef(bert_init_country)
  # 8.942384e+03 5.157849e-02 8.419548e+00
  coef(gomp_init_country)
  # 4.055685e+03 6.385097e+01 8.238666e-02
  
  alg = "default"
  nls_logit_country = logit_init_country
  nls_bert_country = bert_init_country
  nls_gomp_country = gomp_init_country
  
  y_hat_logit_country = predict(nls_logit_country, subset_df_country)
  y_hat_bert_country = predict(nls_bert_country, subset_df_country)
  y_hat_gomp_country = predict(nls_gomp_country, subset_df_country)
  
  x = unique(subset_df_country$Days_after_First_Case)
  
  predict_country = data.frame(x,y_hat_logit_country, y_hat_bert_country, y_hat_gomp_country)
  
  predict_df_country = data.frame(x=rep(predict_country$x,3), 
                                  yhat_cases = c(y_hat_logit_country, y_hat_bert_country, y_hat_gomp_country),
                                  # yhat difference = yhat(t+1) - yhat(t)
                                  yhat_difference = c(y_hat_logit_country - c(0, y_hat_logit_country[-length(y_hat_logit_country)]),  
                                                      y_hat_bert_country - c(0, y_hat_bert_country[-length(y_hat_bert_country)]),
                                                      y_hat_gomp_country - c(0, y_hat_gomp_country[-length(y_hat_gomp_country)])),
                                  type = rep(c("Logistic model", "Bertalanffy model", "Gompertz model"), each = nrow(predict_country)))
  
  t0_country = as.character(sort(subset_df_country$Date)[1])
  model_labels = c("Logistic model", "Bertalanffy model", "Gompertz model")
  models_country = list(nls_logit_country, nls_bert_country, nls_gomp_country)
  
  subset_df_country$Days_after_Start[1] = 0.1 
  nls_pois_country <- glm('Difference ~ log(Days_after_Start) + Days_after_Start',family = 'poisson',subset_df_country)
  
  y_hat_pois_country = predict(nls_pois_country, subset_df_country)
  y_hat_pois_country = exp(y_hat_pois_country)
  
  for (i in length(y_hat_bert_country):2){
    y_hat_pois_country[i] = sum(y_hat_pois_country[1:i])
  } 
  x = unique(subset_df_country$Days_after_First_Case)
  
  predict_country = data.frame(x,y_hat_logit_country, y_hat_bert_country, y_hat_gomp_country, y_hat_pois_country)
  model_labels = c("Logistic model", "Bertalanffy model", "Gompertz model", 'Poisson model')
  
  predict_df_country = data.frame(x=rep(predict_country$x,4), 
                              yhat_cases = c(y_hat_logit_country, y_hat_bert_country, y_hat_gomp_country, y_hat_pois_country),
                              type = rep(model_labels, each = nrow(predict_country)))
  
  t0_country = as.character(sort(subset_df_country$Date)[1])
  models_country = list(nls_logit_country, nls_bert_country, nls_gomp_country, nls_pois_country)
  
  p_1_country = ggplot(data=subset_df_country,aes(x=Days_after_First_Case,y=(Cases)))+
    geom_point(color='blue', shape = 1, size=5)+ 
    theme_bw()+
    geom_line(data=predict_df_country[which(!is.na(match(predict_df_country$type, model_labels))),], 
              aes(x=x,y=(yhat_cases), colour = type, linetype = type), size=1.5)+
    ggtitle(paste(country, 'Fitting plot'))
  
  print(p_1_country)
  
}


print(nls_pois_country$coefficients[2])

