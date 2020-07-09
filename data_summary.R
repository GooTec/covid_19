library(dplyr)
setwd('/data/member/twgoo/COVID-19/trajectory_analysis/')
policy_df = read.csv('oxford_korea.csv', row.names =1)

# input_df = read.csv("https://query.data.world/s/umemrwc5l4krte4tywzenip3jz4vrn", header=TRUE, stringsAsFactors=FALSE);
# 
# 
# selected_df = select(input_df, c('Case_Type', 'Cases','Difference', 'Date', 'Country_Region', 'Province_State'))
# selected_df$Date =  as.Date(selected_df$Date ,format = "%m/%d/%y")  
# preprocessed_df = mutate(selected_df, Days_after_First_Case= as.integer(Date - as.Date('2020-01-22'))) %>% 
#   arrange(Country_Region, Case_Type, Days_after_First_Case)
# 
# confirmed_df = filter(preprocessed_df, Case_Type=='Confirmed') %>% 
#   group_by(Country_Region, Date, Days_after_First_Case)  %>%
#   summarise(Cases = sum(Cases),Difference = sum(Difference))
# 
# country = 'Korea, South'
# confirmed_df_country = filter(confirmed_df, Country_Region== country)
# # unique(confirmed_df$Country_Region)
# first_conf_day_country = min(confirmed_df_country$Days_after_First_Case[which(confirmed_df_country$Cases > 0)])
# first_conf_day_country
# first_conf_date_country = confirmed_df_country$Date[confirmed_df_country$Days_after_First_Case == first_conf_day_country]
# final_conf_date_country = "2020-06-17"
# final_conf_day_country = confirmed_df_country$Days_after_First_Case[confirmed_df_country$Date == final_conf_date_country]
# final_conf_day_country
# print(paste("For fitting model, data from ", first_conf_date_country, " to ", final_conf_date_country, "will be used"))
# 
# confirmed_df_country = mutate(confirmed_df_country, Days_after_Start = Days_after_First_Case - first_conf_day_country)
# confirmed_df_country
# df_country = confirmed_df_country[confirmed_df_country$Days_after_First_Case >= first_conf_day_country & confirmed_df_country$Days_after_First_Case <= final_conf_day_country ,]
# # subset_df_country = confirmed_df_country[confirmed_df_country$Days_after_First_Case >= first_conf_day_country, ]
# # test_df_country = confirmed_df_country[confirmed_df_country$Days_after_First_Case > final_conf_day_country, ]


