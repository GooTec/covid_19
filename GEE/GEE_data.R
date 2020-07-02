setwd('/data/member/twgoo/COVID-19/trajectory_analysis/')
load(file="trajectory_analysis.RData")

input_df = read.csv("https://query.data.world/s/umemrwc5l4krte4tywzenip3jz4vrn", header=TRUE, stringsAsFactors=FALSE);
selected_df = select(input_df, c('Case_Type', 'Cases','Difference', 'Date', 'Country_Region', 'Province_State'))
selected_df$Date =  as.Date(selected_df$Date ,format = "%m/%d/%y")  
preprocessed_df = mutate(selected_df, Days_after_First_Case= as.integer(Date - as.Date('2020-01-22'))) %>% 
  arrange(Country_Region, Case_Type, Days_after_First_Case)

confirmed_df = filter(preprocessed_df, Case_Type=='Confirmed') %>% 
  group_by(Country_Region, Date, Days_after_First_Case)  %>%
  summarise(Cases = sum(Cases),Difference = sum(Difference))

confirmed_df


start_date = '2020-01-01'
for( country in country_list$CountryCode){
  data_country = data[country]
  print(head(data_country))
}

write.csv( data$KOR , 'oxford_korea.csv')
write.csv( data$AUS , 'oxford_aus.csv')
write.csv( data$GBR , 'oxford_gbr.csv')
# data$AUS

print(country_list)
