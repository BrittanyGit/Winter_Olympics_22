# Connor Nickol 

#Read in libraries
library(tidyverse)

#Read in datasets
details<-read.csv("discipline_details.csv")
medals<-read.csv("events_medals.csv")
events<-read.csv("olympic_events.csv")
sample<-read.csv("sample_submission.csv")
test<-read.csv("test.csv")
train<-read.csv("train.csv")
location<-read.csv("world_country_and_usa_states_latitude_and_longitude_values.csv")
cities<-read.csv("worldcities.csv")
air_qual<-read.csv("waqi-covid19-airqualitydata-2020.csv")
missing<-read.csv("Missing_cities.csv")
codes<-read.csv("countries_codes_and_coordinates.csv")

#Break up latitude and longitude into US states and world countries
states<-location%>%select(usa_state,usa_state_code,usa_state_latitude,usa_state_longitude)%>%na.omit()
countries<-location%>%select(country,country_code,latitude,longitude)

#Clean up the cities dataset and add in the missing cities that I created in excel 
cities_clean<-cities%>%
  select(city_ascii,country,lat,lng)%>%
  full_join(missing,by=c("city_ascii"="event_city","lat","lng","country"="Country"))

#Join with the events data so we have lat and lng for the host cities 
events_loc<-events%>%left_join(cities_clean,by=c("event_country"="country","event_city"="city_ascii"))

#Fix missing observations
events_loc[22,11]<-43.6028
events_loc[22,12]<-39.7342                                
events_loc[23,11]<-37.3705
events_loc[23,12]<-128.3900 

#Add 3 letter codes to the countries lat and lng data
codes_clean<-codes%>%select(Country,Alpha.3.code)
countries_codes<-countries%>%left_join(codes_clean,by=c("country"="Country"))

#Remove the whitespace from the 3 letter codes
countries_codes$Alpha.3.code<-trimws(countries_codes$Alpha.3.code, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")

#Add/change missing 3 letter codes
countries_codes[54,5]<-"GER"
countries_codes[41,5]<-"SUI"
countries_codes[246,]<-c("Soviet Union","UR","61.524010","105.318756","URS")
countries_codes[247,]<-c("Russian Athletes","UR","61.524010","105.318756","ROC")
countries_codes[163,5]<-"NED"
countries_codes[248,]<-c("East Germany","EG","51.165691","10.451526","GDR")
countries_codes[249,]<-c("West Germany","EG","51.165691","10.451526","FRG")
countries_codes[96,5]<-"CRO"
countries_codes[250,]<-c("Czechoslovakia","CZ","49.817492","15.472962","TCH")
countries_codes[197,5]<-"SLO"
countries_codes[22,5]<-"BUL"
countries_codes[133,5]<-"LAT"

#Simplify the dataset
event_city<-events_loc%>%select(event_city,event_year,lat,lng)

#Filter to only be golds and add in host lat and lng 
gold<-details%>%select(event_year,discipline,category,
                       date,n_participants,n_country_participants,
                       gold_medalist,gold_country)%>%
  left_join(event_city,by=c("event_year"))

#Remove a few incomplete observations
gold<-gold%>%
  filter(gold_medalist != "—" & gold_medalist != "Mixed team" & gold_country != "EUN")%>%
  mutate(gold_country=as.character(gold_country))

#Rename these columns 
names(gold)[10]<-"host_lat"
names(gold)[11]<-"host_lng"

#Simplify the codes data, remove nas 
medal_country<-countries_codes%>%
  select(Alpha.3.code,latitude,longitude)%>%
  na.omit()

#Rename column
names(medal_country)[1]<-"gold_country"

#Add in the particpating country lat and lng 
gold_all<-gold%>%left_join(medal_country,by=c("gold_country"))%>%
  mutate(distance=0)%>%
  mutate(latitude=as.numeric(latitude),longitude=as.numeric(longitude))

#Function to calculate euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

#Calculate euclidean distance from host country to winning country 
for (i in 1:nrow(gold_all)){
  host<-c(gold_all$host_lat[i],gold_all$host_lng[i])
  medal_country<-c(gold_all$latitude[i],gold_all$longitude[i])
  gold_all$distance[i]<-euclidean(host,medal_country)
}  


