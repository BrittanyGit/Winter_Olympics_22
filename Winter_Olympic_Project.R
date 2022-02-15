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
athletes<-read.csv("athlete_events.csv")

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

## Repeating for silver medals 


#Filter to only be silvers and add in host lat and lng 
silver<-details%>%select(event_year,discipline,category,
                       date,n_participants,n_country_participants,
                       silver_medalist,silver_country)%>%
  left_join(event_city,by=c("event_year"))

#Remove a few incomplete observations
silver<-silver%>%
  filter(silver_medalist != "—" & silver_medalist != "Mixed team" & silver_country != "EUN")%>%
  mutate(silver_country=as.character(silver_country))

#Rename these columns 
names(silver)[10]<-"host_lat"
names(silver)[11]<-"host_lng"

#Simplify the codes data, remove nas 
medal_country<-countries_codes%>%
  select(Alpha.3.code,latitude,longitude)%>%
  na.omit()

#Rename column
names(medal_country)[1]<-"silver_country"

#Add in the participating country lat and lng 
silver_all<-silver%>%left_join(medal_country,by=c("silver_country"))%>%
  mutate(distance=0)%>%
  mutate(latitude=as.numeric(latitude),longitude=as.numeric(longitude))

#Function to calculate euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

#Calculate euclidean distance from host country to winning country 
for (i in 1:nrow(silver_all)){
  host<-c(silver_all$host_lat[i],silver_all$host_lng[i])
  medal_country<-c(silver_all$latitude[i],silver_all$longitude[i])
  silver_all$distance[i]<-euclidean(host,medal_country)
}


## Repeating for bronze medals 


#Filter to only be bronze and add in host lat and lng 
bronze<-details%>%select(event_year,discipline,category,
                         date,n_participants,n_country_participants,
                         bronze_medalist,bronze_country)%>%
  left_join(event_city,by=c("event_year"))

#Remove a few incomplete observations
bronze<-bronze%>%
  filter(bronze_medalist != "—" & bronze_medalist != "Mixed team" & bronze_country != "EUN")%>%
  mutate(bronze_country=as.character(bronze_country))

#Rename these columns 
names(bronze)[10]<-"host_lat"
names(bronze)[11]<-"host_lng"

#Simplify the codes data, remove nas 
medal_country<-countries_codes%>%
  select(Alpha.3.code,latitude,longitude)%>%
  na.omit()

#Rename column
names(medal_country)[1]<-"bronze_country"

#Add in the participating country lat and lng 
bronze_all<-bronze%>%left_join(medal_country,by=c("bronze_country"))%>%
  mutate(distance=0)%>%
  mutate(latitude=as.numeric(latitude),longitude=as.numeric(longitude))

#Function to calculate euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

#Calculate euclidean distance from host country to winning country 
for (i in 1:nrow(bronze_all)){
  host<-c(bronze_all$host_lat[i],bronze_all$host_lng[i])
  medal_country<-c(bronze_all$latitude[i],bronze_all$longitude[i])
  bronze_all$distance[i]<-euclidean(host,medal_country)
}

## Looking at athletes data set
athletes_w<-athletes%>%filter(Season=="Winter")

pre1960<-events%>%filter(event_year<1960)
sum(pre1960$n_medals)

post1960<-events%>%filter(event_year>=1960)
sum(post1960$n_medals)

sum(events$n_medals)

gdp<-read.csv("GDP_DATA.csv")
gdp.2<-gdp%>%select(Country.Name,Country.Code,
                    X1960,X1964,X1968,X1972,X1976,
                    X1980,X1984,X1988,X1992,X1994,X1998,
                    X2002,X2006,X2010,X2014,X2018)

gdp.2[56,2]<-"GER"
gdp.2[177,2]<-"NED"

## Working with the gdp data

#1960
gold_1960<-gold_all%>%filter(event_year==1960)
gdp_1960<-gdp.2%>%select(Country.Code,X1960)
gold_1960_gdp<-gold_1960%>%left_join(gdp_1960,by=c("gold_country"="Country.Code"))
names(gold_1960_gdp)[15]<-"gdp_per_capita"

#1964
gold_1964<-gold_all%>%filter(event_year==1964)
gdp_1964<-gdp.2%>%select(Country.Code,X1964)
gold_1964_gdp<-gold_1964%>%left_join(gdp_1964,by=c("gold_country"="Country.Code"))
names(gold_1964_gdp)[15]<-"gdp_per_capita"

#1968
gold_1968<-gold_all%>%filter(event_year==1968)
gdp_1968<-gdp.2%>%select(Country.Code,X1968)
gold_1968_gdp<-gold_1968%>%left_join(gdp_1968,by=c("gold_country"="Country.Code"))
names(gold_1968_gdp)[15]<-"gdp_per_capita"

#1972
gold_1972<-gold_all%>%filter(event_year==1972)
gdp_1972<-gdp.2%>%select(Country.Code,X1972)
gold_1972_gdp<-gold_1972%>%left_join(gdp_1972,by=c("gold_country"="Country.Code"))
names(gold_1972_gdp)[15]<-"gdp_per_capita"

#1976
gold_1976<-gold_all%>%filter(event_year==1976)
gdp_1976<-gdp.2%>%select(Country.Code,X1976)
gold_1976_gdp<-gold_1960%>%left_join(gdp_1976,by=c("gold_country"="Country.Code"))
names(gold_1976_gdp)[15]<-"gdp_per_capita"

#1980
gold_1980<-gold_all%>%filter(event_year==1980)
gdp_1980<-gdp.2%>%select(Country.Code,X1980)
gold_1980_gdp<-gold_1960%>%left_join(gdp_1980,by=c("gold_country"="Country.Code"))
names(gold_1980_gdp)[15]<-"gdp_per_capita"

#1984
gold_1984<-gold_all%>%filter(event_year==1984)
gdp_1984<-gdp.2%>%select(Country.Code,X1984)
gold_1984_gdp<-gold_1984%>%left_join(gdp_1984,by=c("gold_country"="Country.Code"))
names(gold_1984_gdp)[15]<-"gdp_per_capita"

#1988
gold_1988<-gold_all%>%filter(event_year==1988)
gdp_1988<-gdp.2%>%select(Country.Code,X1988)
gold_1988_gdp<-gold_1988%>%left_join(gdp_1988,by=c("gold_country"="Country.Code"))
names(gold_1988_gdp)[15]<-"gdp_per_capita"

#1992
gold_1992<-gold_all%>%filter(event_year==1992)
gdp_1992<-gdp.2%>%select(Country.Code,X1992)
gold_1992_gdp<-gold_1992%>%left_join(gdp_1992,by=c("gold_country"="Country.Code"))
names(gold_1992_gdp)[15]<-"gdp_per_capita"

#1994
gold_1994<-gold_all%>%filter(event_year==1994)
gdp_1994<-gdp.2%>%select(Country.Code,X1994)
gold_1994_gdp<-gold_1994%>%left_join(gdp_1994,by=c("gold_country"="Country.Code"))
names(gold_1994_gdp)[15]<-"gdp_per_capita"

#1998
gold_1998<-gold_all%>%filter(event_year==1998)
gdp_1998<-gdp.2%>%select(Country.Code,X1998)
gold_1998_gdp<-gold_1992%>%left_join(gdp_1998,by=c("gold_country"="Country.Code"))
names(gold_1998_gdp)[15]<-"gdp_per_capita"

#2002
gold_2002<-gold_all%>%filter(event_year==2002)
gdp_2002<-gdp.2%>%select(Country.Code,X2002)
gold_2002_gdp<-gold_2002%>%left_join(gdp_2002,by=c("gold_country"="Country.Code"))
names(gold_2002_gdp)[15]<-"gdp_per_capita"

#2006
gold_2006<-gold_all%>%filter(event_year==2006)
gdp_2006<-gdp.2%>%select(Country.Code,X2006)
gold_2006_gdp<-gold_2006%>%left_join(gdp_2006,by=c("gold_country"="Country.Code"))
names(gold_2006_gdp)[15]<-"gdp_per_capita"

#2010
gold_2010<-gold_all%>%filter(event_year==2010)
gdp_2010<-gdp.2%>%select(Country.Code,X2010)
gold_2010_gdp<-gold_2010%>%left_join(gdp_2010,by=c("gold_country"="Country.Code"))
names(gold_2010_gdp)[15]<-"gdp_per_capita"

#2014
gold_2014<-gold_all%>%filter(event_year==2014)
gdp_2014<-gdp.2%>%select(Country.Code,X2014)
gold_2014_gdp<-gold_2014%>%left_join(gdp_2014,by=c("gold_country"="Country.Code"))
names(gold_2014_gdp)[15]<-"gdp_per_capita"

#2018
gold_2018<-gold_all%>%filter(event_year==2018)
gdp_2018<-gdp.2%>%select(Country.Code,X2018)
gold_2018_gdp<-gold_2018%>%left_join(gdp_2018,by=c("gold_country"="Country.Code"))
names(gold_2018_gdp)[15]<-"gdp_per_capita"

all_gold_gdp<-gold_1960_gdp%>%full_join(gold_1964_gdp)%>%
  full_join(gold_1968_gdp)%>%
  full_join(gold_1972_gdp)%>%
  full_join(gold_1976_gdp)%>%
  full_join(gold_1980_gdp)%>%
  full_join(gold_1984_gdp)%>%
  full_join(gold_1988_gdp)%>%
  full_join(gold_1992_gdp)%>%
  full_join(gold_1994_gdp)%>%
  full_join(gold_1998_gdp)%>%
  full_join(gold_2002_gdp)%>%
  full_join(gold_2006_gdp)%>%
  full_join(gold_2010_gdp)%>%
  full_join(gold_2014_gdp)%>%
  full_join(gold_2018_gdp)

sum(is.na(all_gold_gdp$gdp_per_capita))

## Repeating for silver

#1960
silver_1960<-silver_all%>%filter(event_year==1960)
gdp_1960<-gdp.2%>%select(Country.Code,X1960)
silver_1960_gdp<-silver_1960%>%left_join(gdp_1960,by=c("silver_country"="Country.Code"))
names(silver_1960_gdp)[15]<-"gdp_per_capita"

#1964
silver_1964<-silver_all%>%filter(event_year==1964)
gdp_1964<-gdp.2%>%select(Country.Code,X1964)
silver_1964_gdp<-silver_1960%>%left_join(gdp_1964,by=c("silver_country"="Country.Code"))
names(silver_1964_gdp)[15]<-"gdp_per_capita"

#1968
silver_1968<-silver_all%>%filter(event_year==1968)
gdp_1968<-gdp.2%>%select(Country.Code,X1968)
silver_1968_gdp<-silver_1968%>%left_join(gdp_1968,by=c("silver_country"="Country.Code"))
names(silver_1968_gdp)[15]<-"gdp_per_capita"

#1972
silver_1972<-silver_all%>%filter(event_year==1972)
gdp_1972<-gdp.2%>%select(Country.Code,X1972)
silver_1972_gdp<-silver_1972%>%left_join(gdp_1972,by=c("silver_country"="Country.Code"))
names(silver_1972_gdp)[15]<-"gdp_per_capita"

#1976
silver_1976<-silver_all%>%filter(event_year==1976)
gdp_1976<-gdp.2%>%select(Country.Code,X1976)
silver_1976_gdp<-silver_1976%>%left_join(gdp_1976,by=c("silver_country"="Country.Code"))
names(silver_1976_gdp)[15]<-"gdp_per_capita"

#1980
silver_1980<-silver_all%>%filter(event_year==1980)
gdp_1980<-gdp.2%>%select(Country.Code,X1980)
silver_1980_gdp<-silver_1980%>%left_join(gdp_1980,by=c("silver_country"="Country.Code"))
names(silver_1980_gdp)[15]<-"gdp_per_capita"

#1984
silver_1984<-silver_all%>%filter(event_year==1984)
gdp_1984<-gdp.2%>%select(Country.Code,X1984)
silver_1984_gdp<-silver_1984%>%left_join(gdp_1984,by=c("silver_country"="Country.Code"))
names(silver_1984_gdp)[15]<-"gdp_per_capita"

#1988
silver_1988<-silver_all%>%filter(event_year==1988)
gdp_1988<-gdp.2%>%select(Country.Code,X1988)
silver_1988_gdp<-silver_1988%>%left_join(gdp_1988,by=c("silver_country"="Country.Code"))
names(silver_1988_gdp)[15]<-"gdp_per_capita"

#1992
silver_1992<-silver_all%>%filter(event_year==1992)
gdp_1992<-gdp.2%>%select(Country.Code,X1992)
silver_1992_gdp<-silver_1992%>%left_join(gdp_1992,by=c("silver_country"="Country.Code"))
names(silver_1992_gdp)[15]<-"gdp_per_capita"

#1994
silver_1994<-silver_all%>%filter(event_year==1994)
gdp_1994<-gdp.2%>%select(Country.Code,X1994)
silver_1994_gdp<-silver_1994%>%left_join(gdp_1994,by=c("silver_country"="Country.Code"))
names(silver_1994_gdp)[15]<-"gdp_per_capita"

#1998
silver_1998<-silver_all%>%filter(event_year==1998)
gdp_1998<-gdp.2%>%select(Country.Code,X1998)
silver_1998_gdp<-silver_1998%>%left_join(gdp_1998,by=c("silver_country"="Country.Code"))
names(silver_1998_gdp)[15]<-"gdp_per_capita"

#2002
silver_2002<-silver_all%>%filter(event_year==2002)
gdp_2002<-gdp.2%>%select(Country.Code,X2002)
silver_2002_gdp<-silver_2002%>%left_join(gdp_2002,by=c("silver_country"="Country.Code"))
names(silver_2002_gdp)[15]<-"gdp_per_capita"

#2006
silver_2006<-silver_all%>%filter(event_year==2006)
gdp_2006<-gdp.2%>%select(Country.Code,X2006)
silver_2006_gdp<-silver_2006%>%left_join(gdp_2006,by=c("silver_country"="Country.Code"))
names(silver_2006_gdp)[15]<-"gdp_per_capita"


#2010
silver_2010<-silver_all%>%filter(event_year==2010)
gdp_2010<-gdp.2%>%select(Country.Code,X2010)
silver_2010_gdp<-silver_2010%>%left_join(gdp_2010,by=c("silver_country"="Country.Code"))
names(silver_2010_gdp)[15]<-"gdp_per_capita"

#2014
silver_2014<-silver_all%>%filter(event_year==2014)
gdp_2014<-gdp.2%>%select(Country.Code,X2014)
silver_2014_gdp<-silver_2014%>%left_join(gdp_2014,by=c("silver_country"="Country.Code"))
names(silver_2014_gdp)[15]<-"gdp_per_capita"

#2018
silver_2018<-silver_all%>%filter(event_year==2018)
gdp_2018<-gdp.2%>%select(Country.Code,X2018)
silver_2018_gdp<-silver_2018%>%left_join(gdp_2018,by=c("silver_country"="Country.Code"))
names(silver_2018_gdp)[15]<-"gdp_per_capita"

all_silver_gdp<-silver_1960_gdp%>%full_join(silver_1964_gdp)%>%
  full_join(silver_1968_gdp)%>%
  full_join(silver_1972_gdp)%>%
  full_join(silver_1976_gdp)%>%
  full_join(silver_1980_gdp)%>%
  full_join(silver_1984_gdp)%>%
  full_join(silver_1988_gdp)%>%
  full_join(silver_1992_gdp)%>%
  full_join(silver_1994_gdp)%>%
  full_join(silver_1998_gdp)%>%
  full_join(silver_2002_gdp)%>%
  full_join(silver_2006_gdp)%>%
  full_join(silver_2010_gdp)%>%
  full_join(silver_2014_gdp)%>%
  full_join(silver_2018_gdp)

sum(is.na(all_silver_gdp$gdp_per_capita))

## Repeating for Bronze

#1960
bronze_1960<-bronze_all%>%filter(event_year==1960)
gdp_1960<-gdp.2%>%select(Country.Code,X1960)
bronze_1960_gdp<-bronze_1960%>%left_join(gdp_1960,by=c("bronze_country"="Country.Code"))
names(bronze_1960_gdp)[15]<-"gdp_per_capita"

#1964
bronze_1964<-bronze_all%>%filter(event_year==1964)
gdp_1964<-gdp.2%>%select(Country.Code,X1964)
bronze_1964_gdp<-bronze_1964%>%left_join(gdp_1964,by=c("bronze_country"="Country.Code"))
names(bronze_1964_gdp)[15]<-"gdp_per_capita"

#1968
bronze_1968<-bronze_all%>%filter(event_year==1968)
gdp_1968<-gdp.2%>%select(Country.Code,X1968)
bronze_1968_gdp<-bronze_1968%>%left_join(gdp_1968,by=c("bronze_country"="Country.Code"))
names(bronze_1968_gdp)[15]<-"gdp_per_capita"

#1972
bronze_1972<-bronze_all%>%filter(event_year==1972)
gdp_1972<-gdp.2%>%select(Country.Code,X1972)
bronze_1972_gdp<-bronze_1972%>%left_join(gdp_1972,by=c("bronze_country"="Country.Code"))
names(bronze_1972_gdp)[15]<-"gdp_per_capita"

#1976
bronze_1976<-bronze_all%>%filter(event_year==1976)
gdp_1976<-gdp.2%>%select(Country.Code,X1976)
bronze_1976_gdp<-bronze_1976%>%left_join(gdp_1976,by=c("bronze_country"="Country.Code"))
names(bronze_1976_gdp)[15]<-"gdp_per_capita"

#1980
bronze_1980<-bronze_all%>%filter(event_year==1980)
gdp_1980<-gdp.2%>%select(Country.Code,X1980)
bronze_1980_gdp<-bronze_1980%>%left_join(gdp_1980,by=c("bronze_country"="Country.Code"))
names(bronze_1980_gdp)[15]<-"gdp_per_capita"

#1984
bronze_1984<-bronze_all%>%filter(event_year==1984)
gdp_1984<-gdp.2%>%select(Country.Code,X1984)
bronze_1984_gdp<-bronze_1984%>%left_join(gdp_1984,by=c("bronze_country"="Country.Code"))
names(bronze_1984_gdp)[15]<-"gdp_per_capita"

#1988
bronze_1988<-bronze_all%>%filter(event_year==1988)
gdp_1988<-gdp.2%>%select(Country.Code,X1988)
bronze_1988_gdp<-bronze_1988%>%left_join(gdp_1988,by=c("bronze_country"="Country.Code"))
names(bronze_1988_gdp)[15]<-"gdp_per_capita"

#1992
bronze_1992<-bronze_all%>%filter(event_year==1992)
gdp_1992<-gdp.2%>%select(Country.Code,X1992)
bronze_1992_gdp<-bronze_1992%>%left_join(gdp_1992,by=c("bronze_country"="Country.Code"))
names(bronze_1992_gdp)[15]<-"gdp_per_capita"

#1994
bronze_1994<-bronze_all%>%filter(event_year==1994)
gdp_1994<-gdp.2%>%select(Country.Code,X1994)
bronze_1994_gdp<-bronze_1994%>%left_join(gdp_1994,by=c("bronze_country"="Country.Code"))
names(bronze_1994_gdp)[15]<-"gdp_per_capita"

#1998
bronze_1998<-bronze_all%>%filter(event_year==1998)
gdp_1998<-gdp.2%>%select(Country.Code,X1998)
bronze_1998_gdp<-bronze_1998%>%left_join(gdp_1998,by=c("bronze_country"="Country.Code"))
names(bronze_1998_gdp)[15]<-"gdp_per_capita"

#2002
bronze_2002<-bronze_all%>%filter(event_year==2002)
gdp_2002<-gdp.2%>%select(Country.Code,X2002)
bronze_2002_gdp<-bronze_2002%>%left_join(gdp_2002,by=c("bronze_country"="Country.Code"))
names(bronze_2002_gdp)[15]<-"gdp_per_capita"

#2006
bronze_2006<-bronze_all%>%filter(event_year==2006)
gdp_2006<-gdp.2%>%select(Country.Code,X2006)
bronze_2006_gdp<-bronze_2006%>%left_join(gdp_2006,by=c("bronze_country"="Country.Code"))
names(bronze_2006_gdp)[15]<-"gdp_per_capita"


#2010
bronze_2010<-bronze_all%>%filter(event_year==2010)
gdp_2010<-gdp.2%>%select(Country.Code,X2010)
bronze_2010_gdp<-bronze_2010%>%left_join(gdp_2010,by=c("bronze_country"="Country.Code"))
names(bronze_2010_gdp)[15]<-"gdp_per_capita"

#2014
bronze_2014<-bronze_all%>%filter(event_year==2014)
gdp_2014<-gdp.2%>%select(Country.Code,X2014)
bronze_2014_gdp<-bronze_2014%>%left_join(gdp_2014,by=c("bronze_country"="Country.Code"))
names(bronze_2014_gdp)[15]<-"gdp_per_capita"

#2018
bronze_2018<-bronze_all%>%filter(event_year==2018)
gdp_2018<-gdp.2%>%select(Country.Code,X2018)
bronze_2018_gdp<-bronze_2018%>%left_join(gdp_2018,by=c("bronze_country"="Country.Code"))
names(bronze_2018_gdp)[15]<-"gdp_per_capita"

all_bronze_gdp<-bronze_1960_gdp%>%full_join(bronze_1964_gdp)%>%
  full_join(bronze_1968_gdp)%>%
  full_join(bronze_1972_gdp)%>%
  full_join(bronze_1976_gdp)%>%
  full_join(bronze_1980_gdp)%>%
  full_join(bronze_1984_gdp)%>%
  full_join(bronze_1988_gdp)%>%
  full_join(bronze_1992_gdp)%>%
  full_join(bronze_1994_gdp)%>%
  full_join(bronze_1998_gdp)%>%
  full_join(bronze_2002_gdp)%>%
  full_join(bronze_2006_gdp)%>%
  full_join(bronze_2010_gdp)%>%
  full_join(bronze_2014_gdp)%>%
  full_join(bronze_2018_gdp)

sum(is.na(all_bronze_gdp$gdp_per_capita))

athletes_w_1960<-athletes_w%>%filter(Year>=1960)

speed_skating_1960<-athletes_w_1960%>%filter(Sport=="Speed Skating" & Year == 1960)%>%filter(Event=="Speed Skating Men's 500 metres")

total<-details%>%filter(Year==1960)

#Gdp data to Athlete data

#1960
ath_1960<-athletes_w_1960%>%filter(Year==1960)%>%left_join(gdp_1960,by=c("NOC"="Country.Code"))
names(ath_1960)[16]<-"gdp_per_capita"

#1964
ath_1964<-athletes_w_1960%>%filter(Year==1964)%>%left_join(gdp_1964,by=c("NOC"="Country.Code"))
names(ath_1964)[16]<-"gdp_per_capita"

#1968
ath_1968<-athletes_w_1960%>%filter(Year==1968)%>%left_join(gdp_1968,by=c("NOC"="Country.Code"))
names(ath_1968)[16]<-"gdp_per_capita"

#1972
ath_1972<-athletes_w_1960%>%filter(Year==1972)%>%left_join(gdp_1972,by=c("NOC"="Country.Code"))
names(ath_1972)[16]<-"gdp_per_capita"

#1976
ath_1976<-athletes_w_1960%>%filter(Year==1976)%>%left_join(gdp_1976,by=c("NOC"="Country.Code"))
names(ath_1976)[16]<-"gdp_per_capita"

#1980
ath_1980<-athletes_w_1960%>%filter(Year==1980)%>%left_join(gdp_1980,by=c("NOC"="Country.Code"))
names(ath_1980)[16]<-"gdp_per_capita"

#1984
ath_1984<-athletes_w_1960%>%filter(Year==1984)%>%left_join(gdp_1984,by=c("NOC"="Country.Code"))
names(ath_1984)[16]<-"gdp_per_capita"

#1988
ath_1988<-athletes_w_1960%>%filter(Year==1988)%>%left_join(gdp_1988,by=c("NOC"="Country.Code"))
names(ath_1988)[16]<-"gdp_per_capita"

#1992
ath_1992<-athletes_w_1960%>%filter(Year==1992)%>%left_join(gdp_1992,by=c("NOC"="Country.Code"))
names(ath_1992)[16]<-"gdp_per_capita"

#1994
ath_1994<-athletes_w_1960%>%filter(Year==1994)%>%left_join(gdp_1994,by=c("NOC"="Country.Code"))
names(ath_1994)[16]<-"gdp_per_capita"

#1998
ath_1998<-athletes_w_1960%>%filter(Year==1998)%>%left_join(gdp_1998,by=c("NOC"="Country.Code"))
names(ath_1998)[16]<-"gdp_per_capita"

#2002
ath_2002<-athletes_w_1960%>%filter(Year==2002)%>%left_join(gdp_2002,by=c("NOC"="Country.Code"))
names(ath_2002)[16]<-"gdp_per_capita"

#2006
ath_2006<-athletes_w_1960%>%filter(Year==2006)%>%left_join(gdp_2006,by=c("NOC"="Country.Code"))
names(ath_2006)[16]<-"gdp_per_capita"

#2010
ath_2010<-athletes_w_1960%>%filter(Year==2010)%>%left_join(gdp_2010,by=c("NOC"="Country.Code"))
names(ath_2010)[16]<-"gdp_per_capita"

#2014
ath_2014<-athletes_w_1960%>%filter(Year==2014)%>%left_join(gdp_2014,by=c("NOC"="Country.Code"))
names(ath_2014)[16]<-"gdp_per_capita"

All_gdp_athletes<-ath_1960%>%full_join(ath_1964)%>%
  full_join(ath_1968)%>%
  full_join(ath_1972)%>%
  full_join(ath_1976)%>%
  full_join(ath_1980)%>%
  full_join(ath_1984)%>%
  full_join(ath_1988)%>%
  full_join(ath_1992)%>%
  full_join(ath_1994)%>%
  full_join(ath_1998)%>%
  full_join(ath_2002)%>%
  full_join(ath_2006)%>%
  full_join(ath_2010)%>%
  full_join(ath_2014)

sum(is.na(All_gdp_athletes$gdp_per_capita))
