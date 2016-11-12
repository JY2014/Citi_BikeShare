
setwd("~/Dropbox/stats139/project/")
df <- read.csv("listings.csv", header=TRUE)
head(df)
unique(df$state)


df_ub <-read.csv("Uber-Jan-Feb-FOIL.csv", header=TRUE)
head(df_ub)
plot(df_ub$trips, df_ub$active_vehicles)


df_bi_07 <-read.csv("201607-citibike-tripdata.csv", header=TRUE)
df_bi_08 <-read.csv("201608-citibike-tripdata.csv", header=TRUE)
df_bi_09 <-read.csv("201609-citibike-tripdata.csv", header=TRUE)

head(df_bi_07)
unique(df_bi_07$start.station.id) ##480+ stations

df_bi = rbind(df_bi_07, df_bi_08, df_bi_09)
df_bi = df_bi[df_bi$end.station.latitude!= 0,]

## coord represents the latitude and longitude of each station
unique(cbind(df_bi_09$start.station.latitude, df_bi_09$start.station.longitude))
coord<-cbind(df_bi$start.station.latitude, df_bi$start.station.longitude, df_bi$start.station.id, df_bi$start.station.name)
station_coord <- unique(coord) ##626 stations in 7, 8,9 months, 616 stations in September only

station_coord_end<-unique(cbind(df_bi$end.station.latitude, df_bi$end.station.longitude, df_bi$end.station.id, as.character(df_bi$end.station.name)))

for(i in (1:length(station_coord_end[, 1]))){
  if(!(station_coord_end[i, 3] %in% station_coord[, 3])){
    print(station_coord_end[i, c(1, 2)])
  }
}


#write.table(station_coord, "station_info.txt", sep="\t")

##http://www.findlatitudeandlongitude.com/batch-reverse-geocode/#.WCTnTMwrLCL
## use this website to convert coordinates to address and zipcode

address <- read.csv("station_coord_address_zip.csv", header=TRUE)

## abstract zipcode from address
get_zipcode <- function(x){
  substr(x, nchar(x)-9, nchar(x)-5)
}
address$returned.address <- as.character(address$returned.address)
address$zipcode <- get_zipcode(address$returned.address)

unique(address$zipcode) ##about 60 unique zipcode

## turn zip_lookup.csv into formalized dictionary
zip_lookup <- read.csv("zip_lookup.csv", header=TRUE)
zip_lookup$ZIP.Codes <-as.character(zip_lookup$ZIP.Codes)
zip_lookup$Neighborhood <-as.character(zip_lookup$Neighborhood)
zip_lookup$Borough <-as.character(zip_lookup$Borough)
zip_lookup$list_zip <- strsplit(zip_lookup$ZIP.Codes, ', ', fixed = TRUE)

## map zipcode to neighborhood and borough
nstations = length(address$zipcode)
address$neighborhood = rep('', nstations)
address$borough = rep('', nstations)
n_neighbor = length(zip_lookup$Neighborhood)

get_neighborhood <- function(zip){
  for (i in (1:n_neighbor)){
    if(zip %in% zip_lookup$list_zip[[i]]){
      res = c(zip_lookup$Neighborhood[i], zip_lookup$Borough[i])
      return (res)
    }
  }
  return ('NA')
}

for (i in (1:nstations)){
  address$neighborhood[i] <- get_neighborhood(address$zipcode[i])[1]
  address$borough[i]<- get_neighborhood(address$zipcode[i])[2]
}

unique(address$neighborhood) ## 18 unique neighoborhood
unique(address$borough) ## 4 unique borough: "Manhattan"  "Brooklyn"   "Queens"     "New Jersey"

## for each observation, map station with neighborhood and borough
round_df <- function(df, digits=5) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)

  (df)
}

df_bi <- round_df(df_bi)
address<- round_df(address)


nobs = length(df_bi$tripduration)
df_bi$start_neighborhood = rep('', nobs)
df_bi$start_borough= rep('', nobs)
df_bi$end_neighborhood = rep('', nobs)
df_bi$end_borough = rep('', nobs)

# for(i in (1:nobs)){
#   df_bi$start_borough[i] = address[address$longitude == df_bi$start.station.longitude[i] & address$latitude == df_bi$start.station.latitude[i], 7]
#   df_bi$start_neighborhood[i] = address[address$longitude == df_bi$start.station.longitude[i] & address$latitude == df_bi$start.station.latitude[i], 6]
#   df_bi$end_borough[i] = address[address$longitude == df_bi$end.station.longitude[i] & address$latitude == df_bi$end.station.latitude[i], 7]
#   df_bi$end_neighborhood[i] = address[address$longitude == df_bi$end.station.longitude[i] & address$latitude == df_bi$end.station.latitude[i], 6]
# }


# write.table(df_bi, "data_partial_finished.txt", sep="\t")
# write.table(address, "address_map.txt", sep="\t")
### implemented using python


df_bi$end_borough[i] = address[address$longitude == round(df_bi$end.station.longitude[i], 5) & address$latitude == round(df_bi$end.station.latitude[i], 5), 7]
plot(df_bi$start.station.longitude, df_bi$start.station.latitude)
plot(df_bi_07$start.station.longitude, df_bi_07$start.station.latitude)
