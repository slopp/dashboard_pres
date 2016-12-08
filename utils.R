mapDescIcon <- function(desc){
  desc
  desc = sub(pattern = "Light", replacement = "", desc, fixed = TRUE)
  desc = sub(pattern = "Heavy", replacement = "", desc, fixed = TRUE)
  curIcon = "ion-android-sunny"
  if (grepl(pattern = "*Snow*|*Ice*", x = desc))
    curIcon = "ion-ios-snow"
  if (grepl(pattern = "*Rain*|*Drizzle*|*Hail*", x = desc))
    curIcon = "ion-ios-rainy-outline"
  if (grepl(pattern = "*Cloud*|*Overcast*|*Fog*", x= desc))
    curIcon = "ion-ios-cloud-outline"
  curIcon
}

mapTempIcon <- function(tmp){
  tempIcon = "fa-thermometer-half"
  if(tmp < 15)
    tempIcon = "fa-thermometer-empty"
  if(tmp > 30 )
    tempIcon = "fa-thermometer-full"
  tempIcon
}
getKnownAreas <- function(){
  data.frame(
    resorts = c("Abasin", 
                "Breckenridge",
                "Copper",
                "Eldora",
                "Keystone",
                "Loveland",
                "Purgatory"),
    ids = c(303001,
            303007,
            303009,
            303011,
            303014,
            303015,
            303017),
    weather_underground = c("39.6423,-105.8717",
                            "39.4817,-106.0384",
                            "39.5014,-106.1516",
                            "39.9372,-105.5827",
                            "39.5589,-105.9106",
                            "39.6800,-105.8979",
                            "37.6269,-107.8360"
                            ),
    stringsAsFactors = FALSE
  )
}

returnAreaID <- function(area_name) {
 areas <- getKnownAreas()
 areas[which(areas$resorts==area_name),2]
}

returnAreaLatLon <- function(area_name){
  areas <- getKnownAreas()
  areas[which(areas$resorts==area_name),3]
  
}

getAreaData <- function(area_name){
  id <- returnAreaID(area_name)
  base_url <- "http://feeds.snocountry.net/conditions.php?apiKey=SnoCountry.example"
  results <- fromJSON(paste0(base_url, "&ids=", id))
  if(results$totalItems != 1)
    stop("Error accessing Snocountry API")
  results$items
}

getCurrentWeather <- function(area_name){
  loc <- returnAreaLatLon(area_name)
  base_url <- "http://api.wunderground.com/api/cbd1b7a6e2e439ea/conditions/q/"
  results <- fromJSON(paste0(base_url, loc, ".json"))
  results$current_observation 
}

