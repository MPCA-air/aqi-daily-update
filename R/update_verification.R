#! /usr/bin/env Rscript

days_past <- 1

today <- Sys.Date()

# AQI conversion functions
source(paste0(aqiwatch_path, "R/aqi_convert.R"))


# Sites ----
sites <- aqi_sites

# Drop outstate sites
sites <- dplyr::filter(sites, !fcst_region %in% c("CA", "ND", "SD", "WI", "IA"))


# Yesterday's official submitted forecast ----
print("Loading official forecasts...")

# Load forecasts from AirNow for all days past yesterday
#aqi_forc <- readLines(paste0("ftp://", creds$user, ":", creds$pwd, "@ftp.airnowapi.org/ReportingArea/reportingarea.dat"))

aqi_forc_all <- tibble()

for (i in 0:4) {

  print(i)
  
  aqi_forc <- readLines(paste0("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/",
                                format(Sys.Date() - i, "%Y/%Y%m%d"),
                               "/reportingarea.dat"))

  aqi_forc <- gsub("[|]", ",", aqi_forc)

  aqi_forc <- read_csv(paste0(aqi_forc, collapse = "\n"), col_names = F)

  # Name columns
  names(aqi_forc) <- c("date_issued",
                       "Date",
                       "time_forecast",
                       "tzone",
                       "DayIndex",
                       "is_forecast",
                       "primary_aqi",
                       "Group",
                       "state",
                       "lat",
                       "long",
                       "param",
                       "aqi",
                       "aqi_cat",
                       "alert_day",
                       "discussion",
                       "agency")


  # Replace missing forecast values with AQI color
  aqi_forc$aqi <- as.character(aqi_forc$aqi)
  
  aqi_forc[is.na(aqi_forc$aqi), ]$aqi <- label2cat(aqi_forc[is.na(aqi_forc$aqi), ]$aqi_cat)


  # Select columns
  aqi_forc <- select(aqi_forc, -c(time_forecast, tzone, primary_aqi, lat, long, alert_day, discussion, agency, aqi_cat))


  # Filter sites, drop Voyageurs
  unique(sites$fcst_region) %>% sort

  aqi_forc <- dplyr::filter(aqi_forc,
                     state %in% c("MN"),
                     Group %in% c(sites$fcst_region, "Leech Lake Nation: Cass Lake"),
                     !grepl("Voyage", Group))

  # Date format
  aqi_forc$Date <-  as.Date(aqi_forc$Date, "%m/%d/%y")


  # Select Day 1 or next most recent forecast for yesterday
  aqi_forc <- dplyr::filter(aqi_forc,
                     is_forecast == "F",
                     DayIndex >= 0,
                     Date >= Sys.Date() - 1)

  # Check for results
  if (nrow(aqi_forc) > 0) {

    # Wide format
    aqi_forc <- spread(aqi_forc, param, aqi)

    # Add Ozone column if missing
    if (!"OZONE" %in% names(aqi_forc)) aqi_forc$OZONE <- NA

    # Add PM2.5 column if missing
    if (!"PM2.5" %in% names(aqi_forc)) aqi_forc$`PM2.5` <- NA

    # Join all
    aqi_forc_all <-  rbind(aqi_forc, aqi_forc_all)

  }
}


# Remove duplicates
aqi_forc <- unique(aqi_forc_all)

# Update Leech Lake name
aqi_forc <- aqi_forc %>% 
            mutate(Group = gsub("Leech Lake Nation: Cass Lake",  "Leech Lake", Group))

##-------------------------------------------------------------#
## Load internal forecasts for missing sites ----
## Update when forecasts moved to Github
if (FALSE) {
aqi_forc_int <- tryCatch(read_csv("AQI_Solutions/Values/All_Values_gen2.csv"),
                         error = function(e) NA)


if (is.na(aqi_forc_int)) { 
  aqi_forc_int <- read_csv("forecast/Forecast_Values.csv")
}


aqi_forc_int <- aqi_forc_int %>% 
                mutate(Group = gsub("Leech Lake Nation: Cass Lake",  "Leech Lake", Group))


# Date format
if (grepl("[/]", aqi_forc_int$Date[1])) {
  aqi_forc_int$Date  <- as.Date(aqi_forc_int$Date, "%m/%d/%Y")
} else{
  aqi_forc_int$Date  <- as.Date(aqi_forc_int$Date)
}


# Clean group names
aqi_forc_int$Group <- gsub("Duluth_WDSE", "Duluth", aqi_forc_int$Group)
aqi_forc_int$Group <- gsub("_", " ", aqi_forc_int$Group)
aqi_forc_int$Group <- gsub("St ", "St. ", aqi_forc_int$Group)
aqi_forc_int$Group <- gsub("Metro", "Twin Cities Metro", aqi_forc_int$Group)
aqi_forc_int$Group <- gsub("MSP", "Minneapolis-St. Paul", aqi_forc_int$Group)

sort(unique(aqi_forc_int$Group))


# Join submitted forecasts
aqi_forc_original <- left_join(aqi_forc_int, 
                               select(aqi_forc, Group, OZONE, `PM2.5`, DayIndex, Date))

aqi_forc_updates <- aqi_forc %>%
                    select(Group, OZONE, `PM2.5`, DayIndex, Date) %>%
                    dplyr::filter(!paste(Date, DayIndex, Group) %in%
                            paste(aqi_forc_original$Date, 
                                  aqi_forc_original$DayIndex, 
                                  aqi_forc_original$Group))


aqi_forc_updates <- left_join(aqi_forc_updates,
                              select(aqi_forc_int, Group, Latitude, Longitude) %>% 
                    unique())


aqi_forc <- bind_rows(aqi_forc_original, aqi_forc_updates)

# Use submitted value if available
aqi_forc  <- aqi_forc %>%
             rowwise() %>%
             mutate(AQI_O3       = ifelse(is.na(OZONE), as.character(AQI_O3_Ens), OZONE),
                    `Max Avg8Hr` = aqi2conc(AQI_O3, "Ozone"),
                    AQI_PM       = ifelse(is.na(`PM2.5`), as.character(AQI_PM_Ens), `PM2.5`),
                    Pm25Avg      = aqi2conc(AQI_PM, "PM25")) %>%
             select(-c(OZONE, `PM2.5`))
}                    
#-------------------------------------------------------------#                     
                         

# Yesterday's model output forecast ----
verify <- aqi_forc

names(verify) <- gsub("Max Avg8Hr ", "mod_max_avg8hr_", names(verify))
names(verify) <- gsub("AQI_O3_", "mod_aqi_o3_", names(verify))
names(verify) <- gsub("Pm25Avg ", "mod_pm25avg_", names(verify))
names(verify) <- gsub("AQI_PM_", "mod_aqi_pm_", names(verify))

verify <- verify %>%
          rename(forecast_date = Date,
                 forecast_day  = DayIndex) 

names(verify) <- gsub("AQI_O3", "fcst_ozone_aqi", names(verify))
names(verify) <- gsub("Max Avg8Hr", "fcst_ozone_ppb", names(verify))
names(verify) <- gsub("AQI_PM", "fcst_pm25_aqi", names(verify))
names(verify) <- gsub("Pm25Avg", "fcst_pm25_ugm3", names(verify))

names(verify) <- tolower(names(verify))


# Rearrange columns with dates first
verify <-  verify %>% select(forecast_date, everything())


# Yesterday's model inputs ----
## Update when forecasts moved to Github
if (FALSE) {
  
print("Model inputs...")

all_inputs <- read_csv("Met_View.csv")

names(all_inputs)[1:3] <- c("forecast_day", "short_name", "site_catid")


#-- Update date format for consistency
#-- Check for "/" slash in date
if (grepl("[/]", all_inputs$Date[1])) {
   all_inputs$Date <- as.Date(all_inputs$Date, "%m/%d/%Y")
} else {
   all_inputs$Date <- as.Date(all_inputs$Date, "%Y-%m-%d")
}

#-- Set date column name
names(all_inputs)[grep("Date", names(all_inputs))] <- "forecast_date"

class(all_inputs$forecast_day)
class(verify$forecast_day)

#-- Join all
verify$site_catid

all_inputs$site_catid

verify <- left_join(verify, select(all_inputs, -short_name))


# Yesterday's HYSPLIT origins ----
print("Loading HYSPLIT...")

load_hys  <- FALSE

days_past_hys <- days_past

while (!load_hys) {

  hys <- tryCatch(read_csv(paste0(Sys.Date() - days_past_hys, "_AQI_raw_HYSPLIT.csv")), error = function(e) NA)

  if (is.na(hys)) {

    days_past_hys <- days_past_hys + 1

  } else {

    load_hys <- TRUE
  }

  if (days_past_hys > 7) load_hys <- TRUE
}


if (!is.na(hys)) {

# Drop run_date and row_id
hys <- hys %>% select(-row_id, -run_date)  
  
# Convert day index column
hys$forecast_day <- as.numeric(gsub("day", "", hys$forecast_day))

# Collapse origin coordinates
hys$background_origin <- paste(hys$lat, hys$lon, sep = ", ")

#-- Drop new MET columns
hys <- select(hys, -c(traj_rain_sum,
                      traj_rain_max,
                      traj_rain_hrs_02,
                      traj_rain_hrs_05,
                      traj_rh,
                      traj_sunflux))


# Split 10m and 500m trajectory into 2 columns
hys_10m  <- dplyr::filter(hys, receptor_height == 10)
hys_500m <- dplyr::filter(hys, receptor_height == 500)

# Wide format by forecast day
#hys_10m            <- spread(hys_10m[ , -c(2:13)], forecast_day, lat_long)
hys_10m             <- hys_10m[ , -c(2:11,13)]

names(hys_10m)[4:ncol(hys_10m)]  <- paste0(names(hys_10m)[4:ncol(hys_10m)], "_10m")

#hys_500m           <- spread(hys_500m[ , -c(2:13)], forecast_day, lat_long)
hys_500m            <- hys_500m[ , -c(2:11,13,14)]

names(hys_500m)[3:ncol(hys_500m)] <- paste0(names(hys_500m)[3:ncol(hys_500m)], "_500m")


#-- Join 10m and 500m tables
hys_origin <- full_join(hys_10m, hys_500m)


#-- Update date format
hys_origin$date <- as.Date(hys_origin$date)


#-- Join HYSPLIT origin to verification table
names(hys_origin)[1:2] <- c("site_catid", "forecast_date")

verify <- left_join(verify, hys_origin)

}
}

# Clean table -----
names(verify) <- gsub(" ", "_", tolower(names(verify)))

verify <- dplyr::filter(verify, !is.na(forecast_day), !is.na(group)) # !is.na(site_catid))


# Load and join to previous days ----
print("Loading previous day verifications...")

all_verify <- try(readRDS(paste0("verification/", Sys.Date()-1, "_verification_table.Rdata")),
                  silent = TRUE)

if ("try-error" %in% class(all_verify)) {

  all_verify <- try(read_csv("verification/verification_table.csv"),
                    silent = TRUE)
}

if ("try-error" %in% class(all_verify)) { 
  all_verify <- verify 
  }


all_verify <- all_verify %>%
              group_by(forecast_date, forecast_day, site_catid) %>%
              slice_head(n = 1) %>%
              ungroup()


#-- Set background types for joining

# Make forecast character string for color only forecasts
all_verify$fcst_ozone_aqi <- as.character(all_verify$fcst_ozone_aqi)
all_verify$fcst_pm25_aqi  <- as.character(all_verify$fcst_pm25_aqi)

# All logical columns to numeric
all_verify <- try(mutate_if(all_verify, is.logical, as.numeric))
verify     <- try(mutate_if(verify, is.logical, as.numeric))

verify$fcst_ozone_aqi <- as.character(verify$fcst_ozone_aqi)
verify$fcst_pm25_aqi  <- as.character(verify$fcst_pm25_aqi)

# Join all
all_verify <- bind_rows(verify, all_verify)

#-- Remove duplicates and NA forecast days
all_verify <- dplyr::filter(all_verify, 
                            !is.na(forecast_day), 
                            !is.na(forecast_date))


##-- Take Max value to use numeric forecast instead of color when both available
get_max <- function(values) {
  return(max(as.numeric(unlist(values)), na.rm = T))
}

max_is_positive <- function(values) {
  return(get_max(unlist(values)) >= 0)
}

first_value <- function(values) {
  sort(values)[1]
}


# Get only current and future dates
collapse <- all_verify %>% 
            filter(forecast_date %in% seq(today-days_past, today+5, 1))


# Collapse down to single forecast row
collapse <- collapse %>%
            group_by(forecast_date, forecast_day, group) %>% #site_catid) %>%
            mutate_all(first_value) %>%
            slice_head(n = 1)


# Add back to full table
all_verify <- filter(all_verify, 
                     !forecast_date %in% seq(today-days_past, today+5, 1))

all_verify <- bind_rows(all_verify, collapse)


# Yesterday's actuals -----
print("Loading actuals...")

actuals <- air_all #readRDS(paste0("data/obs/", Sys.Date() - 1, "_AQI_observed", ".Rdata"))


#-- Header names
actuals <- actuals %>%
           rename(forecast_date = date,
                  #count_ozone_obs =,
                  #count_pm25_obs = ,
                  obs_ozone_ppb = obs_max_ozone_8hr_ppb,
                  obs_pm25_ugm3 = obs_pm25_24hr_ugm3)


#-- Add AQI category
actuals <- ungroup(actuals) %>%
           rowwise() %>%
           mutate(obs_ozone_aqi  = conc2aqi(obs_ozone_ppb, "OZONE"),
                  obs_pm25_aqi   = conc2aqi(obs_pm25_ugm3, "PM25")) %>%
           ungroup()

# Update Voyageurs site ID
actuals[actuals$aqsid == "271370034", "site_catid"] <- "27-137-9000"


# Collapse Duluth sites to one row
if ("27-137-7554" %in% actuals$site_catid & "27-137-7550" %in% actuals$site_catid) {
  
  duluth_pm <- as.numeric(actuals[actuals$site_catid == "27-137-7554", 
                                  c(#"count_pm25_obs",
                                    "obs_pm25_ugm3", "obs_pm25_aqi")]) %>% 
               unlist()
  
  actuals[actuals$site_catid == "27-137-7550", 
          c(#"count_pm25_obs", 
            "obs_pm25_ugm3", "obs_pm25_aqi")] <- as.list(duluth_pm)

}


# Drop non-forcasted sites
actuals <- dplyr::filter(actuals, !air_monitor %in% c("Voyageurs", "Laura McArthur Sch"))

actuals <- dplyr::filter(actuals, !is.na(site_catid))

# Add group
actuals <- actuals %>%
           left_join(sites %>% 
                     rename(group = fcst_region) %>%
                     select(site_catid, group))

# Select yesterday forecasts and drop observation columns
yester_fcst <- dplyr::filter(all_verify, forecast_date == Sys.Date() - 1) %>%
               select(-c(obs_ozone_ppb, obs_pm25_ugm3, 
                         obs_ozone_aqi, obs_pm25_aqi,
                         site_catid, short_name)) #count_ozone_obs, count_pm25_obs

# Most recent forecast (except for current day) 
yester_fcst <- yester_fcst %>% 
               filter(forecast_day > 0) %>% 
               group_by(group) %>%
               arrange(forecast_day) %>%
               slice_head(n = 1)

# Attach actuals to yesterday forecasts
yester_fcst <- left_join(actuals %>% select(-air_monitor, -aqsid), 
                         yester_fcst)


# Yesterday's CMAQ forecast   -----
## Add next week
if (FALSE) {
  
print("CMAQ...")

cmaq_all <- tibble()

# Load CMAQ forecast from past 2 days
for (i in 1:2) {

  cmaq_forc <- try(read_csv(paste0(Sys.Date() - i, "_CMAQ_forecast.csv")), silent = TRUE)

  if (!"try-error" %in% class(cmaq_forc)) {

  cmaq_forc <- select(cmaq_forc, -cmaq_day0_max_ozone_1hr, -cmaq_day1_max_ozone_1hr)

  names(cmaq_forc) [1:2] <- c("day0", "day1")

  # Flip to long format
  cmaq_forc <- tidyr::gather(data  = cmaq_forc, 
                             key   = forecast_day, 
                             value = cmaq_ozone_ppb, 
                             na.rm = FALSE, day0, day1)


  # Add category
  cmaq_forc <- ungroup(cmaq_forc) %>%
               rowwise() %>%
               mutate(cmaq_ozone_aqi = conc2aqi(cmaq_ozone_ppb, "OZONE")) %>%
               ungroup()

  # Add days & date
  cmaq_forc$forecast_day  <- as.numeric(gsub("day", "", cmaq_forc$forecast_day))

  cmaq_forc$forecast_date <- Sys.Date() - i + cmaq_forc$forecast_day

  # Combine
  cmaq_all <- bind_rows(cmaq_forc, cmaq_all)
  }
}


# Attach CMAQ to yesterday forecasts
if (nrow(cmaq_all) > 0) {
  
  yester_fcst <- left_join(yester_fcst %>% select(-cmaq_ozone_ppb, -cmaq_ozone_aqi),
                           cmaq_all)
}

}

# Join yesterday actuals & CMAQ to full table -----

# Drop yesterday from full table
all_verify <- dplyr::filter(all_verify, forecast_date != (Sys.Date() - 1))

# Join new yesterday results to full table
all_verify <- bind_rows(yester_fcst, all_verify)

# Limit table to past 365 days
all_verify <- subset(all_verify, forecast_date > (today - 365))


# Save full verification table ----
print("Saving file...")

all_verify <- select(all_verify, -short_name) %>%
              left_join(select(sites, short_name, site_catid)) %>%
              select(forecast_date, forecast_day, site_catid, short_name, group, everything())

# Keep only current year
all_verify <- all_verify %>%
              subset(as.numeric(format(forecast_date, "%Y")) > (as.numeric(format(Sys.Date(), "%Y")) - 1))

saveRDS(all_verify, paste0("verification/", Sys.Date(), "_verification_table.Rdata"))

write.csv(all_verify, "verification/verification_table.csv", row.names = F)


#-- Create event table ----
events <- dplyr::filter(actuals, forecast_date == Sys.Date() - 1) %>%
          left_join(select(sites, short_name, site_catid)) %>%
          select(forecast_date, short_name, site_catid, obs_ozone_aqi, obs_pm25_aqi)


##
