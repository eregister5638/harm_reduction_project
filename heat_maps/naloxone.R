library(dplyr)
library(sf)
library(rnaturalearth)
library(tidyverse)
library(usmap)
library(ggplot2)
library(tidycensus)
library(fuzzyjoin)
library(stringdist)
library(urbnmapr)


# Load the data
data <- read.csv('/Users/ethan/Documents/PHP1810/harm_reduction_project/data/Municipal_Count_of_Naloxone_Kits_Distributed_by_Year_(All_Sources).csv')

# Define the list of county names
providence_county <- c("Central_Falls", "Cranston", "East_Providence", "Foster", "Johnston", "Lincoln", "North_Providence", "Pawtucket", "Providence", "Scituate", "Smithfield", "Woonsocket", "Burrillville", "Glocester", "North_Smithfield", "Cumberland")
washington_county <- c("Charlestown", "Exeter", "Hopkinton", "Narragansett", "New_Shoreham", "North_Kingstown", "Richmond", "South_Kingstown", "Westerly")
bristol_county <- c("Barrington", "Bristol", "Warren")
kent_county <- c("Coventry", "East_Greenwich", "West_Greenwich", "Warwick", "Cranston", "West_Warwick")
newport_county <- c("Jamestown", "Little_Compton", "Middletown", "Newport", "Portsmouth", "Tiverton")

# Filter the data for 2022 and select only the columns in providence_county
providence_overdose <- data %>% 
  filter(Year == 2022) %>% 
  select(one_of(providence_county))

# Filter the data for 2022 and select only the columns in washington_county
washington_overdose <- data %>% 
  filter(Year == 2022) %>% 
  select(one_of(washington_county))

# Filter the data for 2022 and select only the columns in bristol_county
bristol_overdose <- data %>% 
  filter(Year == 2022) %>% 
  select(one_of(bristol_county))

# Filter the data for 2022 and select only the columns in kent_county
kent_overdose <- data %>% 
  filter(Year == 2022) %>% 
  select(one_of(kent_county))

# Filter the data for 2022 and select only the columns in newport_county
newport_overdose <- data %>% 
  filter(Year == 2022) %>% 
  select(one_of(newport_county))

#data frame
prov_od <- data.frame(t(providence_overdose))
prov_od <- cbind(rownames(prov_od), prov_od)
colnames(prov_od) <- c("Municipality", "Overdoses")
prov_od <- as_tibble(prov_od)
sum_prov <- sum(as.numeric(prov_od$Overdoses), na.rm = TRUE)



#data frame
brist_od <- data.frame(t(bristol_overdose))
brist_od <- cbind(rownames(brist_od), brist_od)
colnames(brist_od) <- c("Municipality", "Overdoses")
brist_od <- as_tibble(brist_od)
sum_brist <- sum(as.numeric(brist_od$Overdoses), na.rm = TRUE)


#data frame
new_od <- data.frame(t(newport_overdose))
new_od <- cbind(rownames(new_od), new_od)
colnames(new_od) <- c("Municipality", "Overdoses")
new_od <- as_tibble(new_od)
sum_new <- sum(as.numeric(new_od$Overdoses), na.rm = TRUE)


#data frame
kent_od <- data.frame(t(kent_overdose))
kent_od <- cbind(rownames(kent_od), kent_od)
colnames(kent_od) <- c("Municipality", "Overdoses")
kent_od <- as_tibble(kent_od)
sum_kent <- sum(as.numeric(kent_od$Overdoses), na.rm = TRUE)



#data frame
wash_od <- data.frame(t(washington_overdose))
wash_od <- cbind(rownames(wash_od), wash_od)
colnames(wash_od) <- c("Municipality", "Overdoses")
wash_od <- as_tibble(wash_od)
sum_wash <- sum(as.numeric(wash_od$Overdoses), na.rm = TRUE)


# Get FIPS codes for Rhode Island counties
#fips <- get_acs(geography = "county", variables = "B01001_001", state = "RI", geometry = TRUE)

# Create data frame of municipality overdose data
county_name = c("Providence County, Rhode Island", "Newport County, Rhode Island", "Washington County, Rhode Island", "Kent County, Rhode Island", "Bristol County, Rhode Island")
GEOID = c(44007, 44005, 44009, 44003, 44001)
od_sums = c(sum_prov, sum_new, sum_wash, sum_kent, sum_brist)
od_per_hund = c((sum_prov*100000)/656672,(sum_new*100000)/85525,(sum_wash*100000)/129735,(sum_kent*100000)/169345,(sum_brist*100000)/50672)#Overdoses rates per 100000
county_od = data.frame(county_name, Overdoses = od_per_hund, GEOID = as.character(GEOID))

# Convert Overdoses to numeric variable
county_od$Overdoses <- as.numeric(county_od$Overdoses)


# Get FIPS codes for Rhode Island counties
fips <- get_acs(geography = "county", variables = "B01001_001", state = "RI", geometry = TRUE)
fips <- fips %>%
  select(GEOID, NAME) %>%
  mutate(FIPS = as.numeric(str_sub(GEOID, start = 10))) %>%
  mutate(GEOID = as.character(GEOID)) # convert GEOID to character

# Fetch geometry for each GEOID
ri_counties <- st_as_sf(fips, wkt = "geometry")
ri_counties <- ri_counties %>%
  filter(GEOID %in% as.character(GEOID))


# Join the fips and county_od data frames
county_od <- left_join(county_od, fips %>% select(GEOID, FIPS, geometry), by = "GEOID")

# get abbreviation for Rhode Island
state_abbr <- state.abb[match("Rhode Island", state.name)]

# add state column to county_od
county_od$state <- state_abbr


counties_sf <- urbnmapr::get_urbn_map("counties", sf = TRUE)

counties_sf <- counties_sf %>%  
  mutate(county_fips = as.numeric(county_fips))



ggplot() +
  geom_sf(county_od,
          mapping = aes(fill = Overdoses, geometry = geometry),
          color = "white", size = 0.05) +
  coord_sf(datum = NA) +
  labs(fill = "Naloxone Kits",
       title = paste0("Rhode Island County Count of Naloxone Kits (2022)"))+
  theme_void()

