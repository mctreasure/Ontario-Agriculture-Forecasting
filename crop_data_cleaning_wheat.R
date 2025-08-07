library(openxlsx)
library(cancensus)
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)

# 2024
wheat_2024 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 1)
wheat_2024[2, 1] <- wheat_2024[1, 1]
wheat_2024 <- wheat_2024[-1, ]
colnames(wheat_2024) <- as.character(wheat_2024[1, ])
wheat_2024 <- wheat_2024[-1, ]
ontario_row <- which(apply(wheat_2024, 1, function(row) any(row == "Ontario")))
wheat_2024 <- wheat_2024[1:ontario_row, ]
wheat_2024 <- wheat_2024[!grepl("Ontario", wheat_2024[, 1]), ]
rownames(wheat_2024) <- NULL

colnames(wheat_2024)[1] <- "County"
wheat_2024 <- cbind(Year = 2024, wheat_2024)

# 2023
wheat_2023 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 2)
wheat_2023[2, 1] <- wheat_2023[1, 1]
wheat_2023 <- wheat_2023[-1, ]
colnames(wheat_2023) <- as.character(wheat_2023[1, ])
wheat_2023 <- wheat_2023[-1, ]
ontario_row <- which(apply(wheat_2023, 1, function(row) any(row == "Ontario")))
wheat_2023 <- wheat_2023[1:ontario_row, ]
wheat_2023 <- wheat_2023[!grepl("Ontario", wheat_2023[, 1]), ]
rownames(wheat_2023) <- NULL

colnames(wheat_2023)[1] <- "County"
wheat_2023 <- cbind(Year = 2023, wheat_2023)

# 2022
wheat_2022 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 3)
wheat_2022[2, 1] <- wheat_2022[1, 1]
wheat_2022 <- wheat_2022[-1, ]
colnames(wheat_2022) <- as.character(wheat_2022[1, ])
wheat_2022 <- wheat_2022[-1, ]
ontario_row <- which(apply(wheat_2022, 1, function(row) any(row == "Ontario")))
wheat_2022 <- wheat_2022[1:ontario_row, ]
wheat_2022 <- wheat_2022[!grepl("Ontario", wheat_2022[, 1]), ]
rownames(wheat_2022) <- NULL

colnames(wheat_2022)[1] <- "County"
wheat_2022 <- cbind(Year = 2022, wheat_2022)

# 2021
wheat_2021 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 4)
wheat_2021[2, 1] <- wheat_2021[1, 1]
wheat_2021 <- wheat_2021[-1, ]
colnames(wheat_2021) <- as.character(wheat_2021[1, ])
wheat_2021 <- wheat_2021[-1, ]
ontario_row <- which(apply(wheat_2021, 1, function(row) any(row == "Ontario")))
wheat_2021 <- wheat_2021[1:ontario_row, ]
wheat_2021 <- wheat_2021[!grepl("Ontario", wheat_2021[, 1]), ]
rownames(wheat_2021) <- NULL

colnames(wheat_2021)[1] <- "County"
wheat_2021 <- cbind(Year = 2021, wheat_2021)

# 2020
wheat_2020 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 5)
wheat_2020[2, 1] <- wheat_2020[1, 1]
wheat_2020 <- wheat_2020[-1, ]
colnames(wheat_2020) <- as.character(wheat_2020[1, ])
wheat_2020 <- wheat_2020[-1, ]
ontario_row <- which(apply(wheat_2020, 1, function(row) any(row == "Ontario")))
wheat_2020 <- wheat_2020[1:ontario_row, ]
wheat_2020 <- wheat_2020[!grepl("Ontario", wheat_2020[, 1]), ]
rownames(wheat_2020) <- NULL

colnames(wheat_2020)[1] <- "County"
wheat_2020 <- cbind(Year = 2020, wheat_2020)

# 2019
wheat_2019 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 6)
wheat_2019[2, 1] <- wheat_2019[1, 1]
wheat_2019 <- wheat_2019[-1, ]
colnames(wheat_2019) <- as.character(wheat_2019[1, ])
wheat_2019 <- wheat_2019[-1, ]
ontario_row <- which(apply(wheat_2019, 1, function(row) any(row == "Ontario")))
wheat_2019 <- wheat_2019[1:ontario_row, ]
wheat_2019 <- wheat_2019[!grepl("Ontario", wheat_2019[, 1]), ]
rownames(wheat_2019) <- NULL

colnames(wheat_2019)[1] <- "County"
wheat_2019 <- cbind(Year = 2019, wheat_2019)

# 2018
wheat_2018 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 7)
# wheat_2018[2, 1] <- wheat_2018[1, 1]
# wheat_2018 <- wheat_2018[-1, ]
colnames(wheat_2018) <- as.character(wheat_2018[1, ])
wheat_2018 <- wheat_2018[-1, ]
ontario_row <- which(apply(wheat_2018, 1, function(row) any(row == "Ontario")))
wheat_2018 <- wheat_2018[1:ontario_row, ]
wheat_2018 <- wheat_2018[!grepl("Ontario", wheat_2018[, 1]), ]
rownames(wheat_2018) <- NULL

colnames(wheat_2018)[1] <- "County"
wheat_2018 <- cbind(Year = 2018, wheat_2018)

# 2017
wheat_2017 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 8)
#wheat_2017[2, 1] <- wheat_2017[1, 1]
#wheat_2017 <- wheat_2017[-1, ]
colnames(wheat_2017) <- as.character(wheat_2017[1, ])
wheat_2017 <- wheat_2017[-1, ]
ontario_row <- which(apply(wheat_2017, 1, function(row) any(row == "Ontario")))
wheat_2017 <- wheat_2017[1:ontario_row, ]
wheat_2017 <- wheat_2017[!grepl("Ontario", wheat_2017[, 1]), ]
rownames(wheat_2017) <- NULL

colnames(wheat_2017)[1] <- "County"
wheat_2017 <- cbind(Year = 2017, wheat_2017)

# 2016
wheat_2016 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 9)
#wheat_2016[2, 1] <- wheat_2016[1, 1]
#wheat_2016 <- wheat_2016[-1, ]
colnames(wheat_2016) <- as.character(wheat_2016[1, ])
wheat_2016 <- wheat_2016[-1, ]
ontario_row <- which(apply(wheat_2016, 1, function(row) any(row == "Ontario")))
wheat_2016 <- wheat_2016[1:ontario_row, ]
wheat_2016 <- wheat_2016[!grepl("Ontario", wheat_2016[, 1]), ]
rownames(wheat_2016) <- NULL

colnames(wheat_2016)[1] <- "County"
wheat_2016 <- cbind(Year = 2016, wheat_2016)

# 2015
wheat_2015 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 10)
#wheat_2015[2, 1] <- wheat_2015[1, 1]
#wheat_2015 <- wheat_2015[-1, ]
colnames(wheat_2015) <- as.character(wheat_2015[1, ])
wheat_2015 <- wheat_2015[-1, ]
ontario_row <- which(apply(wheat_2015, 1, function(row) any(row == "Ontario")))
wheat_2015 <- wheat_2015[1:ontario_row, ]
wheat_2015 <- wheat_2015[!grepl("Ontario", wheat_2015[, 1]), ]
rownames(wheat_2015) <- NULL

colnames(wheat_2015)[1] <- "County"
wheat_2015 <- cbind(Year = 2015, wheat_2015)

# 2014
wheat_2014 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 11)
#wheat_2014[2, 1] <- wheat_2014[1, 1]
#wheat_2014 <- wheat_2014[-1, ]
colnames(wheat_2014) <- as.character(wheat_2014[1, ])
wheat_2014 <- wheat_2014[-1, ]
ontario_row <- which(apply(wheat_2014, 1, function(row) any(row == "Ontario")))
wheat_2014 <- wheat_2014[1:ontario_row, ]
wheat_2014 <- wheat_2014[!grepl("Ontario", wheat_2014[, 1]), ]
rownames(wheat_2014) <- NULL

colnames(wheat_2014)[1] <- "County"
wheat_2014 <- cbind(Year = 2014, wheat_2014)

# 2013
wheat_2013 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 12)
#wheat_2013[2, 1] <- wheat_2013[1, 1]
#wheat_2013 <- wheat_2013[-1, ]
colnames(wheat_2013) <- as.character(wheat_2013[1, ])
wheat_2013 <- wheat_2013[-1, ]
ontario_row <- which(apply(wheat_2013, 1, function(row) any(row == "Ontario")))
wheat_2013 <- wheat_2013[1:ontario_row, ]
wheat_2013 <- wheat_2013[!grepl("Ontario", wheat_2013[, 1]), ]
rownames(wheat_2013) <- NULL

colnames(wheat_2013)[1] <- "County"
wheat_2013 <- cbind(Year = 2013, wheat_2013)

# 2012
wheat_2012 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 13)
#wheat_2012[2, 1] <- wheat_2012[1, 1]
#wheat_2012 <- wheat_2012[-1, ]
colnames(wheat_2012) <- as.character(wheat_2012[1, ])
wheat_2012 <- wheat_2012[-1, ]
ontario_row <- which(apply(wheat_2012, 1, function(row) any(row == "Ontario")))
wheat_2012 <- wheat_2012[1:ontario_row, ]
wheat_2012 <- wheat_2012[!grepl("Ontario", wheat_2012[, 1]), ]
rownames(wheat_2012) <- NULL

colnames(wheat_2012)[1] <- "County"
wheat_2012 <- cbind(Year = 2012, wheat_2012)

# 2011
wheat_2011 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 14)
#wheat_2011[2, 1] <- wheat_2011[1, 1]
#wheat_2011 <- wheat_2011[-1, ]
colnames(wheat_2011) <- as.character(wheat_2011[1, ])
wheat_2011 <- wheat_2011[-1, ]
ontario_row <- which(apply(wheat_2011, 1, function(row) any(row == "Ontario")))
wheat_2011 <- wheat_2011[1:ontario_row, ]
wheat_2011 <- wheat_2011[!grepl("Ontario", wheat_2011[, 1]), ]
rownames(wheat_2011) <- NULL

colnames(wheat_2011)[1] <- "County"
wheat_2011 <- cbind(Year = 2011, wheat_2011)

# 2010
wheat_2010 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 15)
#wheat_2010[2, 1] <- wheat_2010[1, 1]
#wheat_2010 <- wheat_2010[-1, ]
colnames(wheat_2010) <- as.character(wheat_2010[1, ])
wheat_2010 <- wheat_2010[-1, ]
ontario_row <- which(apply(wheat_2010, 1, function(row) any(row == "Ontario")))
wheat_2010 <- wheat_2010[1:ontario_row, ]
wheat_2010 <- wheat_2010[!grepl("Ontario", wheat_2010[, 1]), ]
rownames(wheat_2010) <- NULL

colnames(wheat_2010)[1] <- "County"
wheat_2010 <- cbind(Year = 2010, wheat_2010)

names(wheat_2010)[names(wheat_2010) == "Acres remaining (June 1)"] <- "Acres seeded"

# 2009
wheat_2009 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 16)
#wheat_2009[2, 1] <- wheat_2009[1, 1]
#wheat_2009 <- wheat_2009[-1, ]
colnames(wheat_2009) <- as.character(wheat_2009[1, ])
wheat_2009 <- wheat_2009[-1, ]
ontario_row <- which(apply(wheat_2009, 1, function(row) any(row == "Ontario")))
wheat_2009 <- wheat_2009[1:ontario_row, ]
wheat_2009 <- wheat_2009[!grepl("Ontario", wheat_2009[, 1]), ]
rownames(wheat_2009) <- NULL

colnames(wheat_2009)[1] <- "County"
wheat_2009 <- cbind(Year = 2009, wheat_2009)

names(wheat_2009)[names(wheat_2009) == "Acres remaining (June 1)"] <- "Acres seeded"


# 2008
wheat_2008 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 17)
#wheat_2008[2, 1] <- wheat_2008[1, 1]
#wheat_2008 <- wheat_2008[-1, ]
colnames(wheat_2008) <- as.character(wheat_2008[1, ])
wheat_2008 <- wheat_2008[-1, ]
ontario_row <- which(apply(wheat_2008, 1, function(row) any(row == "Ontario")))
wheat_2008 <- wheat_2008[1:ontario_row, ]
wheat_2008 <- wheat_2008[!grepl("Ontario", wheat_2008[, 1]), ]
rownames(wheat_2008) <- NULL

colnames(wheat_2008)[1] <- "County"
wheat_2008 <- cbind(Year = 2008, wheat_2008)

names(wheat_2008)[names(wheat_2008) == "Acres remaining (June 1)"] <- "Acres seeded"

# 2007
wheat_2007 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 18)
#wheat_2007[2, 1] <- wheat_2007[1, 1]
#wheat_2007 <- wheat_2007[-1, ]
colnames(wheat_2007) <- as.character(wheat_2007[1, ])
wheat_2007 <- wheat_2007[-1, ]
ontario_row <- which(apply(wheat_2007, 1, function(row) any(row == "Ontario")))
wheat_2007 <- wheat_2007[1:ontario_row, ]
wheat_2007 <- wheat_2007[!grepl("Ontario", wheat_2007[, 1]), ]
rownames(wheat_2007) <- NULL

colnames(wheat_2007)[1] <- "County"
wheat_2007 <- cbind(Year = 2007, wheat_2007)

names(wheat_2007)[names(wheat_2007) == "Acres remaining (June 1)"] <- "Acres seeded"

# 2006
wheat_2006 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 19)
#wheat_2006[2, 1] <- wheat_2006[1, 1]
#wheat_2006 <- wheat_2006[-1, ]
colnames(wheat_2006) <- as.character(wheat_2006[1, ])
wheat_2006 <- wheat_2006[-1, ]
ontario_row <- which(apply(wheat_2006, 1, function(row) any(row == "Ontario")))
wheat_2006 <- wheat_2006[1:ontario_row, ]
wheat_2006 <- wheat_2006[!grepl("Ontario", wheat_2006[, 1]), ]
rownames(wheat_2006) <- NULL

colnames(wheat_2006)[1] <- "County"
wheat_2006 <- cbind(Year = 2006, wheat_2006)

names(wheat_2006)[names(wheat_2006) == "Acres remaining (2006 Census)"] <- "Acres seeded"

# 2005
wheat_2005 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 20)
#wheat_2005[2, 1] <- wheat_2005[1, 1]
#wheat_2005 <- wheat_2005[-1, ]
colnames(wheat_2005) <- as.character(wheat_2005[1, ])
wheat_2005 <- wheat_2005[-1, ]
ontario_row <- which(apply(wheat_2005, 1, function(row) any(row == "Ontario")))
wheat_2005 <- wheat_2005[1:ontario_row, ]
wheat_2005 <- wheat_2005[!grepl("Ontario", wheat_2005[, 1]), ]
rownames(wheat_2005) <- NULL

colnames(wheat_2005)[1] <- "County"
wheat_2005 <- cbind(Year = 2005, wheat_2005)

names(wheat_2005)[names(wheat_2005) == "Acres seeded remaining June 1"] <- "Acres seeded"

# 2004
wheat_2004 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/wheat.xlsx", sheet = 21)
colnames(wheat_2004) <- as.character(wheat_2004[1, ])
wheat_2004 <- wheat_2004[-1, ]
ontario_row <- which(apply(wheat_2004, 1, function(row) any(row == "Ontario")))
wheat_2004 <- wheat_2004[1:ontario_row, ]
wheat_2004 <- wheat_2004[!grepl("Ontario", wheat_2004[, 1]), ]
rownames(wheat_2004) <- NULL
colnames(wheat_2004)[1] <- "County"
wheat_2004 <- cbind(Year = 2004, wheat_2004)

names(wheat_2004)[names(wheat_2004) == "June 1 area (acres)"] <- "Acres seeded"
names(wheat_2004)[names(wheat_2004) == "Harvested area (acres)"] <- "Acres harvested "
names(wheat_2004)[names(wheat_2004) == "Yield (bu/acre)"] <- "Yield (bushels/acre)"
names(wheat_2004)[names(wheat_2004) == "Production ('000 bu)"] <- "Production ('000 bushels)"

wheat_data <- bind_rows(mget(paste0("wheat_", 2004:2024)))


wheat_data[wheat_data == "-" | wheat_data == "x"] <- 0
wheat_data[is.na(wheat_data)] <- 0


cols_to_convert <- c("Acres seeded", "Acres harvested ", "Yield (bushels/acre)", 
                     "Production ('000 bushels)", "Production ('000 tonnes)")

wheat_data[cols_to_convert] <- lapply(wheat_data[cols_to_convert], function(x) {
  clean_num <- as.numeric(gsub("[^0-9.]", "", x))
  rounded <- round(clean_num, 2)
  formatted <- ifelse(rounded %% 1 == 0,
                      as.character(as.integer(rounded)),
                      sprintf("%.2f", rounded))
  return(formatted)
})


# https://censusmapper.ca/api#api_overview
options(cancensus.api_key = "CensusMapper_b3e4e303dcb50fad4050328d7c3a8e67")
# Get Ontario's province code (35)
ontario_cd_sf <- get_census(
  dataset = "CA21",  # 2021 Census
  regions = list(PR = "35"),  # Ontario
  level = "CD",  # Census Division
  geo_format = "sf",  # Return as sf object
  labels = "short"
)

# Step 1: Just name and geometry from Ontario shapefile
counties_geometry <- ontario_cd_sf %>%
  select(name, geometry)

# Step 2: Add geometry by matching county names
wheat_data_with_geom <- wheat_data %>%
  rowwise() %>%
  mutate(
    geometry = list({
      matched_geom <- counties_geometry$geometry[str_detect(counties_geometry$name, fixed(County))]
      if (length(matched_geom) > 0) matched_geom[[1]] else st_geometrycollection()
    })
  ) %>%
  ungroup()

# Step 3: Convert to sf object
wheat_data_sf <- st_as_sf(wheat_data_with_geom, crs = st_crs(counties_geometry))

wheat_2024_sf <- wheat_data_sf %>%
  filter(Year == 2024)

wheat_2024_sf <- wheat_2024_sf %>%
  mutate(`Yield (bushels/acre)` = as.numeric(`Yield (bushels/acre)`))


ggplot(data = wheat_2024_sf) +
  geom_sf(aes(fill = `Yield (bushels/acre)`), color = "gray30") +
  scale_fill_viridis_c(option = "D") +
  labs(
    title = "wheat Yield by County - 2024",
    fill = "Yield (bu/ac)"
  ) +
  theme_minimal()

brant_data <- wheat_data_sf %>%
  filter(County == "Brant")

# Prepare data
brant_data <- brant_data %>%
  mutate(
    Year = as.numeric(Year),
    Yield = as.numeric(`Yield (bushels/acre)`)
  ) %>%
  filter(!is.na(Year), !is.na(Yield))

ggplot(brant_data, aes(x = Year, y = Yield)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Wheat Yield Trend in Brant County",
    x = "Year",
    y = "Yield (bu/ac)"
  )


ggsave("/Users/sarithakumarik/Downloads/brant_yield_plot.png", width = 5, height = 3, dpi = 300)

# Filter for Wellington County
wellington_data <- wheat_data_sf %>%
  filter(County == "Wellington")

# Prepare data
wellington_data <- wellington_data %>%
  mutate(
    Year = as.numeric(Year),
    Yield = as.numeric(`Yield (bushels/acre)`)
  ) %>%
  filter(!is.na(Year), !is.na(Yield))

ggplot(wellington_data, aes(x = Year, y = Yield)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Wheat Yield Trend in Wellington County",
    x = "Year",
    y = "Yield (bu/ac)"
  )

# Save the plot
ggsave("/Users/sarithakumarik/Downloads/wellington_yield_plot.png", width = 5, height = 3, dpi = 300)


