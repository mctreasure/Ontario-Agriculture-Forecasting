library(openxlsx)
library(cancensus)
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)

# 2024
corn_2024 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 1)
corn_2024[2, 1] <- corn_2024[1, 1]
corn_2024 <- corn_2024[-1, ]
colnames(corn_2024) <- as.character(corn_2024[1, ])
corn_2024 <- corn_2024[-1, ]
ontario_row <- which(apply(corn_2024, 1, function(row) any(row == "Ontario")))
corn_2024 <- corn_2024[1:ontario_row, ]
corn_2024 <- corn_2024[!grepl("Ontario", corn_2024[, 1]), ]
rownames(corn_2024) <- NULL

colnames(corn_2024)[1] <- "County"
corn_2024 <- cbind(Year = 2024, corn_2024)

# 2023
corn_2023 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 2)
corn_2023[2, 1] <- corn_2023[1, 1]
corn_2023 <- corn_2023[-1, ]
colnames(corn_2023) <- as.character(corn_2023[1, ])
corn_2023 <- corn_2023[-1, ]
ontario_row <- which(apply(corn_2023, 1, function(row) any(row == "Ontario")))
corn_2023 <- corn_2023[1:ontario_row, ]
corn_2023 <- corn_2023[!grepl("Ontario", corn_2023[, 1]), ]
rownames(corn_2023) <- NULL

colnames(corn_2023)[1] <- "County"
corn_2023 <- cbind(Year = 2023, corn_2023)

# 2022
corn_2022 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 3)
corn_2022[2, 1] <- corn_2022[1, 1]
corn_2022 <- corn_2022[-1, ]
colnames(corn_2022) <- as.character(corn_2022[1, ])
corn_2022 <- corn_2022[-1, ]
ontario_row <- which(apply(corn_2022, 1, function(row) any(row == "Ontario")))
corn_2022 <- corn_2022[1:ontario_row, ]
corn_2022 <- corn_2022[!grepl("Ontario", corn_2022[, 1]), ]
rownames(corn_2022) <- NULL

colnames(corn_2022)[1] <- "County"
corn_2022 <- cbind(Year = 2022, corn_2022)

# 2021
corn_2021 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 4)
corn_2021[2, 1] <- corn_2021[1, 1]
corn_2021 <- corn_2021[-1, ]
colnames(corn_2021) <- as.character(corn_2021[1, ])
corn_2021 <- corn_2021[-1, ]
ontario_row <- which(apply(corn_2021, 1, function(row) any(row == "Ontario")))
corn_2021 <- corn_2021[1:ontario_row, ]
corn_2021 <- corn_2021[!grepl("Ontario", corn_2021[, 1]), ]
rownames(corn_2021) <- NULL

colnames(corn_2021)[1] <- "County"
corn_2021 <- cbind(Year = 2021, corn_2021)

# 2020
corn_2020 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 5)
corn_2020[2, 1] <- corn_2020[1, 1]
corn_2020 <- corn_2020[-1, ]
colnames(corn_2020) <- as.character(corn_2020[1, ])
corn_2020 <- corn_2020[-1, ]
ontario_row <- which(apply(corn_2020, 1, function(row) any(row == "Ontario")))
corn_2020 <- corn_2020[1:ontario_row, ]
corn_2020 <- corn_2020[!grepl("Ontario", corn_2020[, 1]), ]
rownames(corn_2020) <- NULL

colnames(corn_2020)[1] <- "County"
corn_2020 <- cbind(Year = 2020, corn_2020)

# 2019
corn_2019 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 6)
corn_2019[2, 1] <- corn_2019[1, 1]
corn_2019 <- corn_2019[-1, ]
colnames(corn_2019) <- as.character(corn_2019[1, ])
corn_2019 <- corn_2019[-1, ]
ontario_row <- which(apply(corn_2019, 1, function(row) any(row == "Ontario")))
corn_2019 <- corn_2019[1:ontario_row, ]
corn_2019 <- corn_2019[!grepl("Ontario", corn_2019[, 1]), ]
rownames(corn_2019) <- NULL

colnames(corn_2019)[1] <- "County"
corn_2019 <- cbind(Year = 2019, corn_2019)

# 2018
corn_2018 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 7)
# corn_2018[2, 1] <- corn_2018[1, 1]
# corn_2018 <- corn_2018[-1, ]
colnames(corn_2018) <- as.character(corn_2018[1, ])
corn_2018 <- corn_2018[-1, ]
ontario_row <- which(apply(corn_2018, 1, function(row) any(row == "Ontario")))
corn_2018 <- corn_2018[1:ontario_row, ]
corn_2018 <- corn_2018[!grepl("Ontario", corn_2018[, 1]), ]
rownames(corn_2018) <- NULL

colnames(corn_2018)[1] <- "County"
corn_2018 <- cbind(Year = 2018, corn_2018)

# 2017
corn_2017 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 8)
#corn_2017[2, 1] <- corn_2017[1, 1]
#corn_2017 <- corn_2017[-1, ]
colnames(corn_2017) <- as.character(corn_2017[1, ])
corn_2017 <- corn_2017[-1, ]
ontario_row <- which(apply(corn_2017, 1, function(row) any(row == "Ontario")))
corn_2017 <- corn_2017[1:ontario_row, ]
corn_2017 <- corn_2017[!grepl("Ontario", corn_2017[, 1]), ]
rownames(corn_2017) <- NULL

colnames(corn_2017)[1] <- "County"
corn_2017 <- cbind(Year = 2017, corn_2017)

# 2016
corn_2016 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 9)
#corn_2016[2, 1] <- corn_2016[1, 1]
#corn_2016 <- corn_2016[-1, ]
colnames(corn_2016) <- as.character(corn_2016[1, ])
corn_2016 <- corn_2016[-1, ]
ontario_row <- which(apply(corn_2016, 1, function(row) any(row == "Ontario")))
corn_2016 <- corn_2016[1:ontario_row, ]
corn_2016 <- corn_2016[!grepl("Ontario", corn_2016[, 1]), ]
rownames(corn_2016) <- NULL

colnames(corn_2016)[1] <- "County"
corn_2016 <- cbind(Year = 2016, corn_2016)

# 2015
corn_2015 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 10)
#corn_2015[2, 1] <- corn_2015[1, 1]
#corn_2015 <- corn_2015[-1, ]
colnames(corn_2015) <- as.character(corn_2015[1, ])
corn_2015 <- corn_2015[-1, ]
ontario_row <- which(apply(corn_2015, 1, function(row) any(row == "Ontario")))
corn_2015 <- corn_2015[1:ontario_row, ]
corn_2015 <- corn_2015[!grepl("Ontario", corn_2015[, 1]), ]
rownames(corn_2015) <- NULL

colnames(corn_2015)[1] <- "County"
corn_2015 <- cbind(Year = 2015, corn_2015)

# 2014
corn_2014 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 11)
#corn_2014[2, 1] <- corn_2014[1, 1]
#corn_2014 <- corn_2014[-1, ]
colnames(corn_2014) <- as.character(corn_2014[1, ])
corn_2014 <- corn_2014[-1, ]
ontario_row <- which(apply(corn_2014, 1, function(row) any(row == "Ontario")))
corn_2014 <- corn_2014[1:ontario_row, ]
corn_2014 <- corn_2014[!grepl("Ontario", corn_2014[, 1]), ]
rownames(corn_2014) <- NULL

colnames(corn_2014)[1] <- "County"
corn_2014 <- cbind(Year = 2014, corn_2014)

# 2013
corn_2013 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 12)
#corn_2013[2, 1] <- corn_2013[1, 1]
#corn_2013 <- corn_2013[-1, ]
colnames(corn_2013) <- as.character(corn_2013[1, ])
corn_2013 <- corn_2013[-1, ]
ontario_row <- which(apply(corn_2013, 1, function(row) any(row == "Ontario")))
corn_2013 <- corn_2013[1:ontario_row, ]
corn_2013 <- corn_2013[!grepl("Ontario", corn_2013[, 1]), ]
rownames(corn_2013) <- NULL

colnames(corn_2013)[1] <- "County"
corn_2013 <- cbind(Year = 2013, corn_2013)

# 2012
corn_2012 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 13)
#corn_2012[2, 1] <- corn_2012[1, 1]
#corn_2012 <- corn_2012[-1, ]
colnames(corn_2012) <- as.character(corn_2012[1, ])
corn_2012 <- corn_2012[-1, ]
ontario_row <- which(apply(corn_2012, 1, function(row) any(row == "Ontario")))
corn_2012 <- corn_2012[1:ontario_row, ]
corn_2012 <- corn_2012[!grepl("Ontario", corn_2012[, 1]), ]
rownames(corn_2012) <- NULL

colnames(corn_2012)[1] <- "County"
corn_2012 <- cbind(Year = 2012, corn_2012)

# 2011
corn_2011 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 14)
#corn_2011[2, 1] <- corn_2011[1, 1]
#corn_2011 <- corn_2011[-1, ]
colnames(corn_2011) <- as.character(corn_2011[1, ])
corn_2011 <- corn_2011[-1, ]
ontario_row <- which(apply(corn_2011, 1, function(row) any(row == "Ontario")))
corn_2011 <- corn_2011[1:ontario_row, ]
corn_2011 <- corn_2011[!grepl("Ontario", corn_2011[, 1]), ]
rownames(corn_2011) <- NULL

colnames(corn_2011)[1] <- "County"
corn_2011 <- cbind(Year = 2011, corn_2011)

# 2010
corn_2010 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 15)
#corn_2010[2, 1] <- corn_2010[1, 1]
#corn_2010 <- corn_2010[-1, ]
colnames(corn_2010) <- as.character(corn_2010[1, ])
corn_2010 <- corn_2010[-1, ]
ontario_row <- which(apply(corn_2010, 1, function(row) any(row == "Ontario")))
corn_2010 <- corn_2010[1:ontario_row, ]
corn_2010 <- corn_2010[!grepl("Ontario", corn_2010[, 1]), ]
rownames(corn_2010) <- NULL

colnames(corn_2010)[1] <- "County"
corn_2010 <- cbind(Year = 2010, corn_2010)

# 2009
corn_2009 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 16)
#corn_2009[2, 1] <- corn_2009[1, 1]
#corn_2009 <- corn_2009[-1, ]
colnames(corn_2009) <- as.character(corn_2009[1, ])
corn_2009 <- corn_2009[-1, ]
ontario_row <- which(apply(corn_2009, 1, function(row) any(row == "Ontario")))
corn_2009 <- corn_2009[1:ontario_row, ]
corn_2009 <- corn_2009[!grepl("Ontario", corn_2009[, 1]), ]
rownames(corn_2009) <- NULL

colnames(corn_2009)[1] <- "County"
corn_2009 <- cbind(Year = 2009, corn_2009)


# 2008
corn_2008 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 17)
#corn_2008[2, 1] <- corn_2008[1, 1]
#corn_2008 <- corn_2008[-1, ]
colnames(corn_2008) <- as.character(corn_2008[1, ])
corn_2008 <- corn_2008[-1, ]
ontario_row <- which(apply(corn_2008, 1, function(row) any(row == "Ontario")))
corn_2008 <- corn_2008[1:ontario_row, ]
corn_2008 <- corn_2008[!grepl("Ontario", corn_2008[, 1]), ]
rownames(corn_2008) <- NULL

colnames(corn_2008)[1] <- "County"
corn_2008 <- cbind(Year = 2008, corn_2008)

# 2007
corn_2007 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 18)
#corn_2007[2, 1] <- corn_2007[1, 1]
#corn_2007 <- corn_2007[-1, ]
colnames(corn_2007) <- as.character(corn_2007[1, ])
corn_2007 <- corn_2007[-1, ]
ontario_row <- which(apply(corn_2007, 1, function(row) any(row == "Ontario")))
corn_2007 <- corn_2007[1:ontario_row, ]
corn_2007 <- corn_2007[!grepl("Ontario", corn_2007[, 1]), ]
rownames(corn_2007) <- NULL

colnames(corn_2007)[1] <- "County"
corn_2007 <- cbind(Year = 2007, corn_2007)

# 2006
corn_2006 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 19)
#corn_2006[2, 1] <- corn_2006[1, 1]
#corn_2006 <- corn_2006[-1, ]
colnames(corn_2006) <- as.character(corn_2006[1, ])
corn_2006 <- corn_2006[-1, ]
ontario_row <- which(apply(corn_2006, 1, function(row) any(row == "Ontario")))
corn_2006 <- corn_2006[1:ontario_row, ]
corn_2006 <- corn_2006[!grepl("Ontario", corn_2006[, 1]), ]
rownames(corn_2006) <- NULL

colnames(corn_2006)[1] <- "County"
corn_2006 <- cbind(Year = 2006, corn_2006)

# 2005
corn_2005 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 20)
#corn_2005[2, 1] <- corn_2005[1, 1]
#corn_2005 <- corn_2005[-1, ]
colnames(corn_2005) <- as.character(corn_2005[1, ])
corn_2005 <- corn_2005[-1, ]
ontario_row <- which(apply(corn_2005, 1, function(row) any(row == "Ontario")))
corn_2005 <- corn_2005[1:ontario_row, ]
corn_2005 <- corn_2005[!grepl("Ontario", corn_2005[, 1]), ]
rownames(corn_2005) <- NULL

colnames(corn_2005)[1] <- "County"
corn_2005 <- cbind(Year = 2005, corn_2005)

# 2004
corn_2004 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/corn.xlsx", sheet = 21)
colnames(corn_2004) <- as.character(corn_2004[1, ])
corn_2004 <- corn_2004[-1, ]
ontario_row <- which(apply(corn_2004, 1, function(row) any(row == "Ontario")))
corn_2004 <- corn_2004[1:ontario_row, ]
corn_2004 <- corn_2004[!grepl("Ontario", corn_2004[, 1]), ]
rownames(corn_2004) <- NULL
colnames(corn_2004)[1] <- "County"
corn_2004 <- cbind(Year = 2004, corn_2004)


corn_data <- bind_rows(mget(paste0("corn_", 2004:2024)))


corn_data[corn_data == "-" | corn_data == "x"] <- 0
corn_data[is.na(corn_data)] <- 0


cols_to_convert <- c("Acres seeded ", "Acres harvested", "Yield (bushels/ac)", 
                     "Production ('000 bushels)", "Production ('000 tonnes)")

corn_data[cols_to_convert] <- lapply(corn_data[cols_to_convert], function(x) {
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
corn_data_with_geom <- corn_data %>%
  rowwise() %>%
  mutate(
    geometry = list({
      matched_geom <- counties_geometry$geometry[str_detect(counties_geometry$name, fixed(County))]
      if (length(matched_geom) > 0) matched_geom[[1]] else st_geometrycollection()
    })
  ) %>%
  ungroup()

# Step 3: Convert to sf object
corn_data_sf <- st_as_sf(corn_data_with_geom, crs = st_crs(counties_geometry))

corn_2024_sf <- corn_data_sf %>%
  filter(Year == 2024)

corn_2024_sf <- corn_2024_sf %>%
  mutate(`Yield (bushels/ac)` = as.numeric(`Yield (bushels/ac)`))


ggplot(data = corn_2024_sf) +
  geom_sf(aes(fill = `Yield (bushels/ac)`), color = "gray30") +
  scale_fill_viridis_c(option = "D") +
  labs(
    title = "Corn Yield by County (2024)",
    fill = "Yield (bu/ac)"
  ) +
  theme_minimal()

# Prepare data
brant_data <- brant_data %>%
  mutate(
    Year = as.numeric(Year),
    Yield = as.numeric(`Yield (bushels/ac)`)
  ) %>%
  filter(!is.na(Year), !is.na(Yield))

ggplot(brant_data, aes(x = Year, y = Yield)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  scale_y_continuous(limits = c(100, NA), expand = c(0, 0)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Corn Yield Trend in Brant County",
    x = "Year",
    y = "Yield"
  )

ggsave("/Users/sarithakumarik/Downloads/brant_yield_plot.png", width = 5, height = 3, dpi = 300)



