library(openxlsx)
library(cancensus)
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)

# 2024
soybeans_2024 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 1)
soybeans_2024[2, 1] <- soybeans_2024[1, 1]
soybeans_2024 <- soybeans_2024[-1, ]
colnames(soybeans_2024) <- as.character(soybeans_2024[1, ])
soybeans_2024 <- soybeans_2024[-1, ]
ontario_row <- which(apply(soybeans_2024, 1, function(row) any(row == "Ontario")))
soybeans_2024 <- soybeans_2024[1:ontario_row, ]
soybeans_2024 <- soybeans_2024[!grepl("Ontario", soybeans_2024[, 1]), ]
rownames(soybeans_2024) <- NULL

colnames(soybeans_2024)[1] <- "County"
soybeans_2024 <- cbind(Year = 2024, soybeans_2024)

# 2023
soybeans_2023 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 2)
soybeans_2023[2, 1] <- soybeans_2023[1, 1]
soybeans_2023 <- soybeans_2023[-1, ]
colnames(soybeans_2023) <- as.character(soybeans_2023[1, ])
soybeans_2023 <- soybeans_2023[-1, ]
ontario_row <- which(apply(soybeans_2023, 1, function(row) any(row == "Ontario")))
soybeans_2023 <- soybeans_2023[1:ontario_row, ]
soybeans_2023 <- soybeans_2023[!grepl("Ontario", soybeans_2023[, 1]), ]
rownames(soybeans_2023) <- NULL

colnames(soybeans_2023)[1] <- "County"
soybeans_2023 <- cbind(Year = 2023, soybeans_2023)

# 2022
soybeans_2022 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 3)
soybeans_2022[2, 1] <- soybeans_2022[1, 1]
soybeans_2022 <- soybeans_2022[-1, ]
colnames(soybeans_2022) <- as.character(soybeans_2022[1, ])
soybeans_2022 <- soybeans_2022[-1, ]
ontario_row <- which(apply(soybeans_2022, 1, function(row) any(row == "Ontario")))
soybeans_2022 <- soybeans_2022[1:ontario_row, ]
soybeans_2022 <- soybeans_2022[!grepl("Ontario", soybeans_2022[, 1]), ]
rownames(soybeans_2022) <- NULL

colnames(soybeans_2022)[1] <- "County"
soybeans_2022 <- cbind(Year = 2022, soybeans_2022)

# 2021
soybeans_2021 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 4)
soybeans_2021[2, 1] <- soybeans_2021[1, 1]
soybeans_2021 <- soybeans_2021[-1, ]
colnames(soybeans_2021) <- as.character(soybeans_2021[1, ])
soybeans_2021 <- soybeans_2021[-1, ]
ontario_row <- which(apply(soybeans_2021, 1, function(row) any(row == "Ontario")))
soybeans_2021 <- soybeans_2021[1:ontario_row, ]
soybeans_2021 <- soybeans_2021[!grepl("Ontario", soybeans_2021[, 1]), ]
rownames(soybeans_2021) <- NULL

colnames(soybeans_2021)[1] <- "County"
soybeans_2021 <- cbind(Year = 2021, soybeans_2021)

# 2020
soybeans_2020 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 5)
soybeans_2020[2, 1] <- soybeans_2020[1, 1]
soybeans_2020 <- soybeans_2020[-1, ]
colnames(soybeans_2020) <- as.character(soybeans_2020[1, ])
soybeans_2020 <- soybeans_2020[-1, ]
ontario_row <- which(apply(soybeans_2020, 1, function(row) any(row == "Ontario")))
soybeans_2020 <- soybeans_2020[1:ontario_row, ]
soybeans_2020 <- soybeans_2020[!grepl("Ontario", soybeans_2020[, 1]), ]
rownames(soybeans_2020) <- NULL

colnames(soybeans_2020)[1] <- "County"
soybeans_2020 <- cbind(Year = 2020, soybeans_2020)

# 2019
soybeans_2019 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 6)
soybeans_2019[2, 1] <- soybeans_2019[1, 1]
soybeans_2019 <- soybeans_2019[-1, ]
colnames(soybeans_2019) <- as.character(soybeans_2019[1, ])
soybeans_2019 <- soybeans_2019[-1, ]
ontario_row <- which(apply(soybeans_2019, 1, function(row) any(row == "Ontario")))
soybeans_2019 <- soybeans_2019[1:ontario_row, ]
soybeans_2019 <- soybeans_2019[!grepl("Ontario", soybeans_2019[, 1]), ]
rownames(soybeans_2019) <- NULL

colnames(soybeans_2019)[1] <- "County"
soybeans_2019 <- cbind(Year = 2019, soybeans_2019)

# 2018
soybeans_2018 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 7)
# soybeans_2018[2, 1] <- soybeans_2018[1, 1]
# soybeans_2018 <- soybeans_2018[-1, ]
colnames(soybeans_2018) <- as.character(soybeans_2018[1, ])
soybeans_2018 <- soybeans_2018[-1, ]
ontario_row <- which(apply(soybeans_2018, 1, function(row) any(row == "Ontario")))
soybeans_2018 <- soybeans_2018[1:ontario_row, ]
soybeans_2018 <- soybeans_2018[!grepl("Ontario", soybeans_2018[, 1]), ]
rownames(soybeans_2018) <- NULL

colnames(soybeans_2018)[1] <- "County"
soybeans_2018 <- cbind(Year = 2018, soybeans_2018)

# 2017
soybeans_2017 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 8)
#soybeans_2017[2, 1] <- soybeans_2017[1, 1]
#soybeans_2017 <- soybeans_2017[-1, ]
colnames(soybeans_2017) <- as.character(soybeans_2017[1, ])
soybeans_2017 <- soybeans_2017[-1, ]
ontario_row <- which(apply(soybeans_2017, 1, function(row) any(row == "Ontario")))
soybeans_2017 <- soybeans_2017[1:ontario_row, ]
soybeans_2017 <- soybeans_2017[!grepl("Ontario", soybeans_2017[, 1]), ]
rownames(soybeans_2017) <- NULL

colnames(soybeans_2017)[1] <- "County"
soybeans_2017 <- cbind(Year = 2017, soybeans_2017)

# 2016
soybeans_2016 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 9)
#soybeans_2016[2, 1] <- soybeans_2016[1, 1]
#soybeans_2016 <- soybeans_2016[-1, ]
colnames(soybeans_2016) <- as.character(soybeans_2016[1, ])
soybeans_2016 <- soybeans_2016[-1, ]
ontario_row <- which(apply(soybeans_2016, 1, function(row) any(row == "Ontario")))
soybeans_2016 <- soybeans_2016[1:ontario_row, ]
soybeans_2016 <- soybeans_2016[!grepl("Ontario", soybeans_2016[, 1]), ]
rownames(soybeans_2016) <- NULL

colnames(soybeans_2016)[1] <- "County"
soybeans_2016 <- cbind(Year = 2016, soybeans_2016)

# 2015
soybeans_2015 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 10)
#soybeans_2015[2, 1] <- soybeans_2015[1, 1]
#soybeans_2015 <- soybeans_2015[-1, ]
colnames(soybeans_2015) <- as.character(soybeans_2015[1, ])
soybeans_2015 <- soybeans_2015[-1, ]
ontario_row <- which(apply(soybeans_2015, 1, function(row) any(row == "Ontario")))
soybeans_2015 <- soybeans_2015[1:ontario_row, ]
soybeans_2015 <- soybeans_2015[!grepl("Ontario", soybeans_2015[, 1]), ]
rownames(soybeans_2015) <- NULL

colnames(soybeans_2015)[1] <- "County"
soybeans_2015 <- cbind(Year = 2015, soybeans_2015)

names(soybeans_2015)[names(soybeans_2015) == "Acres seeded "] <- "Acres seeded"
names(soybeans_2015)[names(soybeans_2015) == "Acres harvested"] <- "Acres harvested "
names(soybeans_2015)[names(soybeans_2015) == "Yield (cwt/ac)"] <- "Yield (bushels/acre)"
names(soybeans_2015)[names(soybeans_2015) == "Production ('000 cwt)"] <- "Production ('000 bu)"



# 2014
soybeans_2014 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 11)
#soybeans_2014[2, 1] <- soybeans_2014[1, 1]
#soybeans_2014 <- soybeans_2014[-1, ]
colnames(soybeans_2014) <- as.character(soybeans_2014[1, ])
soybeans_2014 <- soybeans_2014[-1, ]
ontario_row <- which(apply(soybeans_2014, 1, function(row) any(row == "Ontario")))
soybeans_2014 <- soybeans_2014[1:ontario_row, ]
soybeans_2014 <- soybeans_2014[!grepl("Ontario", soybeans_2014[, 1]), ]
rownames(soybeans_2014) <- NULL

colnames(soybeans_2014)[1] <- "County"
soybeans_2014 <- cbind(Year = 2014, soybeans_2014)

# 2013
soybeans_2013 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 12)
#soybeans_2013[2, 1] <- soybeans_2013[1, 1]
#soybeans_2013 <- soybeans_2013[-1, ]
colnames(soybeans_2013) <- as.character(soybeans_2013[1, ])
soybeans_2013 <- soybeans_2013[-1, ]
ontario_row <- which(apply(soybeans_2013, 1, function(row) any(row == "Ontario")))
soybeans_2013 <- soybeans_2013[1:ontario_row, ]
soybeans_2013 <- soybeans_2013[!grepl("Ontario", soybeans_2013[, 1]), ]
rownames(soybeans_2013) <- NULL

colnames(soybeans_2013)[1] <- "County"
soybeans_2013 <- cbind(Year = 2013, soybeans_2013)

# 2012
soybeans_2012 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 13)
#soybeans_2012[2, 1] <- soybeans_2012[1, 1]
#soybeans_2012 <- soybeans_2012[-1, ]
colnames(soybeans_2012) <- as.character(soybeans_2012[1, ])
soybeans_2012 <- soybeans_2012[-1, ]
ontario_row <- which(apply(soybeans_2012, 1, function(row) any(row == "Ontario")))
soybeans_2012 <- soybeans_2012[1:ontario_row, ]
soybeans_2012 <- soybeans_2012[!grepl("Ontario", soybeans_2012[, 1]), ]
rownames(soybeans_2012) <- NULL

colnames(soybeans_2012)[1] <- "County"
soybeans_2012 <- cbind(Year = 2012, soybeans_2012)

# 2011
soybeans_2011 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 14)
#soybeans_2011[2, 1] <- soybeans_2011[1, 1]
#soybeans_2011 <- soybeans_2011[-1, ]
colnames(soybeans_2011) <- as.character(soybeans_2011[1, ])
soybeans_2011 <- soybeans_2011[-1, ]
ontario_row <- which(apply(soybeans_2011, 1, function(row) any(row == "Ontario")))
soybeans_2011 <- soybeans_2011[1:ontario_row, ]
soybeans_2011 <- soybeans_2011[!grepl("Ontario", soybeans_2011[, 1]), ]
rownames(soybeans_2011) <- NULL

colnames(soybeans_2011)[1] <- "County"
soybeans_2011 <- cbind(Year = 2011, soybeans_2011)

names(soybeans_2011)[names(soybeans_2011) == "Acres seeded(a)"] <- "Acres seeded"

# 2010
soybeans_2010 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 15)
#soybeans_2010[2, 1] <- soybeans_2010[1, 1]
#soybeans_2010 <- soybeans_2010[-1, ]
colnames(soybeans_2010) <- as.character(soybeans_2010[1, ])
soybeans_2010 <- soybeans_2010[-1, ]
ontario_row <- which(apply(soybeans_2010, 1, function(row) any(row == "Ontario")))
soybeans_2010 <- soybeans_2010[1:ontario_row, ]
soybeans_2010 <- soybeans_2010[!grepl("Ontario", soybeans_2010[, 1]), ]
rownames(soybeans_2010) <- NULL

colnames(soybeans_2010)[1] <- "County"
soybeans_2010 <- cbind(Year = 2010, soybeans_2010)


# 2009
soybeans_2009 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 16)
#soybeans_2009[2, 1] <- soybeans_2009[1, 1]
#soybeans_2009 <- soybeans_2009[-1, ]
colnames(soybeans_2009) <- as.character(soybeans_2009[1, ])
soybeans_2009 <- soybeans_2009[-1, ]
ontario_row <- which(apply(soybeans_2009, 1, function(row) any(row == "Ontario")))
soybeans_2009 <- soybeans_2009[1:ontario_row, ]
soybeans_2009 <- soybeans_2009[!grepl("Ontario", soybeans_2009[, 1]), ]
rownames(soybeans_2009) <- NULL

colnames(soybeans_2009)[1] <- "County"
soybeans_2009 <- cbind(Year = 2009, soybeans_2009)



# 2008
soybeans_2008 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 17)
#soybeans_2008[2, 1] <- soybeans_2008[1, 1]
#soybeans_2008 <- soybeans_2008[-1, ]
colnames(soybeans_2008) <- as.character(soybeans_2008[1, ])
soybeans_2008 <- soybeans_2008[-1, ]
ontario_row <- which(apply(soybeans_2008, 1, function(row) any(row == "Ontario")))
soybeans_2008 <- soybeans_2008[1:ontario_row, ]
soybeans_2008 <- soybeans_2008[!grepl("Ontario", soybeans_2008[, 1]), ]
rownames(soybeans_2008) <- NULL

colnames(soybeans_2008)[1] <- "County"
soybeans_2008 <- cbind(Year = 2008, soybeans_2008)



# 2007
soybeans_2007 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 18)
#soybeans_2007[2, 1] <- soybeans_2007[1, 1]
#soybeans_2007 <- soybeans_2007[-1, ]
colnames(soybeans_2007) <- as.character(soybeans_2007[1, ])
soybeans_2007 <- soybeans_2007[-1, ]
ontario_row <- which(apply(soybeans_2007, 1, function(row) any(row == "Ontario")))
soybeans_2007 <- soybeans_2007[1:ontario_row, ]
soybeans_2007 <- soybeans_2007[!grepl("Ontario", soybeans_2007[, 1]), ]
rownames(soybeans_2007) <- NULL

colnames(soybeans_2007)[1] <- "County"
soybeans_2007 <- cbind(Year = 2007, soybeans_2007)


# 2006
soybeans_2006 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 19)
#soybeans_2006[2, 1] <- soybeans_2006[1, 1]
#soybeans_2006 <- soybeans_2006[-1, ]
colnames(soybeans_2006) <- as.character(soybeans_2006[1, ])
soybeans_2006 <- soybeans_2006[-1, ]
ontario_row <- which(apply(soybeans_2006, 1, function(row) any(row == "Ontario")))
soybeans_2006 <- soybeans_2006[1:ontario_row, ]
soybeans_2006 <- soybeans_2006[!grepl("Ontario", soybeans_2006[, 1]), ]
rownames(soybeans_2006) <- NULL

colnames(soybeans_2006)[1] <- "County"
soybeans_2006 <- cbind(Year = 2006, soybeans_2006)

names(soybeans_2006)[names(soybeans_2006) == "Acres seeded (2006 Census)"] <- "Acres seeded"



# 2005
soybeans_2005 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 20)
#soybeans_2005[2, 1] <- soybeans_2005[1, 1]
#soybeans_2005 <- soybeans_2005[-1, ]
colnames(soybeans_2005) <- as.character(soybeans_2005[1, ])
soybeans_2005 <- soybeans_2005[-1, ]
ontario_row <- which(apply(soybeans_2005, 1, function(row) any(row == "Ontario")))
soybeans_2005 <- soybeans_2005[1:ontario_row, ]
soybeans_2005 <- soybeans_2005[!grepl("Ontario", soybeans_2005[, 1]), ]
rownames(soybeans_2005) <- NULL

colnames(soybeans_2005)[1] <- "County"
soybeans_2005 <- cbind(Year = 2005, soybeans_2005)

# 2004
soybeans_2004 <- read.xlsx("/Users/sarithakumarik/Documents/DATA6500/Project/Crop Data/soybeans.xlsx", sheet = 21)
colnames(soybeans_2004) <- as.character(soybeans_2004[1, ])
soybeans_2004 <- soybeans_2004[-1, ]
ontario_row <- which(apply(soybeans_2004, 1, function(row) any(row == "Ontario")))
soybeans_2004 <- soybeans_2004[1:ontario_row, ]
soybeans_2004 <- soybeans_2004[!grepl("Ontario", soybeans_2004[, 1]), ]
rownames(soybeans_2004) <- NULL
colnames(soybeans_2004)[1] <- "County"
soybeans_2004 <- cbind(Year = 2004, soybeans_2004)


soybeans_data <- bind_rows(mget(paste0("soybeans_", 2004:2024)))

soybeans_data[soybeans_data == "-" | soybeans_data == "x"] <- 0
soybeans_data[is.na(soybeans_data)] <- 0


cols_to_convert <- c("Acres seeded", "Acres harvested ", "Yield (bushels/acre)", 
                     "Production ('000 bu)", "Production ('000 tonnes)")

soybeans_data[cols_to_convert] <- lapply(soybeans_data[cols_to_convert], function(x) {
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
soybeans_data_with_geom <- soybeans_data %>%
  rowwise() %>%
  mutate(
    geometry = list({
      matched_geom <- counties_geometry$geometry[str_detect(counties_geometry$name, fixed(County))]
      if (length(matched_geom) > 0) matched_geom[[1]] else st_geometrycollection()
    })
  ) %>%
  ungroup()

# Step 3: Convert to sf object
soybeans_data_sf <- st_as_sf(soybeans_data_with_geom, crs = st_crs(counties_geometry))

soybeans_2024_sf <- soybeans_data_sf %>%
  filter(Year == 2024)

soybeans_2024_sf <- soybeans_2024_sf %>%
  mutate(`Yield (bushels/acre)` = as.numeric(`Yield (bushels/acre)`))


ggplot(data = soybeans_2024_sf) +
  geom_sf(aes(fill = `Yield (bushels/acre)`), color = "gray30") +
  scale_fill_viridis_c(option = "D") +
  labs(
    title = "soybeans Yield by County - 2024",
    fill = "Yield (bu/ac)"
  ) +
  theme_minimal()

brant_data <- soybeans_data_sf %>%
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
    title = "soybeans Yield Trend in Brant County",
    x = "Year",
    y = "Yield (bu/ac)"
  )


ggsave("/Users/sarithakumarik/Downloads/brant_yield_plot.png", width = 5, height = 3, dpi = 300)

# Filter for Wellington County
wellington_data <- soybeans_data_sf %>%
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
    title = "soybeans Yield Trend in Wellington County",
    x = "Year",
    y = "Yield (bu/ac)"
  )

# Save the plot
ggsave("/Users/sarithakumarik/Downloads/wellington_yield_plot.png", width = 5, height = 3, dpi = 300)


