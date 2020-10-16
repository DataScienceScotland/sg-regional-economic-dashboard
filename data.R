# constants ####
vector_of_local_authorities <- c(
  "Scotland",
  "Aberdeen City",
  "Aberdeenshire",
  "Angus",
  "Argyll and Bute",
  "City of Edinburgh",
  "Clackmannanshire",
  "Dumfries and Galloway",
  "Dundee City",
  "East Ayrshire",
  "East Dunbartonshire",
  "East Lothian",
  "East Renfrewshire",
  "Falkirk",
  "Fife",
  "Glasgow City",
  "Highland",
  "Inverclyde",
  "Midlothian",
  "Moray",
  "Na h-Eileanan Siar",
  "North Ayrshire",
  "North Lanarkshire",
  "Orkney Islands",
  "Perth and Kinross",
  "Renfrewshire",
  "Scottish Borders",
  "Shetland Islands",
  "South Ayrshire",
  "South Lanarkshire",
  "Stirling",
  "West Dunbartonshire",
  "West Lothian"
)

# vector_of_indicators <- c(
#   "Regional Gross Value Added (balanced), Current Prices (  millions)",
#   "Number of VAT and/or PAYE registered private sector enterprises (as at March each year)",
#   "Small (0-49 employees) business share of private sector jobs (as at March each year)",
#   "VAT/PAYE business registrations (Business Births)",
#   "VAT/PAYE business registrations (Business Births) per 10,000 adults (16+)",
#   "VAT/PAYE business de-registrations (Business Deaths)",
#   "VAT/PAYE business de-registrations (Business Deaths) per 10,000 adults (16+)",
#   "3-year business survival (survival of newly born VAT/PAYE  businesses)",
#   "3-year business survival rate (survival rate of newly born VAT/PAYE  businesses)",
#   "Number of foreign-owned registered private sector enterprises (as at March each year)",
#   "Number of EU foreign-owned registered private sector enterprises (as at March each year)",
#   "Foreign-owned business share of private sector jobs (as at March each year)",
#   "EU foreign-owned business share of private sector jobs (as at March each year)",
#   "International Exports Value ( million) (experimental statistics)",
#   "EU International Exports Value ( million) (experimental statistics)",
#   "Business Enterprise Research and Development (BERD) expenditure - Current prices ( million)",
#   "Business Enterprise Research and Development (BERD) Jobs",
#   "Number of high growth registered private sector enterprises (as at March each year)",
#   "Jobs in VAT/PAYE Registered Businesses (workplace-based, private and public sector jobs)",
#   "Number of people in employment (aged 16+) (residence based)",
#   "Working age employment rate (residence based)",
#   "95% confidence interval of percentage of working age employment rate",
#   "Number of people aged 16+ in employment who are self-employed",
#   "Percentage of people aged 16+ in employment who are self-employed",
#   "95% confidence interval of percentage of people aged 16+ in employment who are self-employed",
#   "Number of females aged 16+ in employment who are self-employed",
#   "Percentage of females aged 16+ in employment who are self-employed",
#   "95% confidence interval of percentage of females aged 16+ in employment who are self-employed",
#   "Model-based estimates of unemployment - people aged 16+ unemployed",
#   "Unemployment Rate (Percentage of economically active people aged 16+  who are unemployed)",
#   "95% confidence interval of percentage of unemployment rate",
#   "Median gross weekly pay for full time workers (resident analysis)",
#   "Standard error as a percentage of median gross weekly pay for full time workers (resident analysis)",
#   "Median gross weekly pay for full time workers (workplace analysis)",
#   "Standard error as a percentage of median gross weekly pay for full time workers (workplace analysis)",
#   "Total population estimates", 
#   "Population estimates - aged 16+",
#   "Population estimates - working age (16-64)"
# )

vector_of_indicators <- c(
  "Gross Value Added",
  "VAT/PAYE registered enterprises",
  "Small businesses",
  "VAT/PAYE (Business Births)",
  "VAT/PAYE (Business Births) per 10,000 adults",
  "VAT/PAYE (Business Deaths)",
  "VAT/PAYE (Business Deaths) per 10,000 adults",
  "3-year business survival",
  "3-year business survival rate",
  "Foreign-owned enterprises",
  "Foreign-owned enterprises (EU)",
  "Foreign-owned business jobs",
  "Foreign-owned business jobs (EU)",
  "International Exports Value",
  "International Exports Value (EU)",
  "BERD",
  "BERD Jobs",
  "High growth enterprises",
  "VAT/PAYE Businesses Jobs",
  "Employment",
  "Working age employment rate",
  "Working age employment rate (95% CI)",
  "Self-employed",
  "Self-employed rate",
  "Self-employed (95% CI)",
  "Self-employed Females",
  "Self-employed Females rate",
  "Self-employed Females rate (95% CI)",
  "Unemployment",
  "Unemployment rate",
  "Unemployment rate (95% CI)",
  "Median gross weekly pay (RA)",
  "Median gross weekly pay (RA) (SE)",
  "Median gross weekly pay (WE)",
  "Median gross weekly pay (WE) (SE)",
  "Population", 
  "Population (16+)",
  "Population (16-64)"
)

vector_of_regions <- c(
  "Scotland",
  "Aberdeen City Region",
  "Glasgow City Region",
  "Edinburgh and South East Scotland City Region",
  "Stirling, Clackmannanshire and Falkirk Region",
  "Tay Cities Region",
  "Highlands and Islands",
  "Ayrshires",
  "South of Scotland"
)

vector_of_ruandurb <- c(
  "Scotland",
  "Islands & Remote Rural",
  "Mainly Rural",
  "Urban with Substantial Rural areas",
  "Larger Cities"
)

vector_of_enterprise <- c(
  "Scotland",
  "Highlands and Islands Enterprise",
  "South of Scotland Enterprise",
  "Scottish Enterprise"
)

# libraries ####
library(shiny)                # For the app iteslf.
library(shinythemes)          # For the "cerulean" theme.
library(shinyhelper)          # Used for help modal boxes.
library(shinyEffects)         # Used for home page effects.
library(shinyanimate)         # Used for home page effects.
library(shinycssloaders)      # For loaders.
library(magrittr)             # Dependency of other packages. Lets use the pipe operator (%>%).
library(data.table)           # Needed for function na.omit().
library(DT)                   # For the interactive tables.
library(dygraphs)             # For the interactive graphs.
library(leaflet)              # For interactive maps.
library(RColorBrewer)         # Used to create a colour palette for the map.
library(rgdal)                # Needed to load shapfile for the map.
library(plyr)                 # Needed for function revalue(), for editing country names.
library(rmarkdown)            # Used for reports, especially important is function render().
library(knitr)                # Takes Rmd. file and turns it into .md document which includes the R code and it’s output.
library(networkD3)            # Used to create the sankey diagram.
library(treemap)              # Used to create the static treemap.
library(dplyr)                # Used for data manipulation.
library(ggplot2)              # Used for plots in country and sector profiles.
library(plotly)               # Used for the streamgraph.
library(ggsci)                # Used for colour palettes.
library(countrycode)          # Enables conversion from Common country names to ISO codes.
library(knitr)                # Used for sector definitions table
library(kableExtra)           # Used for styling the sector definitions table
library(rsconnect)            # Used for shinyapp.io uploads
library(tidyr)                # Used for data manipulation
library(openxlsx)             # Used to read excel spreadsheets
library(plotly)               # Used for plots

# Dependencies
library(DBI)
library(XML)
library(anytime)
library(backports)
library(broom)
library(cellranger)
library(classInt)
library(cowplot)
library(dbplyr)
library(dichromat)
library(flextable)
library(forcats)
library(fs)
library(gdtools)
library(gtools)
library(haven)
library(leafsync)
library(lubridate)
library(lwgeom)
library(modelr)
library(prettyunits)
library(progress)
library(readxl)
library(reprex)
library(rgeos)
library(rintrojs)
library(rlist)
library(sf)
library(shinyBS)
library(shinyWidgets)
library(shinyalert)
library(shinyjs)
library(tidyverse)
library(tmap)
library(tmaptools)
library(sunburstR)
library(viridis)


# Loading map data ####
mapex <- readOGR(dsn="./www/regional/fwlibrariesandshapefile", layer="Scotland1")
mapex_regions <- readOGR(dsn="./www/regional/fwlibrariesandshapefile_regions", layer="Scotland1")
mapex_regions_data <- mapex_regions@data
OBJECTID <- c(1,
              2,
              3,
              4,
              5,
              6,
              7,
              8,
              9,
              10,
              11,
              12,
              13,
              14,
              15,
              16,
              17,
              18,
              19,
              20,
              21,
              22,
              23,
              24,
              25,
              24,
              27,
              28,
              29,
              30,
              31,
              32
)
mapex_regions_data$OBJECTID <- OBJECTID
mapex_regions@data <- mapex_regions_data


# LOCAL AUTHORITIES ####
t1_la <- read.csv("./www/local_authorities/t1.csv", check.names=FALSE)
t2a_la <- read.csv("./www/local_authorities/t2a.csv", check.names=FALSE)
t2b_la <- read.csv("./www/local_authorities/t2b.csv", check.names=FALSE)
t3a_la <- read.csv("./www/local_authorities/t3a.csv", check.names=FALSE)
t3b_la <- read.csv("./www/local_authorities/t3b.csv", check.names=FALSE)
t4a_la <- read.csv("./www/local_authorities/t4a.csv", check.names=FALSE)
t4b_la <- read.csv("./www/local_authorities/t4b.csv", check.names=FALSE)
t5a_la <- read.csv("./www/local_authorities/t5a.csv", check.names=FALSE)
t5b_la <- read.csv("./www/local_authorities/t5b.csv", check.names=FALSE)
t6a_la <- read.csv("./www/local_authorities/t6a.csv", check.names=FALSE)
t6b_la <- read.csv("./www/local_authorities/t6b.csv", check.names=FALSE)
t7a_la <- read.csv("./www/local_authorities/t7a.csv", check.names=FALSE)
t7b_la <- read.csv("./www/local_authorities/t7b.csv", check.names=FALSE)
t8a_la <- read.csv("./www/local_authorities/t8a.csv", check.names=FALSE)
t8b_la <- read.csv("./www/local_authorities/t8b.csv", check.names=FALSE)
t9_la <- read.csv("./www/local_authorities/t9.csv", check.names=FALSE)
t10_la <- read.csv("./www/local_authorities/t10.csv", check.names=FALSE)
t11_la <- read.csv("./www/local_authorities/t11.csv", check.names=FALSE)
t12_la <- read.csv("./www/local_authorities/t12.csv", check.names=FALSE)
t13_la <- read.csv("./www/local_authorities/t13.csv", check.names=FALSE)
t14a_la <- read.csv("./www/local_authorities/t14a.csv", check.names=FALSE)
t14b_la <- read.csv("./www/local_authorities/t14b.csv", check.names=FALSE)
t15a_la <- read.csv("./www/local_authorities/t15a.csv", check.names=FALSE)
t15b_la <- read.csv("./www/local_authorities/t15b.csv", check.names=FALSE)
t15c_la <- read.csv("./www/local_authorities/t15c.csv", check.names=FALSE)
t16a_la <- read.csv("./www/local_authorities/t16a.csv", check.names=FALSE)
t16b_la <- read.csv("./www/local_authorities/t16b.csv", check.names=FALSE)
t16c_la <- read.csv("./www/local_authorities/t16c.csv", check.names=FALSE)
t17a_la <- read.csv("./www/local_authorities/t17a.csv", check.names=FALSE)
t17b_la <- read.csv("./www/local_authorities/t17b.csv", check.names=FALSE)
t17c_la <- read.csv("./www/local_authorities/t17c.csv", check.names=FALSE)
t18a_la <- read.csv("./www/local_authorities/t18a.csv", check.names=FALSE)
t18b_la <- read.csv("./www/local_authorities/t18b.csv", check.names=FALSE)
t19a_la <- read.csv("./www/local_authorities/t19a.csv", check.names=FALSE)
t19b_la <- read.csv("./www/local_authorities/t19b.csv", check.names=FALSE)
t20_la <- read.csv("./www/local_authorities/t20.csv", check.names=FALSE)
t21_la <- read.csv("./www/local_authorities/t21.csv", check.names=FALSE)
t22_la <- read.csv("./www/local_authorities/t22.csv", check.names=FALSE)
t22_la <- read.csv("./www/local_authorities/t22.csv", check.names=FALSE)
summary_tool_la <- read.csv("./www/local_authorities/summary_tool.csv", check.names=FALSE)

# Highlighted indicators for Local Authorities ####
# t1 for leaflet map in Local Authorities (GVA per head) ####
t1_la_long <- t1_la %>% 
  gather("Year", "Value", 2:ncol(t1_la))
colnames(t1_la_long) <- c("NAME", "Year", "Value")
t1_la_long$Year <- as.numeric(t1_la_long$Year)
start_year_t1_la <- min(t1_la_long$Year)
end_year_t1_la <- max(t1_la_long$Year)

# t3b for leaflet map in Local Authorities (Business registrations per 10,000 adults) ####
t3b_la_long <- t3b_la %>% 
  gather("Year", "Value", 2:ncol(t3b_la))
colnames(t3b_la_long) <- c("NAME", "Year", "Value")
t3b_la_long$Year <- as.numeric(t3b_la_long$Year)
start_year_t3b_la <- min(t3b_la_long$Year)
end_year_t3b_la <- max(t3b_la_long$Year)

# t4b for leaflet map in Local Authorities (Business de-registrations per 10,000 adults) ####
t4b_la_long <- t4b_la %>% 
  gather("Year", "Value", 2:ncol(t4b_la))
colnames(t4b_la_long) <- c("NAME", "Year", "Value")
t4b_la_long$Year <- as.numeric(t4b_la_long$Year)
start_year_t4b_la <- min(t4b_la_long$Year)
end_year_t4b_la <- max(t4b_la_long$Year)

# t5b for leaflet map in Local Authorities (Business 3-year survival rate) ####
t5b_la_long <- t5b_la %>% 
  gather("Year", "Value", 2:ncol(t5b_la))
colnames(t5b_la_long) <- c("NAME", "Year", "Value")
t5b_la_long$Year <- as.numeric(t5b_la_long$Year)
start_year_t5b_la <- min(t5b_la_long$Year)
end_year_t5b_la <- max(t5b_la_long$Year)

# t8a for leaflet map in Local Authorities (International exports per head) ####
t8a_la_long <- t8a_la %>% 
  gather("Year", "Value", 2:ncol(t8a_la))
colnames(t8a_la_long) <- c("NAME", "Year", "Value")
t8a_la_long$Year <- as.numeric(t8a_la_long$Year)
start_year_t8a_la <- min(t8a_la_long$Year)
end_year_t8a_la <- max(t8a_la_long$Year)

# t9 for leaflet map in Local Authorities (BERD per head) ####
t9_la_long <- t9_la %>% 
  gather("Year", "Value", 2:ncol(t9_la))
colnames(t9_la_long) <- c("NAME", "Year", "Value")
t9_la_long$Year <- as.numeric(t9_la_long$Year)
start_year_t9_la <- min(t9_la_long$Year)
end_year_t9_la <- max(t9_la_long$Year)

# t14a for leaflet map in Local Authorities (Employment rate) ####
t14a_la_long <- t14a_la %>% 
  gather("Year", "Value", 2:ncol(t14a_la))
colnames(t14a_la_long) <- c("NAME", "Year", "Value")
t14a_la_long$Year <- as.numeric(t14a_la_long$Year)
start_year_t14a_la <- min(t14a_la_long$Year)
end_year_t14a_la <- max(t14a_la_long$Year)

# t15b for leaflet map in Local Authorities (Self-employment rate) ####
t15b_la_long <- t15b_la %>% 
  gather("Year", "Value", 2:ncol(t15b_la))
colnames(t15b_la_long) <- c("NAME", "Year", "Value")
t15b_la_long$Year <- as.numeric(t15b_la_long$Year)
start_year_t15b_la <- min(t15b_la_long$Year)
end_year_t15b_la <- max(t15b_la_long$Year)

# t17b for leaflet map in Local Authorities (Unemployment rate) ####
t17b_la_long <- t17b_la %>% 
  gather("Year", "Value", 2:ncol(t17b_la))
colnames(t17b_la_long) <- c("NAME", "Year", "Value")
t17b_la_long$Year <- as.numeric(t17b_la_long$Year)
start_year_t17b_la <- min(t17b_la_long$Year)
end_year_t17b_la <- max(t17b_la_long$Year)

# t18a for leaflet map in Local Authorities (Median gross weekly pay) ####
t18a_la_long <- t18a_la %>% 
  gather("Year", "Value", 2:ncol(t18a_la))
colnames(t18a_la_long) <- c("NAME", "Year", "Value")
t18a_la_long$Year <- as.numeric(t18a_la_long$Year)
start_year_t18a_la <- min(t18a_la_long$Year)
end_year_t18a_la <- max(t18a_la_long$Year)


# data manipulations for time series in local authorities tab ####

t1_la_wide <- t1_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t1_la_wide <- which(names(t1_la_wide) %in% c('Aberdeen City'))


t2a_la_long <- t2a_la %>% 
  gather("Year", "Value", 2:ncol(t2a_la))
colnames(t2a_la_long) <- c("NAME", "Year", "Value")
t2a_la_long$Year <- as.numeric(t2a_la_long$Year)
t2a_la_wide <- t2a_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t2a_la_wide <- which(names(t2a_la_wide) %in% c('Aberdeen City'))

t2b_la_long <- t2b_la %>% 
  gather("Year", "Value", 2:ncol(t2b_la))
colnames(t2b_la_long) <- c("NAME", "Year", "Value")
t2b_la_long$Year <- as.numeric(t2b_la_long$Year)
t2b_la_wide <- t2b_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t2b_la_wide <- which(names(t2b_la_wide) %in% c('Aberdeen City'))

t3a_la_long <- t3a_la %>% 
  gather("Year", "Value", 2:ncol(t3a_la))
colnames(t3a_la_long) <- c("NAME", "Year", "Value")
t3a_la_long$Year <- as.numeric(t3a_la_long$Year)
t3a_la_wide <- t3a_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t3a_la_wide <- which(names(t3a_la_wide) %in% c('Aberdeen City'))

t3b_la_long <- t3b_la %>% 
  gather("Year", "Value", 2:ncol(t3b_la))
colnames(t3b_la_long) <- c("NAME", "Year", "Value")
t3b_la_long$Year <- as.numeric(t3b_la_long$Year)
t3b_la_wide <- t3b_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t3b_la_wide <- which(names(t3b_la_wide) %in% c('Aberdeen City'))

t4a_la_long <- t4a_la %>% 
  gather("Year", "Value", 2:ncol(t4a_la))
colnames(t4a_la_long) <- c("NAME", "Year", "Value")
t4a_la_long$Year <- as.numeric(t4a_la_long$Year)
t4a_la_wide <- t4a_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t4a_la_wide <- which(names(t4a_la_wide) %in% c('Aberdeen City'))

t4b_la_long <- t4b_la %>% 
  gather("Year", "Value", 2:ncol(t4b_la))
colnames(t4b_la_long) <- c("NAME", "Year", "Value")
t4b_la_long$Year <- as.numeric(t4b_la_long$Year)
t4b_la_wide <- t4b_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t4b_la_wide <- which(names(t4b_la_wide) %in% c('Aberdeen City'))

t5a_la_long <- t5a_la %>% 
  gather("Year", "Value", 2:ncol(t5a_la))
colnames(t5a_la_long) <- c("NAME", "Year", "Value")
t5a_la_long$Year <- as.numeric(t5a_la_long$Year)
t5a_la_wide <- t5a_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t5a_la_wide <- which(names(t5a_la_wide) %in% c('Aberdeen City'))

t5b_la_long <- t5b_la %>% 
  gather("Year", "Value", 2:ncol(t5b_la))
colnames(t5b_la_long) <- c("NAME", "Year", "Value")
t5b_la_long$Year <- as.numeric(t5b_la_long$Year)
t5b_la_wide <- t5b_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t5b_la_wide <- which(names(t5b_la_wide) %in% c('Aberdeen City'))

t6a_la_long <- t6a_la %>% 
  gather("Year", "Value", 2:ncol(t6a_la))
colnames(t6a_la_long) <- c("NAME", "Year", "Value")
t6a_la_long$Year <- as.numeric(t6a_la_long$Year)
t6a_la_wide <- t6a_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t6a_la_wide <- which(names(t6a_la_wide) %in% c('Aberdeen City'))

t6b_la_long <- t6b_la %>% 
  gather("Year", "Value", 2:ncol(t6b_la))
colnames(t6b_la_long) <- c("NAME", "Year", "Value")
t6b_la_long$Year <- as.numeric(t6b_la_long$Year)
t6b_la_wide <- t6b_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t6b_la_wide <- which(names(t6b_la_wide) %in% c('Aberdeen City'))

t7a_la_long <- t7a_la %>% 
  gather("Year", "Value", 2:ncol(t7a_la))
colnames(t7a_la_long) <- c("NAME", "Year", "Value")
t7a_la_long$Year <- as.numeric(t7a_la_long$Year)
t7a_la_wide <- t7a_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t7a_la_wide <- which(names(t7a_la_wide) %in% c('Aberdeen City'))

t7b_la_long <- t7b_la %>% 
  gather("Year", "Value", 2:ncol(t7b_la))
colnames(t7b_la_long) <- c("NAME", "Year", "Value")
t7b_la_long$Year <- as.numeric(t7b_la_long$Year)
t7b_la_wide <- t7b_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t7b_la_wide <- which(names(t7b_la_wide) %in% c('Aberdeen City'))

t8a_la_long <- t8a_la %>% 
  gather("Year", "Value", 2:ncol(t8a_la))
colnames(t8a_la_long) <- c("NAME", "Year", "Value")
t8a_la_long$Year <- as.numeric(t8a_la_long$Year)
t8a_la_wide <- t8a_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t8a_la_wide <- which(names(t8a_la_wide) %in% c('Aberdeen City'))

t8b_la_long <- t8b_la %>% 
  gather("Year", "Value", 2:ncol(t8b_la))
colnames(t8b_la_long) <- c("NAME", "Year", "Value")
t8b_la_long$Year <- as.numeric(t8b_la_long$Year)
t8b_la_wide <- t8b_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t8b_la_wide <- which(names(t8b_la_wide) %in% c('Aberdeen City'))

t9_la_long <- t9_la %>% 
  gather("Year", "Value", 2:ncol(t9_la))
colnames(t9_la_long) <- c("NAME", "Year", "Value")
t9_la_long$Year <- as.numeric(t9_la_long$Year)
t9_la_wide <- t9_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t9_la_wide <- which(names(t9_la_wide) %in% c('Aberdeen City'))

t10_la_long <- t10_la %>% 
  gather("Year", "Value", 2:ncol(t10_la))
colnames(t10_la_long) <- c("NAME", "Year", "Value")
t10_la_long$Year <- as.numeric(t10_la_long$Year)
t10_la_wide <- t10_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t10_la_wide <- which(names(t10_la_wide) %in% c('Aberdeen City'))

t11_la_long <- t11_la %>% 
  gather("Year", "Value", 2:ncol(t11_la))
colnames(t11_la_long) <- c("NAME", "Year", "Value")
t11_la_long$Year <- as.numeric(t11_la_long$Year)
t11_la_wide <- t11_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t11_la_wide <- which(names(t11_la_wide) %in% c('Aberdeen City'))

t12_la_long <- t12_la %>% 
  gather("Year", "Value", 2:ncol(t12_la))
colnames(t12_la_long) <- c("NAME", "Year", "Value")
t12_la_long$Year <- as.numeric(t12_la_long$Year)
t12_la_wide <- t12_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t12_la_wide <- which(names(t12_la_wide) %in% c('Aberdeen City'))

t13_la_long <- t13_la %>% 
  gather("Year", "Value", 2:ncol(t13_la))
colnames(t13_la_long) <- c("NAME", "Year", "Value")
t13_la_long$Year <- as.numeric(t13_la_long$Year)
t13_la_wide <- t13_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t13_la_wide <- which(names(t13_la_wide) %in% c('Aberdeen City'))

t14a_la_long <- t14a_la %>% 
  gather("Year", "Value", 2:ncol(t14a_la))
colnames(t14a_la_long) <- c("NAME", "Year", "Value")
t14a_la_long$Year <- as.numeric(t14a_la_long$Year)
t14a_la_wide <- t14a_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t14a_la_wide <- which(names(t14a_la_wide) %in% c('Aberdeen City'))

t14b_la_long <- t14b_la %>% 
  gather("Year", "Value", 2:ncol(t14b_la))
colnames(t14b_la_long) <- c("NAME", "Year", "Value")
t14b_la_long$Year <- as.numeric(t14b_la_long$Year)
t14b_la_wide <- t14b_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t14b_la_wide <- which(names(t14b_la_wide) %in% c('Aberdeen City'))

t15a_la_long <- t15a_la %>% 
  gather("Year", "Value", 2:ncol(t15a_la))
colnames(t15a_la_long) <- c("NAME", "Year", "Value")
t15a_la_long$Year <- as.numeric(t15a_la_long$Year)
t15a_la_wide <- t15a_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t15a_la_wide <- which(names(t15a_la_wide) %in% c('Aberdeen City'))

t15b_la_long <- t15b_la %>% 
  gather("Year", "Value", 2:ncol(t15b_la))
colnames(t15b_la_long) <- c("NAME", "Year", "Value")
t15b_la_long$Year <- as.numeric(t15b_la_long$Year)
t15b_la_wide <- t15b_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t15b_la_wide <- which(names(t15b_la_wide) %in% c('Aberdeen City'))

t15c_la_long <- t15c_la %>% 
  gather("Year", "Value", 2:ncol(t15c_la))
colnames(t15c_la_long) <- c("NAME", "Year", "Value")
t15c_la_long$Year <- as.numeric(t15c_la_long$Year)
t15c_la_wide <- t15c_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t15c_la_wide <- which(names(t15c_la_wide) %in% c('Aberdeen City'))

t16a_la_long <- t16a_la %>% 
  gather("Year", "Value", 2:ncol(t16a_la))
colnames(t16a_la_long) <- c("NAME", "Year", "Value")
t16a_la_long$Year <- as.numeric(t16a_la_long$Year)
t16a_la_wide <- t16a_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t16a_la_wide <- which(names(t16a_la_wide) %in% c('Aberdeen City'))

t16b_la_long <- t16b_la %>% 
  gather("Year", "Value", 2:ncol(t16b_la))
colnames(t16b_la_long) <- c("NAME", "Year", "Value")
t16b_la_long$Year <- as.numeric(t16b_la_long$Year)
t16b_la_wide <- t16b_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t16b_la_wide <- which(names(t16b_la_wide) %in% c('Aberdeen City'))

t16c_la_long <- t16c_la %>% 
  gather("Year", "Value", 2:ncol(t16c_la))
colnames(t16c_la_long) <- c("NAME", "Year", "Value")
t16c_la_long$Year <- as.numeric(t16c_la_long$Year)
t16c_la_wide <- t16c_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t16c_la_wide <- which(names(t16c_la_wide) %in% c('Aberdeen City'))

t17a_la_long <- t17a_la %>% 
  gather("Year", "Value", 2:ncol(t17a_la))
colnames(t17a_la_long) <- c("NAME", "Year", "Value")
t17a_la_long$Year <- as.numeric(t17a_la_long$Year)
t17a_la_wide <- t17a_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t17a_la_wide <- which(names(t17a_la_wide) %in% c('Aberdeen City'))

t17b_la_long <- t17b_la %>% 
  gather("Year", "Value", 2:ncol(t17b_la))
colnames(t17b_la_long) <- c("NAME", "Year", "Value")
t17b_la_long$Year <- as.numeric(t17b_la_long$Year)
t17b_la_wide <- t17b_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t17b_la_wide <- which(names(t17b_la_wide) %in% c('Aberdeen City'))

t17c_la_long <- t17c_la %>% 
  gather("Year", "Value", 2:ncol(t17c_la))
colnames(t17c_la_long) <- c("NAME", "Year", "Value")
t17c_la_long$Year <- as.numeric(t17c_la_long$Year)
t17c_la_wide <- t17c_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t17c_la_wide <- which(names(t17c_la_wide) %in% c('Aberdeen City'))

t18a_la_long <- t18a_la %>% 
  gather("Year", "Value", 2:ncol(t18a_la))
colnames(t18a_la_long) <- c("NAME", "Year", "Value")
t18a_la_long$Year <- as.numeric(t18a_la_long$Year)
t18a_la_wide <- t18a_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t18a_la_wide <- which(names(t18a_la_wide) %in% c('Aberdeen City'))

t18b_la_long <- t18b_la %>% 
  gather("Year", "Value", 2:ncol(t18b_la))
colnames(t18b_la_long) <- c("NAME", "Year", "Value")
t18b_la_long$Year <- as.numeric(t18b_la_long$Year)
t18b_la_wide <- t18b_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t18b_la_wide <- which(names(t18b_la_wide) %in% c('Aberdeen City'))

t19a_la_long <- t19a_la %>% 
  gather("Year", "Value", 2:ncol(t19a_la))
colnames(t19a_la_long) <- c("NAME", "Year", "Value")
t19a_la_long$Year <- as.numeric(t19a_la_long$Year)
t19a_la_wide <- t19a_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t19a_la_wide <- which(names(t19a_la_wide) %in% c('Aberdeen City'))

t19b_la_long <- t19b_la %>% 
  gather("Year", "Value", 2:ncol(t19b_la))
colnames(t19b_la_long) <- c("NAME", "Year", "Value")
t19b_la_long$Year <- as.numeric(t19b_la_long$Year)
t19b_la_wide <- t19b_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t19b_la_wide <- which(names(t19b_la_wide) %in% c('Aberdeen City'))

t20_la_long <- t20_la %>% 
  gather("Year", "Value", 2:ncol(t20_la))
colnames(t20_la_long) <- c("NAME", "Year", "Value")
t20_la_long$Year <- as.numeric(t20_la_long$Year)
t20_la_wide <- t20_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t20_la_wide <- which(names(t20_la_wide) %in% c('Aberdeen City'))

t21_la_long <- t21_la %>% 
  gather("Year", "Value", 2:ncol(t21_la))
colnames(t21_la_long) <- c("NAME", "Year", "Value")
t21_la_long$Year <- as.numeric(t21_la_long$Year)
t21_la_wide <- t21_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t21_la_wide <- which(names(t21_la_wide) %in% c('Aberdeen City'))

t22_la_long <- t22_la %>% 
  gather("Year", "Value", 2:ncol(t22_la))
colnames(t22_la_long) <- c("NAME", "Year", "Value")
t22_la_long$Year <- as.numeric(t22_la_long$Year)
t22_la_wide <- t22_la_long %>% 
  spread("NAME", "Value", 2:3)
positions_t22_la_wide <- which(names(t22_la_wide) %in% c('Aberdeen City'))

# REGIONS ####

# t1 for barchart in Regions (GVA per head) ####
t1_rg <- read.csv("./www/regions/t1.csv", check.names=FALSE)
t1_rg_long <- t1_rg %>% 
  gather("Year", "Value", 2:ncol(t1_rg))
colnames(t1_rg_long) <- c("NAME", "Year", "Value")
t1_rg_long$Year <- as.numeric(t1_rg_long$Year)
start_year_t1_rg <- min(t1_rg_long$Year)
end_year_t1_rg <- max(t1_rg_long$Year)

# t3b for barchart in Regions (Business registrations per 10,000 adults) ####
t3b_rg <- read.csv("./www/regions/t3b.csv", check.names=FALSE)
t3b_rg_long <- t3b_rg %>% 
  gather("Year", "Value", 2:ncol(t3b_rg))
colnames(t3b_rg_long) <- c("NAME", "Year", "Value")
t3b_rg_long$Year <- as.numeric(t3b_rg_long$Year)
start_year_t3b_rg <- min(t3b_rg_long$Year)
end_year_t3b_rg <- max(t3b_rg_long$Year)

# t4b for barchart in Regions (Business de-registrations per 10,000 adults) ####
t4b_rg <- read.csv("./www/regions/t4b.csv", check.names=FALSE)
t4b_rg_long <- t4b_rg %>% 
  gather("Year", "Value", 2:ncol(t4b_rg))
colnames(t4b_rg_long) <- c("NAME", "Year", "Value")
t4b_rg_long$Year <- as.numeric(t4b_rg_long$Year)
start_year_t4b_rg <- min(t4b_rg_long$Year)
end_year_t4b_rg <- max(t4b_rg_long$Year)

# t5b for barchart in Regions (Business 3-year survival rate) ####
t5b_rg <- read.csv("./www/regions/t5b.csv", check.names=FALSE)
t5b_rg_long <- t5b_rg %>% 
  gather("Year", "Value", 2:ncol(t5b_rg))
colnames(t5b_rg_long) <- c("NAME", "Year", "Value")
t5b_rg_long$Year <- as.numeric(t5b_rg_long$Year)
start_year_t5b_rg <- min(t5b_rg_long$Year)
end_year_t5b_rg <- max(t5b_rg_long$Year)

# t8a for barchart in Regions (International exports per head) ####
t8a_rg <- read.csv("./www/regions/t8a.csv", check.names=FALSE)
t8a_rg_long <- t8a_rg %>% 
  gather("Year", "Value", 2:ncol(t8a_rg))
colnames(t8a_rg_long) <- c("NAME", "Year", "Value")
t8a_rg_long$Year <- as.numeric(t8a_rg_long$Year)
start_year_t8a_rg <- min(t8a_rg_long$Year)
end_year_t8a_rg <- max(t8a_rg_long$Year)

# t9 for barchart in Regions (BERD per head) ####
t9_rg <- read.csv("./www/regions/t9.csv", check.names=FALSE)
t9_rg_long <- t9_rg %>% 
  gather("Year", "Value", 2:ncol(t9_rg))
colnames(t9_rg_long) <- c("NAME", "Year", "Value")
t9_rg_long$Year <- as.numeric(t9_rg_long$Year)
start_year_t9_rg <- min(t9_rg_long$Year)
end_year_t9_rg <- max(t9_rg_long$Year)

# t14a for barchart in Regions (Employment rate) ####
t14a_rg <- read.csv("./www/regions/t14a.csv", check.names=FALSE)
t14a_rg_long <- t14a_rg %>% 
  gather("Year", "Value", 2:ncol(t14a_rg))
colnames(t14a_rg_long) <- c("NAME", "Year", "Value")
t14a_rg_long$Year <- as.numeric(t14a_rg_long$Year)
start_year_t14a_rg <- min(t14a_rg_long$Year)
end_year_t14a_rg <- max(t14a_rg_long$Year)

# t15b for barchart in Regions (Self-employment rate) ####
t15b_rg <- read.csv("./www/regions/t15b.csv", check.names=FALSE)
t15b_rg_long <- t15b_rg %>% 
  gather("Year", "Value", 2:ncol(t15b_rg))
colnames(t15b_rg_long) <- c("NAME", "Year", "Value")
t15b_rg_long$Year <- as.numeric(t15b_rg_long$Year)
start_year_t15b_rg <- min(t15b_rg_long$Year)
end_year_t15b_rg <- max(t15b_rg_long$Year)

# t17b for barchart in Regions (Unemployment rate) ####
t17b_rg <- read.csv("./www/regions/t17b.csv", check.names=FALSE)
t17b_rg_long <- t17b_rg %>% 
  gather("Year", "Value", 2:ncol(t17b_rg))
colnames(t17b_rg_long) <- c("NAME", "Year", "Value")
t17b_rg_long$Year <- as.numeric(t17b_rg_long$Year)
start_year_t17b_rg <- min(t17b_rg_long$Year)
end_year_t17b_rg <- max(t17b_rg_long$Year)

# t18a for barchart in Regions (Median gross weekly pay) ####
t18a_rg <- read.csv("./www/regions/t18a.csv", check.names=FALSE)
t18a_rg_long <- t18a_rg %>% 
  gather("Year", "Value", 2:ncol(t18a_rg))
colnames(t18a_rg_long) <- c("NAME", "Year", "Value")
t18a_rg_long$Year <- as.numeric(t18a_rg_long$Year)
start_year_t18a_rg <- min(t18a_rg_long$Year)
end_year_t18a_rg <- max(t18a_rg_long$Year)


t1_rg <- read.csv("./www/regions/t1.csv", check.names=FALSE)
t2a_rg <- read.csv("./www/regions/t2a.csv", check.names=FALSE)
t2b_rg <- read.csv("./www/regions/t2b.csv", check.names=FALSE)
t3a_rg <- read.csv("./www/regions/t3a.csv", check.names=FALSE)
t3b_rg <- read.csv("./www/regions/t3b.csv", check.names=FALSE)
t4a_rg <- read.csv("./www/regions/t4a.csv", check.names=FALSE)
t4b_rg <- read.csv("./www/regions/t4b.csv", check.names=FALSE)
t5a_rg <- read.csv("./www/regions/t5a.csv", check.names=FALSE)
t5b_rg <- read.csv("./www/regions/t5b.csv", check.names=FALSE)
t6a_rg <- read.csv("./www/regions/t6a.csv", check.names=FALSE)
t6b_rg <- read.csv("./www/regions/t6b.csv", check.names=FALSE)
t7a_rg <- read.csv("./www/regions/t7a.csv", check.names=FALSE)
t7b_rg <- read.csv("./www/regions/t7b.csv", check.names=FALSE)
t8a_rg <- read.csv("./www/regions/t8a.csv", check.names=FALSE)
t8b_rg <- read.csv("./www/regions/t8b.csv", check.names=FALSE)
t9_rg <- read.csv("./www/regions/t9.csv", check.names=FALSE)
t10_rg <- read.csv("./www/regions/t10.csv", check.names=FALSE)
t11_rg <- read.csv("./www/regions/t11.csv", check.names=FALSE)
t12_rg <- read.csv("./www/regions/t12.csv", check.names=FALSE)
t13_rg <- read.csv("./www/regions/t13.csv", check.names=FALSE)
t14a_rg <- read.csv("./www/regions/t14a.csv", check.names=FALSE)
t14b_rg <- read.csv("./www/regions/t14b.csv", check.names=FALSE)
t15a_rg <- read.csv("./www/regions/t15a.csv", check.names=FALSE)
t15b_rg <- read.csv("./www/regions/t15b.csv", check.names=FALSE)
t15c_rg <- read.csv("./www/regions/t15c.csv", check.names=FALSE)
t16a_rg <- read.csv("./www/regions/t16a.csv", check.names=FALSE)
t16b_rg <- read.csv("./www/regions/t16b.csv", check.names=FALSE)
t16c_rg <- read.csv("./www/regions/t16c.csv", check.names=FALSE)
t17a_rg <- read.csv("./www/regions/t17a.csv", check.names=FALSE)
t17b_rg <- read.csv("./www/regions/t17b.csv", check.names=FALSE)
t17c_rg <- read.csv("./www/regions/t17c.csv", check.names=FALSE)
t18a_rg <- read.csv("./www/regions/t18a.csv", check.names=FALSE)
t18b_rg <- read.csv("./www/regions/t18b.csv", check.names=FALSE)
t19a_rg <- read.csv("./www/regions/t19a.csv", check.names=FALSE)
t19b_rg <- read.csv("./www/regions/t19b.csv", check.names=FALSE)
t20_rg <- read.csv("./www/regions/t20.csv", check.names=FALSE)
t21_rg <- read.csv("./www/regions/t21.csv", check.names=FALSE)
t22_rg <- read.csv("./www/regions/t22.csv", check.names=FALSE)
t22_rg <- read.csv("./www/regions/t22.csv", check.names=FALSE)

# data manipulations for time series in regions tab ####

t1_rg_wide <- t1_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t1_rg_wide <- which(names(t1_rg_wide) %in% c('Aberdeen City Region'))

t2a_rg_long <- t2a_rg %>% 
  gather("Year", "Value", 2:ncol(t2a_rg))
colnames(t2a_rg_long) <- c("NAME", "Year", "Value")
t2a_rg_long$Year <- as.numeric(t2a_rg_long$Year)
t2a_rg_wide <- t2a_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t2a_rg_wide <- which(names(t2a_rg_wide) %in% c('Aberdeen City Region'))

t2b_rg_long <- t2b_rg %>% 
  gather("Year", "Value", 2:ncol(t2b_rg))
colnames(t2b_rg_long) <- c("NAME", "Year", "Value")
t2b_rg_long$Year <- as.numeric(t2b_rg_long$Year)
t2b_rg_wide <- t2b_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t2b_rg_wide <- which(names(t2b_rg_wide) %in% c('Aberdeen City Region'))

t3a_rg_long <- t3a_rg %>% 
  gather("Year", "Value", 2:ncol(t3a_rg))
colnames(t3a_rg_long) <- c("NAME", "Year", "Value")
t3a_rg_long$Year <- as.numeric(t3a_rg_long$Year)
t3a_rg_wide <- t3a_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t3a_rg_wide <- which(names(t3a_rg_wide) %in% c('Aberdeen City Region'))

t3b_rg_long <- t3b_rg %>% 
  gather("Year", "Value", 2:ncol(t3b_rg))
colnames(t3b_rg_long) <- c("NAME", "Year", "Value")
t3b_rg_long$Year <- as.numeric(t3b_rg_long$Year)
t3b_rg_wide <- t3b_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t3b_rg_wide <- which(names(t3b_rg_wide) %in% c('Aberdeen City Region'))

t4a_rg_long <- t4a_rg %>% 
  gather("Year", "Value", 2:ncol(t4a_rg))
colnames(t4a_rg_long) <- c("NAME", "Year", "Value")
t4a_rg_long$Year <- as.numeric(t4a_rg_long$Year)
t4a_rg_wide <- t4a_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t4a_rg_wide <- which(names(t4a_rg_wide) %in% c('Aberdeen City Region'))

t4b_rg_long <- t4b_rg %>% 
  gather("Year", "Value", 2:ncol(t4b_rg))
colnames(t4b_rg_long) <- c("NAME", "Year", "Value")
t4b_rg_long$Year <- as.numeric(t4b_rg_long$Year)
t4b_rg_wide <- t4b_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t4b_rg_wide <- which(names(t4b_rg_wide) %in% c('Aberdeen City Region'))

t5a_rg_long <- t5a_rg %>% 
  gather("Year", "Value", 2:ncol(t5a_rg))
colnames(t5a_rg_long) <- c("NAME", "Year", "Value")
t5a_rg_long$Year <- as.numeric(t5a_rg_long$Year)
t5a_rg_wide <- t5a_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t5a_rg_wide <- which(names(t5a_rg_wide) %in% c('Aberdeen City Region'))

t5b_rg_long <- t5b_rg %>% 
  gather("Year", "Value", 2:ncol(t5b_rg))
colnames(t5b_rg_long) <- c("NAME", "Year", "Value")
t5b_rg_long$Year <- as.numeric(t5b_rg_long$Year)
t5b_rg_wide <- t5b_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t5b_rg_wide <- which(names(t5b_rg_wide) %in% c('Aberdeen City Region'))

t6a_rg_long <- t6a_rg %>% 
  gather("Year", "Value", 2:ncol(t6a_rg))
colnames(t6a_rg_long) <- c("NAME", "Year", "Value")
t6a_rg_long$Year <- as.numeric(t6a_rg_long$Year)
t6a_rg_wide <- t6a_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t6a_rg_wide <- which(names(t6a_rg_wide) %in% c('Aberdeen City Region'))

t6b_rg_long <- t6b_rg %>% 
  gather("Year", "Value", 2:ncol(t6b_rg))
colnames(t6b_rg_long) <- c("NAME", "Year", "Value")
t6b_rg_long$Year <- as.numeric(t6b_rg_long$Year)
t6b_rg_wide <- t6b_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t6b_rg_wide <- which(names(t6b_rg_wide) %in% c('Aberdeen City Region'))

t7a_rg_long <- t7a_rg %>% 
  gather("Year", "Value", 2:ncol(t7a_rg))
colnames(t7a_rg_long) <- c("NAME", "Year", "Value")
t7a_rg_long$Year <- as.numeric(t7a_rg_long$Year)
t7a_rg_wide <- t7a_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t7a_rg_wide <- which(names(t7a_rg_wide) %in% c('Aberdeen City Region'))

t7b_rg_long <- t7b_rg %>% 
  gather("Year", "Value", 2:ncol(t7b_rg))
colnames(t7b_rg_long) <- c("NAME", "Year", "Value")
t7b_rg_long$Year <- as.numeric(t7b_rg_long$Year)
t7b_rg_wide <- t7b_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t7b_rg_wide <- which(names(t7b_rg_wide) %in% c('Aberdeen City Region'))

t8a_rg_long <- t8a_rg %>% 
  gather("Year", "Value", 2:ncol(t8a_rg))
colnames(t8a_rg_long) <- c("NAME", "Year", "Value")
t8a_rg_long$Year <- as.numeric(t8a_rg_long$Year)
t8a_rg_wide <- t8a_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t8a_rg_wide <- which(names(t8a_rg_wide) %in% c('Aberdeen City Region'))

t8b_rg_long <- t8b_rg %>% 
  gather("Year", "Value", 2:ncol(t8b_rg))
colnames(t8b_rg_long) <- c("NAME", "Year", "Value")
t8b_rg_long$Year <- as.numeric(t8b_rg_long$Year)
t8b_rg_wide <- t8b_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t8b_rg_wide <- which(names(t8b_rg_wide) %in% c('Aberdeen City Region'))

t9_rg_long <- t9_rg %>% 
  gather("Year", "Value", 2:ncol(t9_rg))
colnames(t9_rg_long) <- c("NAME", "Year", "Value")
t9_rg_long$Year <- as.numeric(t9_rg_long$Year)
t9_rg_wide <- t9_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t9_rg_wide <- which(names(t9_rg_wide) %in% c('Aberdeen City Region'))

t10_rg_long <- t10_rg %>% 
  gather("Year", "Value", 2:ncol(t10_rg))
colnames(t10_rg_long) <- c("NAME", "Year", "Value")
t10_rg_long$Year <- as.numeric(t10_rg_long$Year)
t10_rg_wide <- t10_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t10_rg_wide <- which(names(t10_rg_wide) %in% c('Aberdeen City Region'))

t11_rg_long <- t11_rg %>% 
  gather("Year", "Value", 2:ncol(t11_rg))
colnames(t11_rg_long) <- c("NAME", "Year", "Value")
t11_rg_long$Year <- as.numeric(t11_rg_long$Year)
t11_rg_wide <- t11_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t11_rg_wide <- which(names(t11_rg_wide) %in% c('Aberdeen City Region'))

t12_rg_long <- t12_rg %>% 
  gather("Year", "Value", 2:ncol(t12_rg))
colnames(t12_rg_long) <- c("NAME", "Year", "Value")
t12_rg_long$Year <- as.numeric(t12_rg_long$Year)
t12_rg_wide <- t12_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t12_rg_wide <- which(names(t12_rg_wide) %in% c('Aberdeen City Region'))

t13_rg_long <- t13_rg %>% 
  gather("Year", "Value", 2:ncol(t13_rg))
colnames(t13_rg_long) <- c("NAME", "Year", "Value")
t13_rg_long$Year <- as.numeric(t13_rg_long$Year)
t13_rg_wide <- t13_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t13_rg_wide <- which(names(t13_rg_wide) %in% c('Aberdeen City Region'))

t14a_rg_long <- t14a_rg %>% 
  gather("Year", "Value", 2:ncol(t14a_rg))
colnames(t14a_rg_long) <- c("NAME", "Year", "Value")
t14a_rg_long$Year <- as.numeric(t14a_rg_long$Year)
t14a_rg_wide <- t14a_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t14a_rg_wide <- which(names(t14a_rg_wide) %in% c('Aberdeen City Region'))

t14b_rg_long <- t14b_rg %>% 
  gather("Year", "Value", 2:ncol(t14b_rg))
colnames(t14b_rg_long) <- c("NAME", "Year", "Value")
t14b_rg_long$Year <- as.numeric(t14b_rg_long$Year)
t14b_rg_wide <- t14b_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t14b_rg_wide <- which(names(t14b_rg_wide) %in% c('Aberdeen City Region'))

t15a_rg_long <- t15a_rg %>% 
  gather("Year", "Value", 2:ncol(t15a_rg))
colnames(t15a_rg_long) <- c("NAME", "Year", "Value")
t15a_rg_long$Year <- as.numeric(t15a_rg_long$Year)
t15a_rg_wide <- t15a_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t15a_rg_wide <- which(names(t15a_rg_wide) %in% c('Aberdeen City Region'))

t15b_rg_long <- t15b_rg %>% 
  gather("Year", "Value", 2:ncol(t15b_rg))
colnames(t15b_rg_long) <- c("NAME", "Year", "Value")
t15b_rg_long$Year <- as.numeric(t15b_rg_long$Year)
t15b_rg_wide <- t15b_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t15b_rg_wide <- which(names(t15b_rg_wide) %in% c('Aberdeen City Region'))

t15c_rg_long <- t15c_rg %>% 
  gather("Year", "Value", 2:ncol(t15c_rg))
colnames(t15c_rg_long) <- c("NAME", "Year", "Value")
t15c_rg_long$Year <- as.numeric(t15c_rg_long$Year)
t15c_rg_wide <- t15c_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t15c_rg_wide <- which(names(t15c_rg_wide) %in% c('Aberdeen City Region'))

t16a_rg_long <- t16a_rg %>% 
  gather("Year", "Value", 2:ncol(t16a_rg))
colnames(t16a_rg_long) <- c("NAME", "Year", "Value")
t16a_rg_long$Year <- as.numeric(t16a_rg_long$Year)
t16a_rg_wide <- t16a_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t16a_rg_wide <- which(names(t16a_rg_wide) %in% c('Aberdeen City Region'))

t16b_rg_long <- t16b_rg %>% 
  gather("Year", "Value", 2:ncol(t16b_rg))
colnames(t16b_rg_long) <- c("NAME", "Year", "Value")
t16b_rg_long$Year <- as.numeric(t16b_rg_long$Year)
t16b_rg_wide <- t16b_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t16b_rg_wide <- which(names(t16b_rg_wide) %in% c('Aberdeen City Region'))

t16c_rg_long <- t16c_rg %>% 
  gather("Year", "Value", 2:ncol(t16c_rg))
colnames(t16c_rg_long) <- c("NAME", "Year", "Value")
t16c_rg_long$Year <- as.numeric(t16c_rg_long$Year)
t16c_rg_wide <- t16c_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t16c_rg_wide <- which(names(t16c_rg_wide) %in% c('Aberdeen City Region'))

t17a_rg_long <- t17a_rg %>% 
  gather("Year", "Value", 2:ncol(t17a_rg))
colnames(t17a_rg_long) <- c("NAME", "Year", "Value")
t17a_rg_long$Year <- as.numeric(t17a_rg_long$Year)
t17a_rg_wide <- t17a_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t17a_rg_wide <- which(names(t17a_rg_wide) %in% c('Aberdeen City Region'))

t17b_rg_long <- t17b_rg %>% 
  gather("Year", "Value", 2:ncol(t17b_rg))
colnames(t17b_rg_long) <- c("NAME", "Year", "Value")
t17b_rg_long$Year <- as.numeric(t17b_rg_long$Year)
t17b_rg_wide <- t17b_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t17b_rg_wide <- which(names(t17b_rg_wide) %in% c('Aberdeen City Region'))

t17c_rg_long <- t17c_rg %>% 
  gather("Year", "Value", 2:ncol(t17c_rg))
colnames(t17c_rg_long) <- c("NAME", "Year", "Value")
t17c_rg_long$Year <- as.numeric(t17c_rg_long$Year)
t17c_rg_wide <- t17c_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t17c_rg_wide <- which(names(t17c_rg_wide) %in% c('Aberdeen City Region'))

t18a_rg_long <- t18a_rg %>% 
  gather("Year", "Value", 2:ncol(t18a_rg))
colnames(t18a_rg_long) <- c("NAME", "Year", "Value")
t18a_rg_long$Year <- as.numeric(t18a_rg_long$Year)
t18a_rg_wide <- t18a_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t18a_rg_wide <- which(names(t18a_rg_wide) %in% c('Aberdeen City Region'))

t18b_rg_long <- t18b_rg %>% 
  gather("Year", "Value", 2:ncol(t18b_rg))
colnames(t18b_rg_long) <- c("NAME", "Year", "Value")
t18b_rg_long$Year <- as.numeric(t18b_rg_long$Year)
t18b_rg_wide <- t18b_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t18b_rg_wide <- which(names(t18b_rg_wide) %in% c('Aberdeen City Region'))

t19a_rg_long <- t19a_rg %>% 
  gather("Year", "Value", 2:ncol(t19a_rg))
colnames(t19a_rg_long) <- c("NAME", "Year", "Value")
t19a_rg_long$Year <- as.numeric(t19a_rg_long$Year)
t19a_rg_wide <- t19a_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t19a_rg_wide <- which(names(t19a_rg_wide) %in% c('Aberdeen City Region'))

t19b_rg_long <- t19b_rg %>% 
  gather("Year", "Value", 2:ncol(t19b_rg))
colnames(t19b_rg_long) <- c("NAME", "Year", "Value")
t19b_rg_long$Year <- as.numeric(t19b_rg_long$Year)
t19b_rg_wide <- t19b_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t19b_rg_wide <- which(names(t19b_rg_wide) %in% c('Aberdeen City Region'))

t20_rg_long <- t20_rg %>% 
  gather("Year", "Value", 2:ncol(t20_rg))
colnames(t20_rg_long) <- c("NAME", "Year", "Value")
t20_rg_long$Year <- as.numeric(t20_rg_long$Year)
t20_rg_wide <- t20_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t20_rg_wide <- which(names(t20_rg_wide) %in% c('Aberdeen City Region'))

t21_rg_long <- t21_rg %>% 
  gather("Year", "Value", 2:ncol(t21_rg))
colnames(t21_rg_long) <- c("NAME", "Year", "Value")
t21_rg_long$Year <- as.numeric(t21_rg_long$Year)
t21_rg_wide <- t21_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t21_rg_wide <- which(names(t21_rg_wide) %in% c('Aberdeen City Region'))

t22_rg_long <- t22_rg %>% 
  gather("Year", "Value", 2:ncol(t22_rg))
colnames(t22_rg_long) <- c("NAME", "Year", "Value")
t22_rg_long$Year <- as.numeric(t22_rg_long$Year)
t22_rg_wide <- t22_rg_long %>% 
  spread("NAME", "Value", 2:3)
positions_t22_rg_wide <- which(names(t22_rg_wide) %in% c('Aberdeen City Region'))


summary_tool_r <- read.csv("./www/regions/summary_tool.csv", check.names=FALSE)

# RURAL AND URBAN ####

# t1 for barchart in Rural and Urban (GVA per head) ####
t1_ru <- read.csv("./www/rural_and_urban/t1.csv", check.names=FALSE)
t1_ru_long <- t1_ru %>% 
  gather("Year", "Value", 2:ncol(t1_ru))
colnames(t1_ru_long) <- c("NAME", "Year", "Value")
t1_ru_long$Year <- as.numeric(t1_ru_long$Year)
start_year_t1_ru <- min(t1_ru_long$Year)
end_year_t1_ru <- max(t1_ru_long$Year)

# t3b for barchart in Rural and Urban (Business registrations per 10,000 adults) ####
t3b_ru <- read.csv("./www/rural_and_urban/t3b.csv", check.names=FALSE)
t3b_ru_long <- t3b_ru %>% 
  gather("Year", "Value", 2:ncol(t3b_ru))
colnames(t3b_ru_long) <- c("NAME", "Year", "Value")
t3b_ru_long$Year <- as.numeric(t3b_ru_long$Year)
start_year_t3b_ru <- min(t3b_ru_long$Year)
end_year_t3b_ru <- max(t3b_ru_long$Year)

# t4b for barchart in Rural and Urban (Business de-registrations per 10,000 adults) ####
t4b_ru <- read.csv("./www/rural_and_urban/t4b.csv", check.names=FALSE)
t4b_ru_long <- t4b_ru %>% 
  gather("Year", "Value", 2:ncol(t4b_ru))
colnames(t4b_ru_long) <- c("NAME", "Year", "Value")
t4b_ru_long$Year <- as.numeric(t4b_ru_long$Year)
start_year_t4b_ru <- min(t4b_ru_long$Year)
end_year_t4b_ru <- max(t4b_ru_long$Year)

# t5b for barchart in Rural and Urban (Business 3-year survival rate) ####
t5b_ru <- read.csv("./www/rural_and_urban/t5b.csv", check.names=FALSE)
t5b_ru_long <- t5b_ru %>% 
  gather("Year", "Value", 2:ncol(t5b_ru))
colnames(t5b_ru_long) <- c("NAME", "Year", "Value")
t5b_ru_long$Year <- as.numeric(t5b_ru_long$Year)
start_year_t5b_ru <- min(t5b_ru_long$Year)
end_year_t5b_ru <- max(t5b_ru_long$Year)

# t8a for barchart in Rural and Urban (International exports per head) ####
t8a_ru <- read.csv("./www/rural_and_urban/t8a.csv", check.names=FALSE)
t8a_ru_long <- t8a_ru %>% 
  gather("Year", "Value", 2:ncol(t8a_ru))
colnames(t8a_ru_long) <- c("NAME", "Year", "Value")
t8a_ru_long$Year <- as.numeric(t8a_ru_long$Year)
start_year_t8a_ru <- min(t8a_ru_long$Year)
end_year_t8a_ru <- max(t8a_ru_long$Year)

# t9 for barchart in Rural and Urban (BERD per head) ####
t9_ru <- read.csv("./www/rural_and_urban/t9.csv", check.names=FALSE)
t9_ru_long <- t9_ru %>% 
  gather("Year", "Value", 2:ncol(t9_ru))
colnames(t9_ru_long) <- c("NAME", "Year", "Value")
t9_ru_long$Year <- as.numeric(t9_ru_long$Year)
start_year_t9_ru <- min(t9_ru_long$Year)
end_year_t9_ru <- max(t9_ru_long$Year)

# t14a for barchart in Rural and Urban (Employment rate) ####
t14a_ru <- read.csv("./www/rural_and_urban/t14a.csv", check.names=FALSE)
t14a_ru_long <- t14a_ru %>% 
  gather("Year", "Value", 2:ncol(t14a_ru))
colnames(t14a_ru_long) <- c("NAME", "Year", "Value")
t14a_ru_long$Year <- as.numeric(t14a_ru_long$Year)
start_year_t14a_ru <- min(t14a_ru_long$Year)
end_year_t14a_ru <- max(t14a_ru_long$Year)

# t15b for barchart in Rural and Urban (Self-employment rate) ####
t15b_ru <- read.csv("./www/rural_and_urban/t15b.csv", check.names=FALSE)
t15b_ru_long <- t15b_ru %>% 
  gather("Year", "Value", 2:ncol(t15b_ru))
colnames(t15b_ru_long) <- c("NAME", "Year", "Value")
t15b_ru_long$Year <- as.numeric(t15b_ru_long$Year)
start_year_t15b_ru <- min(t15b_ru_long$Year)
end_year_t15b_ru <- max(t15b_ru_long$Year)

# t17b for barchart in Rural and Urban (Unemployment rate) ####
t17b_ru <- read.csv("./www/rural_and_urban/t17b.csv", check.names=FALSE)
t17b_ru_long <- t17b_ru %>% 
  gather("Year", "Value", 2:ncol(t17b_ru))
colnames(t17b_ru_long) <- c("NAME", "Year", "Value")
t17b_ru_long$Year <- as.numeric(t17b_ru_long$Year)
start_year_t17b_ru <- min(t17b_ru_long$Year)
end_year_t17b_ru <- max(t17b_ru_long$Year)

# t18a for barchart in Rural and Urban (Median gross weekly pay) ####
t18a_ru <- read.csv("./www/rural_and_urban/t18a.csv", check.names=FALSE)
t18a_ru_long <- t18a_ru %>% 
  gather("Year", "Value", 2:ncol(t18a_ru))
colnames(t18a_ru_long) <- c("NAME", "Year", "Value")
t18a_ru_long$Year <- as.numeric(t18a_ru_long$Year)
start_year_t18a_ru <- min(t18a_ru_long$Year)
end_year_t18a_ru <- max(t18a_ru_long$Year)

t1_ru <- read.csv("./www/rural_and_urban/t1.csv", check.names=FALSE)
t2a_ru <- read.csv("./www/rural_and_urban/t2a.csv", check.names=FALSE)
t2b_ru <- read.csv("./www/rural_and_urban/t2b.csv", check.names=FALSE)
t3a_ru <- read.csv("./www/rural_and_urban/t3a.csv", check.names=FALSE)
t3b_ru <- read.csv("./www/rural_and_urban/t3b.csv", check.names=FALSE)
t4a_ru <- read.csv("./www/rural_and_urban/t4a.csv", check.names=FALSE)
t4b_ru <- read.csv("./www/rural_and_urban/t4b.csv", check.names=FALSE)
t5a_ru <- read.csv("./www/rural_and_urban/t5a.csv", check.names=FALSE)
t5b_ru <- read.csv("./www/rural_and_urban/t5b.csv", check.names=FALSE)
t6a_ru <- read.csv("./www/rural_and_urban/t6a.csv", check.names=FALSE)
t6b_ru <- read.csv("./www/rural_and_urban/t6b.csv", check.names=FALSE)
t7a_ru <- read.csv("./www/rural_and_urban/t7a.csv", check.names=FALSE)
t7b_ru <- read.csv("./www/rural_and_urban/t7b.csv", check.names=FALSE)
t8a_ru <- read.csv("./www/rural_and_urban/t8a.csv", check.names=FALSE)
t8b_ru <- read.csv("./www/rural_and_urban/t8b.csv", check.names=FALSE)
t9_ru <- read.csv("./www/rural_and_urban/t9.csv", check.names=FALSE)
t10_ru <- read.csv("./www/rural_and_urban/t10.csv", check.names=FALSE)
t11_ru <- read.csv("./www/rural_and_urban/t11.csv", check.names=FALSE)
t12_ru <- read.csv("./www/rural_and_urban/t12.csv", check.names=FALSE)
t13_ru <- read.csv("./www/rural_and_urban/t13.csv", check.names=FALSE)
t14a_ru <- read.csv("./www/rural_and_urban/t14a.csv", check.names=FALSE)
t14b_ru <- read.csv("./www/rural_and_urban/t14b.csv", check.names=FALSE)
t15a_ru <- read.csv("./www/rural_and_urban/t15a.csv", check.names=FALSE)
t15b_ru <- read.csv("./www/rural_and_urban/t15b.csv", check.names=FALSE)
t15c_ru <- read.csv("./www/rural_and_urban/t15c.csv", check.names=FALSE)
t16a_ru <- read.csv("./www/rural_and_urban/t16a.csv", check.names=FALSE)
t16b_ru <- read.csv("./www/rural_and_urban/t16b.csv", check.names=FALSE)
t16c_ru <- read.csv("./www/rural_and_urban/t16c.csv", check.names=FALSE)
t17a_ru <- read.csv("./www/rural_and_urban/t17a.csv", check.names=FALSE)
t17b_ru <- read.csv("./www/rural_and_urban/t17b.csv", check.names=FALSE)
t17c_ru <- read.csv("./www/rural_and_urban/t17c.csv", check.names=FALSE)
t18a_ru <- read.csv("./www/rural_and_urban/t18a.csv", check.names=FALSE)
t18b_ru <- read.csv("./www/rural_and_urban/t18b.csv", check.names=FALSE)
t19a_ru <- read.csv("./www/rural_and_urban/t19a.csv", check.names=FALSE)
t19b_ru <- read.csv("./www/rural_and_urban/t19b.csv", check.names=FALSE)
t20_ru <- read.csv("./www/rural_and_urban/t20.csv", check.names=FALSE)
t21_ru <- read.csv("./www/rural_and_urban/t21.csv", check.names=FALSE)
t22_ru <- read.csv("./www/rural_and_urban/t22.csv", check.names=FALSE)
t22_ru <- read.csv("./www/rural_and_urban/t22.csv", check.names=FALSE)

# data manipulations for time series in rural and urban tab ####

t1_ru_wide <- t1_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t1_ru_wide <- which(names(t1_ru_wide) %in% c('Islands & Remote Rural'))

t2a_ru_long <- t2a_ru %>% 
  gather("Year", "Value", 2:ncol(t2a_ru))
colnames(t2a_ru_long) <- c("NAME", "Year", "Value")
t2a_ru_long$Year <- as.numeric(t2a_ru_long$Year)
t2a_ru_wide <- t2a_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t2a_ru_wide <- which(names(t2a_ru_wide) %in% c('Islands & Remote Rural'))

t2b_ru_long <- t2b_ru %>% 
  gather("Year", "Value", 2:ncol(t2b_ru))
colnames(t2b_ru_long) <- c("NAME", "Year", "Value")
t2b_ru_long$Year <- as.numeric(t2b_ru_long$Year)
t2b_ru_wide <- t2b_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t2b_ru_wide <- which(names(t2b_ru_wide) %in% c('Islands & Remote Rural'))

t3a_ru_long <- t3a_ru %>% 
  gather("Year", "Value", 2:ncol(t3a_ru))
colnames(t3a_ru_long) <- c("NAME", "Year", "Value")
t3a_ru_long$Year <- as.numeric(t3a_ru_long$Year)
t3a_ru_wide <- t3a_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t3a_ru_wide <- which(names(t3a_ru_wide) %in% c('Islands & Remote Rural'))

t3b_ru_long <- t3b_ru %>% 
  gather("Year", "Value", 2:ncol(t3b_ru))
colnames(t3b_ru_long) <- c("NAME", "Year", "Value")
t3b_ru_long$Year <- as.numeric(t3b_ru_long$Year)
t3b_ru_wide <- t3b_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t3b_ru_wide <- which(names(t3b_ru_wide) %in% c('Islands & Remote Rural'))

t4a_ru_long <- t4a_ru %>% 
  gather("Year", "Value", 2:ncol(t4a_ru))
colnames(t4a_ru_long) <- c("NAME", "Year", "Value")
t4a_ru_long$Year <- as.numeric(t4a_ru_long$Year)
t4a_ru_wide <- t4a_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t4a_ru_wide <- which(names(t4a_ru_wide) %in% c('Islands & Remote Rural'))

t4b_ru_long <- t4b_ru %>% 
  gather("Year", "Value", 2:ncol(t4b_ru))
colnames(t4b_ru_long) <- c("NAME", "Year", "Value")
t4b_ru_long$Year <- as.numeric(t4b_ru_long$Year)
t4b_ru_wide <- t4b_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t4b_ru_wide <- which(names(t4b_ru_wide) %in% c('Islands & Remote Rural'))

t5a_ru_long <- t5a_ru %>% 
  gather("Year", "Value", 2:ncol(t5a_ru))
colnames(t5a_ru_long) <- c("NAME", "Year", "Value")
t5a_ru_long$Year <- as.numeric(t5a_ru_long$Year)
t5a_ru_wide <- t5a_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t5a_ru_wide <- which(names(t5a_ru_wide) %in% c('Islands & Remote Rural'))

t5b_ru_long <- t5b_ru %>% 
  gather("Year", "Value", 2:ncol(t5b_ru))
colnames(t5b_ru_long) <- c("NAME", "Year", "Value")
t5b_ru_long$Year <- as.numeric(t5b_ru_long$Year)
t5b_ru_wide <- t5b_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t5b_ru_wide <- which(names(t5b_ru_wide) %in% c('Islands & Remote Rural'))

t6a_ru_long <- t6a_ru %>% 
  gather("Year", "Value", 2:ncol(t6a_ru))
colnames(t6a_ru_long) <- c("NAME", "Year", "Value")
t6a_ru_long$Year <- as.numeric(t6a_ru_long$Year)
t6a_ru_wide <- t6a_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t6a_ru_wide <- which(names(t6a_ru_wide) %in% c('Islands & Remote Rural'))

t6b_ru_long <- t6b_ru %>% 
  gather("Year", "Value", 2:ncol(t6b_ru))
colnames(t6b_ru_long) <- c("NAME", "Year", "Value")
t6b_ru_long$Year <- as.numeric(t6b_ru_long$Year)
t6b_ru_wide <- t6b_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t6b_ru_wide <- which(names(t6b_ru_wide) %in% c('Islands & Remote Rural'))

t7a_ru_long <- t7a_ru %>% 
  gather("Year", "Value", 2:ncol(t7a_ru))
colnames(t7a_ru_long) <- c("NAME", "Year", "Value")
t7a_ru_long$Year <- as.numeric(t7a_ru_long$Year)
t7a_ru_wide <- t7a_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t7a_ru_wide <- which(names(t7a_ru_wide) %in% c('Islands & Remote Rural'))

t7b_ru_long <- t7b_ru %>% 
  gather("Year", "Value", 2:ncol(t7b_ru))
colnames(t7b_ru_long) <- c("NAME", "Year", "Value")
t7b_ru_long$Year <- as.numeric(t7b_ru_long$Year)
t7b_ru_wide <- t7b_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t7b_ru_wide <- which(names(t7b_ru_wide) %in% c('Islands & Remote Rural'))

t8a_ru_long <- t8a_ru %>% 
  gather("Year", "Value", 2:ncol(t8a_ru))
colnames(t8a_ru_long) <- c("NAME", "Year", "Value")
t8a_ru_long$Year <- as.numeric(t8a_ru_long$Year)
t8a_ru_wide <- t8a_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t8a_ru_wide <- which(names(t8a_ru_wide) %in% c('Islands & Remote Rural'))

t8b_ru_long <- t8b_ru %>% 
  gather("Year", "Value", 2:ncol(t8b_ru))
colnames(t8b_ru_long) <- c("NAME", "Year", "Value")
t8b_ru_long$Year <- as.numeric(t8b_ru_long$Year)
t8b_ru_wide <- t8b_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t8b_ru_wide <- which(names(t8b_ru_wide) %in% c('Islands & Remote Rural'))

t9_ru_long <- t9_ru %>% 
  gather("Year", "Value", 2:ncol(t9_ru))
colnames(t9_ru_long) <- c("NAME", "Year", "Value")
t9_ru_long$Year <- as.numeric(t9_ru_long$Year)
t9_ru_wide <- t9_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t9_ru_wide <- which(names(t9_ru_wide) %in% c('Islands & Remote Rural'))

t10_ru_long <- t10_ru %>% 
  gather("Year", "Value", 2:ncol(t10_ru))
colnames(t10_ru_long) <- c("NAME", "Year", "Value")
t10_ru_long$Year <- as.numeric(t10_ru_long$Year)
t10_ru_wide <- t10_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t10_ru_wide <- which(names(t10_ru_wide) %in% c('Islands & Remote Rural'))

t11_ru_long <- t11_ru %>% 
  gather("Year", "Value", 2:ncol(t11_ru))
colnames(t11_ru_long) <- c("NAME", "Year", "Value")
t11_ru_long$Year <- as.numeric(t11_ru_long$Year)
t11_ru_wide <- t11_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t11_ru_wide <- which(names(t11_ru_wide) %in% c('Islands & Remote Rural'))

t12_ru_long <- t12_ru %>% 
  gather("Year", "Value", 2:ncol(t12_ru))
colnames(t12_ru_long) <- c("NAME", "Year", "Value")
t12_ru_long$Year <- as.numeric(t12_ru_long$Year)
t12_ru_wide <- t12_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t12_ru_wide <- which(names(t12_ru_wide) %in% c('Islands & Remote Rural'))

t13_ru_long <- t13_ru %>% 
  gather("Year", "Value", 2:ncol(t13_ru))
colnames(t13_ru_long) <- c("NAME", "Year", "Value")
t13_ru_long$Year <- as.numeric(t13_ru_long$Year)
t13_ru_wide <- t13_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t13_ru_wide <- which(names(t13_ru_wide) %in% c('Islands & Remote Rural'))

t14a_ru_long <- t14a_ru %>% 
  gather("Year", "Value", 2:ncol(t14a_ru))
colnames(t14a_ru_long) <- c("NAME", "Year", "Value")
t14a_ru_long$Year <- as.numeric(t14a_ru_long$Year)
t14a_ru_wide <- t14a_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t14a_ru_wide <- which(names(t14a_ru_wide) %in% c('Islands & Remote Rural'))

t14b_ru_long <- t14b_ru %>% 
  gather("Year", "Value", 2:ncol(t14b_ru))
colnames(t14b_ru_long) <- c("NAME", "Year", "Value")
t14b_ru_long$Year <- as.numeric(t14b_ru_long$Year)
t14b_ru_wide <- t14b_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t14b_ru_wide <- which(names(t14b_ru_wide) %in% c('Islands & Remote Rural'))

t15a_ru_long <- t15a_ru %>% 
  gather("Year", "Value", 2:ncol(t15a_ru))
colnames(t15a_ru_long) <- c("NAME", "Year", "Value")
t15a_ru_long$Year <- as.numeric(t15a_ru_long$Year)
t15a_ru_wide <- t15a_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t15a_ru_wide <- which(names(t15a_ru_wide) %in% c('Islands & Remote Rural'))

t15b_ru_long <- t15b_ru %>% 
  gather("Year", "Value", 2:ncol(t15b_ru))
colnames(t15b_ru_long) <- c("NAME", "Year", "Value")
t15b_ru_long$Year <- as.numeric(t15b_ru_long$Year)
t15b_ru_wide <- t15b_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t15b_ru_wide <- which(names(t15b_ru_wide) %in% c('Islands & Remote Rural'))

t15c_ru_long <- t15c_ru %>% 
  gather("Year", "Value", 2:ncol(t15c_ru))
colnames(t15c_ru_long) <- c("NAME", "Year", "Value")
t15c_ru_long$Year <- as.numeric(t15c_ru_long$Year)
t15c_ru_wide <- t15c_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t15c_ru_wide <- which(names(t15c_ru_wide) %in% c('Islands & Remote Rural'))

t16a_ru_long <- t16a_ru %>% 
  gather("Year", "Value", 2:ncol(t16a_ru))
colnames(t16a_ru_long) <- c("NAME", "Year", "Value")
t16a_ru_long$Year <- as.numeric(t16a_ru_long$Year)
t16a_ru_wide <- t16a_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t16a_ru_wide <- which(names(t16a_ru_wide) %in% c('Islands & Remote Rural'))

t16b_ru_long <- t16b_ru %>% 
  gather("Year", "Value", 2:ncol(t16b_ru))
colnames(t16b_ru_long) <- c("NAME", "Year", "Value")
t16b_ru_long$Year <- as.numeric(t16b_ru_long$Year)
t16b_ru_wide <- t16b_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t16b_ru_wide <- which(names(t16b_ru_wide) %in% c('Islands & Remote Rural'))

t16c_ru_long <- t16c_ru %>% 
  gather("Year", "Value", 2:ncol(t16c_ru))
colnames(t16c_ru_long) <- c("NAME", "Year", "Value")
t16c_ru_long$Year <- as.numeric(t16c_ru_long$Year)
t16c_ru_wide <- t16c_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t16c_ru_wide <- which(names(t16c_ru_wide) %in% c('Islands & Remote Rural'))

t17a_ru_long <- t17a_ru %>% 
  gather("Year", "Value", 2:ncol(t17a_ru))
colnames(t17a_ru_long) <- c("NAME", "Year", "Value")
t17a_ru_long$Year <- as.numeric(t17a_ru_long$Year)
t17a_ru_wide <- t17a_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t17a_ru_wide <- which(names(t17a_ru_wide) %in% c('Islands & Remote Rural'))

t17b_ru_long <- t17b_ru %>% 
  gather("Year", "Value", 2:ncol(t17b_ru))
colnames(t17b_ru_long) <- c("NAME", "Year", "Value")
t17b_ru_long$Year <- as.numeric(t17b_ru_long$Year)
t17b_ru_wide <- t17b_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t17b_ru_wide <- which(names(t17b_ru_wide) %in% c('Islands & Remote Rural'))

t17c_ru_long <- t17c_ru %>% 
  gather("Year", "Value", 2:ncol(t17c_ru))
colnames(t17c_ru_long) <- c("NAME", "Year", "Value")
t17c_ru_long$Year <- as.numeric(t17c_ru_long$Year)
t17c_ru_wide <- t17c_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t17c_ru_wide <- which(names(t17c_ru_wide) %in% c('Islands & Remote Rural'))

t18a_ru_long <- t18a_ru %>% 
  gather("Year", "Value", 2:ncol(t18a_ru))
colnames(t18a_ru_long) <- c("NAME", "Year", "Value")
t18a_ru_long$Year <- as.numeric(t18a_ru_long$Year)
t18a_ru_wide <- t18a_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t18a_ru_wide <- which(names(t18a_ru_wide) %in% c('Islands & Remote Rural'))

t18b_ru_long <- t18b_ru %>% 
  gather("Year", "Value", 2:ncol(t18b_ru))
colnames(t18b_ru_long) <- c("NAME", "Year", "Value")
t18b_ru_long$Year <- as.numeric(t18b_ru_long$Year)
t18b_ru_wide <- t18b_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t18b_ru_wide <- which(names(t18b_ru_wide) %in% c('Islands & Remote Rural'))

t19a_ru_long <- t19a_ru %>% 
  gather("Year", "Value", 2:ncol(t19a_ru))
colnames(t19a_ru_long) <- c("NAME", "Year", "Value")
t19a_ru_long$Year <- as.numeric(t19a_ru_long$Year)
t19a_ru_wide <- t19a_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t19a_ru_wide <- which(names(t19a_ru_wide) %in% c('Islands & Remote Rural'))

t19b_ru_long <- t19b_ru %>% 
  gather("Year", "Value", 2:ncol(t19b_ru))
colnames(t19b_ru_long) <- c("NAME", "Year", "Value")
t19b_ru_long$Year <- as.numeric(t19b_ru_long$Year)
t19b_ru_wide <- t19b_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t19b_ru_wide <- which(names(t19b_ru_wide) %in% c('Islands & Remote Rural'))

t20_ru_long <- t20_ru %>% 
  gather("Year", "Value", 2:ncol(t20_ru))
colnames(t20_ru_long) <- c("NAME", "Year", "Value")
t20_ru_long$Year <- as.numeric(t20_ru_long$Year)
t20_ru_wide <- t20_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t20_ru_wide <- which(names(t20_ru_wide) %in% c('Islands & Remote Rural'))

t21_ru_long <- t21_ru %>% 
  gather("Year", "Value", 2:ncol(t21_ru))
colnames(t21_ru_long) <- c("NAME", "Year", "Value")
t21_ru_long$Year <- as.numeric(t21_ru_long$Year)
t21_ru_wide <- t21_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t21_ru_wide <- which(names(t21_ru_wide) %in% c('Islands & Remote Rural'))

t22_ru_long <- t22_ru %>% 
  gather("Year", "Value", 2:ncol(t22_ru))
colnames(t22_ru_long) <- c("NAME", "Year", "Value")
t22_ru_long$Year <- as.numeric(t22_ru_long$Year)
t22_ru_wide <- t22_ru_long %>% 
  spread("NAME", "Value", 2:3)
positions_t22_ru_wide <- which(names(t22_ru_wide) %in% c('Islands & Remote Rural'))





summary_tool_ru <- read.csv("./www/rural_and_urban/summary_tool.csv", check.names=FALSE)



# ENTERPRISE REGION AREAS ####

# t1 for barchart in Enterprise (GVA per head) ####
t1_e <- read.csv("./www/enterprise_region_areas/t1.csv", check.names=FALSE)
t1_e_long <- t1_e %>% 
  gather("Year", "Value", 2:ncol(t1_e))
colnames(t1_e_long) <- c("NAME", "Year", "Value")
t1_e_long$Year <- as.numeric(t1_e_long$Year)
start_year_t1_e <- min(t1_e_long$Year)
end_year_t1_e <- max(t1_e_long$Year)

# t3b for barchart in Enterprise (Business registrations per 10,000 adults) ####
t3b_e <- read.csv("./www/enterprise_region_areas/t3b.csv", check.names=FALSE)
t3b_e_long <- t3b_e %>% 
  gather("Year", "Value", 2:ncol(t3b_e))
colnames(t3b_e_long) <- c("NAME", "Year", "Value")
t3b_e_long$Year <- as.numeric(t3b_e_long$Year)
start_year_t3b_e <- min(t3b_e_long$Year)
end_year_t3b_e <- max(t3b_e_long$Year)

# t4b for barchart in Enterprise (Business de-registrations per 10,000 adults) ####
t4b_e <- read.csv("./www/enterprise_region_areas/t4b.csv", check.names=FALSE)
t4b_e_long <- t4b_e %>% 
  gather("Year", "Value", 2:ncol(t4b_e))
colnames(t4b_e_long) <- c("NAME", "Year", "Value")
t4b_e_long$Year <- as.numeric(t4b_e_long$Year)
start_year_t4b_e <- min(t4b_e_long$Year)
end_year_t4b_e <- max(t4b_e_long$Year)

# t5b for barchart in Enterprise (Business 3-year survival rate) ####
t5b_e <- read.csv("./www/enterprise_region_areas/t5b.csv", check.names=FALSE)
t5b_e_long <- t5b_e %>% 
  gather("Year", "Value", 2:ncol(t5b_e))
colnames(t5b_e_long) <- c("NAME", "Year", "Value")
t5b_e_long$Year <- as.numeric(t5b_e_long$Year)
start_year_t5b_e <- min(t5b_e_long$Year)
end_year_t5b_e <- max(t5b_e_long$Year)

# t8a for barchart in Enterprise (International exports per head) ####
t8a_e <- read.csv("./www/enterprise_region_areas/t8a.csv", check.names=FALSE)
t8a_e_long <- t8a_e %>% 
  gather("Year", "Value", 2:ncol(t8a_e))
colnames(t8a_e_long) <- c("NAME", "Year", "Value")
t8a_e_long$Year <- as.numeric(t8a_e_long$Year)
start_year_t8a_e <- min(t8a_e_long$Year)
end_year_t8a_e <- max(t8a_e_long$Year)

# t9 for barchart in Enterprise (BERD per head) ####
t9_e <- read.csv("./www/enterprise_region_areas/t9.csv", check.names=FALSE)
t9_e_long <- t9_e %>% 
  gather("Year", "Value", 2:ncol(t9_e))
colnames(t9_e_long) <- c("NAME", "Year", "Value")
t9_e_long$Year <- as.numeric(t9_e_long$Year)
start_year_t9_e <- min(t9_e_long$Year)
end_year_t9_e <- max(t9_e_long$Year)

# t14a for barchart in Enterprise (Employment rate) ####
t14a_e <- read.csv("./www/enterprise_region_areas/t14a.csv", check.names=FALSE)
t14a_e_long <- t14a_e %>% 
  gather("Year", "Value", 2:ncol(t14a_e))
colnames(t14a_e_long) <- c("NAME", "Year", "Value")
t14a_e_long$Year <- as.numeric(t14a_e_long$Year)
start_year_t14a_e <- min(t14a_e_long$Year)
end_year_t14a_e <- max(t14a_e_long$Year)

# t15b for barchart in Enterprise (Self-employment rate) ####
t15b_e <- read.csv("./www/enterprise_region_areas/t15b.csv", check.names=FALSE)
t15b_e_long <- t15b_e %>% 
  gather("Year", "Value", 2:ncol(t15b_e))
colnames(t15b_e_long) <- c("NAME", "Year", "Value")
t15b_e_long$Year <- as.numeric(t15b_e_long$Year)
start_year_t15b_e <- min(t15b_e_long$Year)
end_year_t15b_e <- max(t15b_e_long$Year)

# t17b for barchart in Enterprise (Unemployment rate) ####
t17b_e <- read.csv("./www/enterprise_region_areas/t17b.csv", check.names=FALSE)
t17b_e_long <- t17b_e %>% 
  gather("Year", "Value", 2:ncol(t17b_e))
colnames(t17b_e_long) <- c("NAME", "Year", "Value")
t17b_e_long$Year <- as.numeric(t17b_e_long$Year)
start_year_t17b_e <- min(t17b_e_long$Year)
end_year_t17b_e <- max(t17b_e_long$Year)

# t18a for barchart in Enterprise (Median gross weekly pay) ####
t18a_e <- read.csv("./www/enterprise_region_areas/t18a.csv", check.names=FALSE)
t18a_e_long <- t18a_e %>% 
  gather("Year", "Value", 2:ncol(t18a_e))
colnames(t18a_e_long) <- c("NAME", "Year", "Value")
t18a_e_long$Year <- as.numeric(t18a_e_long$Year)
start_year_t18a_e <- min(t18a_e_long$Year)
end_year_t18a_e <- max(t18a_e_long$Year)

t1_e <- read.csv("./www/enterprise_region_areas/t1.csv", check.names=FALSE)
t2a_e <- read.csv("./www/enterprise_region_areas/t2a.csv", check.names=FALSE)
t2b_e <- read.csv("./www/enterprise_region_areas/t2b.csv", check.names=FALSE)
t3a_e <- read.csv("./www/enterprise_region_areas/t3a.csv", check.names=FALSE)
t3b_e <- read.csv("./www/enterprise_region_areas/t3b.csv", check.names=FALSE)
t4a_e <- read.csv("./www/enterprise_region_areas/t4a.csv", check.names=FALSE)
t4b_e <- read.csv("./www/enterprise_region_areas/t4b.csv", check.names=FALSE)
t5a_e <- read.csv("./www/enterprise_region_areas/t5a.csv", check.names=FALSE)
t5b_e <- read.csv("./www/enterprise_region_areas/t5b.csv", check.names=FALSE)
t6a_e <- read.csv("./www/enterprise_region_areas/t6a.csv", check.names=FALSE)
t6b_e <- read.csv("./www/enterprise_region_areas/t6b.csv", check.names=FALSE)
t7a_e <- read.csv("./www/enterprise_region_areas/t7a.csv", check.names=FALSE)
t7b_e <- read.csv("./www/enterprise_region_areas/t7b.csv", check.names=FALSE)
t8a_e <- read.csv("./www/enterprise_region_areas/t8a.csv", check.names=FALSE)
t8b_e <- read.csv("./www/enterprise_region_areas/t8b.csv", check.names=FALSE)
t9_e <- read.csv("./www/enterprise_region_areas/t9.csv", check.names=FALSE)
t10_e <- read.csv("./www/enterprise_region_areas/t10.csv", check.names=FALSE)
t11_e <- read.csv("./www/enterprise_region_areas/t11.csv", check.names=FALSE)
t12_e <- read.csv("./www/enterprise_region_areas/t12.csv", check.names=FALSE)
t13_e <- read.csv("./www/enterprise_region_areas/t13.csv", check.names=FALSE)
t14a_e <- read.csv("./www/enterprise_region_areas/t14a.csv", check.names=FALSE)
t14b_e <- read.csv("./www/enterprise_region_areas/t14b.csv", check.names=FALSE)
t15a_e <- read.csv("./www/enterprise_region_areas/t15a.csv", check.names=FALSE)
t15b_e <- read.csv("./www/enterprise_region_areas/t15b.csv", check.names=FALSE)
t15c_e <- read.csv("./www/enterprise_region_areas/t15c.csv", check.names=FALSE)
t16a_e <- read.csv("./www/enterprise_region_areas/t16a.csv", check.names=FALSE)
t16b_e <- read.csv("./www/enterprise_region_areas/t16b.csv", check.names=FALSE)
t16c_e <- read.csv("./www/enterprise_region_areas/t16c.csv", check.names=FALSE)
t17a_e <- read.csv("./www/enterprise_region_areas/t17a.csv", check.names=FALSE)
t17b_e <- read.csv("./www/enterprise_region_areas/t17b.csv", check.names=FALSE)
t17c_e <- read.csv("./www/enterprise_region_areas/t17c.csv", check.names=FALSE)
t18a_e <- read.csv("./www/enterprise_region_areas/t18a.csv", check.names=FALSE)
t18b_e <- read.csv("./www/enterprise_region_areas/t18b.csv", check.names=FALSE)
t19a_e <- read.csv("./www/enterprise_region_areas/t19a.csv", check.names=FALSE)
t19b_e <- read.csv("./www/enterprise_region_areas/t19b.csv", check.names=FALSE)
t20_e <- read.csv("./www/enterprise_region_areas/t20.csv", check.names=FALSE)
t21_e <- read.csv("./www/enterprise_region_areas/t21.csv", check.names=FALSE)
t22_e <- read.csv("./www/enterprise_region_areas/t22.csv", check.names=FALSE)
t22_e <- read.csv("./www/enterprise_region_areas/t22.csv", check.names=FALSE)

# data manipulations for time series in enterprise tab ####

t1_e_wide <- t1_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t1_e_wide <- which(names(t1_e_wide) %in% c('Highlands and Islands Enterprise'))

t2a_e_long <- t2a_e %>% 
  gather("Year", "Value", 2:ncol(t2a_e))
colnames(t2a_e_long) <- c("NAME", "Year", "Value")
t2a_e_long$Year <- as.numeric(t2a_e_long$Year)
t2a_e_wide <- t2a_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t2a_e_wide <- which(names(t2a_e_wide) %in% c('Highlands and Islands Enterprise'))

t2b_e_long <- t2b_e %>% 
  gather("Year", "Value", 2:ncol(t2b_e))
colnames(t2b_e_long) <- c("NAME", "Year", "Value")
t2b_e_long$Year <- as.numeric(t2b_e_long$Year)
t2b_e_wide <- t2b_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t2b_e_wide <- which(names(t2b_e_wide) %in% c('Highlands and Islands Enterprise'))

t3a_e_long <- t3a_e %>% 
  gather("Year", "Value", 2:ncol(t3a_e))
colnames(t3a_e_long) <- c("NAME", "Year", "Value")
t3a_e_long$Year <- as.numeric(t3a_e_long$Year)
t3a_e_wide <- t3a_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t3a_e_wide <- which(names(t3a_e_wide) %in% c('Highlands and Islands Enterprise'))

t3b_e_long <- t3b_e %>% 
  gather("Year", "Value", 2:ncol(t3b_e))
colnames(t3b_e_long) <- c("NAME", "Year", "Value")
t3b_e_long$Year <- as.numeric(t3b_e_long$Year)
t3b_e_wide <- t3b_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t3b_e_wide <- which(names(t3b_e_wide) %in% c('Highlands and Islands Enterprise'))

t4a_e_long <- t4a_e %>% 
  gather("Year", "Value", 2:ncol(t4a_e))
colnames(t4a_e_long) <- c("NAME", "Year", "Value")
t4a_e_long$Year <- as.numeric(t4a_e_long$Year)
t4a_e_wide <- t4a_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t4a_e_wide <- which(names(t4a_e_wide) %in% c('Highlands and Islands Enterprise'))

t4b_e_long <- t4b_e %>% 
  gather("Year", "Value", 2:ncol(t4b_e))
colnames(t4b_e_long) <- c("NAME", "Year", "Value")
t4b_e_long$Year <- as.numeric(t4b_e_long$Year)
t4b_e_wide <- t4b_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t4b_e_wide <- which(names(t4b_e_wide) %in% c('Highlands and Islands Enterprise'))

t5a_e_long <- t5a_e %>% 
  gather("Year", "Value", 2:ncol(t5a_e))
colnames(t5a_e_long) <- c("NAME", "Year", "Value")
t5a_e_long$Year <- as.numeric(t5a_e_long$Year)
t5a_e_wide <- t5a_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t5a_e_wide <- which(names(t5a_e_wide) %in% c('Highlands and Islands Enterprise'))

t5b_e_long <- t5b_e %>% 
  gather("Year", "Value", 2:ncol(t5b_e))
colnames(t5b_e_long) <- c("NAME", "Year", "Value")
t5b_e_long$Year <- as.numeric(t5b_e_long$Year)
t5b_e_wide <- t5b_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t5b_e_wide <- which(names(t5b_e_wide) %in% c('Highlands and Islands Enterprise'))

t6a_e_long <- t6a_e %>% 
  gather("Year", "Value", 2:ncol(t6a_e))
colnames(t6a_e_long) <- c("NAME", "Year", "Value")
t6a_e_long$Year <- as.numeric(t6a_e_long$Year)
t6a_e_wide <- t6a_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t6a_e_wide <- which(names(t6a_e_wide) %in% c('Highlands and Islands Enterprise'))

t6b_e_long <- t6b_e %>% 
  gather("Year", "Value", 2:ncol(t6b_e))
colnames(t6b_e_long) <- c("NAME", "Year", "Value")
t6b_e_long$Year <- as.numeric(t6b_e_long$Year)
t6b_e_wide <- t6b_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t6b_e_wide <- which(names(t6b_e_wide) %in% c('Highlands and Islands Enterprise'))

t7a_e_long <- t7a_e %>% 
  gather("Year", "Value", 2:ncol(t7a_e))
colnames(t7a_e_long) <- c("NAME", "Year", "Value")
t7a_e_long$Year <- as.numeric(t7a_e_long$Year)
t7a_e_wide <- t7a_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t7a_e_wide <- which(names(t7a_e_wide) %in% c('Highlands and Islands Enterprise'))

t7b_e_long <- t7b_e %>% 
  gather("Year", "Value", 2:ncol(t7b_e))
colnames(t7b_e_long) <- c("NAME", "Year", "Value")
t7b_e_long$Year <- as.numeric(t7b_e_long$Year)
t7b_e_wide <- t7b_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t7b_e_wide <- which(names(t7b_e_wide) %in% c('Highlands and Islands Enterprise'))

t8a_e_long <- t8a_e %>% 
  gather("Year", "Value", 2:ncol(t8a_e))
colnames(t8a_e_long) <- c("NAME", "Year", "Value")
t8a_e_long$Year <- as.numeric(t8a_e_long$Year)
t8a_e_wide <- t8a_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t8a_e_wide <- which(names(t8a_e_wide) %in% c('Highlands and Islands Enterprise'))

t8b_e_long <- t8b_e %>% 
  gather("Year", "Value", 2:ncol(t8b_e))
colnames(t8b_e_long) <- c("NAME", "Year", "Value")
t8b_e_long$Year <- as.numeric(t8b_e_long$Year)
t8b_e_wide <- t8b_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t8b_e_wide <- which(names(t8b_e_wide) %in% c('Highlands and Islands Enterprise'))

t9_e_long <- t9_e %>% 
  gather("Year", "Value", 2:ncol(t9_e))
colnames(t9_e_long) <- c("NAME", "Year", "Value")
t9_e_long$Year <- as.numeric(t9_e_long$Year)
t9_e_wide <- t9_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t9_e_wide <- which(names(t9_e_wide) %in% c('Highlands and Islands Enterprise'))

t10_e_long <- t10_e %>% 
  gather("Year", "Value", 2:ncol(t10_e))
colnames(t10_e_long) <- c("NAME", "Year", "Value")
t10_e_long$Year <- as.numeric(t10_e_long$Year)
t10_e_wide <- t10_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t10_e_wide <- which(names(t10_e_wide) %in% c('Highlands and Islands Enterprise'))

t11_e_long <- t11_e %>% 
  gather("Year", "Value", 2:ncol(t11_e))
colnames(t11_e_long) <- c("NAME", "Year", "Value")
t11_e_long$Year <- as.numeric(t11_e_long$Year)
t11_e_wide <- t11_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t11_e_wide <- which(names(t11_e_wide) %in% c('Highlands and Islands Enterprise'))

t12_e_long <- t12_e %>% 
  gather("Year", "Value", 2:ncol(t12_e))
colnames(t12_e_long) <- c("NAME", "Year", "Value")
t12_e_long$Year <- as.numeric(t12_e_long$Year)
t12_e_wide <- t12_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t12_e_wide <- which(names(t12_e_wide) %in% c('Highlands and Islands Enterprise'))

t13_e_long <- t13_e %>% 
  gather("Year", "Value", 2:ncol(t13_e))
colnames(t13_e_long) <- c("NAME", "Year", "Value")
t13_e_long$Year <- as.numeric(t13_e_long$Year)
t13_e_wide <- t13_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t13_e_wide <- which(names(t13_e_wide) %in% c('Highlands and Islands Enterprise'))

t14a_e_long <- t14a_e %>% 
  gather("Year", "Value", 2:ncol(t14a_e))
colnames(t14a_e_long) <- c("NAME", "Year", "Value")
t14a_e_long$Year <- as.numeric(t14a_e_long$Year)
t14a_e_wide <- t14a_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t14a_e_wide <- which(names(t14a_e_wide) %in% c('Highlands and Islands Enterprise'))

t14b_e_long <- t14b_e %>% 
  gather("Year", "Value", 2:ncol(t14b_e))
colnames(t14b_e_long) <- c("NAME", "Year", "Value")
t14b_e_long$Year <- as.numeric(t14b_e_long$Year)
t14b_e_wide <- t14b_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t14b_e_wide <- which(names(t14b_e_wide) %in% c('Highlands and Islands Enterprise'))

t15a_e_long <- t15a_e %>% 
  gather("Year", "Value", 2:ncol(t15a_e))
colnames(t15a_e_long) <- c("NAME", "Year", "Value")
t15a_e_long$Year <- as.numeric(t15a_e_long$Year)
t15a_e_wide <- t15a_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t15a_e_wide <- which(names(t15a_e_wide) %in% c('Highlands and Islands Enterprise'))

t15b_e_long <- t15b_e %>% 
  gather("Year", "Value", 2:ncol(t15b_e))
colnames(t15b_e_long) <- c("NAME", "Year", "Value")
t15b_e_long$Year <- as.numeric(t15b_e_long$Year)
t15b_e_wide <- t15b_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t15b_e_wide <- which(names(t15b_e_wide) %in% c('Highlands and Islands Enterprise'))

t15c_e_long <- t15c_e %>% 
  gather("Year", "Value", 2:ncol(t15c_e))
colnames(t15c_e_long) <- c("NAME", "Year", "Value")
t15c_e_long$Year <- as.numeric(t15c_e_long$Year)
t15c_e_wide <- t15c_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t15c_e_wide <- which(names(t15c_e_wide) %in% c('Highlands and Islands Enterprise'))

t16a_e_long <- t16a_e %>% 
  gather("Year", "Value", 2:ncol(t16a_e))
colnames(t16a_e_long) <- c("NAME", "Year", "Value")
t16a_e_long$Year <- as.numeric(t16a_e_long$Year)
t16a_e_wide <- t16a_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t16a_e_wide <- which(names(t16a_e_wide) %in% c('Highlands and Islands Enterprise'))

t16b_e_long <- t16b_e %>% 
  gather("Year", "Value", 2:ncol(t16b_e))
colnames(t16b_e_long) <- c("NAME", "Year", "Value")
t16b_e_long$Year <- as.numeric(t16b_e_long$Year)
t16b_e_wide <- t16b_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t16b_e_wide <- which(names(t16b_e_wide) %in% c('Highlands and Islands Enterprise'))

t16c_e_long <- t16c_e %>% 
  gather("Year", "Value", 2:ncol(t16c_e))
colnames(t16c_e_long) <- c("NAME", "Year", "Value")
t16c_e_long$Year <- as.numeric(t16c_e_long$Year)
t16c_e_wide <- t16c_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t16c_e_wide <- which(names(t16c_e_wide) %in% c('Highlands and Islands Enterprise'))

t17a_e_long <- t17a_e %>% 
  gather("Year", "Value", 2:ncol(t17a_e))
colnames(t17a_e_long) <- c("NAME", "Year", "Value")
t17a_e_long$Year <- as.numeric(t17a_e_long$Year)
t17a_e_wide <- t17a_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t17a_e_wide <- which(names(t17a_e_wide) %in% c('Highlands and Islands Enterprise'))

t17b_e_long <- t17b_e %>% 
  gather("Year", "Value", 2:ncol(t17b_e))
colnames(t17b_e_long) <- c("NAME", "Year", "Value")
t17b_e_long$Year <- as.numeric(t17b_e_long$Year)
t17b_e_wide <- t17b_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t17b_e_wide <- which(names(t17b_e_wide) %in% c('Highlands and Islands Enterprise'))

t17c_e_long <- t17c_e %>% 
  gather("Year", "Value", 2:ncol(t17c_e))
colnames(t17c_e_long) <- c("NAME", "Year", "Value")
t17c_e_long$Year <- as.numeric(t17c_e_long$Year)
t17c_e_wide <- t17c_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t17c_e_wide <- which(names(t17c_e_wide) %in% c('Highlands and Islands Enterprise'))

t18a_e_long <- t18a_e %>% 
  gather("Year", "Value", 2:ncol(t18a_e))
colnames(t18a_e_long) <- c("NAME", "Year", "Value")
t18a_e_long$Year <- as.numeric(t18a_e_long$Year)
t18a_e_wide <- t18a_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t18a_e_wide <- which(names(t18a_e_wide) %in% c('Highlands and Islands Enterprise'))

t18b_e_long <- t18b_e %>% 
  gather("Year", "Value", 2:ncol(t18b_e))
colnames(t18b_e_long) <- c("NAME", "Year", "Value")
t18b_e_long$Year <- as.numeric(t18b_e_long$Year)
t18b_e_wide <- t18b_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t18b_e_wide <- which(names(t18b_e_wide) %in% c('Highlands and Islands Enterprise'))

t19a_e_long <- t19a_e %>% 
  gather("Year", "Value", 2:ncol(t19a_e))
colnames(t19a_e_long) <- c("NAME", "Year", "Value")
t19a_e_long$Year <- as.numeric(t19a_e_long$Year)
t19a_e_wide <- t19a_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t19a_e_wide <- which(names(t19a_e_wide) %in% c('Highlands and Islands Enterprise'))

t19b_e_long <- t19b_e %>% 
  gather("Year", "Value", 2:ncol(t19b_e))
colnames(t19b_e_long) <- c("NAME", "Year", "Value")
t19b_e_long$Year <- as.numeric(t19b_e_long$Year)
t19b_e_wide <- t19b_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t19b_e_wide <- which(names(t19b_e_wide) %in% c('Highlands and Islands Enterprise'))

t20_e_long <- t20_e %>% 
  gather("Year", "Value", 2:ncol(t20_e))
colnames(t20_e_long) <- c("NAME", "Year", "Value")
t20_e_long$Year <- as.numeric(t20_e_long$Year)
t20_e_wide <- t20_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t20_e_wide <- which(names(t20_e_wide) %in% c('Highlands and Islands Enterprise'))

t21_e_long <- t21_e %>% 
  gather("Year", "Value", 2:ncol(t21_e))
colnames(t21_e_long) <- c("NAME", "Year", "Value")
t21_e_long$Year <- as.numeric(t21_e_long$Year)
t21_e_wide <- t21_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t21_e_wide <- which(names(t21_e_wide) %in% c('Highlands and Islands Enterprise'))

t22_e_long <- t22_e %>% 
  gather("Year", "Value", 2:ncol(t22_e))
colnames(t22_e_long) <- c("NAME", "Year", "Value")
t22_e_long$Year <- as.numeric(t22_e_long$Year)
t22_e_wide <- t22_e_long %>% 
  spread("NAME", "Value", 2:3)
positions_t22_e_wide <- which(names(t22_e_wide) %in% c('Highlands and Islands Enterprise'))


summary_tool_e <- read.csv("./www/enterprise_region_areas/summary_tool.csv", check.names=FALSE)

