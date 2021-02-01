#####################################################################      
# Project:  Downloading Data using R                                                
# Author(s): Monroe Gamble    
# Last revision: 11/04/2019                                                   

# This script demonstrates how to download data from a variety of sources using R
#####################################################################



#set workind Driectory  -----------------------------------------
setwd('***REPLACE W/ YOUR DIRECTORY***/DownloadingDataR')

# Install Packages -----------------------------------------
# Pro-tip: Ctrl + click to open file

source('Supplements/Setup.R') #SETUP FILE NOT INCLUDED. Install Packages below.#

# Load Libraries -----------------------------------------
library(tidyverse)


###### Haver Example ###############################
library(Haver)

# Use the documentation
# ?Haver

# Haver US GDP Data, Core PCE & WTI ---------------------------------------------------------------------
# GDPA@USECON Trade-weighted major currencies index (Mar 1973=100)
# JCXFEBM@USECON Core Personal Consumption Expenditures (PCE)
# PZTEX@USECON West Texas intermediate, Cushing (CME Group)

# Set start and end dates
haver_start <- as.Date('01/31/1990', format = "%m/%d/%Y")
haver_end <- Sys.Date()  #Today's Date

# Step 1. Just Read it in ----------------------------------------------

# Quarterly GDP
haver.data(codes = 'USECON:gdp', start = haver_start, end = haver_end, 
           freq = 'q', rtype = 'data.frame' , eop.dates = TRUE) #%>% #data is unwiedly!!!
#  head()
# tail(10)

# Pro-tip: Autoformat Ctrl+Shift+A

# Step 2. Assignment Operator  ----------------------------------------------

# Store Data Frame as Tibble
GDP <-
  haver.data(
    codes = 'USECON:gdp',
    start = haver_start,
    end = haver_end,
    freq = 'q',
    rtype = 'data.frame' ,
    eop.dates = TRUE
  )

GDP

#Head for first 6 rows / tail for last few
GDP %>% head()
GDP %>% tail()

# Convert data to tibble - "modern data.frame"
GDP %>% as_tibble()

# Fix rows
GDP %>% rownames_to_column("Date") %>% as_tibble()

#Pro-tip: Pipe-it up!!! (%>%)  https://www.datacamp.com/community/tutorials/pipe-r-tutorial
# https://www.youtube.com/watch?v=e66pZFg3j_8

# Step 3. Downloading multiple variables   ----------------------------------------------

haver.data(
  codes = c('USECON:gdp', 'USECON:JCXFEBM'),
  start = haver_start,
  end = haver_end,
  freq = 'q',
  rtype = 'data.frame' ,
  eop.dates = TRUE
) %>%
  rownames_to_column("Date") %>%
  as_tibble()


#Put it in a list: GDP, PCE, WTI
var_list <- c('USECON:gdp', 'USECON:JCXFEBM', 'USECON:PZTEX')  # Not case sensitive

  haver.data(
    codes = var_list,
    start = haver_start,
    end = haver_end,
    freq = 'q',
    rtype = 'data.frame' ,
    eop.dates = TRUE
  ) %>%
    rownames_to_column("Date") %>%
    as_tibble()


# Pro-tip: If you do it over and over CREATE A FUNCITON!!!
fix_rows <- 
  function(x) {
    x %>%
      rownames_to_column("Date") %>%
      as_tibble()
  }

# Download Data using function to fix rows
  haver.data(
    codes = var_list,
    start = haver_start,
    end = haver_end,
    freq = 'q',
    rtype = 'data.frame' ,
    eop.dates = TRUE
  ) %>%
  fix_rows()

# Add data pull check to function
fix_rows <- 
  function(x) {
    data <-
      x %>%
        rownames_to_column("Date") %>%
        as_tibble()
      
      if (nrow(x) == 0) {
        stop(paste("Haver pull for didn't return any data."), call.=FALSE)
      } else {
        cat("Pulled Haver data.")
      }
      
    return(data) # Tell function to return data
    
  }

# Download data using function to fix rows & check data
  haver.data(
    codes = var_list,
    start = haver_start,
    end = haver_end,
    freq = 'q',
    rtype = 'data.frame' ,
    eop.dates = TRUE
  ) %>%
  fix_rows()

#Don't forget to store data
data <- 
  haver.data(
    codes = var_list,
    start = haver_start,
    end = haver_end,
    freq = 'q',
    rtype = 'data.frame' ,
    eop.dates = TRUE
  ) %>%
  fix_rows()

# Clean environment
rm(GDP)



##### Exporting & Importing Tabular Data ######################
library(readr)
# Export / Save Tabular Data ---------------------------------------
# dir.create("data")
# unlink("data", recursive = T)

# Save CSV
write_csv(data, "GDP_PCE_WTI.csv") 
# Save TSV
write_tsv(data, "GDP_PCE_WTI.tsv")

# Save TXT (write csv can be used to create .txt file)
write_csv(data, "GDP_PCE_WTI.txt") 

# Save as Rdata file (Large files)
write_rds(data, "GDP_PCE_WTI.Rds")

# saves in current directory
getwd()

# List files in directory
list.files()

# Delete file
unlink("GDP_PCE_WTI.Rds")
# Delete mutliple files
file.remove("GDP_PCE_WTI.tsv", "GDP_PCE_WTI.txt")

# List files in directory
list.files()

# Save Delimitted file
write_delim(data, "GDP_PCE_WTI_DELIM.txt", delim = " ") #space
# Append - Same file Name Overwrties file, specifying append adds to file
write_delim(data, "GDP_PCE_WTI_DELIM.txt", delim = "@@", append = T)


# Overwite file
write_delim(data, "GDP_PCE_WTI_DELIM.txt", delim = "/")


# Import / Load Data Tabular Data ---------------------------------------

# wipes everything
rm(list = ls())

# Load CSV
read_csv("GDP_PCE_WTI.csv")
# Load Delimited
read_delim("GDP_PCE_WTI_DELIM.txt", delim = "/")

# Don't forget to store data
data <- read_csv("GDP_PCE_WTI.csv")

data


###### FRED ###############################
library(fredr)
#SP500        S&P 500 (https://fred.stlouisfed.org/series/SP500)
#DCOILWTICO   Crude Oil Prices: West Texas Intermediate (WTI) - Cushing, Oklahoma

# Download S&P 500 data
sp500 <-  
  fredr(
    series_id = 'SP500',
  )

#Requires Fed Key
source("Source/FredKey.R")

sp500 <-  
  fredr(
    series_id = 'SP500',
  )

read.csv("FRED_Data.xlsx")

ggplot(sp500, aes(x=date, y=value)) + 
  geom_line() + 
  theme_minimal()

# Download WTI
WTI <-  
  fredr(
    series_id = 'DCOILWTICO',
  )

#Pro-tip: FRED data download defaults to a tibble

# # NBER Recessions
# fredr(
#   series_id = 'USRECQ',
# ) %>% tail(40)

###### Plotly ###############################
library(plotly)
plot_ly(sp500, x = ~date, y = ~value, type = 'scatter', mode = 'lines')



##### Exporting & Importing Excel Data  ######################
library(readxl) # Read Files
library(xlsx) # Export Files (not part of tidyverse)


# Export / Save Excel ------------------------------------------------ 
write.xlsx(sp500, "sp500.xlsx", row.names = FALSE)

# Pass it a data frame
sp500_df <- sp500 %>% as.data.frame()

# Save Excel file using 'xlsx' package
write.xlsx(sp500_df, "FRED_Data.xlsx", row.names = FALSE, sheetName = "sp500")

# Convert WTI to data frame
WTI_df <- WTI %>% as.data.frame()

# Specifying Append adds a sheet to existing file
write.xlsx(WTI_df, "FRED_Data.xlsx", row.names = FALSE, append = T, sheetName = "WTI")

# Clean environment
rm(sp500, sp500_df, WTI, WTI_df)


# Import / Load Excel Data -----------------------------------------------------

# Load by sheet name or index (multiple sheets)
sp500 <- read.xlsx("FRED_Data.xlsx", sheetName = "sp500") %>% 
  as_tibble() #Loads at dataframe, convert to tibble

sp500

# Load by Index
WTI <- read.xlsx("FRED_Data.xlsx", sheetIndex = 2) %>% 
  as_tibble() #Loads at dataframe, convert to tibble

WTI

# Load using readxl (tidyverse)
read_excel("FRED_Data.xlsx", sheet = 1) #Loads as tibble
read_xlsx("FRED_Data.xlsx", sheet = 1) #Loads as tibble


##### Load Data from the web

# SURVEY OF PROFESSIONAL FORECASTERS (SPF) -------------------------------------

dispersion1 <- 'https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/dispersion_corecpi.xlsx?la=en'

download.file(dispersion1, destfile = "SPF_Forecast.xlsx", mode = 'wb')

SPF_dispersion <-
  readxl::read_xlsx('SPF_Forecast.xlsx', skip = 9, na = '#N/A')

SPF_dispersion
  
# Advanced:  Downloading multiple sheets ----------------------------------------------------------------

# SPF Dispersion 1
dispersion1 <-
  'https://www.philadelphiafed.org:443/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/historical-data/dispersion_1.xlsx'

# SPF Dispersion 2
dispersion2 <-
  'https://www.philadelphiafed.org:443/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/historical-data/dispersion_2.xlsx'

# Save SPF Data
lapply(list(dispersion1, dispersion2), function(x) {
  download.file(x, destfile = paste0(sub(".*/(.*.xlsx).*", "\\1", x)), mode = 'wb')})

# Retrieve Sheets
sheetnames <- list('PCE', 'COREPCE', 'UNEMP', 'RGDP')

disp1 <- 
  lapply(sheetnames[1:3], function(x){read_xlsx('dispersion_1.xlsx', sheet = x, skip = 9, na = '#N/A')}) %>% 
  as.data.frame()

disp2 <- read_xlsx('dispersion_2.xlsx', sheet = sheetnames[[4]], skip = 9, na = '#N/A') %>%
  as.data.frame()

# Create excel workbook
write.xlsx(disp1, "dispersion_merge2.xlsx", row.names = FALSE, append = T, sheetName = "dispersion 1")
# Add Second Sheet
write.xlsx(disp2, "dispersion_merge2.xlsx",  row.names = FALSE, append = T, sheetName = "dispersion 2")



###### Quantmod ############################
library(quantmod)

# Load Market Data -----------------------------------------

getSymbols("AMZN")
AMZN_df <- data.frame(AMZN) %>% 
  rownames_to_column() %>% 
  rename(Date = rowname) %>%
  mutate(Date = as.Date(Date))

getSymbols("GOOGL")
GOOGL_df <- data.frame(GOOGL) %>% 
  rownames_to_column() %>% 
  rename(Date = rowname) %>%
  mutate(Date = as.Date(Date))

getSymbols("^GSPC")
sp500_df <- data.frame(GSPC) %>% 
  rownames_to_column() %>% 
  rename(Date = rowname) %>%
  mutate(Date = as.Date(Date))

# Plot using Plotly
plot_ly(sp500_df, x = ~Date, y = ~GSPC.Close, type = 'scatter', mode = 'lines', name = 'S&P 500') %>%
  add_trace(data = AMZN_df, y = ~AMZN.Close, name = "AMZN") %>%
  add_trace(data = GOOGL_df, y = ~GOOGL.Close, name = "GOOGL") %>%
  layout(
    yaxis = list(title = "Dollars (US)",
                 hoverformat = '$,f')
  )



#########################################################################
# Data Import Cheatsheet: https://resources.rstudio.com/rstudio-cheatsheets/data-import-cheat-sheet
