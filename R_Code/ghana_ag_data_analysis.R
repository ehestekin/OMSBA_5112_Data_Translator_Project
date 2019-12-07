# Data translator project

# Ghana Living Standards Survey

#Business Problem ------------------------------------------

# The ACME corporation is considering moving 
# into agricultural inputs in Ghana. 
# To get an idea of whom to target for their sales efforts 
# they have hired you to analyze what determines 
# agricultural profit there. They are especially interested 
# in what effect household educational attainment and 
# the characteristics of the local area has on profit, 
# but if you find other results of interest, they would like
# to hear those as well. They have left the choice of 
# explanatory variables and variable definitions to you, 
# but to be able to compare across regions they want at
# least one specification that examines profit per area unit


# Import lib and data----------------------------------------

library(tidyverse)
library(haven)


#array of all .dta files in dataset (not using this section)----
file_list <- list.files('Raw_Data/glss4_new/', pattern = '\\.dta*', recursive = TRUE)

#read in file from list
test <- read_dta(paste('Raw_Data/glss4_new/' , file_list[1], sep = ''))

#household and location info------------------------------------
id_df         <- read_dta('Raw_Data/glss4_new/sec0a.dta')
education_df  <- read_dta('Raw_Data/glss4_new/sec2a.dta')
land_chars_df <- read_dta('Raw_Data/glss4_new/sec8a1.dta')

#financials-----------------------------------------------------
ag_profit_df    <- read_dta('Raw_Data/glss4_new/aggregates/agg2.dta')
costs_land_df   <- read_dta('Raw_Data/glss4_new/aggregates/exp3.dta')
costs_crops_df  <- read_dta('Raw_Data/glss4_new/aggregates/exp4.dta')
costs_livestock <- read_dta('Raw_Data/glss4_new/aggregates/exp5.dta')

#community info-------------------------------------------------
community_df        <- read_dta('Raw_Data/glss4_new/community/cs2.dta')
community_health_df <- read_dta('Raw_Data/glss4_new/community/cs4b.dta')
community_ag_df     <- read_dta('Raw_Data/glss4_new/community/cs5b.dta')


