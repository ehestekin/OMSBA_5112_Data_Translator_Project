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
ag_profit_df       <- read_dta('Raw_Data/glss4_new/aggregates/agg2.dta')
costs_land_df      <- read_dta('Raw_Data/glss4_new/aggregates/exp3.dta')
costs_crops_df     <- read_dta('Raw_Data/glss4_new/aggregates/exp4.dta')
costs_livestock_df <- read_dta('Raw_Data/glss4_new/aggregates/exp5.dta')

#community info-------------------------------------------------
community_df        <- read_dta('Raw_Data/glss4_new/community/cs2.dta')
community_health_df <- read_dta('Raw_Data/glss4_new/community/cs4b.dta')
community_ag_df     <- read_dta('Raw_Data/glss4_new/community/cs5b.dta')


# ---------------------- Organize data --------------------------

#comm doesn't use clust while everything else does.  examine reg/dist/eanum and clust
clust_map_df <- select(id_df, c(region, district, eanum, clust))

#sort by clust and make sure clust doesn't duplicate
clust_map_df <- clust_map_df %>% arrange(clust) %>% distinct()

#looks like clust is just '4' + eanum (reg/dist are unused)
#i.e. eanum 4 is clust 4004 and eanum 930 is clust 4930
test_eanum_mut <- clust_map_df %>% mutate(clust_mut = eanum + 4000)

#above works, do same for all comm dfs
#select(clust,everything()) puts clust in first column
community_ag_df <- community_ag_df %>% mutate %>%
                                       select(clust, everything())
community_df <- community_df %>% mutate(clust = eanum + 4000) %>%
                                 select(clust, everything())
community_health_df <- community_health_df %>% mutate(clust = eanum + 4000) %>%
                                               select(clust, everything())


#want to tidy with clust and nh being one data point in each row
#----------------------- Tidy Cost Data ------------------------

#separate farmcd into columns so cost of farm 1, cost of farm2 etc.
#this allows households with multiple farms to be contained in one row
#fill in NA with 0 (0 cost for that farm because it doesn't exist.  allows for summing household cost)
costs_land_tidy_df <- costs_land_df %>% 
                        spread(key = farmcd, value = landexp, fill = 0) %>%
                          arrange(clust)

#similarly for crop expense code
costs_crops_tidy_df <- costs_crops_df %>%
                        spread(key = crpexpcd, value = cropexp, fill = 0) %>%
                         arrange(clust)

#and livestock expense
costs_livestock_tidy_df <- costs_livestock_df %>%
                            spread(key = crpexpcd, value = livexp, fill = 0) %>%
                             arrange(clust)

#sum up expenses in each set into new column
costs_land_tidy_df <- costs_land_tidy_df %>%
                       mutate(expsum = rowSums(costs_land_tidy_df[,-(1:2)])) %>% #don't include first two columns in sum
                        select(nh,clust,expsum, everything()) #reorder with sum in front of individual pieces


costs_crops_tidy_df <- costs_crops_tidy_df %>%
                        mutate(expsum = rowSums(costs_crops_tidy_df[,-(1:2)])) %>%
                         select(nh,clust,expsum, everything())

costs_livestock_tidy_df <- costs_livestock_tidy_df %>%
                            mutate(expsum = rowSums(costs_livestock_tidy_df[,-(1:2)])) %>%
                             select(nh,clust,expsum, everything())

