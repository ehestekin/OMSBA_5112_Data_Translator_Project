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
#195 rural, 105 urban (only have comm surveys from rural)
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
community_ag_df <- community_ag_df %>% mutate(clust = eanum + 4000) %>%
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

#ag_profit_df already tidy from the start

#---------------------- Tidy Education Data ---------------------

#similar to cost data, move individual education into columns so the data is
#tidy by household

#only interested in 5+ y/o (s1q23 = 1)
#pivot wider is like spread but seems to handle multiple columns better
education_tidy_df <- education_df %>% filter(s1q23 == 1) %>%
                          pivot_wider(names_from = pid, values_from = c(s2aq1:s2aq17))

#for some reason column order is weird.  sort by col name xxx1_1, xxx1_2,...
#found function str_sort which can treat strings as numbers
#normal sort() did alpha i.e. _1, _10, _11 not _1, _2, _3
education_tidy_df <- education_tidy_df %>% select(str_sort(names(.), numeric = TRUE))

#----------------------- Tidy Community Health Data --------------------

#see G4QComm for enums (page 18)

#some communities have more than one data point.  filter down to most popular
#add number of occurances of clust
community_health_df <- community_health_df %>%
                        add_count(clust) %>% select(clust, n, everything())

community_df <- community_df %>% 
                 add_count(clust) %>% select(clust, n, everything())

community_ag_df <- community_ag_df %>% 
                    add_count(clust) %>% select(clust, n, everything())

#6 different enums for health questions so 6 rows per community
community_health_df_single_hh_comms <- community_health_df %>% filter(n == 6)

community_health_df_multi_hh_comms <- community_health_df %>% filter(n > 6)

#other two are 1 per comm
community_df_single_hh_comms <- community_df %>% filter(n == 1)

community_df_multi_hh_comms <- community_df %>% filter(n > 1)

community_ag_df_single_hh_comms <- community_ag_df %>% filter(n == 1)

community_ag_df_multi_hh_comms <- community_ag_df %>% filter(n > 1)

#use the mode of a column to find the most popular value in each duplicate set
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ag first -----------------------------------------------------
#init filtered down dfs for multi hh comms
unique_clust_ag <- unique(community_ag_df_multi_hh_comms$clust)
community_ag_df_multi_hh_comms_filtered <- community_ag_df_multi_hh_comms[unique_clust_ag,]

for (i in 1:length(unique_clust_ag)){
  #get the most common value from each column for each clust
  this_df <- community_ag_df_multi_hh_comms[community_ag_df_multi_hh_comms$clust == unique_clust_ag[i],]
  
  for (j in names(this_df)){
    #get each column mode
    community_ag_df_multi_hh_comms_filtered[i,j] <- Mode(this_df[[j]])
    
  }
  
}

community_ag_tidy_df <- rbind(community_ag_df_multi_hh_comms_filtered, community_ag_df_single_hh_comms)

# ---------------------------------------- community df next ------------------------------------------
#do same as above
unique_clust <- unique(community_df_multi_hh_comms$clust)
community_df_multi_hh_comms_filtered <- community_df_multi_hh_comms[unique_clust,]

for (i in 1:length(unique_clust)){
  this_df <- community_df_multi_hh_comms[community_df_multi_hh_comms$clust == unique_clust[i],]
  
  for (j in names(this_df)){
    community_df_multi_hh_comms_filtered[i,j] <- Mode(this_df[[j]])
  }
}

community_tidy_df <- rbind(community_df_multi_hh_comms_filtered, community_df_single_hh_comms)

# -------------------------- hardest one is health due to needing to spread on 's4bq0' ---------------

unique_health_clust <- unique(community_health_df_multi_hh_comms$clust)
#note this will be too small due to 6 rows per clust but seems to work quickly enough
community_health_df_multi_hh_filtered <- community_health_df_multi_hh_comms[unique_health_clust,]

#need to keep track of rows since their are two vars controlling 'this_df'
n_row = 0

for (i in 1:length(unique_health_clust)){
  for (health_type in 10:15){
    this_df <- community_health_df_multi_hh_comms[(community_health_df_multi_hh_comms$clust == unique_health_clust[i] & 
                                                    community_health_df_multi_hh_comms$s4bq0 == health_type),]
    n_row = n_row + 1
    
    for (j in names(this_df)){
      community_health_df_multi_hh_filtered[n_row,j] <- Mode(this_df[[j]])
    }
  }
}

# ---------------- can now tidy community health data (split into one clust per row) -----------------

community_health_tidy_df <- rbind(community_health_df_multi_hh_filtered, community_health_df_single_hh_comms) %>% 
                          pivot_wider(names_from = s4bq0, values_from = c(s4bq5:s4bq10))

#all data should be tidy by household  and clust now :)


