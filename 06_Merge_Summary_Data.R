##############################################
#Program to assess bias in who the campaign is contacting and/or being successful in making contacts with. 
# Author: KA
setwd("~/Desktop/R/EvictionLab")
rm(list = ls())
source("header.R")
##############################################

load("state_sum_gen_orders.RData") 

base_set <- state_sum_gen_orders

load("state_sum_moratoriums.RData") 

mora_set <- state_sum_moratoriums

load("state_sum_moratoriums_max.RData") 

mora_max_set <- state_sum_moratoriums_max

load("state_sum_Cares_Act.RData") 

cares_set <- state_sum_Cares_Act

base_set2 <-  sqldf("Select A.*,
                            B.source_filter_counts as top_mora_count_flag, 
                            B.source_filter_consec_days as most_days_acted_flag, 
                            B.score_avg_mora_term as avg_mora_score,
                            B.total_days_of_action as total_mora_days, 
                            B.counts as total_mora_acts,
                            B.day_first_action as first_mora_day,
                            B.action_thru_date as latest_mora_day,
                            B.score_avg_st1_score,
                            B.score_avg_st2_score,
                            B.score_avg_st3_score,
                            B.score_avg_st4_score,
                            B.score_avg_st5_score,
                            B.stage_max
                          from base_set as A
                          left join mora_set as B
                          on A.state_mod = B.state_mod AND
                             A.Source_of_Action = B.Source_of_Action ")


base_set3 <-  sqldf("Select A.*,
                            B.source_filter_counts as top_moramax_count_flag, 
                            B.source_filter_consec_days as most_days_acted_moramax_flag, 
                            B.max_score_filter as moramax_source_flag,
                            B.max_score,
                            B.total_days_of_action as total_moramax_days, 
                            B.counts as total_moramax_acts,
                            B.day_first_action as first_moramax_day,
                            B.action_thru_date as latest_moramax_day
                          from base_set2 as A
                          left join mora_max_set as B
                          on A.state_mod = B.state_mod AND
                             A.Source_of_Action = B.Source_of_Action ")

base_set3$first_mora_day <- as.Date(base_set3$first_mora_day, origin = "1970-01-01")
base_set3$latest_mora_day <- as.Date(base_set3$latest_mora_day, origin = "1970-01-01")
base_set3$first_moramax_day <- as.Date(base_set3$first_moramax_day, origin = "1970-01-01")
base_set3$latest_moramax_day <- as.Date(base_set3$latest_moramax_day, origin = "1970-01-01")

base_set4 <-  sqldf("Select A.*,
                            B.source_filter_counts as cares_count_flag, 
                            B.source_filter_consec_days as cares_days_acted_flag, 
                            B.total_days_of_action as total_cares_days, 
                            B.counts as total_cares_acts,
                            B.day_first_action as first_cares_day,
                            B.action_thru_date as latest_cares_day
                          from base_set3 as A
                          left join cares_set as B
                          on A.state_mod = B.state_mod AND
                             A.Source_of_Action = B.Source_of_Action ")


base_set4$first_cares_day <- as.Date(base_set4$first_cares_day, origin = "1970-01-01")
base_set4$latest_cares_day <- as.Date(base_set4$latest_cares_day, origin = "1970-01-01")

write.csv(base_set4, file = "evictions_state_consolidated_data.csv", na='')

#write.xlsx(base_set4, "evictions_state_consolidated_data.xlsx")

