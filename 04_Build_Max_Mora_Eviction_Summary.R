##############################################
#Program to summarize maximum coverage of eviction orders by source and state. 
# Author: KA
setwd("~/Desktop/R/EvictionLab")
rm(list = ls())
source("header.R")
##############################################

#Load in cleaned data
load("state_mora_cleaned.RData")

master_sub <- sqldf("Select state_mod,
                            Source_of_Action, 
                            Name_of_Source,
                            group_id,
                            date_eff_fmt, 
                            Expired_Replaced,
                            new_ex_date, 
                            days_btwn as days, 
                            bp_score
                          from state_mora_cleaned
                     order by group_id, state_mod, date_eff_fmt")

state_sum1 <- sqldf("Select state_mod,
                            Source_of_Action, 
                            Name_of_Source,
                            group_id,
                            bp_score,
                            count(Source_of_Action) as counts,
                            min(date_eff_fmt) as day_first_action, 
                            max(new_ex_date) as action_thru_date, 
                            sum(days) as total_days_of_action 
                          from master_sub
                          group by group_id, bp_score
                          order by group_id, state_mod, Source_of_Action, bp_score")



state_sum1$day_first_action <- as.Date(state_sum1$day_first_action, origin = "1970-01-01")

state_sum1$action_thru_date <- as.Date(state_sum1$action_thru_date, origin = "1970-01-01")

state_sum1$action_range <- as.numeric(state_sum1$action_thru_date - state_sum1$day_first_action)

state_sum1$test <- as.numeric(state_sum1$total_days_of_action - state_sum1$action_range)

state_sum2 <- sqldf("Select state_mod,
                            max(action_range) as range_max, 
                            max(counts) as counts_max, 
                            max(total_days_of_action) as consec_days_max,
                            max(bp_score) as max_score
                          from state_sum1
                          group by state_mod
                          order by state_mod")

state_sum3 <-  sqldf("Select A.*,
                            B.range_max, 
                            B.counts_max, 
                            B.consec_days_max,
                            B.max_score
                          from state_sum1 as A
                          left join state_sum2 as B
                          on A.state_mod = B.state_mod")

state_sum3$source_filter_range[state_sum3$action_range == state_sum3$range_max] <- 1

state_sum3$source_filter_counts[state_sum3$counts == state_sum3$counts_max] <- 1

state_sum3$source_filter_consec_days[state_sum3$total_days_of_action == state_sum3$consec_days_max] <- 1

state_sum3$max_score_filter[state_sum3$bp_score == state_sum3$max_score] <- 1

state_sum4 <- state_sum3[state_sum3$max_score_filter == 1,]

state_sum4 <- state_sum4[!is.na(state_sum4$state_mod),]


state_sum_moratoriums_max <- state_sum4
save(state_sum_moratoriums_max, file = "state_sum_moratoriums_max.RData")

