##############################################
#Program to group coverage of general eviction orders by source and state. 
# Author: KA
setwd("~/Desktop/R/EvictionLab")
rm(list = ls())
source("header.R")
##############################################

#Load in cleaned data
load("master_raw_cleaned.RData") 

#Combine State Names and Source of Action to create a unique identifier for grouping the same kinds of orders in each state. 
master_raw_cleaned$lag_ref <- paste(master_raw_cleaned$state_mod, master_raw_cleaned$Source_of_Action, sep = "")
master_raw_cleaned$group_id <- master_raw_cleaned %>% group_indices(lag_ref) 

#Sort data
master_clean_temp1 <- sqldf("Select *
                          from master_raw_cleaned
                          order by state_mod, group_id")

#Drop variables not needed for this analysis. 
#master_clean_temp1 <- master_clean_temp1[ -c(1,15:49) ]


#Convert dataframe to datatable to be able to use datatable specific functions 
master_clean_temp2 <- data.table(master_clean_temp1)

master_clean_temp2 <- master_clean_temp2[order(group_id, -date_eff_fmt),]

#We seek to determine what the cumulative period of coverage was for orders from a particular source (without regard to 
#the order's strength of protection). To do this, we will first need to come up with precise start and stop dates for each order.
#In instances where an order is replaced by a more recent order, even though it was initially meant to carry through for a longer 
#period of time, we will want to record the old order's date as expiring on the day of the new order. This way, we will be able
#to identify gaps in coverage for specific orders too. To do this, we will make use of the fact that a more recent order's start 
#date can be treated as an older order's end date (aka the row below it). 

#Take the effective date of the row above and set it as a potential expiration date for the current row. 
master_clean_temp2[, lag_value:=c(NA, date_eff_fmt[-.N]), by=group_id]

#Reformat variable as date
master_clean_temp2$lag_value_2 <- as.Date(master_clean_temp2$lag_value, origin = "1970-01-01")

#Create ID that can be match rows back correctly. 
master_clean_temp2$ID <- seq.int(nrow(master_clean_temp2))
master_clean_temp2$date_mod <- gsub("/", "", master_clean_temp2$Expiration_Date)

#For effective dates with actual date values, test if they run past current date. 
subset_w_dates <- master_clean_temp2[!is.na(master_clean_temp2$date_ex_fmt),]

currentdate <- Sys.Date()

#If they do, set the new expiration date as today since we can't know whether the orders will actually carry through beyond today. 
subset_w_dates$thru_today <- subset_w_dates$date_ex_fmt > currentdate
subset_w_dates$new_ex_date[subset_w_dates$thru_today == TRUE] <- currentdate 

#If they don't, set keep the expiration date the same (aka set new_ex_date = old_ex_date). 
subset_w_dates$new_ex_date[subset_w_dates$thru_today == FALSE] <- subset_w_dates$date_ex_fmt[subset_w_dates$thru_today == FALSE] 
subset_w_dates$new_ex_date <- as.Date(subset_w_dates$new_ex_date, origin = "1970-01-01")

#For effective dates with values other than just a date, follow methodology rules. 
subset_amb_dates <- master_clean_temp2[is.na(master_clean_temp2$date_ex_fmt),]

#Drop rows where the expired_replaced information is missing. 
subset_amb_dates2 <- subset_amb_dates[!is.na(subset_amb_dates$Expired_Replaced),]

#When an order is listed as expiring in part or full, assume the order expired on the day a newer order took effect. 
subset_amb_dates2$new_ex_date[subset_amb_dates2$Expired_Replaced == "Y"] <- subset_amb_dates2$lag_value_2[subset_amb_dates2$Expired_Replaced == "Y"] 
subset_amb_dates2$new_ex_date[subset_amb_dates2$Expired_Replaced == "Partial"] <- subset_amb_dates2$lag_value_2[subset_amb_dates2$Expired_Replaced == "Partial"] 
subset_amb_dates2$new_ex_date <- as.Date(subset_amb_dates2$new_ex_date, origin = "1970-01-01")

#When an order is listed as not yet expired, set the expired date as today's date. 
subset_amb_dates2$new_ex_date[subset_amb_dates2$Expired_Replaced == "N"] <- currentdate


#Merge back on subset datasets to master to have a complete list of new_ex_date. 

#First merge the two subsets together
subs_joined <- rbind(subset_w_dates, subset_amb_dates2, fill = TRUE)

#Then merge the combined subsets to the master data. 
master_clean_temp3 <- sqldf("Select A.*, B.new_ex_date 
                                from master_clean_temp2 as A 
                                left join subs_joined as B 
                                on A.ID = B.ID")

master_clean_temp4 <- master_clean_temp3[!is.na(master_clean_temp3$new_ex_date),]


#In instances where there was a newer order found, set the expiration date as the earlier date between the start of the newer
#order or the listed expiration date. 
master_clean_temp4$ex_min[!is.na(master_clean_temp4$lag_value_2)] <- pmin(master_clean_temp4$new_ex_date[!is.na(master_clean_temp4$lag_value_2)], master_clean_temp4$lag_value_2[!is.na(master_clean_temp4$lag_value_2)])

#If no newer order is found, set the expiration date to the listed expiration date. 
master_clean_temp4$ex_min[is.na(master_clean_temp4$lag_value_2)] <- master_clean_temp4$new_ex_date[is.na(master_clean_temp4$lag_value_2)]

master_clean_temp4$ex_min <- as.Date(master_clean_temp4$ex_min, origin = "1970-01-01")

#Calculate days between the effective date and expiration date for each order. 
master_clean_temp4$days_btwn <- master_clean_temp4$ex_min - master_clean_temp4$date_eff_fmt

master_sub <- sqldf("Select state_mod,
                            Source_of_Action, 
                            Name_of_Source,
                            group_id,
                            date_eff_fmt, 
                            Expired_Replaced,
                            new_ex_date, 
                            days_btwn as days
                          from master_clean_temp4
                     order by group_id, state_mod, date_eff_fmt")

state_sum1 <- sqldf("Select state_mod,
                            Source_of_Action, 
                            Name_of_Source,
                            Type_of_Action,
                            group_id,
                            count(Source_of_Action) as counts,
                            min(date_eff_fmt) as day_first_action, 
                            max(new_ex_date) as action_thru_date, 
                            sum(days_btwn) as total_days_of_action
                          from master_clean_temp4
                          group by group_id
                          order by group_id, state_mod, Source_of_Action")

state_sum1$day_first_action <- as.Date(state_sum1$day_first_action, origin = "1970-01-01")

state_sum1$action_thru_date <- as.Date(state_sum1$action_thru_date, origin = "1970-01-01")

state_sum1$action_range <- as.numeric(state_sum1$action_thru_date - state_sum1$day_first_action)

state_sum1$test <- as.numeric(state_sum1$total_days_of_action - state_sum1$action_range)

state_sum2 <- sqldf("Select state_mod,
                            max(action_range) as range_max, 
                            max(counts) as counts_max, 
                            max(total_days_of_action) as consec_days_max,
                            min(day_first_action) as first_to_act_date
                          from state_sum1
                          group by state_mod
                          order by state_mod")

state_sum3 <-  sqldf("Select A.*,
                            B.range_max, 
                            B.counts_max, 
                            B.consec_days_max,
                            B.first_to_act_date
                          from state_sum1 as A
                          left join state_sum2 as B
                          on A.state_mod = B.state_mod")

state_sum3$first_to_act_date <- as.Date(state_sum3$first_to_act_date, origin = "1970-01-01")

#Create flag for whether source represents the maximum range of coverage for a state. 
state_sum3$source_filter_range[state_sum3$action_range == state_sum3$range_max] <- 1

#Create flag for whether source represents the most issues ordered in a state. 
state_sum3$source_filter_counts[state_sum3$counts == state_sum3$counts_max] <- 1

#Create flag for whether source represents the most day covered by orders in a state. 
state_sum3$source_filter_consec_days[state_sum3$total_days_of_action == state_sum3$consec_days_max] <- 1

#Create flag for whether source was the first to act in the state. 
state_sum3$first_to_act_date_filter[state_sum3$day_first_action == state_sum3$first_to_act_date] <- 1


state_sum_gen_orders <- state_sum3

save(state_sum_gen_orders, file = "state_sum_gen_orders.RData")

