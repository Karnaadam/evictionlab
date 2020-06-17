##############################################
#Program to import and clean eviction moratorium data
# Author: KA
setwd("~/Desktop/R/EvictionLab")
rm(list = ls())
source("header.R")
##############################################

#Specify which file from your directory to import. Assumes csv format. 
myfile <- file.choose()

#Import the file
master_raw <-  read.delim(myfile, header = TRUE, sep = ",", dec = ".", na.strings = c("", "NA"))

#Drop all rows that contain only missing data. 
master_clean_temp1 <- master_raw[rowSums(is.na(master_raw)) != ncol(master_raw),]

#Replace all periods in varnames with dashes for ease of use. 
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "\\.", replacement = "_")  

#Rename long variable names for ease of use.: 
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Order_declaration_affects_residential_eviction_civil_proceedings", replacement = "Affects_Res_Evic")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Hyperlink_to_Source__Not_Included_in_State_Summary_Row_", replacement = "No_Hyperlink")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Date_Moratorium_Began_to_Lift", replacement = "Mora_Lift_Date")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Prohibits_law_enforcement_from_executing_new_and_past_orders_of_eviction", replacement = "No_Evic_Enforce_By_PD")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Status_of_non_emergency_civil_court_proceedings__", replacement = "Non_E_Civil_Status")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Remote_hearings_allowed_in_non_emergency_civil_cases", replacement = "R_Hearing_Non_E_Civil")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Applies_to_civil_cases__which_should_include_eviction_cases_", replacement = "Evic_indirect_civil")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Applies_to_eviction_cases_directly", replacement = "Direct_Evic_App")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Applies_to_commercial_eviction_directly", replacement = "C_Evic_App")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Applies_to_foreclosure_or_foreclosure_eviction_cases_directly", replacement = "FC_Evic_App")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Exempts_criminal_activity__damage_to_property__emergency__nuisance_or_cases_to_protect_public_health_from_eviction_freeze_", replacement = "Exempts_crim_others")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Only_applies_to_certain_eviction_cases", replacement = "Select_Evic_App")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "If_limited_to_certain_eviction_cases__the_freeze_only_applies_to_these_cases___", replacement = "Select_Apps")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Tolls__extends_or_stays_court_deadlines", replacement = "Affects_Court_Deadlines")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Date_to_which_deadlines_are_tolled", replacement = "Date_Tolled")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Suspends_all_five_stages_of_eviction__notice__filing__hearing__ruling__execution__", replacement = "All_five_stages")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "X1__Suspends_Notice_of_Eviction_to_Tenant", replacement = "Stage_1")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "X2__Suspends_Filing_of_Eviction_Claim", replacement = "Stage_2")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "X3__Suspends_Hearings_on_Eviction", replacement = "Stage_3")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "X4__Stays_Order__Judgment_or_Writ_of_Eviction_", replacement = "Stage_4")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "X5__Suspends_enforcement_of_new_order_of_eviction", replacement = "Stage_5")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Prohibits_issuance_of_late_fees_to_landlord", replacement = "No_LL_Late_fees")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Requires_certification_that_property_is_not_covered_under_CARES_Act_Moratorium", replacement = "Cares_Act")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Order_Declaration_discusses_effect_of_housing_on_public_health", replacement = "Discusses_PH")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Order_Declaration_discusses_economic_consequences_on_housing", replacement = "Discusses_Econ")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Brief_Summary_of_What_Order_Does", replacement = "Order_Summary")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Moratorium_Extended_Past_Emergency_Declaration", replacement = "Mora_Goes_Past_ED")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "No_Reporting_to_Credit_Bureau", replacement = "No_Credit_Report")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Grace_Period_to_Pay_Rent", replacement = "Rent_Grace_Period")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Length_of_Time_for_No_Late_Fees__mm_dd_yyyy", replacement = "No_Late_Fees_Length")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Length_of_Time_for_No_Rent_Raises_mm_dd_yyyy", replacement = "No_Rent_Raise_Length")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Length_of_Time_for_Foreclosure_Moratorium_mm_dd_yyyy", replacement = "FC_Mora_Length")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Length_of_time_for_Housing_Stabilization_mm_dd_yyyy", replacement = "House_Stable_Length")  
names(master_clean_temp1) <- gsub(x = names(master_clean_temp1), pattern = "Foreclosure_Moratorium", replacement = "FC_Mora")  

#Drop Columns Not relevant to Data Analysis
master_clean_temp2 <- master_clean_temp1[ -c(1) ]
master_clean_temp2 <- master_clean_temp2[,!grepl("SUMMARY",names(master_clean_temp2))]
master_clean_temp2 <- master_clean_temp2[,!grepl("Hyperlink",names(master_clean_temp2))]
master_clean_temp2 <- master_clean_temp2[,!grepl("STATUS",names(master_clean_temp2))]

#Drop Rows Not relevant to Data Analysis
master_clean_temp3 <- master_clean_temp2[!grepl("SUMMARY",master_clean_temp2[,1]),]

#Drop Rows with missing data in key variables 
master_clean_temp4 <- master_clean_temp3[!is.na(master_clean_temp3$Source_of_Action),] 
master_clean_temp4 <- master_clean_temp4[!is.na(master_clean_temp4$Expiration_Date),]
master_clean_temp4 <- master_clean_temp4[!is.na(master_clean_temp4$Effective_Date),]
master_clean_temp5 <- master_clean_temp4[!master_clean_temp4$Expired_Replaced == "N/A",] #I assume that these orders have not been implemented yet (i.e. are pending legislative actions)

#Strip spaces from state names so that they can be properly grouped
master_clean_temp5$state_mod <- gsub(x = master_clean_temp5$STATE, pattern = " ", replacement = "")  

#Clean and Format Date Variables
master_clean_temp5$date_trim <- gsub("2020", "20", master_clean_temp5$Effective_Date)
master_clean_temp5$date_trim <- gsub("03", "3", master_clean_temp5$date_trim)
master_clean_temp5$date_trim <- gsub("04", "4", master_clean_temp5$date_trim)
master_clean_temp5$date_eff_fmt <- as.Date(master_clean_temp5$date_trim, format = "%m/%d/%y")

master_clean_temp5$date_trim <- gsub("2020", "20", master_clean_temp5$Expiration_Date)
master_clean_temp5$date_trim <- gsub("03", "3", master_clean_temp5$date_trim)
master_clean_temp5$date_trim <- gsub("04", "4", master_clean_temp5$date_trim)
master_clean_temp5$date_ex_fmt <- as.Date(master_clean_temp5$date_trim, format = "%m/%d/%y")

master_raw_cleaned <- master_clean_temp5

#Save dataset
save(master_raw_cleaned, file = "master_raw_cleaned.RData")

