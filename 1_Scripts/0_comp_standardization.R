# Script to standardize survey data from each location (SF, Delta, VA, SC)
library(tidyverse)


# Data Upload -------------------------------------------------------------
ba <- read.csv("0_Data/template_data/original_datasets/CA-data.csv", na.strings=c("","NA"))[-c(1:2),] # adds NA for blank rows & remove first two rows
va <- read.csv("0_Data/template_data/original_datasets/VA-data.csv", na.strings=c("","NA"))[-c(1:2),] 
sc <- read.csv("0_Data/template_data/original_datasets/SC-data.csv", na.strings=c("","NA"))[-c(1:2),]
delta <- read.csv("0_Data/template_data/original_datasets/Delta-data.csv") # cleaned & de-identified version of survey data

ba <- read.csv("0_Data/template_data/standardized_datasets/ba.csv", na.strings=c("","NA"))[-c(1:2),] # adds NA for blank rows & remove first two rows
va <- read.csv("0_Data/template_data/standardized_datasets/va.csv", na.strings=c("","NA"))[-c(1:2),] 
sc <- read.csv("0_Data/template_data/standardized_datasets/sc.csv", na.strings=c("","NA"))[-c(1:2),]
delta <- read.csv("0_Data/template_data/standardized_datasets/delta.csv") # cleaned & de-identified version of survey data

table(ba$collabActivities)

# Column Standardization --------------------------------------------------
## Delta -------------------------------------------------------------------
str(delta)

## Combine columns
delta_missing_orgs <- delta %>%
  pivot_longer(cols = starts_with("missing_org"), names_to = "missing_org_column", values_to = "missing_org_name") %>%
  filter(missing_org_name != "") %>%
  group_by(ResponseId) %>%
  summarize(missing_collaborators = paste0(missing_org_name, collapse = "; "))
str(delta_missing_orgs)

## Rename column name to match variable code listed in comparative codebook
delta_std <- delta %>%
  rename(involve = involvement, 
         org = primary_org,
         org_type = org_type_assigned,
         location = scale_location,
         concernShort = concern_25_slr,
         concernLong = concern_2100_slr,
         collabActivities = collab_acts,
         forum_other = forum_involvement_o, 
         forumProgress_art = forum2_action_art,
         forumProgress_baycan = forum2_action_baycan,
         forumProgress_crc = forum2_action_crc,
         forumProgress_da = forum2_action_da,
         forumProgress_sfep = forum2_action_sfep,
         forumProgress_ccccl = forum2_action_ccccl,
         forumProgress_ccse = forum2_action_ccse,
         forumProgress_sccc = forum2_action_sccc,
         forumProgress_rpp = forum2_action_rpp,
         forumProgress_sr = forum2_action_sr,
         forumProgress_sjrcc = forum2_action_sjrcc,
         forumProgress_sbrr = forum2_action_sbrr,
         forumProgress_yccac = forum2_action_yccac,
         forumProgress_ycrrc = forum2_action_ycrrc,
         forumProgress_ryc = forum2_action_ryc,
         forumProgress_other = forum_action_o)

# Reorg network data to add to delta dataset
delta_network <- delta_edgelist %>%
  left_join(., select(delta_roster, clean_org_name, org_type), by = c("alter" = "clean_org_name")) %>%
  mutate(org_type = case_when(
    org_type %in% c("local_gov", "mj_entity", "reg_gov", "enviro_sd","service_sd", "water_sd", "fire_sd") ~ "local_reg_gov",
    org_type %in% c("msg", "cbo_ngo", "industry", "ecr", "enviro", NA, "tribe", "ag_entity", "service_provider", "individual") ~ "ngo",
    TRUE ~ org_type)) %>%
  pivot_wider(names_from = "org_type", values_from = "alter") %>%
  group_by(ResponseId) %>%
  summarise(across(c(fed_gov, state_gov, local_reg_gov, ngo), ~ str_c(na.omit(.), collapse = "; ")), .groups = "drop") %>%
  rename(collaboratorsFed = fed_gov,
         collaboratorsState = state_gov,
         collaboratorsLocalRegional = local_reg_gov,
         collaboratorsNGO = ngo)

# Attach network data to delta dataset
delta_std <- delta_std %>%
  left_join(., delta_network) %>%
  left_join(., delta_missing_orgs)

# Select just comparative columns
delta_std <- delta_std %>%
  select(ResponseId, involve, org, org_type, tasks, location, concernShort, concernLong, top_sector, collabActivities, barriers, forum_art, forum_baycan, forum_crc, forum_da, forum_sfep, forum_ccccl, forum_ccse, forum_sccc, forum_rpp, forum_sr, forum_sjrcc, forum_sbrr, forum_yccac, forum_ycrrc, forum_ryc, forum_other, starts_with("forumProgress"), starts_with("collaborators"), missing_collaborators) %>%
  mutate_all(na_if,"") # make sure blank cells have NA values 


## VA ----------------------------------------------------------------------
str(va)

## Several variables have an other column with text- combine columns to retain that information
va <- va %>%
  unite("org_type", c("X4", "X4_14_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("X9", c("X9", "X9_11_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("X10", c("X10", "X10_13_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("X11", c("X11", "X11_13_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("X12", c("X12", "X12_15_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("X13", c("X13", "X13_13_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("X15.6", c("X15.6", "X15.6_12_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("X15.6.1", c("X15.6.1", "X15.6_12_TEXT.1"), sep = ": ", na.rm = TRUE) %>%
  unite("X15.6.2", c("X15.6.2", "X15.6_12_TEXT.2"), sep = ": ", na.rm = TRUE) %>%
  unite("X15.6.3", c("X15.6.3", "X15.6_12_TEXT.3"), sep = ": ", na.rm = TRUE) %>%
  unite("X15.6.4", c("X15.6.4", "X15.6_12_TEXT.4"), sep = ": ", na.rm = TRUE) %>%
  unite("X17", c("X17", "X17_11_TEXT"), sep = ": ", na.rm = TRUE)
  
va_std <- va %>%
  rename(involve = X1, 
         org = X2_5,
         title = X3, 
         informShort = X7_1,
         informLong = X7_2,
         concernShort = X6_1,
         concernLong = X6_2,
         agree = X8,
         science = X9,
         impact = X5,
         top_sector = X10,
         policy = X11,
         collabActivities = X12,
         barriers = X13,
         resources = X14, 
         forum_1 = X15_1,
         forum_2 = X15_2,
         forum_3 = X15_3,
         forum_4 = X15_4,
         forum_5 = X15_5,
         forumImpact_1 = X15.1_1,
         forumImpact_2 = X15.1_2,
         forumImpact_3 = X15.1_3,
         forumImpact_4 = X15.1_4,
         forumImpact_5 = X15.1_5,
         forumGoals_1 = X15.2_1,
         forumGoals_2 = X15.2_2,
         forumGoals_3 = X15.2_3,
         forumGoals_4 = X15.2_4,
         forumGoals_5 = X15.2_5,
         forumProgress_1 = X15.3_1,
         forumProgress_2 = X15.3_2,
         forumProgress_3 = X15.3_3,
         forumProgress_4 = X15.3_4,
         forumProgress_5 = X15.3_5,
         forumFairness_1 = X15.4_1,
         forumFairness_2 = X15.4_2,
         forumFairness_3 = X15.4_3,
         forumFairness_4 = X15.4_4,
         forumFairness_5 = X15.4_5,
         forumProcess_1 = X15.5_1,
         forumProcess_2 = X15.5_2,
         forumProcess_3 = X15.5_3,
         forumProcess_4 = X15.5_4,
         forumProcess_5 = X15.5_5,
         forumBarriers_1 = X15.6,
         forumBarriers_2 = X15.6.1,
         forumBarriers_3 = X15.6.2,
         forumBarriers_4 = X15.6.3,
         forumBarriers_5 = X15.6.4,
         collaboratorFactors = X17,
         collaboratorsFed = X16_5,
         collaboratorsState = X16_6,
         collaboratorsLocalRegional = X16_7,
         collaboratorsNGO = X16_8,
         missing_collaborators = X18
  )

# Select just comparative columns
va_std <- va_std %>%
  select(ResponseId, involve, org, org_type, title, informShort, informLong, concernShort, concernLong, agree, science, impact, top_sector, policy, collabActivities, barriers, starts_with("forum"), starts_with("collaborators"), missing_collaborators) %>%
  mutate_all(na_if,"") # make sure blank cells have NA values 


## SC ----------------------------------------------------------------------
str(sc)

## SC had rank questions for sectors of concern and policy preferences. In order to match the other surveys, I filtered the top three answers for each question & dropped the rank portion of these data
sc_top_concerns <- sc %>%
  select(ResponseId, starts_with("Q15")) %>%
  select(-Q15_14_TEXT) %>% # remove text for other responses 
  pivot_longer(-ResponseId, names_to = "sector", values_to = "rank") %>%
  mutate(sector = case_when(
    sector == "Q15_1" ~ "Transportation Infrastructure",
    sector == "Q15_2" ~ "Water Supply Infrastructure",
    sector == "Q15_3" ~ "Waste and Stormwater",
    sector == "Q15_4" ~ "Energy Infrastructure",
    sector == "Q15_5" ~ "Ecosystems Health",
    sector == "Q15_6" ~ "Coastlines",
    sector == "Q15_7" ~ "Commercial Developments",
    sector == "Q15_8" ~ "Historically Underserved Communities",
    sector == "Q15_9" ~ "Economic Growth",
    sector == "Q15_10" ~ "Property Values",
    sector == "Q15_11" ~ "Availability of Housing",
    sector == "Q15_12" ~ "Affordability of Housing",
    sector == "Q15_13" ~ "Public Health",
    sector == "Q15_14" ~ "Other"
  ))%>%
  filter(rank %in% c(1, 2, 3)) %>%
  group_by(ResponseId) %>%
  summarize(top_sector = paste0(sector, collapse = ", "))

sc_policy_preferences <- sc %>%
  select(ResponseId, starts_with("Q16")) %>%
  select(-Q16_13_TEXT) %>% # remove text for other responses 
  pivot_longer(-ResponseId, names_to = "policy", values_to = "rank") %>%
  mutate(policy = case_when(
    policy == "Q16_1" ~ "Create a regional sea-level rise adaptation plan for Charleston, Berkeley, and Dorchester counties",
    policy == "Q16_2" ~ "Complete vulnerability assessments for the three counties as soon as possible",
    policy == "Q16_3" ~ "Pass a tax measure at the local/county/state level to address sea-level rise",
    policy == "Q16_4" ~ "Develop faster/more efficient permitting processes that incorporate considerations of sea-level rise",
    policy == "Q16_5" ~ "Create a single information platform concerning the status of projects related to sea-level rise in the Charleston area, cost projections and scenarios",
    policy == "Q16_6" ~ "Focus attention on the impact of sea-level rise on historically underserved communities in the area",
    policy == "Q16_7" ~ "Promote projects aimed at accommodating sea-level rise with infrastructure",
    policy == "Q16_8" ~ "Promote projects with different or innovative design solutions",
    policy == "Q16_9" ~ "Support local jurisdictions to respond to sea-level rise threats as they see fit",
    policy == "Q16_10" ~ "Establish a new regional authority to address sea-level rise in the area",
    policy == "Q16_11" ~ "Empower an existing regional authority to address sea-level rise in the area",
    policy == "Q16_12" ~ "Empower an existing regional authority to address sea-level rise in the area",
    policy == "Q16_13" ~ "Other",
    policy == "Q16_14" ~ "Build a seawall"
  )) %>%
  filter(rank %in% c(1, 2, 3)) %>%
  group_by(ResponseId) %>%
  summarize(policy = paste0(policy, collapse = "; "))

# Add new columns to original data
sc <- sc %>%
  left_join(., sc_top_concerns) %>%
  left_join(., sc_policy_preferences)

# Combine "other" columns with main column
sc <- sc %>%
  unite("org_type", c("Q4", "Q4_10_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("Q6", c("Q6", "Q6_9_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("Q12", c("Q12", "Q12_11_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("Q17", c("Q17", "Q17_14_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("Q18", c("Q18", "Q18_14_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("Q20", c("Q20", "Q20_5_TEXT", "Q20_6_TEXT", "Q20_7_TEXT"), sep = ", ", na.rm = TRUE) %>% # collab initiative participation
  unite("Q20.6A", c("Q20.6A", "Q20.6A_11_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("Q20.6B", c("Q20.6B", "Q20.6B_11_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("Q20.6C", c("Q20.6C", "Q20.6C_11_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("Q20.6D", c("Q20.6D", "Q20.6D_11_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("Q22", c("Q22", "Q22_10_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("Q23_4", c("Q23_4", "Q23_5", "Q23_6", "Q23_7", "Q23_8"), sep = ", ", na.rm = TRUE) #missing collaborators
  
sc_std <- sc %>%
  rename(involve = QID1, 
         org_representation = Q2,
         org =  Q3_1,
         title = Q5, 
         tasks = Q6,
         affiliations = Q7,
         location = Q58,
         informShort = Q9_1,
         informLong = Q9_2,
         concernShort = Q10_1,
         concernLong = Q10_2,
         agree = Q11,
         science = Q12,
         info = Q13,
         impact = Q14,
         collabActivities = Q17,
         barriers = Q18,
         resources = Q19, 
         forum_name = Q20,
         forummImpact_DD = Q20.1A,
         forumImpact_RMSI = Q20.1B,
         forumImpact_CSLRSP = Q20.1C,
         forumImpact_CRHMP = Q20.1D,
         forummGoals_DD = Q20.2A,
         forumGoals_RMSI = Q20.2B,
         forumGoals_CSLRSP = Q20.2C,
         forumGoals_CRHMP = Q20.2D,
         forummProgress_DD = Q20.3A,
         forumProgress_RMSI = Q20.3B,
         forumProgress_CSLRSP = Q20.3C,
         forumProgress_CRHMP = Q20.3D,
         forummFairness_DD = Q20.4A,
         forumFairness_RMSI = Q20.4B,
         forumFairness_CSLRSP = Q20.4C,
         forumFairness_CRHMP = Q20.4D,
         forummProcess_DD = Q20.5A,
         forumProcess_RMSI = Q20.5B,
         forumProcess_CSLRSP = Q20.5C,
         forumProcess_CRHMP = Q20.5D,
         forummBarriers_DD = Q20.6A,
         forumBarriers_RMSI = Q20.6B,
         forumBarriers_CSLRSP = Q20.6C,
         forumBarriers_CRHMP = Q20.6D,
         collaboratorFactors = Q22,
         collaboratorsFed = Q21_1,
         collaboratorsState = Q21_2,
         collaboratorsLocalRegional = Q21_3,
         collaboratorsNGO = Q21_4,
         missing_collaborators = Q23_4
  )  

# Select just comparative columns
sc_std <- sc_std %>%
  select(ResponseId, involve, org_representation, org, org_type, title, tasks, affiliations, location, informShort, informLong, concernShort, concernLong, agree, science, info, impact, top_sector, policy, collabActivities, barriers, resources, starts_with("forum"), starts_with("collaborators"), missing_collaborators) %>%
  mutate_all(na_if,"") # make sure blank cells have NA values 

## BA ----------------------------------------------------------------------
str(ba)

# Combine "other" columns with main column & combine instances where several columns are related to one variable (e.g., collapse list of federal organization collaborators into just one column)
ba <- ba %>%
  unite("org_type", c("Q4", "Q4_17_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("tasks", c("Q8", "Q8_10_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("info", c("Q32", "Q32_13_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("science", c("Q15", "Q15_10_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("top_sector", c("Q17_0_GROUP", "Q17_13_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("policy", c(starts_with("Q18")), sep = ", ", na.rm = TRUE) %>%
  unite("collabActivities", c("Q19", "Q19_15_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("barriers", c("Q20", "Q20_13_TEXT"), sep = ": ", na.rm = TRUE) %>%
  unite("affiliations", c(starts_with("Q40_1")), sep = ", ", na.rm = TRUE) %>% # affiliations
  unite("collaboratorFactors", c(starts_with("Q25")), sep = ", ", na.rm = TRUE) %>% # affiliations
  unite("collaboratorFed", c(starts_with("Q24")), sep = ", ", na.rm = TRUE) %>%
  unite("collaboratorState", c(starts_with("Q36")), sep = ", ", na.rm = TRUE) %>%
  unite("collaboratorLocalRegional", c(starts_with("Q44")), sep = ", ", na.rm = TRUE) %>%
  unite("collaboratorNGO", c(starts_with("Q37")), sep = ", ", na.rm = TRUE) %>%
  unite("missing_collaborators", c(starts_with("Q41")), sep = ", ", na.rm = TRUE)

# Rename columns that haven't been updated in the previous step
ba_std <- ba %>%
  rename(involve = Q1, 
         org_representation = Q2,
         org =  Q3_1,
         title = Q6, 
         location = Q10,
         informShort = Q11_1,
         informLong = Q11_2,
         concernShort = Q12_1,
         concernLong = Q12_2,
         agree = Q13,
         impact = Q16,
         resources = Q21, 
         forum_1 = Q34_1,
         forum_2 = Q34_2,
         forum_3 = Q34_3,
         forum_4 = Q34_4,
         forum_5 = Q34_5,
         forumImpact_1 = Q23c_1,
         forumImpact_2 = Q23c_2,
         forumImpact_3 = Q23c_3,
         forumImpact_4 = Q23c_4,
         forumImpact_5 = Q23c_5,
         forumGoals_1 = Q22.1_1,
         forumGoals_2 = Q22.1_1,
         forumGoals_3 = Q22.1_1,
         forumGoals_4 = Q22.1_1,
         forumGoals_5 = Q22.1_1,
         forumProgress_1 = Q22.2_1,
         forumProgress_2 = Q22.2_1,
         forumProgress_3 = Q22.2_1,
         forumProgress_4 = Q22.2_1,
         forumProgress_5 = Q22.2_1,
         forumFairness_1 = Q23b_1,
         forumFairness_2 = Q23b_2,
         forumFairness_3 = Q23b_3,
         forumFairness_4 = Q23b_4,
         forumFairness_5 = Q23b_5,
         forumProcess_1 = Q23a.1_1,
         forumProcess_2 = Q23a.1_2,
         forumProcess_3 = Q23a.1_3,
         forumProcess_4 = Q23a.1_4,
         forumProcess_5 = Q23a.1_5,
         forumBarriers_1 = Q23a.2_1,
         forumBarriers_2 = Q23a.2_2,
         forumBarriers_3 = Q23a.2_3,
         forumBarriers_4 = Q23a.2_4,
         forumBarriers_5 = Q23a.2_5
  )  

# Select just comparative columns
ba_std <- ba_std %>%
  select(ResponseId, involve, org_representation, org, org_type, title, tasks, affiliations, location, informShort, informLong, concernShort, concernLong, agree, science, info, impact, top_sector, policy, collabActivities, barriers, resources, starts_with("forum"), starts_with("collaborators"), missing_collaborators) %>%
  mutate_all(na_if,"") # make sure blank cells have NA values

# Row Cleaning -----------------------------------------------------------
## This next section pulls code from the FV code for the collaboration paper to clean the org names
## Delta -------------------------------------------------------------------
### org ----
# covert everything to lower case like the other datasets 
delta_std$org <- tolower(delta_std$org)

### orgtype
delta_std <- delta_std %>%
  mutate(org_type = case_when(
    org_type == "cbo_ngo" ~ "No-profit organization/Non-governmental organization/Community-based organization",                    
    org_type == "ecr" ~ "Education/Consulting/Research",                                           
    org_type == "enviro" ~ "Environmental Group",                                                     
    org_type == "reg_gov" ~ "Regional government",                                                     
    org_type == "msg" ~ "Multi-stakeholder group",                                                      
    org_type == "local_gov" ~ "Local government (cities, counties)",                                     
    org_type == "fed_gov" ~ "Federal government",                                                      
    org_type == "industry" ~ "Trade/Business/Industry Group",                                           
    org_type == "state_gov" ~ "State government",                                                        
    org_type == "enviro_sd" ~ "Environmental Special District (e.g. Park district, open space district)",
    org_type == "water_sd" ~ "Water Infrastructure Special District (e.g. irrigation district)",        
    org_type == "ag_entity" ~ "Agriculture",                                                  
    org_type == "mj_entity" ~ "Multi-jurisdictional regulatory/planning entity",
    org_type == "service_sd" ~ "Service Special District", 
    org_type == "fire_sd" ~ "Fire Special District", 
    org_type == "tribe" ~ "Tribal Entity",
    org_type == "service_provider" ~ "Service Provider"
  ))

table(delta_std$org_type)

## VA ----------------------------------------------------------------------
### org ----
sort(va_std$org)
va_std$org <- trimws(va_std$org)

# delete everything after a comma
va_std$org <- gsub("(.*),.*", "\\1", va_std$org)
# create a space where there's a dash
va_std$org <- gsub("-", " ", va_std$org)
# replace & with and
va_std$org <- gsub("\\&", "and", va_std$org)
va_std$org <- gsub("\\+", "and", va_std$org)
# delete everything in brackets
va_std$org <- str_replace(va_std$org, " \\s*\\([^\\)]+\\)", "")
# find column
which(colnames(va_std)=='org')
va_std$org <- str_trim(va_std$org)
va_std$org <- tolower(va_std$org)
sort(unique(va_std$org))
va_std$org  <- va_std$org %>%
  case_when(
    grepl("city of hampton public works", .)==TRUE ~ "city of hampton",
    grepl("city of hampton va", .)==TRUE ~ "city of hampton",
    grepl("city of portsmouth va", .)==TRUE ~ "city of portsmouth",
    grepl("odu", .)==TRUE ~ "old dominion university",
    grepl("odu research foundation", .)==TRUE ~ "old dominion university",
    grepl("office of alumni relations   odu", .)==TRUE ~ "old dominion university",
    grepl("old dominion u", .)==TRUE ~ "old dominion university",
    grepl("old dominion university   facilities management", .)==TRUE ~ "old dominion university",
    grepl("old dominion university research foundation", .)==TRUE ~ "old dominion university",
    grepl("southampton county department of community development", .)==TRUE ~ "southampton county",
    .default = .
  )

sort(va_std$org)

### org type ----
va_std$org_type
# delete everything in brackets
va_std$org_type <- str_replace(va_std$org_type, " \\s*\\([^\\)]+\\)", "")
# find column
which(colnames(va_std)=='org_type')
va_std$org_type <- str_trim(va_std$org_type)

sort(va_std$org_type)

# make org dummy variables 

#va_std$org_type <- as.character(va_std$org_type)
#orgSplit <- strsplit(va_std$org_type, ",")

#orgLev <- unique(unlist(orgSplit))

#va_std$orgDummy <- lapply(orgSplit, function(x) table(factor(x, levels = orgLev)))
#sort(va_std$org_type)


va_std$org_type <- str_trim(va_std$org_type)
va_std$org_type <- tolower(va_std$org_type)

sort(va_std$org_type)

va_std$org_type <- gsub("community-based organization,other", "community-based organization", va_std$org_type)
va_std$org_type <- gsub("environmental group,community-based organization", "environmental group", va_std$org_type)
va_std$org_type <- gsub("local government,trade/business/industry group", "local government", va_std$org_type)
va_std$org_type <- gsub("multi-jurisdictional regulatory/planning entity", "multi-jurisdictional", va_std$org_type)
va_std$org_type <- gsub("regional government,local government", "regional government", va_std$org_type)
va_std$org_type <- gsub("state government,education/consulting/research", "state government", va_std$org_type)
va_std$org_type <- gsub("trade/business/industry group,education/consulting/research", "trade/business/industry group", va_std$org_type)

va_std$org_type <- str_replace(va_std$org_type, " \\s*\\([^\\)]+\\)", "")
sort(va_std$org_type)

## SC ----------------------------------------------------------------------
### org -------
sort(sc_std$org)
sc_std$org <- trimws(sc_std$org)

# delete everything after a comma
sc_std$org <- gsub("(.*),.*", "\\1", sc_std$org)
# create a space where there's a dash
sc_std$org <- gsub("-", " ", sc_std$org)
# replace & with and
sc_std$org <- gsub("\\&", "and", sc_std$org)
sc_std$org <- gsub("\\+", "and", sc_std$org)
# delete everything in brackets
sc_std$org <- str_replace(sc_std$org, " \\s*\\([^\\)]+\\)", "")
# find column
which(colnames(sc_std)=='org')
sc_std$org <- str_trim(sc_std$org)
sc_std$org <- tolower(sc_std$org)
sc_std$org  <- sc_std$org %>%
  case_when(
    grepl("berkeley charleston dorchester council of governments", .)==TRUE ~ "bcdcog",
    grepl("charleston county emergency management department", .)==TRUE ~ "charleston county",
    grepl("charleston county government", .)==TRUE ~ "charleston county",
    grepl("charleston county park and recreation commission", .)==TRUE ~ "charleston county",
    grepl("charleston county parks and rec commission", .)==TRUE ~ "charleston county",
    grepl("charleston county public works", .)==TRUE ~ "charleston county",
    grepl("city", .)==TRUE ~ "city of charleston",
    grepl("city of charleston   department of parks", .)==TRUE ~ "city of charleston",
    grepl("city of charleston planning", .)==TRUE ~ "city of charleston",
    grepl("city of charleston planning department", .)==TRUE ~ "city of charleston",
    grepl("city of charleston stormwater services", .)==TRUE ~ "city of charleston",
    grepl("city of charlestonj", .)==TRUE ~ "city of charleston",
    grepl("noaa/national weather service", .)==TRUE ~ "noaa",
    grepl("noaa/national weather service charleston sc", .)==TRUE ~ "noaa",
    grepl("personal research as cofc faculty member", .)==TRUE ~ "college of charleston",
    grepl("riley center", .)==TRUE ~ "college of charleston",
    grepl("ton o kiawah island", .)==TRUE ~ "town of kiawah island",
    grepl("town of mount pleasant planning department", .)==TRUE ~ "town of mount pleasant",
    grepl("us army corps of engineers", .)==TRUE ~ "usace",
    grepl("n/a", .)==TRUE ~ "",
    .default = .
  )

sort(sc_std$org)

### org type -----
# delete everything in brackets
sc_std$org_type <- str_replace(sc_std$org_type, " \\s*\\([^\\)]+\\)", "")
# find column
which(colnames(sc_std)=='org_type')
sc_std$org_type <- str_trim(sc_std$org_type)
sort(sc_std$org_type)
sc_std$org_type <- str_trim(sc_std$org_type)
sc_std$org_type <- tolower(sc_std$org_type)
sort(sc_std$org_type)

# recode multiple choices into one
sc_std$org_type <- gsub("consulting/research,infrastructure", "consulting/research", sc_std$org_type)
sc_std$org_type <- gsub("consulting/research,infrastructure", "infrastructure", sc_std$org_type)
sc_std$org_type <- gsub("consulting/research,other", "consulting/research", sc_std$org_type)
sc_std$org_type <- gsub("consulting/research,education/academic institution", "consulting/research", sc_std$org_type)
sc_std$org_type <- gsub("federal government,state government", "federal government", sc_std$org_type)
sc_std$org_type <- gsub("federal government,regional government,local government,non-profit/non-governmental organization,consulting/research", "federal government", sc_std$org_type)
sc_std$org_type <- gsub("federal government,state government,regional government,local government,consulting/research,education/academic institution", "federal government", sc_std$org_type)
sc_std$org_type <- gsub("federal government,state government", "federal government", sc_std$org_type)
sc_std$org_type <- gsub("federal government,state government,regional government,local government,infrastructure", "federal government", sc_std$org_type)
sc_std$org_type <- gsub("federal government,infrastructure", "federal government", sc_std$org_type)
sc_std$org_type <- gsub("local government,multi-stakeholder group", "local government", sc_std$org_type)
sc_std$org_type <- gsub("local government,non-profit/non-governmental organization", "local government", sc_std$org_type)
sc_std$org_type <- gsub("local government,non-profit/non-governmental organization,consulting/research,education/academic institution", "local government", sc_std$org_type)
sc_std$org_type <- gsub("local government,consulting/research,education/academic institution", "local government", sc_std$org_type)
sc_std$org_type <- gsub("local government,education/academic institution", "local government", sc_std$org_type)
sc_std$org_type <- gsub("local government,infrastructure", "local government", sc_std$org_type)
sc_std$org_type <- gsub("local government,consulting/research,multi-stakeholder group", "local government", sc_std$org_type)
sc_std$org_type <- gsub("multi-stakeholder group,other", "other", sc_std$org_type)
sc_std$org_type <- gsub("non-profit/non-governmental organization,consulting/research", "non-profit/non-governmental organization", sc_std$org_type)
sc_std$org_type <- gsub("non-profit/non-governmental organization,infrastructure", "non-profit/non-governmental organization", sc_std$org_type)
sc_std$org_type <- gsub("state government,local government,non-profit/non-governmental organization,consulting/research", "state government", sc_std$org_type)
sc_std$org_type <- gsub("state government,regional government,local government,education/academic institution,multi-stakeholder group", "state government", sc_std$org_type)
sc_std$org_type <- gsub("state government,local government,consulting/research", "state government", sc_std$org_type)
sc_std$org_type <- gsub("state government,education/academic institution", "state government", sc_std$org_type)
sc_std$org_type <- gsub("state government,infrastructure", "state government", sc_std$org_type)
sc_std$org_type <- gsub("state government,non-profit/non-governmental organization", "state government", sc_std$org_type)
sc_std$org_type <- gsub("state government,regional government,local government,multi-stakeholder group,infrastructure", "state government", sc_std$org_type)
sc_std$org_type <- gsub("state government,regional government,local government", "state government", sc_std$org_type)

sort(sc_std$org_type)
fixtype <- sc_std %>% select(ResponseId, org, org_type)

sc_std$org_type <- str_replace(sc_std$org_type, " \\s*\\([^\\)]+\\)", "")

sc_std$org_type <- ifelse(grepl("college of charleston", sc_std$org),
                    "education/academic institution", sc_std$org_type)

sc_std$org_type <- ifelse(grepl("davis and floyd", sc_std$org),
                    "consulting/research", sc_std$org_type)

sc_std$org_type <- ifelse(grepl("explore charleston", sc_std$org),
                    "other", sc_std$org_type)

sc_std$org_type <- ifelse(grepl("charleston medical district", sc_std$org),
                    "other", sc_std$org_type)

sc_std$org_type <- ifelse(grepl("medical university of south carolina ", sc_std$org), 
                    "education/academic institution", sc_std$org_type)

sc_std$org_type <- ifelse(grepl("city of charleston", sc_std$org), 
                    "local government", sc_std$org_type)

sc_std$org_type <- ifelse(grepl("landscape pavers", sc_std$org) ,
                    "other", sc_std$org_type)

fixtype <- sc_std %>% select(org, org_type)

table(sc_std$org_type)
fixtype <- sc_std %>% select(ResponseId, org, org_type)

sort(sc_std$org_type)


## BA ----------------------------------------------------------------------
### org -----
sort(ba_std$org)
ba_std$org <- trimws(ba_std$org)

# delete everthing after a comma
ba_std$org <- gsub("(.*),.*", "\\1", ba_std$org)
# create a space where there's a dash
ba_std$org <- gsub("-", " ", ba_std$org)
# replace & with and
ba_std$org <- gsub("\\&", "and", ba_std$org)
ba_std$org <- gsub("\\+", "and", ba_std$org)
# delete everything in brackets
ba_std$org <- str_replace(ba_std$org, " \\s*\\([^\\)]+\\)", "")
# find column
which(colnames(ba_std)=='org')
ba_std$org <- str_trim(ba_std$org)
ba_std$org <- tolower(ba_std$org)
ba_std$org  <- ba_std$org %>%
  case_when(
    grepl("bcdc", .)==TRUE ~ "bcdc",
    grepl("conservation and development", .)==TRUE ~ "bcdc",
    grepl("u.s. geological survey", .)==TRUE ~ "usgs",
    grepl("u.s. bureau of reclamation", .)==TRUE ~ "us bor",
    grepl("u.s. fish and wildlife service", .)==TRUE ~ "usfws",
    grepl("united states fish and wildlife service", .)==TRUE ~ "usfws",
    grepl("us geological survey", .)==TRUE ~ "usgs",
    grepl("us epa", .)==TRUE ~ "epa",
    grepl("port of sf", .)==TRUE ~ "port of sf",
    grepl("port of san francisco", .)==TRUE ~ "port of sf",
    grepl("city and county", .)==TRUE ~ "city and county of sf",
    grepl("city & county", .)==TRUE ~ "city and county of sf",
    grepl("city of sf", .)==TRUE ~ "city and county of sf",
    grepl("sf public works", .)==TRUE ~ "city and county of sf",
    grepl("city of san francisco", .)==TRUE ~ "city and county of sf",
    grepl("san francisco planning department", .)==TRUE ~ "city and county of sf",
    grepl("sf department of the environment", .)==TRUE ~ "city and county of sf",
    grepl("public utilties commission", .)==TRUE ~ "san francisco public utilities commission",
    grepl("county of san mateo", .)==TRUE ~ "san mateo county",
    grepl("governor", .)==TRUE ~ "california governor office",
    grepl("county of marin", .)==TRUE ~ "marin county",
    grepl("county of solano", .)==TRUE ~ "solano county",
    grepl("county of contra costa", .)==TRUE ~ "contra costa county",
    grepl("marin county", .)==TRUE ~ "marin county",
    grepl("santa claravalley open space authority", .)==TRUE ~ "santa clara valley open space authority",
    grepl("santa clara county", .)==TRUE ~ "santa clara county",
    grepl("davis", .)==TRUE ~ "uc davis",
    grepl("uc berkeley", .)==TRUE ~ "uc berkeley",
    grepl("stanford", .)==TRUE ~ "stanford",
    grepl("city of berkeley", .)==TRUE ~ "city of berkeley",
    grepl("university of california, berkeley", .)==TRUE ~ "uc berkeley",
    grepl("sierra club", .)==TRUE ~ "sierra club",
    grepl("cal. department of transportation", .)==TRUE ~ "caltrans",
    grepl("caltrans", .)==TRUE ~ "caltrans",
    grepl("cmg", .)==TRUE ~ "cmg landscape architecture",
    grepl("army", .)==TRUE ~ "usace",
    grepl("caltrain", .)==TRUE ~ "caltrain",
    grepl("cdfw", .)==TRUE ~ "california department of fish and wildlife",
    grepl("sfei", .)==TRUE ~ "san francisco estuary institute",
    grepl("fema", .)==TRUE ~ "fema",
    grepl("greenaction", .)==TRUE ~ "greenaction",
    grepl("gehl", .)==TRUE ~ "gehl",
    grepl("harvey", .)==TRUE ~ "harvey and assoc",
    grepl("page", .)==TRUE ~ "page",
    grepl("dellums", .)==TRUE ~ "dellums",
    grepl("decline to state", .)==TRUE ~ "",
    grepl("i cannot list them", .)==TRUE ~ "",
    grepl("n/a", .)==TRUE ~ "",
    grepl("multiple orgs/network", .)==TRUE ~ "",
    grepl("bionic", .)==TRUE ~ "bionic",
    grepl("alameda county water district", .)==TRUE ~ "alameda county flood control and water conservation district",
    grepl("mtc", .)==TRUE ~ "metropolitan transportation commission",
    grepl("cri", .)==TRUE ~ "climate readiness institute",
    .default = .
  )

sort(ba_std$org)

### org type ----
ba_std$org_type <- str_replace(ba_std$org_type, " \\s*\\([^\\)]+\\)", "")

ba_std$org_type <- ifelse(grepl("aecom", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("arcadis", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("\\bpage\\b", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("transit", ba_std$org),
                    "Local government", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("architect", ba_std$org), 
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("wrt", ba_std$org), 
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("conservation and development commission", ba_std$org) ,
                    "State government", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("press", ba_std$org) ,
                    "Media", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("ascent", ba_std$org) ,
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("bionic", ba_std$org) ,
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("fleet", ba_std$org) ,
                    "Education/Consulting/Research", ba_std$org_type)


ba_std$org_type <- ifelse(grepl("joint powers", ba_std$org),
                    "Regional government", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("sfmta", ba_std$org),
                    "Regional government", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("san francisco county transportation", ba_std$org),
                    "Regional government", ba_std$org_type)


ba_std$org_type <- ifelse(grepl("citizens committee", ba_std$org) ,
                    "Environmental Group", ba_std$org_type)


ba_std$org_type <- ifelse(grepl("city of", ba_std$org),
                    "Local government", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("town of", ba_std$org),
                    "Local government", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("county", ba_std$org),
                    "Local government", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("hayward", ba_std$org),
                    "Local government", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("bcdc", ba_std$org),
                    "State government", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("zone 7 water agency", ba_std$org),
                    "Water Infrastructure Special District", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("resource conservation", ba_std$org),
                    "Environmental Special District", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("park district", ba_std$org),
                    "Environmental Special District", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("midpeninsula", ba_std$org),
                    "Environmental Special District", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("reclamation district", ba_std$org),
                    "Environmental Special District", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("district", ba_std$org)==T & grepl("flood", ba_std$org)==T,
                    "Water Infrastructure Special District", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("sanitary district", ba_std$org)==T,
                    "Water Infrastructure Special District", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("friant", ba_std$org)==T,
                    "Water Infrastructure Special District", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("district", ba_std$org)==T & grepl("water", ba_std$org)==T,
                    "Water Infrastructure Special District", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("environmental forum", ba_std$org),
                    "Environmental Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("save the bay", ba_std$org),
                    "Environmental Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("associates", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("swa", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("tetra", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("pax", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)


ba_std$org_type <- ifelse(grepl("andeavor", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("systematics", ba_std$org),
                    "Trade/Business/Industry Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("capmarkets", ba_std$org),
                    "Trade/Business/Industry Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("financ", ba_std$org),
                    "Trade/Business/Industry Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("exploratorium", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("research", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("university", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("consult", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("engineering", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("operations", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("hassell", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("design", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("mithun", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("mig", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("studio", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("uc berkeley", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("ucsf", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("einstellung", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("conservation science", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("friends of", ba_std$org),
                    "Environmental Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("wildlands", ba_std$org),
                    "Environmental Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("greenbelt", ba_std$org),
                    "Environmental Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("baykeeper", ba_std$org),
                    "Environmental Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("conservation league", ba_std$org),
                    "Environmental Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("audubon", ba_std$org),
                    "Environmental Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("shore", ba_std$org),
                    "Environmental Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("sierra", ba_std$org),
                    "Environmental Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("ecology center", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("nature conservancy", ba_std$org),
                    "Environmental Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("greenaction", ba_std$org),
                    "Community-based organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("neighborhood improvement", ba_std$org),
                    "Community-based organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("bay joint venture", ba_std$org),
                    "Environmental Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("port of", ba_std$org),
                    "Local government", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("airport", ba_std$org),
                    "Local government", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("utilities", ba_std$org),
                    "Local government", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("transportation authority", ba_std$org),
                    "Local government", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("metropolitan transportation", ba_std$org),
                    "Regional government", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("sonoma clean power", ba_std$org),
                    "Trade/Business/Industry Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("wells fargo", ba_std$org),
                    "Trade/Business/Industry Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("gensler", ba_std$org),
                    "Trade/Business/Industry Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("company", ba_std$org),
                    "Trade/Business/Industry Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("katrisk", ba_std$org),
                    "Trade/Business/Industry Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("soltrans", ba_std$org),
                    "Trade/Business/Industry Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("jabba", ba_std$org),
                    "Trade/Business/Industry Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("holdings", ba_std$org),
                    "Trade/Business/Industry Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("caltrain", ba_std$org),
                    "Trade/Business/Industry Group", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("cambridge", ba_std$org),
                    "Trade/Business/Industry Group", ba_std$org_type)


fixtype <- ba_std %>% select(org, org_type)

# make Env NGOs into Env Groups so that NGO refers to NGOs that have no env focus

ba_std$org_type <- ifelse(grepl("institute", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("multiplier", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("environmental action", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("women", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("footprint", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("disability", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("gehl", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("yimby", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("sscra", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("venture", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("local government commission", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("college", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("academy", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("kiewit", ba_std$org),
                    "Education/Consulting/Research", ba_std$org_type)


ba_std$org_type <- ifelse(grepl("netherlands", ba_std$org),
                    "Other", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("mvr", ba_std$org),
                    "Other", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("\\bbridge\\b", ba_std$org),
                    "Other", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("som", ba_std$org),
                    "Other", ba_std$org_type)


ba_std$org_type <- ifelse(grepl("chinatown", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("laborers", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("youth", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("legacy fund", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("bay planning", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("marin community", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("telegraph", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("climate one", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

ba_std$org_type <- ifelse(grepl("marin economic", ba_std$org),
                    "No-profit organization/Non-governmental organization", ba_std$org_type)

# include 'involved on my own' in the respondents!
ba_std$org_type <- ifelse(grepl('\\bown\\b', ba_std$Q2)==TRUE, 'Own involvement',
                    ifelse(is.na(ba_std$Q2) & ba_std$org_type == "", 'Prefer not say',
                           ifelse(ba_std$org=="" & ba_std$org_type == "", 'Prefer not say',
                                  ba_std$org_type)))

table(ba_std$org_type)
fixtype <- ba_std %>% select(ResponseId, org, org_type)


# Export ------------------------------------------------------------------
write.csv(delta_std, "0_Data/raw/standardized_datasets/delta.csv")
write.csv(sc_std, "0_Data/raw/standardized_datasets/sc.csv")
write.csv(va_std, "0_Data/raw/standardized_datasets/va.csv")
write.csv(ba_std, "0_Data/raw/standardized_datasets/ba.csv")






