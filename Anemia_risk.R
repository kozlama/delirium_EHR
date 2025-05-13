####################################################################################
# Longitudinal analysis from risk factor to delirium diagnosis 
# Example script for anemia diagnosis as risk factor 
# Apply for other diagnoses (bipolar disorder, melanocytic nevus) 
# Generates main figure 5 in manuscript
####################################################################################

# Connect to database
# Establish diagnostic cohort ======
# Diagnosis of interest: anemia
diagnosis_codes <- c("439777") # anemia, 432876 for bipolar disorder, 4271013 for melanocytic nevus

# Extract patients with diagnosis of interest (first visit)
sqlstring <- sprintf("SELECT DISTINCT diagnosis.[person_id], diagnosis.[condition_concept_id], diagnosis.[condition_start_date], diagnosis.[condition_end_date],diagnosis.[visit_occurrence_id],
                     concept.[concept_name],
                     visit.[visit_start_date], visit.[visit_end_date], visit.[visit_concept_id]
                     FROM [omop].[condition_occurrence] AS diagnosis
                     INNER JOIN omop.concept AS concept
                     ON diagnosis.[condition_concept_id] = concept.[concept_id]
                     
                     INNER JOIN omop.[visit_occurrence] AS visit
                     ON diagnosis.[visit_occurrence_id] = visit.[visit_occurrence_id] AND
                     diagnosis.[person_id] = visit.[person_id]
                     
                     WHERE diagnosis.[condition_concept_id] LIKE '%s'", 
                     paste(as.numeric(diagnosis_codes), collapse = "' OR diagnosis.[condition_concept_id] LIKE '"))

res <- dbGetQuery(con, sqlstring)

# Filter for first visit with anemia diagnosis
res_filtered <- res %>%
  arrange(visit_start_date) %>%
  group_by(person_id) %>%
  top_n(n=-1, wt = visit_start_date) %>%
  distinct(person_id, visit_start_date, .keep_all = T) 

length(unique(res_filtered$person_id)) #14469 unique patients

# merge with demographic information
dbExecute(con, "IF OBJECT_ID('tempdb.dbo.#tmppt', 'U') IS NOT NULL DROP TABLE #tmppt")
dbWriteTable(con, '#tmppt', res_filtered)
res_demo <- dbGetQuery(con, "SELECT DISTINCT person.[person_id], person.[gender_source_value], person.[race_source_value], person.[birth_datetime],
                  death.[death_date],
                  obs.[observation_period_start_date], obs.[observation_period_end_date]
                  FROM omop.person AS person
                  RIGHT JOIN #tmppt ON person.[person_id] = #tmppt.[person_id] 
                  LEFT JOIN omop.[visit_Occurrence] AS visit ON visit.[visit_occurrence_id] = #tmppt.[visit_occurrence_id]
                  LEFT JOIN omop.death AS death ON death.[person_id] = #tmppt.[person_id]
                  LEFT JOIN omop.[observation_period] AS obs ON obs.[person_id] = #tmppt.[person_id]")

res_merged <- merge(res_filtered,res_demo, by="person_id")

# calculate age at admission
res_merged$age_admit <- lubridate::time_length(difftime(res_merged$visit_start_date, res_merged$birth_datetime), "years")

# calculate duration in healthcare system
res_merged$service <- lubridate::time_length(difftime(res_merged$visit_start_date, res_merged$observation_period_start_date), "years")

# calculate length of data available after visit
res_merged$timespan <- lubridate::time_length(difftime(res_merged$observation_period_end_date, res_merged$visit_start_date), "years")
summary(res_merged$timespan)

# rename columns and clean up data 
# like remove patients with no birth dates and remove nonbinary / unknown sex
res_clean <- res_merged %>%
  rename(gender = gender_source_value,
         race = race_source_value) %>%
  filter(!is.na(birth_datetime),
         gender == "Female" | gender == "Male",
         service <= age_admit) 

res_clean$race[which(res_clean$race == "Declined")] <- "Unknown/Declined"
res_clean$race[which(res_clean$race == "Unknown")] <- "Unknown/Declined"
res_clean$race[which(res_clean$race == "")] <- "Unknown/Declined"
res_clean$race[which(res_clean$race == "Other Pacific Islander")] <- "Native Hawaiian or Other Pacific Islander"
res_clean$race[which(res_clean$race == "Native Hawaiian")] <- "Native Hawaiian or Other Pacific Islander"

table(res_clean$race)
table(res_clean$gender)

a <- subset(res_clean, !is.na(death_date))
b <- subset(a, visit_end_date > death_date)

res_clean <- subset(res_clean, !person_id %in% b$person_id)

# Finding control cohort with no diagnosis of interest =======
# time frame of visits defined in diagnosis cohort list:
res_clean <- res_clean %>%
  arrange(visit_start_date)
res_clean$visit_start_date[1] #1991-10-03
res_clean$visit_start_date[nrow(res_clean)] #2022-11-23
res_clean <- subset(res_clean, visit_start_date <= "2023-04-05")
res_clean$visit_start_date[nrow(res_clean)]

sqlstring <- sprintf("SELECT DISTINCT visit.[person_id], visit.[visit_start_date], visit.[visit_end_date], visit.[visit_occurrence_id], visit.[visit_concept_id]
                     FROM omop.[visit_occurrence] AS visit
                     WHERE visit.[visit_start_date] < '2023-01-08' AND
                     visit.[visit_start_date] > '1975-01-09'")

ctrl_res <- dbGetQuery(con, sqlstring)

# remove patients with diagnosis of interest
ctrl_res_filtered <- ctrl_res[which(!(ctrl_res$person_id %in% res_clean$person_id)),]
length(unique(ctrl_res_filtered$person_id)) 

# Filter for random? visit
ctrl_visit <- ctrl_res_filtered %>%
  arrange(visit_start_date) %>%
  group_by(person_id) %>%
  slice_sample(n=1) %>%
  distinct(person_id, visit_start_date, .keep_all = T) 

length(unique(ctrl_visit$person_id)) == nrow(ctrl_visit)

ctrl.visit.num <- data.frame(table(ctrl_res_filtered$person_id)) # calculate number of visits total per patient
ctrl_visit$visit_num <- ctrl.visit.num$Freq[match(as.factor(ctrl_visit$person_id), ctrl.visit.num$Var1)]
ctrl_visit$visit_num <- ctrl_visit$visit_num - 1
summary(ctrl_visit$visit_num)

ctrl_visit <- subset(ctrl_visit, visit_num < 1000)

# calculate how many visits diagnosis cohort patients have had prior to their diagnosis visit:
ad_visits <- ctrl_res[which(ctrl_res$person_id %in% res_clean$person_id),]
ad_visits$visit_threshold <- res_clean$visit_start_date[match(ad_visits$person_id, res_clean$person_id)]
ad_visits_filt <- subset(ad_visits, visit_end_date < visit_threshold | visit_start_date == visit_threshold)

ad.visit.num <- data.frame(table(ad_visits_filt$person_id))
res_clean$visit_num <- as.numeric(ad.visit.num$Freq)[match(as.factor(res_clean$person_id), ad.visit.num$Var1)]

summary(res_clean$visit_num)

res_clean <- subset(res_clean, visit_num < 1000)
res_clean <- subset(res_clean, !is.na(visit_num))

# merge with demographic information =======
dbExecute(con, "IF OBJECT_ID('tempdb.dbo.#tmppt', 'U') IS NOT NULL DROP TABLE #tmppt")
dbWriteTable(con, '#tmppt', ctrl_visit)
ctrl_demo <- dbGetQuery(con, "SELECT DISTINCT person.[person_id], person.[gender_source_value], person.[race_source_value], person.[birth_datetime],
                     death.[death_date], 
                     obs.[observation_period_start_date], obs.[observation_period_end_date]
                     FROM omop.person AS person
                     RIGHT JOIN #tmppt ON person.[person_id] = #tmppt.[person_id]
                     LEFT JOIN omop.[visit_Occurrence] AS visit ON visit.[visit_occurrence_id] = #tmppt.[visit_occurrence_id]
                     LEFT JOIN omop.death AS death ON person.[person_id] = death.[person_id]
                     LEFT JOIN omop.[observation_period] AS obs ON obs.[person_id] = #tmppt.[person_id]")

ctrl_merged <- ctrl_demo %>%
  filter(!is.na(birth_datetime)) %>%
  rename(gender = gender_source_value,
         race = race_source_value) %>%
  merge(ctrl_visit, by = "person_id")  %>%
  filter(gender == "Female" | gender == "Male")

# calculate age at admission
ctrl_merged$age_admit <- lubridate::time_length(difftime(ctrl_merged$visit_start_date, ctrl_merged$birth_datetime), "years")

# calculate duration in healthcare system
ctrl_merged$service <- lubridate::time_length(difftime(ctrl_merged$visit_start_date, ctrl_merged$observation_period_start_date), "years")

# calculate duration of data post visit
ctrl_merged$timespan <- lubridate::time_length(difftime(ctrl_merged$observation_period_end_date, ctrl_merged$visit_start_date), "years")

# clean up race information
ctrl_merged$race[which(ctrl_merged$race == "Declined")] <- "Unknown/Declined"
ctrl_merged$race[which(ctrl_merged$race == "Unknown")] <- "Unknown/Declined"
ctrl_merged$race[which(ctrl_merged$race == "")] <- "Unknown/Declined"
ctrl_merged$race[which(ctrl_merged$race == "Other Pacific Islander")] <- "Native Hawaiian or Other Pacific Islander"
ctrl_merged$race[which(ctrl_merged$race == "Native Hawaiian")] <- "Native Hawaiian or Other Pacific Islander"

table(ctrl_merged$race)
table(ctrl_merged$gender)

a <- subset(ctrl_merged, !is.na(death_date))
b <- subset(a, visit_end_date > death_date)

ctrl_merged <- subset(ctrl_merged, !person_id %in% b$person_id)
ctrl_merged <- ctrl_merged %>%
  filter(service <= age_admit)

# merge control and diagnosis cohorts ======
ad_group <- res_clean %>%
  select(person_id, gender, race, birth_datetime, death_date, visit_start_date,
         visit_end_date, visit_occurrence_id, age_admit, service, visit_num, timespan) %>%
  mutate(visit_num = visit_num - 1,
         group = 1,
         birth_datetime = as.Date(birth_datetime),
         death_date = as.Date(death_date),
         visit_start_date = as.Date(visit_start_date),
         visit_end_date = as.Date(visit_end_date))

ctrl_group <- ctrl_merged %>%
  select(person_id, gender, race, birth_datetime, death_date, visit_start_date,
         visit_end_date, visit_occurrence_id, age_admit, service, visit_num, timespan) %>%
  mutate(group = 0,
         visit_occurrence_id = as.integer(visit_occurrence_id))

full_cohort <- rbind(ad_group, ctrl_group)
full_cohort$time_to_death <- lubridate::time_length(difftime(full_cohort$death_date, full_cohort$visit_start_date), "years")
full_cohort$timespan[!is.na(full_cohort$time_to_death)] <- full_cohort$time_to_death[!is.na(full_cohort$time_to_death)]

# number of comorbidities and filter out those who have had delirium before anemia diagnosis ======
dbExecute(con, "IF OBJECT_ID('tempdb.dbo.#tmpfull', 'U') IS NOT NULL DROP TABLE #tmpmatched")
dbWriteTable(con, '#tmpfull', full_cohort)
num.diag <- dbGetQuery(con, "SELECT DISTINCT diagnosis.[person_id], 
                         diagnosis.[condition_concept_id], diagnosis.[condition_start_date], 
                         #tmpfull.[visit_start_date]
                         FROM omop.[condition_occurrence] AS diagnosis
                         RIGHT JOIN #tmpfull ON diagnosis.[person_id] = #tmpfull.[person_id]
                         WHERE diagnosis.[condition_start_date] <= #tmpfull.[visit_start_date]")

# Filter for first time diagnoses
diag.unique <- num.diag %>% 
  distinct(person_id, condition_concept_id, .keep_all = TRUE)
length(unique(diag.unique$person_id)) # 371315 unique patients

# filter out people with a delirium diagnosis 373995
delirium_pts <- unique(subset(diag.unique, condition_concept_id == 373995)$person_id)

filtered_cohort <- full_cohort %>%
  filter(!person_id %in% delirium_pts)

# calculate total diagnoses 
total.num.diag <- data.frame(table(diag.unique$person_id))
filtered_cohort$total_comorb <- total.num.diag$Freq[match(filtered_cohort$person_id, total.num.diag$Var1)]
filtered_cohort <- subset(filtered_cohort, timespan >= 0)
summary(filtered_cohort$timespan)

# use MatchItR to identify appropriate cohort ====
library(MatchIt)
filtered_cohort$visit_num[which(is.na(filtered_cohort$visit_num))]<-0 # NA value for visit_num
filtered_cohort$total_comorb[which(is.na(filtered_cohort$total_comorb))]<-0
filtered_cohort$race[which(filtered_cohort$race == "White or Caucasian")] <- "White"
filtered_cohort$race[which(filtered_cohort$race == "*Unknown")] <- "Unknown/Declined"
filtered_cohort$race[which(filtered_cohort$race == "*Unspecified")] <- "Unknown/Declined"

table(filtered_cohort$race)

filtered_cohort

# 1:1 NN PS matching w/o replacement
m.out1 <- matchit(group ~ age_admit + race + gender + service + visit_num + total_comorb + timespan,
                  data = filtered_cohort, #could subset to patients with 10+ years of data
                  method = "nearest", 
                  distance = "glm",
                  ratio = 1)
# summary(m.out1, un = F)
# plot(m.out1, type = "jitter", interactive = FALSE)
# plot(m.out1, type = "qq", interactive = FALSE,
#      which.xs = c("age_admit", "total_comorb"))
plot(summary(m.out1))

matched_data <- match.data(m.out1)

###### Time to delirium diagnosis #######
# Connect to database
# Input cohort data with demographic info - matched_cohort

# Get information about delirium diagnosis

# Calculate Kaplan Meier curves and cox regression models for anemia diagnosis for all patients =======
matched_cohort$duration_to_delirium <- lubridate::time_length(difftime(matched_cohort$delirium_visit, matched_cohort$visit_start_date), "years")
matched_cohort$status <- matched_cohort$duration_to_delirium
matched_cohort$status[is.na(matched_cohort$status)] <- 0 # patients with no delirium visits
matched_cohort$status[!is.na(matched_cohort$duration_to_delirium)] <- 1 # patients with a delirium date considered an event

matched_cohort$time <- matched_cohort$duration_to_delirium
matched_cohort$time[is.na(matched_cohort$duration_to_delirium)] <- matched_cohort$timespan[is.na(matched_cohort$duration_to_delirium)]

matched_cohort = subset(matched_cohort, time >= 0)
matched_cohort$time_adjusted <- matched_cohort$time

summary(subset(matched_cohort, status == 1)$time) 
matched_cohort$time_adjusted[matched_cohort$time_adjusted > max(subset(matched_cohort, status == 1)$time)] = max(subset(matched_cohort, status == 1)$time)
matched_cohort$status_adjusted <- matched_cohort$status
matched_cohort$status_adjusted[matched_cohort$time > max(subset(matched_cohort, status == 1)$time)] = 0

summary(matched_cohort$time_adjusted)

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(ggsurvfit)

survfit2(Surv(time_adjusted, status_adjusted) ~ group, data = subset(matched_cohort, time_adjusted > 0)) %>% 
  ggsurvfit()  +
  labs(
    x = "Years",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval() +
  scale_y_continuous(
    limits = c(0.95, 1),
    labels = scales::percent, 
    expand = c(0.01, 0)
  ) +
  add_risktable() +
  theme_classic() +
  ylab("Percentage Survival") +
  xlab("Follow-up Time (years)") +
  scale_color_manual(values = c("grey","orange")) +
  scale_fill_manual(values = c("grey","orange"))

# COX regression model for group
cox1 <- coxph(Surv(time_adjusted, status_adjusted) ~ group, data = subset(matched_cohort, time_adjusted > 0))
cox4 <- coxph(Surv(time_adjusted, status_adjusted) ~ group + gender + age_admit + race + service + visit_num + total_comorb + timespan, data = subset(matched_cohort, time_adjusted > 0))

res1 <- summary(cox1)
res4 <- summary(cox4)

full_res <- data.frame(matrix(ncol = 0, nrow = 2)) %>%
  mutate(p_value = c(res1$waldtest["pvalue"], res4$waldtest["pvalue"]),
         HR = c(res1$coefficients[,"exp(coef)"], res4$coefficients[1,"exp(coef)"]),
         HR.confint.lower = c(res1$conf.int[,"lower .95"], res4$conf.int[1,"lower .95"]),
         HR.confint.upper = c(res1$conf.int[,"upper .95"], res4$conf.int[1,"upper .95"]))

ggplot(full_res, aes(x = row.names(full_res), y = HR)) +
  geom_errorbar(aes(ymin=HR.confint.lower, ymax=HR.confint.upper), width=.1) +
  geom_point(size = 5, shape = 18, color = "orange") +
  theme_classic() +
  labs(y = "Hazard Ratio", x= "") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  coord_flip() 
