# Emergency Department Patient Profile Analysis
# Loading required libraries
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)
library(lubridate)
library(tidyr)
library(scales)
library(reshape2)

# Load the data
load("data-aktin.RData")

# Check data structure
cat("Dataset dimensions:", dim(data.aktin), "\n")
cat("Column names:\n")
print(colnames(data.aktin))

# Basic data exploration
cat("\nFirst few rows:\n")
print(head(data.aktin))

# Check data types
cat("\nData types:\n")
print(sapply(data.aktin, class))

# Function to create age groups
create_age_groups <- function(age) {
  ifelse(age <= 20, "0-20",
         ifelse(age <= 40, "21-40",
                ifelse(age <= 60, "41-60", ">60")))
}

# Function to get department columns (ending with .date)
get_department_columns <- function(data) {
  date_cols <- grep("\\.date$", colnames(data), value = TRUE)
  return(date_cols)
}

# Function to calculate percentages
calculate_percentage <- function(count, total) {
  round((count / total) * 100, 2)
}

# Data preprocessing
data_clean <- data.aktin %>%
  mutate(
    age = as.numeric(floor((as.Date(visit.date) - as.Date(birth.date))/365.25)),
    age_group = create_age_groups(age),
    triage_label = case_when(
      triage == 1 ~ "ROT",
      triage == 2 ~ "ORANGE", 
      triage == 3 ~ "GELB",
      triage == 4 ~ "GRÜN",
      triage == 5 ~ "BLAU",
      TRUE ~ "Unknown"
    ),
    visit_month = format(as.Date(visit.date), "%Y-%m")
  )

# Get department columns
department_cols <- get_department_columns(data_clean)
cat("\nDepartment columns found:", length(department_cols), "\n")
print(department_cols)

# 1. GENERAL PATIENT PROFILE - NUMERICAL DISTRIBUTION

# Male Patients Analysis
male_patients <- data_clean %>% filter(sex == "M")
female_patients <- data_clean %>% filter(sex == "W")

cat("\n=== MALE PATIENTS ANALYSIS ===\n")
cat("Total male patients:", nrow(male_patients), 
    "(", calculate_percentage(nrow(male_patients), nrow(data_clean)), "%)\n")

# Male age distribution
male_age_dist <- male_patients %>%
  group_by(age_group) %>%
  summarise(count = n()) %>%
  mutate(percentage = calculate_percentage(count, nrow(male_patients)))

cat("\nMale age distribution:\n")
print(male_age_dist)

# Male triage distribution
male_triage_dist <- male_patients %>%
  group_by(triage, triage_label) %>%
  summarise(count = n()) %>%
  mutate(percentage = calculate_percentage(count, nrow(male_patients)))

cat("\nMale triage distribution:\n")
print(male_triage_dist)

# Define image departments (from sonography.date to xray.misc.date)
image_depts <- c(
  "sonography.date", "echo.cardiography.date", "ecg.date", "laboratory.date", "test.urine.date", "cct.date", "ct.date", "mri.date", "trauma.scan.date", "xray.extremity.date", "xray.basin.date", "xray.spine.date", "xray.thorax.date", "xray.misc.date"
)

# Add image department visit counts to Male_Age_Triage_Cross
male_age_triage_img <- male_patients %>%
  group_by(age_group, triage, triage_label) %>%
  summarise(
    count = n(),
    percentage = calculate_percentage(n(), sum(n())),
    across(all_of(image_depts), ~sum(!is.na(.)), .names = "{.col}_visits")
  )

# Female Patients Analysis
cat("\n=== FEMALE PATIENTS ANALYSIS ===\n")
cat("Total female patients:", nrow(female_patients), 
    "(", calculate_percentage(nrow(female_patients), nrow(data_clean)), "%)\n")

# Female age distribution
female_age_dist <- female_patients %>%
  group_by(age_group) %>%
  summarise(count = n()) %>%
  mutate(percentage = calculate_percentage(count, nrow(female_patients)))

cat("\nFemale age distribution:\n")
print(female_age_dist)

# Female triage distribution
female_triage_dist <- female_patients %>%
  group_by(triage, triage_label) %>%
  summarise(count = n()) %>%
  mutate(percentage = calculate_percentage(count, nrow(female_patients)))

cat("\nFemale triage distribution:\n")
print(female_triage_dist)

# Add image department visit counts to Female_Age_Triage_Cross
female_age_triage_img <- female_patients %>%
  group_by(age_group, triage, triage_label) %>%
  summarise(
    count = n(),
    percentage = calculate_percentage(n(), sum(n())),
    across(all_of(image_depts), ~sum(!is.na(.)), .names = "{.col}_visits")
  )

# 2. DEPARTMENT ANALYSIS BY TRIAGE AND GENDER

# Function to analyze department visits by triage and gender
analyze_departments_by_triage_gender <- function(data, gender) {
  dept_analysis <- list()
  
  for (triage_code in 1:5) {
    triage_data <- data %>% 
      filter(sex == gender, triage == triage_code)
    
    if (nrow(triage_data) > 0) {
      dept_counts <- data.frame()
      
      for (dept in department_cols) {
        # Count non-NA values (patients who visited this department)
        visit_count <- sum(!is.na(triage_data[[dept]]))
        if (visit_count > 0) {
          dept_counts <- rbind(dept_counts, data.frame(
            department = dept,
            count = visit_count,
            percentage = calculate_percentage(visit_count, nrow(triage_data))
          ))
        }
      }
      
      if (nrow(dept_counts) > 0) {
        dept_counts <- dept_counts %>% 
          arrange(desc(count)) %>%
          head(10) # Top 10 departments
        
        dept_analysis[[paste0("Triage_", triage_code)]] <- dept_counts
      }
    }
  }
  
  return(dept_analysis)
}

# Analyze departments for male patients by triage
male_dept_analysis <- analyze_departments_by_triage_gender(data_clean, "M")
cat("\n=== MALE PATIENTS - DEPARTMENT ANALYSIS BY TRIAGE ===\n")
for (triage in names(male_dept_analysis)) {
  cat("\n", triage, ":\n")
  print(male_dept_analysis[[triage]])
}

# Analyze departments for female patients by triage
female_dept_analysis <- analyze_departments_by_triage_gender(data_clean, "W")
cat("\n=== FEMALE PATIENTS - DEPARTMENT ANALYSIS BY TRIAGE ===\n")
for (triage in names(female_dept_analysis)) {
  cat("\n", triage, ":\n")
  print(female_dept_analysis[[triage]])
}

# Debug: Check female patients data
cat("\n=== DEBUG: FEMALE PATIENTS DATA ===\n")
cat("Total female patients:", nrow(female_patients), "\n")
cat("Female patients with triage codes:\n")
print(table(female_patients$triage, useNA = "ifany"))
cat("Female patients with department visits:\n")
for (dept in department_cols[1:5]) { # Check first 5 departments
  visit_count <- sum(!is.na(female_patients[[dept]]))
  cat(dept, ":", visit_count, "visits\n")
}

# Create full cross-table for department by triage (male)
male_dept_by_triage_full <- expand.grid(
  department = department_cols,
  triage = as.character(1:5)
) %>%
  left_join(
    data_clean %>% filter(sex == "M") %>%
      pivot_longer(all_of(department_cols), names_to = "department", values_to = "visit_date") %>%
      filter(!is.na(visit_date)) %>%
      mutate(triage = as.character(triage)) %>%
      group_by(department, triage) %>%
      summarise(count = n(), .groups = 'drop'),
    by = c("department", "triage")
  ) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  group_by(triage) %>%
  mutate(percentage = calculate_percentage(count, sum(count))) %>%
  ungroup() %>%
  pivot_wider(names_from = triage, values_from = c(count, percentage), names_glue = "Triage_{triage}_{.value}")

# Create full cross-table for department by triage (female)
female_dept_by_triage_full <- expand.grid(
  department = department_cols,
  triage = as.character(1:5)
) %>%
  left_join(
    data_clean %>% filter(sex == "W") %>%
      pivot_longer(all_of(department_cols), names_to = "department", values_to = "visit_date") %>%
      filter(!is.na(visit_date)) %>%
      mutate(triage = as.character(triage)) %>%
      group_by(department, triage) %>%
      summarise(count = n(), .groups = 'drop'),
    by = c("department", "triage")
  ) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  group_by(triage) %>%
  mutate(percentage = calculate_percentage(count, sum(count))) %>%
  ungroup() %>%
  pivot_wider(names_from = triage, values_from = c(count, percentage), names_glue = "Triage_{triage}_{.value}")

# 3. VISIT DATES ANALYSIS

# Monthly patient counts
monthly_patients <- data_clean %>%
  group_by(visit_month) %>%
  summarise(
    total_patients = n(),
    .groups = 'drop'
  ) %>%
  arrange(visit_month)

cat("\n=== MONTHLY PATIENT COUNTS ===\n")
print(monthly_patients)

# Monthly triage distribution
monthly_triage <- data_clean %>%
  group_by(visit_month, triage, triage_label) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(visit_month) %>%
  mutate(percentage = calculate_percentage(count, sum(count))) %>%
  arrange(visit_month, triage)

cat("\n=== MONTHLY TRIAGE DISTRIBUTION ===\n")
print(monthly_triage)

# 3.1. Monthly patient counts (with month names) - AGGREGATED BY MONTH NAME ONLY
data_clean$visit_month_name <- format(as.Date(data_clean$visit.date), "%B %Y")
data_clean$visit_month_only <- month.name[month(as.Date(data_clean$visit.date))]

# Monthly patients aggregated by month name only (12 months total)
monthly_patients_by_month <- data_clean %>%
  group_by(visit_month_only) %>%
  summarise(total_patients = n(), .groups = 'drop') %>%
  mutate(visit_month_only = factor(visit_month_only, levels = month.name)) %>%
  arrange(visit_month_only)

# 3.2. Monthly triage code distribution (count and %) - AGGREGATED BY MONTH NAME ONLY
monthly_triage_by_month <- data_clean %>%
  group_by(visit_month_only, triage, triage_label) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(visit_month_only) %>%
  mutate(percentage = calculate_percentage(count, sum(count))) %>%
  arrange(visit_month_only, triage)

# 3.3. Monthly department intensity by triage code - AGGREGATED BY MONTH NAME ONLY
monthly_dept_by_triage_by_month <- data_clean %>%
  pivot_longer(all_of(department_cols), names_to = "department", values_to = "visit_date") %>%
  filter(!is.na(visit_date)) %>%
  group_by(visit_month_only, triage, triage_label, department) %>%
  summarise(visits = n(), .groups = 'drop')

# For each month and triage, find the department(s) with max visits - AGGREGATED BY MONTH NAME ONLY
top_dept_by_month_triage_by_month <- monthly_dept_by_triage_by_month %>%
  group_by(visit_month_only, triage, triage_label) %>%
  filter(visits == max(visits)) %>%
  ungroup()

# Add image department visit counts to Monthly_Triage_By_Month
monthly_triage_by_month_img <- data_clean %>%
  group_by(visit_month_only, triage, triage_label) %>%
  summarise(
    count = n(),
    percentage = calculate_percentage(n(), sum(n())),
    across(all_of(image_depts), ~sum(!is.na(.)), .names = "{.col}_visits")
  )

# Original monthly analysis (keeping for backward compatibility)
monthly_patients_named <- data_clean %>%
  group_by(visit_month_name) %>%
  summarise(total_patients = n(), .groups = 'drop') %>%
  arrange(visit_month_name)

# 3.2. Monthly triage code distribution (count and %)
monthly_triage_named <- data_clean %>%
  group_by(visit_month_name, triage, triage_label) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(visit_month_name) %>%
  mutate(percentage = calculate_percentage(count, sum(count))) %>%
  arrange(visit_month_name, triage)

# 3.3. Monthly department intensity by triage code
monthly_dept_by_triage <- data_clean %>%
  pivot_longer(all_of(department_cols), names_to = "department", values_to = "visit_date") %>%
  filter(!is.na(visit_date)) %>%
  group_by(visit_month_name, triage, triage_label, department) %>%
  summarise(visits = n(), .groups = 'drop')

# For each month and triage, find the department(s) with max visits
top_dept_by_month_triage <- monthly_dept_by_triage %>%
  group_by(visit_month_name, triage, triage_label) %>%
  filter(visits == max(visits)) %>%
  ungroup()

# Add image department visit counts to Monthly_Triage_Named
monthly_triage_named_img <- data_clean %>%
  group_by(visit_month_name, triage, triage_label) %>%
  summarise(
    count = n(),
    percentage = calculate_percentage(n(), sum(n())),
    across(all_of(image_depts), ~sum(!is.na(.)), .names = "{.col}_visits")
  )

# 4. HYGIENE.REASON ANALYSIS

# Check if hygiene.reason exists
if ("hygiene.reason" %in% colnames(data_clean)) {
  cat("\n=== HYGIENE.REASON ANALYSIS ===\n")
  
  # Hygiene reason by triage distribution
  hygiene_triage <- data_clean %>%
    filter(!is.na(hygiene.reason)) %>%
    group_by(hygiene.reason, triage, triage_label) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(hygiene.reason) %>%
    mutate(percentage = calculate_percentage(count, sum(count)))
  
  cat("\nHygiene reason by triage distribution:\n")
  print(hygiene_triage)
  
  # Department analysis by hygiene reason
  hygiene_dept_analysis <- list()
  
  for (reason in unique(data_clean$hygiene.reason[!is.na(data_clean$hygiene.reason)])) {
    reason_data <- data_clean %>% filter(hygiene.reason == reason)
    
    dept_counts <- data.frame()
    for (dept in department_cols) {
      visit_count <- sum(!is.na(reason_data[[dept]]))
      if (visit_count > 0) {
        dept_counts <- rbind(dept_counts, data.frame(
          department = dept,
          count = visit_count,
          percentage = calculate_percentage(visit_count, nrow(reason_data))
        ))
      }
    }
    
    if (nrow(dept_counts) > 0) {
      dept_counts <- dept_counts %>% 
        arrange(desc(count)) %>%
        head(10)
      
      hygiene_dept_analysis[[reason]] <- dept_counts
    }
  }
  
  cat("\nDepartment analysis by hygiene reason:\n")
  for (reason in names(hygiene_dept_analysis)) {
    cat("\nHygiene reason:", reason, "\n")
    print(hygiene_dept_analysis[[reason]])
  }
} else {
  cat("\nhygiene.reason column not found in the dataset.\n")
}

# Save results to Excel
cat("\nSaving results to Excel...\n")

results_list <- list(
  "Male_Age_Distribution" = male_age_dist,
  "Male_Triage_Distribution" = male_triage_dist,
  "Male_Age_Triage_Cross" = male_age_triage_img,
  "Male_Dept_By_Triage" = male_dept_by_triage_full,
  "Female_Age_Distribution" = female_age_dist,
  "Female_Triage_Distribution" = female_triage_dist,
  "Female_Age_Triage_Cross" = female_age_triage_img,
  "Female_Dept_By_Triage" = female_dept_by_triage_full,
  "Monthly_Patients" = monthly_patients,
  "Monthly_Patients_Named" = monthly_patients_named,
  "Monthly_Patients_By_Month" = monthly_patients_by_month,
  "Monthly_Triage" = monthly_triage,
  "Monthly_Triage_Named" = monthly_triage_named,
  "Monthly_Triage_By_Month" = monthly_triage_by_month,
  "Monthly_Triage_Named_Imaging" = monthly_triage_named_img,
  "Monthly_Triage_By_Month_Imaging" = monthly_triage_by_month_img,
  "Top_Dept_By_Month_Triage" = top_dept_by_month_triage,
  "Top_Dept_By_Month_Triage_By_Month" = top_dept_by_month_triage_by_month
)
if ("hygiene.reason" %in% colnames(data_clean)) {
  results_list[["Hygiene_Triage"]] <- hygiene_triage
}

writexl::write_xlsx(results_list, "emergency_analysis_results.xlsx")

cat("Analysis completed! Results saved to 'emergency_analysis_results.xlsx'\n")

# --- PLOTS & TABLES ---

# 1. Monthly patient count bar plot (12 months only)
png("monthly_patient_count.png", width=900, height=500)
ggplot(monthly_patients_by_month, aes(x=visit_month_only, y=total_patients)) +
  geom_bar(stat="identity", fill="#4682B4") +
  geom_text(aes(label=total_patients), vjust=-0.5, size=4) +
  theme_minimal() +
  labs(title="Monthly Emergency Patient Count (All Years Combined)", x="Month", y="Total Number of Patients") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_x_discrete(limits=month.name)
dev.off()

# 2. Monthly triage code stacked bar plot (12 months only)
png("monthly_triage_stacked.png", width=1000, height=500)
ggplot(monthly_triage_by_month, aes(x=visit_month_only, y=count, fill=triage_label)) +
  geom_bar(stat="identity", position="stack") +
  scale_fill_manual(values=c("ROT"="#FF0000", "ORANGE"="#FFA500", "GELB"="#FFFF00", "GRÜN"="#00FF00", "BLAU"="#0000FF")) +
  theme_minimal() +
  labs(title="Monthly Triage Code Distribution (All Years Combined)", x="Month", y="Number of Patients", fill="Triage Code") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_x_discrete(limits=month.name)
dev.off()

# 3. Heatmap: Image department visits by triage group (x-axis, numeric triage only) - aggregated across all months
img_long_by_triage <- monthly_triage_by_month_img %>%
  select(triage, ends_with("_visits")) %>%
  # Extract numeric part from triage (e.g., "1 ROT" -> "1")
  mutate(triage = as.numeric(sub("^([0-9]+).*", "\\1", as.character(triage)))) %>%
  filter(!is.na(triage) & triage >= 1 & triage <= 5) %>%
  group_by(triage) %>%
  summarise(across(ends_with("_visits"), sum, na.rm=TRUE), .groups='drop') %>%
  pivot_longer(cols=ends_with("_visits"), names_to="department", values_to="visits") %>%
  mutate(department = gsub("_visits", "", department))

# Ensure all triage groups are present and in correct order
triage_levels <- as.character(1:5)
img_long_by_triage$triage <- factor(img_long_by_triage$triage, levels=triage_levels)

# Debug: Check what data we have
cat("Triage groups in data:\n")
print(unique(img_long_by_triage$triage))
cat("Total rows:", nrow(img_long_by_triage), "\n")
cat("Sample data:\n")
print(head(img_long_by_triage, 10))

png("triage_dept_heatmap.png", width=1600, height=1000)
ggplot(img_long_by_triage, aes(x=triage, y=department, fill=visits)) +
  geom_tile(color="black", size=0.8) +
  geom_text(aes(label=ifelse(visits > 0, visits, "")), 
            color="black", size=4, fontface="bold") +
  scale_fill_gradient(low="white", high="#1976D2") +
  scale_x_discrete(drop=FALSE) +
  theme_minimal() +
  labs(title="Imaging Department Visits by Triage (All Months Combined)", 
       x="Triage", y="Department", fill="Visits") +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10, face="bold"),
        axis.text.y = element_text(size=9),
        plot.title = element_text(size=16, face="bold"),
        axis.title = element_text(size=12, face="bold"))
dev.off()

# 4. Top departments by month and triage (table) - 12 months only
top_dept_table_by_month <- top_dept_by_month_triage_by_month %>%
  arrange(visit_month_only, triage, desc(visits))

# Original top departments table (keeping for backward compatibility)
top_dept_table <- top_dept_by_month_triage %>%
  arrange(visit_month_name, triage, desc(visits))

# Add summary tables to Excel
table_summaries <- list(
  "Top_Dept_By_Month_Triage_Table" = top_dept_table,
  "Top_Dept_By_Month_Triage_By_Month_Table" = top_dept_table_by_month
)
writexl::write_xlsx(c(results_list, table_summaries), "emergency_analysis_results.xlsx")

cat("Analysis completed! Results saved to 'emergency_analysis_results.xlsx'\n") 