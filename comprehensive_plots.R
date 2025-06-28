# Comprehensive Plots and Tables for Emergency Analysis Results
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(reshape2)
library(lubridate)

# Age group fonksiyonu (herkes için erişilebilir)
create_age_groups <- function(age) {
  ifelse(age <= 20, "0-20",
         ifelse(age <= 40, "21-40",
                ifelse(age <= 60, "41-60", ">60")))
}

# Load all data from Excel
excel_file <- "emergency_analysis_results.xlsx"

# Department name mapping
replace_dept_names <- function(dept_col) {
  mapping <- c(
    "sonography.date" = "Sonography",
    "echo.cardiography.date" = "Echocardiography",
    "ecg.date" = "ECG",
    "laboratory.date" = "Laboratory",
    "test.urine.date" = "Urine Test",
    "cct.date" = "CCT",
    "ct.date" = "CT",
    "mri.date" = "MRI",
    "trauma.scan.date" = "Trauma Scan",
    "xray.extremity.date" = "X-ray Extremity",
    "xray.basin.date" = "X-ray Basin",
    "xray.spine.date" = "X-ray Spine",
    "xray.thorax.date" = "X-ray Thorax",
    "xray.misc.date" = "X-ray Misc"
  )
  return(ifelse(dept_col %in% names(mapping), mapping[dept_col], dept_col))
}

# Load data-aktin.RData
load('data-aktin.RData')

# 1. DEMOGRAPHIC ANALYSIS PLOTS

# Male Age Distribution
male_age <- read_excel(excel_file, sheet = "Male_Age_Distribution")
png("male_age_distribution.png", width=800, height=500)
ggplot(male_age, aes(x=age_group, y=count, fill=age_group)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(count, "\n(", percentage, "%)")), vjust=-0.5, size=4) +
  scale_fill_brewer(palette="Blues") +
  theme_minimal() +
  labs(title="Male Patients Age Distribution", x="Age Group", y="Number of Patients", fill="Age Group") +
  theme(legend.position="none")
dev.off()

# Female Age Distribution
female_age <- read_excel(excel_file, sheet = "Female_Age_Distribution")
png("female_age_distribution.png", width=800, height=500)
ggplot(female_age, aes(x=age_group, y=count, fill=age_group)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(count, "\n(", percentage, "%)")), vjust=-0.5, size=4) +
  scale_fill_brewer(palette="Reds") +
  theme_minimal() +
  labs(title="Female Patients Age Distribution", x="Age Group", y="Number of Patients", fill="Age Group") +
  theme(legend.position="none")
dev.off()

# 2. TRIAGE ANALYSIS PLOTS

# Male Triage Distribution (doğrudan data.aktin'den, renkli ve sıralı)
male_patients <- data.aktin[data.aktin$sex == "M", ]
male_patients$triage_label <- sub("^[0-9] ", "", as.character(male_patients$triage))
triage_levels <- c("ROT", "ORANGE", "GELB", "GRÜN", "BLAU")
male_patients$triage_label <- factor(male_patients$triage_label, levels = triage_levels)
male_triage <- male_patients %>%
  filter(!is.na(triage_label)) %>%
  group_by(triage_label) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = round(100 * count / sum(count), 2))

png("male_triage_distribution.png", width=800, height=500)
ggplot(male_triage, aes(x=triage_label, y=count, fill=triage_label)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(count, "\n(", percentage, "%)")), vjust=-0.5, size=4) +
  scale_fill_manual(values=c("ROT"="#FF0000", "ORANGE"="#FFA500", "GELB"="#FFFF00", "GRÜN"="#00FF00", "BLAU"="#0000FF")) +
  theme_minimal() +
  labs(title="Male Patients Triage Distribution", x="Triage Code", y="Number of Patients", fill="Triage") +
  theme(legend.position="none")
dev.off()

# Male Age-Triage Cross Table (doğrudan data.aktin'den, renkli ve sıralı)
male_patients$age <- as.numeric(floor((as.Date(male_patients$visit.date) - as.Date(male_patients$birth.date))/365.25))
male_patients$age_group <- create_age_groups(male_patients$age)
male_age_triage_long <- male_patients %>%
  filter(!is.na(triage_label)) %>%
  group_by(age_group, triage_label) %>%
  summarise(count = n(), .groups = 'drop')

png("male_age_triage_heatmap.png", width=1000, height=600)
ggplot(male_age_triage_long, aes(x=triage_label, y=age_group, fill=count)) +
  geom_tile(color="white") +
  geom_text(aes(label=count), color="black", size=4) +
  scale_fill_gradient(low="#E3F2FD", high="#1976D2") +
  theme_minimal() +
  labs(title="Male Patients: Age-Triage Cross Table", x="Triage Code", y="Age Group", fill="Number of Patients")
dev.off()

# Female Triage Distribution (doğrudan data.aktin'den, renkli ve sıralı)
female_patients <- data.aktin[data.aktin$sex == "W", ]
female_patients$triage_label <- sub("^[0-9] ", "", as.character(female_patients$triage))
triage_levels <- c("ROT", "ORANGE", "GELB", "GRÜN", "BLAU")
female_patients$triage_label <- factor(female_patients$triage_label, levels = triage_levels)
female_triage <- female_patients %>%
  filter(!is.na(triage_label)) %>%
  group_by(triage_label) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = round(100 * count / sum(count), 2))

png("female_triage_distribution.png", width=800, height=500)
ggplot(female_triage, aes(x=triage_label, y=count, fill=triage_label)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(count, "\n(", percentage, "%)")), vjust=-0.5, size=4) +
  scale_fill_manual(values=c("ROT"="#FF0000", "ORANGE"="#FFA500", "GELB"="#FFFF00", "GRÜN"="#00FF00", "BLAU"="#0000FF")) +
  theme_minimal() +
  labs(title="Female Patients Triage Distribution", x="Triage Code", y="Number of Patients", fill="Triage") +
  theme(legend.position="none")
dev.off()

# 3. AGE-TRIAGE CROSS ANALYSIS

# Female Age-Triage Cross Table
# Sheet'ten okuma yerine doğrudan data.aktin'den üret
female_patients <- data.aktin[data.aktin$sex == "W", ]
# Age group fonksiyonu
female_patients$age <- as.numeric(floor((as.Date(female_patients$visit.date) - as.Date(female_patients$birth.date))/365.25))
female_patients$age_group <- create_age_groups(female_patients$age)
# Triage label'ı faktörün karakterinden çek
female_patients$triage_label <- sub("^[0-9] ", "", as.character(female_patients$triage))
triage_levels <- c("ROT", "ORANGE", "GELB", "GRÜN", "BLAU")
female_patients$triage_label <- factor(female_patients$triage_label, levels = triage_levels)
female_age_triage_long <- female_patients %>%
  filter(!is.na(triage_label)) %>%
  group_by(age_group, triage_label) %>%
  summarise(count = n(), .groups = 'drop')

png("female_age_triage_heatmap.png", width=1000, height=600)
ggplot(female_age_triage_long, aes(x=triage_label, y=age_group, fill=count)) +
  geom_tile(color="white") +
  geom_text(aes(label=count), color="black", size=4) +
  scale_fill_gradient(low="#FFEBEE", high="#D32F2F") +
  theme_minimal() +
  labs(title="Female Patients: Age-Triage Cross Table", x="Triage Code", y="Age Group", fill="Number of Patients")
dev.off()

# Debug: Female hasta sayısı ve dağılımı
cat("Total female patients:", nrow(female_patients), "\n")
cat("Female patients by triage_label:\n")
print(table(female_patients$triage_label, useNA = "ifany"))
cat("Female patients by age_group:\n")
print(table(female_patients$age_group, useNA = "ifany"))

# Triage sütunu ve değerlerini kontrol et
cat("data.aktin sütun isimleri:\n")
print(colnames(data.aktin))
cat("İlk 10 female hastanın triage değeri:\n")
print(head(data.aktin[data.aktin$sex == 'W', c('triage')], 10))

# 4. DEPARTMENT ANALYSIS

# Male Department by Triage (heatmap, renkli ve sayılı)
male_patients <- data.aktin[data.aktin$sex == "M", ]
department_cols <- grep("\\.date$", colnames(data.aktin), value = TRUE)
# Triage label'ı faktörün karakterinden çek
male_patients$triage_label <- sub("^[0-9] ", "", as.character(male_patients$triage))
male_patients$triage_label <- factor(male_patients$triage_label, levels = triage_levels)

male_dept_long <- male_patients %>%
  pivot_longer(cols = all_of(department_cols), names_to = "department", values_to = "visit_date") %>%
  filter(!is.na(visit_date)) %>%
  group_by(department, triage_label) %>%
  summarise(Freq = n(), .groups = 'drop')

male_dept_long$department <- replace_dept_names(male_dept_long$department)

png("male_dept_triage_heatmap.png", width=1200, height=800)
ggplot(male_dept_long, aes(x=triage_label, y=department, fill=triage_label)) +
  geom_tile(aes(alpha=Freq), color="white") +
  geom_text(aes(label=ifelse(Freq>0, Freq, "")), color="black", size=3) +
  scale_fill_manual(values=c("ROT"="#FF0000", "ORANGE"="#FFA500", "GELB"="#FFFF00", "GRÜN"="#00FF00", "BLAU"="#0000FF")) +
  scale_alpha(range=c(0,1), guide="none") +
  theme_minimal() +
  labs(title="Male Patients: Department-Triage Distribution", x="Triage Code", y="Department", fill="Triage Code") +
  theme(axis.text.y = element_text(size=8))
dev.off()

# Female Department by Triage (heatmap, renkli ve sayılı)
female_patients <- data.aktin[data.aktin$sex == "W", ]
department_cols <- grep("\\.date$", colnames(data.aktin), value = TRUE)
# Triage label'ı faktörün karakterinden çek
female_patients$triage_label <- sub("^[0-9] ", "", as.character(female_patients$triage))
triage_levels <- c("ROT", "ORANGE", "GELB", "GRÜN", "BLAU")
female_patients$triage_label <- factor(female_patients$triage_label, levels = triage_levels)

female_dept_long <- female_patients %>%
  pivot_longer(cols = all_of(department_cols), names_to = "department", values_to = "visit_date") %>%
  filter(!is.na(visit_date)) %>%
  group_by(department, triage_label) %>%
  summarise(Freq = n(), .groups = 'drop')

female_dept_long$department <- replace_dept_names(female_dept_long$department)

png("female_dept_triage_heatmap.png", width=1200, height=800)
ggplot(female_dept_long, aes(x=triage_label, y=department, fill=triage_label)) +
  geom_tile(aes(alpha=Freq), color="white") +
  geom_text(aes(label=ifelse(Freq>0, Freq, "")), color="black", size=3) +
  scale_fill_manual(values=c("ROT"="#FF0000", "ORANGE"="#FFA500", "GELB"="#FFFF00", "GRÜN"="#00FF00", "BLAU"="#0000FF")) +
  scale_alpha(range=c(0,1), guide=FALSE) +
  theme_minimal() +
  labs(title="Female Patients: Department-Triage Distribution", x="Triage Code", y="Department", fill="Triage Code") +
  theme(axis.text.y = element_text(size=8))
dev.off()

# 5. MONTHLY ANALYSIS

# Monthly Patient Counts (sadece 12 ay, triage_label yok)
monthly_patients <- read_excel(excel_file, sheet = "Monthly_Patients_By_Month")
monthly_patients$visit_month_only <- factor(monthly_patients$visit_month_only, levels=month.name)

png("monthly_patient_trend.png", width=1000, height=500)
ggplot(monthly_patients, aes(x=visit_month_only, y=total_patients, group=1)) +
  geom_bar(stat="identity", fill="#1976D2") +
  geom_text(aes(label=total_patients), vjust=-0.5, size=4) +
  theme_minimal() +
  labs(title="Monthly Emergency Patient Count Trend (All Years Combined)", x="Month", y="Total Number of Patients") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_x_discrete(limits=month.name)
dev.off()

# Monthly Triage Distribution
monthly_triage <- read_excel(excel_file, sheet = "Monthly_Triage_By_Month")
monthly_triage$visit_month_only <- factor(monthly_triage$visit_month_only, levels=month.name)

png("monthly_triage_distribution.png", width=1200, height=600)
ggplot(monthly_triage, aes(x=visit_month_only, y=count, fill=triage_label)) +
  geom_bar(stat="identity", position="stack") +
  scale_fill_manual(values=c("ROT"="#FF0000", "ORANGE"="#FFA500", "GELB"="#FFFF00", "GRÜN"="#00FF00", "BLAU"="#0000FF")) +
  theme_minimal() +
  labs(title="Monthly Triage Code Distribution (All Years Combined)", x="Month", y="Number of Patients", fill="Triage Code") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_x_discrete(limits=month.name)
dev.off()

# Monthly Imaging Department Visits (Triage groups on x-axis instead of months)
monthly_imaging <- read_excel(excel_file, sheet = "Monthly_Triage_By_Month_Imag")
img_cols <- grep("_visits$", colnames(monthly_imaging), value=TRUE)

# Aggregate by triage group and department (sum across all months)
triage_dept_visits <- monthly_imaging %>%
  select(triage, all_of(img_cols)) %>%
  # Extract numeric part from triage (e.g., "1 ROT" -> "1")
  mutate(triage = as.numeric(sub("^([0-9]+).*", "\\1", as.character(triage)))) %>%
  filter(!is.na(triage) & triage >= 1 & triage <= 5) %>%
  group_by(triage) %>%
  summarise(across(all_of(img_cols), sum, na.rm=TRUE), .groups='drop') %>%
  pivot_longer(cols=img_cols, names_to="department", values_to="visits") %>%
  mutate(department = gsub("_visits", "", department),
         department = replace_dept_names(department))

# Ensure all triage groups are present and in correct order
triage_levels <- as.character(1:5)
triage_dept_visits$triage <- factor(triage_dept_visits$triage, levels=triage_levels)

# Debug: Check what data we have
cat("Triage groups in data:\n")
print(unique(triage_dept_visits$triage))
cat("Total rows:", nrow(triage_dept_visits), "\n")
cat("Sample data:\n")
print(head(triage_dept_visits, 10))

png("triage_dept_heatmap.png", width=1600, height=1000)
ggplot(triage_dept_visits, aes(x=triage, y=department, fill=visits)) +
  geom_tile(color="black", size=0.8) +
  geom_text(aes(label=ifelse(visits > 0, visits, "")), 
            color="black", size=4, fontface="bold") +
  scale_fill_gradient(low="white", high="#1976D2") +
  scale_x_discrete(drop=FALSE) +
  theme_minimal() +
  labs(title="Imaging Department Visits by Triage (All Months Combined)", 
       x="Triage", y="Imaging Department", fill="Number of Visits") +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10, face="bold"),
        axis.text.y = element_text(size=9),
        plot.title = element_text(size=16, face="bold"),
        axis.title = element_text(size=12, face="bold"))
dev.off()

# 6. IMAGING DEPARTMENT ANALYSIS

# Monthly Imaging Department Visits
monthly_imaging <- read_excel(excel_file, sheet = "Monthly_Triage_Named_Imaging")
png("monthly_imaging_analysis.png", width=1400, height=800)
img_cols <- grep("_visits$", colnames(monthly_imaging), value=TRUE)
monthly_img_long <- monthly_imaging %>%
  select(visit_month_name, triage_label, all_of(img_cols)) %>%
  pivot_longer(cols=img_cols, names_to="department", values_to="visits") %>%
  mutate(department = gsub("_visits", "", department),
         department = replace_dept_names(department))

ggplot(monthly_img_long, aes(x=visit_month_name, y=department, fill=visits)) +
  geom_tile(color="white") +
  geom_text(aes(label=ifelse(visits > 0, visits, "")), color="black", size=2.5) +
  facet_wrap(~triage_label, ncol=3) +
  scale_fill_gradient(low="#F5F5F5", high="#1976D2") +
  theme_minimal() +
  labs(title="Monthly Imaging Department Visits by Triage Code", x="Month", y="Imaging Department", fill="Number of Visits") +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=8),
        axis.text.y = element_text(size=8))
dev.off()

# 7. TOP DEPARTMENTS BY MONTH AND TRIAGE

# --- NEW: Calculate imaging department visits by month and triage directly from data.aktin ---
# Imaging department columns
imaging_depts <- c(
  "sonography.date", "echo.cardiography.date", "ecg.date", "laboratory.date", "test.urine.date", "cct.date", "ct.date", "mri.date", "trauma.scan.date", "xray.extremity.date", "xray.basin.date", "xray.spine.date", "xray.thorax.date", "xray.misc.date"
)
imaging_dept_names <- c(
  "sonography.date" = "Sonography",
  "echo.cardiography.date" = "Echocardiography",
  "ecg.date" = "ECG",
  "laboratory.date" = "Laboratory",
  "test.urine.date" = "Urine Test",
  "cct.date" = "CCT",
  "ct.date" = "CT",
  "mri.date" = "MRI",
  "trauma.scan.date" = "Trauma Scan",
  "xray.extremity.date" = "X-ray Extremity",
  "xray.basin.date" = "X-ray Basin",
  "xray.spine.date" = "X-ray Spine",
  "xray.thorax.date" = "X-ray Thorax",
  "xray.misc.date" = "X-ray Misc"
)

# Prepare data - aggregate by month name only (not year)
aktin_long <- data.aktin %>%
  mutate(visit_month_name = format(as.Date(visit.date), "%B"),  # Only month name, no year
         triage = as.character(sub("^([0-9]+).*", "\\1", as.character(triage)))) %>%
  pivot_longer(cols = all_of(imaging_depts), names_to = "department", values_to = "visit_date") %>%
  filter(!is.na(visit_date), triage %in% as.character(1:5)) %>%
  group_by(visit_month_name, triage, department) %>%
  summarise(visits = n(), .groups = 'drop')
aktin_long$department <- imaging_dept_names[aktin_long$department]

# Ensure months are in correct order
aktin_long$visit_month_name <- factor(aktin_long$visit_month_name, levels = month.name)

# Plot
png("top_departments_monthly.png", width=1200, height=800)
ggplot(aktin_long, aes(x=visit_month_name, y=department, size=visits, color=triage)) +
  geom_point(alpha=0.7) +
  scale_size_continuous(range=c(2, 8)) +
  scale_color_manual(values=c("1"="#FF0000", "2"="#FFA500", "3"="#FFFF00", "4"="#00FF00", "5"="#0000FF")) +
  theme_minimal() +
  labs(title="Most Visited Imaging Departments by Month and Triage (All Years Combined)", x="Month", y="Department", size="Number of Visits", color="Triage") +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.text.y = element_text(size=8)) +
  scale_x_discrete(limits=month.name)
dev.off()

# 8. HYGIENE ANALYSIS (if available)

if("Hygiene_Triage" %in% excel_sheets(excel_file)) {
  hygiene_triage <- read_excel(excel_file, sheet = "Hygiene_Triage")
  png("hygiene_triage_analysis.png", width=1000, height=600)
  ggplot(hygiene_triage, aes(x=hygiene.reason, y=count, fill=triage_label)) +
    geom_bar(stat="identity", position="stack") +
    scale_fill_manual(values=c("ROT"="#FF0000", "ORANGE"="#FFA500", "GELB"="#FFFF00", "GRÜN"="#00FF00", "BLAU"="#0000FF")) +
    theme_minimal() +
    labs(title="Triage Distribution by Hygiene Reason", x="Hygiene Reason", y="Number of Patients", fill="Triage Code") +
    theme(axis.text.x = element_text(angle=45, hjust=1, size=8))
  dev.off()
}

# 9. SUMMARY DASHBOARD

# Triage color mapping (including Unknown)
triage_colors <- c(
  "ROT" = "#FF0000",
  "ORANGE" = "#FFA500",
  "GELB" = "#FFFF00",
  "GRÜN" = "#00FF00",
  "BLAU" = "#0000FF",
  "Unknown" = "#888888"
)

# Imaging departments for Top 10 (sadece bu departmanlar)
imaging_depts <- c(
  "sonography.date", "echo.cardiography.date", "ecg.date", "laboratory.date", "test.urine.date", "cct.date", "ct.date", "mri.date", "trauma.scan.date", "xray.extremity.date", "xray.basin.date", "xray.spine.date", "xray.thorax.date", "xray.misc.date"
)
# Departman isimlerini İngilizce'ye çevir
imaging_dept_names <- c(
  "sonography.date" = "Sonography",
  "echo.cardiography.date" = "Echocardiography",
  "ecg.date" = "ECG",
  "laboratory.date" = "Laboratory",
  "test.urine.date" = "Urine Test",
  "cct.date" = "CCT",
  "ct.date" = "CT",
  "mri.date" = "MRI",
  "trauma.scan.date" = "Trauma Scan",
  "xray.extremity.date" = "X-ray Extremity",
  "xray.basin.date" = "X-ray Basin",
  "xray.spine.date" = "X-ray Spine",
  "xray.thorax.date" = "X-ray Thorax",
  "xray.misc.date" = "X-ray Misc"
)

# --- FACET DATA HAZIRLA (DASHBOARD'DAN ÖNCE OLMALI) ---
triage_map <- c(
  "1 ROT" = "ROT",
  "2 ORANGE" = "ORANGE",
  "3 GELB" = "GELB",
  "4 GRÜN" = "GRÜN",
  "5 BLAU" = "BLAU"
)
data.aktin$triage_group <- triage_map[as.character(data.aktin$triage)]
data.aktin$triage_group[is.na(data.aktin$triage_group)] <- "Unknown"

facet_data <- lapply(c("ROT", "ORANGE", "GELB", "GRÜN", "BLAU"), function(triage) {
  sub <- data.aktin[data.aktin$triage_group == triage, ]
  counts <- sapply(imaging_depts, function(col) sum(!is.na(sub[[col]])))
  df <- data.frame(
    triage = triage,
    department = names(counts),
    visits = as.numeric(counts)
  )
  df <- df[order(-df$visits), ]
  df <- head(df, 5)
  df$department <- imaging_dept_names[df$department]
  df
})
facet_df <- do.call(rbind, facet_data)
# --- FACET DATA HAZIRLA SONU ---

# Create a summary dashboard with key metrics
png("emergency_dashboard.png", width=1600, height=1200)
par(mfrow=c(2,3))

# 1. Triage Distribution (PIE CHART)
triage_levels <- c("ROT", "ORANGE", "GELB", "GRÜN", "BLAU", "Unknown")
triage_colors <- c(
  "ROT" = "#FF0000",
  "ORANGE" = "#FFA500",
  "GELB" = "#FFFF00",
  "GRÜN" = "#00FF00",
  "BLAU" = "#0000FF",
  "Unknown" = "#888888"
)
triage_summary <- rbind(male_triage, female_triage) %>%
  group_by(triage_label) %>%
  summarise(total_count = sum(count))
triage_summary <- merge(
  data.frame(triage_label=triage_levels),
  triage_summary,
  by="triage_label",
  all.x=TRUE
)
triage_summary$total_count[is.na(triage_summary$total_count)] <- 0
pie_colors <- triage_colors[triage_summary$triage_label]
pie_labels <- paste(triage_summary$triage_label, "\n", triage_summary$total_count, " patients")
pie(triage_summary$total_count, labels=pie_labels, col=pie_colors, main="Triage Distribution (All Groups)")

# 2. Gender Distribution
gender_summary <- data.frame(
  Gender = c("Male", "Female"),
  Count = c(sum(male_age$count), sum(female_age$count))
)
barplot(gender_summary$Count, names.arg=gender_summary$Gender, 
        col=c("#1976D2", "#D32F2F"), main="Gender Distribution")

# 3. Monthly Trend (All Years Combined, by Month Name)
monthly_trend_data <- read_excel(excel_file, sheet = "Monthly_Patients_By_Month")
monthly_trend_data$visit_month_only <- factor(monthly_trend_data$visit_month_only, levels=month.name)
barplot(monthly_trend_data$total_patients, names.arg=monthly_trend_data$visit_month_only, col="#1976D2",
        main="Monthly Trend (All Years Combined)", xlab="Month", ylab="Total Number of Patients", las=2)

# 4. Age Distribution (Barplot)
age_summary <- rbind(male_age, female_age) %>%
  group_by(age_group) %>%
  summarise(total_count = sum(count))
barplot(age_summary$total_count, names.arg=age_summary$age_group,
        col="#4CAF50", main="Age Distribution", ylab="Number of Patients", xlab="Age Group")

# 5. Top 5 Most Visited Imaging Departments (Barplot, data.aktin'den, sadece NA olmayan tarihleri say)
visit_counts <- sapply(imaging_depts, function(col) sum(!is.na(data.aktin[[col]])))
visit_df <- data.frame(department=names(visit_counts), visits=as.numeric(visit_counts))
visit_df$department <- imaging_dept_names[visit_df$department]
visit_df <- visit_df[order(-visit_df$visits), ]
visit_df <- head(visit_df, 5)
bp <- barplot(visit_df$visits, names.arg=visit_df$department, col="#FF9800", main="Top 5 Most Visited Imaging Departments", las=2, ylab="Number of Visits", xlab="Department")
text(x=bp, y=visit_df$visits, labels=visit_df$visits, pos=3, cex=1.2, col="black")

# 6. panel: boş bırak
plot.new()

par(mfrow=c(1,1))
dev.off()

cat("All plots and tables have been generated successfully!\n")
cat("Generated files:\n")
cat("- male_age_distribution.png\n")
cat("- female_age_distribution.png\n")
cat("- male_triage_distribution.png\n")
cat("- female_triage_distribution.png\n")
cat("- male_age_triage_heatmap.png\n")
cat("- female_age_triage_heatmap.png\n")
cat("- male_dept_triage_heatmap.png\n")
cat("- female_dept_triage_heatmap.png\n")
cat("- monthly_patient_trend.png\n")
cat("- monthly_triage_distribution.png\n")
cat("- monthly_imaging_analysis.png\n")
cat("- top_departments_monthly.png\n")
cat("- emergency_dashboard.png\n")
if("Hygiene_Triage" %in% excel_sheets(excel_file)) {
  cat("- hygiene_triage_analysis.png\n")
} 