# Emergency Department Data Analysis Project

This project is developed for comprehensive analysis and visualization of emergency department patient data.

## 📊 Project Description

This project analyzes emergency department patient profiles, triage codes, and department visits. Results are exported to Excel files and PNG visualizations.

## 📁 File Structure

### Main R Scripts
- `emergency_analysis.R` - Data processing and Excel output
- `comprehensive_plots.R` - Visualization and dashboard creation

### Data Files
- `data-aktin.RData` - Main dataset
- `emergency_analysis_results.xlsx` - Analysis results (multiple sheets)
- `ai3-codebook.xlsx` - Data coding guide

### Visualizations
- `emergency_dashboard.png` - Main dashboard
- `top_departments_monthly.png` - Monthly most visited departments
- `male_age_distribution.png` - Male patient age distribution
- `female_age_distribution.png` - Female patient age distribution
- `male_triage_distribution.png` - Male patient triage distribution
- `female_triage_distribution.png` - Female patient triage distribution
- `male_age_triage_heatmap.png` - Male patient age-triage cross table
- `female_age_triage_heatmap.png` - Female patient age-triage cross table
- `male_dept_triage_heatmap.png` - Male patient department-triage distribution
- `female_dept_triage_heatmap.png` - Female patient department-triage distribution
- `monthly_patient_trend.png` - Monthly patient trend
- `monthly_triage_distribution.png` - Monthly triage distribution
- `monthly_imaging_analysis.png` - Monthly imaging analysis
- `triage_dept_heatmap.png` - Triage-department heatmap

## 🚀 Usage

### Requirements
```r
# Required R packages
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(reshape2)
library(lubridate)
library(openxlsx)
```

### Execution
1. **Data Analysis:**
   ```r
   source("emergency_analysis.R")
   ```

2. **Visualization:**
   ```r
   source("comprehensive_plots.R")
   ```

## 📈 Analysis Features

### Demographic Analysis
- Patient distribution by age groups (male/female)
- Gender-based analyses

### Triage Analysis
- Patient distribution by triage codes (1-5)
- Age-triage cross analysis
- Gender-based triage distribution

### Department Analysis
- Imaging department visit analysis
- Triage-department heatmaps
- Most visited departments

### Time Series Analysis
- Monthly patient trends (all years combined)
- Monthly triage distributions
- Seasonal pattern analysis

## 🏥 Imaging Departments

Analyzed imaging departments:
- Sonography (Ultrasound)
- Echocardiography
- ECG (Electrocardiography)
- Laboratory
- Urine Test
- CCT (Cranial CT)
- CT (Computed Tomography)
- MRI (Magnetic Resonance Imaging)
- Trauma Scan
- X-ray Extremity
- X-ray Basin
- X-ray Spine
- X-ray Thorax
- X-ray Misc

## 📊 Dashboard Features

Main dashboard (`emergency_dashboard.png`) includes:
1. Triage distribution (pie chart)
2. Gender distribution
3. Monthly trend (all years combined)
4. Age distribution
5. Top 5 most visited imaging departments

## 🔧 Technical Details

- **Data Format:** RData and Excel
- **Visualization:** ggplot2
- **Data Processing:** dplyr, tidyr
- **Output Formats:** PNG, Excel (multiple sheets)

## 📝 Notes

- All monthly analyses are performed by combining years
- Triage codes are used numerically between 1-5
- Imaging departments are named in English
- All visualizations are saved in high-resolution PNG format

## 👨‍💻 Developer

This project is developed by [Senol Dogan](https://github.com/SenolDogan).

## 📄 License

This project is open source and can be used for educational purposes. 