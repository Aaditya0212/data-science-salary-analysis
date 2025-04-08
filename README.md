# 📊 Data Science Salary Analysis (2023)

**An analytical exploration of global data science salaries by role, experience, and geography, built using R. This project provides actionable insights for career decisions and salary expectations in the data science field.**

---

## 🧠 Project Overview

In this project, we analyze and visualize data science salary data from 2023 to uncover trends in compensation based on:

- **Experience Level**
- **Job Title**
- **Region/Location**
- **Company Size**
- **Remote Work Ratio**

The dataset provides an overview of salaries across various positions within the data science field, offering valuable insights for both aspiring professionals and hiring managers.

---

## 📁 Dataset Overview

- **Source**: Custom dataset containing real-world data science salary information
- **Total Entries**: 375+ records
- **Key Features**:
  - `work_year`: Year of the data entry (2023)
  - `experience_level`: Role experience (e.g., EN, MI, SE, EX)
  - `job_title`: Specific job title (e.g., Data Scientist, ML Engineer)
  - `salary_in_usd`: Salary in USD for standardized comparison
  - `company_size`: Small (S), Medium (M), Large (L)
  - `remote_ratio`: Percentage of remote work allowed by company
  - `employee_residence`: Country of employee’s residence
  - `company_location`: Location of the company

---

## 🔍 Key Insights

- **Top Earners**: Senior roles (`SE`) and executive roles (`EX`) command the highest salaries, with **Principal Data Scientists** and **Machine Learning Engineers** at the top.
- **Regional Insights**: **North America** (US, CA) offers the highest salaries, while emerging markets show a strong upward trend in compensation.
- **Remote Work**: Companies offering remote opportunities tend to offer higher salaries, reflecting the increased demand for flexible work arrangements.
- **Company Size**: Large companies (L) typically provide the highest salaries, while smaller companies (S) offer more modest compensation packages.

---

## 📈 Data Visualizations

This project includes several visualizations that break down:
- Salary distributions by **experience level**
- Salary comparison across **job titles**
- Salary variance by **region**
- **Company size** and its correlation with salary data

The insights drawn from these visualizations can be used by both job seekers to align their expectations and recruiters to benchmark salaries in their region.

---

## 🛠️ Tech Stack

- **Programming Language**: R
- **Tools**: RStudio
- **Libraries**: 
  - `ggplot2` for data visualization
  - `dplyr`, `tidyverse` for data wrangling
  - `readr` for data input and output

---

## 🚀 How to Run

1. Clone the repository:

```bash
git clone https://github.com/Aaditya0212/data-science-salary-analysis.git