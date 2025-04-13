# Data Science Salary Analysis

This project analyzes salary trends in the data science field across roles, experience levels, company types, and geographic locations. The goal is to uncover key insights that can guide professionals, recruiters, and organizations in understanding compensation dynamics within the data science industry.

## Project Objective

The primary objective of this analysis is to explore how factors such as experience level, job title, company size, employment type, and work location impact data science salaries. By visualizing and interpreting these relationships, we aim to identify trends and potential gaps that can support career decisions and business planning.

## Dataset

- **Source**: [Kaggle – Data Science Salaries](https://www.kaggle.com/datasets/ruchi798/data-science-job-salaries)
- **Records**: 3,000+ salary entries from 2020–2023
- **Features**:
  - Job Title
  - Experience Level
  - Employment Type
  - Company Size
  - Remote Ratio
  - Salary (in USD)
  - Country
  - Work Year

## Tools and Technologies

- Python (Pandas, NumPy)
- Matplotlib and Seaborn (for visualization)
- Jupyter Notebook
- Git & GitHub

## Project Workflow

### 1. Data Cleaning and Preprocessing
- Removed missing or duplicate entries
- Standardized categorical variables
- Converted currencies to a common format (USD)

### 2. Exploratory Data Analysis (EDA)
- Salary distribution by experience level, company size, and role
- Geographic variation in salaries across countries
- Trends in remote work adoption and its impact on pay

### 3. Key Visualizations
- Box plots for salary ranges across roles and company types
- Bar graphs highlighting top-paying job titles
- Heatmaps showing correlation between features
- Time series plots for salary changes by year

## Key Insights

- **Experience Level**: Executive-level roles significantly outpace junior and mid-level positions in salary.
- **Company Size**: Larger companies tend to offer higher compensation, especially for remote roles.
- **Geography**: Salaries vary widely by country, with U.S.-based roles offering the highest averages.
- **Job Title**: Data Architects and Machine Learning Engineers lead in median salary.

## How to Use This Repository

1. Clone the repository:
   ```bash
   git clone https://github.com/YOUR_USERNAME/data-science-salary-analysis.git