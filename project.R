library(ggplot2)
library(dplyr)
library(scales)
library(sf)
library(viridis)
library(stringr)
library(ggridges) 


# Load dataset
df <- read.csv("ds_salaries.csv")

# 1. Bar Chart: Top 10 Highest Paying Data Science Roles
avg_salary_by_job <- df %>%
  group_by(job_title) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE)) %>%
  arrange(desc(avg_salary))

top_10_jobs <- avg_salary_by_job[1:10,]

ggplot(top_10_jobs, aes(x = reorder(job_title, avg_salary), y = avg_salary, fill = avg_salary)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.97) +
  geom_text(aes(label = scales::dollar(avg_salary, accuracy = 1, scale = 1e-3, suffix = "K")),
            hjust = -0.1, size = 5, fontface = "bold") + 
  coord_flip() +  
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K", accuracy = 1), 
                     expand = expansion(mult = c(0, 0.15))) + 
  labs(
    title = "Top 10 Highest Paying Data Science Roles",
    subtitle = "Surprisingly, Principal roles earn more than Directors/Managers",
    x = "", y = "Average Salary (USD)",
  ) +
  theme_minimal(base_size = 16) +  
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.margin = margin(10, 30, 10, 10),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()  
  ) +
  scale_fill_gradient(low = "lightblue", high = "darkblue")

# 2.Line Chart: Salary Trends Over Time
avg_salary_by_year <- df %>%
  group_by(work_year) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE))

ggplot(avg_salary_by_year, aes(x = work_year, y = avg_salary)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  theme_minimal(base_size = 16) +
  labs(
    title = "Trends in Data Science Salaries Over Time",
    subtitle = "Salaries have shown a consistent upward trend",
    x = "", y = ""
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
  ) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K", accuracy = 1))


# 3. Histogram: Remote vs On-Site Salaries
df$work_type <- ifelse(df$remote_ratio > 50, "Remote", "On-Site")

med_remote <- median(df$salary_in_usd[df$work_type == "Remote"], na.rm = TRUE)
med_onsite <- median(df$salary_in_usd[df$work_type == "On-Site"], na.rm = TRUE)

ggplot(df, aes(x = salary_in_usd, fill = work_type)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 40) +  
  geom_density(aes(y = ..density.. * max(..count..)), alpha = 0.3) +  
  geom_vline(xintercept = med_remote, color = "darkblue", linetype = "dashed", size = 1) +  
  geom_vline(xintercept = med_onsite, color = "skyblue", linetype = "dashed", size = 1) +  
  scale_fill_manual(values = c("Remote" = "darkblue", "On-Site" = "skyblue")) +  
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K", accuracy = 1)) +  
  labs(
    title = "Salary Distribution: Remote vs. On-Site Data Science Jobs",
    subtitle = "Does Remote Work Pay More? (Dashed Lines = Median Salary)",
    x = "",
    y = "Count",
    fill = "Work Type",
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = c(0.85, 0.85)
  )

# 4.Boxplot: Salary Distribution by Employment Type
# Convert employment type abbreviations to full names
df$employment_type <- recode(df$employment_type,
                             "FT" = "Full-Time",
                             "PT" = "Part-Time",
                             "CT" = "Contract",
                             "FL" = "Freelance")

# Order employment types by median salary
median_salaries <- df %>%
  group_by(employment_type) %>%
  summarise(median_salary = median(salary_in_usd, na.rm = TRUE)) %>%
  arrange(desc(median_salary))

df$employment_type <- factor(df$employment_type, levels = median_salaries$employment_type)

ggplot(df, aes(x = employment_type, y = salary_in_usd, fill = employment_type)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, outlier.color = "black", outlier.fill = "white") +
  theme_minimal(base_size = 16) +
  labs(
    title = "Salary Distribution by Employment Type",
    subtitle = "Which Employment Type Has the Most Stable Income?",
    x = "Employment Type", y = ""
  ) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K", accuracy = 1)) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
  )

# 5. Bar: Salary by Region (Bar Chart)
# Categorize countries into regions
df$region <- recode(df$company_location,
                    "US" = "North America", "CA" = "North America", "MX" = "North America",
                    "GB" = "Europe", "DE" = "Europe", "FR" = "Europe", "ES" = "Europe", "IT" = "Europe",
                    "IN" = "Asia", "CN" = "Asia", "JP" = "Asia", "SG" = "Asia",
                    "AU" = "Oceania", "NZ" = "Oceania",
                    .default = "Other")

# Compute average salary by region
avg_salary_by_region <- df %>%
  group_by(region) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE)) %>%
  arrange(desc(avg_salary))

# Create the bar chart
ggplot(avg_salary_by_region, aes(x = reorder(region, avg_salary), y = avg_salary, fill = region)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +  
  geom_text(aes(label = paste0("$", round(avg_salary / 1000), "K")), 
            hjust = -0.1, size = 5, fontface = "bold") +  
  coord_flip() +  
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K", accuracy = 1),
                     expand = expansion(mult = c(0, 0.15))) +  
  labs(
    title = "Average Data Science Salaries by Region",
    subtitle = "Where in the world are the highest paying jobs?",
    x = "Region", y = "",
    caption = "Regions: NA = US, CA, MX | EU = GB, DE, FR, ES, IT | Asia = IN, CN, JP, SG | Oceania = AU, NZ | Other = South America, Africa, etc."
  ) +
  theme_minimal(base_size = 16) +  
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 50, 10, 10)  
  ) +
  scale_fill_manual(values = c("North America" = "#4C72B0", "Europe" = "#55A868", "Asia" = "#C44E52", "Oceania" = "#8172B3", "Other" = "#6D6D6D"))


# 6. Box Plot: Salary Distribution
# Identify the top 5 highest-paying job titles
top_jobs <- df %>%
  group_by(job_title) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE)) %>%
  arrange(desc(avg_salary)) %>%
  slice(1:5) %>%
  pull(job_title)


# Mapping experience levels to meaningful labels
df$experience_level <- factor(df$experience_level, 
                              levels = c("EN", "MI", "SE", "EX"),
                              labels = c("Entry-Level", "Mid-Level", "Senior-Level", "Executive-Level"))

# Create the boxplot
ggplot(df, aes(x = experience_level, y = salary_in_usd, fill = experience_level)) +
  geom_boxplot(alpha = 0.7, outlier.color = "black", outlier.size = 2) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K")) +  
  labs(
    title = "Salary Distribution by Experience Level",
    subtitle = "Comparing salary ranges across different experience levels",
    x = "Experience Level",
    y = "Salary (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "none"  # Remove legend as it's redundant
  ) +
  scale_fill_manual(values = c("#F4A9A8", "#A6CE82", "#76B7B2", "#C5A3FF"))  

#7. Bar: Countries Paying the Highest
# Filter and aggregate data correctly
entry_level_salaries <- df %>%
  filter(experience_level == "Entry-Level", !is.na(salary_in_usd)) %>%
  group_by(company_location) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_salary)) %>%
  head(10)

# Convert country codes to full names
entry_level_salaries$country_name <- countrycode(entry_level_salaries$company_location, 
                                                 origin = "iso2c", destination = "country.name")

# Ensure correct ordering
entry_level_salaries <- entry_level_salaries %>%
  arrange(desc(avg_salary))

# Create bar plot with improvements
ggplot(entry_level_salaries, aes(x = reorder(country_name, avg_salary), y = avg_salary, fill = avg_salary)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Countries Paying the Highest Entry-Level Salaries",
    subtitle = "Where in the world should you consider working after graduation?",
    x = "Country",
    y = "Average Entry-Level Salary (USD)"
  ) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") + 
  scale_y_continuous(labels = dollar_format(prefix = "$", scale = 1e-3, suffix = "K"),
                     expand = c(0, 0.05)) +  # Prevent clipping
  expand_limits(y = max(entry_level_salaries$avg_salary) * 1.1) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.margin = margin(10, 20, 10, 10),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank() 
  )

#8. Line chart: Data Science Salaries & Job Demand Over Time

# Aggregate data: Compute average salary & job count per year
df_summary <- df %>%
  group_by(work_year) %>%
  summarise(
    avg_salary = mean(salary_in_usd, na.rm = TRUE),
    job_demand = n(),  
    .groups = "drop"
  )

# Compute scaling factor for job demand to match salary range
scaling_factor <- max(df_summary$avg_salary) / max(df_summary$job_demand)

# Create the dual-line chart
ggplot(df_summary, aes(x = work_year)) +
  
geom_line(aes(y = avg_salary, color = "Salary Growth"), size = 1.5) +
geom_point(aes(y = avg_salary, color = "Salary Growth"), size = 3, shape = 16) +
  

  geom_line(aes(y = job_demand * scaling_factor, color = "Job Demand"), 
            linetype = "dashed", size = 1.5) +
  geom_point(aes(y = job_demand * scaling_factor, color = "Job Demand"), 
             size = 3, shape = 16) +
  
  scale_color_manual(values = c("Salary Growth" = "#1f77b4", "Job Demand" = "#d62728")) +
  
  # Formatting Y-axis
  scale_y_continuous(
    name = "Average Salary (USD)",
    labels = scales::dollar_format(prefix = "$", scale = 1e-3, suffix = "K"),
    sec.axis = sec_axis(~ . / scaling_factor, 
                        name = "Job Postings", labels = comma, 
                        breaks = seq(0, max(df_summary$job_demand), by = 500))
  ) +
  
  # X-axis Formatting
  scale_x_continuous(breaks = unique(df_summary$work_year), labels = unique(df_summary$work_year)) +
  
  # Labels & Styling
  labs(
    title = "Trends in Data Science Salaries & Job Demand Over Time",
    subtitle = "How salaries and job availability have evolved in recent years",
    x = "Year",
    color = "Metric"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.y.right = element_text(size = 14, color = "#d62728"),
    legend.position = "top",
    legend.title = element_blank()
  )
