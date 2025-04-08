# Load necessary libraries
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)  
library(dplyr) 
library(viridis) 
library(countrycode)

# Read dataset
df <- read.csv("ds_salaries.csv")

# ---- Top 10 Highest Paying Data Science Jobs ---- #
avg_salary_by_job <- df %>%
  group_by(job_title) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE)) %>%
  arrange(desc(avg_salary))

top_10_jobs <- avg_salary_by_job[1:10,]

ggplot(top_10_jobs, aes(x = reorder(job_title, avg_salary), y = avg_salary, fill = avg_salary)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.97) +
  geom_text(aes(label = scales::dollar(avg_salary, accuracy = 1, scale = 1e-3, suffix = "K")), 
            hjust = -0.09, size = 5, fontface = "bold") +  
  coord_flip() +  
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K", accuracy = 1), 
                     expand = expansion(mult = c(0, 0.15))) + 
  labs(
    title = "Top 10 Highest Paying Data Science Roles",
    subtitle = "Based on Average Salary (USD) Across Job Titles",
    x = "", y = "", caption = "Source: ds_salaries.csv"
  ) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.margin = margin(10, 90, 10, 10)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue")

# ---- Job Trends Over Time ---- #
df <- df %>%
  mutate(start_date = as.Date(paste0(work_year, "-01-01")))

jobs_salaries_per_year <- df %>%
  group_by(start_date) %>%
  summarise(job_count = n(),  
            avg_salary = mean(salary_in_usd, na.rm = TRUE))

ggplot(jobs_salaries_per_year, aes(x = start_date)) +
  geom_line(aes(y = job_count, color = "Job Count"), size = 1.5) +
  geom_point(aes(y = job_count, color = "Job Count"), size = 4) +
  geom_line(aes(y = avg_salary / 500, color = "Avg Salary (Scaled)"), size = 1.5, linetype = "dashed") +
  geom_point(aes(y = avg_salary / 500, color = "Avg Salary (Scaled)"), size = 4, shape = 17) +
  labs(
    title = "Trends in Data Science Job Postings & Salaries Over Time",
    subtitle = "Job Openings vs. Average Salary (Scaled) Per Year",
    x = "", y = "", color = "Metrics", caption = "Source: ds_salaries.csv"
  ) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = c(0.15, 0.85),
        legend.background = element_rect(fill = "white", color = "black")) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black")

# ---- Salary Distribution: Remote vs. On-Site ---- #
df$work_type <- ifelse(df$remote_ratio > 50, "Remote", "On-Site")

med_remote <- median(df$salary_in_usd[df$work_type == "Remote"], na.rm = TRUE)
med_onsite <- median(df$salary_in_usd[df$work_type == "On-Site"], na.rm = TRUE)

ggplot(df, aes(x = salary_in_usd, fill = work_type)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 40) +  
  geom_density(aes(y = ..density.. * max(..count..)), alpha = 0.3) +  
  geom_vline(xintercept = med_remote, color = "darkblue", linetype = "dashed", size = 1) +  
  geom_vline(xintercept = med_onsite, color = "skyblue", linetype = "dashed", size = 1) +  
  scale_fill_manual(values = c("Remote" = "darkblue", "On-Site" = "skyblue")) +  
  scale_x_continuous(labels = dollar_format()) +  
  labs(
    title = "Salary Distribution: Remote vs. On-Site Data Science Jobs",
    subtitle = "Does Remote Work Pay More? (Dashed Lines = Median Salary)",
    x = "", y = "", fill = "Work Type", caption = "Source: ds_salaries.csv"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom")

# ---- Salary Distribution by Employment Type ---- #
employment_labels <- c("FT" = "Full-Time", "CT" = "Contract", 
                       "FL" = "Freelance", "PT" = "Part-Time")

ggplot(df, aes(x = employment_type, y = salary_in_usd, fill = employment_type)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.color = "red", outlier.fill = "red") +  
  stat_summary(fun = mean, geom = "point", shape = 8, size = 3, color = "black") +  
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +  
  scale_x_discrete(labels = employment_labels) +
  labs(
    title = "Salary Distribution by Employment Type",
    subtitle = "Which Employment Type Has the Most Stable Income?",
    x = "Employment Type", y = "", caption = "Source: ds_salaries.csv"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none") +
  scale_fill_manual(values = c("FL" = "#E69F00", "CT" = "#56B4E9", 
                               "PT" = "#009E73", "FT" = "#D55E00"),
                    labels = employment_labels)

# ---- World Map: Data Science Salaries ---- #
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

salary_by_country <- df %>%
  group_by(company_location) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE)) %>%
  rename(country = company_location)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

salary_by_country <- df %>%
  group_by(company_location) %>%
  summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE)) %>%
  rename(country = company_location)

world <- world %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(longitude = st_coordinates(centroid)[,1],
         latitude = st_coordinates(centroid)[,2])

map_data <- world %>%
  left_join(salary_by_country, by = c("iso_a2" = "country"))

map_data <- map_data %>%
  filter(!is.na(longitude) & !is.na(latitude) & !is.na(avg_salary))

ggplot(data = world) +
  geom_sf(fill = "gray85", color = "white") +  
  geom_point(data = map_data, aes(x = longitude, y = latitude, 
                                  size = avg_salary, color = avg_salary), 
             alpha = 0.5) +  
  scale_size_continuous(range = c(3, 15), labels = scales::dollar_format(scale = 1e-3, suffix = "K")) +  
  scale_color_viridis_c(option = "plasma", direction = -1, labels = scales::dollar_format(scale = 1e-3, suffix = "K")) +  
  labs(
    title = "💰 Data Science Salaries Around the World",
    subtitle = "Bubble Size = Average Salary | Color = Salary Level",
    color = "Avg Salary (USD)",
    size = "Salary (USD)",
    caption = "Source: ds_salaries.csv"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14, margin = margin(b = 10)),
    legend.position = "bottom"
  )

