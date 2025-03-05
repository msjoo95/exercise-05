# Challenge 1

library (tidyverse)

# Step 1
f<-"https://raw.githubusercontent.com/difiore/ada-datasets/main/IMDB-movies.csv"
d<-read_csv(f, col_names = TRUE)

# Step 2
d <- d %>% 
  filter(startYear >= 1920 & startYear <=1979) %>%
  filter(runtimeMinutes >= 60 & runtimeMinutes <= 180) %>% 
  mutate(decade = case_when((startYear >= 1920 & startYear < 1930) ~ '20s',
                            (startYear >= 1930 & startYear < 1940) ~ '30s',
                            (startYear >= 1940 & startYear < 1950) ~ '40s',
                            (startYear >= 1950 & startYear < 1960) ~ '50s',
                            (startYear >= 1960 & startYear < 1970) ~ '60s',
                            (startYear >= 1970 & startYear < 1980) ~ '70s'))

# Step 3
ggplot(d, aes(x=runtimeMinutes)) +
  geom_histogram(color="darkblue", fill="lightblue") +
  facet_wrap(~decade) +
  labs(title="runtimeMinutes for each decade",x="decade", y = "count")+
  theme_minimal()

# Step 4
results <- d %>% 
  group_by(decade) %>% 
  summarise(pop_mean = mean(runtimeMinutes),
            pop_std = sqrt(sum((runtimeMinutes - mean(runtimeMinutes))**2) / n()),
            pop_SE = pop_std/sqrt(100))
results

# Step 5
results_sample <- d %>% 
  group_by(decade) %>% # 순서 변경??? no
  sample_n(100, replace = FALSE) %>% 
  summarise(sample_mean = mean(runtimeMinutes),
            sample_std = sd(runtimeMinutes),
            SE = sample_std/sqrt(100)) # Step 6
results_sample

# Step 7
#The sample means, standard deviations, and standard error are close to the population means, with minor differences, indicating that the sample represents the population well.
#Overall, the sample provides a reasonable estimate of the population values.

# Step 8
library(mosaic)

sampling_distribution <- do(1000)* (
  d %>% 
  group_by(decade) %>% 
  sample_n(100, replace = FALSE) %>% 
  summarise(sample_mean = mean(runtimeMinutes),
            sample_std = sd(runtimeMinutes))
  )

# Step 9
results_sampling_distribution <- sampling_distribution %>% 
  group_by(decade) %>% 
  summarise(mean = mean(sample_mean),
            std = sd(sample_mean))

ggplot(sampling_distribution, aes(x=sample_mean)) +
  geom_histogram(color="darkblue", fill="lightblue") +
  facet_wrap(~decade) +
  labs(title="sampling distribution of sample means by decade",x="decade")+
  theme_minimal()
#looks like normal distributions (bell-shaped and symmetric)


# Step 10
results
results_sample
results_sampling_distribution

# Population SE (pop_SE) is the most accurate theoretically but often unavailable in real studies.
# Single Sample SE (sample_SE) closely matches pop_SE, though it depends on the chosen sample.
# Sampling Distribution SE (std) represents the variability of sample means but tends to be smaller than pop_SE due to reduced variance in averages.

# In this case, sample_SE best approximates pop_SE.
# More sampling improves mean estimation but may underestimate SE.


###############

# Challenge 2

# Step 1
f<-"https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv"
z<-read_csv(f, col_names = TRUE)

# Step 2
pop_stdv <- function(x){
  s<-sqrt(sum((x-mean(x))**2)/length(x))
  return(s)
}

z %>% 
  summarise(across(c(height,weight,age,zombies_killed,years_of_education), 
                   list(pop_mean = mean, pop_sd = pop_stdv)))

# Step 3 
variables <- c("height", "weight", "age", "zombies_killed", "years_of_education")
for (var in variables){
  print(
    ggplot(z, aes(x = gender, y = .data[[var]], fill = gender)) +
    geom_boxplot()
  )
}

# Step 4
ggplot(z, aes(x = age, y = height, color = gender)) +
  geom_point()

ggplot(z, aes(x = age, y = weight, color = gender)) +
  geom_point()

# Step 5
variables <- c("height", "weight", "age", "zombies_killed", "years_of_education")
for (var in variables){
  print(
    ggplot(z, aes(x = .data[[var]])) +
      geom_histogram(fill = "skyblue", color = "black")
  )
}

for (var in variables){
  print(
  ggplot(z, aes(sample = .data[[var]])) +
    stat_qq() +
    stat_qq_line(color = "red")
  )
}

# Step 6
z_sample50 <- z %>% 
  sample_n(50, replace = FALSE)

se <- function(x){
  s_e<-sd(x)/sqrt(length(x))
  return(s_e)
}

z_sample50_stats <- z_sample50 %>% 
  summarise(across(c(height,weight,age,zombies_killed,years_of_education), 
                   list(sample_mean = mean, sample_sd = sd, sample_SE = se)))

# theoretical 95% confidence interval
z_sample50_stats$height_sample_mean + qnorm(c(0.025, 0.975)) * z_sample50_stats$height_sample_SE
z_sample50_stats$weight_sample_mean + qnorm(c(0.025, 0.975)) * z_sample50_stats$weight_sample_SE
z_sample50_stats$age_sample_mean + qnorm(c(0.025, 0.975)) * z_sample50_stats$age_sample_SE
z_sample50_stats$zombies_killed_sample_mean + qnorm(c(0.025, 0.975)) * z_sample50_stats$zombies_killed_sample_SE
z_sample50_stats$years_of_education_sample_mean + qnorm(c(0.025, 0.975)) * z_sample50_stats$years_of_education_sample_SE

# Step 7
library(mosaic)

z_sampling_distribution <- do(199)* (
  z %>% 
    sample_n(50, replace = FALSE) %>%
    summarise(across(c(height, weight, age, zombies_killed, years_of_education), mean))
)

z_sampling_distribution <- bind_rows(z_sample50 %>%
                            summarise(across(c(height, weight, age, zombies_killed, years_of_education), mean)), 
                            z_sampling_distribution)

# What are the means and standard deviations of the sampling distribution for each variable?
z_sampling_distribution_stats <- z_sampling_distribution %>% 
  select(c(height,weight,age,zombies_killed,years_of_education)) %>% 
  summarise(across(everything(), list(mean = mean, sd = sd)))

# How do the standard deviations of the sampling distribution for each variable compare to the standard errors estimated from your first sample of size 50?
z_sampling_distribution_stats
z_sample50_stats

# Step 8
#What do they look like? Are they normally distributed? What about for those variables that you concluded were not originally drawn from a normal distribution?
z_sampling_distribution %>%
  ggplot(aes(x = height)) +
  geom_histogram(fill = "skyblue", color = "black")

z_sampling_distribution %>%
  ggplot(aes(x = weight)) +
  geom_histogram(fill = "skyblue", color = "black")

z_sampling_distribution %>%
  ggplot(aes(x = age)) +
  geom_histogram(fill = "skyblue", color = "black")

z_sampling_distribution %>%
  ggplot(aes(x = zombies_killed)) +
  geom_histogram(fill = "skyblue", color = "black")

z_sampling_distribution %>%
  ggplot(aes(x = years_of_education)) +
  geom_histogram(fill = "skyblue", color = "black")

# Step 9
ci_results <- z_sampling_distribution %>%
  summarise(across(
    everything(),
    list(
      CI_lower = ~ quantile(., 0.025, na.rm = TRUE),
      CI_upper = ~ quantile(., 0.975, na.rm = TRUE)
    )
  ))
# How do the various 95% CIs you estimated compare to one another (i.e., the CI based on one sample and the corresponding sample standard deviation versus the CI based on simulation where you created a sampling distribution across 200 samples)?

# Step 10
library(mosaic)

n_boot <- 1000
n <- 50
boot <- do(n_boot) * summarise(
  sample_n(z, n, replace = TRUE),
  height_mean = mean(height),
  weight_mean = mean(weight),
  age_mean = mean(age),
  zombies_killed_mean = mean(zombies_killed),
  years_of_education_mean = mean(years_of_education)
)

boot_ci <- boot %>%
  summarise(across(everything(), list(
    CI_lower = ~ quantile(., 0.025, na.rm = TRUE),
    CI_upper = ~ quantile(., 0.975, na.rm = TRUE)
    )
  ))
