#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   4.3.1
#criado:     17.07.2023
#modificado: 07.01.2026

# 0. INDEX
{
# 1. SUMMARY STATISTICS
# 2. VARIATION
# 2.1. TYPICAL VALUES
# 2.2. UNUSUAL VALUES
# 3. MISSING VALUES
# 3.1. EXPLICIT MISSING VALUES
# 3.2. IMPLICIT MISSING VALUES
# 3.3. FACTORS AND EMPTY GROUPS
# 4. COVARIATION
# 4.1. CATEGORICAL AND NUMERICAL VARIABLES
# 4.2. TWO CATEGORICAL VARIABLES
# 4.3. TWO NUMERICAL VARIABLES
# 5. PATTERNS AND MODELS

}
# 1. SUMMARY STATISTICS
{
#from https://r4ds.hadley.nz/numbers.html#numeric-summaries
library("nycflights13") #collection of datasets
library("tidyverse")    #collection of packages for data analysis
  
### Central tendency
flights |>
  group_by(year, month, day) |>
  summarize(
    mean = mean(dep_delay, na.rm = TRUE),
    median = median(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = mean, y = median)) + 
  geom_abline(slope = 1, intercept = 0, color = "white", linewidth = 2) +
  geom_point()

### Minimum, maximum and quantiles
flights |>
  group_by(year, month, day) |>
  summarize(
    q95 = quantile(air_time, 0.95, na.rm = TRUE),
    max = max(air_time, na.rm = TRUE),
    .groups = "drop"
  )

### Spread
flights |> 
  group_by(origin, dest) |> 
  summarize(
    #sd = sqrt(sum(X - mean(X)^2)/(n - 1))
    sd = sd(air_time, na.rm=TRUE),
    #mad = median(abs(X - median(X)))* 1/qnorm(3/4)
    mad = mad(air_time, constant = 1/qnorm(3/4),na.rm=TRUE),
    #iqr = (quantile(X, 3/4) - quantile(X, 1/4))*1/(qnorm(3/4) - qnorm(1/4))
    iqr = IQR(air_time, na.rm=TRUE)/(qnorm(3/4) - qnorm(1/4)), 
    n = n(),
    .groups = "drop"
  )

### Positions
flights |> 
  group_by(year, month, day) |> 
  mutate(r = row_number(sched_dep_time)) |> 
  filter(r %in% c(1, max(r)))  

### Distributions
flights |>
  filter(dep_delay < 120) |> 
  ggplot(aes(x = dep_delay, group = interaction(day, month))) + 
  geom_freqpoly(binwidth = 5, alpha = 1/5)

}
# 2. VARIATION
{
#from https://r4ds.hadley.nz/EDA.html#variation
library("nycflights13") #collection of datasets
library("skimr")        #function skim() for descriptive statistics
library("tidyverse")    #collection of packages for data analysis

#metadata
?diamonds

#data inspection
glimpse(diamonds)

#descriptive statistics
skim(diamonds)

## 2.1. TYPICAL VALUES

#visualize distribution of "carat"
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.5)

#visualize distribution of "carat" in detail
diamonds |> 
  filter(carat < 3) |>                 #filter in smaller diamonds
  ggplot(aes(x = carat)) +
  geom_histogram(binwidth = 0.01) +
  geom_vline(xintercept=c(seq(0,3,0.5)),linetype = "dashed",color="red")

#visualize distribution of "y"
ggplot(diamonds, aes(x = y)) + 
  geom_histogram(binwidth = 0.5)
  
#visualize distribution of "y" in detail
ggplot(diamonds, aes(x = y)) + 
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

## 2.2. UNUSUAL VALUES

#look at the data "price", "x", "y" and "z"
diamonds |> 
  filter(between(y, 3, 20)) |>         #filter out unusual values
  select(price, x, y, z) |>
  arrange(y)

#drop entire row of "unusual" data
diamonds2 <- diamonds |> 
  filter(between(y, 3, 20))

#replacing "unusual" data with missing values
diamonds2 <- diamonds |> 
  mutate(y = if_else(y < 3 | y > 20, NA, y))
diamonds2 |> filter(is.na(y)) |> select(price, x, y, z)

#look at relation between "x" and "y"
ggplot(diamonds2, aes(x = x, y = y)) + 
  geom_point()

#look at relation between "x" and "y" (turn off warning)
ggplot(diamonds2, aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)

#meaningless missing values
diamonds |>
  filter(!between(y, 3, 20))

#meaningful missing values
flights |>
  #group flights per hour of schedule departure
  group_by(hour = sched_dep_time %/% 100) |>
  summarize(
    #calculate proportion of NAs in dep_time
    prop_cancelled = mean(is.na(dep_time)),
    #calculate total number of flights
    n = n()) |>
  #filter-out flights schedule for before 1:00
  filter(hour > 1) |>
  #select variables hour and prop_cancelled
  ggplot(aes(x = hour, y = prop_cancelled)) +
  #create lineplot of variables
  geom_line(color = "grey50") +
  #create scatterpoints of variables with size = n
  geom_point(aes(size = n))

}
# 3. MISSING VALUES
{
#from https://r4ds.hadley.nz/missing-values.html
library("nycflights13") #collection of datasets
library("tidyverse")    #collection of packages for data analysis

## 3.1. EXPLICIT MISSING VALUES

treatment <- tribble(
  ~person,           ~treatment, ~response,
  "Derrick Whitmore", 1,         7,
  NA,                 2,         10,
  NA,                 3,         NA,
  "Katherine Burke",  1,         4
)

### Last observation carried forward
treatment |>
  fill(everything())

### Fixed values
treatment |>
  mutate(response = coalesce(response, 0)) |>
  fill(everything())

### Numeric value represents NA
csv <- "
person,          treatment, response
Derrick Whitmore,        1,        7
Derrick Whitmore,        2,       10
Derrick Whitmore,        3,       99
Katherine Burke,         1,        4"

#deal with NAs when reading data
read_csv(csv, na = "99")

#deal with NAs after reading data
read_csv(csv) |>
  mutate(response = na_if(response, 99))

### NaN
v <- c(NA, NaN)

#operation with NaNs (mathematical operations)
v * 10
#operation with NaNs (logical operations)
v == 1
#operation with NaNs (is.na)
is.na(v)
#operation with NaNs (is.nan)
is.nan(v)

#undefined operations in arithmetic 1
0 / 0 
#undefined operations in arithmetic 2
sqrt(-1)
#undefined operations in arithmetic 3
log(-1)
#undefined operations with infinity 1
0 * Inf
#undefined operations with infinity 2
Inf - Inf
#undefined operations in trignometry
asin(2)
## 3.2. IMPLICIT MISSING VALUES

stocks <- tibble(
  year  = c(2020, 2020, 2020, 2020, 2021, 2021, 2021),
  qtr   = c(   1,    2,    3,    4,    2,    3,    4),
  price = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
#1. Price in 2020 Q4 is explicitly missing
#2. Price in 2021 Q1 is implicitly missing

### Pivoting
#create columns for each quarter
stocks |>
  pivot_wider(
    names_from = qtr, 
    values_from = price
  )

#create columns for each quarter and transpose them to rows
stocks |>
  pivot_wider(
    names_from = qtr, 
    values_from = price
  ) |>
  pivot_longer(
    cols = `1`:`4`,
    names_to = "qtr",
    values_to = "price"
  )

### Complete
#complete present cases
stocks |>
  complete(year, qtr)

#complete present and specified cases
stocks |>
  complete(year = 2019:2021, qtr)

### Joins
#obtain airport destinations not in database "airports"
flights |> 
  distinct(faa = dest) |>              #calculate the distinct "dest"
  anti_join(airports)                  #obtain absent "dest"

#obtain planes not in database "planes"
flights |> 
  distinct(tailnum) |>                 #calculate the distinct "dest"
  anti_join(planes)                    #obtain absent "dest"

## 3.3. FACTORS AND EMPTY GROUPS

health <- tibble(
  name   = c("Ikaia", "Oletta", "Leriah", "Dashay", "Tresaun"),
  smoker = factor(c("no", "no", "no", "no", "no"), levels = c("yes", "no")),
  age    = c(34, 88, 75, 47, 56),
)

#empty groups with count
health |> count(smoker)
health |> count(smoker, .drop = FALSE)

#empty groups with ggplot
ggplot(health, aes(x = smoker)) +
  geom_bar() +
  scale_x_discrete()
ggplot(health, aes(x = smoker)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

#empty groups with group_by()
health |> 
  group_by(smoker, .drop = FALSE) |> 
  summarize(
    n = n(),
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  )

#empty groups with complete()
health |> 
  group_by(smoker) |> 
  summarize(
    n = n(),
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  ) |> 
  complete(smoker)

}
# 4. COVARIATION
{
#from https://r4ds.hadley.nz/EDA.html#covariation
library("hexbin")       #function geom_hex() for data visualization
library("tidyverse")    #collection of packages for data analysis
library("janitor")      #simple tools for examining and cleaning dirty data
## 4.1. CATEGORICAL AND NUMERICAL VARIABLES

#use geom_freqpoly()
ggplot(diamonds, aes(x = price, color = cut)) + 
  geom_freqpoly(binwidth = 500, linewidth = 0.75)

#use geom_freqpoly() with densities
ggplot(diamonds, aes(x = price, y = after_stat(density), color = cut)) + 
  geom_freqpoly(binwidth = 500, linewidth = 0.75)  

#use geom_density()
ggplot(diamonds, aes(x = price, color = cut)) + 
  geom_density(bw = 500, linewidth = 0.75)  

#use geom_boxplot()
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()

## 4.2. TWO CATEGORICAL VARIABLES

#table with count() and pivot_wider()
diamonds |> count(color, cut) |>
  pivot_wider(
    names_from = "cut",
    values_from = "n"
  )

#table with tabyl()
diamonds |> tabyl(color, cut)

#table with tabyl() and adorn_*
diamonds |> tabyl(color, cut) |>
  #add totals
  adorn_totals("col") %>%
  #turn into percentages by column
  adorn_percentages("col") %>% 
  #format percentages
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  #add back counts
  adorn_ns(position = "front")

#geom_count()
ggplot(diamonds, aes(x = cut, y = color)) +
  geom_count()

#geom_tile()  
diamonds |> 
  count(color, cut) |>
  ggplot(aes(x = cut, y = color)) +
  geom_tile(aes(fill = n))

#geom_bar()
ggplot(diamonds, aes(x = cut, fill = color)) +
  geom_bar()

## 4.3. TWO NUMERICAL VARIABLES

smaller <- diamonds |> 
  filter(carat < 3)                    #filter in smaller diamonds

#geom_point()
ggplot(smaller, aes(x = carat, y = price)) +
  geom_point()

#geom_point() with alpha
ggplot(smaller, aes(x = carat, y = price)) + 
  geom_point(alpha = 0.01)

#geom_bin2d()
ggplot(smaller, aes(x = carat, y = price)) +
  geom_bin2d()

#geom_hex()
ggplot(smaller, aes(x = carat, y = price)) +
  geom_hex()

#geom_boxplot()
ggplot(smaller, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.1)))
  
}
# 5. PATTERNS AND MODELS
{
#from https://r4ds.hadley.nz/EDA.html#patterns-and-models
library("tidymodels") #collection of packages for data modelling
library("tidyverse")  #collection of packages for data analysis

#create data set
set.seed(123)
diamonds2 <- diamonds |>
  #log-transform variables
  mutate(
    log_price = log(price),
    log_carat = log(carat)
  ) |>
  #take sample of 1000 observations
  slice_sample(n = 1000)

#look at relation log(price) ~ log(carat)
diamonds2 |>
  ggplot(aes(x = log_carat, y = log_price)) +
  geom_point()

#functions linear_reg(), set_engine() and fit()
diamonds_fit <- linear_reg() |>
  set_engine("lm") |>
  fit(formula = log_price ~ log_carat, data = diamonds2)

#function tidy()  
tidy(diamonds_fit)

#function glance()
glance(diamonds_fit)

#function augment()
diamonds_aug <- augment(diamonds_fit, new_data = diamonds2) |>
  mutate(.resid = exp(.resid))

#unexplained variation in "price" using "carat"
ggplot(diamonds_aug, aes(x = carat, y = .resid)) + 
  geom_point()

#modelling unexplained variation in "price" using "cut"
ggplot(diamonds_aug, aes(x = cut, y = .resid)) + 
  geom_boxplot()

}
