#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   4.3.1
#criado:     05.07.2023
#modificado: 08.01.2026

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
library("nycflights13")
library("tidyverse")

# a) Brainstorm at least 5 different summary statistics (center, location,
# spread) to describe the typical delay of flights to each destination. When is
# mean() useful? When is median() useful? When might you want to use something
# else? Should you use arrival delay or departure delay?

flights |>
  mutate(
    v = dep_delay                           #select dep_delay as variable in use
#   v = arr_delay                           #select arr_delay as variable in use
  ) |>
  filter(v > 0) |>                          #filter in delayed flights
  group_by(dest, origin) |>                 #group by "dest" and "origin"
  summarize(
    n = n(),
    #center
    mean = mean(v),
    median = median(v),
    #location
    min = min(v),
    max = max(v),
    Q25 = quantile(v, prob = 0.25),
    Q75 = quantile(v, prob = 0.75),
    #spread
    sd = sd(v),
    mad = mad(v, constant = 1/qnorm(3/4)),
    iqr = IQR(v)/(qnorm(3/4) - qnorm(1/4)),
    .groups = "drop"
  ) |>
  filter(n > 5) |>                          #filter for groups with obs > 5
  arrange(dest)                             #order by "dest"

# b) Which destination shows the greatest variation in air speed?

flights |>
  #group by "dest"
  group_by(dest) |>
  summarize(
    #calculate variation of air time per group
    var = var(air_time, na.rm = TRUE),
    #calculate number of non-NA observations per group
    n = sum(!is.na(air_time))
  ) |> 
  arrange(desc(var))

# c) The travel distance to airport "EGE" has no zero spread in the database. 
# Create a plot to further explore the adventures of EGE. Can you find any
# evidence that the airport moved locations?

#check value of distance across months
flights |>
  filter(dest == "EGE") |>
  ggplot(aes(x = month, y = distance, color = origin)) +
  geom_point(size = 2) +
  geom_jitter(width = 0.2, height = 0.2, shape = 1, size = 1) + 
  scale_x_continuous(breaks = 1:12)

}
# 2. VARIATION
{
library("tidyverse")
library("skimr")

## 2.1. TYPICAL VALUES

# a) Explore the distribution of each of the x, y, and z variables in diamonds.
# What do you learn? Think about a diamond and how you might decide which
# dimension is the length, width, and depth.

#calculate summary statistics
diamonds |>
  select(x, y, z) |>
  skim()

#visualize distributions
diamonds |>  
  ggplot(mapping = aes(x = x)) +
    geom_histogram(binwidth = 0.1)
diamonds |>  
  ggplot(mapping = aes(x = y)) +
    geom_histogram(binwidth = 0.1)
diamonds |>  
  ggplot(mapping = aes(x = z)) +
    geom_histogram(binwidth = 0.1)

# Conclusions 1
# 1. x and y are larger than z,
# 2. there are outliers,
# 3. they are all right skewed,
# 4. they are multimodal or “spiky”.

#look at 0 values
diamonds |>
  filter(x == 0 | y == 0 | z == 0)     #filter observations with value 0

# Conclusions 2
# 1. Zero is missing values?

#look at highest values of "y" and "z"
diamonds |>
  arrange(desc(y)) |>
  slice_head(n = 6)
diamonds |>
  arrange(desc(z)) |>
  slice_head(n = 6)

# Conclusions 3
# 1. Highest values are data errors?

#visualize joint distributions
ggplot(diamonds, aes(x = x, y = y)) +
  geom_point()
ggplot(diamonds, aes(x = x, y = z)) +
  geom_point()
ggplot(diamonds, aes(x = y, y = z)) +
  geom_point()

# Conclusions 4
# 1. Unusual values are clearly outliers

#visualize distributions without unusual observations
diamonds |>
  filter(x > 0, x < 10) |>             #filter out unusual observations
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous(breaks = 1:10)
diamonds |>
  filter(y > 0, y < 10) |>             #filter out unusual observations
  ggplot(aes(x = y)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous(breaks = 1:10)
diamonds |>
  filter(z > 0, z < 10) |>             #filter out unusual observations
  ggplot(aes(x = z)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous(breaks = 1:10)

# b) Explore the distribution of price. Do you discover anything unusual or
# surprising? (Hint: Carefully think about the binwidth and make sure you try a
# wide range of values.)

#visualize distribution of "price" with sensible binwidth value
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 100)

#visualize detailed distribution of "price" for values smaller than 2500
diamonds |>
  filter(price < 2500) |>
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 10)

# Conclusions 1:
# 1. No diamonds with price $1,500
# 2. Bulge in distribution around $750.

#check if there is any rounding effect when setting up "price"
diamonds |>
  mutate(ending = price %% 10) |>      #calculate ending algorism of "price"
  ggplot(aes(x = ending)) +
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = 0:9)

# Conclusions 2:
# 1. No rounding to 5 or 0

# c) How many diamonds are 0.99 carat? How many are 1 carat? What do you think
# is the cause of the difference? 

#count number of observations for 0.99 and 1.00
diamonds |>
  filter(carat %in% c(0.99, 1.00)) |>
  count(carat)

#check if there is any rounding effect when setting up "carat"
diamonds |>
  mutate(ending = carat %% 10) |>      #calculate ending algorism of "carat"
  ggplot(aes(x = ending)) +
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = 0:9)

# Conclusions:
# 1. Rounding up to 1.00?

# d) Compare and contrast coord_cartesian() vs xlim() or ylim() when zooming in
# on a histogram. What happens if you leave binwidth unset? What happens if you
# try and zoom so only half a bar shows?

#zooming using coord_cartesian()
ggplot(diamonds, aes(x = price)) +
  geom_histogram() +
  coord_cartesian(xlim = c(100, 5000), ylim = c(0, 3000))

#zooming using xlim() and ylim()
ggplot(diamonds, aes(x = price)) +
  geom_histogram() +
  xlim(100, 5000) + ylim(0, 3000)

## 2.2. UNUSUAL VALUES [No exercises]

}
# 3. MISSING VALUES
{
library("nycflights13")
library("tidyverse")

## 3.1. EXPLICIT MISSING VALUES

# a) What happens to missing values in a histogram? What happens to missing
# values in a bar chart? Why is there a difference?

#missing values in a histogram
diamonds |>
  mutate(y = if_else(y < 3 | y > 20, NA, y)) |>
  ggplot(aes(x = y)) +
  geom_histogram()

#missing values in a bar plot
diamonds |>
  mutate(cut = if_else(y < 3 | y > 20, NA, cut)) |>
  ggplot(aes(x = cut)) +
  geom_bar()

# b) What does na.rm = TRUE do in mean() and sum()?

#setting up na.rm = TRUE
diamonds |>
  mutate(carat = if_else(y < 3 | y > 20, NA, carat)) |>
  summarize(
    average_with_NA = mean(carat),
    average_without_NA = mean(carat, na.rm = TRUE)
  )
  
## 3.2. IMPLICIT MISSING VALUES

# a) Can you find any relationship between the carrier and the rows that appear
# to be missing from planes?

#check planes missing from database "planes"
flights |> 
  distinct(tailnum) |>
  anti_join(planes)

#look at planes missing from database "planes" per carrier
flights |> 
  #calculate if tailnum is absent
  mutate(absent = !tailnum %in% planes$tailnum) |>
  #group by "carrier"
  group_by(carrier) |>
  summarize(
    n_absent = sum(absent),       #calculate number of "absent"
    n_total = n(),                #calculate number of observations
    p_absent = mean(absent)       #calculate probability of "absent"
  ) |>
  filter(n_absent > 0) |>
  arrange(desc(p_absent))
  
## 3.3. FACTORS AND EMPTY GROUPS [No exercises]

}
# 4. COVARIATION
{
library("nycflights13")
library("lvplot")
library("ggbeeswarm")
library("gridExtra")
library("tidyverse")
library("janitor")

## 4.1. CATEGORICAL AND NUMERICAL VARIABLES

# a) Use what you’ve learned to improve the visualization of the departure times
# of cancelled vs. non-cancelled flights.  

#prepare data
flights2 <- flights |>
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  )  

#using boxplot
flights2 |>
  ggplot(aes(y = sched_dep_time, x = cancelled)) +
  geom_boxplot()

#using density lines
flights2 |>
  ggplot(aes(x = sched_dep_time, color = cancelled)) +
  geom_density()

# b) What variable in the diamonds dataset is most important for predicting the
# price of a diamond? How is that variable correlated with cut? Why does the
# combination of those two relationships lead to lower quality diamonds being
# more expensive?

#look at "price" vs. "carat": price ~ carat 
ggplot(diamonds, aes(x = carat, y = price, group = cut_width(carat, 0.1))) +
  geom_boxplot()

# Conclusions 1:
# 1. "carat" is strongly related to "price"

#look at "carat" vs. "cut": carat ~ cut  
ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_boxplot()

# Conclusions 2:
# 1. "carat" is negatively related to "cut"

# c) One problem with boxplots is that they were developed in an era of much
# smaller datasets and tend to display a prohibitively large number of “outlying
# values”. One approach to remedy this problem is the letter value plot. Install
# the lvplot package, and try using geom_lv() to display the distribution of
# price vs. cut. What do you learn? How do you interpret the plots?

#using simple boxplot
ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_boxplot()

#using boxplot with jittered outliers
diamonds |>
  group_by(cut) |>
  mutate(
    carat_out_lower = carat < quantile(carat,0.25) - IQR(carat) * 2.0,
    carat_out_upper = carat > quantile(carat,0.75) + IQR(carat) * 2.0,
    carat_out = if_else(carat_out_lower | carat_out_upper, carat, NA)
  ) |>
  ggplot(aes(x = cut, y = carat)) +
  geom_boxplot(outlier.shape = NA, coef = 2.0) +
  geom_jitter(aes(y = carat_out),
    height = 0,
    width = 0.1,
    shape = 1,
    size = 1.5,
    alpha = 0.5
  )

#using letter-value plopt
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_lv()
  
# d) Create a visualization of diamond prices vs. a categorical variable from
# the diamonds dataset using geom_violin(), then a faceted geom_histogram(),
# then a colored geom_freqpoly(), and then a colored geom_density(). Compare and
# contrast the four plots. What are the pros and cons of each method of
# visualizing the distribution of a numerical variable based on the levels of a
# categorical variable?

#geom_freqpoly()
gg1 <- ggplot(diamonds, aes(x = price, y = after_stat(density), color = cut)) +
  geom_freqpoly(binwidth = 500)

#geom_histogram() with facet_wrap()
gg2 <- ggplot(diamonds, aes(x = price)) +
  geom_histogram() +
  facet_wrap(~cut, ncol = 1, scales = "free_y")

#geom_violin()
gg3 <- ggplot(diamonds, aes(x = price, y = cut)) +
  geom_violin() +
  scale_y_discrete(limits=rev)

#geom_density()
gg4 <- ggplot(diamonds, aes(x = price, color = cut)) +
  geom_density(bw = 500)

grid.arrange(gg1, gg4, gg2, gg3, ncol = 2)

# e) If you have a small dataset, it’s sometimes useful to use geom_jitter() to
# avoid overplotting to more easily see the relationship between a continuous
# and categorical variable. The ggbeeswarm package provides a number of methods
# similar to geom_jitter(). List them and briefly describe what each one does.

#get a small dataset
set.seed(123)
diamonds2 <- diamonds |> slice_sample(n = 250)

#geom_point()
gg1 <- ggplot(diamonds2, aes(x = cut, y = price)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = "point")

#geom_jitter()
gg2 <- ggplot(diamonds2, aes(x = cut, y = price)) +
  geom_jitter(height = 0, width = 0.2) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = "jitter")

#geom_quasirandom() with quasirandom()
gg3 <- ggplot(diamonds2, aes(x = cut, y = price)) +
  geom_quasirandom() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = "quasirandom")

#geom_beeswarm()
gg4 <- ggplot(diamonds2, aes(x = cut, y = price)) +
  geom_beeswarm() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = "beeswarm")

grid.arrange(gg1, gg2, gg3, gg4, ncol = 2)

## 4.2. TWO CATEGORICAL VARIABLES

# a) How could you rescale the count dataset above to more clearly show the
# distribution of cut within color, or color within cut?

#"cut (%)" within each category of "color"
diamonds |>
  count(color, cut) |>
  group_by(color) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = color, y = cut, fill = prop)) +
  geom_tile()

#"color (%)" within each category of "cut"
diamonds |>
  count(color, cut) |>
  group_by(cut) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = cut, y = color, fill = prop)) +
  geom_tile()

# b) What different data insights do you get with a segmented bar chart if color
# is mapped to the x aesthetic and cut is mapped to the fill aesthetic?
# Calculate the counts that fall into each of the segments.

#geom_bar()
ggplot(diamonds, aes(x = color, fill = cut)) +
  geom_bar()

#geom_bar() with position = "fill"
ggplot(diamonds, aes(x = color, fill = cut)) +
  geom_bar(position = "fill")

#table using count() and pivot_wider()
diamonds |> 
  count(color, cut) |>
  pivot_wider(
    names_from = color,
    values_from = n
  )

#table using tabyl
diamonds |>
  tabyl(cut, color)

# c) Use geom_tile() together with dplyr to explore how average flight delays
# vary by destination and month of year. What makes the plot difficult to read?
# How could you improve it?

#simple plot
flights |>
  group_by(month, dest) |>
  summarise(
    dep_delay = mean(dep_delay, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile() +
  labs(x = "Month", y = "Destination", fill = "Departure Delay") + 
  theme(axis.text = element_text(size=6))

# Problems:
# 1. too many rows
# 2. rows with absent values
# 3. order of rows not meaningful

#updated plot  
flights |>
  #calculate mean(dep_delay) only for cases with at least 5 observations
  group_by(month, dest) |>
  summarise(
    dep_delay = if_else(n() > 5, mean(dep_delay, na.rm = TRUE), NA),
    .groups = "drop"
  ) |>
  #complete cases without observations
  complete(month, dest) |>
  #filter out "dest" with absent values
  group_by(dest) |>
  mutate(dest_with_na = any(is.na(dep_delay))) |>
  ungroup() |>
  filter(!dest_with_na) |>
  #order "dest" by mean(dep_delay) across months
  mutate(dest = fct_reorder(dest, dep_delay, mean)) |>
  #create plot
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile() +
  labs(x = "Month", y = "Destination", fill = "Departure Delay")  +
  theme(axis.text = element_text(size=8))

## 4.3. TWO NUMERICAL VARIABLES

# a) Instead of summarizing the conditional distribution with a box plot, you
# could use a frequency polygon. What do you need to consider when using
# cut_width() vs cut_number()? How does that impact a visualization of the 2d
# distribution of carat and price?

#geom_freqpoly() with cut_number()
ggplot(diamonds, aes(color = cut_number(carat, 5), x = price)) +
  geom_freqpoly() +
  labs(x = "Price", y = "Count", color = "Carat")
  
#geom_freqpoly() with cut_width()
ggplot(diamonds, aes(color = cut_width(carat, 1, boundary = 0), x = price)) +
  geom_freqpoly() +
  labs(x = "Price", y = "Count", color = "Carat")

#geom_freqpoly() with cut_width() using after_stat(density)
ggplot(diamonds, aes(color = cut_width(carat, 1, boundary = 0), x = price)) +
  geom_freqpoly(aes(y = after_stat(density))) +
  labs(x = "Price", y = "Count", color = "Carat")

# b) Visualize the distribution of carat, partitioned by price.

#geom_boxplot() with cut_number()
ggplot(diamonds, aes(x = carat, y = cut_number(price, 10))) +
  geom_boxplot() +
  labs(y = "Price")

#geom_boxplot() with cut_width()
ggplot(diamonds, aes(x = carat, y = cut_width(price, 2000, boundary = 0))) +
  geom_boxplot(varwidth = TRUE) +
  labs(y = "Price")
  
# c) How does the price distribution of very large diamonds compare to small
# diamonds. Is it as you expect, or does it surprise you?

#distribution of "price" partitioned by "carat"
ggplot(diamonds, aes(x = price, y = cut_number(carat, 5))) +
  geom_boxplot() +
  labs(y = "Carat")

# Conclusions:
# 1. Large diamonds have a more spread distribution of price than smaller ones

# d) Combine two of the techniques you’ve learned to visualize the combined
# distribution of cut, carat, and price.

#geom_hex() and facet_wrap()
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_hex() +
  facet_wrap(~cut, ncol = 1)

#geom_boxplot() and colour
ggplot(diamonds, aes(x = cut, y = price, colour = cut_number(carat, 5))) +
  geom_boxplot() +
  labs(colour = "Carat")

# e) Two dimensional plots reveal outliers that are not visible in one
# dimensional plots. For example, some points in the plot below have an unusual
# combination of x and y values, which makes the points outliers even though
# their x and y values appear normal when examined separately. Why is a
# scatterplot a better display than a binned plot for this case?
#   diamonds |> 
#     filter(x > 0, between(y, 3, 20)) |> 
#     ggplot(aes(x = x, y = y)) +
#     geom_point()

#joint distribution using geom_bin2d()
diamonds |> 
  filter(x > 0, between(y, 3, 20)) |> 
  ggplot(aes(x = x, y = y)) +
  geom_bin2d()

}
# 5. PATTERNS AND MODELS
{
library("tidymodels")
library("tidyverse")

# a) Obtain a sample with 20 observation from the dataset diamonds. Make the
# appropriate data transformation and fit a linear model between the price and 
# the weight of diamonds. Plot the observations, the fitted value and the
# residuals. Take a look at geom_segment() for plotting the residuals.

#obtain a sample
set.seed(123)
diamonds2 <- diamonds |>
  mutate(
    log_price = log(price),
    log_carat = log(carat)
  ) |>
  slice_sample(n = 20)

#fit a model
diamonds_fit <- linear_reg() |>
  set_engine("lm") |>
  fit(log_price ~ log_carat, data = diamonds2)

#add residuals and predictors
augment(diamonds_fit, new_data = diamonds2) |>
  ggplot(aes(x = log_carat, y = log_price)) +
  geom_point() +
  stat_smooth(method = "lm", formula = "y ~ x", se = FALSE) +
  geom_segment(aes(xend = log_carat, yend = .pred), color = "red")

}
