#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   4.3.1
#criado:     05.01.2023
#modificado: 08.04.2026

# 0. INDEX
{
# 1. NUMERIC VECTORS
# 1.1. COUNTS
# 1.2. NUMERIC TRANSFORMATION
# 1.3. GENERAL TRANSFORMATION
# 1.4 SUMMARY STATISTICS
# 2. FACTORS
# 2.1. BASICS
# 2.2. DATASET gss_cat
# 2.3. MODIFYING FACTOR ORDER
# 2.4. MODIFYING FACTOR LEVEL
# 3. LOGICAL VECTORS
# 3.1. COMPARISONS
# 3.2. BOOLEAN ALGEBRA
# 3.3. SUMMARIES
# 3.4. CONDITIONAL TRANSFORMATION

}
# 1. NUMERIC VECTORS
{
library("nycflights13")
library("tidyverse")

## 1.1. COUNTS

# a) How can you use count() to count the number of rows with a missing value
# for a given variable?

#number of missing values for a given variable
flights |> mutate(is_miss = is.na(dep_time)) |> count(is_miss)

#number of missing values for at least one variable
flights |> mutate(is_miss = if_any(everything(), ~ is.na(.))) |> count(is_miss)

#number of missing values for each variable
flights |>
  select(where(~ any(is.na(.)))) |>
  summarise(across(everything(), ~ sum(is.na(.))))

# b) Expand the following calls to count() to instead use group_by(),
# summarize(), and arrange():
#    1. flights |> count(dest, sort = TRUE)
flights |> 
  group_by(dest) |>
  summarize(n = n()) |>
  arrange(desc(n))

#    2. flights |> count(tailnum, wt = distance)
flights |> 
  group_by(tailnum) |> 
  summarize(total_dist = sum(distance))

## 1.2. NUMERIC TRANSFORMATION

# a) Explain in words what each line of the following code does:
# flights |> 
#   group_by(hour = sched_dep_time %/% 100) |> 
#   summarize(prop_cancelled = mean(is.na(dep_time)), n = n()) |> 
#   filter(hour > 1) |> 
#   ggplot(aes(x = hour, y = prop_cancelled)) +
#   geom_line(color = "grey50") + 
#   geom_point(aes(size = n))
#use dataset flights
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

# b) Currently dep_time is convenient to look at, but hard to compute with
# because it’s not really a continuous numbers. You can see the basic problem by
# running the code below: there’s a gap between each hour.
# flights |> 
#   filter(month == 1, day == 1, !is.na(dep_delay)) |> 
#   ggplot(aes(x = dep_time, y = dep_delay)) +
#   geom_point()
# Convert them to a more truthful representation of time (either fractional
# hours or minutes since midnight).
flights |>
  mutate(
    hour = dep_time %/% 100,  #extract hours of schedule departure
    minute = dep_time %% 100, #extract minutes of schedule departure
#   time1 = hour*60 + minute  #minutes since midnight
    time1 = hour + minute/60  #fractional hours since midnight
  ) |>
  filter(month == 1, day == 1, !is.na(dep_delay)) |>
  ggplot(aes(x = time1, y = dep_delay)) +
  geom_point() + 
# labs(x = "minutes since midnight")
  labs(x = "fractional hours since midnight")

# c) Round dep_time to the nearest five minutes

#naive approach
flights |>
  mutate(
    dep_time2 = round(dep_time/5)*5,
    .keep = "used"
  )

#correct rounding to 60 minutes
flights |>
  mutate(
    #extract hours of departure
    dep_time_hh = dep_time %/% 100,
    #extract minutes of departure in 5min interval
    dep_time_mm = round(dep_time %% 100 / 5) * 5,
    #compute departure time using 5min interval
    dep_time2 = if_else(
      dep_time_mm <= 55,
      dep_time_hh * 100 + dep_time_mm, #rounded minutes less than 60
      (dep_time_hh + 1) * 100          #rounded minutes equal to 60
    ),
    .keep = "used"
  )

## 1.3. GENERAL TRANSFORMATION

# a) Find the 12 most delayed flights (in departure) using a ranking function.
# How do you want to handle ties? Carefully read the documentation for 
# row_number().
?row_number
?base::rank

#using row_number()
flights |>
  mutate(ranks = row_number(desc(dep_delay))) |>
  select(tailnum, dep_delay, ranks) |>
  arrange(ranks) |>
  filter(ranks <= 12)

#using min_rank()
flights |>
  mutate(ranks = min_rank(desc(dep_delay))) |>
  select(tailnum, dep_delay, ranks) |>
  arrange(ranks) |>
  filter(ranks <= 12)

# b) Which plane (tailnum) has the worst on-time record? Use dep_delay.

#only select the worst on-time record overall
flights |>
  filter(!is.na(dep_delay)) |>           #filter out NA values
  filter(dep_delay == max(dep_delay)) |> #filter in maximum value
  select(tailnum, dep_delay)             #select variables tailnum and dep_delay

#calculate worst on-time record for each plane
flights |>
  filter(!is.na(dep_delay)) |>     #filter out NA values
  group_by(tailnum) |>             #group by "tailnum"
  summarize(
    dep_delay_rec = max(dep_delay) #calculate maximum delay per group
  ) |>
  arrange(desc(dep_delay_rec))

# c) At which hour should you fly if you want to avoid more than one hour delays
# in departure as much as possible?
flights |>
  filter(!is.na(dep_delay)) |>
  group_by(hour) |>
  summarise(
    n_delay = sum(dep_delay > 60),             #n_delay; number of delays
    p_delay = mean(dep_delay > 60),            #p_delay: proportion of delays
    m_delay = mean(dep_delay[dep_delay > 0]),  #m_delay; mean delay
    t_delay = sum(dep_delay[dep_delay > 0])    #t_delay: total minutes delayed
  )

# d) What does flights |> group_by(dest) |> filter(row_number() < 4) do? What
# does flights |> group_by(dest) |> filter(row_number(desc(dep_delay)) < 4) do?

#flights |> group_by(dest) |> filter(row_number() < 4)
flights |>
  #group by "dest"
  group_by(dest) |>
  #filter for the first 3 rows per group
  filter(row_number() < 4) |>
  select(dest, year:dep_time) |> arrange(dest)

#flights |> group_by(dest) |> filter(row_number(desc(dep_delay)) < 4)
flights |>
  #group by "dest"
  group_by(dest) |>
  #filter for the 3 smallest departure delays per group
  filter(row_number(desc(dep_delay)) < 4) |>
  select(dest, year:dep_delay) |> arrange(dest, desc(dep_delay))

# e) For each destination, compute the total minutes of delay. For each flight 
# (from a given carrier, origin and destination), compute the proportion of the
# total delay for its destination. Consider only positive departure delays.

#total minutes of delay for each destination
flights |>
  #filter out early departures and canceled flights
  filter(dep_delay > 0) |>
  #group by "dest"
  group_by(dest) |>
  #sum total delay per group
  summarize(total_delay = sum(dep_delay))

#proportion of total delay per flight for each destination
flights |>
  #filter out early departures and canceled flights
  filter(dep_delay > 0) |>
  #group by "dest", "origin", "carrier" and flight"
  group_by(dest, origin, carrier, flight) |>
  #sum up delay time per group
  summarize(total_delay = sum(dep_delay), .groups = "drop") |>
  #group by "dest"
  group_by(dest) |>
  #calculate proportion of delay per group
  mutate(p_delay = total_delay/sum(total_delay)) |>
  #ungroup
  ungroup() |>
  #arrange by "dest", "origin", "carrier" and "p_delay" by descending order
  arrange(dest, origin, carrier, desc(p_delay))

# f) Delays are typically temporally correlated: even once the problem that
# caused the initial delay has been resolved, later flights are delayed to allow
# earlier flights to leave. Using lag(), explore how the average flight delay
# for an hour is related to the average delay for the previous hour (e.g. plot,
# correlation). Use the following data set:
# flight_ymdh <-  flights |> 
#   filter(!is.na(dep_delay)) |>
#   mutate(hour = dep_time %/% 100) |> 
#   group_by(year, month, day, hour) |> 
#   summarize(
#     n = n(),
#     mean_dep_delay = mean(dep_delay),
#     .groups = "drop"
#   )
# Consider only average delays with at least 5 observations.
flight_ymdh <- flights |>
  #filter out cancelled flights
  filter(!is.na(dep_delay)) |>
  #extract hour of departure
  mutate(hour = dep_time %/% 100) |>
  #group by "year", "month", "day" and "hour"
  group_by(year, month, day, hour) |>
  summarize(
    #calculate number of observations per group
    n = n(),
    #calculate average delay per group
    mean_dep_delay = mean(dep_delay),
    .groups = "drop"
  )

flight_dep_lag <- flight_ymdh |>  
  #make sure data is order by time
  arrange(year, month, day, hour) |>
  #group by "year", "month" and "day"
  group_by(year, month, day) |>
  #create variable of previous average delay
  mutate(
    n_lag = lag(n, n = 1),
    mean_dep_delay_lag = lag(mean_dep_delay, n = 1)
  ) |>
  #ungroup
  ungroup() |>
  filter(
    #filter out mean_dep_delay without previous time point
    !is.na(mean_dep_delay_lag),
    #filter out mean_dep_delay calculated from less than 5 observations
    n >= 5,
    #filter out mean_dep_delay_lag calculated from less than 5 observations
    n_lag >= 5,
  )

#calculate correlation
flight_dep_lag |>
  summarize(r = cor(mean_dep_delay, mean_dep_delay_lag, use = "complete.obs"))

#create visualization
flight_dep_lag |>
  ggplot(aes(x = mean_dep_delay_lag, y = mean_dep_delay)) +
  geom_bin2d(bins = 50) +
  stat_smooth(method = "lm", formula = "y ~ x", se = FALSE)
    
rm(flight_ymdh, flight_dep_lag)

# g) Look at each origin and destination. Can you find flights that are
# suspiciously fast (i.e. flights that represent a potential data entry error)?
# Compute the air time of a flight relative to the shortest flight to that
# destination. Which flights were most delayed in the air?

#most delayed flights for each "origin" and "dest"
flights |>
  #filter out cancelled flights
  filter(!is.na(air_time)) |>
  #group by "origin" and "dest"
  group_by(origin, dest) |>
  mutate(
    #calculate minimum air time per group
    min_air_time = min(air_time),
    #calculate air_time relative to minimum per group
    rel_air_time = air_time/min_air_time,
    #calculate the top most delayed flight per group
    flg_air_time = min_rank(desc(rel_air_time)) == 1
  ) |>
  #ungroup
  ungroup() |>
  #keep only the top most delayed flight per group
  filter(flg_air_time) |>
  #select only important variables
  select(origin, dest, tailnum, air_time, min_air_time, rel_air_time) |>
  #arrange by relative metric in descending order
  arrange(desc(rel_air_time))

#visualize air time distributions for each "origin" and "dest"
flights |>
  #filter out cancelled flights
  filter(!is.na(air_time)) |>
  #group by "origin" and "dest"
  group_by(origin, dest) |>
  #calculate number of observations per group
  mutate(n = n()) |>
  #ungroup
  ungroup() |>
  #filter out groups with less than 5 observations
  filter(n >= 5) |>
  #create plot
  ggplot(aes(x = air_time, y = dest)) +
  geom_boxplot() +
  facet_wrap(~ origin, ncol = 3) +
  theme(axis.text = element_text(size = 6))

#extreme air time flights for each "origin" and "dest"
flights |>
  #filter out cancelled flights
  filter(!is.na(air_time)) |>
  #group by "origin" and "dest"
  group_by(origin, dest) |>
  mutate(
    #calculate Q25(air_time) per group
    at_Q25 = quantile(air_time, prob = 0.25),
    #calculate Q50(air_time) per group
    at_Q50 = quantile(air_time, prob = 0.50),
    #calculate Q75(air_time) per group
    at_Q75 = quantile(air_time, prob = 0.75),
    #calculate suspiciousness
    flg_air_time = air_time > at_Q75 + 10 * (at_Q75 - at_Q25)
  ) |>
  #ungroup
  ungroup() |>
  #keep only suspicious flights
  filter(flg_air_time) |>
  #select only important variables
  select(origin, dest, tailnum, air_time, at_Q25, at_Q50, at_Q75) |>
  #arrange by air_time in descending order
  arrange(desc(air_time))

# h) Find all destinations that are flown by at least two carriers. Use those
# destinations to come up with a relative ranking of the carriers based on their
# performance (i.e. smaller air_time) for the same origin and destination.
flights |>
  #filter out NA values 
  filter(!is.na(air_time)) |>
  #group by "dest"
  group_by(dest) |>
  #calculate number of observations per group
  mutate(n_carriers = n_distinct(carrier)) |>
  #ungroup
  ungroup() |>
  #filter for groups with at least 2 observations
  filter(n_carriers >= 2) |>
  #group by "dest", "origin" and carrier"
  group_by(dest, origin, carrier) |>
  #calculate performance by group ("dest", "origin" and carrier")
  summarize(mean_air_time = mean(air_time)) |>
  #calculate rank of performance by group ("dest" and "origin")
  mutate(rank = min_rank(mean_air_time)) |>
  #ungroup
  ungroup() |>
  #select important variables
  select(dest, origin, rank, carrier, mean_air_time) |>
  #arrange by "dest", "origin" and "rank"
  arrange(dest, origin, rank)

}
# 2. FACTORES
{
library("tidyverse")

# 2.1. BASICS
#[no exercises]

# 2.2. DATASET gss_cat

# a) Explore the distribution of rincome (reported income). What makes the
# default bar chart hard to understand? How could you improve the plot?
#default plot
gss_cat |>
  ggplot(aes(x = rincome)) +
  geom_bar()

#improved plot
gss_cat |>
  #relevel "Not applicable"
  mutate(rincome= fct_relevel(rincome, "Not applicable")) |>
  #use aesthetic y instead of x
  ggplot(aes(y = rincome)) +
  geom_bar()

# b) What is the most common religion in this survey? What’s the most common
# political party?
#most common religion
gss_cat |> count(relig, sort = TRUE)

#most common political party
gss_cat |> count(partyid, sort = TRUE)

# c) Which religion does denomination (denom) apply to? How can you find out
# with a table? How can you find out with a visualization?

#check possible values of "denom"
gss_cat |> count(denom) |> print(n = Inf)

#check values of "relig" and "denom"
x <- c("No answer", "Other", "Don't know", "Not applicable", "No denomination")
gss_cat |> filter(!denom %in% x) |> count(relig, denom) |> print(n = Inf)

#visualization using geom_bar()
gss_cat |>
  ggplot(aes(y = relig, fill = denom)) +
  geom_bar() +
  theme(legend.position = "bottom", legend.box = "vertical")
  
#visualization using geom_count()
gss_cat |>
  ggplot(aes(x = relig, y = denom)) +
  geom_count() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(axis.text.y = element_text(size = 7))

rm(x)

# 2.3. MODIFYING FACTOR ORDER

# a) For each factor in gss_cat identify whether the order of the levels is
# arbitrary or principled.
gss_cat |> count(marital) #somewhat principled
gss_cat |> count(race)    #principled by count of observations
gss_cat |> count(rincome) #principled
gss_cat |> count(partyid) #principled
gss_cat |> count(relig)   #arbitrary
gss_cat |> count(denom)   #arbitrary

# b) Why did moving “Not applicable” to the front of the levels move it to the
# bottom of the plot?
gss_cat |>
  #move closer to origin of axis
  ggplot(aes(y = fct_relevel(rincome, "Not applicable"))) +
  geom_bar()

# 2.4. MODIFYING FACTOR LEVEL

# a) How have the proportions of people identifying as Democrat, Republican, and
# Independent changed over time?
gss_cat_mod <- gss_cat |>
  mutate(
    partyid = fct_collapse(partyid,
      "other" = c("No answer", "Don't know", "Other party"),
      "rep" = c("Strong republican", "Not str republican"),
      "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
      "dem" = c("Not str democrat", "Strong democrat")
    )
  )

#visualization using geom_point() and geom_line()
gss_cat_mod |>
  count(year, partyid) |>
  group_by(year) |>
  mutate(p = n/sum(n)) |>
  ggplot(aes(x = year, y = p, colour = partyid)) +
  geom_point() +
  geom_line()

#visualization using geom_bar(position = "fill")
gss_cat_mod |>
  ggplot(aes(x = year, fill = partyid)) +
  geom_bar(position = "fill")

rm(gss_cat_mod)

# b) How could you collapse rincome into a small set of categories?
gss_cat |> count(rincome)
gss_cat |>
  mutate(
    rincome = fct_collapse(rincome,
      "Not applicable" = "Not applicable",
      "N/A"            = c("No answer",
                           "Don't know",
                           "Refused"),
      "$25000 or more" = "$25000 or more",
      "$10000 - 24999" = c("$20000 - 24999",
                           "$15000 - 19999",
                           "$10000 - 14999"),
      "$5000 - 9999"   = c("$8000 to 9999",
                           "$7000 to 7999",
                           "$6000 to 6999",
                           "$5000 to 5999"),
      "Lt $5000"       = c("$4000 to 4999",
                           "$3000 to 3999",
                           "$1000 to 2999",
                           "Lt $1000")
    )
  ) |>
  count(rincome)

# c) Notice there are 9 groups (excluding other) in the fct_lump example above.
# Why not 10? (Hint: type ?fct_lump, and find the default for the argument
# other_level is “Other”.)
#lump everything except n most frequent
?fct_lump
#keep other_level="Other"
gss_cat |>
  mutate(relig = fct_lump_n(relig, n = 10)) |>
  count(relig) |>
  print(n = Inf)

#change to other_level="Other answer"
gss_cat |>
  mutate(relig = fct_lump_n(relig, n = 10, other_level="Other answer")) |>
  count(relig) |>
  print(n = Inf)

}
# 3. LOGICAL VECTORS
{
library("nycflights13")
library("tidyverse")

# 3.1. COMPARISON

# a) How does dplyr::near() work? Type near to see the source code. 
# Is sqrt(2)^2 near 2?
?near
near
near(sqrt(2)^2, 2)
near(sqrt(2)^2, 2, tol = 1e-16)

# b) Use mutate(), is.na(), and count() together to describe how the missing
# values in dep_time, sched_dep_time and dep_delay are connected.
flights |>
  mutate(
    #calculate number of missing values for sched_dep_time
    sched_dep_time_na = is.na(sched_dep_time),
    #calculate number of missing values for dep_time
    dep_time_na = is.na(dep_time),
    #calculate number of missing values for dep_delay
    dep_delay_na = is.na(dep_delay),
    .keep="used"
  ) |>
  #count by sched_dep_time_na, dep_time_na and dep_delay_na
  count(sched_dep_time_na, dep_time_na, dep_delay_na)

# 3.2. BOOLEAN ALGEBRA

# a) Find all flights where arr_delay is missing but dep_delay is not. Find all
# flights where neither arr_time nor sched_arr_time are missing, but arr_delay
# is.

#arr_delay is missing but dep_delay is not
flights |>
  filter(is.na(arr_delay) & !is.na(dep_delay)) |>
  select(year:day, sched_dep_time, dep_time, dep_delay, arr_delay)

#neither arr_time nor sched_arr_time are missing, but arr_delay is
flights |>
  filter(!is.na(arr_time) & !is.na(sched_arr_time) & is.na(arr_delay)) |>
  select(year:day, sched_arr_time, arr_time, arr_delay)

# b) How many flights have a missing dep_time? What other variables are missing 
# in these rows? What might these rows represent?

#number of flights with missing dep_time
flights |> filter(is.na(dep_time)) |> count()

#look at flights with missing dep_time
flights |>
  #filter in cancelled flights
  filter(is.na(dep_time)) |>
  #select any columns with missing values
  select(where(~ any(is.na(.)))) |>
  #calculate number of missing values
  summarise(across(everything(),~ sum(is.na(.))))

#look at flights with missing dep_time
flights |>
  #select any columns with missing values
  select(where(~ any(is.na(.)))) |>
  #calculate number of missing values for all remaining columns
  mutate(across(everything(), ~is.na(.)), .keep = "used") |>
  #count by all remaining columns
  count(across(everything()))

# c) Assuming that a missing dep_time implies that a flight is cancelled, look
# at the number of cancelled flights per day. Is there a pattern? Is there a 
# connection between the proportion of cancelled flights and the average delay
# of non-cancelled flights?

#search for pattern in number of cancelled flights per day
flights |>
  #group by "year", "month", "day"
  group_by(year, month, day) |>
  summarise(
    #number of missing in "dep_time" per groups
    n_cancel = sum(is.na(dep_time)),
    .groups = "drop"
  ) |> 
  mutate(
    #calculate day of the year
    year_day = row_number()
  ) |>
  ggplot(aes(x = year_day, y = n_cancel)) +
  geom_line() +
  geom_point()

#search for connection between proportion of cancelled and average delay
flights |>
  #group by "year", "month", "day"
  group_by(year, month, day) |>
  summarise(
    #proportion of missing in "dep_time" per groups
    prop_cancel = mean(is.na(dep_time)),
    #average departure delay per groups
    mean_dep_delay   = mean(dep_delay, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = mean_dep_delay, y = prop_cancel)) +
  geom_point()

# 3.3. SUMMARIES

# a) What will sum(is.na(x)) tell you? How about mean(is.na(x))?
set.seed(123)
v_length <- 9
prop_na <- 1/3
NA_index <- sample(1:v_length, size = prop_na*v_length, replace = FALSE)
v <- 1:v_length
v[NA_index] <- NA
#v <- (1, 2, NA, 4, 5, NA, 7, 8, NA)
v
sum(is.na(v))  #v_length*prop_na
mean(is.na(v)) #prop_na

rm(v_length, prop_na, NA_index, v)

# b) What does prod() return when applied to a logical vector? What logical
# summary function is it equivalent to? What does max() return when applied to a
# logical vector? What logical summary function is it equivalent to? Read the
# documentation and perform a few experiments.
?prod
?max

tb <- tibble(
  all_TRUE = c(TRUE,  TRUE,  TRUE),   #all TRUE
  all_FALSE = c(FALSE, FALSE, FALSE), #all FALSE
  mixed = c(TRUE,  FALSE, FALSE)      #mixed
)
tb

#prod (all)
tb |> 
  summarize(
    all_TRUE = as.logical(prod(all_TRUE)),
    all_FALSE = as.logical(prod(all_FALSE)),
    mixed = as.logical(prod(mixed))
  )

#max (any)
tb |> 
  summarize(
    all_TRUE = as.logical(max(all_TRUE)),
    all_FALSE = as.logical(max(all_FALSE)),
    mixed = as.logical(max(mixed))
  )

rm(tb)

# 3.4. CONDITIONAL TRANSFORMATIONS

# a) A number is even if it’s divisible by two, which in R you can find out with
# x %% 2 == 0. Use this fact and if_else() to determine whether each number
# between 0 and 20 is even or odd.
tibble(numbers = 1:20) |>
  mutate(eve_odd = if_else(numbers %% 2 == 0, "even", "odd"))

# b) Given a vector of days like x <- c("Monday", "Saturday", "Wednesday"), use
# an if_else() statement to label them as weekends or weekdays.
set.seed(123)
wdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
  "Sunday")
tibble(wday = sample(wdays, 15, rep=TRUE)) |>
  mutate(wlab = if_else(
    wday %in% c("Saturday", "Sunday"),
    "weekends",
    "weekdays")
  )

rm(wdays)

# c) Use if_else() to compute the absolute value of a numeric vector called x.
v <- -6:6
if_else(v < 0, -v, v)

rm(v)

# d) Write a case_when() statement that uses the month and day columns from
# flights to label a selection of important US holidays (e.g., New Years Day,
# 4th of July, Thanksgiving, and Christmas). First create a logical column that
# is either TRUE or FALSE, and then create a character column that either gives
# the name of the holiday or is NA.
#follow instructions
flights |>
  mutate(
    is_important =
      month ==  1 & day ==  1 |  #New Years Day
      month ==  4 & day ==  7 |  #4th of July
      month == 11 & day == 28 |  #Thanksgiving 2013
      month == 12 & day == 25,   #Christmas
    important_day = case_when(
      !is_important           ~ NA,
      month ==  1 & day ==  1 ~ "New Years Day",
      month ==  4 & day ==  7 ~ "4th of July",
      month == 11 & day == 28 ~ "Thanksgiving",
      month == 12 & day == 25 ~ "Christmas"
    ),
    .keep = "used"
  ) |>
  group_by(is_important,important_day) |>
  count()

#revert order of variable creation
flights |>
  mutate(
    important_day = case_when(
      month ==  1 & day ==  1 ~ "New Years Day",
      month ==  4 & day ==  7 ~ "4th of July",
      month == 11 & day == 28 ~ "Thanksgiving",
      month == 12 & day == 25 ~ "Christmas",
      .default = NA
    ),
    is_important = !is.na(important_day),
    .keep = "used"
  ) |>
  group_by(is_important,important_day) |>
  count()

}
