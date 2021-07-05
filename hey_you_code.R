library(tidyverse)
library(lubridate)
library(zoo)

baseline_df <- read.csv("baseline_df.csv")

# Inspect data
head(baseline_df)
dim(baseline_df)
n_distinct(baseline_df)
glimpse(baseline_df)
str(baseline_df)

# Check for duplicates if there's any
anyDuplicated(baseline_df)

# Convert transaction date from character to date
baseline_df <- baseline_df%>% 
  mutate(date = as.Date(date))

# Add additional columns from date field
baseline_df <- baseline_df %>% 
  mutate(month = format(as.Date(date), "%m"),
         day = format(as.Date(date), "%d"),
         year = format(as.Date(date),"%Y"))

# Analyze
# Visualize the distribution of customer's behavior

ggplot(data = baseline_df, mapping = aes(x = customers))+
  geom_histogram(binwidth = 500, color = "orange")+
  annotate("rect", xmin = 12500, xmax = 14000, ymin = 10, ymax = 13.5, alpha = 0.3)+
  annotate("rect", xmin = 16500, xmax = 19000, ymin = 10, ymax = 13.5, alpha = 0.3)

# Get a clearer view of the distribution composition
ggplot(data = baseline_df, mapping = aes(x = customers, color =year))+
  geom_density()

# Movement of customers over time
ggplot(data = baseline_df, mapping = aes(x = date, y = customers))+
  geom_area(fill = "orange")+
  geom_smooth()

# Add columns for to divide years into 2 group
baseline_df <- baseline_df %>% 
  mutate(years = case_when(year == "2018" | year == "2019" ~"2018-2019",
                           year == "2020" | year == "2021" ~"2020-2021"))

#2018-2019 vs 2020-2021
#Venues vs customers
ggplot(data = baseline_df, mapping = aes(x = venues, y = customers))+
  geom_point(color = "orange")+
  facet_wrap(~years)

#Customers vs orders
ggplot(data = baseline_df, mapping = aes(x = customers, y = orders))+
  geom_point(color = "orange")+
  facet_wrap(~years)

# Orders vs tpv
ggplot(data = baseline_df, mapping = aes(x = orders, y = tpv))+
  geom_point(color = "orange")+
  facet_wrap(~years)

# tpv vs fee
ggplot(data = baseline_df, mapping = aes(x = tpv, y = fee))+
geom_point(color = "orange")+
  facet_wrap(~years)

# Identify outliers in number of orders, profit, etc...
ggplot(baseline_df, aes(x = year, y = orders))+
  geom_boxplot()

# Remove orders outliers
baseline_df <- baseline_df %>%
  mutate(orders = ifelse(orders < 15000 & year == "2018", NA, orders))

baseline_df <- baseline_df %>%
  mutate(orders = ifelse(orders < 12500 & year == "2019", NA, orders))

baseline_df <- baseline_df %>%
  mutate(orders = ifelse(orders < 5000 & year == "2020", NA, orders))

baseline_df <- baseline_df %>%
  mutate(orders = ifelse(orders < 7500 & year == "2021", NA, orders))

# Trend of orders over time
ggplot(baseline_df, aes(x = date, y = orders))+
  geom_area(fill = "orange", na.rm = TRUE)+
  geom_smooth()

# Remove fee outliers
ggplot(baseline_df, aes(x = year, y = fee))+
  geom_boxplot()

baseline_df <- baseline_df%>%
  mutate(fee = ifelse(fee < 1130, NA, fee))

# Trend of profit over time
ggplot(baseline_df, aes(x = date, y = fee))+
  geom_area(fill = "orange", na.rm = TRUE)+
  geom_smooth()

# Remove outliers from ave_cust_spend
baseline_df <- baseline_df %>%
  mutate(ave_cust_spend = ifelse((ave_cust_spend > 20 & year == "2018")|(ave_cust_spend > 20 & year == "2019")|(ave_cust_spend > 25 & year == "2020")|(ave_cust_spend > 25 & year == "2021"), NA,ave_cust_spend))

# Trend of average customer spending
ggplot(baseline_df, aes(x = date, y = ave_cust_spend))+
  geom_line(color = "orange", na.rm = TRUE)

# Remove outliers from orders per customer
baseline_df <- baseline_df %>%
  mutate(orders_per_cust = ifelse((orders_per_cust  > 3 & year == "2019")|(orders_per_cust  > 3.02 & year == "2020")|(orders_per_cust  > 3.02 & year == "2021"), NA,orders_per_cust ))

# Trend of orders per customer over time
ggplot(baseline_df, aes(x = date, y = orders_per_cust))+
  geom_line(color = "orange", na.rm = TRUE)+
  geom_smooth()

# identify outliers
ggplot(baseline_df, aes(x = year, y = customers))+
  geom_boxplot()

# Remove customers outliers
baseline_df <- baseline_df%>%
  mutate(customers = ifelse((customers < 14000 & year == "2018")|
                              (customers < 12500 & year == "2019")|
                              (customers < 5000 & year == "2020")|
                              (customers < 7500 & year == "2021")
                            , NA, customers))

# Remove venues outliers
baseline_df <- baseline_df%>%
  mutate(venues = ifelse((venues < 1000 & year == "2018")|
                           (venues < 1500 & year == "2019")|
                           (venues < 1500 & year == "2020")|
                           (venues < 1500 & year == "2021")
                         , NA, venues))


# Only include years 2020 and 2021 for comparison for the month of May
ai <- baseline_df %>% 
  filter(month %in% c("05"), year %in% c("2020", "2021"))

# Summarize on a monthly basis using average
ai2 <- ai %>% 
  group_by(year, month) %>% 
  drop_na() %>% 
  summarize(mean_customers = mean(customers),
            mean_orders = mean(orders),
            mean_venues = mean(venues),
            mean_fee = mean(fee))

# Compare may 2020 and may 2021 (start of AI implementation)
ggplot(ai2, aes(x = year, y = mean_customers, fill = year))+
  geom_col()

ggplot(ai2, aes(x = year, y = mean_orders, fill = year))+
  geom_col()

ggplot(ai2, aes(x = year, y = mean_fee, fill = year))+
  geom_col()


# Compare from January 2020 to June 2021 orders and profit

ai3 <- baseline_df %>%
  filter(year %in% c("2020", "2021")) %>%
  drop_na() %>%
  group_by(year, month) %>%
  summarize(mean_customers = mean(customers),
            mean_orders = mean(orders),
            mean_tpv = mean(tpv),
            mean_fee = mean(fee))

ai3 <- unite(ai3, "mon_year", year, month, sep = '-')

ai3<- ai3 %>% 
  mutate(mon_year = as.Date(as.yearmon(mon_year)))

ggplot(ai3, aes(x = mon_year, y = mean_orders))+
  geom_col(fill = "orange")

ggplot(ai3, aes(x = mon_year, y = mean_fee))+
  geom_col(fill = "orange")

# End of Code
