Visualizations
================
She Chee Yee (A0240383L)
2023-04-29

``` r
volcano = read.csv("../data/volcano_practice.csv")
region = read.csv("../data/continents_practice.csv")
```

## Including Plots

How many countries are there in this data set? How many unique types of
volcanoes are there in the data? What is the second most common volcano
type in the data?

``` r
volcano %>% distinct(country) %>% count()
```

    ##    n
    ## 1 69

``` r
volcano %>% mutate(type = str_remove(primary_volcano_type, "\\(s\\)|\\(es\\)|\\?")) %>%
                     distinct(type) %>% count()
```

    ##    n
    ## 1 16

``` r
volcano %>% mutate(type = str_remove(primary_volcano_type, "\\(s\\)|\\(es\\)|\\?")) %>% 
  group_by(type) %>% count(sort = TRUE)
```

    ## # A tibble: 16 × 2
    ## # Groups:   type [16]
    ##    type                   n
    ##    <chr>              <int>
    ##  1 Stratovolcano        431
    ##  2 Shield               116
    ##  3 Caldera               72
    ##  4 Pyroclastic cone      71
    ##  5 Volcanic field        68
    ##  6 Complex               45
    ##  7 Lava dome             28
    ##  8 Submarine             22
    ##  9 Fissure vent          11
    ## 10 Compound               9
    ## 11 Maar                   8
    ## 12 Tuff cone              8
    ## 13 Pyroclastic shield     7
    ## 14 Crater rows            5
    ## 15 Subglacial             5
    ## 16 Lava cone              3

``` r
volcano = volcano %>% mutate(type = str_remove(primary_volcano_type,"\\(s\\)|\\(es\\)|\\?"))

volcano %>% mutate(type = reorder(type, elevation, median)) %>%
  ggplot(aes(x = type, y = elevation)) +
  geom_boxplot() +
  coord_flip()
```

![](EDA_files/figure-gfm/unnamed-chunk-3-1.png)<!-- --> Next, let us
examine the number of volcanoes across continents. The file
continents_practice.csv classifies countries into continents. We’ve read
it in as region. Fill in the blanks in the code to create the bar chart
below.

2 ways to do this. 1. use geom_bar(), aes is Continent, geom_text with
\`label = ..count.., stat = “count”

2.  summarize the data with group_by, geom_col(x = continents, y =
    count, label = count) then geom_text()

``` r
continents= read.csv("../data/continents_practice.csv")

volcano =volcano %>% left_join(continents, by = c("country" = "Entity")) 



ggplot(data = volcano, aes(Continent)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count")
```

    ## Warning: The dot-dot notation (`..count..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(count)` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](EDA_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
v1 =volcano %>% group_by(Continent) %>%
  summarize(count = n())

ggplot(data = v1, aes(Continent, count, label = count)) +
  geom_col() +
  geom_text(nudge_y = 10.0)
```

![](EDA_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
v1 %>% mutate(pct = count/sum(count) * 100)
```

    ## # A tibble: 7 × 3
    ##   Continent     count   pct
    ##   <chr>         <int> <dbl>
    ## 1 Africa           74  8.14
    ## 2 Antarctica       17  1.87
    ## 3 Asia            262 28.8 
    ## 4 Europe          168 18.5 
    ## 5 North America   218 24.0 
    ## 6 Oceania          69  7.59
    ## 7 South America   101 11.1

The company wants to figure out when people begin Christmas shopping, so
that they can start preparing for it. ▶ Write the R code to create an
object named qn1 that contain the number of Christmas-related items sold
in each month. Specifically, look for the following keywords in the
Description column: CHRISTMASS, XMAS, REINDEER, or SANTA

``` r
retail = readRDS("../data/retail_practice.rds")%>%
  mutate(InvoiceDate = as.Date(InvoiceDate),
         month = month(InvoiceDate, label = TRUE, abbr =TRUE),
         year = year(InvoiceDate), 
         day = day(InvoiceDate))


qn1 = retail %>% filter(str_detect(Description, "CHRISTMAS|XMAS|REINDEER|SANTA")) %>%
  group_by(year, month) %>% summarize(n = n(), .groups = "drop")

qn1.1 = retail %>% filter(str_detect(Description, "CHRISTMAS|XMAS|REINDEER|SANTA")) %>%
  count(year, month)

qn1 %>% head(4)
```

    ## # A tibble: 4 × 3
    ##    year month     n
    ##   <dbl> <ord> <int>
    ## 1  2010 Dec      80
    ## 2  2011 Jan       3
    ## 3  2011 Feb       2
    ## 4  2011 Mar       1

``` r
qn1.1 %>% head(4)
```

    ## # A tibble: 4 × 3
    ##    year month     n
    ##   <dbl> <ord> <int>
    ## 1  2010 Dec      80
    ## 2  2011 Jan       3
    ## 3  2011 Feb       2
    ## 4  2011 Mar       1

``` r
retail %>% group_by(year, month) %>% distinct(day) %>% count()
```

    ## # A tibble: 13 × 3
    ## # Groups:   year, month [13]
    ##     year month     n
    ##    <dbl> <ord> <int>
    ##  1  2010 Dec      20
    ##  2  2011 Jan      24
    ##  3  2011 Feb      24
    ##  4  2011 Mar      27
    ##  5  2011 Apr      21
    ##  6  2011 May      25
    ##  7  2011 Jun      26
    ##  8  2011 Jul      26
    ##  9  2011 Aug      26
    ## 10  2011 Sep      26
    ## 11  2011 Oct      26
    ## 12  2011 Nov      26
    ## 13  2011 Dec       8

``` r
qn2 = retail %>% 
  mutate(InvoiceDate = as_date(InvoiceDate),
         month = substr(InvoiceDate, 1, 7)) %>%
  # Compute number of unique days in each month and monthly revenue
  group_by(month) %>%
  summarize(days = n_distinct(InvoiceDate),
            rev = sum(UnitPrice*Quantity)/1000) %>%
  ungroup()

qn2 %>% filter(month != "2011-12") %>% summarize(mean_days = mean(days))
```

    ## # A tibble: 1 × 1
    ##   mean_days
    ##       <dbl>
    ## 1      24.8

``` r
ggplot(data = qn2, aes(x = factor(month), y = rev)) +
  geom_point(size = 6) +
  geom_line(aes(group = 1)) +
  geom_text(aes(label = days), color = "white", size = 2.7) +
  annotate(geom = "text", x = 12.6, y = 53.5, label = "days") +
  labs(title = "Monthly revenue in thousands of pounds, all items",
       x = "Month", y = "Revenue in thousands of pounds",
       subtitle = "Number of unique transaction days in each month shown in text.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30))
```

![](EDA_files/figure-gfm/unnamed-chunk-6-1.png)<!-- --> Notice that 2011
Dec only have 8 days, we can try to increase the number of days and
quantify by scaling the number of days to average days per month in the
dataset.

``` r
library(nycflights13)
library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)

flights %>% filter(month == 12, origin == "JFK") %>% group_by(dest) %>% count(sort = TRUE)
```

    ## # A tibble: 64 × 2
    ## # Groups:   dest [64]
    ##    dest      n
    ##    <chr> <int>
    ##  1 LAX     947
    ##  2 SFO     698
    ##  3 BOS     495
    ##  4 MCO     451
    ##  5 SJU     450
    ##  6 FLL     385
    ##  7 LAS     309
    ##  8 MIA     289
    ##  9 BUF     286
    ## 10 TPA     282
    ## # ℹ 54 more rows

``` r
flights %>% group_by(carrier) %>%
  summarize(count = n(), avg_dist = sum(distance)/count) %>%
  arrange(desc(avg_dist))
```

    ## # A tibble: 16 × 3
    ##    carrier count avg_dist
    ##    <chr>   <int>    <dbl>
    ##  1 HA        342    4983 
    ##  2 VX       5162    2499.
    ##  3 AS        714    2402 
    ##  4 F9        685    1620 
    ##  5 UA      58665    1529.
    ##  6 AA      32729    1340.
    ##  7 DL      48110    1237.
    ##  8 B6      54635    1069.
    ##  9 WN      12275     996.
    ## 10 FL       3260     665.
    ## 11 MQ      26397     570.
    ## 12 EV      54173     563.
    ## 13 US      20536     553.
    ## 14 9E      18460     530.
    ## 15 OO         32     501.
    ## 16 YV        601     375.

``` r
flights %>% filter(arr_delay > 0, !is.na(arr_delay)) %>%
  group_by(month, day) %>%
  summarize(avg_delay = sum(arr_delay) / n()) %>%
  arrange(desc(avg_delay))
```

    ## `summarise()` has grouped output by 'month'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 365 × 3
    ## # Groups:   month [12]
    ##    month   day avg_delay
    ##    <int> <int>     <dbl>
    ##  1     7    10     110. 
    ##  2     9     2     103. 
    ##  3     7    22     102. 
    ##  4     9    12     100. 
    ##  5     3     8      97.5
    ##  6     4    10      94.1
    ##  7     6    24      92.1
    ##  8     5    23      88.5
    ##  9     6    27      88.4
    ## 10     7    28      86.6
    ## # ℹ 355 more rows

``` r
flights %>% filter(month == 7, day == 4) %>% 
  left_join(planes, by = c("tailnum")) %>% 
  filter(!is.na(dep_time)) %>%
  summarize(avg_seats = round(mean(seats, na.rm = TRUE), 2)) %>% pull(avg_seats)
```

    ## [1] 140.56

``` r
flights %>% mutate(dep_hour = as.integer(sched_dep_time / 100)) %>%
  group_by(dep_hour) %>%
  mutate(over = case_when(dep_delay > 15 ~ 1,
                          TRUE ~ 0)) %>%
  summarize(p = sum(over) / n()) %>%
  arrange(desc(p))
```

    ## # A tibble: 20 × 2
    ##    dep_hour      p
    ##       <int>  <dbl>
    ##  1       21 0.358 
    ##  2       20 0.345 
    ##  3       19 0.334 
    ##  4       22 0.309 
    ##  5       18 0.301 
    ##  6       17 0.296 
    ##  7       16 0.275 
    ##  8       15 0.266 
    ##  9       23 0.257 
    ## 10       14 0.227 
    ## 11       13 0.211 
    ## 12       12 0.177 
    ## 13       11 0.154 
    ## 14       10 0.136 
    ## 15        9 0.118 
    ## 16        8 0.110 
    ## 17        7 0.0810
    ## 18        6 0.0738
    ## 19        5 0.0594
    ## 20        1 0

``` r
# What plane traveled the most times in 2013? Plot the number of trips per week over the year for that plane.
plane = flights  %>% left_join(planes, by = c("tailnum" = "tailnum")) %>%
  filter(year.x == 2013, !(is.na(dep_time))) %>% 
  group_by(tailnum) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>% 
  top_n(1, count) %>% pull(tailnum)

most = flights %>% filter(tailnum == plane, !(is.na(dep_time)))
most_trip = flights %>% filter(tailnum == plane, !(is.na(dep_time))) %>%
  unite(col = date,year,  month, day, sep = "-") %>%
  mutate(date = as.Date(date),
         week = week(date)) %>%
  group_by(week) %>%
  summarize(count = n())

ggplot(data = most_trip, aes(week, count)) + 
  geom_point(color = "red3", size = 3) + 
  geom_line() + 
  labs(x = "Week of the year", y = "", title = "Trips per week for N725MQ in 2013") + 
  theme_minimal()
```

![](EDA_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
colnms <- colnames(most)

# filter
most %>%
  filter_at(vars(all_of(colnms)), any_vars(is.na(.)))
```

    ## # A tibble: 2 × 19
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ## 1  2013     5    20     1837           1800        37     2252           2025
    ## 2  2013     6     5      742            700        42     1116            920
    ## # ℹ 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>

scatterplot

``` r
data(mpg)
str(mpg)
```

    ## tibble [234 × 11] (S3: tbl_df/tbl/data.frame)
    ##  $ manufacturer: chr [1:234] "audi" "audi" "audi" "audi" ...
    ##  $ model       : chr [1:234] "a4" "a4" "a4" "a4" ...
    ##  $ displ       : num [1:234] 1.8 1.8 2 2 2.8 2.8 3.1 1.8 1.8 2 ...
    ##  $ year        : int [1:234] 1999 1999 2008 2008 1999 1999 2008 1999 1999 2008 ...
    ##  $ cyl         : int [1:234] 4 4 4 4 6 6 6 4 4 4 ...
    ##  $ trans       : chr [1:234] "auto(l5)" "manual(m5)" "manual(m6)" "auto(av)" ...
    ##  $ drv         : chr [1:234] "f" "f" "f" "f" ...
    ##  $ cty         : int [1:234] 18 21 20 21 16 18 18 18 16 20 ...
    ##  $ hwy         : int [1:234] 29 29 31 30 26 26 27 26 25 28 ...
    ##  $ fl          : chr [1:234] "p" "p" "p" "p" ...
    ##  $ class       : chr [1:234] "compact" "compact" "compact" "compact" ...

``` r
df = mpg %>% mutate(trans = str_sub(trans, 1, 4))

df1 = mpg %>% mutate(trans = case_when(str_detect(trans, "^auto") ~ "auto",
                                       str_detect(trans, "^manual") ~ "manual",
                                       TRUE ~ trans))

ggplot(data = df1, aes(x = displ, y = hwy, label = trans)) +
  geom_point(size = 2,  alpha = 1) 
```

![](EDA_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggplot(data = df1, aes(x = displ, y = hwy, label = trans)) +
  geom_point(aes(shape = class),position = "jitter")  + 
  scale_shape_manual(values = 1:7)
```

![](EDA_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
ggplot(data = cars, aes(speed, dist)) + 
  geom_point(aes(size = 2.0,alpha = 0.4), show.legend = FALSE) +
  labs(title = "Relationship between Speed and Braking", 
       ylab = "Stopping distance (ft)",
       xlab = "Speed (mph)")
```

![](EDA_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

Histogram

``` r
heights = read.csv("../data/heights.csv", header= TRUE, stringsAsFactors = TRUE)

ggplot(data = heights) + 
  geom_histogram(aes(x = earn/1000, y = ..density.., fill = sex),  
                 binwidth = 10, color = "white", boundary = 0) 
```

![](EDA_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggplot(data = heights) + 
  geom_histogram(aes(x = earn/1000, y = ..density.., fill = sex),  
                 binwidth = 10, color = "white", boundary = 0, position = "dodge") +
  scale_fill_discrete(name = "Gender", labels = c("Female", "Male"))
```

![](EDA_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
ggplot(data = heights) + 
  geom_density(aes(x= earn/1000, fill = sex), alpha = 0.2)
```

![](EDA_files/figure-gfm/unnamed-chunk-9-3.png)<!-- --> CDF

``` r
heights %>%
  filter(sex == "female") %>%
  sample_n(20) %>%
  ggplot(aes(x = earn/1000)) + 
  stat_ecdf()
```

![](EDA_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
heights %>%
  ggplot(aes(x = earn/1000, col = sex)) +
  stat_ecdf()
```

![](EDA_files/figure-gfm/unnamed-chunk-10-2.png)<!-- --> normal dist

``` r
ggplot(data = data.frame(x = seq(-5, 5)), aes(x)) +
stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
labs(x = "", y = "", title = "A standard normal distribution")
```

![](EDA_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
heights_f = heights %>% filter(sex == "female")

heights_f %>%
  ggplot(aes(height)) + 
  geom_density(fill = "lightblue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean(heights_f$height), sd(heights_f$height)))
```

![](EDA_files/figure-gfm/unnamed-chunk-11-2.png)<!-- --> line graphs

``` r
UN_data = read_excel("../data/UNESCAP_population_2010_2015.xlsx", sheet = 3)
head(UN_data)
```

    ## # A tibble: 6 × 7
    ##   e_fname     Y2010 Y2011 Y2012 Y2013 Y2014 Y2015
    ##   <chr>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Afghanistan  2447  2459  2454  2438  2422  2412
    ## 2 Armenia        92    94    97    99   101   101
    ## 3 Australia     710   731   740   743   745   752
    ## 4 Azerbaijan    333   348   370   394   413   425
    ## 5 Bangladesh   7725  7622  7565  7540  7525  7503
    ## 6 Bhutan         35    35    35    34    33    32

``` r
pop1 = UN_data %>% gather(Y2010:Y2015, key = years, value = population) %>%
  mutate(years = as.integer(str_sub(years, 2, 5))) %>%
  filter(e_fname %in% c("Singapore", "Malaysia", "Cambodia",
  "Thailand", "Viet Nam", "Lao PDR",
  "Brunei Darussalam"))

ggplot(data = pop1, aes(years, population)) +
  geom_line(aes(color = e_fname))
```

![](EDA_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
end_pts = pop1 %>% filter(years == 2015) %>%
  mutate(fname = recode(e_fname, "Brunei Darussalam" = "Brunei"))

ggplot(data = pop1, aes(years, population, group = e_fname)) + 
  geom_line() +
  geom_point(data = end_pts, aes(years)) + 
  geom_text(data = end_pts, aes(label = fname), hjust = "left", nudge_x = 0.1) +
  geom_vline(xintercept = 2011, linetype = "dashed") +
  geom_hline(yintercept = 1000, linetype = "dashed") +
  geom_abline(slope = 1)
```

![](EDA_files/figure-gfm/unnamed-chunk-12-2.png)<!-- --> reconstruction
of graph

``` r
library(dslabs)
```

    ## 
    ## Attaching package: 'dslabs'

    ## The following object is masked _by_ '.GlobalEnv':
    ## 
    ##     heights

``` r
library(ggthemes)

df = murders
head(df)
```

    ##        state abb region population total
    ## 1    Alabama  AL  South    4779736   135
    ## 2     Alaska  AK   West     710231    19
    ## 3    Arizona  AZ   West    6392017   232
    ## 4   Arkansas  AR  South    2915918    93
    ## 5 California  CA   West   37253956  1257
    ## 6   Colorado  CO   West    5029196    65

``` r
df %>% distinct(state)
```

    ##                   state
    ## 1               Alabama
    ## 2                Alaska
    ## 3               Arizona
    ## 4              Arkansas
    ## 5            California
    ## 6              Colorado
    ## 7           Connecticut
    ## 8              Delaware
    ## 9  District of Columbia
    ## 10              Florida
    ## 11              Georgia
    ## 12               Hawaii
    ## 13                Idaho
    ## 14             Illinois
    ## 15              Indiana
    ## 16                 Iowa
    ## 17               Kansas
    ## 18             Kentucky
    ## 19            Louisiana
    ## 20                Maine
    ## 21             Maryland
    ## 22        Massachusetts
    ## 23             Michigan
    ## 24            Minnesota
    ## 25          Mississippi
    ## 26             Missouri
    ## 27              Montana
    ## 28             Nebraska
    ## 29               Nevada
    ## 30        New Hampshire
    ## 31           New Jersey
    ## 32           New Mexico
    ## 33             New York
    ## 34       North Carolina
    ## 35         North Dakota
    ## 36                 Ohio
    ## 37             Oklahoma
    ## 38               Oregon
    ## 39         Pennsylvania
    ## 40         Rhode Island
    ## 41       South Carolina
    ## 42         South Dakota
    ## 43            Tennessee
    ## 44                Texas
    ## 45                 Utah
    ## 46              Vermont
    ## 47             Virginia
    ## 48           Washington
    ## 49        West Virginia
    ## 50            Wisconsin
    ## 51              Wyoming

``` r
df %>% count(state)
```

    ##                   state n
    ## 1               Alabama 1
    ## 2                Alaska 1
    ## 3               Arizona 1
    ## 4              Arkansas 1
    ## 5            California 1
    ## 6              Colorado 1
    ## 7           Connecticut 1
    ## 8              Delaware 1
    ## 9  District of Columbia 1
    ## 10              Florida 1
    ## 11              Georgia 1
    ## 12               Hawaii 1
    ## 13                Idaho 1
    ## 14             Illinois 1
    ## 15              Indiana 1
    ## 16                 Iowa 1
    ## 17               Kansas 1
    ## 18             Kentucky 1
    ## 19            Louisiana 1
    ## 20                Maine 1
    ## 21             Maryland 1
    ## 22        Massachusetts 1
    ## 23             Michigan 1
    ## 24            Minnesota 1
    ## 25          Mississippi 1
    ## 26             Missouri 1
    ## 27              Montana 1
    ## 28             Nebraska 1
    ## 29               Nevada 1
    ## 30        New Hampshire 1
    ## 31           New Jersey 1
    ## 32           New Mexico 1
    ## 33             New York 1
    ## 34       North Carolina 1
    ## 35         North Dakota 1
    ## 36                 Ohio 1
    ## 37             Oklahoma 1
    ## 38               Oregon 1
    ## 39         Pennsylvania 1
    ## 40         Rhode Island 1
    ## 41       South Carolina 1
    ## 42         South Dakota 1
    ## 43            Tennessee 1
    ## 44                Texas 1
    ## 45                 Utah 1
    ## 46              Vermont 1
    ## 47             Virginia 1
    ## 48           Washington 1
    ## 49        West Virginia 1
    ## 50            Wisconsin 1
    ## 51              Wyoming 1

``` r
r = df %>% summarize(rate = sum(total)/ (sum(population) / 1e6)) %>%
  pull(rate)

df %>% ggplot(aes(x = population/1e6, y = total)) +
  geom_point(aes(color = region), size = 3,  show.legend = FALSE) +
  geom_text(aes(label = abb), nudge_x = 0.07) +
  scale_x_log10() + 
  scale_y_log10() +
  labs(title = "US Gun Murders in 2010",
    x = "Population in millions (log scale)",
    y = "Total number of murders (log scale)") +
  geom_abline(slope = 1, intercept = log10(r), lty = 2, color = "darkgrey") + 
  theme_economist()
```

![](EDA_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

bar charts

``` r
budget_cat = c("Manpower", "Asset", "Other") # create data frame
amount = c(519.4, 38.0, 141.4)
op_budget = data.frame(budget_cat, amount)
barplot(op_budget$amount, border = NA, names.arg = op_budget$budget_cat)
```

![](EDA_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
ggplot(data = op_budget, aes(budget_cat, amount)) +
  geom_col()
```

![](EDA_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
op_budget %>% mutate(budget_cat = reorder(budget_cat, -amount)) %>%
  ggplot(aes(budget_cat, amount)) + 
  geom_col()
```

![](EDA_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

``` r
imda_data = readRDS("../data/imda.rds")
str(imda_data)
```

    ## 'data.frame':    210 obs. of  6 variables:
    ##  $ ever_used     : chr  "97.1" "32.9" "39.5" "30.9" ...
    ##  $ age           : chr  "15-19" "15-19" "15-19" "15-19" ...
    ##  $ sample_size   : chr  "161" "161" "161" "161" ...
    ##  $ year          : chr  "2013" "2013" "2013" "2013" ...
    ##  $ _id           : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ media_activity: chr  "Watch MediaCorp TV" "Watch Singtel TV" "Watch StarHub TV" "Watch animation" ...

``` r
young_adults = imda_data %>% filter(age == "20-29", year == 2015) %>%
  mutate(pct = as.numeric(ever_used),
         media_activity = reorder(media_activity, pct))

ggplot(data = young_adults) +
  geom_col(aes(media_activity, pct, fill = media_activity)) + 
  coord_flip()
```

![](EDA_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
tv = imda_data %>%
  filter(media_activity %in% c("Watch MediaCorp TV",
                               "Watch StarHub TV", "Watch Singtel TV")) %>%
  mutate(pct = as.numeric(ever_used),
         age = reorder(as.factor(age), as.numeric(substr(age, 1, 2))))


p1 = ggplot(tv, aes(x = year, y = pct)) +
  geom_col(aes(fill = media_activity),
           position = "dodge") +
  facet_wrap(~ age) +
  labs(title = "Percentage of Young Adults ...",
       x = "Activity", y = "Percentage", fill = "Activity") +
  theme(legend.position = "bottom")
```

smoothed line & loess

``` r
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(position = "jitter") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 5)) +
  labs(x = "Engine Displacement (l)", y = "Highway Miles per Gallon")
```

![](EDA_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(position = "jitter") +
  geom_smooth(method = "loess", span = 0.2) +
  labs(x = "Engine Displacement (l)", y = "Highway Miles per Gallon")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](EDA_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

loess by group

``` r
ggplot(mpg, aes(x = displ, y = hwy, col = drv)) +
  geom_point(position = "jitter") +
  geom_smooth(aes(linetype = drv),
              method = "loess", show.legend = FALSE) +
  labs(x = "Engine Displacement (l)", y = "Highway Miles per Gallon") +
  scale_color_discrete(name = "Drive type",
  labels = c("Four-wheel", "Front-wheel", "Rear-wheel")) +
  theme(legend.position = "top")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](EDA_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

rug plot

``` r
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(position = "jitter") +
  geom_rug(position = "jitter") +
  labs(x = "Engine Displacement (l)", y = "Highway Miles per Gallon")
```

![](EDA_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

boxplot

``` r
df = mpg %>% mutate(class = reorder(class, hwy, FUN = median))

ggplot(df, aes(x = class, y = hwy)) +
  geom_boxplot(outlier.color = "red",  outlier.shape = 1) +
  labs(x = "Class of Car", y = "Highway Fuel Efficiency")
```

![](EDA_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

remove outliers

``` r
heights2 = heights %>%
  group_by(sex) %>%
  mutate(max_earn = quantile(earn, 0.75) + 1.5 * IQR(earn),
  min_earn = quantile(earn, 0.25) - 1.5 * IQR(earn)) %>%
  filter(earn <= max_earn & earn >= min_earn) %>% ungroup()

ggplot(data = heights2) +
geom_boxplot(aes(x = earn, y = sex))
```

![](EDA_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

2D Histograms

``` r
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_bin2d(binwidth = c(0.5, 5)) +
  scale_fill_gradient(name = "Count", low = "white", high = "red") +
  labs(title = "2D Bins",
       x = "Engine Displacement (l)", y = "Highway Miles per Gallon")
```

![](EDA_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_hex(binwidth = c(0.5, 5)) +
  scale_fill_gradient(name = "Count", low = "white", high = "red") +
  labs(title = "Hexagonal Bins",
       x = "Engine Displacement (l)", y = "Highway Miles per Gallon")
```

![](EDA_files/figure-gfm/unnamed-chunk-21-2.png)<!-- -->

``` r
library(ggrepel)
data(gapminder)

summary(gapminder)
```

    ##                 country           year      infant_mortality life_expectancy
    ##  Albania            :   57   Min.   :1960   Min.   :  1.50   Min.   :13.20  
    ##  Algeria            :   57   1st Qu.:1974   1st Qu.: 16.00   1st Qu.:57.50  
    ##  Angola             :   57   Median :1988   Median : 41.50   Median :67.54  
    ##  Antigua and Barbuda:   57   Mean   :1988   Mean   : 55.31   Mean   :64.81  
    ##  Argentina          :   57   3rd Qu.:2002   3rd Qu.: 85.10   3rd Qu.:73.00  
    ##  Armenia            :   57   Max.   :2016   Max.   :276.90   Max.   :83.90  
    ##  (Other)            :10203                  NA's   :1453                    
    ##    fertility       population             gdp               continent   
    ##  Min.   :0.840   Min.   :3.124e+04   Min.   :4.040e+07   Africa  :2907  
    ##  1st Qu.:2.200   1st Qu.:1.333e+06   1st Qu.:1.846e+09   Americas:2052  
    ##  Median :3.750   Median :5.009e+06   Median :7.794e+09   Asia    :2679  
    ##  Mean   :4.084   Mean   :2.701e+07   Mean   :1.480e+11   Europe  :2223  
    ##  3rd Qu.:6.000   3rd Qu.:1.523e+07   3rd Qu.:5.540e+10   Oceania : 684  
    ##  Max.   :9.220   Max.   :1.376e+09   Max.   :1.174e+13                  
    ##  NA's   :187     NA's   :185         NA's   :2972                       
    ##              region    
    ##  Western Asia   :1026  
    ##  Eastern Africa : 912  
    ##  Western Africa : 912  
    ##  Caribbean      : 741  
    ##  South America  : 684  
    ##  Southern Europe: 684  
    ##  (Other)        :5586

``` r
gapminder %>% filter(!is.na(gdp)) %>%
  group_by(year) %>%
  count()
```

    ## # A tibble: 52 × 2
    ## # Groups:   year [52]
    ##     year     n
    ##    <int> <int>
    ##  1  1960    95
    ##  2  1961    96
    ##  3  1962    96
    ##  4  1963    96
    ##  5  1964    96
    ##  6  1965   102
    ##  7  1966   104
    ##  8  1967   105
    ##  9  1968   106
    ## 10  1969   106
    ## # ℹ 42 more rows

``` r
df = gapminder %>% filter(year == 2011) %>%
  mutate(gdpc = gdp / population) %>%
  filter(!is.na(gdpc))
  

ggplot(data = df, aes(gdpc, life_expectancy)) + 
  geom_point(aes(color = continent, size = population)) + 
  scale_x_log10() +
  geom_text_repel(aes(label = country), show.legend = FALSE, size = 2) +
  guides(size = "none")
```

    ## Warning: ggrepel: 75 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](EDA_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

Top streaming services in the US: There are only a few data points on
the graph. We can manually create the data frame.

``` r
services = c("Netflix", "PrimeVideo", "hulu", 
             "Disney", "AppleTV", "peacock", "HBO", "Paramount", "others")
level =  c("Netflix", "PrimeVideo", "hulu", 
             "Disney", "AppleTV", "peacock", "HBO", "Paramount", "others")

streaming = tibble::tribble(
  ~service, ~`2020`, ~`2021`,
  "Netflix", 29, 20,
  "Prime", 21, 16,
  "Hulu", 16, 13,
  "Disney", 12, 11,
  "Apple", 4, 5,
  "Peacock", 0, 5,
  "HBO", 3, 12,
  "Paramount", 2, 3,
  "Others", 13, 15
)


y_2020 = c(29,21,16,12,4,0,3,2,13)
y_2021 = c(20,16,13,11,5,5,12,3,15)

df = data.frame(services, y_2020, y_2021)

df = df %>% gather(`y_2020`:`y_2021`, key = "year", value = "pct") %>%
  mutate(year = (str_sub(year, 3, 7))) %>%
  mutate(services = factor(services, ordered = TRUE,
                           levels = level))

ggplot(data = df, aes(services, pct, label = paste0(pct, "%"), fill = year)) +
  geom_col(position = "dodge") + 
  geom_text(aes(services, pct + 1, group = year), position = position_dodge(width = 1)) + 
  scale_fill_manual(values = c("2020" = "red3", "2021" = "black")) + 
  scale_y_continuous(labels = scales::label_number(suffix = "%")) + 
  labs(x = "", y= "", title = "US Streaming Market Share") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")
```

![](EDA_files/figure-gfm/unnamed-chunk-23-1.png)<!-- --> improvement

``` r
growth = df %>%
  group_by(services) %>%
  mutate(pct_change = pct - lag(pct)) %>%
  na.omit() 


ggplot(data= growth) + 
  geom_col(aes(services, pct_change, fill = pct_change > 0)) + 
  coord_flip() + 
  scale_fill_manual(values = c(`TRUE` = "darkcyan", `FALSE` = "red3"))
```

![](EDA_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

How Chinese New Year compares with Thanksgiving: cny_thanksgiving.xls

``` r
cny = read_excel("../data/cny_thanksgiving.xlsx") %>%
  mutate(Population = case_when(Country == "China" ~ 1339724852,
                                Country == "USA" ~ 307745538),
         Per_capita = Amount/Population,
         Amount = Amount / 1e9) %>%
  filter(Category != "Population Census 2010") %>%
  rename(total = Amount) %>%
  gather(total, Per_capita, key = "mertics", value = "amount")

ggplot(data = cny %>% filter(mertics == "Per_capita"), aes(Country, amount, fill = Country)) + 
  geom_col(show.legend = FALSE)+ 
  facet_wrap(~ Category, scales = "free_y", labeller = label_wrap_gen()) + 
  scale_fill_manual(values = c("China" = "red3", "USA" = "blue")) + 
  labs(x = "" , y = "", fill = "",
       title = "How does Chinese New Year compare to Thanksgiving?") + 
  theme_minimal()
```

![](EDA_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

Monthly cereal consumption: personal_consumption_expenditures.xlsx.

``` r
cereal = read_excel("../data/personal_consumption_expenditures.xlsx") %>%
  filter(`Sub-Category` == "Cereals") %>%
  rename(subcat = `Sub-Category`,
         amount = `Millions of Dollars`) %>%
  mutate(amount = amount /1000)

ggplot(data = cereal, aes(x = Month, y = amount)) +
  geom_line(linewidth = 2) + 
  geom_area(fill = "cornflowerblue", alpha = 0.2) + 
  labs(x= "", y = "", title = "Well that sure was unusual",
       subtitle = "Monthly US personal consumption expenditures on cereals",
       caption = "Source: US Burea") + 
  theme_minimal()
```

![](EDA_files/figure-gfm/unnamed-chunk-26-1.png)<!-- --> graph 2

``` r
covid = cereal %>% mutate(year = year(Month),
                          month = month(Month, label = TRUE, abbr = TRUE)) %>%
  filter(year == 2020 | year == 2019) %>%
  select(amount:month) %>%
  spread(key = year, value = amount)


ggplot(data = covid, aes(month, amount)) +
    geom_segment(aes(x = month, y = `2020`, xend = month, yend = `2019`), color = "grey") + 
  geom_point(data = covid, aes(x = month, y = `2019`), color = "grey", alpha = 0.5, size = 5) + 
  geom_point(data = covid, aes(x = month, y =`2020`), color = "indianred4", size = 5) + 
  labs(x = "", y= "Expenditure in billions of dollars",
       title = "Consumption of Cereals during the pandemic, 2020 (red) 2019 (grey)", 
       caption = "Source: US Burea of Economic Analysis")
```

![](EDA_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

Who do we spend time with in our life:
time-spent-with-relationships-by-age-us.csv.

``` r
time = read.csv("../data/time-spent-with-relationships-by-age-us.csv")
time = time %>% rename(alone = 4,
                friends = 5,
                children = 6,
                family = `Time.spent.with.with.parents..siblings.and.other.family..by.age.of.respondent..United.States.`, 
                partner = `Time.spent.with.partner..by.age.of.respondent..United.States.`,
                coworkers = `Time.spent.with.coworkers..by.age.of.respondent..United.States.`,)

time = time %>% gather(`alone`:`coworkers`, key = "type", value = "minutes") %>%
  filter(Year <= 80)

labels = time %>% filter(Year == 80 )

ggplot(data = time, aes(Year, minutes, color = type), show.legend = FALSE) +
  geom_point(show.legend = FALSE) + 
  geom_line(show.legend = FALSE) + 
  geom_text_repel(data = labels, aes(label = type), show.legend = FALSE, nudge_x = 3.5) + 
  scale_y_continuous(labels = scales::label_number(suffix = " mins"),
                     breaks = seq(0,400,100)) + 
  scale_x_continuous(breaks = c(15,20,30,40,50,60,70,80), limits = c(15,90)) + 
  labs(x = "Age", y = "", 
       title = "Who Americans spend their time with, by age") + 
  scale_x_continuous(guide = "none") + # this removes the legend 
  theme_minimal() + 
  labs(x = "", y= "lorem ipsum",
       title = "lorem ipsum", 
       caption = "Source: US Burea of Economic Analysis")
```

    ## Scale for x is already present.
    ## Adding another scale for x, which will replace the existing scale.

![](EDA_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

NBA plots

``` r
library(ggrepel)
nba = read.csv("../data/NBA_Team_stats.csv") %>%
  separate(Year, into = c("start_year", "end_year"), convert = TRUE) %>%
  filter(start_year >= 2010) %>%
  select(`No`:`Pf`, start_year) %>%
  gather(`Pts`:`Pf`, key = "type", value = "values")

pts = nba %>% filter(type == "Pts") 
end_labels = pts %>% filter(start_year == 2021) %>%
  top_n(10, values)

top_10 = end_labels %>% pull(Team)

pts = pts %>% filter(Team %in% top_10)

ggplot(data = pts, aes(start_year, values, color = Team, show.legend = FALSE)) +
  geom_line(aes(groups = Team), show.legend = FALSE) +
  geom_text_repel(data = end_labels, aes(start_year, values, label = Team), show.legend = FALSE, 
                  nudge_x = 2) +
  scale_x_continuous(labels = scales::label_number(suffix = " season")) + 
  scale_y_continuous(breaks = seq(100,130,5)) +
  labs(x = "Points", y = "Seasons", title = "NBA Average points for each team (2010-2021)" ) + 
  theme_minimal()
```

    ## Warning in geom_line(aes(groups = Team), show.legend = FALSE): Ignoring unknown
    ## aesthetics: groups

![](EDA_files/figure-gfm/unnamed-chunk-29-1.png)<!-- --> nba players

``` r
players = read.csv("../data/NBA_Player_Stats.csv") %>%
  separate(Year, into = c("start", "end")) %>%
  mutate(start = as.numeric(start), 
         end = as.numeric(end)) %>%
  filter(start >= 2010)

players %>% distinct(Player)
```

    ##                        Player
    ## 1                 Jeff Adrien
    ## 2               Arron Afflalo
    ## 3                Maurice Ager
    ## 4               Alexis Ajinça
    ## 5               Solomon Alabi
    ## 6                Cole Aldrich
    ## 7           LaMarcus Aldridge
    ## 8                 Malik Allen
    ## 9                  Ray Allen*
    ## 10                 Tony Allen
    ## 11            Al-Farouq Aminu
    ## 12               Lou Amundson
    ## 13             Chris Andersen
    ## 14             David Andersen
    ## 15             James Anderson
    ## 16              Ryan Anderson
    ## 17            Carmelo Anthony
    ## 18               Joel Anthony
    ## 19           Hilton Armstrong
    ## 20              Carlos Arroyo
    ## 21             Darrell Arthur
    ## 22                  Ömer Aşık
    ## 23              D.J. Augustin
    ## 24               Luke Babbitt
    ## 25            Renaldo Balkman
    ## 26               Marcus Banks
    ## 27            Leandro Barbosa
    ## 28                 J.J. Barea
    ## 29            Andrea Bargnani
    ## 30                Matt Barnes
    ## 31                Earl Barron
    ## 32               Brandon Bass
    ## 33                Tony Battie
    ## 34              Shane Battier
    ## 35              Nicolas Batum
    ## 36             Jerryd Bayless
    ## 37            Michael Beasley
    ## 38          Rodrigue Beaubois
    ## 39            Marco Belinelli
    ## 40               Charlie Bell
    ## 41                  Raja Bell
    ## 42                 Mike Bibby
    ## 43            Andris Biedriņš
    ## 44           Chauncey Billups
    ## 45               DeJuan Blair
    ## 46                Steve Blake
    ## 47             Andray Blatche
    ## 48               Eric Bledsoe
    ## 49               Keith Bogans
    ## 50               Andrew Bogut
    ## 51                Matt Bonner
    ## 52              Trevor Booker
    ## 53              Carlos Boozer
    ## 54                Chris Bosh*
    ## 55               Earl Boykins
    ## 56             Craig Brackins
    ## 57              Avery Bradley
    ## 58                Elton Brand
    ## 59               Corey Brewer
    ## 60              Ronnie Brewer
    ## 61               Jon Brockman
    ## 62               Aaron Brooks
    ## 63              Derrick Brown
    ## 64                Kwame Brown
    ## 65              Shannon Brown
    ## 66               Kobe Bryant*
    ## 67             Chase Budinger
    ## 68               Caron Butler
    ## 69              Rasual Butler
    ## 70               Andrew Bynum
    ## 71                 Will Bynum
    ## 72              José Calderón
    ## 73               Marcus Camby
    ## 74           Derrick Caracter
    ## 75             Brian Cardinal
    ## 76              Rodney Carney
    ## 77            DeMarre Carroll
    ## 78               Matt Carroll
    ## 79             Anthony Carter
    ## 80               Vince Carter
    ## 81                Omri Casspi
    ## 82             Mario Chalmers
    ## 83             Tyson Chandler
    ## 84            Wilson Chandler
    ## 85             Josh Childress
    ## 86                 Earl Clark
    ## 87             Jarron Collins
    ## 88              Jason Collins
    ## 89            Sherron Collins
    ## 90            Darren Collison
    ## 91              Nick Collison
    ## 92                Mike Conley
    ## 93                 Brian Cook
    ## 94               Daequan Cook
    ## 95              Marcus Cousin
    ## 96           DeMarcus Cousins
    ## 97             Jamal Crawford
    ## 98            Jordan Crawford
    ## 99           Dante Cunningham
    ## 100             Stephen Curry
    ## 101          Samuel Dalembert
    ## 102             Erick Dampier
    ## 103           Antonio Daniels
    ## 104           Marquis Daniels
    ## 105               Baron Davis
    ## 106                  Ed Davis
    ## 107                Glen Davis
    ## 108               Austin Daye
    ## 109            Carlos Delfino
    ## 110                 Luol Deng
    ## 111             DeMar DeRozan
    ## 112                Boris Diaw
    ## 113                 Ike Diogu
    ## 114             DeSagana Diop
    ## 115             Keyon Dooling
    ## 116               Joey Dorsey
    ## 117             Toney Douglas
    ## 118     Chris Douglas-Roberts
    ## 119            Zabian Dowdell
    ## 120              Goran Dragić
    ## 121              Jared Dudley
    ## 122               Chris Duhon
    ## 123               Tim Duncan*
    ## 124             Mike Dunleavy
    ## 125             Ronald Dupree
    ## 126              Kevin Durant
    ## 127              Devin Ebanks
    ## 128           Wayne Ellington
    ## 129               Monta Ellis
    ## 130           Francisco Elson
    ## 131                Melvin Ely
    ## 132               Semih Erden
    ## 133              Jeremy Evans
    ## 134             Maurice Evans
    ## 135              Reggie Evans
    ## 136              Tyreke Evans
    ## 137             Patrick Ewing
    ## 138          Christian Eyenga
    ## 139             Jordan Farmar
    ## 140            Derrick Favors
    ## 141            Raymond Felton
    ## 142            Rudy Fernández
    ## 143            Kyrylo Fesenko
    ## 144             Landry Fields
    ## 145              Derek Fisher
    ## 146               Jonny Flynn
    ## 147               Gary Forbes
    ## 148                 T.J. Ford
    ## 149               Jeff Foster
    ## 150                Randy Foye
    ## 151             Channing Frye
    ## 152              Dan Gadzuric
    ## 153           Sundiata Gaines
    ## 154          Danilo Gallinari
    ## 155          Francisco García
    ## 156            Kevin Garnett*
    ## 157                Marc Gasol
    ## 158                 Pau Gasol
    ## 159                  Rudy Gay
    ## 160                Alonzo Gee
    ## 161               Paul George
    ## 162             Daniel Gibson
    ## 163                Taj Gibson
    ## 164            Manu Ginóbili*
    ## 165                Ryan Gomes
    ## 166               Drew Gooden
    ## 167                Ben Gordon
    ## 168               Eric Gordon
    ## 169             Marcin Gortat
    ## 170               Joey Graham
    ## 171            Stephen Graham
    ## 172             Danny Granger
    ## 173                Aaron Gray
    ## 174               Danny Green
    ## 175                Jeff Green
    ## 176              Willie Green
    ## 177              Donté Greene
    ## 178              Orien Greene
    ## 179             Blake Griffin
    ## 180             Hamed Haddadi
    ## 181          Richard Hamilton
    ## 182          Tyler Hansbrough
    ## 183            Luke Harangody
    ## 184              James Harden
    ## 185             Al Harrington
    ## 186              Devin Harris
    ## 187              Manny Harris
    ## 188               Mike Harris
    ## 189             Udonis Haslem
    ## 190             Spencer Hawes
    ## 191               Chuck Hayes
    ## 192            Gordon Hayward
    ## 193             Lazar Hayward
    ## 194           Brendan Haywood
    ## 195               Luther Head
    ## 196          Gerald Henderson
    ## 197              Xavier Henry
    ## 198               Roy Hibbert
    ## 199              J.J. Hickson
    ## 200                      Nenê
    ## 201               George Hill
    ## 202               Grant Hill*
    ## 203               Jordan Hill
    ## 204              Kirk Hinrich
    ## 205              Jrue Holiday
    ## 206              Ryan Hollins
    ## 207                Al Horford
    ## 208               Eddie House
    ## 209             Dwight Howard
    ## 210               Josh Howard
    ## 211              Juwan Howard
    ## 212             Lester Hudson
    ## 213            Kris Humphries
    ## 214               Serge Ibaka
    ## 215            Andre Iguodala
    ## 216        Zydrunas Ilgauskas
    ## 217               D.J. Mbenga
    ## 218            Ersan İlyasova
    ## 219                Royal Ivey
    ## 220              Jarrett Jack
    ## 221           Darnell Jackson
    ## 222           Stephen Jackson
    ## 223              Damion James
    ## 224              LeBron James
    ## 225            Antawn Jamison
    ## 226            Othyus Jeffers
    ## 227              Al Jefferson
    ## 228         Richard Jefferson
    ## 229            Jared Jeffries
    ## 230          Brandon Jennings
    ## 231              Eugene Jeter
    ## 232               Yi Jianlian
    ## 233              Amir Johnson
    ## 234             Armon Johnson
    ## 235             Chris Johnson
    ## 236             James Johnson
    ## 237               Joe Johnson
    ## 238              Trey Johnson
    ## 239            Wesley Johnson
    ## 240             Dahntay Jones
    ## 241           Dominique Jones
    ## 242               James Jones
    ## 243             Solomon Jones
    ## 244            DeAndre Jordan
    ## 245               Chris Kaman
    ## 246              Jason Kapono
    ## 247               Jason Kidd*
    ## 248          Andrei Kirilenko
    ## 249              Linas Kleiza
    ## 250               Kyle Korver
    ## 251              Kosta Koufos
    ## 252              Nenad Krstić
    ## 253               Carl Landry
    ## 254                  Acie Law
    ## 255                Gani Lawal
    ## 256                 Ty Lawson
    ## 257              Courtney Lee
    ## 258                 David Lee
    ## 259             Rashard Lewis
    ## 260                Jeremy Lin
    ## 261          Shaun Livingston
    ## 262               Brook Lopez
    ## 263               Robin Lopez
    ## 264                Kevin Love
    ## 265                Kyle Lowry
    ## 266            John Lucas III
    ## 267            Corey Maggette
    ## 268           Jamaal Magloire
    ## 269               Ian Mahinmi
    ## 270              Shawn Marion
    ## 271                Sean Marks
    ## 272            Cartier Martin
    ## 273             Kenyon Martin
    ## 274              Kevin Martin
    ## 275               Roger Mason
    ## 276           Wesley Matthews
    ## 277             Jason Maxiell
    ## 278               Eric Maynor
    ## 279                 O.J. Mayo
    ## 280          Luc Mbah a Moute
    ## 281           Antonio McDyess
    ## 282              JaVale McGee
    ## 283            Tracy McGrady*
    ## 284           Dominic McGuire
    ## 285            Josh McRoberts
    ## 286               Jodie Meeks
    ## 287         Pops Mensah-Bonsu
    ## 288                C.J. Miles
    ## 289             Darko Miličić
    ## 290              Andre Miller
    ## 291               Brad Miller
    ## 292               Mike Miller
    ## 293               Patty Mills
    ## 294              Paul Millsap
    ## 295                 Yao Ming*
    ## 296             Nazr Mohammed
    ## 297               Greg Monroe
    ## 298              Jamario Moon
    ## 299            Anthony Morrow
    ## 300            Timofey Mozgov
    ## 301             Byron Mullens
    ## 302               Troy Murphy
    ## 303            Hamady N'Diaye
    ## 304            Eduardo Nájera
    ## 305               Steve Nash*
    ## 306                 Gary Neal
    ## 307             Jameer Nelson
    ## 308               Joakim Noah
    ## 309            Andrés Nocioni
    ## 310               Steve Novak
    ## 311             Dirk Nowitzki
    ## 312           Jermaine O'Neal
    ## 313         Shaquille O'Neal*
    ## 314           Fabricio Oberto
    ## 315                Lamar Odom
    ## 316              Emeka Okafor
    ## 317               Mehmet Okur
    ## 318             Travis Outlaw
    ## 319               Larry Owens
    ## 320             Zaza Pachulia
    ## 321            Anthony Parker
    ## 322               Tony Parker
    ## 323         Patrick Patterson
    ## 324                Chris Paul
    ## 325            Sasha Pavlović
    ## 326            Nikola Peković
    ## 327          Kendrick Perkins
    ## 328           Morris Peterson
    ## 329               Johan Petro
    ## 330              Paul Pierce*
    ## 331           Mickaël Piétrus
    ## 332            Dexter Pittman
    ## 333          Quincy Pondexter
    ## 334               James Posey
    ## 335                 Leon Powe
    ## 336               Josh Powell
    ## 337                A.J. Price
    ## 338              Ronnie Price
    ## 339           Tayshaun Prince
    ## 340            Joel Przybilla
    ## 341               Chris Quinn
    ## 342       Vladimir Radmanović
    ## 343          Anthony Randolph
    ## 344             Zach Randolph
    ## 345              Theo Ratliff
    ## 346              Andy Rautins
    ## 347              Michael Redd
    ## 348               J.J. Redick
    ## 349          Jason Richardson
    ## 350        Quentin Richardson
    ## 351              Luke Ridnour
    ## 352             Nate Robinson
    ## 353               Rajon Rondo
    ## 354              Derrick Rose
    ## 355              Quinton Ross
    ## 356               Brandon Roy
    ## 357              Brandon Rush
    ## 358              John Salmons
    ## 359           Samardo Samuels
    ## 360             Larry Sanders
    ## 361          Brian Scalabrine
    ## 362                Luis Scola
    ## 363           Thabo Sefolosha
    ## 364            Kevin Séraphin
    ## 365            Ramon Sessions
    ## 366            Mustafa Shakur
    ## 367              Garret Siler
    ## 368             Bobby Simmons
    ## 369             Brian Skinner
    ## 370               Craig Smith
    ## 371                 Ish Smith
    ## 372                J.R. Smith
    ## 373               Jason Smith
    ## 374                 Joe Smith
    ## 375                Josh Smith
    ## 376           Darius Songaila
    ## 377         Marreese Speights
    ## 378            Tiago Splitter
    ## 379          Jerry Stackhouse
    ## 380          Lance Stephenson
    ## 381         DeShawn Stevenson
    ## 382           Peja Stojaković
    ## 383         Amar'e Stoudemire
    ## 384            Rodney Stuckey
    ## 385            DaJuan Summers
    ## 386                   Pape Sy
    ## 387           Jermaine Taylor
    ## 388               Jeff Teague
    ## 389         Sebastian Telfair
    ## 390            Garrett Temple
    ## 391               Jason Terry
    ## 392           Hasheem Thabeet
    ## 393               Etan Thomas
    ## 394               Kurt Thomas
    ## 395              Tyrus Thomas
    ## 396            Jason Thompson
    ## 397               Al Thornton
    ## 398           Marcus Thornton
    ## 399          Anthony Tolliver
    ## 400              Ronny Turiaf
    ## 401             Hedo Türkoğlu
    ## 402               Evan Turner
    ## 403                 Ekpe Udoh
    ## 404                 Ime Udoka
    ## 405                Beno Udrih
    ## 406                  Ben Uzoh
    ## 407          Anderson Varejão
    ## 408           Greivis Vásquez
    ## 409        Charlie Villanueva
    ## 410             Sasha Vujačić
    ## 411               Dwyane Wade
    ## 412                 Von Wafer
    ## 413              Henry Walker
    ## 414                 John Wall
    ## 415              Ben Wallace*
    ## 416            Gerald Wallace
    ## 417               Luke Walton
    ## 418             Willie Warren
    ## 419             Hakim Warrick
    ## 420               C.J. Watson
    ## 421               Earl Watson
    ## 422               Kyle Weaver
    ## 423           Martell Webster
    ## 424               Sonny Weems
    ## 425                David West
    ## 426              Delonte West
    ## 427                Mario West
    ## 428         Russell Westbrook
    ## 429                D.J. White
    ## 430          Hassan Whiteside
    ## 431              Chris Wilcox
    ## 432            Damien Wilkins
    ## 433            Deron Williams
    ## 434            Jason Williams
    ## 435            Jawad Williams
    ## 436              Lou Williams
    ## 437           Marvin Williams
    ## 438               Mo Williams
    ## 439           Reggie Williams
    ## 440           Shawne Williams
    ## 441          Shelden Williams
    ## 442         Terrence Williams
    ## 443         Metta World Peace
    ## 444            Antoine Wright
    ## 445            Brandan Wright
    ## 446             Dorell Wright
    ## 447             Julian Wright
    ## 448                Nick Young
    ## 449                 Sam Young
    ## 450            Thaddeus Young
    ## 451              Blake Ahearn
    ## 452               Lavoy Allen
    ## 453             Morris Almond
    ## 454             Alan Anderson
    ## 455            Gilbert Arenas
    ## 456              Trevor Ariza
    ## 457              Gustavo Ayón
    ## 458              Keith Benson
    ## 459           Bismack Biyombo
    ## 460            MarShon Brooks
    ## 461                Alec Burks
    ## 462              Jimmy Butler
    ## 463             Derrick Byars
    ## 464               Norris Cole
    ## 465                Eddy Curry
    ## 466                Josh Davis
    ## 467               Eric Dawson
    ## 468            Justin Dentmon
    ## 469              Jerome Dyson
    ## 470              Andre Emmett
    ## 471            Kenneth Faried
    ## 472                Jeff Foote
    ## 473          Courtney Fortson
    ## 474           Jimmer Fredette
    ## 475              Enes Freedom
    ## 476          Mickell Gladness
    ## 477          Andrew Goudelock
    ## 478              Gerald Green
    ## 479           Jordan Hamilton
    ## 480             Justin Harper
    ## 481           Josh Harrellson
    ## 482             Terrel Harris
    ## 483             Tobias Harris
    ## 484              Cory Higgins
    ## 485          Darington Hobson
    ## 486           Tyler Honeycutt
    ## 487             Dennis Horner
    ## 488              Larry Hughes
    ## 489              Kyrie Irving
    ## 490            Reggie Jackson
    ## 491                Mike James
    ## 492           Charles Jenkins
    ## 493             Jonas Jerebko
    ## 494          Carldell Johnson
    ## 495              Ivan Johnson
    ## 496            JaJuan Johnson
    ## 497             Jerome Jordan
    ## 498               Cory Joseph
    ## 499              D.J. Kennedy
    ## 500            Brandon Knight
    ## 501               Malcolm Lee
    ## 502             Kawhi Leonard
    ## 503             Travis Leslie
    ## 504                 Jon Leuer
    ## 505           DeAndre Liggins
    ## 506              Shelvin Mack
    ## 507            Vernon Macklin
    ## 508             E'Twaun Moore
    ## 509               Mikki Moore
    ## 510             Darius Morris
    ## 511             Marcus Morris
    ## 512           Markieff Morris
    ## 513              Daniel Orton
    ## 514             Jannero Pargo
    ## 515              Jeremy Pargo
    ## 516          Chandler Parsons
    ## 517                 Ryan Reid
    ## 518               Ricky Rubio
    ## 519            Walker Russell
    ## 520                Josh Selby
    ## 521             Iman Shumpert
    ## 522              Xavier Silas
    ## 523           Chris Singleton
    ## 524           James Singleton
    ## 525              Donald Sloan
    ## 526                Greg Smith
    ## 527               Jerry Smith
    ## 528               Nolan Smith
    ## 529             Greg Stiemsma
    ## 530              Julyan Stone
    ## 531             Isaiah Thomas
    ## 532              Lance Thomas
    ## 533            Malcolm Thomas
    ## 534            Trey Thompkins
    ## 535             Klay Thompson
    ## 536           Mychel Thompson
    ## 537          Tristan Thompson
    ## 538            Jamaal Tinsley
    ## 539              Jeremy Tyler
    ## 540              Edwin Ubiles
    ## 541                Jan Veselý
    ## 542            Nikola Vučević
    ## 543              Kemba Walker
    ## 544            Darryl Watkins
    ## 545          Derrick Williams
    ## 546           Elliot Williams
    ## 547           Jordan Williams
    ## 548             Sean Williams
    ## 549              Chris Wright
    ## 550                Quincy Acy
    ## 551              Josh Akognon
    ## 552                Jeff Ayres
    ## 553           Harrison Barnes
    ## 554               Will Barton
    ## 555               Aron Baynes
    ## 556             Kent Bazemore
    ## 557              Bradley Beal
    ## 558          Patrick Beverley
    ## 559             Victor Claver
    ## 560               Will Conroy
    ## 561            Chris Copeland
    ## 562               Jae Crowder
    ## 563          Jared Cunningham
    ## 564             Anthony Davis
    ## 565             Nando De Colo
    ## 566            Andre Drummond
    ## 567               Kim English
    ## 568              Festus Ezeli
    ## 569             Evan Fournier
    ## 570             Joel Freeland
    ## 571            Diante Garrett
    ## 572          Mickaël Gelabale
    ## 573            Draymond Green
    ## 574            Ben Hansbrough
    ## 575          Maurice Harkless
    ## 576               John Henson
    ## 577            Justin Holiday
    ## 578             Bernard James
    ## 579              John Jenkins
    ## 580           Orlando Johnson
    ## 581       Darius Johnson-Odom
    ## 582              DeQuan Jones
    ## 583               Kevin Jones
    ## 584               Perry Jones
    ## 585            Terrence Jones
    ## 586               Kris Joseph
    ## 587    Michael Kidd-Gilchrist
    ## 588       Viacheslav Kravtsov
    ## 589                Doron Lamb
    ## 590               Jeremy Lamb
    ## 591            Meyers Leonard
    ## 592            Damian Lillard
    ## 593             Scott Machado
    ## 594          Kendall Marshall
    ## 595                  Fab Melo
    ## 596           Khris Middleton
    ## 597             Darius Miller
    ## 598             Quincy Miller
    ## 599        Donatas Motiejūnas
    ## 600           Arnett Moultrie
    ## 601              Kevin Murphy
    ## 602          Andrew Nicholson
    ## 603              Kyle O'Quinn
    ## 604             Tim Ohlbrecht
    ## 605             Miles Plumlee
    ## 606            Pablo Prigioni
    ## 607          Shavlik Randolph
    ## 608             Austin Rivers
    ## 609             Brian Roberts
    ## 610           Thomas Robinson
    ## 611             Terrence Ross
    ## 612              Robert Sacre
    ## 613                Mike Scott
    ## 614         Tornike Shengelia
    ## 615              Alexey Shved
    ## 616                Henry Sims
    ## 617              Kyle Singler
    ## 618           Jared Sullinger
    ## 619               Jeff Taylor
    ## 620            Tyshawn Taylor
    ## 621            Marquis Teague
    ## 622           Mirza Teletović
    ## 623               P.J. Tucker
    ## 624         Jonas Valančiūnas
    ## 625            Jarvis Varnado
    ## 626              Dion Waiters
    ## 627           Rasheed Wallace
    ## 628              Maalik Wayns
    ## 629               James White
    ## 630               Tony Wroten
    ## 631               Luke Zeller
    ## 632              Tyler Zeller
    ## 633              Steven Adams
    ## 634                Chris Babb
    ## 635           Anthony Bennett
    ## 636               Vander Blue
    ## 637             Lorenzo Brown
    ## 638            Reggie Bullock
    ## 639                Trey Burke
    ## 640             Dwight Buycks
    ## 641             Nick Calathes
    ## 642  Kentavious Caldwell-Pope
    ## 643             Isaiah Canaan
    ## 644   Michael Carter-Williams
    ## 645          Dionte Christmas
    ## 646                 Ian Clark
    ## 647          Robert Covington
    ## 648              Allen Crabbe
    ## 649                Seth Curry
    ## 650              Troy Daniels
    ## 651               Gigi Datome
    ## 652            Brandon Davies
    ## 653            Dewayne Dedmon
    ## 654       Matthew Dellavedova
    ## 655              Gorgui Dieng
    ## 656             Shane Edwards
    ## 657       Vítor Luiz Faverani
    ## 658             Carrick Felix
    ## 659           Jamaal Franklin
    ## 660               Rudy Gobert
    ## 661            Archie Goodwin
    ## 662           Jorge Gutiérrez
    ## 663           Justin Hamilton
    ## 664          Tim Hardaway Jr.
    ## 665              Elias Harris
    ## 666              Solomon Hill
    ## 667             Scotty Hopson
    ## 668             Robbie Hummel
    ## 669            Sergey Karasev
    ## 670                Ryan Kelly
    ## 671             Ognjen Kuzmić
    ## 672              Shane Larkin
    ## 673                Ricky Ledo
    ## 674                  Alex Len
    ## 675              Ray McCallum
    ## 676               CJ McCollum
    ## 677              Ben McLemore
    ## 678                 Gal Mekel
    ## 679             Tony Mitchell
    ## 680          Shabazz Muhammad
    ## 681               Erik Murphy
    ## 682              Toure' Murry
    ## 683              Mike Muscala
    ## 684           Nemanja Nedović
    ## 685            James Nunnally
    ## 686                 Greg Oden
    ## 687            Victor Oladipo
    ## 688              Kelly Olynyk
    ## 689             Arinze Onuaku
    ## 690             Mason Plumlee
    ## 691           Otto Porter Jr.
    ## 692              Phil Pressey
    ## 693        Miroslav Raduljica
    ## 694             Glen Rice Jr.
    ## 695            Andre Roberson
    ## 696           Dennis Schröder
    ## 697               Peyton Siva
    ## 698               Chris Smith
    ## 699                Tony Snell
    ## 700         James Southerland
    ## 701             D.J. Stephens
    ## 702             Adonis Thomas
    ## 703           Hollis Thompson
    ## 704               Casper Ware
    ## 705               Royce White
    ## 706               Jeff Withey
    ## 707              Nate Wolters
    ## 708               Cody Zeller
    ## 709              Jordan Adams
    ## 710            Furkan Aldemir
    ## 711             Kyle Anderson
    ## 712     Giannis Antetokounmpo
    ## 713          Jerrelle Benimon
    ## 714               Sim Bhullar
    ## 715               Tarik Black
    ## 716          Bojan Bogdanović
    ## 717              Jabari Brown
    ## 718              Markel Brown
    ## 719             Bruno Caboclo
    ## 720              Clint Capela
    ## 721               Will Cherry
    ## 722       Patrick Christopher
    ## 723           Jordan Clarkson
    ## 724               Jack Cooley
    ## 725              Bryce Cotton
    ## 726             Andre Dawkins
    ## 727         Spencer Dinwiddie
    ## 728              Zoran Dragić
    ## 729             Larry Drew II
    ## 730          Cleanthony Early
    ## 731           James Ennis III
    ## 732               Tyler Ennis
    ## 733                Dante Exum
    ## 734               Tim Frazier
    ## 735         Langston Galloway
    ## 736              Aaron Gordon
    ## 737               Drew Gordon
    ## 738              Jerami Grant
    ## 739               Erick Green
    ## 740            JaMychal Green
    ## 741             P.J. Hairston
    ## 742               Gary Harris
    ## 743                Joe Harris
    ## 744               Rodney Hood
    ## 745                Joe Ingles
    ## 746            Cory Jefferson
    ## 747             Grant Jerrett
    ## 748              Nick Johnson
    ## 749             Tyler Johnson
    ## 750           Sean Kilpatrick
    ## 751                 Alex Kirk
    ## 752         Joffrey Lauvergne
    ## 753               Zach LaVine
    ## 754               Kalin Lucas
    ## 755              Devyn Marble
    ## 756      James Michael McAdoo
    ## 757            K.J. McDaniels
    ## 758            Doug McDermott
    ## 759              Mitch McGary
    ## 760              Jerel McNeal
    ## 761            Elijah Millsap
    ## 762            Nikola Mirotić
    ## 763             Eric Moreland
    ## 764            Shabazz Napier
    ## 765              Nerlens Noel
    ## 766            Lucas Nogueira
    ## 767              Jusuf Nurkić
    ## 768           Johnny O'Bryant
    ## 769       Kostas Papanikolaou
    ## 770             Jabari Parker
    ## 771             Adreian Payne
    ## 772             Elfrid Payton
    ## 773             Dwight Powell
    ## 774             Julius Randle
    ## 775        Glenn Robinson III
    ## 776              Damjan Rudež
    ## 777            JaKarr Sampson
    ## 778              Marcus Smart
    ## 779                Russ Smith
    ## 780              Nik Stauskas
    ## 781            David Stockton
    ## 782            Jarnell Stokes
    ## 783               Noah Vonleh
    ## 784               T.J. Warren
    ## 785                David Wear
    ## 786               Travis Wear
    ## 787        Shayne Whittington
    ## 788            Andrew Wiggins
    ## 789               C.J. Wilcox
    ## 790               James Young
    ## 791           Cliff Alexander
    ## 792           Justin Anderson
    ## 793    Thanasis Antetokounmpo
    ## 794           Nemanja Bjelica
    ## 795              Devin Booker
    ## 796             Anthony Brown
    ## 797       Willie Cauley-Stein
    ## 798          Rakeem Christmas
    ## 799               Coty Clarke
    ## 800           Pat Connaughton
    ## 801            Branden Dawson
    ## 802        Bryce Dejean-Jones
    ## 803                Sam Dekker
    ## 804                Duje Dukan
    ## 805              Jarell Eddie
    ## 806         Cristiano Felício
    ## 807              Jerian Grant
    ## 808          Montrezl Harrell
    ## 809            Aaron Harrison
    ## 810             Mario Hezonja
    ## 811           Darrun Hilliard
    ## 812   Rondae Hollis-Jefferson
    ## 813            Richaun Holmes
    ## 814           Marcelo Huertas
    ## 815              Josh Huestis
    ## 816               R.J. Hunter
    ## 817             Damien Inglis
    ## 818           Stanley Johnson
    ## 819              Nikola Jokić
    ## 820                Tyus Jones
    ## 821            Frank Kaminsky
    ## 822                Sasha Kaun
    ## 823              Kevon Looney
    ## 824                Trey Lyles
    ## 825          Boban Marjanović
    ## 826             Jarell Martin
    ## 827            T.J. McConnell
    ## 828          Chris McCullough
    ## 829              Jordan McRae
    ## 830               Salah Mejri
    ## 831             Jordan Mickey
    ## 832              Luis Montero
    ## 833           Emmanuel Mudiay
    ## 834            Xavier Munford
    ## 835           Larry Nance Jr.
    ## 836                 Raul Neto
    ## 837              J.J. O'Brien
    ## 838             Jahlil Okafor
    ## 839           Kelly Oubre Jr.
    ## 840           Lamar Patterson
    ## 841             Cameron Payne
    ## 842               Tibor Pleiß
    ## 843              Bobby Portis
    ## 844        Kristaps Porziņģis
    ## 845             Norman Powell
    ## 846               Willie Reed
    ## 847           Josh Richardson
    ## 848              Terry Rozier
    ## 849          D'Angelo Russell
    ## 850          Jonathon Simmons
    ## 851            Alex Stepheson
    ## 852               Edy Tavares
    ## 853              Axel Toupane
    ## 854        Karl-Anthony Towns
    ## 855              Myles Turner
    ## 856             Rashad Vaughn
    ## 857             Briante Weber
    ## 858             Alan Williams
    ## 859           Justise Winslow
    ## 860            Christian Wood
    ## 861              Delon Wright
    ## 862                 Joe Young
    ## 863              Álex Abrines
    ## 864             Malik Beasley
    ## 865           DeAndre' Bembry
    ## 866             Dragan Bender
    ## 867                Ben Bentil
    ## 868             Dāvis Bertāns
    ## 869             Joel Bolomboy
    ## 870           Malcolm Brogdon
    ## 871               Bobby Brown
    ## 872              Jaylen Brown
    ## 873          Nicolás Brussino
    ## 874           Marquese Chriss
    ## 875            Semaj Christon
    ## 876                Quinn Cook
    ## 877             Deyonta Davis
    ## 878           Malcolm Delaney
    ## 879             Cheick Diallo
    ## 880                 Kris Dunn
    ## 881            Henry Ellenson
    ## 882               Joel Embiid
    ## 883                Kay Felder
    ## 884              Yogi Ferrell
    ## 885       Dorian Finney-Smith
    ## 886               Bryn Forbes
    ## 887           Patricio Garino
    ## 888           Michael Gbinije
    ## 889       Marcus Georges-Hunt
    ## 890           Jonathan Gibson
    ## 891            Treveon Graham
    ## 892              A.J. Hammons
    ## 893           Andrew Harrison
    ## 894       Juancho Hernangómez
    ## 895         Willy Hernangómez
    ## 896               Buddy Hield
    ## 897          Danuel House Jr.
    ## 898            Brandon Ingram
    ## 899         Demetrius Jackson
    ## 900            Pierre Jackson
    ## 901             Brice Johnson
    ## 902              Damian Jones
    ## 903         Derrick Jones Jr.
    ## 904      Mindaugas Kuzminskas
    ## 905           Skal Labissière
    ## 906      Nicolás Laprovíttola
    ## 907               Jake Layman
    ## 908              Caris LeVert
    ## 909                Shawn Long
    ## 910   Timothé Luwawu-Cabarrot
    ## 911               Sheldon Mac
    ## 912                Thon Maker
    ## 913             Patrick McCaw
    ## 914           Rodney McGruder
    ## 915           Dejounte Murray
    ## 916              Jamal Murray
    ## 917             Maurice Ndour
    ## 918             Georges Niang
    ## 919               David Nwaba
    ## 920             Daniel Ochefu
    ## 921            Chinanu Onuaku
    ## 922      Georgios Papagiannis
    ## 923            Gary Payton II
    ## 924          Marshall Plumlee
    ## 925              Jakob Poeltl
    ## 926            Alex Poythress
    ## 927            Taurean Prince
    ## 928            Tim Quarterman
    ## 929            Chasson Randle
    ## 930        Malachi Richardson
    ## 931          Sergio Rodríguez
    ## 932          Domantas Sabonis
    ## 933               Dario Šarić
    ## 934          Tomáš Satoranský
    ## 935              Wayne Selden
    ## 936             Pascal Siakam
    ## 937             Diamond Stone
    ## 938             Isaiah Taylor
    ## 939                Mike Tobey
    ## 940                Tyler Ulis
    ## 941             Jarrod Uthoff
    ## 942          Denzel Valentine
    ## 943             Fred VanVleet
    ## 944               Okaro White
    ## 945          Isaiah Whitehead
    ## 946             Troy Williams
    ## 947              Kyle Wiltjer
    ## 948         Stephen Zimmerman
    ## 949               Paul Zipser
    ## 950               Ivica Zubac
    ## 951               Bam Adebayo
    ## 952             Jarrett Allen
    ## 953              Kadeem Allen
    ## 954              Ike Anigbogu
    ## 955                OG Anunoby
    ## 956          Ryan Arcidiacono
    ## 957                 Ron Baker
    ## 958              Wade Baldwin
    ## 959                Lonzo Ball
    ## 960               Jordan Bell
    ## 961                Khem Birch
    ## 962               Jabari Bird
    ## 963          Antonio Blakeney
    ## 964         Bogdan Bogdanović
    ## 965             Chris Boucher
    ## 966              Tony Bradley
    ## 967             Dillon Brooks
    ## 968            Sterling Brown
    ## 969             Thomas Bryant
    ## 970               Alex Caruso
    ## 971           Tyler Cavanaugh
    ## 972              Gian Clavell
    ## 973        Antonius Cleveland
    ## 974              John Collins
    ## 975              Zach Collins
    ## 976         Kyle Collinsworth
    ## 977             Charles Cooke
    ## 978             Matt Costello
    ## 979              Torrey Craig
    ## 980              Tyler Dorsey
    ## 981            Damyean Dotson
    ## 982              Milton Doyle
    ## 983                 PJ Dozier
    ## 984               Jawun Evans
    ## 985         Terrance Ferguson
    ## 986              De'Aaron Fox
    ## 987            Markelle Fultz
    ## 988                 Josh Gray
    ## 989           Daniel Hamilton
    ## 990        Shaquille Harrison
    ## 991                 Josh Hart
    ## 992               Nigel Hayes
    ## 993              Reggie Hearn
    ## 994                Myke Henry
    ## 995              Isaiah Hicks
    ## 996              John Holland
    ## 997              Vince Hunter
    ## 998              Andre Ingram
    ## 999            Jonathan Isaac
    ## 1000               Wes Iwundu
    ## 1001            Aaron Jackson
    ## 1002             Josh Jackson
    ## 1003           Justin Jackson
    ## 1004           Dakari Johnson
    ## 1005            Omari Johnson
    ## 1006              Jalen Jones
    ## 1007             Luke Kennard
    ## 1008              Maxi Kleber
    ## 1009           Furkan Korkmaz
    ## 1010              Luke Kornet
    ## 1011               Kyle Kuzma
    ## 1012                T.J. Leaf
    ## 1013               Damion Lee
    ## 1014           Walt Lemon Jr.
    ## 1015              Tyler Lydon
    ## 1016             Josh Magette
    ## 1017          Lauri Markkanen
    ## 1018          Frank Mason III
    ## 1019          Mangok Mathiang
    ## 1020              Erik McCree
    ## 1021      Trey McKinney-Jones
    ## 1022         Alfonzo McKinnie
    ## 1023           Malcolm Miller
    ## 1024         Donovan Mitchell
    ## 1025          Naz Mitrou-Long
    ## 1026               Malik Monk
    ## 1027                Ben Moore
    ## 1028            Jaylen Morris
    ## 1029             Monte Morris
    ## 1030         Johnathan Motley
    ## 1031              Abdel Nader
    ## 1032          Frank Ntilikina
    ## 1033            Royce O'Neale
    ## 1034             Semi Ojeleye
    ## 1035               Cedi Osman
    ## 1036             Marcus Paige
    ## 1037            Justin Patton
    ## 1038             Brandon Paul
    ## 1039         London Perrantes
    ## 1040              Alec Peters
    ## 1041             Jacob Pullen
    ## 1042            Rodney Purvis
    ## 1043                  Zhou Qi
    ## 1044                Ivan Rabb
    ## 1045      Xavier Rathan-Mayes
    ## 1046               Davon Reed
    ## 1047           Devin Robinson
    ## 1048              Ben Simmons
    ## 1049             Kobi Simmons
    ## 1050         Dennis Smith Jr.
    ## 1051            Edmond Sumner
    ## 1052           Caleb Swanigan
    ## 1053             Jayson Tatum
    ## 1054           Miloš Teodosić
    ## 1055             Daniel Theis
    ## 1056      Sindarius Thornwell
    ## 1057           Tyrone Wallace
    ## 1058           Derrick Walton
    ## 1059            Jameel Warney
    ## 1060           James Webb III
    ## 1061             Andrew White
    ## 1062            Derrick White
    ## 1063              Jacob Wiley
    ## 1064            C.J. Williams
    ## 1065            Matt Williams
    ## 1066              D.J. Wilson
    ## 1067             Jamil Wilson
    ## 1068       Guerschon Yabusele
    ## 1069               Ante Žižić
    ## 1070             Jaylen Adams
    ## 1071                Deng Adel
    ## 1072   DeVaughn Akoon-Purcell
    ## 1073             Rawle Alkins
    ## 1074            Grayson Allen
    ## 1075        Marvin Bagley III
    ## 1076                 Mo Bamba
    ## 1077         Keita Bates-Diop
    ## 1078           Dairis Bertāns
    ## 1079        Jaron Blossomgame
    ## 1080             Jonah Bolden
    ## 1081              Isaac Bonga
    ## 1082            Mikal Bridges
    ## 1083            Miles Bridges
    ## 1084           Isaiah Briscoe
    ## 1085           Ryan Broekhoff
    ## 1086              Bruce Brown
    ## 1087           Troy Brown Jr.
    ## 1088            Jalen Brunson
    ## 1089            Deonte Burton
    ## 1090             Jevon Carter
    ## 1091       Wendell Carter Jr.
    ## 1092             Troy Caupain
    ## 1093              Joe Chealey
    ## 1094            Chris Chiozza
    ## 1095               Gary Clark
    ## 1096            Bonzie Colson
    ## 1097              Mitch Creek
    ## 1098              Tyler Davis
    ## 1099            Ángel Delgado
    ## 1100        Marcus Derrickson
    ## 1101           Hamidou Diallo
    ## 1102         Donte DiVincenzo
    ## 1103              Luka Dončić
    ## 1104             Trevon Duval
    ## 1105            Vince Edwards
    ## 1106             Drew Eubanks
    ## 1107              Jacob Evans
    ## 1108           Melvin Frazier
    ## 1109            Billy Garrett
    ## 1110              Harry Giles
    ## 1111  Shai Gilgeous-Alexander
    ## 1112          Brandon Goodwin
    ## 1113          Devonte' Graham
    ## 1114           Donte Grantham
    ## 1115            Dusty Hannahs
    ## 1116       Isaiah Hartenstein
    ## 1117        Haywood Highsmith
    ## 1118            Aaron Holiday
    ## 1119            Kevin Huerter
    ## 1120          Isaac Humphries
    ## 1121       Chandler Hutchison
    ## 1122            Frank Jackson
    ## 1123        Jaren Jackson Jr.
    ## 1124          Amile Jefferson
    ## 1125            Alize Johnson
    ## 1126             B.J. Johnson
    ## 1127           Jemerrio Jones
    ## 1128              George King
    ## 1129               Kevin Knox
    ## 1130           Rodions Kurucs
    ## 1131              Zach Lofton
    ## 1132              Jordan Loyd
    ## 1133              Daryl Macon
    ## 1134              J.P. Macura
    ## 1135              Yante Maten
    ## 1136           Tahjere McCall
    ## 1137        De'Anthony Melton
    ## 1138            Chimezie Metu
    ## 1139             Shake Milton
    ## 1140              Džanan Musa
    ## 1141           Svi Mykhailiuk
    ## 1142               Elie Okobo
    ## 1143              Josh Okogie
    ## 1144              Theo Pinson
    ## 1145         Cameron Reynolds
    ## 1146          Duncan Robinson
    ## 1147          Jerome Robinson
    ## 1148        Mitchell Robinson
    ## 1149          Brandon Sampson
    ## 1150            Collin Sexton
    ## 1151            Landry Shamet
    ## 1152            Jordan Sibert
    ## 1153          Anfernee Simons
    ## 1154             Zhaire Smith
    ## 1155             Ray Spalding
    ## 1156           Omari Spellman
    ## 1157            Jared Terrell
    ## 1158            Emanuel Terry
    ## 1159             Khyri Thomas
    ## 1160           Gary Trent Jr.
    ## 1161            Allonzo Trier
    ## 1162        Jarred Vanderbilt
    ## 1163            Moritz Wagner
    ## 1164         Lonnie Walker IV
    ## 1165           Brad Wanamaker
    ## 1166          Julian Washburn
    ## 1167            Yuta Watanabe
    ## 1168             Thomas Welsh
    ## 1169       Johnathan Williams
    ## 1170         Kenrich Williams
    ## 1171          Robert Williams
    ## 1172               Trae Young
    ## 1173           Kyle Alexander
    ## 1174 Nickeil Alexander-Walker
    ## 1175     Kostas Antetokounmpo
    ## 1176            Deandre Ayton
    ## 1177            Darius Bazley
    ## 1178             Goga Bitadze
    ## 1179                  Bol Bol
    ## 1180           Marques Bolden
    ## 1181              Jordan Bone
    ## 1182              Brian Bowen
    ## 1183                Ky Bowman
    ## 1184         Jarrell Brantley
    ## 1185         Ignas Brazdeikis
    ## 1186           Oshae Brissett
    ## 1187        Charlie Brown Jr.
    ## 1188              Moses Brown
    ## 1189           Devontae Cacok
    ## 1190            Vlatko Čančar
    ## 1191           Zylan Cheatham
    ## 1192           Brandon Clarke
    ## 1193              Nic Claxton
    ## 1194            Chris Clemons
    ## 1195              Amir Coffey
    ## 1196               Tyler Cook
    ## 1197           Jarrett Culver
    ## 1198            Terence Davis
    ## 1199            Luguentz Dort
    ## 1200          Sekou Doumbouya
    ## 1201           Carsen Edwards
    ## 1202               Tacko Fall
    ## 1203           Bruno Fernando
    ## 1204          Michael Frazier
    ## 1205           Wenyen Gabriel
    ## 1206           Daniel Gafford
    ## 1207           Darius Garland
    ## 1208            Javonte Green
    ## 1209            Marko Guduric
    ## 1210                 Kyle Guy
    ## 1211            Rui Hachimura
    ## 1212               Devon Hall
    ## 1213               Donta Hall
    ## 1214             Jared Harper
    ## 1215             Jaxson Hayes
    ## 1216          Dewan Hernandez
    ## 1217              Tyler Herro
    ## 1218             Kevin Hervey
    ## 1219             Jaylen Hoard
    ## 1220      Talen Horton-Tucker
    ## 1221           William Howard
    ## 1222          De'Andre Hunter
    ## 1223             Justin James
    ## 1224          DaQuan Jeffries
    ## 1225                Ty Jerome
    ## 1226          Cameron Johnson
    ## 1227           Keldon Johnson
    ## 1228        Mfiondu Kabengele
    ## 1229             Stanton Kidd
    ## 1230               Louis King
    ## 1231             John Konchar
    ## 1232           Romeo Langford
    ## 1233                  Vic Law
    ## 1234             Jalen Lecque
    ## 1235            Nassir Little
    ## 1236             Terance Mann
    ## 1237             Caleb Martin
    ## 1238              Cody Martin
    ## 1239          Jeremiah Martin
    ## 1240             Kelan Martin
    ## 1241         Garrison Mathews
    ## 1242          Jalen McDaniels
    ## 1243        Jordan McLaughlin
    ## 1244             Nicolò Melli
    ## 1245                Eric Mika
    ## 1246              Adam Mokoka
    ## 1247              Matt Mooney
    ## 1248                Ja Morant
    ## 1249             Juwan Morgan
    ## 1250            Mychal Mulder
    ## 1251             Malik Newman
    ## 1252             Zach Norvell
    ## 1253            Jaylen Nowell
    ## 1254            Kendrick Nunn
    ## 1255                KZ Okpala
    ## 1256                 Miye Oni
    ## 1257              Tariq Owens
    ## 1258            Eric Paschall
    ## 1259         Anžejs Pasečņiks
    ## 1260             Norvel Pelle
    ## 1261          Vincent Poirier
    ## 1262           Shamorie Ponds
    ## 1263             Jordan Poole
    ## 1264         Kevin Porter Jr.
    ## 1265       Michael Porter Jr.
    ## 1266              Josh Reaves
    ## 1267              Cam Reddish
    ## 1268                 Naz Reid
    ## 1269          Justin Robinson
    ## 1270              Isaiah Roby
    ## 1271             Luka Šamanić
    ## 1272        Admiral Schofield
    ## 1273            Marial Shayok
    ## 1274              Chris Silva
    ## 1275           Alen Smailagić
    ## 1276                Max Strus
    ## 1277              Matt Thomas
    ## 1278         Matisse Thybulle
    ## 1279    Juan Toscano-Anderson
    ## 1280            Rayjon Tucker
    ## 1281             Gabe Vincent
    ## 1282                Dean Wade
    ## 1283          P.J. Washington
    ## 1284           Tremont Waters
    ## 1285              Paul Watson
    ## 1286   Quinndary Weatherspoon
    ## 1287               Coby White
    ## 1288           Grant Williams
    ## 1289      Nigel Williams-Goss
    ## 1290          Zion Williamson
    ## 1291    Justin Wright-Foreman
    ## 1292         Precious Achiuwa
    ## 1293        Ty-Shon Alexander
    ## 1294             Cole Anthony
    ## 1295           Udoka Azubuike
    ## 1296             Dwayne Bacon
    ## 1297             Desmond Bane
    ## 1298               RJ Barrett
    ## 1299               Saddiq Bey
    ## 1300                Tyler Bey
    ## 1301           Keljin Blevins
    ## 1302             Amida Brimah
    ## 1303            Armoni Brooks
    ## 1304            Elijah Bryant
    ## 1305         Facundo Campazzo
    ## 1306            Devin Cannady
    ## 1307         Vernon Carey Jr.
    ## 1308             Nate Darling
    ## 1309             Gabriel Deck
    ## 1310           Mamadi Diakite
    ## 1311             Devon Dotson
    ## 1312          Anthony Edwards
    ## 1313                CJ Elleby
    ## 1314              Malik Fitts
    ## 1315            Malachi Flynn
    ## 1316            Trent Forrest
    ## 1317            Robert Franks
    ## 1318             Anthony Gill
    ## 1319        Freddie Gillespie
    ## 1320               Josh Green
    ## 1321            Ashton Hagans
    ## 1322        Tyrese Haliburton
    ## 1323                Josh Hall
    ## 1324             R.J. Hampton
    ## 1325             Jalen Harris
    ## 1326            Killian Hayes
    ## 1327              Nate Hinton
    ## 1328            Markus Howard
    ## 1329            Elijah Hughes
    ## 1330               Isaiah Joe
    ## 1331              Mason Jones
    ## 1332                Tre Jones
    ## 1333            Nathan Knight
    ## 1334             Anthony Lamb
    ## 1335                Saben Lee
    ## 1336           Kira Lewis Jr.
    ## 1337             Didi Louzada
    ## 1338              Will Magnay
    ## 1339             Théo Maledon
    ## 1340               Karim Mané
    ## 1341             Nico Mannion
    ## 1342            Naji Marshall
    ## 1343        Kenyon Martin Jr.
    ## 1344           Dakota Mathias
    ## 1345             Tyrese Maxey
    ## 1346              Skylar Mays
    ## 1347          Jaden McDaniels
    ## 1348           Sean McDermott
    ## 1349              Sam Merrill
    ## 1350            Aaron Nesmith
    ## 1351               Zeke Nnaji
    ## 1352             Jordan Nwora
    ## 1353              Chuma Okeke
    ## 1354           Onyeka Okongwu
    ## 1355              Isaac Okoro
    ## 1356           Cameron Oliver
    ## 1357             Daniel Oturu
    ## 1358             Reggie Perry
    ## 1359       Aleksej Pokusevski
    ## 1360            Jontay Porter
    ## 1361         Payton Pritchard
    ## 1362        Immanuel Quickley
    ## 1363          Jahmi'us Ramsey
    ## 1364                Paul Reed
    ## 1365            Nick Richards
    ## 1366             Grant Riller
    ## 1367               Jay Scrubb
    ## 1368        Deividas Sirvydis
    ## 1369              Jalen Smith
    ## 1370          Cassius Stanley
    ## 1371            Lamar Stevens
    ## 1372           Isaiah Stewart
    ## 1373            Jae'Sean Tate
    ## 1374             Tyrell Terry
    ## 1375           Brodric Thomas
    ## 1376           Killian Tillie
    ## 1377       Xavier Tillman Sr.
    ## 1378               Obi Toppin
    ## 1379            Devin Vassell
    ## 1380         Greg Whittington
    ## 1381         Patrick Williams
    ## 1382            Dylan Windler
    ## 1383          Cassius Winston
    ## 1384            James Wiseman
    ## 1385        Robert Woodard II
    ## 1386             Santi Aldama
    ## 1387            Jose Alvarado
    ## 1388              LaMelo Ball
    ## 1389            Dalano Banton
    ## 1390               Cat Barber
    ## 1391           Scottie Barnes
    ## 1392               Paris Bass
    ## 1393           Charles Bassey
    ## 1394          Leandro Bolmaro
    ## 1395       Brandon Boston Jr.
    ## 1396          James Bouknight
    ## 1397       Chaundee Brown Jr.
    ## 1398           Greg Brown III
    ## 1399            Shaq Buchanan
    ## 1400             Jared Butler
    ## 1401              Ahmad Caver
    ## 1402        Justin Champagnie
    ## 1403         Josh Christopher
    ## 1404           Sharife Cooper
    ## 1405            Petr Cornelie
    ## 1406        Jarron Cumberland
    ## 1407          Cade Cunningham
    ## 1408          Javin DeLaurier
    ## 1409              Ayo Dosunmu
    ## 1410              Jeff Dowtin
    ## 1411             Chris Duarte
    ## 1412           David Duke Jr.
    ## 1413          Jaime Echenique
    ## 1414          Kessler Edwards
    ## 1415              Rob Edwards
    ## 1416               Aleem Ford
    ## 1417           Marcus Garrett
    ## 1418             Usman Garuba
    ## 1419               Luka Garza
    ## 1420              Josh Giddey
    ## 1421           Jordan Goodwin
    ## 1422          Hassani Gravett
    ## 1423              Jalen Green
    ## 1424           Quentin Grimes
    ## 1425               Tyler Hall
    ## 1426               Sam Hauser
    ## 1427              Aaron Henry
    ## 1428             Malcolm Hill
    ## 1429                 Jay Huff
    ## 1430               Feron Hunt
    ## 1431             Bones Hyland
    ## 1432           Isaiah Jackson
    ## 1433            DeJon Jarreau
    ## 1434            David Johnson
    ## 1435            Jalen Johnson
    ## 1436             Keon Johnson
    ## 1437             Carlik Jones
    ## 1438            Herbert Jones
    ## 1439                Kai Jones
    ## 1440     Georgios Kalaitzakis
    ## 1441              Braxton Key
    ## 1442            Corey Kispert
    ## 1443               Vit Krejci
    ## 1444         Arnoldas Kulboka
    ## 1445         Jonathan Kuminga
    ## 1446             Jock Landale
    ## 1447            Scottie Lewis
    ## 1448            Isaiah Livers
    ## 1449         Gabriel Lundberg
    ## 1450    Sandro Mamukelashvili
    ## 1451                 Tre Mann
    ## 1452            Miles McBride
    ## 1453              Mac McClung
    ## 1454          Cameron McGriff
    ## 1455       JaQuori McLaughlin
    ## 1456          Davion Mitchell
    ## 1457              Evan Mobley
    ## 1458              Moses Moody
    ## 1459              Xavier Moon
    ## 1460               Ade Murkey
    ## 1461          Trey Murphy III
    ## 1462          RJ Nembhard Jr.
    ## 1463              Daishen Nix
    ## 1464           Eugene Omoruyi
    ## 1465            Jaysean Paige
    ## 1466           Trayvon Palmer
    ## 1467             Kevin Pangos
    ## 1468          Jamorko Pickett
    ## 1469                Yves Pons
    ## 1470             Micah Potter
    ## 1471             Myles Powell
    ## 1472             Joshua Primo
    ## 1473           Trevelin Queen
    ## 1474            Neemias Queta
    ## 1475            Austin Reaves
    ## 1476   Jeremiah Robinson-Earl
    ## 1477                Matt Ryan
    ## 1478             Olivier Sarr
    ## 1479           Jordan Schakel
    ## 1480                Tre Scott
    ## 1481           Alperen Şengün
    ## 1482           Day'Ron Sharpe
    ## 1483          Marko Simonovic
    ## 1484           Zavier Simpson
    ## 1485             Jericho Sims
    ## 1486            Javonte Smart
    ## 1487             Xavier Sneed
    ## 1488           Jaden Springer
    ## 1489              Jalen Suggs
    ## 1490              Craig Sword
    ## 1491             Keifer Sykes
    ## 1492             Terry Taylor
    ## 1493                Jon Teske
    ## 1494               Cam Thomas
    ## 1495                  JT Thor
    ## 1496              Isaiah Todd
    ## 1497             Franz Wagner
    ## 1498            Ish Wainright
    ## 1499              M.J. Walker
    ## 1500     Duane Washington Jr.
    ## 1501         Lindy Waters III
    ## 1502          Trendon Watford
    ## 1503             Joe Wieskamp
    ## 1504            Aaron Wiggins
    ## 1505        Lindell Wigginton
    ## 1506         Brandon Williams
    ## 1507          Ziaire Williams
    ## 1508       McKinley Wright IV
    ## 1509             Moses Wright
    ## 1510                Gabe York
    ## 1511           Omer Yurtseven

``` r
allstars_2023 = c("Giannis Antetokounmpo", "Lauri Markkanen", "Donovan Mitchell", "Ja Morant",
                  "Jayson Tatum", "LeBron James", "Luka Dončić", "Joel Embiid", "Kyrie Irving",
                  "Nikola Jokić")

global_type = "AST"
allstars = players %>% filter(Player %in% allstars_2023) %>%
  gather(`G`:`PTS`, key = "type", value = "values")

end_labelss = allstars %>% filter(end == 2022, type == global_type) %>% arrange(desc(values))

allstars_df = allstars %>% mutate(Player = factor(Player, levels = end_labelss %>% pull(Player)))

ggplot(data = allstars_df %>% filter(type == global_type), aes(start, values, color = Player)) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(labels = scales::label_number(suffix = "season")) +
  labs(x = "Points", y = "Seasons",
       title = "Average AST per season for the 2023 All-Star teams from 2010 season onwards") +
  theme_minimal()
```

![](EDA_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

jimmmy butler

``` r
butler = tibble::tribble(
  ~season, ~`reg`, ~`playoffs`,
  "2022-2023", 23.0, 36.5,
  "2021-2022", 21.4, 27.4,
  "2020-2021", 21.5, 14.5,
  "2019-2020", 33.8, 38.4,
  "2018-2019", 33.2, 35.1,
  "2017-2018", 22.2, 15.8,
  "2016-2017", 37.0, 39.8,
  "2014-2015", 38.7, 42.2,
  "2013-2014", 38.7, 43.5,
  "2012-2013", 26.0, 40.8,
)


colors = c("reg" = "grey", "playoffs" = "indianred4")

ggplot(data = butler, aes(season, points)) + 
  geom_segment(aes(x = season, y = `reg`, xend = season, yend = `playoffs`), color = "grey")+
  geom_point(data = butler, aes(x = season, y = `reg`, color = "reg"), size = 5) +
  geom_point(data = butler, aes(x = season, y = `playoffs`, color = "playoffs"), size = 5) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Seasons", y= "Average points",
       title = "Scoring Performance of Jimmy Butler",
       caption = "Source: hoopsstats.com",
       color = "Legend") +
  scale_color_manual(values = colors,  labels = c("Playoffs", "Regular Season")) + 
  theme_clean() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 9, face = "italic"),
        plot.caption = element_text(face = "italic"),
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9))
```

![](EDA_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
butler = butler %>% gather(`reg`:`playoffs`, key = "period", value = "points")
```

BASE R

``` r
a1 = seq(13, 21)
a1
```

    ## [1] 13 14 15 16 17 18 19 20 21

``` r
a2 = seq(13,21, by = 2)
a2
```

    ## [1] 13 15 17 19 21

``` r
a1 + a2
```

    ## Warning in a1 + a2: longer object length is not a multiple of shorter object
    ## length

    ## [1] 26 29 32 35 38 31 34 37 40

``` r
sum(1:100) == 100* (100+1)/2
```

    ## [1] TRUE

``` r
data(airquality)
summary(airquality)
```

    ##      Ozone           Solar.R           Wind             Temp      
    ##  Min.   :  1.00   Min.   :  7.0   Min.   : 1.700   Min.   :56.00  
    ##  1st Qu.: 18.00   1st Qu.:115.8   1st Qu.: 7.400   1st Qu.:72.00  
    ##  Median : 31.50   Median :205.0   Median : 9.700   Median :79.00  
    ##  Mean   : 42.13   Mean   :185.9   Mean   : 9.958   Mean   :77.88  
    ##  3rd Qu.: 63.25   3rd Qu.:258.8   3rd Qu.:11.500   3rd Qu.:85.00  
    ##  Max.   :168.00   Max.   :334.0   Max.   :20.700   Max.   :97.00  
    ##  NA's   :37       NA's   :7                                       
    ##      Month            Day      
    ##  Min.   :5.000   Min.   : 1.0  
    ##  1st Qu.:6.000   1st Qu.: 8.0  
    ##  Median :7.000   Median :16.0  
    ##  Mean   :6.993   Mean   :15.8  
    ##  3rd Qu.:8.000   3rd Qu.:23.0  
    ##  Max.   :9.000   Max.   :31.0  
    ## 

``` r
class(airquality)
```

    ## [1] "data.frame"

``` r
str(airquality)
```

    ## 'data.frame':    153 obs. of  6 variables:
    ##  $ Ozone  : int  41 36 12 18 NA 28 23 19 8 NA ...
    ##  $ Solar.R: int  190 118 149 313 NA NA 299 99 19 194 ...
    ##  $ Wind   : num  7.4 8 12.6 11.5 14.3 14.9 8.6 13.8 20.1 8.6 ...
    ##  $ Temp   : int  67 72 74 62 56 66 65 59 61 69 ...
    ##  $ Month  : int  5 5 5 5 5 5 5 5 5 5 ...
    ##  $ Day    : int  1 2 3 4 5 6 7 8 9 10 ...

``` r
sum(is.na(airquality ))
```

    ## [1] 44

``` r
which(is.na(airquality))
```

    ##  [1]   5  10  25  26  27  32  33  34  35  36  37  39  42  43  45  46  52  53  54
    ## [20]  55  56  57  58  59  60  61  65  72  75  83  84 102 103 107 115 119 150 158
    ## [39] 159 164 180 249 250 251

``` r
df = na.omit(airquality)

df_summer = df[df$Month %in% c(5,6),]
df_fall = df[df$Month %in% c(7, 8, 9),]

df$Season = ifelse(df$Month %in% c(5,6), "Summer", "Fall")
df$Season = factor(df$Season)

summary(df$Temp)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   57.00   71.00   79.00   77.79   84.50   97.00

``` r
str(df)
```

    ## 'data.frame':    111 obs. of  7 variables:
    ##  $ Ozone  : int  41 36 12 18 23 19 8 16 11 14 ...
    ##  $ Solar.R: int  190 118 149 313 299 99 19 256 290 274 ...
    ##  $ Wind   : num  7.4 8 12.6 11.5 8.6 13.8 20.1 9.7 9.2 10.9 ...
    ##  $ Temp   : int  67 72 74 62 65 59 61 69 66 68 ...
    ##  $ Month  : int  5 5 5 5 5 5 5 5 5 5 ...
    ##  $ Day    : int  1 2 3 4 7 8 9 12 13 14 ...
    ##  $ Season : Factor w/ 2 levels "Fall","Summer": 2 2 2 2 2 2 2 2 2 2 ...
    ##  - attr(*, "na.action")= 'omit' Named int [1:42] 5 6 10 11 25 26 27 32 33 34 ...
    ##   ..- attr(*, "names")= chr [1:42] "5" "6" "10" "11" ...

``` r
tapply(df$Temp, df$Season, summary)
```

    ## $Fall
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   63.00   76.25   81.00   81.23   86.00   97.00 
    ## 
    ## $Summer
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   57.00   62.00   68.00   69.67   76.00   90.00

``` r
df$Month[df$Month == 5] = "May"
df$Month[df$Month == 6] = "June"
df$Month[df$Month == 7] = "July"
df$Month[df$Month == 8] = "August"
df$Month[df$Month == 9] = "September"
df$Month = factor(df$Month, levels = c("May", "June", "July", "August","September"))


boxplot(df$Temp ~ df$Month, xlab = "Month", ylab = "Temperature",
          main = "Monthly temperature in New York")
```

![](EDA_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
ggplot(data = df, aes(Month, Temp)) + 
  geom_boxplot() +
  labs(x = "Month", y = "Temperature", title = "Monthly temperature in New York")
```

![](EDA_files/figure-gfm/unnamed-chunk-32-2.png)<!-- --> LUBRIDATE

``` r
shanghai = read.csv("../data/Shanghai.csv")

head(shanghai, 3)
```

    ##   Year Month  Temp
    ## 1 1841     1 1.809
    ## 2 1841     2 3.366
    ## 3 1841     3 7.122

``` r
shanghai$Date = paste0(shanghai$Year, "-", shanghai$Month)

shanghai$Date = ym(shanghai$Date)
shanghai
```

    ##      Year Month   Temp       Date
    ## 1    1841     1  1.809 1841-01-01
    ## 2    1841     2  3.366 1841-02-01
    ## 3    1841     3  7.122 1841-03-01
    ## 4    1841     4 13.306 1841-04-01
    ## 5    1841     5 19.007 1841-05-01
    ## 6    1841     6 22.943 1841-06-01
    ## 7    1841     7 27.783 1841-07-01
    ## 8    1841     8 27.092 1841-08-01
    ## 9    1841     9 22.326 1841-09-01
    ## 10   1841    10 16.971 1841-10-01
    ## 11   1841    11 11.281 1841-11-01
    ## 12   1841    12  5.429 1841-12-01
    ## 13   1842     1  3.028 1842-01-01
    ## 14   1842     2  4.396 1842-02-01
    ## 15   1842     3  9.670 1842-03-01
    ## 16   1842     4 14.659 1842-04-01
    ## 17   1842     5 18.873 1842-05-01
    ## 18   1842     6 23.730 1842-06-01
    ## 19   1842     7 26.674 1842-07-01
    ## 20   1842     8 27.013 1842-08-01
    ## 21   1842     9 22.710 1842-09-01
    ## 22   1842    10 16.388 1842-10-01
    ## 23   1842    11 10.988 1842-11-01
    ## 24   1842    12  7.003 1842-12-01
    ## 25   1843     1  3.824 1843-01-01
    ## 26   1843     2  5.798 1843-02-01
    ## 27   1843     3  8.402 1843-03-01
    ## 28   1843     4 14.369 1843-04-01
    ## 29   1843     5 18.206 1843-05-01
    ## 30   1843     6 23.542 1843-06-01
    ## 31   1843     7 27.272 1843-07-01
    ## 32   1843     8 27.372 1843-08-01
    ## 33   1843     9 22.907 1843-09-01
    ## 34   1843    10 16.430 1843-10-01
    ## 35   1843    11 11.285 1843-11-01
    ## 36   1843    12  6.939 1843-12-01
    ## 37   1844     1  2.947 1844-01-01
    ## 38   1844     2  4.643 1844-02-01
    ## 39   1844     3  9.385 1844-03-01
    ## 40   1844     4 13.767 1844-04-01
    ## 41   1844     5 18.404 1844-05-01
    ## 42   1844     6 23.572 1844-06-01
    ## 43   1844     7 27.127 1844-07-01
    ## 44   1844     8 27.434 1844-08-01
    ## 45   1844     9 22.523 1844-09-01
    ## 46   1844    10 17.642 1844-10-01
    ## 47   1844    11 11.652 1844-11-01
    ## 48   1844    12  4.883 1844-12-01
    ## 49   1845     1  4.007 1845-01-01
    ## 50   1845     2  4.631 1845-02-01
    ## 51   1845     3  9.182 1845-03-01
    ## 52   1845     4 14.101 1845-04-01
    ## 53   1845     5 19.785 1845-05-01
    ## 54   1845     6 25.068 1845-06-01
    ## 55   1845     7 28.089 1845-07-01
    ## 56   1845     8 27.013 1845-08-01
    ## 57   1845     9 22.493 1845-09-01
    ## 58   1845    10 16.613 1845-10-01
    ## 59   1845    11 10.935 1845-11-01
    ## 60   1845    12  3.196 1845-12-01
    ## 61   1846     1  3.069 1846-01-01
    ## 62   1846     2  5.426 1846-02-01
    ## 63   1846     3  9.063 1846-03-01
    ## 64   1846     4 13.837 1846-04-01
    ## 65   1846     5 17.913 1846-05-01
    ## 66   1846     6 24.561 1846-06-01
    ## 67   1846     7 28.271 1846-07-01
    ## 68   1846     8 28.225 1846-08-01
    ## 69   1846     9 24.028 1846-09-01
    ## 70   1846    10 17.600 1846-10-01
    ## 71   1846    11 10.921 1846-11-01
    ## 72   1846    12  7.315 1846-12-01
    ## 73   1847     1  3.313 1847-01-01
    ## 74   1847     2  3.611 1847-02-01
    ## 75   1847     3  9.457 1847-03-01
    ## 76   1847     4 15.242 1847-04-01
    ## 77   1847     5 18.795 1847-05-01
    ## 78   1847     6 23.137 1847-06-01
    ## 79   1847     7 28.233 1847-07-01
    ## 80   1847     8 27.047 1847-08-01
    ## 81   1847     9 23.715 1847-09-01
    ## 82   1847    10 17.139 1847-10-01
    ## 83   1847    11 13.528 1847-11-01
    ## 84   1847    12  6.133 1847-12-01
    ## 85   1848     1  2.136 1848-01-01
    ## 86   1848     2  3.908 1848-02-01
    ## 87   1848     3  8.909 1848-03-01
    ## 88   1848     4 13.058 1848-04-01
    ## 89   1848     5 20.159 1848-05-01
    ## 90   1848     6 23.209 1848-06-01
    ## 91   1848     7 27.219 1848-07-01
    ## 92   1848     8 26.066 1848-08-01
    ## 93   1848     9 22.831 1848-09-01
    ## 94   1848    10 16.960 1848-10-01
    ## 95   1848    11  9.463 1848-11-01
    ## 96   1848    12  6.344 1848-12-01
    ## 97   1849     1  3.326 1849-01-01
    ## 98   1849     2  6.955 1849-02-01
    ## 99   1849     3  9.445 1849-03-01
    ## 100  1849     4 12.957 1849-04-01
    ## 101  1849     5 18.813 1849-05-01
    ## 102  1849     6 22.954 1849-06-01
    ## 103  1849     7 26.527 1849-07-01
    ## 104  1849     8 27.619 1849-08-01
    ## 105  1849     9 23.435 1849-09-01
    ## 106  1849    10 17.080 1849-10-01
    ## 107  1849    11 10.509 1849-11-01
    ## 108  1849    12  5.701 1849-12-01
    ## 109  1850     1  2.847 1850-01-01
    ## 110  1850     2  4.200 1850-02-01
    ## 111  1850     3  9.583 1850-03-01
    ## 112  1850     4 12.975 1850-04-01
    ## 113  1850     5 19.929 1850-05-01
    ## 114  1850     6 24.008 1850-06-01
    ## 115  1850     7 27.425 1850-07-01
    ## 116  1850     8 27.723 1850-08-01
    ## 117  1850     9 23.004 1850-09-01
    ## 118  1850    10 18.110 1850-10-01
    ## 119  1850    11 11.192 1850-11-01
    ## 120  1850    12  6.169 1850-12-01
    ## 121  1851     1  4.301 1851-01-01
    ## 122  1851     2  4.989 1851-02-01
    ## 123  1851     3  8.043 1851-03-01
    ## 124  1851     4 12.499 1851-04-01
    ## 125  1851     5 18.442 1851-05-01
    ## 126  1851     6 23.568 1851-06-01
    ## 127  1851     7 27.893 1851-07-01
    ## 128  1851     8 27.404 1851-08-01
    ## 129  1851     9 22.143 1851-09-01
    ## 130  1851    10 15.659 1851-10-01
    ## 131  1851    11 12.205 1851-11-01
    ## 132  1851    12  5.525 1851-12-01
    ## 133  1852     1  2.933 1852-01-01
    ## 134  1852     2  2.996 1852-02-01
    ## 135  1852     3  7.244 1852-03-01
    ## 136  1852     4 13.154 1852-04-01
    ## 137  1852     5 19.775 1852-05-01
    ## 138  1852     6 25.444 1852-06-01
    ## 139  1852     7 29.383 1852-07-01
    ## 140  1852     8 27.358 1852-08-01
    ## 141  1852     9 22.277 1852-09-01
    ## 142  1852    10 18.115 1852-10-01
    ## 143  1852    11 11.613 1852-11-01
    ## 144  1852    12  4.968 1852-12-01
    ## 145  1853     1  2.170 1853-01-01
    ## 146  1853     2  3.186 1853-02-01
    ## 147  1853     3  9.512 1853-03-01
    ## 148  1853     4 13.918 1853-04-01
    ## 149  1853     5 19.411 1853-05-01
    ## 150  1853     6 24.810 1853-06-01
    ## 151  1853     7 29.494 1853-07-01
    ## 152  1853     8 26.774 1853-08-01
    ## 153  1853     9 24.029 1853-09-01
    ## 154  1853    10 17.644 1853-10-01
    ## 155  1853    11 12.778 1853-11-01
    ## 156  1853    12  5.774 1853-12-01
    ## 157  1854     1  4.707 1854-01-01
    ## 158  1854     2  3.081 1854-02-01
    ## 159  1854     3  8.780 1854-03-01
    ## 160  1854     4 13.968 1854-04-01
    ## 161  1854     5 20.340 1854-05-01
    ## 162  1854     6 24.419 1854-06-01
    ## 163  1854     7 28.974 1854-07-01
    ## 164  1854     8 28.481 1854-08-01
    ## 165  1854     9 23.230 1854-09-01
    ## 166  1854    10 17.973 1854-10-01
    ## 167  1854    11 11.694 1854-11-01
    ## 168  1854    12  6.203 1854-12-01
    ## 169  1855     1  0.789 1855-01-01
    ## 170  1855     2  4.517 1855-02-01
    ## 171  1855     3  9.190 1855-03-01
    ## 172  1855     4 14.850 1855-04-01
    ## 173  1855     5 20.708 1855-05-01
    ## 174  1855     6 24.437 1855-06-01
    ## 175  1855     7 28.748 1855-07-01
    ## 176  1855     8 27.543 1855-08-01
    ## 177  1855     9 23.493 1855-09-01
    ## 178  1855    10 17.502 1855-10-01
    ## 179  1855    11 11.465 1855-11-01
    ## 180  1855    12  6.854 1855-12-01
    ## 181  1856     1  0.635 1856-01-01
    ## 182  1856     2  2.825 1856-02-01
    ## 183  1856     3  8.785 1856-03-01
    ## 184  1856     4 14.537 1856-04-01
    ## 185  1856     5 19.826 1856-05-01
    ## 186  1856     6 24.048 1856-06-01
    ## 187  1856     7 28.603 1856-07-01
    ## 188  1856     8 28.233 1856-08-01
    ## 189  1856     9 24.011 1856-09-01
    ## 190  1856    10 17.037 1856-10-01
    ## 191  1856    11 11.900 1856-11-01
    ## 192  1856    12  6.103 1856-12-01
    ## 193  1857     1  2.818 1857-01-01
    ## 194  1857     2  5.654 1857-02-01
    ## 195  1857     3  9.433 1857-03-01
    ## 196  1857     4 15.370 1857-04-01
    ## 197  1857     5 18.921 1857-05-01
    ## 198  1857     6 24.765 1857-06-01
    ## 199  1857     7 28.588 1857-07-01
    ## 200  1857     8 27.430 1857-08-01
    ## 201  1857     9 22.621 1857-09-01
    ## 202  1857    10 17.743 1857-10-01
    ## 203  1857    11 11.003 1857-11-01
    ## 204  1857    12  6.779 1857-12-01
    ## 205  1858     1  3.668 1858-01-01
    ## 206  1858     2  4.885 1858-02-01
    ## 207  1858     3  8.742 1858-03-01
    ## 208  1858     4 14.573 1858-04-01
    ## 209  1858     5 18.885 1858-05-01
    ## 210  1858     6 24.138 1858-06-01
    ## 211  1858     7 27.806 1858-07-01
    ## 212  1858     8 26.062 1858-08-01
    ## 213  1858     9 23.369 1858-09-01
    ## 214  1858    10 17.021 1858-10-01
    ## 215  1858    11 10.964 1858-11-01
    ## 216  1858    12  5.791 1858-12-01
    ## 217  1859     1  2.916 1859-01-01
    ## 218  1859     2  4.089 1859-02-01
    ## 219  1859     3  8.309 1859-03-01
    ## 220  1859     4 16.178 1859-04-01
    ## 221  1859     5 20.316 1859-05-01
    ## 222  1859     6 25.198 1859-06-01
    ## 223  1859     7 27.444 1859-07-01
    ## 224  1859     8 27.839 1859-08-01
    ## 225  1859     9 23.135 1859-09-01
    ## 226  1859    10 17.451 1859-10-01
    ## 227  1859    11 10.597 1859-11-01
    ## 228  1859    12  4.210 1859-12-01
    ## 229  1860     1  2.469 1860-01-01
    ## 230  1860     2  5.209 1860-02-01
    ## 231  1860     3  7.028 1860-03-01
    ## 232  1860     4 14.231 1860-04-01
    ## 233  1860     5 17.993 1860-05-01
    ## 234  1860     6 24.009 1860-06-01
    ## 235  1860     7 26.972 1860-07-01
    ## 236  1860     8 27.564 1860-08-01
    ## 237  1860     9 23.322 1860-09-01
    ## 238  1860    10 16.247 1860-10-01
    ## 239  1860    11  9.508 1860-11-01
    ## 240  1860    12  6.020 1860-12-01
    ## 241  1861     1  1.306 1861-01-01
    ## 242  1861     2  2.705 1861-02-01
    ## 243  1861     3  8.301 1861-03-01
    ## 244  1861     4 14.915 1861-04-01
    ## 245  1861     5 19.134 1861-05-01
    ## 246  1861     6 24.190 1861-06-01
    ## 247  1861     7 28.951 1861-07-01
    ## 248  1861     8 28.379 1861-08-01
    ## 249  1861     9 21.970 1861-09-01
    ## 250  1861    10 17.182 1861-10-01
    ## 251  1861    11 11.341 1861-11-01
    ## 252  1861    12  5.102 1861-12-01
    ## 253  1862     1 -0.050 1862-01-01
    ## 254  1862     2  1.783 1862-02-01
    ## 255  1862     3  6.913 1862-03-01
    ## 256  1862     4 13.623 1862-04-01
    ## 257  1862     5 19.842 1862-05-01
    ## 258  1862     6 24.537 1862-06-01
    ## 259  1862     7 28.167 1862-07-01
    ## 260  1862     8 26.340 1862-08-01
    ## 261  1862     9 23.012 1862-09-01
    ## 262  1862    10 16.592 1862-10-01
    ## 263  1862    11  9.245 1862-11-01
    ## 264  1862    12  3.606 1862-12-01
    ## 265  1863     1  3.722 1863-01-01
    ## 266  1863     2  6.114 1863-02-01
    ## 267  1863     3  8.824 1863-03-01
    ## 268  1863     4 12.218 1863-04-01
    ## 269  1863     5 18.168 1863-05-01
    ## 270  1863     6 24.125 1863-06-01
    ## 271  1863     7 29.720 1863-07-01
    ## 272  1863     8 27.122 1863-08-01
    ## 273  1863     9 22.477 1863-09-01
    ## 274  1863    10 15.760 1863-10-01
    ## 275  1863    11  9.177 1863-11-01
    ## 276  1863    12  4.943 1863-12-01
    ## 277  1864     1  0.865 1864-01-01
    ## 278  1864     2  2.876 1864-02-01
    ## 279  1864     3  6.359 1864-03-01
    ## 280  1864     4 13.762 1864-04-01
    ## 281  1864     5 18.600 1864-05-01
    ## 282  1864     6 23.647 1864-06-01
    ## 283  1864     7 28.326 1864-07-01
    ## 284  1864     8 27.021 1864-08-01
    ## 285  1864     9 22.627 1864-09-01
    ## 286  1864    10 18.242 1864-10-01
    ## 287  1864    11 11.308 1864-11-01
    ## 288  1864    12  4.507 1864-12-01
    ## 289  1865     1  4.107 1865-01-01
    ## 290  1865     2  4.573 1865-02-01
    ## 291  1865     3  8.337 1865-03-01
    ## 292  1865     4 14.668 1865-04-01
    ## 293  1865     5 19.699 1865-05-01
    ## 294  1865     6 24.587 1865-06-01
    ## 295  1865     7 28.002 1865-07-01
    ## 296  1865     8 28.032 1865-08-01
    ## 297  1865     9 23.821 1865-09-01
    ## 298  1865    10 17.431 1865-10-01
    ## 299  1865    11 10.777 1865-11-01
    ## 300  1865    12  4.583 1865-12-01
    ## 301  1866     1  3.341 1866-01-01
    ## 302  1866     2  4.665 1866-02-01
    ## 303  1866     3  8.492 1866-03-01
    ## 304  1866     4 12.445 1866-04-01
    ## 305  1866     5 19.768 1866-05-01
    ## 306  1866     6 23.276 1866-06-01
    ## 307  1866     7 28.236 1866-07-01
    ## 308  1866     8 29.019 1866-08-01
    ## 309  1866     9 22.614 1866-09-01
    ## 310  1866    10 17.364 1866-10-01
    ## 311  1866    11 10.786 1866-11-01
    ## 312  1866    12  6.456 1866-12-01
    ## 313  1867     1  2.710 1867-01-01
    ## 314  1867     2  4.688 1867-02-01
    ## 315  1867     3 10.121 1867-03-01
    ## 316  1867     4 14.802 1867-04-01
    ## 317  1867     5 19.540 1867-05-01
    ## 318  1867     6 24.734 1867-06-01
    ## 319  1867     7 28.335 1867-07-01
    ## 320  1867     8 27.435 1867-08-01
    ## 321  1867     9 22.662 1867-09-01
    ## 322  1867    10 18.407 1867-10-01
    ## 323  1867    11 10.846 1867-11-01
    ## 324  1867    12  7.087 1867-12-01
    ## 325  1868     1  3.654 1868-01-01
    ## 326  1868     2  4.816 1868-02-01
    ## 327  1868     3  8.416 1868-03-01
    ## 328  1868     4 14.569 1868-04-01
    ## 329  1868     5 19.023 1868-05-01
    ## 330  1868     6 23.886 1868-06-01
    ## 331  1868     7 28.396 1868-07-01
    ## 332  1868     8 27.528 1868-08-01
    ## 333  1868     9 23.417 1868-09-01
    ## 334  1868    10 18.371 1868-10-01
    ## 335  1868    11 11.875 1868-11-01
    ## 336  1868    12  6.337 1868-12-01
    ## 337  1869     1  3.796 1869-01-01
    ## 338  1869     2  5.717 1869-02-01
    ## 339  1869     3  8.652 1869-03-01
    ## 340  1869     4 13.584 1869-04-01
    ## 341  1869     5 20.326 1869-05-01
    ## 342  1869     6 24.592 1869-06-01
    ## 343  1869     7 28.446 1869-07-01
    ## 344  1869     8 28.196 1869-08-01
    ## 345  1869     9 24.158 1869-09-01
    ## 346  1869    10 18.492 1869-10-01
    ## 347  1869    11 11.216 1869-11-01
    ## 348  1869    12  5.869 1869-12-01
    ## 349  1870     1  2.951 1870-01-01
    ## 350  1870     2  4.064 1870-02-01
    ## 351  1870     3  9.589 1870-03-01
    ## 352  1870     4 14.728 1870-04-01
    ## 353  1870     5 20.099 1870-05-01
    ## 354  1870     6 25.073 1870-06-01
    ## 355  1870     7 28.367 1870-07-01
    ## 356  1870     8 27.744 1870-08-01
    ## 357  1870     9 23.907 1870-09-01
    ## 358  1870    10 18.291 1870-10-01
    ## 359  1870    11 11.211 1870-11-01
    ## 360  1870    12  5.405 1870-12-01
    ## 361  1871     1  3.227 1871-01-01
    ## 362  1871     2  4.505 1871-02-01
    ## 363  1871     3  9.848 1871-03-01
    ## 364  1871     4 14.886 1871-04-01
    ## 365  1871     5 20.650 1871-05-01
    ## 366  1871     6 25.535 1871-06-01
    ## 367  1871     7 29.336 1871-07-01
    ## 368  1871     8 28.782 1871-08-01
    ## 369  1871     9 24.941 1871-09-01
    ## 370  1871    10 19.181 1871-10-01
    ## 371  1871    11 11.391 1871-11-01
    ## 372  1871    12  3.465 1871-12-01
    ## 373  1872     1  2.642 1872-01-01
    ## 374  1872     2  2.931 1872-02-01
    ## 375  1872     3  9.544 1872-03-01
    ## 376  1872     4 15.708 1872-04-01
    ## 377  1872     5 20.312 1872-05-01
    ## 378  1872     6 23.171 1872-06-01
    ## 379  1872     7 29.908 1872-07-01
    ## 380  1872     8 28.197 1872-08-01
    ## 381  1872     9 23.451 1872-09-01
    ## 382  1872    10 17.886 1872-10-01
    ## 383  1872    11 11.721 1872-11-01
    ## 384  1872    12  7.975 1872-12-01
    ## 385  1873     1  1.974 1873-01-01
    ## 386  1873     2  4.159 1873-02-01
    ## 387  1873     3  7.844 1873-03-01
    ## 388  1873     4 15.897 1873-04-01
    ## 389  1873     5 20.073 1873-05-01
    ## 390  1873     6 23.409 1873-06-01
    ## 391  1873     7 28.695 1873-07-01
    ## 392  1873     8 27.234 1873-08-01
    ## 393  1873     9 23.470 1873-09-01
    ## 394  1873    10 17.011 1873-10-01
    ## 395  1873    11 11.967 1873-11-01
    ## 396  1873    12  7.113 1873-12-01
    ## 397  1874     1  1.138 1874-01-01
    ## 398  1874     2  4.237 1874-02-01
    ## 399  1874     3  7.508 1874-03-01
    ## 400  1874     4 15.042 1874-04-01
    ## 401  1874     5 19.629 1874-05-01
    ## 402  1874     6 25.916 1874-06-01
    ## 403  1874     7 27.950 1874-07-01
    ## 404  1874     8 27.928 1874-08-01
    ## 405  1874     9 23.734 1874-09-01
    ## 406  1874    10 18.120 1874-10-01
    ## 407  1874    11 10.137 1874-11-01
    ## 408  1874    12  6.770 1874-12-01
    ## 409  1875     1  2.119 1875-01-01
    ## 410  1875     2  4.307 1875-02-01
    ## 411  1875     3 10.134 1875-03-01
    ## 412  1875     4 13.639 1875-04-01
    ## 413  1875     5 20.689 1875-05-01
    ## 414  1875     6 23.790 1875-06-01
    ## 415  1875     7 28.620 1875-07-01
    ## 416  1875     8 27.627 1875-08-01
    ## 417  1875     9 22.882 1875-09-01
    ## 418  1875    10 17.569 1875-10-01
    ## 419  1875    11 10.960 1875-11-01
    ## 420  1875    12  3.276 1875-12-01
    ## 421  1876     1  1.160 1876-01-01
    ## 422  1876     2  5.070 1876-02-01
    ## 423  1876     3  9.335 1876-03-01
    ## 424  1876     4 14.235 1876-04-01
    ## 425  1876     5 20.257 1876-05-01
    ## 426  1876     6 23.168 1876-06-01
    ## 427  1876     7 27.533 1876-07-01
    ## 428  1876     8 27.728 1876-08-01
    ## 429  1876     9 23.454 1876-09-01
    ## 430  1876    10 18.288 1876-10-01
    ## 431  1876    11 10.562 1876-11-01
    ## 432  1876    12  6.362 1876-12-01
    ## 433  1877     1  2.717 1877-01-01
    ## 434  1877     2  2.603 1877-02-01
    ## 435  1877     3  8.397 1877-03-01
    ## 436  1877     4 15.627 1877-04-01
    ## 437  1877     5 19.831 1877-05-01
    ## 438  1877     6 24.533 1877-06-01
    ## 439  1877     7 27.600 1877-07-01
    ## 440  1877     8 26.553 1877-08-01
    ## 441  1877     9 23.451 1877-09-01
    ## 442  1877    10 16.347 1877-10-01
    ## 443  1877    11 11.746 1877-11-01
    ## 444  1877    12  5.814 1877-12-01
    ## 445  1878     1  0.325 1878-01-01
    ## 446  1878     2  3.199 1878-02-01
    ## 447  1878     3  9.605 1878-03-01
    ## 448  1878     4 14.544 1878-04-01
    ## 449  1878     5 19.312 1878-05-01
    ## 450  1878     6 23.949 1878-06-01
    ## 451  1878     7 28.347 1878-07-01
    ## 452  1878     8 27.426 1878-08-01
    ## 453  1878     9 23.934 1878-09-01
    ## 454  1878    10 18.409 1878-10-01
    ## 455  1878    11 11.726 1878-11-01
    ## 456  1878    12  4.730 1878-12-01
    ## 457  1879     1  2.929 1879-01-01
    ## 458  1879     2  5.640 1879-02-01
    ## 459  1879     3  8.126 1879-03-01
    ## 460  1879     4 13.697 1879-04-01
    ## 461  1879     5 20.185 1879-05-01
    ## 462  1879     6 24.406 1879-06-01
    ## 463  1879     7 29.462 1879-07-01
    ## 464  1879     8 29.295 1879-08-01
    ## 465  1879     9 23.984 1879-09-01
    ## 466  1879    10 17.511 1879-10-01
    ## 467  1879    11 12.659 1879-11-01
    ## 468  1879    12  5.709 1879-12-01
    ## 469  1880     1  1.948 1880-01-01
    ## 470  1880     2  4.372 1880-02-01
    ## 471  1880     3  9.193 1880-03-01
    ## 472  1880     4 13.893 1880-04-01
    ## 473  1880     5 20.525 1880-05-01
    ## 474  1880     6 23.667 1880-06-01
    ## 475  1880     7 26.817 1880-07-01
    ## 476  1880     8 26.470 1880-08-01
    ## 477  1880     9 24.109 1880-09-01
    ## 478  1880    10 18.818 1880-10-01
    ## 479  1880    11 10.075 1880-11-01
    ## 480  1880    12  3.505 1880-12-01
    ## 481  1881     1  2.130 1881-01-01
    ## 482  1881     2  5.275 1881-02-01
    ## 483  1881     3  6.434 1881-03-01
    ## 484  1881     4 14.331 1881-04-01
    ## 485  1881     5 18.998 1881-05-01
    ## 486  1881     6 24.164 1881-06-01
    ## 487  1881     7 27.992 1881-07-01
    ## 488  1881     8 27.919 1881-08-01
    ## 489  1881     9 23.728 1881-09-01
    ## 490  1881    10 18.122 1881-10-01
    ## 491  1881    11 12.394 1881-11-01
    ## 492  1881    12  6.062 1881-12-01
    ## 493  1882     1  4.545 1882-01-01
    ## 494  1882     2  4.488 1882-02-01
    ## 495  1882     3  8.589 1882-03-01
    ## 496  1882     4 14.285 1882-04-01
    ## 497  1882     5 19.575 1882-05-01
    ## 498  1882     6 23.584 1882-06-01
    ## 499  1882     7 26.750 1882-07-01
    ## 500  1882     8 26.294 1882-08-01
    ## 501  1882     9 23.466 1882-09-01
    ## 502  1882    10 18.976 1882-10-01
    ## 503  1882    11 11.024 1882-11-01
    ## 504  1882    12  4.869 1882-12-01
    ## 505  1883     1  2.027 1883-01-01
    ## 506  1883     2  3.618 1883-02-01
    ## 507  1883     3  8.607 1883-03-01
    ## 508  1883     4 14.561 1883-04-01
    ## 509  1883     5 18.750 1883-05-01
    ## 510  1883     6 24.914 1883-06-01
    ## 511  1883     7 28.084 1883-07-01
    ## 512  1883     8 27.299 1883-08-01
    ## 513  1883     9 23.404 1883-09-01
    ## 514  1883    10 18.272 1883-10-01
    ## 515  1883    11 11.032 1883-11-01
    ## 516  1883    12  4.361 1883-12-01
    ## 517  1884     1  4.034 1884-01-01
    ## 518  1884     2  2.932 1884-02-01
    ## 519  1884     3  8.513 1884-03-01
    ## 520  1884     4 13.514 1884-04-01
    ## 521  1884     5 18.867 1884-05-01
    ## 522  1884     6 23.494 1884-06-01
    ## 523  1884     7 27.267 1884-07-01
    ## 524  1884     8 26.522 1884-08-01
    ## 525  1884     9 23.831 1884-09-01
    ## 526  1884    10 17.434 1884-10-01
    ## 527  1884    11  9.297 1884-11-01
    ## 528  1884    12  3.327 1884-12-01
    ## 529  1885     1  2.127 1885-01-01
    ## 530  1885     2  2.167 1885-02-01
    ## 531  1885     3  7.863 1885-03-01
    ## 532  1885     4 12.876 1885-04-01
    ## 533  1885     5 19.168 1885-05-01
    ## 534  1885     6 23.218 1885-06-01
    ## 535  1885     7 26.399 1885-07-01
    ## 536  1885     8 27.914 1885-08-01
    ## 537  1885     9 22.970 1885-09-01
    ## 538  1885    10 17.975 1885-10-01
    ## 539  1885    11 10.128 1885-11-01
    ## 540  1885    12  6.111 1885-12-01
    ## 541  1886     1  1.847 1886-01-01
    ## 542  1886     2  1.191 1886-02-01
    ## 543  1886     3  8.515 1886-03-01
    ## 544  1886     4 14.053 1886-04-01
    ## 545  1886     5 19.518 1886-05-01
    ## 546  1886     6 23.049 1886-06-01
    ## 547  1886     7 27.936 1886-07-01
    ## 548  1886     8 27.327 1886-08-01
    ## 549  1886     9 22.264 1886-09-01
    ## 550  1886    10 18.604 1886-10-01
    ## 551  1886    11 11.651 1886-11-01
    ## 552  1886    12  4.748 1886-12-01
    ## 553  1887     1  2.562 1887-01-01
    ## 554  1887     2  3.660 1887-02-01
    ## 555  1887     3  8.571 1887-03-01
    ## 556  1887     4 15.339 1887-04-01
    ## 557  1887     5 19.255 1887-05-01
    ## 558  1887     6 23.197 1887-06-01
    ## 559  1887     7 27.553 1887-07-01
    ## 560  1887     8 28.266 1887-08-01
    ## 561  1887     9 23.924 1887-09-01
    ## 562  1887    10 17.994 1887-10-01
    ## 563  1887    11 11.635 1887-11-01
    ## 564  1887    12  5.890 1887-12-01
    ## 565  1888     1  3.847 1888-01-01
    ## 566  1888     2  2.380 1888-02-01
    ## 567  1888     3  9.628 1888-03-01
    ## 568  1888     4 14.152 1888-04-01
    ## 569  1888     5 19.827 1888-05-01
    ## 570  1888     6 23.381 1888-06-01
    ## 571  1888     7 28.147 1888-07-01
    ## 572  1888     8 28.121 1888-08-01
    ## 573  1888     9 23.296 1888-09-01
    ## 574  1888    10 17.599 1888-10-01
    ## 575  1888    11 12.420 1888-11-01
    ## 576  1888    12  7.650 1888-12-01
    ## 577  1889     1  1.070 1889-01-01
    ## 578  1889     2  3.382 1889-02-01
    ## 579  1889     3  9.185 1889-03-01
    ## 580  1889     4 13.803 1889-04-01
    ## 581  1889     5 18.759 1889-05-01
    ## 582  1889     6 25.288 1889-06-01
    ## 583  1889     7 28.276 1889-07-01
    ## 584  1889     8 27.310 1889-08-01
    ## 585  1889     9 22.438 1889-09-01
    ## 586  1889    10 17.586 1889-10-01
    ## 587  1889    11 11.138 1889-11-01
    ## 588  1889    12  5.395 1889-12-01
    ## 589  1890     1  3.710 1890-01-01
    ## 590  1890     2  6.815 1890-02-01
    ## 591  1890     3  8.066 1890-03-01
    ## 592  1890     4 15.572 1890-04-01
    ## 593  1890     5 19.790 1890-05-01
    ## 594  1890     6 24.216 1890-06-01
    ## 595  1890     7 28.257 1890-07-01
    ## 596  1890     8 27.236 1890-08-01
    ## 597  1890     9 22.663 1890-09-01
    ## 598  1890    10 17.118 1890-10-01
    ## 599  1890    11 12.958 1890-11-01
    ## 600  1890    12  8.250 1890-12-01
    ## 601  1891     1  2.988 1891-01-01
    ## 602  1891     2  4.293 1891-02-01
    ## 603  1891     3  8.749 1891-03-01
    ## 604  1891     4 13.431 1891-04-01
    ## 605  1891     5 20.044 1891-05-01
    ## 606  1891     6 23.993 1891-06-01
    ## 607  1891     7 27.170 1891-07-01
    ## 608  1891     8 27.275 1891-08-01
    ## 609  1891     9 23.505 1891-09-01
    ## 610  1891    10 19.423 1891-10-01
    ## 611  1891    11 11.869 1891-11-01
    ## 612  1891    12  5.896 1891-12-01
    ## 613  1892     1  3.350 1892-01-01
    ## 614  1892     2  4.762 1892-02-01
    ## 615  1892     3  6.598 1892-03-01
    ## 616  1892     4 14.237 1892-04-01
    ## 617  1892     5 18.907 1892-05-01
    ## 618  1892     6 24.532 1892-06-01
    ## 619  1892     7 28.588 1892-07-01
    ## 620  1892     8 28.541 1892-08-01
    ## 621  1892     9 22.547 1892-09-01
    ## 622  1892    10 16.845 1892-10-01
    ## 623  1892    11 12.129 1892-11-01
    ## 624  1892    12  4.096 1892-12-01
    ## 625  1893     1  0.734 1893-01-01
    ## 626  1893     2  2.338 1893-02-01
    ## 627  1893     3  8.814 1893-03-01
    ## 628  1893     4 14.250 1893-04-01
    ## 629  1893     5 18.839 1893-05-01
    ## 630  1893     6 24.138 1893-06-01
    ## 631  1893     7 28.359 1893-07-01
    ## 632  1893     8 26.961 1893-08-01
    ## 633  1893     9 24.413 1893-09-01
    ## 634  1893    10 17.195 1893-10-01
    ## 635  1893    11 10.150 1893-11-01
    ## 636  1893    12  5.374 1893-12-01
    ## 637  1894     1  3.791 1894-01-01
    ## 638  1894     2  4.897 1894-02-01
    ## 639  1894     3  8.873 1894-03-01
    ## 640  1894     4 15.614 1894-04-01
    ## 641  1894     5 19.810 1894-05-01
    ## 642  1894     6 24.899 1894-06-01
    ## 643  1894     7 28.943 1894-07-01
    ## 644  1894     8 28.695 1894-08-01
    ## 645  1894     9 23.669 1894-09-01
    ## 646  1894    10 17.742 1894-10-01
    ## 647  1894    11 12.468 1894-11-01
    ## 648  1894    12  5.328 1894-12-01
    ## 649  1895     1  1.393 1895-01-01
    ## 650  1895     2  4.082 1895-02-01
    ## 651  1895     3  7.746 1895-03-01
    ## 652  1895     4 14.936 1895-04-01
    ## 653  1895     5 19.860 1895-05-01
    ## 654  1895     6 24.354 1895-06-01
    ## 655  1895     7 26.902 1895-07-01
    ## 656  1895     8 27.552 1895-08-01
    ## 657  1895     9 22.524 1895-09-01
    ## 658  1895    10 17.300 1895-10-01
    ## 659  1895    11 11.139 1895-11-01
    ## 660  1895    12  5.399 1895-12-01
    ## 661  1896     1  3.563 1896-01-01
    ## 662  1896     2  3.599 1896-02-01
    ## 663  1896     3  6.946 1896-03-01
    ## 664  1896     4 14.829 1896-04-01
    ## 665  1896     5 19.211 1896-05-01
    ## 666  1896     6 24.558 1896-06-01
    ## 667  1896     7 27.445 1896-07-01
    ## 668  1896     8 27.921 1896-08-01
    ## 669  1896     9 23.865 1896-09-01
    ## 670  1896    10 18.045 1896-10-01
    ## 671  1896    11 13.397 1896-11-01
    ## 672  1896    12  5.332 1896-12-01
    ## 673  1897     1  4.292 1897-01-01
    ## 674  1897     2  1.995 1897-02-01
    ## 675  1897     3  8.427 1897-03-01
    ## 676  1897     4 13.356 1897-04-01
    ## 677  1897     5 19.632 1897-05-01
    ## 678  1897     6 24.295 1897-06-01
    ## 679  1897     7 27.993 1897-07-01
    ## 680  1897     8 28.288 1897-08-01
    ## 681  1897     9 23.618 1897-09-01
    ## 682  1897    10 18.089 1897-10-01
    ## 683  1897    11 12.874 1897-11-01
    ## 684  1897    12  4.323 1897-12-01
    ## 685  1898     1  4.412 1898-01-01
    ## 686  1898     2  6.747 1898-02-01
    ## 687  1898     3  8.205 1898-03-01
    ## 688  1898     4 13.555 1898-04-01
    ## 689  1898     5 20.133 1898-05-01
    ## 690  1898     6 24.548 1898-06-01
    ## 691  1898     7 28.948 1898-07-01
    ## 692  1898     8 27.967 1898-08-01
    ## 693  1898     9 24.327 1898-09-01
    ## 694  1898    10 18.277 1898-10-01
    ## 695  1898    11 13.074 1898-11-01
    ## 696  1898    12  5.772 1898-12-01
    ## 697  1899     1  3.152 1899-01-01
    ## 698  1899     2  5.449 1899-02-01
    ## 699  1899     3  9.619 1899-03-01
    ## 700  1899     4 14.228 1899-04-01
    ## 701  1899     5 19.579 1899-05-01
    ## 702  1899     6 24.995 1899-06-01
    ## 703  1899     7 28.097 1899-07-01
    ## 704  1899     8 27.117 1899-08-01
    ## 705  1899     9 22.710 1899-09-01
    ## 706  1899    10 16.019 1899-10-01
    ## 707  1899    11 10.854 1899-11-01
    ## 708  1899    12  8.236 1899-12-01
    ## 709  1900     1  1.599 1900-01-01
    ## 710  1900     2  4.166 1900-02-01
    ## 711  1900     3  8.460 1900-03-01
    ## 712  1900     4 14.510 1900-04-01
    ## 713  1900     5 20.304 1900-05-01
    ## 714  1900     6 23.576 1900-06-01
    ## 715  1900     7 28.102 1900-07-01
    ## 716  1900     8 28.107 1900-08-01
    ## 717  1900     9 23.657 1900-09-01
    ## 718  1900    10 18.245 1900-10-01
    ## 719  1900    11 12.331 1900-11-01
    ## 720  1900    12  6.535 1900-12-01
    ## 721  1901     1  4.543 1901-01-01
    ## 722  1901     2  1.348 1901-02-01
    ## 723  1901     3  8.330 1901-03-01
    ## 724  1901     4 14.676 1901-04-01
    ## 725  1901     5 18.763 1901-05-01
    ## 726  1901     6 23.483 1901-06-01
    ## 727  1901     7 26.533 1901-07-01
    ## 728  1901     8 27.379 1901-08-01
    ## 729  1901     9 22.907 1901-09-01
    ## 730  1901    10 18.225 1901-10-01
    ## 731  1901    11 11.402 1901-11-01
    ## 732  1901    12  5.511 1901-12-01
    ## 733  1902     1  5.398 1902-01-01
    ## 734  1902     2  4.561 1902-02-01
    ## 735  1902     3 10.767 1902-03-01
    ## 736  1902     4 14.262 1902-04-01
    ## 737  1902     5 20.020 1902-05-01
    ## 738  1902     6 23.800 1902-06-01
    ## 739  1902     7 27.501 1902-07-01
    ## 740  1902     8 26.812 1902-08-01
    ## 741  1902     9 22.575 1902-09-01
    ## 742  1902    10 18.460 1902-10-01
    ## 743  1902    11 14.078 1902-11-01
    ## 744  1902    12  6.978 1902-12-01
    ## 745  1903     1  2.815 1903-01-01
    ## 746  1903     2  4.133 1903-02-01
    ## 747  1903     3  9.653 1903-03-01
    ## 748  1903     4 14.486 1903-04-01
    ## 749  1903     5 19.385 1903-05-01
    ## 750  1903     6 23.606 1903-06-01
    ## 751  1903     7 26.777 1903-07-01
    ## 752  1903     8 28.030 1903-08-01
    ## 753  1903     9 23.822 1903-09-01
    ## 754  1903    10 17.844 1903-10-01
    ## 755  1903    11 10.983 1903-11-01
    ## 756  1903    12  5.172 1903-12-01
    ## 757  1904     1  2.961 1904-01-01
    ## 758  1904     2  6.638 1904-02-01
    ## 759  1904     3  8.548 1904-03-01
    ## 760  1904     4 14.284 1904-04-01
    ## 761  1904     5 19.155 1904-05-01
    ## 762  1904     6 24.585 1904-06-01
    ## 763  1904     7 27.475 1904-07-01
    ## 764  1904     8 27.214 1904-08-01
    ## 765  1904     9 23.235 1904-09-01
    ## 766  1904    10 17.607 1904-10-01
    ## 767  1904    11 10.843 1904-11-01
    ## 768  1904    12  5.126 1904-12-01
    ## 769  1905     1  5.842 1905-01-01
    ## 770  1905     2  1.912 1905-02-01
    ## 771  1905     3  7.086 1905-03-01
    ## 772  1905     4 12.330 1905-04-01
    ## 773  1905     5 19.385 1905-05-01
    ## 774  1905     6 24.767 1905-06-01
    ## 775  1905     7 28.427 1905-07-01
    ## 776  1905     8 26.619 1905-08-01
    ## 777  1905     9 23.618 1905-09-01
    ## 778  1905    10 17.393 1905-10-01
    ## 779  1905    11 11.444 1905-11-01
    ## 780  1905    12  7.225 1905-12-01
    ## 781  1906     1  3.244 1906-01-01
    ## 782  1906     2  3.431 1906-02-01
    ## 783  1906     3  8.534 1906-03-01
    ## 784  1906     4 14.472 1906-04-01
    ## 785  1906     5 19.372 1906-05-01
    ## 786  1906     6 24.281 1906-06-01
    ## 787  1906     7 27.680 1906-07-01
    ## 788  1906     8 27.597 1906-08-01
    ## 789  1906     9 23.395 1906-09-01
    ## 790  1906    10 17.577 1906-10-01
    ## 791  1906    11 10.463 1906-11-01
    ## 792  1906    12  5.946 1906-12-01
    ## 793  1907     1  4.908 1907-01-01
    ## 794  1907     2  2.599 1907-02-01
    ## 795  1907     3  7.714 1907-03-01
    ## 796  1907     4 13.998 1907-04-01
    ## 797  1907     5 20.351 1907-05-01
    ## 798  1907     6 23.562 1907-06-01
    ## 799  1907     7 25.855 1907-07-01
    ## 800  1907     8 28.004 1907-08-01
    ## 801  1907     9 22.929 1907-09-01
    ## 802  1907    10 18.854 1907-10-01
    ## 803  1907    11 11.936 1907-11-01
    ## 804  1907    12  6.160 1907-12-01
    ## 805  1908     1  4.053 1908-01-01
    ## 806  1908     2  3.532 1908-02-01
    ## 807  1908     3  7.968 1908-03-01
    ## 808  1908     4 12.707 1908-04-01
    ## 809  1908     5 19.780 1908-05-01
    ## 810  1908     6 24.232 1908-06-01
    ## 811  1908     7 27.656 1908-07-01
    ## 812  1908     8 27.743 1908-08-01
    ## 813  1908     9 23.416 1908-09-01
    ## 814  1908    10 18.286 1908-10-01
    ## 815  1908    11 11.344 1908-11-01
    ## 816  1908    12  7.378 1908-12-01
    ## 817  1909     1  3.202 1909-01-01
    ## 818  1909     2  4.513 1909-02-01
    ## 819  1909     3  7.366 1909-03-01
    ## 820  1909     4 14.907 1909-04-01
    ## 821  1909     5 19.870 1909-05-01
    ## 822  1909     6 23.047 1909-06-01
    ## 823  1909     7 28.350 1909-07-01
    ## 824  1909     8 28.194 1909-08-01
    ## 825  1909     9 24.862 1909-09-01
    ## 826  1909    10 18.330 1909-10-01
    ## 827  1909    11 12.168 1909-11-01
    ## 828  1909    12  5.294 1909-12-01
    ## 829  1910     1  3.087 1910-01-01
    ## 830  1910     2  3.078 1910-02-01
    ## 831  1910     3  8.143 1910-03-01
    ## 832  1910     4 12.774 1910-04-01
    ## 833  1910     5 18.559 1910-05-01
    ## 834  1910     6 24.415 1910-06-01
    ## 835  1910     7 28.378 1910-07-01
    ## 836  1910     8 27.782 1910-08-01
    ## 837  1910     9 23.054 1910-09-01
    ## 838  1910    10 17.738 1910-10-01
    ## 839  1910    11 12.153 1910-11-01
    ## 840  1910    12  3.921 1910-12-01
    ## 841  1911     1  3.131 1911-01-01
    ## 842  1911     2  4.537 1911-02-01
    ## 843  1911     3  8.644 1911-03-01
    ## 844  1911     4 14.305 1911-04-01
    ## 845  1911     5 18.675 1911-05-01
    ## 846  1911     6 23.128 1911-06-01
    ## 847  1911     7 27.247 1911-07-01
    ## 848  1911     8 27.445 1911-08-01
    ## 849  1911     9 24.662 1911-09-01
    ## 850  1911    10 16.966 1911-10-01
    ## 851  1911    11 11.637 1911-11-01
    ## 852  1911    12  5.352 1911-12-01
    ## 853  1912     1  2.586 1912-01-01
    ## 854  1912     2  6.599 1912-02-01
    ## 855  1912     3  8.287 1912-03-01
    ## 856  1912     4 15.242 1912-04-01
    ## 857  1912     5 20.133 1912-05-01
    ## 858  1912     6 24.984 1912-06-01
    ## 859  1912     7 28.054 1912-07-01
    ## 860  1912     8 27.251 1912-08-01
    ## 861  1912     9 22.169 1912-09-01
    ## 862  1912    10 17.330 1912-10-01
    ## 863  1912    11  9.799 1912-11-01
    ## 864  1912    12  4.852 1912-12-01
    ## 865  1913     1  3.228 1913-01-01
    ## 866  1913     2  4.228 1913-02-01
    ## 867  1913     3  7.886 1913-03-01
    ## 868  1913     4 13.658 1913-04-01
    ## 869  1913     5 18.964 1913-05-01
    ## 870  1913     6 24.174 1913-06-01
    ## 871  1913     7 26.767 1913-07-01
    ## 872  1913     8 28.103 1913-08-01
    ## 873  1913     9 22.868 1913-09-01
    ## 874  1913    10 17.861 1913-10-01
    ## 875  1913    11 11.896 1913-11-01
    ## 876  1913    12  4.655 1913-12-01
    ## 877  1914     1  5.109 1914-01-01
    ## 878  1914     2  6.328 1914-02-01
    ## 879  1914     3 10.394 1914-03-01
    ## 880  1914     4 14.132 1914-04-01
    ## 881  1914     5 19.201 1914-05-01
    ## 882  1914     6 24.942 1914-06-01
    ## 883  1914     7 29.883 1914-07-01
    ## 884  1914     8 28.056 1914-08-01
    ## 885  1914     9 23.113 1914-09-01
    ## 886  1914    10 18.623 1914-10-01
    ## 887  1914    11 12.384 1914-11-01
    ## 888  1914    12  6.056 1914-12-01
    ## 889  1915     1  3.469 1915-01-01
    ## 890  1915     2  4.786 1915-02-01
    ## 891  1915     3  8.394 1915-03-01
    ## 892  1915     4 13.329 1915-04-01
    ## 893  1915     5 20.297 1915-05-01
    ## 894  1915     6 24.646 1915-06-01
    ## 895  1915     7 28.110 1915-07-01
    ## 896  1915     8 27.583 1915-08-01
    ## 897  1915     9 22.901 1915-09-01
    ## 898  1915    10 19.394 1915-10-01
    ## 899  1915    11 13.090 1915-11-01
    ## 900  1915    12  7.289 1915-12-01
    ## 901  1916     1  4.621 1916-01-01
    ## 902  1916     2  4.784 1916-02-01
    ## 903  1916     3  7.701 1916-03-01
    ## 904  1916     4 14.646 1916-04-01
    ## 905  1916     5 19.667 1916-05-01
    ## 906  1916     6 24.461 1916-06-01
    ## 907  1916     7 27.123 1916-07-01
    ## 908  1916     8 26.945 1916-08-01
    ## 909  1916     9 23.950 1916-09-01
    ## 910  1916    10 17.545 1916-10-01
    ## 911  1916    11 12.160 1916-11-01
    ## 912  1916    12  5.394 1916-12-01
    ## 913  1917     1  0.224 1917-01-01
    ## 914  1917     2  3.694 1917-02-01
    ## 915  1917     3  7.756 1917-03-01
    ## 916  1917     4 14.973 1917-04-01
    ## 917  1917     5 19.483 1917-05-01
    ## 918  1917     6 24.241 1917-06-01
    ## 919  1917     7 27.945 1917-07-01
    ## 920  1917     8 27.893 1917-08-01
    ## 921  1917     9 24.536 1917-09-01
    ## 922  1917    10 17.541 1917-10-01
    ## 923  1917    11 10.068 1917-11-01
    ## 924  1917    12  2.730 1917-12-01
    ## 925  1918     1  0.637 1918-01-01
    ## 926  1918     2  5.067 1918-02-01
    ## 927  1918     3  8.646 1918-03-01
    ## 928  1918     4 14.149 1918-04-01
    ## 929  1918     5 19.518 1918-05-01
    ## 930  1918     6 23.732 1918-06-01
    ## 931  1918     7 27.414 1918-07-01
    ## 932  1918     8 28.052 1918-08-01
    ## 933  1918     9 23.463 1918-09-01
    ## 934  1918    10 18.157 1918-10-01
    ## 935  1918    11 11.321 1918-11-01
    ## 936  1918    12  6.456 1918-12-01
    ## 937  1919     1  2.832 1919-01-01
    ## 938  1919     2  4.424 1919-02-01
    ## 939  1919     3 10.129 1919-03-01
    ## 940  1919     4 15.992 1919-04-01
    ## 941  1919     5 20.846 1919-05-01
    ## 942  1919     6 24.944 1919-06-01
    ## 943  1919     7 27.128 1919-07-01
    ## 944  1919     8 27.976 1919-08-01
    ## 945  1919     9 22.699 1919-09-01
    ## 946  1919    10 17.776 1919-10-01
    ## 947  1919    11 11.440 1919-11-01
    ## 948  1919    12  5.017 1919-12-01
    ## 949  1920     1  3.402 1920-01-01
    ## 950  1920     2  2.780 1920-02-01
    ## 951  1920     3  8.472 1920-03-01
    ## 952  1920     4 13.648 1920-04-01
    ## 953  1920     5 19.332 1920-05-01
    ## 954  1920     6 24.126 1920-06-01
    ## 955  1920     7 28.140 1920-07-01
    ## 956  1920     8 28.069 1920-08-01
    ## 957  1920     9 24.431 1920-09-01
    ## 958  1920    10 18.746 1920-10-01
    ## 959  1920    11 13.650 1920-11-01
    ## 960  1920    12  7.449 1920-12-01
    ## 961  1921     1  3.426 1921-01-01
    ## 962  1921     2  5.777 1921-02-01
    ## 963  1921     3  8.678 1921-03-01
    ## 964  1921     4 13.943 1921-04-01
    ## 965  1921     5 19.612 1921-05-01
    ## 966  1921     6 22.572 1921-06-01
    ## 967  1921     7 28.254 1921-07-01
    ## 968  1921     8 27.549 1921-08-01
    ## 969  1921     9 22.331 1921-09-01
    ## 970  1921    10 16.685 1921-10-01
    ## 971  1921    11 11.503 1921-11-01
    ## 972  1921    12  6.682 1921-12-01
    ## 973  1922     1  2.830 1922-01-01
    ## 974  1922     2  5.977 1922-02-01
    ## 975  1922     3  9.373 1922-03-01
    ## 976  1922     4 15.348 1922-04-01
    ## 977  1922     5 19.711 1922-05-01
    ## 978  1922     6 24.988 1922-06-01
    ## 979  1922     7 28.343 1922-07-01
    ## 980  1922     8 28.386 1922-08-01
    ## 981  1922     9 23.408 1922-09-01
    ## 982  1922    10 18.206 1922-10-01
    ## 983  1922    11 11.175 1922-11-01
    ## 984  1922    12  5.533 1922-12-01
    ## 985  1923     1  2.546 1923-01-01
    ## 986  1923     2  3.246 1923-02-01
    ## 987  1923     3 10.400 1923-03-01
    ## 988  1923     4 13.840 1923-04-01
    ## 989  1923     5 19.897 1923-05-01
    ## 990  1923     6 24.154 1923-06-01
    ## 991  1923     7 27.125 1923-07-01
    ## 992  1923     8 28.673 1923-08-01
    ## 993  1923     9 23.199 1923-09-01
    ## 994  1923    10 17.599 1923-10-01
    ## 995  1923    11 12.400 1923-11-01
    ## 996  1923    12  6.190 1923-12-01
    ## 997  1924     1  4.104 1924-01-01
    ## 998  1924     2  4.281 1924-02-01
    ## 999  1924     3  7.000 1924-03-01
    ## 1000 1924     4 15.675 1924-04-01
    ## 1001 1924     5 19.152 1924-05-01
    ## 1002 1924     6 23.723 1924-06-01
    ## 1003 1924     7 28.745 1924-07-01
    ## 1004 1924     8 28.092 1924-08-01
    ## 1005 1924     9 23.786 1924-09-01
    ## 1006 1924    10 18.137 1924-10-01
    ## 1007 1924    11 10.314 1924-11-01
    ## 1008 1924    12  5.650 1924-12-01
    ## 1009 1925     1  2.359 1925-01-01
    ## 1010 1925     2  3.215 1925-02-01
    ## 1011 1925     3  9.100 1925-03-01
    ## 1012 1925     4 13.185 1925-04-01
    ## 1013 1925     5 20.247 1925-05-01
    ## 1014 1925     6 25.236 1925-06-01
    ## 1015 1925     7 27.129 1925-07-01
    ## 1016 1925     8 27.345 1925-08-01
    ## 1017 1925     9 23.103 1925-09-01
    ## 1018 1925    10 18.269 1925-10-01
    ## 1019 1925    11 13.477 1925-11-01
    ## 1020 1925    12  5.176 1925-12-01
    ## 1021 1926     1  3.364 1926-01-01
    ## 1022 1926     2  4.475 1926-02-01
    ## 1023 1926     3  9.544 1926-03-01
    ## 1024 1926     4 14.545 1926-04-01
    ## 1025 1926     5 21.433 1926-05-01
    ## 1026 1926     6 22.900 1926-06-01
    ## 1027 1926     7 28.217 1926-07-01
    ## 1028 1926     8 29.548 1926-08-01
    ## 1029 1926     9 24.711 1926-09-01
    ## 1030 1926    10 16.693 1926-10-01
    ## 1031 1926    11 12.486 1926-11-01
    ## 1032 1926    12  4.610 1926-12-01
    ## 1033 1927     1  3.299 1927-01-01
    ## 1034 1927     2  3.980 1927-02-01
    ## 1035 1927     3  7.801 1927-03-01
    ## 1036 1927     4 14.423 1927-04-01
    ## 1037 1927     5 19.938 1927-05-01
    ## 1038 1927     6 24.306 1927-06-01
    ## 1039 1927     7 29.088 1927-07-01
    ## 1040 1927     8 28.538 1927-08-01
    ## 1041 1927     9 23.448 1927-09-01
    ## 1042 1927    10 17.598 1927-10-01
    ## 1043 1927    11 13.735 1927-11-01
    ## 1044 1927    12  7.347 1927-12-01
    ## 1045 1928     1  3.633 1928-01-01
    ## 1046 1928     2  4.515 1928-02-01
    ## 1047 1928     3  9.335 1928-03-01
    ## 1048 1928     4 15.240 1928-04-01
    ## 1049 1928     5 21.016 1928-05-01
    ## 1050 1928     6 24.149 1928-06-01
    ## 1051 1928     7 28.220 1928-07-01
    ## 1052 1928     8 26.766 1928-08-01
    ## 1053 1928     9 23.569 1928-09-01
    ## 1054 1928    10 17.491 1928-10-01
    ## 1055 1928    11 12.206 1928-11-01
    ## 1056 1928    12  6.263 1928-12-01
    ## 1057 1929     1  3.230 1929-01-01
    ## 1058 1929     2  3.863 1929-02-01
    ## 1059 1929     3  9.349 1929-03-01
    ## 1060 1929     4 15.462 1929-04-01
    ## 1061 1929     5 20.493 1929-05-01
    ## 1062 1929     6 24.653 1929-06-01
    ## 1063 1929     7 28.474 1929-07-01
    ## 1064 1929     8 28.374 1929-08-01
    ## 1065 1929     9 23.421 1929-09-01
    ## 1066 1929    10 17.909 1929-10-01
    ## 1067 1929    11 11.033 1929-11-01
    ## 1068 1929    12  5.239 1929-12-01
    ## 1069 1930     1  0.705 1930-01-01
    ## 1070 1930     2  6.078 1930-02-01
    ## 1071 1930     3 10.337 1930-03-01
    ## 1072 1930     4 15.017 1930-04-01
    ## 1073 1930     5 20.603 1930-05-01
    ## 1074 1930     6 24.330 1930-06-01
    ## 1075 1930     7 29.123 1930-07-01
    ## 1076 1930     8 28.349 1930-08-01
    ## 1077 1930     9 22.906 1930-09-01
    ## 1078 1930    10 17.976 1930-10-01
    ## 1079 1930    11 10.519 1930-11-01
    ## 1080 1930    12  6.774 1930-12-01
    ## 1081 1931     1  2.921 1931-01-01
    ## 1082 1931     2  3.143 1931-02-01
    ## 1083 1931     3 10.020 1931-03-01
    ## 1084 1931     4 13.888 1931-04-01
    ## 1085 1931     5 18.677 1931-05-01
    ## 1086 1931     6 24.922 1931-06-01
    ## 1087 1931     7 26.280 1931-07-01
    ## 1088 1931     8 28.511 1931-08-01
    ## 1089 1931     9 22.937 1931-09-01
    ## 1090 1931    10 17.361 1931-10-01
    ## 1091 1931    11 13.240 1931-11-01
    ## 1092 1931    12  5.776 1931-12-01
    ## 1093 1932     1  4.865 1932-01-01
    ## 1094 1932     2  3.184 1932-02-01
    ## 1095 1932     3  8.910 1932-03-01
    ## 1096 1932     4 14.813 1932-04-01
    ## 1097 1932     5 19.763 1932-05-01
    ## 1098 1932     6 23.641 1932-06-01
    ## 1099 1932     7 29.785 1932-07-01
    ## 1100 1932     8 28.598 1932-08-01
    ## 1101 1932     9 22.910 1932-09-01
    ## 1102 1932    10 17.467 1932-10-01
    ## 1103 1932    11 12.235 1932-11-01
    ## 1104 1932    12  6.332 1932-12-01
    ## 1105 1933     1  0.225 1933-01-01
    ## 1106 1933     2  3.994 1933-02-01
    ## 1107 1933     3  7.142 1933-03-01
    ## 1108 1933     4 13.857 1933-04-01
    ## 1109 1933     5 21.097 1933-05-01
    ## 1110 1933     6 23.933 1933-06-01
    ## 1111 1933     7 29.291 1933-07-01
    ## 1112 1933     8 28.988 1933-08-01
    ## 1113 1933     9 24.335 1933-09-01
    ## 1114 1933    10 17.592 1933-10-01
    ## 1115 1933    11 12.581 1933-11-01
    ## 1116 1933    12  7.891 1933-12-01
    ## 1117 1934     1  0.458 1934-01-01
    ## 1118 1934     2  4.076 1934-02-01
    ## 1119 1934     3  8.239 1934-03-01
    ## 1120 1934     4 12.421 1934-04-01
    ## 1121 1934     5 20.564 1934-05-01
    ## 1122 1934     6 25.476 1934-06-01
    ## 1123 1934     7 30.392 1934-07-01
    ## 1124 1934     8 29.513 1934-08-01
    ## 1125 1934     9 23.713 1934-09-01
    ## 1126 1934    10 16.570 1934-10-01
    ## 1127 1934    11 10.982 1934-11-01
    ## 1128 1934    12  7.747 1934-12-01
    ## 1129 1935     1  3.851 1935-01-01
    ## 1130 1935     2  5.671 1935-02-01
    ## 1131 1935     3 11.453 1935-03-01
    ## 1132 1935     4 14.170 1935-04-01
    ## 1133 1935     5 20.930 1935-05-01
    ## 1134 1935     6 24.336 1935-06-01
    ## 1135 1935     7 28.591 1935-07-01
    ## 1136 1935     8 28.144 1935-08-01
    ## 1137 1935     9 23.161 1935-09-01
    ## 1138 1935    10 19.568 1935-10-01
    ## 1139 1935    11 12.959 1935-11-01
    ## 1140 1935    12  3.168 1935-12-01
    ## 1141 1936     1  1.260 1936-01-01
    ## 1142 1936     2  3.034 1936-02-01
    ## 1143 1936     3  5.831 1936-03-01
    ## 1144 1936     4 14.381 1936-04-01
    ## 1145 1936     5 18.420 1936-05-01
    ## 1146 1936     6 24.736 1936-06-01
    ## 1147 1936     7 27.617 1936-07-01
    ## 1148 1936     8 27.269 1936-08-01
    ## 1149 1936     9 24.012 1936-09-01
    ## 1150 1936    10 18.740 1936-10-01
    ## 1151 1936    11 12.451 1936-11-01
    ## 1152 1936    12  5.665 1936-12-01
    ## 1153 1937     1  4.057 1937-01-01
    ## 1154 1937     2  4.845 1937-02-01
    ## 1155 1937     3  8.978 1937-03-01
    ## 1156 1937     4 14.700 1937-04-01
    ## 1157 1937     5 20.146 1937-05-01
    ## 1158 1937     6 23.068 1937-06-01
    ## 1159 1937     7 28.661 1937-07-01
    ## 1160 1937     8 28.131 1937-08-01
    ## 1161 1937     9 24.158 1937-09-01
    ## 1162 1937    10 18.500 1937-10-01
    ## 1163 1937    11 11.796 1937-11-01
    ## 1164 1937    12  6.038 1937-12-01
    ## 1165 1938     1  2.059 1938-01-01
    ## 1166 1938     2  4.532 1938-02-01
    ## 1167 1938     3  8.963 1938-03-01
    ## 1168 1938     4 16.311 1938-04-01
    ## 1169 1938     5 21.731 1938-05-01
    ## 1170 1938     6 23.870 1938-06-01
    ## 1171 1938     7 28.272 1938-07-01
    ## 1172 1938     8 27.907 1938-08-01
    ## 1173 1938     9 23.430 1938-09-01
    ## 1174 1938    10 19.996 1938-10-01
    ## 1175 1938    11 11.907 1938-11-01
    ## 1176 1938    12  6.903 1938-12-01
    ## 1177 1939     1  3.746 1939-01-01
    ## 1178 1939     2  5.085 1939-02-01
    ## 1179 1939     3  9.581 1939-03-01
    ## 1180 1939     4 14.015 1939-04-01
    ## 1181 1939     5 20.145 1939-05-01
    ## 1182 1939     6 25.147 1939-06-01
    ## 1183 1939     7 27.837 1939-07-01
    ## 1184 1939     8 27.885 1939-08-01
    ## 1185 1939     9 23.502 1939-09-01
    ## 1186 1939    10 18.880 1939-10-01
    ## 1187 1939    11 12.068 1939-11-01
    ## 1188 1939    12  5.640 1939-12-01
    ## 1189 1940     1  3.496 1940-01-01
    ## 1190 1940     2  4.740 1940-02-01
    ## 1191 1940     3  8.622 1940-03-01
    ## 1192 1940     4 14.847 1940-04-01
    ## 1193 1940     5 20.262 1940-05-01
    ## 1194 1940     6 24.678 1940-06-01
    ## 1195 1940     7 28.848 1940-07-01
    ## 1196 1940     8 27.615 1940-08-01
    ## 1197 1940     9 23.264 1940-09-01
    ## 1198 1940    10 18.403 1940-10-01
    ## 1199 1940    11 12.617 1940-11-01
    ## 1200 1940    12  7.551 1940-12-01
    ## 1201 1941     1  5.060 1941-01-01
    ## 1202 1941     2  5.130 1941-02-01
    ## 1203 1941     3 10.243 1941-03-01
    ## 1204 1941     4 15.019 1941-04-01
    ## 1205 1941     5 20.473 1941-05-01
    ## 1206 1941     6 24.919 1941-06-01
    ## 1207 1941     7 26.831 1941-07-01
    ## 1208 1941     8 27.301 1941-08-01
    ## 1209 1941     9 22.755 1941-09-01
    ## 1210 1941    10 18.403 1941-10-01
    ## 1211 1941    11 12.746 1941-11-01
    ## 1212 1941    12  7.817 1941-12-01
    ## 1213 1942     1  3.198 1942-01-01
    ## 1214 1942     2  3.446 1942-02-01
    ## 1215 1942     3 10.860 1942-03-01
    ## 1216 1942     4 15.122 1942-04-01
    ## 1217 1942     5 19.317 1942-05-01
    ## 1218 1942     6 24.050 1942-06-01
    ## 1219 1942     7 29.546 1942-07-01
    ## 1220 1942     8 28.825 1942-08-01
    ## 1221 1942     9 24.311 1942-09-01
    ## 1222 1942    10 18.134 1942-10-01
    ## 1223 1942    11 12.503 1942-11-01
    ## 1224 1942    12  5.207 1942-12-01
    ## 1225 1943     1  2.053 1943-01-01
    ## 1226 1943     2  4.658 1943-02-01
    ## 1227 1943     3  9.714 1943-03-01
    ## 1228 1943     4 13.999 1943-04-01
    ## 1229 1943     5 20.507 1943-05-01
    ## 1230 1943     6 23.727 1943-06-01
    ## 1231 1943     7 27.147 1943-07-01
    ## 1232 1943     8 27.993 1943-08-01
    ## 1233 1943     9 24.056 1943-09-01
    ## 1234 1943    10 18.507 1943-10-01
    ## 1235 1943    11 12.422 1943-11-01
    ## 1236 1943    12  7.352 1943-12-01
    ## 1237 1944     1  4.945 1944-01-01
    ## 1238 1944     2  4.568 1944-02-01
    ## 1239 1944     3  9.046 1944-03-01
    ## 1240 1944     4 14.150 1944-04-01
    ## 1241 1944     5 20.129 1944-05-01
    ## 1242 1944     6 24.826 1944-06-01
    ## 1243 1944     7 29.355 1944-07-01
    ## 1244 1944     8 27.904 1944-08-01
    ## 1245 1944     9 24.610 1944-09-01
    ## 1246 1944    10 18.176 1944-10-01
    ## 1247 1944    11 13.575 1944-11-01
    ## 1248 1944    12  2.354 1944-12-01
    ## 1249 1945     1  1.512 1945-01-01
    ## 1250 1945     2  2.265 1945-02-01
    ## 1251 1945     3  8.495 1945-03-01
    ## 1252 1945     4 15.093 1945-04-01
    ## 1253 1945     5 19.338 1945-05-01
    ## 1254 1945     6 24.631 1945-06-01
    ## 1255 1945     7 28.032 1945-07-01
    ## 1256 1945     8 28.334 1945-08-01
    ## 1257 1945     9 24.487 1945-09-01
    ## 1258 1945    10 17.621 1945-10-01
    ## 1259 1945    11 14.884 1945-11-01
    ## 1260 1945    12  6.213 1945-12-01
    ## 1261 1946     1  5.211 1946-01-01
    ## 1262 1946     2  6.972 1946-02-01
    ## 1263 1946     3  8.626 1946-03-01
    ## 1264 1946     4 16.520 1946-04-01
    ## 1265 1946     5 19.574 1946-05-01
    ## 1266 1946     6 25.617 1946-06-01
    ## 1267 1946     7 27.579 1946-07-01
    ## 1268 1946     8 28.923 1946-08-01
    ## 1269 1946     9 25.116 1946-09-01
    ## 1270 1946    10 18.035 1946-10-01
    ## 1271 1946    11 13.708 1946-11-01
    ## 1272 1946    12  4.554 1946-12-01
    ## 1273 1947     1  4.221 1947-01-01
    ## 1274 1947     2  2.754 1947-02-01
    ## 1275 1947     3  9.425 1947-03-01
    ## 1276 1947     4 15.254 1947-04-01
    ## 1277 1947     5 19.038 1947-05-01
    ## 1278 1947     6 23.126 1947-06-01
    ## 1279 1947     7 28.704 1947-07-01
    ## 1280 1947     8 28.934 1947-08-01
    ## 1281 1947     9 24.806 1947-09-01
    ## 1282 1947    10 16.437 1947-10-01
    ## 1283 1947    11 12.717 1947-11-01
    ## 1284 1947    12  5.067 1947-12-01
    ## 1285 1948     1  3.020 1948-01-01
    ## 1286 1948     2  5.667 1948-02-01
    ## 1287 1948     3  9.134 1948-03-01
    ## 1288 1948     4 16.014 1948-04-01
    ## 1289 1948     5 19.510 1948-05-01
    ## 1290 1948     6 25.007 1948-06-01
    ## 1291 1948     7 28.000 1948-07-01
    ## 1292 1948     8 28.725 1948-08-01
    ## 1293 1948     9 24.551 1948-09-01
    ## 1294 1948    10 18.310 1948-10-01
    ## 1295 1948    11 11.144 1948-11-01
    ## 1296 1948    12  8.805 1948-12-01
    ## 1297 1949     1  3.397 1949-01-01
    ## 1298 1949     2  6.753 1949-02-01
    ## 1299 1949     3  9.435 1949-03-01
    ## 1300 1949     4 15.102 1949-04-01
    ## 1301 1949     5 21.221 1949-05-01
    ## 1302 1949     6 23.436 1949-06-01
    ## 1303 1949     7 27.640 1949-07-01
    ## 1304 1949     8 27.936 1949-08-01
    ## 1305 1949     9 25.457 1949-09-01
    ## 1306 1949    10 18.480 1949-10-01
    ## 1307 1949    11 12.563 1949-11-01
    ## 1308 1949    12  7.302 1949-12-01
    ## 1309 1950     1  6.168 1950-01-01
    ## 1310 1950     2  4.670 1950-02-01
    ## 1311 1950     3 10.138 1950-03-01
    ## 1312 1950     4 15.077 1950-04-01
    ## 1313 1950     5 20.483 1950-05-01
    ## 1314 1950     6 24.401 1950-06-01
    ## 1315 1950     7 28.725 1950-07-01
    ## 1316 1950     8 28.006 1950-08-01
    ## 1317 1950     9 23.555 1950-09-01
    ## 1318 1950    10 18.900 1950-10-01
    ## 1319 1950    11 11.854 1950-11-01
    ## 1320 1950    12  4.772 1950-12-01
    ## 1321 1951     1  3.410 1951-01-01
    ## 1322 1951     2  5.137 1951-02-01
    ## 1323 1951     3  8.200 1951-03-01
    ## 1324 1951     4 13.225 1951-04-01
    ## 1325 1951     5 20.989 1951-05-01
    ## 1326 1951     6 23.953 1951-06-01
    ## 1327 1951     7 27.381 1951-07-01
    ## 1328 1951     8 29.071 1951-08-01
    ## 1329 1951     9 22.948 1951-09-01
    ## 1330 1951    10 19.382 1951-10-01
    ## 1331 1951    11 12.258 1951-11-01
    ## 1332 1951    12  8.217 1951-12-01
    ## 1333 1952     1  4.803 1952-01-01
    ## 1334 1952     2  3.577 1952-02-01
    ## 1335 1952     3  7.751 1952-03-01
    ## 1336 1952     4 15.598 1952-04-01
    ## 1337 1952     5 20.097 1952-05-01
    ## 1338 1952     6 25.280 1952-06-01
    ## 1339 1952     7 27.821 1952-07-01
    ## 1340 1952     8 27.680 1952-08-01
    ## 1341 1952     9 24.128 1952-09-01
    ## 1342 1952    10 17.501 1952-10-01
    ## 1343 1952    11 14.581 1952-11-01
    ## 1344 1952    12  4.352 1952-12-01
    ## 1345 1953     1  3.329 1953-01-01
    ## 1346 1953     2  5.711 1953-02-01
    ## 1347 1953     3  9.692 1953-03-01
    ## 1348 1953     4 14.463 1953-04-01
    ## 1349 1953     5 21.569 1953-05-01
    ## 1350 1953     6 25.608 1953-06-01
    ## 1351 1953     7 30.402 1953-07-01
    ## 1352 1953     8 29.988 1953-08-01
    ## 1353 1953     9 23.508 1953-09-01
    ## 1354 1953    10 19.704 1953-10-01
    ## 1355 1953    11 12.910 1953-11-01
    ## 1356 1953    12  7.530 1953-12-01
    ## 1357 1954     1  4.857 1954-01-01
    ## 1358 1954     2  5.271 1954-02-01
    ## 1359 1954     3  7.813 1954-03-01
    ## 1360 1954     4 14.343 1954-04-01
    ## 1361 1954     5 19.655 1954-05-01
    ## 1362 1954     6 22.966 1954-06-01
    ## 1363 1954     7 26.454 1954-07-01
    ## 1364 1954     8 28.676 1954-08-01
    ## 1365 1954     9 24.288 1954-09-01
    ## 1366 1954    10 17.681 1954-10-01
    ## 1367 1954    11 14.333 1954-11-01
    ## 1368 1954    12  3.890 1954-12-01
    ## 1369 1955     1  0.458 1955-01-01
    ## 1370 1955     2  6.428 1955-02-01
    ## 1371 1955     3  8.424 1955-03-01
    ## 1372 1955     4 15.028 1955-04-01
    ## 1373 1955     5 20.161 1955-05-01
    ## 1374 1955     6 24.801 1955-06-01
    ## 1375 1955     7 28.045 1955-07-01
    ## 1376 1955     8 27.710 1955-08-01
    ## 1377 1955     9 25.565 1955-09-01
    ## 1378 1955    10 16.913 1955-10-01
    ## 1379 1955    11 10.888 1955-11-01
    ## 1380 1955    12  8.350 1955-12-01
    ## 1381 1956     1  2.916 1956-01-01
    ## 1382 1956     2  4.520 1956-02-01
    ## 1383 1956     3  7.931 1956-03-01
    ## 1384 1956     4 15.270 1956-04-01
    ## 1385 1956     5 18.186 1956-05-01
    ## 1386 1956     6 25.260 1956-06-01
    ## 1387 1956     7 29.805 1956-07-01
    ## 1388 1956     8 27.590 1956-08-01
    ## 1389 1956     9 23.606 1956-09-01
    ## 1390 1956    10 17.403 1956-10-01
    ## 1391 1956    11 10.041 1956-11-01
    ## 1392 1956    12  4.806 1956-12-01
    ## 1393 1957     1  3.932 1957-01-01
    ## 1394 1957     2  1.915 1957-02-01
    ## 1395 1957     3  7.660 1957-03-01
    ## 1396 1957     4 14.452 1957-04-01
    ## 1397 1957     5 19.194 1957-05-01
    ## 1398 1957     6 23.642 1957-06-01
    ## 1399 1957     7 29.243 1957-07-01
    ## 1400 1957     8 27.369 1957-08-01
    ## 1401 1957     9 21.578 1957-09-01
    ## 1402 1957    10 17.117 1957-10-01
    ## 1403 1957    11 13.834 1957-11-01
    ## 1404 1957    12  7.312 1957-12-01
    ## 1405 1958     1  2.417 1958-01-01
    ## 1406 1958     2  4.848 1958-02-01
    ## 1407 1958     3  9.870 1958-03-01
    ## 1408 1958     4 15.758 1958-04-01
    ## 1409 1958     5 19.081 1958-05-01
    ## 1410 1958     6 25.096 1958-06-01
    ## 1411 1958     7 29.599 1958-07-01
    ## 1412 1958     8 27.252 1958-08-01
    ## 1413 1958     9 23.563 1958-09-01
    ## 1414 1958    10 16.158 1958-10-01
    ## 1415 1958    11 11.519 1958-11-01
    ## 1416 1958    12  7.636 1958-12-01
    ## 1417 1959     1  2.264 1959-01-01
    ## 1418 1959     2  5.655 1959-02-01
    ## 1419 1959     3 11.068 1959-03-01
    ## 1420 1959     4 15.018 1959-04-01
    ## 1421 1959     5 19.536 1959-05-01
    ## 1422 1959     6 24.535 1959-06-01
    ## 1423 1959     7 29.246 1959-07-01
    ## 1424 1959     8 29.423 1959-08-01
    ## 1425 1959     9 23.718 1959-09-01
    ## 1426 1959    10 18.680 1959-10-01
    ## 1427 1959    11 12.264 1959-11-01
    ## 1428 1959    12  7.066 1959-12-01
    ## 1429 1960     1  4.337 1960-01-01
    ## 1430 1960     2  7.100 1960-02-01
    ## 1431 1960     3 11.466 1960-03-01
    ## 1432 1960     4 14.610 1960-04-01
    ## 1433 1960     5 19.509 1960-05-01
    ## 1434 1960     6 24.528 1960-06-01
    ## 1435 1960     7 29.792 1960-07-01
    ## 1436 1960     8 27.379 1960-08-01
    ## 1437 1960     9 24.794 1960-09-01
    ## 1438 1960    10 18.168 1960-10-01
    ## 1439 1960    11 12.817 1960-11-01
    ## 1440 1960    12  4.695 1960-12-01
    ## 1441 1961     1  3.490 1961-01-01
    ## 1442 1961     2  5.561 1961-02-01
    ## 1443 1961     3  9.995 1961-03-01
    ## 1444 1961     4 15.741 1961-04-01
    ## 1445 1961     5 20.112 1961-05-01
    ## 1446 1961     6 25.444 1961-06-01
    ## 1447 1961     7 30.013 1961-07-01
    ## 1448 1961     8 29.297 1961-08-01
    ## 1449 1961     9 24.673 1961-09-01
    ## 1450 1961    10 18.868 1961-10-01
    ## 1451 1961    11 14.232 1961-11-01
    ## 1452 1961    12  6.810 1961-12-01
    ## 1453 1962     1  2.207 1962-01-01
    ## 1454 1962     2  6.481 1962-02-01
    ## 1455 1962     3  9.304 1962-03-01
    ## 1456 1962     4 13.531 1962-04-01
    ## 1457 1962     5 19.549 1962-05-01
    ## 1458 1962     6 24.231 1962-06-01
    ## 1459 1962     7 29.036 1962-07-01
    ## 1460 1962     8 28.053 1962-08-01
    ## 1461 1962     9 24.189 1962-09-01
    ## 1462 1962    10 17.860 1962-10-01
    ## 1463 1962    11 11.971 1962-11-01
    ## 1464 1962    12  6.927 1962-12-01
    ## 1465 1963     1  1.466 1963-01-01
    ## 1466 1963     2  4.036 1963-02-01
    ## 1467 1963     3 10.299 1963-03-01
    ## 1468 1963     4 15.216 1963-04-01
    ## 1469 1963     5 20.659 1963-05-01
    ## 1470 1963     6 23.744 1963-06-01
    ## 1471 1963     7 28.806 1963-07-01
    ## 1472 1963     8 28.536 1963-08-01
    ## 1473 1963     9 24.330 1963-09-01
    ## 1474 1963    10 17.010 1963-10-01
    ## 1475 1963    11 13.372 1963-11-01
    ## 1476 1963    12  6.902 1963-12-01
    ## 1477 1964     1  5.021 1964-01-01
    ## 1478 1964     2  1.706 1964-02-01
    ## 1479 1964     3  9.516 1964-03-01
    ## 1480 1964     4 17.364 1964-04-01
    ## 1481 1964     5 19.707 1964-05-01
    ## 1482 1964     6 23.964 1964-06-01
    ## 1483 1964     7 30.341 1964-07-01
    ## 1484 1964     8 28.597 1964-08-01
    ## 1485 1964     9 25.794 1964-09-01
    ## 1486 1964    10 18.765 1964-10-01
    ## 1487 1964    11 12.015 1964-11-01
    ## 1488 1964    12  6.171 1964-12-01
    ## 1489 1965     1  4.983 1965-01-01
    ## 1490 1965     2  5.947 1965-02-01
    ## 1491 1965     3  8.380 1965-03-01
    ## 1492 1965     4 13.077 1965-04-01
    ## 1493 1965     5 20.644 1965-05-01
    ## 1494 1965     6 23.427 1965-06-01
    ## 1495 1965     7 28.859 1965-07-01
    ## 1496 1965     8 27.309 1965-08-01
    ## 1497 1965     9 22.837 1965-09-01
    ## 1498 1965    10 18.313 1965-10-01
    ## 1499 1965    11 13.999 1965-11-01
    ## 1500 1965    12  4.995 1965-12-01
    ## 1501 1966     1  4.424 1966-01-01
    ## 1502 1966     2  7.054 1966-02-01
    ## 1503 1966     3 10.785 1966-03-01
    ## 1504 1966     4 14.687 1966-04-01
    ## 1505 1966     5 19.852 1966-05-01
    ## 1506 1966     6 24.618 1966-06-01
    ## 1507 1966     7 28.454 1966-07-01
    ## 1508 1966     8 29.309 1966-08-01
    ## 1509 1966     9 22.375 1966-09-01
    ## 1510 1966    10 18.337 1966-10-01
    ## 1511 1966    11 12.754 1966-11-01
    ## 1512 1966    12  5.402 1966-12-01
    ## 1513 1967     1  2.365 1967-01-01
    ## 1514 1967     2  4.397 1967-02-01
    ## 1515 1967     3  9.645 1967-03-01
    ## 1516 1967     4 14.534 1967-04-01
    ## 1517 1967     5 20.598 1967-05-01
    ## 1518 1967     6 25.589 1967-06-01
    ## 1519 1967     7 28.789 1967-07-01
    ## 1520 1967     8 30.421 1967-08-01
    ## 1521 1967     9 24.117 1967-09-01
    ## 1522 1967    10 18.179 1967-10-01
    ## 1523 1967    11 12.281 1967-11-01
    ## 1524 1967    12  1.697 1967-12-01
    ## 1525 1968     1  2.889 1968-01-01
    ## 1526 1968     2  2.081 1968-02-01
    ## 1527 1968     3  9.768 1968-03-01
    ## 1528 1968     4 14.756 1968-04-01
    ## 1529 1968     5 19.865 1968-05-01
    ## 1530 1968     6 24.178 1968-06-01
    ## 1531 1968     7 27.507 1968-07-01
    ## 1532 1968     8 28.061 1968-08-01
    ## 1533 1968     9 24.036 1968-09-01
    ## 1534 1968    10 17.181 1968-10-01
    ## 1535 1968    11 13.889 1968-11-01
    ## 1536 1968    12  9.375 1968-12-01
    ## 1537 1969     1  3.265 1969-01-01
    ## 1538 1969     2  2.226 1969-02-01
    ## 1539 1969     3  7.729 1969-03-01
    ## 1540 1969     4 15.288 1969-04-01
    ## 1541 1969     5 20.962 1969-05-01
    ## 1542 1969     6 23.523 1969-06-01
    ## 1543 1969     7 27.310 1969-07-01
    ## 1544 1969     8 28.632 1969-08-01
    ## 1545 1969     9 25.147 1969-09-01
    ## 1546 1969    10 17.996 1969-10-01
    ## 1547 1969    11 10.543 1969-11-01
    ## 1548 1969    12  5.051 1969-12-01
    ## 1549 1970     1  1.925 1970-01-01
    ## 1550 1970     2  6.362 1970-02-01
    ## 1551 1970     3  6.423 1970-03-01
    ## 1552 1970     4 13.919 1970-04-01
    ## 1553 1970     5 19.952 1970-05-01
    ## 1554 1970     6 23.169 1970-06-01
    ## 1555 1970     7 27.766 1970-07-01
    ## 1556 1970     8 28.694 1970-08-01
    ## 1557 1970     9 24.527 1970-09-01
    ## 1558 1970    10 18.414 1970-10-01
    ## 1559 1970    11 12.039 1970-11-01
    ## 1560 1970    12  6.692 1970-12-01
    ## 1561 1971     1  2.928 1971-01-01
    ## 1562 1971     2  4.355 1971-02-01
    ## 1563 1971     3  8.229 1971-03-01
    ## 1564 1971     4 14.849 1971-04-01
    ## 1565 1971     5 20.513 1971-05-01
    ## 1566 1971     6 25.676 1971-06-01
    ## 1567 1971     7 30.617 1971-07-01
    ## 1568 1971     8 29.001 1971-08-01
    ## 1569 1971     9 23.439 1971-09-01
    ## 1570 1971    10 16.519 1971-10-01
    ## 1571 1971    11 12.420 1971-11-01
    ## 1572 1971    12  6.088 1971-12-01
    ## 1573 1972     1  4.528 1972-01-01
    ## 1574 1972     2  2.369 1972-02-01
    ## 1575 1972     3  9.212 1972-03-01
    ## 1576 1972     4 14.329 1972-04-01
    ## 1577 1972     5 19.472 1972-05-01
    ## 1578 1972     6 24.322 1972-06-01
    ## 1579 1972     7 27.471 1972-07-01
    ## 1580 1972     8 27.500 1972-08-01
    ## 1581 1972     9 22.768 1972-09-01
    ## 1582 1972    10 17.712 1972-10-01
    ## 1583 1972    11 12.325 1972-11-01
    ## 1584 1972    12  6.498 1972-12-01
    ## 1585 1973     1  4.424 1973-01-01
    ## 1586 1973     2  6.598 1973-02-01
    ## 1587 1973     3 10.396 1973-03-01
    ## 1588 1973     4 16.648 1973-04-01
    ## 1589 1973     5 19.535 1973-05-01
    ## 1590 1973     6 23.788 1973-06-01
    ## 1591 1973     7 28.690 1973-07-01
    ## 1592 1973     8 29.022 1973-08-01
    ## 1593 1973     9 22.195 1973-09-01
    ## 1594 1973    10 17.762 1973-10-01
    ## 1595 1973    11 12.028 1973-11-01
    ## 1596 1973    12  4.805 1973-12-01
    ## 1597 1974     1  3.306 1974-01-01
    ## 1598 1974     2  3.931 1974-02-01
    ## 1599 1974     3  8.786 1974-03-01
    ## 1600 1974     4 16.254 1974-04-01
    ## 1601 1974     5 20.897 1974-05-01
    ## 1602 1974     6 23.303 1974-06-01
    ## 1603 1974     7 27.243 1974-07-01
    ## 1604 1974     8 27.590 1974-08-01
    ## 1605 1974     9 23.292 1974-09-01
    ## 1606 1974    10 17.788 1974-10-01
    ## 1607 1974    11 13.038 1974-11-01
    ## 1608 1974    12  5.694 1974-12-01
    ## 1609 1975     1  4.775 1975-01-01
    ## 1610 1975     2  5.704 1975-02-01
    ## 1611 1975     3  9.462 1975-03-01
    ## 1612 1975     4 14.666 1975-04-01
    ## 1613 1975     5 19.205 1975-05-01
    ## 1614 1975     6 24.092 1975-06-01
    ## 1615 1975     7 28.381 1975-07-01
    ## 1616 1975     8 28.763 1975-08-01
    ## 1617 1975     9 26.395 1975-09-01
    ## 1618 1975    10 19.121 1975-10-01
    ## 1619 1975    11 12.492 1975-11-01
    ## 1620 1975    12  4.761 1975-12-01
    ## 1621 1976     1  3.554 1976-01-01
    ## 1622 1976     2  6.652 1976-02-01
    ## 1623 1976     3  8.397 1976-03-01
    ## 1624 1976     4 14.240 1976-04-01
    ## 1625 1976     5 20.407 1976-05-01
    ## 1626 1976     6 23.880 1976-06-01
    ## 1627 1976     7 26.515 1976-07-01
    ## 1628 1976     8 29.092 1976-08-01
    ## 1629 1976     9 22.406 1976-09-01
    ## 1630 1976    10 18.226 1976-10-01
    ## 1631 1976    11  9.055 1976-11-01
    ## 1632 1976    12  6.118 1976-12-01
    ## 1633 1977     1  0.432 1977-01-01
    ## 1634 1977     2  3.726 1977-02-01
    ## 1635 1977     3 10.451 1977-03-01
    ## 1636 1977     4 16.120 1977-04-01
    ## 1637 1977     5 18.898 1977-05-01
    ## 1638 1977     6 23.875 1977-06-01
    ## 1639 1977     7 29.103 1977-07-01
    ## 1640 1977     8 26.839 1977-08-01
    ## 1641 1977     9 24.022 1977-09-01
    ## 1642 1977    10 19.671 1977-10-01
    ## 1643 1977    11 12.169 1977-11-01
    ## 1644 1977    12  8.695 1977-12-01
    ## 1645 1978     1  3.936 1978-01-01
    ## 1646 1978     2  4.801 1978-02-01
    ## 1647 1978     3  8.373 1978-03-01
    ## 1648 1978     4 15.866 1978-04-01
    ## 1649 1978     5 20.644 1978-05-01
    ## 1650 1978     6 25.472 1978-06-01
    ## 1651 1978     7 30.071 1978-07-01
    ## 1652 1978     8 29.352 1978-08-01
    ## 1653 1978     9 23.950 1978-09-01
    ## 1654 1978    10 18.157 1978-10-01
    ## 1655 1978    11 12.483 1978-11-01
    ## 1656 1978    12  6.893 1978-12-01
    ## 1657 1979     1  5.122 1979-01-01
    ## 1658 1979     2  7.021 1979-02-01
    ## 1659 1979     3  9.467 1979-03-01
    ## 1660 1979     4 14.613 1979-04-01
    ## 1661 1979     5 19.360 1979-05-01
    ## 1662 1979     6 25.710 1979-06-01
    ## 1663 1979     7 28.140 1979-07-01
    ## 1664 1979     8 28.149 1979-08-01
    ## 1665 1979     9 23.171 1979-09-01
    ## 1666 1979    10 18.690 1979-10-01
    ## 1667 1979    11 11.135 1979-11-01
    ## 1668 1979    12  7.933 1979-12-01
    ## 1669 1980     1  3.937 1980-01-01
    ## 1670 1980     2  3.432 1980-02-01
    ## 1671 1980     3  7.906 1980-03-01
    ## 1672 1980     4 14.139 1980-04-01
    ## 1673 1980     5 19.991 1980-05-01
    ## 1674 1980     6 25.227 1980-06-01
    ## 1675 1980     7 27.403 1980-07-01
    ## 1676 1980     8 25.243 1980-08-01
    ## 1677 1980     9 22.341 1980-09-01
    ## 1678 1980    10 17.923 1980-10-01
    ## 1679 1980    11 14.115 1980-11-01
    ## 1680 1980    12  5.238 1980-12-01
    ## 1681 1981     1  2.158 1981-01-01
    ## 1682 1981     2  5.157 1981-02-01
    ## 1683 1981     3 10.909 1981-03-01
    ## 1684 1981     4 15.315 1981-04-01
    ## 1685 1981     5 20.653 1981-05-01
    ## 1686 1981     6 24.791 1981-06-01
    ## 1687 1981     7 28.927 1981-07-01
    ## 1688 1981     8 28.229 1981-08-01
    ## 1689 1981     9 22.590 1981-09-01
    ## 1690 1981    10 16.353 1981-10-01
    ## 1691 1981    11 10.516 1981-11-01
    ## 1692 1981    12  5.380 1981-12-01
    ## 1693 1982     1  3.917 1982-01-01
    ## 1694 1982     2  4.802 1982-02-01
    ## 1695 1982     3  9.423 1982-03-01
    ## 1696 1982     4 14.223 1982-04-01
    ## 1697 1982     5 22.215 1982-05-01
    ## 1698 1982     6 23.942 1982-06-01
    ## 1699 1982     7 27.000 1982-07-01
    ## 1700 1982     8 27.508 1982-08-01
    ## 1701 1982     9 22.745 1982-09-01
    ## 1702 1982    10 19.580 1982-10-01
    ## 1703 1982    11 13.377 1982-11-01
    ## 1704 1982    12  5.620 1982-12-01
    ## 1705 1983     1  3.875 1983-01-01
    ## 1706 1983     2  4.811 1983-02-01
    ## 1707 1983     3  9.023 1983-03-01
    ## 1708 1983     4 15.716 1983-04-01
    ## 1709 1983     5 21.255 1983-05-01
    ## 1710 1983     6 23.223 1983-06-01
    ## 1711 1983     7 28.018 1983-07-01
    ## 1712 1983     8 28.260 1983-08-01
    ## 1713 1983     9 24.889 1983-09-01
    ## 1714 1983    10 18.414 1983-10-01
    ## 1715 1983    11 11.882 1983-11-01
    ## 1716 1983    12  5.290 1983-12-01
    ## 1717 1984     1  1.688 1984-01-01
    ## 1718 1984     2  2.822 1984-02-01
    ## 1719 1984     3  8.915 1984-03-01
    ## 1720 1984     4 14.030 1984-04-01
    ## 1721 1984     5 19.526 1984-05-01
    ## 1722 1984     6 24.760 1984-06-01
    ## 1723 1984     7 28.919 1984-07-01
    ## 1724 1984     8 28.281 1984-08-01
    ## 1725 1984     9 23.206 1984-09-01
    ## 1726 1984    10 18.188 1984-10-01
    ## 1727 1984    11 14.180 1984-11-01
    ## 1728 1984    12  5.029 1984-12-01
    ## 1729 1985     1  3.439 1985-01-01
    ## 1730 1985     2  5.121 1985-02-01
    ## 1731 1985     3  7.140 1985-03-01
    ## 1732 1985     4 15.541 1985-04-01
    ## 1733 1985     5 20.986 1985-05-01
    ## 1734 1985     6 23.727 1985-06-01
    ## 1735 1985     7 28.564 1985-07-01
    ## 1736 1985     8 28.202 1985-08-01
    ## 1737 1985     9 24.988 1985-09-01
    ## 1738 1985    10 18.625 1985-10-01
    ## 1739 1985    11 11.897 1985-11-01
    ## 1740 1985    12  4.212 1985-12-01
    ## 1741 1986     1  3.414 1986-01-01
    ## 1742 1986     2  3.998 1986-02-01
    ## 1743 1986     3  9.350 1986-03-01
    ## 1744 1986     4 14.596 1986-04-01
    ## 1745 1986     5 20.824 1986-05-01
    ## 1746 1986     6 24.674 1986-06-01
    ## 1747 1986     7 27.768 1986-07-01
    ## 1748 1986     8 27.883 1986-08-01
    ## 1749 1986     9 22.878 1986-09-01
    ## 1750 1986    10 17.237 1986-10-01
    ## 1751 1986    11 11.963 1986-11-01
    ## 1752 1986    12  6.641 1986-12-01
    ## 1753 1987     1  4.970 1987-01-01
    ## 1754 1987     2  6.202 1987-02-01
    ## 1755 1987     3  8.434 1987-03-01
    ## 1756 1987     4 14.188 1987-04-01
    ## 1757 1987     5 19.803 1987-05-01
    ## 1758 1987     6 23.227 1987-06-01
    ## 1759 1987     7 27.295 1987-07-01
    ## 1760 1987     8 28.611 1987-08-01
    ## 1761 1987     9 23.016 1987-09-01
    ## 1762 1987    10 19.471 1987-10-01
    ## 1763 1987    11 13.048 1987-11-01
    ## 1764 1987    12  6.010 1987-12-01
    ## 1765 1988     1  4.592 1988-01-01
    ## 1766 1988     2  3.880 1988-02-01
    ## 1767 1988     3  7.626 1988-03-01
    ## 1768 1988     4 15.609 1988-04-01
    ## 1769 1988     5 20.264 1988-05-01
    ## 1770 1988     6 24.682 1988-06-01
    ## 1771 1988     7 30.172 1988-07-01
    ## 1772 1988     8 27.203 1988-08-01
    ## 1773 1988     9 23.021 1988-09-01
    ## 1774 1988    10 18.463 1988-10-01
    ## 1775 1988    11 11.736 1988-11-01
    ## 1776 1988    12  6.829 1988-12-01
    ## 1777 1989     1  4.714 1989-01-01
    ## 1778 1989     2  5.324 1989-02-01
    ## 1779 1989     3  9.867 1989-03-01
    ## 1780 1989     4 15.115 1989-04-01
    ## 1781 1989     5 19.697 1989-05-01
    ## 1782 1989     6 24.495 1989-06-01
    ## 1783 1989     7 26.872 1989-07-01
    ## 1784 1989     8 27.321 1989-08-01
    ## 1785 1989     9 23.647 1989-09-01
    ## 1786 1989    10 18.436 1989-10-01
    ## 1787 1989    11 12.034 1989-11-01
    ## 1788 1989    12  7.332 1989-12-01
    ## 1789 1990     1  4.528 1990-01-01
    ## 1790 1990     2  5.604 1990-02-01
    ## 1791 1990     3 11.685 1990-03-01
    ## 1792 1990     4 15.206 1990-04-01
    ## 1793 1990     5 20.966 1990-05-01
    ## 1794 1990     6 26.138 1990-06-01
    ## 1795 1990     7 30.500 1990-07-01
    ## 1796 1990     8 29.211 1990-08-01
    ## 1797 1990     9 23.890 1990-09-01
    ## 1798 1990    10 18.188 1990-10-01
    ## 1799 1990    11 14.392 1990-11-01
    ## 1800 1990    12  6.794 1990-12-01
    ## 1801 1991     1  4.084 1991-01-01
    ## 1802 1991     2  6.061 1991-02-01
    ## 1803 1991     3  8.725 1991-03-01
    ## 1804 1991     4 14.217 1991-04-01
    ## 1805 1991     5 19.944 1991-05-01
    ## 1806 1991     6 24.683 1991-06-01
    ## 1807 1991     7 28.570 1991-07-01
    ## 1808 1991     8 27.192 1991-08-01
    ## 1809 1991     9 23.705 1991-09-01
    ## 1810 1991    10 17.797 1991-10-01
    ## 1811 1991    11 12.180 1991-11-01
    ## 1812 1991    12  6.498 1991-12-01
    ## 1813 1992     1  3.858 1992-01-01
    ## 1814 1992     2  6.488 1992-02-01
    ## 1815 1992     3  8.329 1992-03-01
    ## 1816 1992     4 15.661 1992-04-01
    ## 1817 1992     5 20.627 1992-05-01
    ## 1818 1992     6 23.554 1992-06-01
    ## 1819 1992     7 28.328 1992-07-01
    ## 1820 1992     8 27.489 1992-08-01
    ## 1821 1992     9 23.761 1992-09-01
    ## 1822 1992    10 16.814 1992-10-01
    ## 1823 1992    11 11.304 1992-11-01
    ## 1824 1992    12  7.456 1992-12-01
    ## 1825 1993     1  2.671 1993-01-01
    ## 1826 1993     2  6.823 1993-02-01
    ## 1827 1993     3  9.353 1993-03-01
    ## 1828 1993     4 15.058 1993-04-01
    ## 1829 1993     5 19.393 1993-05-01
    ## 1830 1993     6 24.938 1993-06-01
    ## 1831 1993     7 27.132 1993-07-01
    ## 1832 1993     8 26.562 1993-08-01
    ## 1833 1993     9 23.868 1993-09-01
    ## 1834 1993    10 17.687 1993-10-01
    ## 1835 1993    11 12.908 1993-11-01
    ## 1836 1993    12  5.346 1993-12-01
    ## 1837 1994     1  4.536 1994-01-01
    ## 1838 1994     2  5.054 1994-02-01
    ## 1839 1994     3  8.971 1994-03-01
    ## 1840 1994     4 16.117 1994-04-01
    ## 1841 1994     5 22.877 1994-05-01
    ## 1842 1994     6 25.142 1994-06-01
    ## 1843 1994     7 30.858 1994-07-01
    ## 1844 1994     8 29.439 1994-08-01
    ## 1845 1994     9 23.796 1994-09-01
    ## 1846 1994    10 18.045 1994-10-01
    ## 1847 1994    11 14.931 1994-11-01
    ## 1848 1994    12  8.337 1994-12-01
    ## 1849 1995     1  4.157 1995-01-01
    ## 1850 1995     2  5.502 1995-02-01
    ## 1851 1995     3 10.482 1995-03-01
    ## 1852 1995     4 14.505 1995-04-01
    ## 1853 1995     5 20.159 1995-05-01
    ## 1854 1995     6 23.672 1995-06-01
    ## 1855 1995     7 28.717 1995-07-01
    ## 1856 1995     8 29.107 1995-08-01
    ## 1857 1995     9 24.569 1995-09-01
    ## 1858 1995    10 18.568 1995-10-01
    ## 1859 1995    11 11.472 1995-11-01
    ## 1860 1995    12  5.408 1995-12-01
    ## 1861 1996     1  3.785 1996-01-01
    ## 1862 1996     2  4.307 1996-02-01
    ## 1863 1996     3  8.230 1996-03-01
    ## 1864 1996     4 13.960 1996-04-01
    ## 1865 1996     5 20.173 1996-05-01
    ## 1866 1996     6 25.261 1996-06-01
    ## 1867 1996     7 27.676 1996-07-01
    ## 1868 1996     8 28.255 1996-08-01
    ## 1869 1996     9 24.938 1996-09-01
    ## 1870 1996    10 18.921 1996-10-01
    ## 1871 1996    11 12.339 1996-11-01
    ## 1872 1996    12  6.650 1996-12-01
    ## 1873 1997     1  3.398 1997-01-01
    ## 1874 1997     2  5.508 1997-02-01
    ## 1875 1997     3 11.101 1997-03-01
    ## 1876 1997     4 15.544 1997-04-01
    ## 1877 1997     5 22.762 1997-05-01
    ## 1878 1997     6 25.393 1997-06-01
    ## 1879 1997     7 27.874 1997-07-01
    ## 1880 1997     8 27.992 1997-08-01
    ## 1881 1997     9 22.727 1997-09-01
    ## 1882 1997    10 18.852 1997-10-01
    ## 1883 1997    11 12.938 1997-11-01
    ## 1884 1997    12  7.022 1997-12-01
    ## 1885 1998     1  3.785 1998-01-01
    ## 1886 1998     2  7.363 1998-02-01
    ## 1887 1998     3  9.995 1998-03-01
    ## 1888 1998     4 18.058 1998-04-01
    ## 1889 1998     5 20.940 1998-05-01
    ## 1890 1998     6 24.226 1998-06-01
    ## 1891 1998     7 29.720 1998-07-01
    ## 1892 1998     8 29.431 1998-08-01
    ## 1893 1998     9 24.095 1998-09-01
    ## 1894 1998    10 19.789 1998-10-01
    ## 1895 1998    11 15.169 1998-11-01
    ## 1896 1998    12  8.116 1998-12-01
    ## 1897 1999     1  5.750 1999-01-01
    ## 1898 1999     2  7.289 1999-02-01
    ## 1899 1999     3 10.030 1999-03-01
    ## 1900 1999     4 15.617 1999-04-01
    ## 1901 1999     5 21.235 1999-05-01
    ## 1902 1999     6 23.361 1999-06-01
    ## 1903 1999     7 26.326 1999-07-01
    ## 1904 1999     8 27.061 1999-08-01
    ## 1905 1999     9 25.625 1999-09-01
    ## 1906 1999    10 18.929 1999-10-01
    ## 1907 1999    11 12.296 1999-11-01
    ## 1908 1999    12  6.589 1999-12-01
    ## 1909 2000     1  3.938 2000-01-01
    ## 1910 2000     2  4.323 2000-02-01
    ## 1911 2000     3 11.015 2000-03-01
    ## 1912 2000     4 16.362 2000-04-01
    ## 1913 2000     5 21.860 2000-05-01
    ## 1914 2000     6 25.084 2000-06-01
    ## 1915 2000     7 29.115 2000-07-01
    ## 1916 2000     8 28.128 2000-08-01
    ## 1917 2000     9 23.997 2000-09-01
    ## 1918 2000    10 18.918 2000-10-01
    ## 1919 2000    11 11.932 2000-11-01
    ## 1920 2000    12  8.178 2000-12-01
    ## 1921 2001     1  5.146 2001-01-01
    ## 1922 2001     2  6.512 2001-02-01
    ## 1923 2001     3 11.164 2001-03-01
    ## 1924 2001     4 15.446 2001-04-01
    ## 1925 2001     5 21.630 2001-05-01
    ## 1926 2001     6 24.559 2001-06-01
    ## 1927 2001     7 29.818 2001-07-01
    ## 1928 2001     8 26.889 2001-08-01
    ## 1929 2001     9 24.150 2001-09-01
    ## 1930 2001    10 19.221 2001-10-01
    ## 1931 2001    11 12.472 2001-11-01
    ## 1932 2001    12  6.205 2001-12-01
    ## 1933 2002     1  6.232 2002-01-01
    ## 1934 2002     2  8.121 2002-02-01
    ## 1935 2002     3 12.847 2002-03-01
    ## 1936 2002     4 16.717 2002-04-01
    ## 1937 2002     5 19.781 2002-05-01
    ## 1938 2002     6 25.862 2002-06-01
    ## 1939 2002     7 28.018 2002-07-01
    ## 1940 2002     8 27.100 2002-08-01
    ## 1941 2002     9 24.178 2002-09-01
    ## 1942 2002    10 18.942 2002-10-01
    ## 1943 2002    11 12.181 2002-11-01
    ## 1944 2002    12  6.669 2002-12-01
    ## 1945 2003     1  3.434 2003-01-01
    ## 1946 2003     2  6.623 2003-02-01
    ## 1947 2003     3  9.910 2003-03-01
    ## 1948 2003     4 15.434 2003-04-01
    ## 1949 2003     5 20.165 2003-05-01
    ## 1950 2003     6 24.736 2003-06-01
    ## 1951 2003     7 29.224 2003-07-01
    ## 1952 2003     8 28.701 2003-08-01
    ## 1953 2003     9 25.588 2003-09-01
    ## 1954 2003    10 18.123 2003-10-01
    ## 1955 2003    11 13.025 2003-11-01
    ## 1956 2003    12  5.943 2003-12-01
    ## 1957 2004     1  3.877 2004-01-01
    ## 1958 2004     2  8.355 2004-02-01
    ## 1959 2004     3 10.034 2004-03-01
    ## 1960 2004     4 16.446 2004-04-01
    ## 1961 2004     5 21.159 2004-05-01
    ## 1962 2004     6 24.734 2004-06-01
    ## 1963 2004     7 29.555 2004-07-01
    ## 1964 2004     8 28.598 2004-08-01
    ## 1965 2004     9 23.751 2004-09-01
    ## 1966 2004    10 18.300 2004-10-01
    ## 1967 2004    11 14.041 2004-11-01
    ## 1968 2004    12  8.042 2004-12-01
    ## 1969 2005     1  2.628 2005-01-01
    ## 1970 2005     2  3.901 2005-02-01
    ## 1971 2005     3  8.927 2005-03-01
    ## 1972 2005     4 17.724 2005-04-01
    ## 1973 2005     5 20.796 2005-05-01
    ## 1974 2005     6 26.724 2005-06-01
    ## 1975 2005     7 29.129 2005-07-01
    ## 1976 2005     8 27.733 2005-08-01
    ## 1977 2005     9 25.819 2005-09-01
    ## 1978 2005    10 18.390 2005-10-01
    ## 1979 2005    11 14.726 2005-11-01
    ## 1980 2005    12  4.472 2005-12-01
    ## 1981 2006     1  5.020 2006-01-01
    ## 1982 2006     2  5.117 2006-02-01
    ## 1983 2006     3 11.187 2006-03-01
    ## 1984 2006     4 16.594 2006-04-01
    ## 1985 2006     5 20.777 2006-05-01
    ## 1986 2006     6 25.741 2006-06-01
    ## 1987 2006     7 29.084 2006-07-01
    ## 1988 2006     8 29.697 2006-08-01
    ## 1989 2006     9 23.070 2006-09-01
    ## 1990 2006    10 21.104 2006-10-01
    ## 1991 2006    11 14.637 2006-11-01
    ## 1992 2006    12  6.945 2006-12-01
    ## 1993 2007     1  4.532 2007-01-01
    ## 1994 2007     2  9.156 2007-02-01
    ## 1995 2007     3 11.667 2007-03-01
    ## 1996 2007     4 15.528 2007-04-01
    ## 1997 2007     5 22.832 2007-05-01
    ## 1998 2007     6 25.001 2007-06-01
    ## 1999 2007     7 29.232 2007-07-01
    ## 2000 2007     8 29.119 2007-08-01
    ## 2001 2007     9 24.415 2007-09-01
    ## 2002 2007    10 19.266 2007-10-01
    ## 2003 2007    11 12.744 2007-11-01
    ## 2004 2007    12  8.017 2007-12-01
    ## 2005 2008     1  3.059 2008-01-01
    ## 2006 2008     2  3.393 2008-02-01
    ## 2007 2008     3 11.279 2008-03-01
    ## 2008 2008     4 15.754 2008-04-01
    ## 2009 2008     5 21.625 2008-05-01
    ## 2010 2008     6 24.040 2008-06-01
    ## 2011 2008     7 29.603 2008-07-01
    ## 2012 2008     8 27.765 2008-08-01
    ## 2013 2008     9 25.097 2008-09-01
    ## 2014 2008    10 19.866 2008-10-01
    ## 2015 2008    11 12.506 2008-11-01
    ## 2016 2008    12  6.705 2008-12-01
    ## 2017 2009     1  3.250 2009-01-01
    ## 2018 2009     2  8.329 2009-02-01
    ## 2019 2009     3 10.310 2009-03-01
    ## 2020 2009     4 16.271 2009-04-01
    ## 2021 2009     5 21.779 2009-05-01
    ## 2022 2009     6 26.214 2009-06-01
    ## 2023 2009     7 28.560 2009-07-01
    ## 2024 2009     8 27.799 2009-08-01
    ## 2025 2009     9 24.671 2009-09-01
    ## 2026 2009    10 20.325 2009-10-01
    ## 2027 2009    11 10.885 2009-11-01
    ## 2028 2009    12  5.751 2009-12-01
    ## 2029 2010     1  4.635 2010-01-01
    ## 2030 2010     2  6.886 2010-02-01
    ## 2031 2010     3  9.237 2010-03-01
    ## 2032 2010     4 12.860 2010-04-01
    ## 2033 2010     5 20.827 2010-05-01
    ## 2034 2010     6 24.184 2010-06-01
    ## 2035 2010     7 28.615 2010-07-01
    ## 2036 2010     8 30.128 2010-08-01
    ## 2037 2010     9 25.361 2010-09-01
    ## 2038 2010    10 18.032 2010-10-01
    ## 2039 2010    11 13.149 2010-11-01
    ## 2040 2010    12  7.067 2010-12-01
    ## 2041 2011     1  0.700 2011-01-01
    ## 2042 2011     2  5.583 2011-02-01
    ## 2043 2011     3  9.176 2011-03-01
    ## 2044 2011     4 16.022 2011-04-01
    ## 2045 2011     5 21.481 2011-05-01
    ## 2046 2011     6 24.638 2011-06-01
    ## 2047 2011     7 29.253 2011-07-01
    ## 2048 2011     8 27.806 2011-08-01
    ## 2049 2011     9 23.782 2011-09-01
    ## 2050 2011    10 18.371 2011-10-01
    ## 2051 2011    11 15.798 2011-11-01
    ## 2052 2011    12  5.606 2011-12-01
    ## 2053 2012     1  3.875 2012-01-01
    ## 2054 2012     2  3.928 2012-02-01
    ## 2055 2012     3  9.445 2012-03-01
    ## 2056 2012     4 17.525 2012-04-01
    ## 2057 2012     5 21.454 2012-05-01
    ## 2058 2012     6 24.565 2012-06-01
    ## 2059 2012     7 29.441 2012-07-01
    ## 2060 2012     8 28.567 2012-08-01
    ## 2061 2012     9 23.217 2012-09-01
    ## 2062 2012    10 18.979 2012-10-01
    ## 2063 2012    11 11.530 2012-11-01
    ## 2064 2012    12  5.168 2012-12-01

``` r
shanghai_2012 = shanghai[shanghai$Year == 2012,]

plot(shanghai_2012$Month, shanghai_2012$Temp, xlab = "Month", 
     ylab = "Temperature", type = "b", cex = 01, pch = 19,
     main = "Monthly temperature in Shanghai 2012") 
```

![](EDA_files/figure-gfm/unnamed-chunk-33-1.png)<!-- --> FUNCTIONS

``` r
# Q1.1 pass_check
pass_check = function(grade) {
  
  # Check if the input is numeric
  # If not, stop the current execution and print an error
  if(!is.numeric(grade)) {
    stop("The input is not numeric.")
  } 
  
  if (grade >= 5){
    print("pass")
  } else {
    print("fail")
  }
  
}

factor_convert = function(df) {
  
  for(i in 1:ncol(df)) {
    
    var = df[, i]
    
    if(is.character(var)) {
      df[, i] = as.factor(var)
    }
  }
  
  return(df)
  
}

select(df, where(is.factor)) 
```

    ##         Month Season
    ## 1         May Summer
    ## 2         May Summer
    ## 3         May Summer
    ## 4         May Summer
    ## 7         May Summer
    ## 8         May Summer
    ## 9         May Summer
    ## 12        May Summer
    ## 13        May Summer
    ## 14        May Summer
    ## 15        May Summer
    ## 16        May Summer
    ## 17        May Summer
    ## 18        May Summer
    ## 19        May Summer
    ## 20        May Summer
    ## 21        May Summer
    ## 22        May Summer
    ## 23        May Summer
    ## 24        May Summer
    ## 28        May Summer
    ## 29        May Summer
    ## 30        May Summer
    ## 31        May Summer
    ## 38       June Summer
    ## 40       June Summer
    ## 41       June Summer
    ## 44       June Summer
    ## 47       June Summer
    ## 48       June Summer
    ## 49       June Summer
    ## 50       June Summer
    ## 51       June Summer
    ## 62       July   Fall
    ## 63       July   Fall
    ## 64       July   Fall
    ## 66       July   Fall
    ## 67       July   Fall
    ## 68       July   Fall
    ## 69       July   Fall
    ## 70       July   Fall
    ## 71       July   Fall
    ## 73       July   Fall
    ## 74       July   Fall
    ## 76       July   Fall
    ## 77       July   Fall
    ## 78       July   Fall
    ## 79       July   Fall
    ## 80       July   Fall
    ## 81       July   Fall
    ## 82       July   Fall
    ## 85       July   Fall
    ## 86       July   Fall
    ## 87       July   Fall
    ## 88       July   Fall
    ## 89       July   Fall
    ## 90       July   Fall
    ## 91       July   Fall
    ## 92       July   Fall
    ## 93     August   Fall
    ## 94     August   Fall
    ## 95     August   Fall
    ## 99     August   Fall
    ## 100    August   Fall
    ## 101    August   Fall
    ## 104    August   Fall
    ## 105    August   Fall
    ## 106    August   Fall
    ## 108    August   Fall
    ## 109    August   Fall
    ## 110    August   Fall
    ## 111    August   Fall
    ## 112    August   Fall
    ## 113    August   Fall
    ## 114    August   Fall
    ## 116    August   Fall
    ## 117    August   Fall
    ## 118    August   Fall
    ## 120    August   Fall
    ## 121    August   Fall
    ## 122    August   Fall
    ## 123    August   Fall
    ## 124 September   Fall
    ## 125 September   Fall
    ## 126 September   Fall
    ## 127 September   Fall
    ## 128 September   Fall
    ## 129 September   Fall
    ## 130 September   Fall
    ## 131 September   Fall
    ## 132 September   Fall
    ## 133 September   Fall
    ## 134 September   Fall
    ## 135 September   Fall
    ## 136 September   Fall
    ## 137 September   Fall
    ## 138 September   Fall
    ## 139 September   Fall
    ## 140 September   Fall
    ## 141 September   Fall
    ## 142 September   Fall
    ## 143 September   Fall
    ## 144 September   Fall
    ## 145 September   Fall
    ## 146 September   Fall
    ## 147 September   Fall
    ## 148 September   Fall
    ## 149 September   Fall
    ## 151 September   Fall
    ## 152 September   Fall
    ## 153 September   Fall

``` r
df[,sapply(df, is.factor)]
```

    ##         Month Season
    ## 1         May Summer
    ## 2         May Summer
    ## 3         May Summer
    ## 4         May Summer
    ## 7         May Summer
    ## 8         May Summer
    ## 9         May Summer
    ## 12        May Summer
    ## 13        May Summer
    ## 14        May Summer
    ## 15        May Summer
    ## 16        May Summer
    ## 17        May Summer
    ## 18        May Summer
    ## 19        May Summer
    ## 20        May Summer
    ## 21        May Summer
    ## 22        May Summer
    ## 23        May Summer
    ## 24        May Summer
    ## 28        May Summer
    ## 29        May Summer
    ## 30        May Summer
    ## 31        May Summer
    ## 38       June Summer
    ## 40       June Summer
    ## 41       June Summer
    ## 44       June Summer
    ## 47       June Summer
    ## 48       June Summer
    ## 49       June Summer
    ## 50       June Summer
    ## 51       June Summer
    ## 62       July   Fall
    ## 63       July   Fall
    ## 64       July   Fall
    ## 66       July   Fall
    ## 67       July   Fall
    ## 68       July   Fall
    ## 69       July   Fall
    ## 70       July   Fall
    ## 71       July   Fall
    ## 73       July   Fall
    ## 74       July   Fall
    ## 76       July   Fall
    ## 77       July   Fall
    ## 78       July   Fall
    ## 79       July   Fall
    ## 80       July   Fall
    ## 81       July   Fall
    ## 82       July   Fall
    ## 85       July   Fall
    ## 86       July   Fall
    ## 87       July   Fall
    ## 88       July   Fall
    ## 89       July   Fall
    ## 90       July   Fall
    ## 91       July   Fall
    ## 92       July   Fall
    ## 93     August   Fall
    ## 94     August   Fall
    ## 95     August   Fall
    ## 99     August   Fall
    ## 100    August   Fall
    ## 101    August   Fall
    ## 104    August   Fall
    ## 105    August   Fall
    ## 106    August   Fall
    ## 108    August   Fall
    ## 109    August   Fall
    ## 110    August   Fall
    ## 111    August   Fall
    ## 112    August   Fall
    ## 113    August   Fall
    ## 114    August   Fall
    ## 116    August   Fall
    ## 117    August   Fall
    ## 118    August   Fall
    ## 120    August   Fall
    ## 121    August   Fall
    ## 122    August   Fall
    ## 123    August   Fall
    ## 124 September   Fall
    ## 125 September   Fall
    ## 126 September   Fall
    ## 127 September   Fall
    ## 128 September   Fall
    ## 129 September   Fall
    ## 130 September   Fall
    ## 131 September   Fall
    ## 132 September   Fall
    ## 133 September   Fall
    ## 134 September   Fall
    ## 135 September   Fall
    ## 136 September   Fall
    ## 137 September   Fall
    ## 138 September   Fall
    ## 139 September   Fall
    ## 140 September   Fall
    ## 141 September   Fall
    ## 142 September   Fall
    ## 143 September   Fall
    ## 144 September   Fall
    ## 145 September   Fall
    ## 146 September   Fall
    ## 147 September   Fall
    ## 148 September   Fall
    ## 149 September   Fall
    ## 151 September   Fall
    ## 152 September   Fall
    ## 153 September   Fall

RDS

``` r
hawk = readRDS("../data/hawker_ctr_raw.rds")
hawk = hawk[[1]][-1]

all_names = sapply(hawk, function(x) x$NAME)

compute_dist = function(name1, name2) {
  
  id_1 = which(all_names == name1)
  id_2 = which(all_names == name2)
  
  coord_1 = as.numeric(str_split(hawk[[id_1]]$XY, ",")[[1]])
  coord_2 = as.numeric(str_split(hawk[[id_2]]$XY, ",")[[1]])
  
  dist = sqrt(sum((coord_1 - coord_2)^2))
  dist
}

dist_example = compute_dist("Tanglin Halt Market", "Taman Jurong Market & Food Centre")

hawk_combn = combn(all_names, 2)
compute_dist_v = Vectorize(compute_dist)
all_dist = compute_dist_v(hawk_combn[1, ], hawk_combn[2, ])
dist_df = data.frame(hawker1 = hawk_combn[1, ],
                     hawker2 = hawk_combn[2, ],
                     dist = all_dist, stringsAsFactors = FALSE)

dist_df
```

    ##                                     hawker1
    ## 1        Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 2        Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 3        Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 4        Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 5        Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 6        Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 7        Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 8        Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 9        Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 10       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 11       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 12       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 13       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 14       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 15       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 16       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 17       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 18       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 19       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 20       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 21       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 22       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 23       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 24       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 25       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 26       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 27       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 28       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 29       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 30       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 31       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 32       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 33       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 34       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 35       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 36       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 37       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 38       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 39       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 40       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 41       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 42       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 43       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 44       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 45       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 46       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 47       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 48       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 49       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 50       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 51       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 52       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 53       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 54       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 55       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 56       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 57       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 58       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 59       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 60       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 61       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 62       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 63       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 64       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 65       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 66       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 67       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 68       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 69       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 70       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 71       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 72       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 73       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 74       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 75       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 76       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 77       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 78       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 79       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 80       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 81       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 82       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 83       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 84       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 85       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 86       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 87       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 88       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 89       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 90       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 91       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 92       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 93       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 94       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 95       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 96       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 97       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 98       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 99       Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 100      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 101      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 102      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 103      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 104      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 105      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 106      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 107      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 108      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 109      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 110      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 111      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 112      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 113      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 114      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 115      Blks 1A/ 2A/ 3A Commonwealth Drive
    ## 116               Blks 20/21 Marsiling Lane
    ## 117               Blks 20/21 Marsiling Lane
    ## 118               Blks 20/21 Marsiling Lane
    ## 119               Blks 20/21 Marsiling Lane
    ## 120               Blks 20/21 Marsiling Lane
    ## 121               Blks 20/21 Marsiling Lane
    ## 122               Blks 20/21 Marsiling Lane
    ## 123               Blks 20/21 Marsiling Lane
    ## 124               Blks 20/21 Marsiling Lane
    ## 125               Blks 20/21 Marsiling Lane
    ## 126               Blks 20/21 Marsiling Lane
    ## 127               Blks 20/21 Marsiling Lane
    ## 128               Blks 20/21 Marsiling Lane
    ## 129               Blks 20/21 Marsiling Lane
    ## 130               Blks 20/21 Marsiling Lane
    ## 131               Blks 20/21 Marsiling Lane
    ## 132               Blks 20/21 Marsiling Lane
    ## 133               Blks 20/21 Marsiling Lane
    ## 134               Blks 20/21 Marsiling Lane
    ## 135               Blks 20/21 Marsiling Lane
    ## 136               Blks 20/21 Marsiling Lane
    ## 137               Blks 20/21 Marsiling Lane
    ## 138               Blks 20/21 Marsiling Lane
    ## 139               Blks 20/21 Marsiling Lane
    ## 140               Blks 20/21 Marsiling Lane
    ## 141               Blks 20/21 Marsiling Lane
    ## 142               Blks 20/21 Marsiling Lane
    ## 143               Blks 20/21 Marsiling Lane
    ## 144               Blks 20/21 Marsiling Lane
    ## 145               Blks 20/21 Marsiling Lane
    ## 146               Blks 20/21 Marsiling Lane
    ## 147               Blks 20/21 Marsiling Lane
    ## 148               Blks 20/21 Marsiling Lane
    ## 149               Blks 20/21 Marsiling Lane
    ## 150               Blks 20/21 Marsiling Lane
    ## 151               Blks 20/21 Marsiling Lane
    ## 152               Blks 20/21 Marsiling Lane
    ## 153               Blks 20/21 Marsiling Lane
    ## 154               Blks 20/21 Marsiling Lane
    ## 155               Blks 20/21 Marsiling Lane
    ## 156               Blks 20/21 Marsiling Lane
    ## 157               Blks 20/21 Marsiling Lane
    ## 158               Blks 20/21 Marsiling Lane
    ## 159               Blks 20/21 Marsiling Lane
    ## 160               Blks 20/21 Marsiling Lane
    ## 161               Blks 20/21 Marsiling Lane
    ## 162               Blks 20/21 Marsiling Lane
    ## 163               Blks 20/21 Marsiling Lane
    ## 164               Blks 20/21 Marsiling Lane
    ## 165               Blks 20/21 Marsiling Lane
    ## 166               Blks 20/21 Marsiling Lane
    ## 167               Blks 20/21 Marsiling Lane
    ## 168               Blks 20/21 Marsiling Lane
    ## 169               Blks 20/21 Marsiling Lane
    ## 170               Blks 20/21 Marsiling Lane
    ## 171               Blks 20/21 Marsiling Lane
    ## 172               Blks 20/21 Marsiling Lane
    ## 173               Blks 20/21 Marsiling Lane
    ## 174               Blks 20/21 Marsiling Lane
    ## 175               Blks 20/21 Marsiling Lane
    ## 176               Blks 20/21 Marsiling Lane
    ## 177               Blks 20/21 Marsiling Lane
    ## 178               Blks 20/21 Marsiling Lane
    ## 179               Blks 20/21 Marsiling Lane
    ## 180               Blks 20/21 Marsiling Lane
    ## 181               Blks 20/21 Marsiling Lane
    ## 182               Blks 20/21 Marsiling Lane
    ## 183               Blks 20/21 Marsiling Lane
    ## 184               Blks 20/21 Marsiling Lane
    ## 185               Blks 20/21 Marsiling Lane
    ## 186               Blks 20/21 Marsiling Lane
    ## 187               Blks 20/21 Marsiling Lane
    ## 188               Blks 20/21 Marsiling Lane
    ## 189               Blks 20/21 Marsiling Lane
    ## 190               Blks 20/21 Marsiling Lane
    ## 191               Blks 20/21 Marsiling Lane
    ## 192               Blks 20/21 Marsiling Lane
    ## 193               Blks 20/21 Marsiling Lane
    ## 194               Blks 20/21 Marsiling Lane
    ## 195               Blks 20/21 Marsiling Lane
    ## 196               Blks 20/21 Marsiling Lane
    ## 197               Blks 20/21 Marsiling Lane
    ## 198               Blks 20/21 Marsiling Lane
    ## 199               Blks 20/21 Marsiling Lane
    ## 200               Blks 20/21 Marsiling Lane
    ## 201               Blks 20/21 Marsiling Lane
    ## 202               Blks 20/21 Marsiling Lane
    ## 203               Blks 20/21 Marsiling Lane
    ## 204               Blks 20/21 Marsiling Lane
    ## 205               Blks 20/21 Marsiling Lane
    ## 206               Blks 20/21 Marsiling Lane
    ## 207               Blks 20/21 Marsiling Lane
    ## 208               Blks 20/21 Marsiling Lane
    ## 209               Blks 20/21 Marsiling Lane
    ## 210               Blks 20/21 Marsiling Lane
    ## 211               Blks 20/21 Marsiling Lane
    ## 212               Blks 20/21 Marsiling Lane
    ## 213               Blks 20/21 Marsiling Lane
    ## 214               Blks 20/21 Marsiling Lane
    ## 215               Blks 20/21 Marsiling Lane
    ## 216               Blks 20/21 Marsiling Lane
    ## 217               Blks 20/21 Marsiling Lane
    ## 218               Blks 20/21 Marsiling Lane
    ## 219               Blks 20/21 Marsiling Lane
    ## 220               Blks 20/21 Marsiling Lane
    ## 221               Blks 20/21 Marsiling Lane
    ## 222               Blks 20/21 Marsiling Lane
    ## 223               Blks 20/21 Marsiling Lane
    ## 224               Blks 20/21 Marsiling Lane
    ## 225               Blks 20/21 Marsiling Lane
    ## 226               Blks 20/21 Marsiling Lane
    ## 227               Blks 20/21 Marsiling Lane
    ## 228               Blks 20/21 Marsiling Lane
    ## 229               Blks 20/21 Marsiling Lane
    ## 230              Blks 221A/B Boon Lay Place
    ## 231              Blks 221A/B Boon Lay Place
    ## 232              Blks 221A/B Boon Lay Place
    ## 233              Blks 221A/B Boon Lay Place
    ## 234              Blks 221A/B Boon Lay Place
    ## 235              Blks 221A/B Boon Lay Place
    ## 236              Blks 221A/B Boon Lay Place
    ## 237              Blks 221A/B Boon Lay Place
    ## 238              Blks 221A/B Boon Lay Place
    ## 239              Blks 221A/B Boon Lay Place
    ## 240              Blks 221A/B Boon Lay Place
    ## 241              Blks 221A/B Boon Lay Place
    ## 242              Blks 221A/B Boon Lay Place
    ## 243              Blks 221A/B Boon Lay Place
    ## 244              Blks 221A/B Boon Lay Place
    ## 245              Blks 221A/B Boon Lay Place
    ## 246              Blks 221A/B Boon Lay Place
    ## 247              Blks 221A/B Boon Lay Place
    ## 248              Blks 221A/B Boon Lay Place
    ## 249              Blks 221A/B Boon Lay Place
    ## 250              Blks 221A/B Boon Lay Place
    ## 251              Blks 221A/B Boon Lay Place
    ## 252              Blks 221A/B Boon Lay Place
    ## 253              Blks 221A/B Boon Lay Place
    ## 254              Blks 221A/B Boon Lay Place
    ## 255              Blks 221A/B Boon Lay Place
    ## 256              Blks 221A/B Boon Lay Place
    ## 257              Blks 221A/B Boon Lay Place
    ## 258              Blks 221A/B Boon Lay Place
    ## 259              Blks 221A/B Boon Lay Place
    ## 260              Blks 221A/B Boon Lay Place
    ## 261              Blks 221A/B Boon Lay Place
    ## 262              Blks 221A/B Boon Lay Place
    ## 263              Blks 221A/B Boon Lay Place
    ## 264              Blks 221A/B Boon Lay Place
    ## 265              Blks 221A/B Boon Lay Place
    ## 266              Blks 221A/B Boon Lay Place
    ## 267              Blks 221A/B Boon Lay Place
    ## 268              Blks 221A/B Boon Lay Place
    ## 269              Blks 221A/B Boon Lay Place
    ## 270              Blks 221A/B Boon Lay Place
    ## 271              Blks 221A/B Boon Lay Place
    ## 272              Blks 221A/B Boon Lay Place
    ## 273              Blks 221A/B Boon Lay Place
    ## 274              Blks 221A/B Boon Lay Place
    ## 275              Blks 221A/B Boon Lay Place
    ## 276              Blks 221A/B Boon Lay Place
    ## 277              Blks 221A/B Boon Lay Place
    ## 278              Blks 221A/B Boon Lay Place
    ## 279              Blks 221A/B Boon Lay Place
    ## 280              Blks 221A/B Boon Lay Place
    ## 281              Blks 221A/B Boon Lay Place
    ## 282              Blks 221A/B Boon Lay Place
    ## 283              Blks 221A/B Boon Lay Place
    ## 284              Blks 221A/B Boon Lay Place
    ## 285              Blks 221A/B Boon Lay Place
    ## 286              Blks 221A/B Boon Lay Place
    ## 287              Blks 221A/B Boon Lay Place
    ## 288              Blks 221A/B Boon Lay Place
    ## 289              Blks 221A/B Boon Lay Place
    ## 290              Blks 221A/B Boon Lay Place
    ## 291              Blks 221A/B Boon Lay Place
    ## 292              Blks 221A/B Boon Lay Place
    ## 293              Blks 221A/B Boon Lay Place
    ## 294              Blks 221A/B Boon Lay Place
    ## 295              Blks 221A/B Boon Lay Place
    ## 296              Blks 221A/B Boon Lay Place
    ## 297              Blks 221A/B Boon Lay Place
    ## 298              Blks 221A/B Boon Lay Place
    ## 299              Blks 221A/B Boon Lay Place
    ## 300              Blks 221A/B Boon Lay Place
    ## 301              Blks 221A/B Boon Lay Place
    ## 302              Blks 221A/B Boon Lay Place
    ## 303              Blks 221A/B Boon Lay Place
    ## 304              Blks 221A/B Boon Lay Place
    ## 305              Blks 221A/B Boon Lay Place
    ## 306              Blks 221A/B Boon Lay Place
    ## 307              Blks 221A/B Boon Lay Place
    ## 308              Blks 221A/B Boon Lay Place
    ## 309              Blks 221A/B Boon Lay Place
    ## 310              Blks 221A/B Boon Lay Place
    ## 311              Blks 221A/B Boon Lay Place
    ## 312              Blks 221A/B Boon Lay Place
    ## 313              Blks 221A/B Boon Lay Place
    ## 314              Blks 221A/B Boon Lay Place
    ## 315              Blks 221A/B Boon Lay Place
    ## 316              Blks 221A/B Boon Lay Place
    ## 317              Blks 221A/B Boon Lay Place
    ## 318              Blks 221A/B Boon Lay Place
    ## 319              Blks 221A/B Boon Lay Place
    ## 320              Blks 221A/B Boon Lay Place
    ## 321              Blks 221A/B Boon Lay Place
    ## 322              Blks 221A/B Boon Lay Place
    ## 323              Blks 221A/B Boon Lay Place
    ## 324              Blks 221A/B Boon Lay Place
    ## 325              Blks 221A/B Boon Lay Place
    ## 326              Blks 221A/B Boon Lay Place
    ## 327              Blks 221A/B Boon Lay Place
    ## 328              Blks 221A/B Boon Lay Place
    ## 329              Blks 221A/B Boon Lay Place
    ## 330              Blks 221A/B Boon Lay Place
    ## 331              Blks 221A/B Boon Lay Place
    ## 332              Blks 221A/B Boon Lay Place
    ## 333              Blks 221A/B Boon Lay Place
    ## 334              Blks 221A/B Boon Lay Place
    ## 335              Blks 221A/B Boon Lay Place
    ## 336              Blks 221A/B Boon Lay Place
    ## 337              Blks 221A/B Boon Lay Place
    ## 338              Blks 221A/B Boon Lay Place
    ## 339              Blks 221A/B Boon Lay Place
    ## 340              Blks 221A/B Boon Lay Place
    ## 341              Blks 221A/B Boon Lay Place
    ## 342              Blks 221A/B Boon Lay Place
    ## 343                Blks 22A/B Havelock Road
    ## 344                Blks 22A/B Havelock Road
    ## 345                Blks 22A/B Havelock Road
    ## 346                Blks 22A/B Havelock Road
    ## 347                Blks 22A/B Havelock Road
    ## 348                Blks 22A/B Havelock Road
    ## 349                Blks 22A/B Havelock Road
    ## 350                Blks 22A/B Havelock Road
    ## 351                Blks 22A/B Havelock Road
    ## 352                Blks 22A/B Havelock Road
    ## 353                Blks 22A/B Havelock Road
    ## 354                Blks 22A/B Havelock Road
    ## 355                Blks 22A/B Havelock Road
    ## 356                Blks 22A/B Havelock Road
    ## 357                Blks 22A/B Havelock Road
    ## 358                Blks 22A/B Havelock Road
    ## 359                Blks 22A/B Havelock Road
    ## 360                Blks 22A/B Havelock Road
    ## 361                Blks 22A/B Havelock Road
    ## 362                Blks 22A/B Havelock Road
    ## 363                Blks 22A/B Havelock Road
    ## 364                Blks 22A/B Havelock Road
    ## 365                Blks 22A/B Havelock Road
    ## 366                Blks 22A/B Havelock Road
    ## 367                Blks 22A/B Havelock Road
    ## 368                Blks 22A/B Havelock Road
    ## 369                Blks 22A/B Havelock Road
    ## 370                Blks 22A/B Havelock Road
    ## 371                Blks 22A/B Havelock Road
    ## 372                Blks 22A/B Havelock Road
    ## 373                Blks 22A/B Havelock Road
    ## 374                Blks 22A/B Havelock Road
    ## 375                Blks 22A/B Havelock Road
    ## 376                Blks 22A/B Havelock Road
    ## 377                Blks 22A/B Havelock Road
    ## 378                Blks 22A/B Havelock Road
    ## 379                Blks 22A/B Havelock Road
    ## 380                Blks 22A/B Havelock Road
    ## 381                Blks 22A/B Havelock Road
    ## 382                Blks 22A/B Havelock Road
    ## 383                Blks 22A/B Havelock Road
    ## 384                Blks 22A/B Havelock Road
    ## 385                Blks 22A/B Havelock Road
    ## 386                Blks 22A/B Havelock Road
    ## 387                Blks 22A/B Havelock Road
    ## 388                Blks 22A/B Havelock Road
    ## 389                Blks 22A/B Havelock Road
    ## 390                Blks 22A/B Havelock Road
    ## 391                Blks 22A/B Havelock Road
    ## 392                Blks 22A/B Havelock Road
    ## 393                Blks 22A/B Havelock Road
    ## 394                Blks 22A/B Havelock Road
    ## 395                Blks 22A/B Havelock Road
    ## 396                Blks 22A/B Havelock Road
    ## 397                Blks 22A/B Havelock Road
    ## 398                Blks 22A/B Havelock Road
    ## 399                Blks 22A/B Havelock Road
    ## 400                Blks 22A/B Havelock Road
    ## 401                Blks 22A/B Havelock Road
    ## 402                Blks 22A/B Havelock Road
    ## 403                Blks 22A/B Havelock Road
    ## 404                Blks 22A/B Havelock Road
    ## 405                Blks 22A/B Havelock Road
    ## 406                Blks 22A/B Havelock Road
    ## 407                Blks 22A/B Havelock Road
    ## 408                Blks 22A/B Havelock Road
    ## 409                Blks 22A/B Havelock Road
    ## 410                Blks 22A/B Havelock Road
    ## 411                Blks 22A/B Havelock Road
    ## 412                Blks 22A/B Havelock Road
    ## 413                Blks 22A/B Havelock Road
    ## 414                Blks 22A/B Havelock Road
    ## 415                Blks 22A/B Havelock Road
    ## 416                Blks 22A/B Havelock Road
    ## 417                Blks 22A/B Havelock Road
    ## 418                Blks 22A/B Havelock Road
    ## 419                Blks 22A/B Havelock Road
    ## 420                Blks 22A/B Havelock Road
    ## 421                Blks 22A/B Havelock Road
    ## 422                Blks 22A/B Havelock Road
    ## 423                Blks 22A/B Havelock Road
    ## 424                Blks 22A/B Havelock Road
    ## 425                Blks 22A/B Havelock Road
    ## 426                Blks 22A/B Havelock Road
    ## 427                Blks 22A/B Havelock Road
    ## 428                Blks 22A/B Havelock Road
    ## 429                Blks 22A/B Havelock Road
    ## 430                Blks 22A/B Havelock Road
    ## 431                Blks 22A/B Havelock Road
    ## 432                Blks 22A/B Havelock Road
    ## 433                Blks 22A/B Havelock Road
    ## 434                Blks 22A/B Havelock Road
    ## 435                Blks 22A/B Havelock Road
    ## 436                Blks 22A/B Havelock Road
    ## 437                Blks 22A/B Havelock Road
    ## 438                Blks 22A/B Havelock Road
    ## 439                Blks 22A/B Havelock Road
    ## 440                Blks 22A/B Havelock Road
    ## 441                Blks 22A/B Havelock Road
    ## 442                Blks 22A/B Havelock Road
    ## 443                Blks 22A/B Havelock Road
    ## 444                Blks 22A/B Havelock Road
    ## 445                Blks 22A/B Havelock Road
    ## 446                Blks 22A/B Havelock Road
    ## 447                Blks 22A/B Havelock Road
    ## 448                Blks 22A/B Havelock Road
    ## 449                Blks 22A/B Havelock Road
    ## 450                Blks 22A/B Havelock Road
    ## 451                Blks 22A/B Havelock Road
    ## 452                Blks 22A/B Havelock Road
    ## 453                Blks 22A/B Havelock Road
    ## 454                Blks 22A/B Havelock Road
    ## 455                Blks 79/79A Circuit Road
    ## 456                Blks 79/79A Circuit Road
    ## 457                Blks 79/79A Circuit Road
    ## 458                Blks 79/79A Circuit Road
    ## 459                Blks 79/79A Circuit Road
    ## 460                Blks 79/79A Circuit Road
    ## 461                Blks 79/79A Circuit Road
    ## 462                Blks 79/79A Circuit Road
    ## 463                Blks 79/79A Circuit Road
    ## 464                Blks 79/79A Circuit Road
    ## 465                Blks 79/79A Circuit Road
    ## 466                Blks 79/79A Circuit Road
    ## 467                Blks 79/79A Circuit Road
    ## 468                Blks 79/79A Circuit Road
    ## 469                Blks 79/79A Circuit Road
    ## 470                Blks 79/79A Circuit Road
    ## 471                Blks 79/79A Circuit Road
    ## 472                Blks 79/79A Circuit Road
    ## 473                Blks 79/79A Circuit Road
    ## 474                Blks 79/79A Circuit Road
    ## 475                Blks 79/79A Circuit Road
    ## 476                Blks 79/79A Circuit Road
    ## 477                Blks 79/79A Circuit Road
    ## 478                Blks 79/79A Circuit Road
    ## 479                Blks 79/79A Circuit Road
    ## 480                Blks 79/79A Circuit Road
    ## 481                Blks 79/79A Circuit Road
    ## 482                Blks 79/79A Circuit Road
    ## 483                Blks 79/79A Circuit Road
    ## 484                Blks 79/79A Circuit Road
    ## 485                Blks 79/79A Circuit Road
    ## 486                Blks 79/79A Circuit Road
    ## 487                Blks 79/79A Circuit Road
    ## 488                Blks 79/79A Circuit Road
    ## 489                Blks 79/79A Circuit Road
    ## 490                Blks 79/79A Circuit Road
    ## 491                Blks 79/79A Circuit Road
    ## 492                Blks 79/79A Circuit Road
    ## 493                Blks 79/79A Circuit Road
    ## 494                Blks 79/79A Circuit Road
    ## 495                Blks 79/79A Circuit Road
    ## 496                Blks 79/79A Circuit Road
    ## 497                Blks 79/79A Circuit Road
    ## 498                Blks 79/79A Circuit Road
    ## 499                Blks 79/79A Circuit Road
    ## 500                Blks 79/79A Circuit Road
    ## 501                Blks 79/79A Circuit Road
    ## 502                Blks 79/79A Circuit Road
    ## 503                Blks 79/79A Circuit Road
    ## 504                Blks 79/79A Circuit Road
    ## 505                Blks 79/79A Circuit Road
    ## 506                Blks 79/79A Circuit Road
    ## 507                Blks 79/79A Circuit Road
    ## 508                Blks 79/79A Circuit Road
    ## 509                Blks 79/79A Circuit Road
    ## 510                Blks 79/79A Circuit Road
    ## 511                Blks 79/79A Circuit Road
    ## 512                Blks 79/79A Circuit Road
    ## 513                Blks 79/79A Circuit Road
    ## 514                Blks 79/79A Circuit Road
    ## 515                Blks 79/79A Circuit Road
    ## 516                Blks 79/79A Circuit Road
    ## 517                Blks 79/79A Circuit Road
    ## 518                Blks 79/79A Circuit Road
    ## 519                Blks 79/79A Circuit Road
    ## 520                Blks 79/79A Circuit Road
    ## 521                Blks 79/79A Circuit Road
    ## 522                Blks 79/79A Circuit Road
    ## 523                Blks 79/79A Circuit Road
    ## 524                Blks 79/79A Circuit Road
    ## 525                Blks 79/79A Circuit Road
    ## 526                Blks 79/79A Circuit Road
    ## 527                Blks 79/79A Circuit Road
    ## 528                Blks 79/79A Circuit Road
    ## 529                Blks 79/79A Circuit Road
    ## 530                Blks 79/79A Circuit Road
    ## 531                Blks 79/79A Circuit Road
    ## 532                Blks 79/79A Circuit Road
    ## 533                Blks 79/79A Circuit Road
    ## 534                Blks 79/79A Circuit Road
    ## 535                Blks 79/79A Circuit Road
    ## 536                Blks 79/79A Circuit Road
    ## 537                Blks 79/79A Circuit Road
    ## 538                Blks 79/79A Circuit Road
    ## 539                Blks 79/79A Circuit Road
    ## 540                Blks 79/79A Circuit Road
    ## 541                Blks 79/79A Circuit Road
    ## 542                Blks 79/79A Circuit Road
    ## 543                Blks 79/79A Circuit Road
    ## 544                Blks 79/79A Circuit Road
    ## 545                Blks 79/79A Circuit Road
    ## 546                Blks 79/79A Circuit Road
    ## 547                Blks 79/79A Circuit Road
    ## 548                Blks 79/79A Circuit Road
    ## 549                Blks 79/79A Circuit Road
    ## 550                Blks 79/79A Circuit Road
    ## 551                Blks 79/79A Circuit Road
    ## 552                Blks 79/79A Circuit Road
    ## 553                Blks 79/79A Circuit Road
    ## 554                Blks 79/79A Circuit Road
    ## 555                Blks 79/79A Circuit Road
    ## 556                Blks 79/79A Circuit Road
    ## 557                Blks 79/79A Circuit Road
    ## 558                Blks 79/79A Circuit Road
    ## 559                Blks 79/79A Circuit Road
    ## 560                Blks 79/79A Circuit Road
    ## 561                Blks 79/79A Circuit Road
    ## 562                Blks 79/79A Circuit Road
    ## 563                Blks 79/79A Circuit Road
    ## 564                Blks 79/79A Circuit Road
    ## 565                Blks 79/79A Circuit Road
    ## 566                Blks 91/92 Whampoa Drive
    ## 567                Blks 91/92 Whampoa Drive
    ## 568                Blks 91/92 Whampoa Drive
    ## 569                Blks 91/92 Whampoa Drive
    ## 570                Blks 91/92 Whampoa Drive
    ## 571                Blks 91/92 Whampoa Drive
    ## 572                Blks 91/92 Whampoa Drive
    ## 573                Blks 91/92 Whampoa Drive
    ## 574                Blks 91/92 Whampoa Drive
    ## 575                Blks 91/92 Whampoa Drive
    ## 576                Blks 91/92 Whampoa Drive
    ## 577                Blks 91/92 Whampoa Drive
    ## 578                Blks 91/92 Whampoa Drive
    ## 579                Blks 91/92 Whampoa Drive
    ## 580                Blks 91/92 Whampoa Drive
    ## 581                Blks 91/92 Whampoa Drive
    ## 582                Blks 91/92 Whampoa Drive
    ## 583                Blks 91/92 Whampoa Drive
    ## 584                Blks 91/92 Whampoa Drive
    ## 585                Blks 91/92 Whampoa Drive
    ## 586                Blks 91/92 Whampoa Drive
    ## 587                Blks 91/92 Whampoa Drive
    ## 588                Blks 91/92 Whampoa Drive
    ## 589                Blks 91/92 Whampoa Drive
    ## 590                Blks 91/92 Whampoa Drive
    ## 591                Blks 91/92 Whampoa Drive
    ## 592                Blks 91/92 Whampoa Drive
    ## 593                Blks 91/92 Whampoa Drive
    ## 594                Blks 91/92 Whampoa Drive
    ## 595                Blks 91/92 Whampoa Drive
    ## 596                Blks 91/92 Whampoa Drive
    ## 597                Blks 91/92 Whampoa Drive
    ## 598                Blks 91/92 Whampoa Drive
    ## 599                Blks 91/92 Whampoa Drive
    ## 600                Blks 91/92 Whampoa Drive
    ## 601                Blks 91/92 Whampoa Drive
    ## 602                Blks 91/92 Whampoa Drive
    ## 603                Blks 91/92 Whampoa Drive
    ## 604                Blks 91/92 Whampoa Drive
    ## 605                Blks 91/92 Whampoa Drive
    ## 606                Blks 91/92 Whampoa Drive
    ## 607                Blks 91/92 Whampoa Drive
    ## 608                Blks 91/92 Whampoa Drive
    ## 609                Blks 91/92 Whampoa Drive
    ## 610                Blks 91/92 Whampoa Drive
    ## 611                Blks 91/92 Whampoa Drive
    ## 612                Blks 91/92 Whampoa Drive
    ## 613                Blks 91/92 Whampoa Drive
    ## 614                Blks 91/92 Whampoa Drive
    ## 615                Blks 91/92 Whampoa Drive
    ## 616                Blks 91/92 Whampoa Drive
    ## 617                Blks 91/92 Whampoa Drive
    ## 618                Blks 91/92 Whampoa Drive
    ## 619                Blks 91/92 Whampoa Drive
    ## 620                Blks 91/92 Whampoa Drive
    ## 621                Blks 91/92 Whampoa Drive
    ## 622                Blks 91/92 Whampoa Drive
    ## 623                Blks 91/92 Whampoa Drive
    ## 624                Blks 91/92 Whampoa Drive
    ## 625                Blks 91/92 Whampoa Drive
    ## 626                Blks 91/92 Whampoa Drive
    ## 627                Blks 91/92 Whampoa Drive
    ## 628                Blks 91/92 Whampoa Drive
    ## 629                Blks 91/92 Whampoa Drive
    ## 630                Blks 91/92 Whampoa Drive
    ## 631                Blks 91/92 Whampoa Drive
    ## 632                Blks 91/92 Whampoa Drive
    ## 633                Blks 91/92 Whampoa Drive
    ## 634                Blks 91/92 Whampoa Drive
    ## 635                Blks 91/92 Whampoa Drive
    ## 636                Blks 91/92 Whampoa Drive
    ## 637                Blks 91/92 Whampoa Drive
    ## 638                Blks 91/92 Whampoa Drive
    ## 639                Blks 91/92 Whampoa Drive
    ## 640                Blks 91/92 Whampoa Drive
    ## 641                Blks 91/92 Whampoa Drive
    ## 642                Blks 91/92 Whampoa Drive
    ## 643                Blks 91/92 Whampoa Drive
    ## 644                Blks 91/92 Whampoa Drive
    ## 645                Blks 91/92 Whampoa Drive
    ## 646                Blks 91/92 Whampoa Drive
    ## 647                Blks 91/92 Whampoa Drive
    ## 648                Blks 91/92 Whampoa Drive
    ## 649                Blks 91/92 Whampoa Drive
    ## 650                Blks 91/92 Whampoa Drive
    ## 651                Blks 91/92 Whampoa Drive
    ## 652                Blks 91/92 Whampoa Drive
    ## 653                Blks 91/92 Whampoa Drive
    ## 654                Blks 91/92 Whampoa Drive
    ## 655                Blks 91/92 Whampoa Drive
    ## 656                Blks 91/92 Whampoa Drive
    ## 657                Blks 91/92 Whampoa Drive
    ## 658                Blks 91/92 Whampoa Drive
    ## 659                Blks 91/92 Whampoa Drive
    ## 660                Blks 91/92 Whampoa Drive
    ## 661                Blks 91/92 Whampoa Drive
    ## 662                Blks 91/92 Whampoa Drive
    ## 663                Blks 91/92 Whampoa Drive
    ## 664                Blks 91/92 Whampoa Drive
    ## 665                Blks 91/92 Whampoa Drive
    ## 666                Blks 91/92 Whampoa Drive
    ## 667                Blks 91/92 Whampoa Drive
    ## 668                Blks 91/92 Whampoa Drive
    ## 669                Blks 91/92 Whampoa Drive
    ## 670                Blks 91/92 Whampoa Drive
    ## 671                Blks 91/92 Whampoa Drive
    ## 672                Blks 91/92 Whampoa Drive
    ## 673                Blks 91/92 Whampoa Drive
    ## 674                Blks 91/92 Whampoa Drive
    ## 675                Blks 91/92 Whampoa Drive
    ## 676                      Bukit Timah Market
    ## 677                      Bukit Timah Market
    ## 678                      Bukit Timah Market
    ## 679                      Bukit Timah Market
    ## 680                      Bukit Timah Market
    ## 681                      Bukit Timah Market
    ## 682                      Bukit Timah Market
    ## 683                      Bukit Timah Market
    ## 684                      Bukit Timah Market
    ## 685                      Bukit Timah Market
    ## 686                      Bukit Timah Market
    ## 687                      Bukit Timah Market
    ## 688                      Bukit Timah Market
    ## 689                      Bukit Timah Market
    ## 690                      Bukit Timah Market
    ## 691                      Bukit Timah Market
    ## 692                      Bukit Timah Market
    ## 693                      Bukit Timah Market
    ## 694                      Bukit Timah Market
    ## 695                      Bukit Timah Market
    ## 696                      Bukit Timah Market
    ## 697                      Bukit Timah Market
    ## 698                      Bukit Timah Market
    ## 699                      Bukit Timah Market
    ## 700                      Bukit Timah Market
    ## 701                      Bukit Timah Market
    ## 702                      Bukit Timah Market
    ## 703                      Bukit Timah Market
    ## 704                      Bukit Timah Market
    ## 705                      Bukit Timah Market
    ## 706                      Bukit Timah Market
    ## 707                      Bukit Timah Market
    ## 708                      Bukit Timah Market
    ## 709                      Bukit Timah Market
    ## 710                      Bukit Timah Market
    ## 711                      Bukit Timah Market
    ## 712                      Bukit Timah Market
    ## 713                      Bukit Timah Market
    ## 714                      Bukit Timah Market
    ## 715                      Bukit Timah Market
    ## 716                      Bukit Timah Market
    ## 717                      Bukit Timah Market
    ## 718                      Bukit Timah Market
    ## 719                      Bukit Timah Market
    ## 720                      Bukit Timah Market
    ## 721                      Bukit Timah Market
    ## 722                      Bukit Timah Market
    ## 723                      Bukit Timah Market
    ## 724                      Bukit Timah Market
    ## 725                      Bukit Timah Market
    ## 726                      Bukit Timah Market
    ## 727                      Bukit Timah Market
    ## 728                      Bukit Timah Market
    ## 729                      Bukit Timah Market
    ## 730                      Bukit Timah Market
    ## 731                      Bukit Timah Market
    ## 732                      Bukit Timah Market
    ## 733                      Bukit Timah Market
    ## 734                      Bukit Timah Market
    ## 735                      Bukit Timah Market
    ## 736                      Bukit Timah Market
    ## 737                      Bukit Timah Market
    ## 738                      Bukit Timah Market
    ## 739                      Bukit Timah Market
    ## 740                      Bukit Timah Market
    ## 741                      Bukit Timah Market
    ## 742                      Bukit Timah Market
    ## 743                      Bukit Timah Market
    ## 744                      Bukit Timah Market
    ## 745                      Bukit Timah Market
    ## 746                      Bukit Timah Market
    ## 747                      Bukit Timah Market
    ## 748                      Bukit Timah Market
    ## 749                      Bukit Timah Market
    ## 750                      Bukit Timah Market
    ## 751                      Bukit Timah Market
    ## 752                      Bukit Timah Market
    ## 753                      Bukit Timah Market
    ## 754                      Bukit Timah Market
    ## 755                      Bukit Timah Market
    ## 756                      Bukit Timah Market
    ## 757                      Bukit Timah Market
    ## 758                      Bukit Timah Market
    ## 759                      Bukit Timah Market
    ## 760                      Bukit Timah Market
    ## 761                      Bukit Timah Market
    ## 762                      Bukit Timah Market
    ## 763                      Bukit Timah Market
    ## 764                      Bukit Timah Market
    ## 765                      Bukit Timah Market
    ## 766                      Bukit Timah Market
    ## 767                      Bukit Timah Market
    ## 768                      Bukit Timah Market
    ## 769                      Bukit Timah Market
    ## 770                      Bukit Timah Market
    ## 771                      Bukit Timah Market
    ## 772                      Bukit Timah Market
    ## 773                      Bukit Timah Market
    ## 774                      Bukit Timah Market
    ## 775                      Bukit Timah Market
    ## 776                      Bukit Timah Market
    ## 777                      Bukit Timah Market
    ## 778                      Bukit Timah Market
    ## 779                      Bukit Timah Market
    ## 780                      Bukit Timah Market
    ## 781                      Bukit Timah Market
    ## 782                      Bukit Timah Market
    ## 783                      Bukit Timah Market
    ## 784                      Bukit Timah Market
    ## 785                        Chinatown Market
    ## 786                        Chinatown Market
    ## 787                        Chinatown Market
    ## 788                        Chinatown Market
    ## 789                        Chinatown Market
    ## 790                        Chinatown Market
    ## 791                        Chinatown Market
    ## 792                        Chinatown Market
    ## 793                        Chinatown Market
    ## 794                        Chinatown Market
    ## 795                        Chinatown Market
    ## 796                        Chinatown Market
    ## 797                        Chinatown Market
    ## 798                        Chinatown Market
    ## 799                        Chinatown Market
    ## 800                        Chinatown Market
    ## 801                        Chinatown Market
    ## 802                        Chinatown Market
    ## 803                        Chinatown Market
    ## 804                        Chinatown Market
    ## 805                        Chinatown Market
    ## 806                        Chinatown Market
    ## 807                        Chinatown Market
    ## 808                        Chinatown Market
    ## 809                        Chinatown Market
    ## 810                        Chinatown Market
    ## 811                        Chinatown Market
    ## 812                        Chinatown Market
    ## 813                        Chinatown Market
    ## 814                        Chinatown Market
    ## 815                        Chinatown Market
    ## 816                        Chinatown Market
    ## 817                        Chinatown Market
    ## 818                        Chinatown Market
    ## 819                        Chinatown Market
    ## 820                        Chinatown Market
    ## 821                        Chinatown Market
    ## 822                        Chinatown Market
    ## 823                        Chinatown Market
    ## 824                        Chinatown Market
    ## 825                        Chinatown Market
    ## 826                        Chinatown Market
    ## 827                        Chinatown Market
    ## 828                        Chinatown Market
    ## 829                        Chinatown Market
    ## 830                        Chinatown Market
    ## 831                        Chinatown Market
    ## 832                        Chinatown Market
    ## 833                        Chinatown Market
    ## 834                        Chinatown Market
    ## 835                        Chinatown Market
    ## 836                        Chinatown Market
    ## 837                        Chinatown Market
    ## 838                        Chinatown Market
    ## 839                        Chinatown Market
    ## 840                        Chinatown Market
    ## 841                        Chinatown Market
    ## 842                        Chinatown Market
    ## 843                        Chinatown Market
    ## 844                        Chinatown Market
    ## 845                        Chinatown Market
    ## 846                        Chinatown Market
    ## 847                        Chinatown Market
    ## 848                        Chinatown Market
    ## 849                        Chinatown Market
    ## 850                        Chinatown Market
    ## 851                        Chinatown Market
    ## 852                        Chinatown Market
    ## 853                        Chinatown Market
    ## 854                        Chinatown Market
    ## 855                        Chinatown Market
    ## 856                        Chinatown Market
    ## 857                        Chinatown Market
    ## 858                        Chinatown Market
    ## 859                        Chinatown Market
    ## 860                        Chinatown Market
    ## 861                        Chinatown Market
    ## 862                        Chinatown Market
    ## 863                        Chinatown Market
    ## 864                        Chinatown Market
    ## 865                        Chinatown Market
    ## 866                        Chinatown Market
    ## 867                        Chinatown Market
    ## 868                        Chinatown Market
    ## 869                        Chinatown Market
    ## 870                        Chinatown Market
    ## 871                        Chinatown Market
    ## 872                        Chinatown Market
    ## 873                        Chinatown Market
    ## 874                        Chinatown Market
    ## 875                        Chinatown Market
    ## 876                        Chinatown Market
    ## 877                        Chinatown Market
    ## 878                        Chinatown Market
    ## 879                        Chinatown Market
    ## 880                        Chinatown Market
    ## 881                        Chinatown Market
    ## 882                        Chinatown Market
    ## 883                        Chinatown Market
    ## 884                        Chinatown Market
    ## 885                        Chinatown Market
    ## 886                        Chinatown Market
    ## 887                        Chinatown Market
    ## 888                        Chinatown Market
    ## 889                        Chinatown Market
    ## 890                        Chinatown Market
    ## 891                        Chinatown Market
    ## 892                        Chinatown Market
    ## 893                 Chomp Chomp Food Centre
    ## 894                 Chomp Chomp Food Centre
    ## 895                 Chomp Chomp Food Centre
    ## 896                 Chomp Chomp Food Centre
    ## 897                 Chomp Chomp Food Centre
    ## 898                 Chomp Chomp Food Centre
    ## 899                 Chomp Chomp Food Centre
    ## 900                 Chomp Chomp Food Centre
    ## 901                 Chomp Chomp Food Centre
    ## 902                 Chomp Chomp Food Centre
    ## 903                 Chomp Chomp Food Centre
    ## 904                 Chomp Chomp Food Centre
    ## 905                 Chomp Chomp Food Centre
    ## 906                 Chomp Chomp Food Centre
    ## 907                 Chomp Chomp Food Centre
    ## 908                 Chomp Chomp Food Centre
    ## 909                 Chomp Chomp Food Centre
    ## 910                 Chomp Chomp Food Centre
    ## 911                 Chomp Chomp Food Centre
    ## 912                 Chomp Chomp Food Centre
    ## 913                 Chomp Chomp Food Centre
    ## 914                 Chomp Chomp Food Centre
    ## 915                 Chomp Chomp Food Centre
    ## 916                 Chomp Chomp Food Centre
    ## 917                 Chomp Chomp Food Centre
    ## 918                 Chomp Chomp Food Centre
    ## 919                 Chomp Chomp Food Centre
    ## 920                 Chomp Chomp Food Centre
    ## 921                 Chomp Chomp Food Centre
    ## 922                 Chomp Chomp Food Centre
    ## 923                 Chomp Chomp Food Centre
    ## 924                 Chomp Chomp Food Centre
    ## 925                 Chomp Chomp Food Centre
    ## 926                 Chomp Chomp Food Centre
    ## 927                 Chomp Chomp Food Centre
    ## 928                 Chomp Chomp Food Centre
    ## 929                 Chomp Chomp Food Centre
    ## 930                 Chomp Chomp Food Centre
    ## 931                 Chomp Chomp Food Centre
    ## 932                 Chomp Chomp Food Centre
    ## 933                 Chomp Chomp Food Centre
    ## 934                 Chomp Chomp Food Centre
    ## 935                 Chomp Chomp Food Centre
    ## 936                 Chomp Chomp Food Centre
    ## 937                 Chomp Chomp Food Centre
    ## 938                 Chomp Chomp Food Centre
    ## 939                 Chomp Chomp Food Centre
    ## 940                 Chomp Chomp Food Centre
    ## 941                 Chomp Chomp Food Centre
    ## 942                 Chomp Chomp Food Centre
    ## 943                 Chomp Chomp Food Centre
    ## 944                 Chomp Chomp Food Centre
    ## 945                 Chomp Chomp Food Centre
    ## 946                 Chomp Chomp Food Centre
    ## 947                 Chomp Chomp Food Centre
    ## 948                 Chomp Chomp Food Centre
    ## 949                 Chomp Chomp Food Centre
    ## 950                 Chomp Chomp Food Centre
    ## 951                 Chomp Chomp Food Centre
    ## 952                 Chomp Chomp Food Centre
    ## 953                 Chomp Chomp Food Centre
    ## 954                 Chomp Chomp Food Centre
    ## 955                 Chomp Chomp Food Centre
    ## 956                 Chomp Chomp Food Centre
    ## 957                 Chomp Chomp Food Centre
    ## 958                 Chomp Chomp Food Centre
    ## 959                 Chomp Chomp Food Centre
    ## 960                 Chomp Chomp Food Centre
    ## 961                 Chomp Chomp Food Centre
    ## 962                 Chomp Chomp Food Centre
    ## 963                 Chomp Chomp Food Centre
    ## 964                 Chomp Chomp Food Centre
    ## 965                 Chomp Chomp Food Centre
    ## 966                 Chomp Chomp Food Centre
    ## 967                 Chomp Chomp Food Centre
    ## 968                 Chomp Chomp Food Centre
    ## 969                 Chomp Chomp Food Centre
    ## 970                 Chomp Chomp Food Centre
    ## 971                 Chomp Chomp Food Centre
    ## 972                 Chomp Chomp Food Centre
    ## 973                 Chomp Chomp Food Centre
    ## 974                 Chomp Chomp Food Centre
    ## 975                 Chomp Chomp Food Centre
    ## 976                 Chomp Chomp Food Centre
    ## 977                 Chomp Chomp Food Centre
    ## 978                 Chomp Chomp Food Centre
    ## 979                 Chomp Chomp Food Centre
    ## 980                 Chomp Chomp Food Centre
    ## 981                 Chomp Chomp Food Centre
    ## 982                 Chomp Chomp Food Centre
    ## 983                 Chomp Chomp Food Centre
    ## 984                 Chomp Chomp Food Centre
    ## 985                 Chomp Chomp Food Centre
    ## 986                 Chomp Chomp Food Centre
    ## 987                 Chomp Chomp Food Centre
    ## 988                 Chomp Chomp Food Centre
    ## 989                 Chomp Chomp Food Centre
    ## 990                 Chomp Chomp Food Centre
    ## 991                 Chomp Chomp Food Centre
    ## 992                 Chomp Chomp Food Centre
    ## 993                 Chomp Chomp Food Centre
    ## 994                 Chomp Chomp Food Centre
    ## 995                 Chomp Chomp Food Centre
    ## 996                 Chomp Chomp Food Centre
    ## 997                 Chomp Chomp Food Centre
    ## 998                 Chomp Chomp Food Centre
    ## 999                 Chomp Chomp Food Centre
    ## 1000        Chong Pang Market & Food Centre
    ## 1001        Chong Pang Market & Food Centre
    ## 1002        Chong Pang Market & Food Centre
    ## 1003        Chong Pang Market & Food Centre
    ## 1004        Chong Pang Market & Food Centre
    ## 1005        Chong Pang Market & Food Centre
    ## 1006        Chong Pang Market & Food Centre
    ## 1007        Chong Pang Market & Food Centre
    ## 1008        Chong Pang Market & Food Centre
    ## 1009        Chong Pang Market & Food Centre
    ## 1010        Chong Pang Market & Food Centre
    ## 1011        Chong Pang Market & Food Centre
    ## 1012        Chong Pang Market & Food Centre
    ## 1013        Chong Pang Market & Food Centre
    ## 1014        Chong Pang Market & Food Centre
    ## 1015        Chong Pang Market & Food Centre
    ## 1016        Chong Pang Market & Food Centre
    ## 1017        Chong Pang Market & Food Centre
    ## 1018        Chong Pang Market & Food Centre
    ## 1019        Chong Pang Market & Food Centre
    ## 1020        Chong Pang Market & Food Centre
    ## 1021        Chong Pang Market & Food Centre
    ## 1022        Chong Pang Market & Food Centre
    ## 1023        Chong Pang Market & Food Centre
    ## 1024        Chong Pang Market & Food Centre
    ## 1025        Chong Pang Market & Food Centre
    ## 1026        Chong Pang Market & Food Centre
    ## 1027        Chong Pang Market & Food Centre
    ## 1028        Chong Pang Market & Food Centre
    ## 1029        Chong Pang Market & Food Centre
    ## 1030        Chong Pang Market & Food Centre
    ## 1031        Chong Pang Market & Food Centre
    ## 1032        Chong Pang Market & Food Centre
    ## 1033        Chong Pang Market & Food Centre
    ## 1034        Chong Pang Market & Food Centre
    ## 1035        Chong Pang Market & Food Centre
    ## 1036        Chong Pang Market & Food Centre
    ## 1037        Chong Pang Market & Food Centre
    ## 1038        Chong Pang Market & Food Centre
    ## 1039        Chong Pang Market & Food Centre
    ## 1040        Chong Pang Market & Food Centre
    ## 1041        Chong Pang Market & Food Centre
    ## 1042        Chong Pang Market & Food Centre
    ## 1043        Chong Pang Market & Food Centre
    ## 1044        Chong Pang Market & Food Centre
    ## 1045        Chong Pang Market & Food Centre
    ## 1046        Chong Pang Market & Food Centre
    ## 1047        Chong Pang Market & Food Centre
    ## 1048        Chong Pang Market & Food Centre
    ## 1049        Chong Pang Market & Food Centre
    ## 1050        Chong Pang Market & Food Centre
    ## 1051        Chong Pang Market & Food Centre
    ## 1052        Chong Pang Market & Food Centre
    ## 1053        Chong Pang Market & Food Centre
    ## 1054        Chong Pang Market & Food Centre
    ## 1055        Chong Pang Market & Food Centre
    ## 1056        Chong Pang Market & Food Centre
    ## 1057        Chong Pang Market & Food Centre
    ## 1058        Chong Pang Market & Food Centre
    ## 1059        Chong Pang Market & Food Centre
    ## 1060        Chong Pang Market & Food Centre
    ## 1061        Chong Pang Market & Food Centre
    ## 1062        Chong Pang Market & Food Centre
    ## 1063        Chong Pang Market & Food Centre
    ## 1064        Chong Pang Market & Food Centre
    ## 1065        Chong Pang Market & Food Centre
    ## 1066        Chong Pang Market & Food Centre
    ## 1067        Chong Pang Market & Food Centre
    ## 1068        Chong Pang Market & Food Centre
    ## 1069        Chong Pang Market & Food Centre
    ## 1070        Chong Pang Market & Food Centre
    ## 1071        Chong Pang Market & Food Centre
    ## 1072        Chong Pang Market & Food Centre
    ## 1073        Chong Pang Market & Food Centre
    ## 1074        Chong Pang Market & Food Centre
    ## 1075        Chong Pang Market & Food Centre
    ## 1076        Chong Pang Market & Food Centre
    ## 1077        Chong Pang Market & Food Centre
    ## 1078        Chong Pang Market & Food Centre
    ## 1079        Chong Pang Market & Food Centre
    ## 1080        Chong Pang Market & Food Centre
    ## 1081        Chong Pang Market & Food Centre
    ## 1082        Chong Pang Market & Food Centre
    ## 1083        Chong Pang Market & Food Centre
    ## 1084        Chong Pang Market & Food Centre
    ## 1085        Chong Pang Market & Food Centre
    ## 1086        Chong Pang Market & Food Centre
    ## 1087        Chong Pang Market & Food Centre
    ## 1088        Chong Pang Market & Food Centre
    ## 1089        Chong Pang Market & Food Centre
    ## 1090        Chong Pang Market & Food Centre
    ## 1091        Chong Pang Market & Food Centre
    ## 1092        Chong Pang Market & Food Centre
    ## 1093        Chong Pang Market & Food Centre
    ## 1094        Chong Pang Market & Food Centre
    ## 1095        Chong Pang Market & Food Centre
    ## 1096        Chong Pang Market & Food Centre
    ## 1097        Chong Pang Market & Food Centre
    ## 1098        Chong Pang Market & Food Centre
    ## 1099        Chong Pang Market & Food Centre
    ## 1100        Chong Pang Market & Food Centre
    ## 1101        Chong Pang Market & Food Centre
    ## 1102        Chong Pang Market & Food Centre
    ## 1103        Chong Pang Market & Food Centre
    ## 1104        Chong Pang Market & Food Centre
    ## 1105        Chong Pang Market & Food Centre
    ## 1106                     Dunman Food Centre
    ## 1107                     Dunman Food Centre
    ## 1108                     Dunman Food Centre
    ## 1109                     Dunman Food Centre
    ## 1110                     Dunman Food Centre
    ## 1111                     Dunman Food Centre
    ## 1112                     Dunman Food Centre
    ## 1113                     Dunman Food Centre
    ## 1114                     Dunman Food Centre
    ## 1115                     Dunman Food Centre
    ## 1116                     Dunman Food Centre
    ## 1117                     Dunman Food Centre
    ## 1118                     Dunman Food Centre
    ## 1119                     Dunman Food Centre
    ## 1120                     Dunman Food Centre
    ## 1121                     Dunman Food Centre
    ## 1122                     Dunman Food Centre
    ## 1123                     Dunman Food Centre
    ## 1124                     Dunman Food Centre
    ## 1125                     Dunman Food Centre
    ## 1126                     Dunman Food Centre
    ## 1127                     Dunman Food Centre
    ## 1128                     Dunman Food Centre
    ## 1129                     Dunman Food Centre
    ## 1130                     Dunman Food Centre
    ## 1131                     Dunman Food Centre
    ## 1132                     Dunman Food Centre
    ## 1133                     Dunman Food Centre
    ## 1134                     Dunman Food Centre
    ## 1135                     Dunman Food Centre
    ## 1136                     Dunman Food Centre
    ## 1137                     Dunman Food Centre
    ## 1138                     Dunman Food Centre
    ## 1139                     Dunman Food Centre
    ## 1140                     Dunman Food Centre
    ## 1141                     Dunman Food Centre
    ## 1142                     Dunman Food Centre
    ## 1143                     Dunman Food Centre
    ## 1144                     Dunman Food Centre
    ## 1145                     Dunman Food Centre
    ## 1146                     Dunman Food Centre
    ## 1147                     Dunman Food Centre
    ## 1148                     Dunman Food Centre
    ## 1149                     Dunman Food Centre
    ## 1150                     Dunman Food Centre
    ## 1151                     Dunman Food Centre
    ## 1152                     Dunman Food Centre
    ## 1153                     Dunman Food Centre
    ## 1154                     Dunman Food Centre
    ## 1155                     Dunman Food Centre
    ## 1156                     Dunman Food Centre
    ## 1157                     Dunman Food Centre
    ## 1158                     Dunman Food Centre
    ## 1159                     Dunman Food Centre
    ## 1160                     Dunman Food Centre
    ## 1161                     Dunman Food Centre
    ## 1162                     Dunman Food Centre
    ## 1163                     Dunman Food Centre
    ## 1164                     Dunman Food Centre
    ## 1165                     Dunman Food Centre
    ## 1166                     Dunman Food Centre
    ## 1167                     Dunman Food Centre
    ## 1168                     Dunman Food Centre
    ## 1169                     Dunman Food Centre
    ## 1170                     Dunman Food Centre
    ## 1171                     Dunman Food Centre
    ## 1172                     Dunman Food Centre
    ## 1173                     Dunman Food Centre
    ## 1174                     Dunman Food Centre
    ## 1175                     Dunman Food Centre
    ## 1176                     Dunman Food Centre
    ## 1177                     Dunman Food Centre
    ## 1178                     Dunman Food Centre
    ## 1179                     Dunman Food Centre
    ## 1180                     Dunman Food Centre
    ## 1181                     Dunman Food Centre
    ## 1182                     Dunman Food Centre
    ## 1183                     Dunman Food Centre
    ## 1184                     Dunman Food Centre
    ## 1185                     Dunman Food Centre
    ## 1186                     Dunman Food Centre
    ## 1187                     Dunman Food Centre
    ## 1188                     Dunman Food Centre
    ## 1189                     Dunman Food Centre
    ## 1190                     Dunman Food Centre
    ## 1191                     Dunman Food Centre
    ## 1192                     Dunman Food Centre
    ## 1193                     Dunman Food Centre
    ## 1194                     Dunman Food Centre
    ## 1195                     Dunman Food Centre
    ## 1196                     Dunman Food Centre
    ## 1197                     Dunman Food Centre
    ## 1198                     Dunman Food Centre
    ## 1199                     Dunman Food Centre
    ## 1200                     Dunman Food Centre
    ## 1201                     Dunman Food Centre
    ## 1202                     Dunman Food Centre
    ## 1203                     Dunman Food Centre
    ## 1204                     Dunman Food Centre
    ## 1205                     Dunman Food Centre
    ## 1206                     Dunman Food Centre
    ## 1207                     Dunman Food Centre
    ## 1208                     Dunman Food Centre
    ## 1209                     Dunman Food Centre
    ## 1210                     Dunman Food Centre
    ## 1211         East Coast Lagoon Food Village
    ## 1212         East Coast Lagoon Food Village
    ## 1213         East Coast Lagoon Food Village
    ## 1214         East Coast Lagoon Food Village
    ## 1215         East Coast Lagoon Food Village
    ## 1216         East Coast Lagoon Food Village
    ## 1217         East Coast Lagoon Food Village
    ## 1218         East Coast Lagoon Food Village
    ## 1219         East Coast Lagoon Food Village
    ## 1220         East Coast Lagoon Food Village
    ## 1221         East Coast Lagoon Food Village
    ## 1222         East Coast Lagoon Food Village
    ## 1223         East Coast Lagoon Food Village
    ## 1224         East Coast Lagoon Food Village
    ## 1225         East Coast Lagoon Food Village
    ## 1226         East Coast Lagoon Food Village
    ## 1227         East Coast Lagoon Food Village
    ## 1228         East Coast Lagoon Food Village
    ## 1229         East Coast Lagoon Food Village
    ## 1230         East Coast Lagoon Food Village
    ## 1231         East Coast Lagoon Food Village
    ## 1232         East Coast Lagoon Food Village
    ## 1233         East Coast Lagoon Food Village
    ## 1234         East Coast Lagoon Food Village
    ## 1235         East Coast Lagoon Food Village
    ## 1236         East Coast Lagoon Food Village
    ## 1237         East Coast Lagoon Food Village
    ## 1238         East Coast Lagoon Food Village
    ## 1239         East Coast Lagoon Food Village
    ## 1240         East Coast Lagoon Food Village
    ## 1241         East Coast Lagoon Food Village
    ## 1242         East Coast Lagoon Food Village
    ## 1243         East Coast Lagoon Food Village
    ## 1244         East Coast Lagoon Food Village
    ## 1245         East Coast Lagoon Food Village
    ## 1246         East Coast Lagoon Food Village
    ## 1247         East Coast Lagoon Food Village
    ## 1248         East Coast Lagoon Food Village
    ## 1249         East Coast Lagoon Food Village
    ## 1250         East Coast Lagoon Food Village
    ## 1251         East Coast Lagoon Food Village
    ## 1252         East Coast Lagoon Food Village
    ## 1253         East Coast Lagoon Food Village
    ## 1254         East Coast Lagoon Food Village
    ## 1255         East Coast Lagoon Food Village
    ## 1256         East Coast Lagoon Food Village
    ## 1257         East Coast Lagoon Food Village
    ## 1258         East Coast Lagoon Food Village
    ## 1259         East Coast Lagoon Food Village
    ## 1260         East Coast Lagoon Food Village
    ## 1261         East Coast Lagoon Food Village
    ## 1262         East Coast Lagoon Food Village
    ## 1263         East Coast Lagoon Food Village
    ## 1264         East Coast Lagoon Food Village
    ## 1265         East Coast Lagoon Food Village
    ## 1266         East Coast Lagoon Food Village
    ## 1267         East Coast Lagoon Food Village
    ## 1268         East Coast Lagoon Food Village
    ## 1269         East Coast Lagoon Food Village
    ## 1270         East Coast Lagoon Food Village
    ## 1271         East Coast Lagoon Food Village
    ## 1272         East Coast Lagoon Food Village
    ## 1273         East Coast Lagoon Food Village
    ## 1274         East Coast Lagoon Food Village
    ## 1275         East Coast Lagoon Food Village
    ## 1276         East Coast Lagoon Food Village
    ## 1277         East Coast Lagoon Food Village
    ## 1278         East Coast Lagoon Food Village
    ## 1279         East Coast Lagoon Food Village
    ## 1280         East Coast Lagoon Food Village
    ## 1281         East Coast Lagoon Food Village
    ## 1282         East Coast Lagoon Food Village
    ## 1283         East Coast Lagoon Food Village
    ## 1284         East Coast Lagoon Food Village
    ## 1285         East Coast Lagoon Food Village
    ## 1286         East Coast Lagoon Food Village
    ## 1287         East Coast Lagoon Food Village
    ## 1288         East Coast Lagoon Food Village
    ## 1289         East Coast Lagoon Food Village
    ## 1290         East Coast Lagoon Food Village
    ## 1291         East Coast Lagoon Food Village
    ## 1292         East Coast Lagoon Food Village
    ## 1293         East Coast Lagoon Food Village
    ## 1294         East Coast Lagoon Food Village
    ## 1295         East Coast Lagoon Food Village
    ## 1296         East Coast Lagoon Food Village
    ## 1297         East Coast Lagoon Food Village
    ## 1298         East Coast Lagoon Food Village
    ## 1299         East Coast Lagoon Food Village
    ## 1300         East Coast Lagoon Food Village
    ## 1301         East Coast Lagoon Food Village
    ## 1302         East Coast Lagoon Food Village
    ## 1303         East Coast Lagoon Food Village
    ## 1304         East Coast Lagoon Food Village
    ## 1305         East Coast Lagoon Food Village
    ## 1306         East Coast Lagoon Food Village
    ## 1307         East Coast Lagoon Food Village
    ## 1308         East Coast Lagoon Food Village
    ## 1309         East Coast Lagoon Food Village
    ## 1310         East Coast Lagoon Food Village
    ## 1311         East Coast Lagoon Food Village
    ## 1312         East Coast Lagoon Food Village
    ## 1313         East Coast Lagoon Food Village
    ## 1314         East Coast Lagoon Food Village
    ## 1315                   Geylang Serai Market
    ## 1316                   Geylang Serai Market
    ## 1317                   Geylang Serai Market
    ## 1318                   Geylang Serai Market
    ## 1319                   Geylang Serai Market
    ## 1320                   Geylang Serai Market
    ## 1321                   Geylang Serai Market
    ## 1322                   Geylang Serai Market
    ## 1323                   Geylang Serai Market
    ## 1324                   Geylang Serai Market
    ## 1325                   Geylang Serai Market
    ## 1326                   Geylang Serai Market
    ## 1327                   Geylang Serai Market
    ## 1328                   Geylang Serai Market
    ## 1329                   Geylang Serai Market
    ## 1330                   Geylang Serai Market
    ## 1331                   Geylang Serai Market
    ## 1332                   Geylang Serai Market
    ## 1333                   Geylang Serai Market
    ## 1334                   Geylang Serai Market
    ## 1335                   Geylang Serai Market
    ## 1336                   Geylang Serai Market
    ## 1337                   Geylang Serai Market
    ## 1338                   Geylang Serai Market
    ## 1339                   Geylang Serai Market
    ## 1340                   Geylang Serai Market
    ## 1341                   Geylang Serai Market
    ## 1342                   Geylang Serai Market
    ## 1343                   Geylang Serai Market
    ## 1344                   Geylang Serai Market
    ## 1345                   Geylang Serai Market
    ## 1346                   Geylang Serai Market
    ## 1347                   Geylang Serai Market
    ## 1348                   Geylang Serai Market
    ## 1349                   Geylang Serai Market
    ## 1350                   Geylang Serai Market
    ## 1351                   Geylang Serai Market
    ## 1352                   Geylang Serai Market
    ## 1353                   Geylang Serai Market
    ## 1354                   Geylang Serai Market
    ## 1355                   Geylang Serai Market
    ## 1356                   Geylang Serai Market
    ## 1357                   Geylang Serai Market
    ## 1358                   Geylang Serai Market
    ## 1359                   Geylang Serai Market
    ## 1360                   Geylang Serai Market
    ## 1361                   Geylang Serai Market
    ## 1362                   Geylang Serai Market
    ## 1363                   Geylang Serai Market
    ## 1364                   Geylang Serai Market
    ## 1365                   Geylang Serai Market
    ## 1366                   Geylang Serai Market
    ## 1367                   Geylang Serai Market
    ## 1368                   Geylang Serai Market
    ## 1369                   Geylang Serai Market
    ## 1370                   Geylang Serai Market
    ## 1371                   Geylang Serai Market
    ## 1372                   Geylang Serai Market
    ## 1373                   Geylang Serai Market
    ## 1374                   Geylang Serai Market
    ## 1375                   Geylang Serai Market
    ## 1376                   Geylang Serai Market
    ## 1377                   Geylang Serai Market
    ## 1378                   Geylang Serai Market
    ## 1379                   Geylang Serai Market
    ## 1380                   Geylang Serai Market
    ## 1381                   Geylang Serai Market
    ## 1382                   Geylang Serai Market
    ## 1383                   Geylang Serai Market
    ## 1384                   Geylang Serai Market
    ## 1385                   Geylang Serai Market
    ## 1386                   Geylang Serai Market
    ## 1387                   Geylang Serai Market
    ## 1388                   Geylang Serai Market
    ## 1389                   Geylang Serai Market
    ## 1390                   Geylang Serai Market
    ## 1391                   Geylang Serai Market
    ## 1392                   Geylang Serai Market
    ## 1393                   Geylang Serai Market
    ## 1394                   Geylang Serai Market
    ## 1395                   Geylang Serai Market
    ## 1396                   Geylang Serai Market
    ## 1397                   Geylang Serai Market
    ## 1398                   Geylang Serai Market
    ## 1399                   Geylang Serai Market
    ## 1400                   Geylang Serai Market
    ## 1401                   Geylang Serai Market
    ## 1402                   Geylang Serai Market
    ## 1403                   Geylang Serai Market
    ## 1404                   Geylang Serai Market
    ## 1405                   Geylang Serai Market
    ## 1406                   Geylang Serai Market
    ## 1407                   Geylang Serai Market
    ## 1408                   Geylang Serai Market
    ## 1409                   Geylang Serai Market
    ## 1410                   Geylang Serai Market
    ## 1411                   Geylang Serai Market
    ## 1412                   Geylang Serai Market
    ## 1413                   Geylang Serai Market
    ## 1414                   Geylang Serai Market
    ## 1415                   Geylang Serai Market
    ## 1416                   Geylang Serai Market
    ## 1417                   Geylang Serai Market
    ## 1418                Golden Mile Food Centre
    ## 1419                Golden Mile Food Centre
    ## 1420                Golden Mile Food Centre
    ## 1421                Golden Mile Food Centre
    ## 1422                Golden Mile Food Centre
    ## 1423                Golden Mile Food Centre
    ## 1424                Golden Mile Food Centre
    ## 1425                Golden Mile Food Centre
    ## 1426                Golden Mile Food Centre
    ## 1427                Golden Mile Food Centre
    ## 1428                Golden Mile Food Centre
    ## 1429                Golden Mile Food Centre
    ## 1430                Golden Mile Food Centre
    ## 1431                Golden Mile Food Centre
    ## 1432                Golden Mile Food Centre
    ## 1433                Golden Mile Food Centre
    ## 1434                Golden Mile Food Centre
    ## 1435                Golden Mile Food Centre
    ## 1436                Golden Mile Food Centre
    ## 1437                Golden Mile Food Centre
    ## 1438                Golden Mile Food Centre
    ## 1439                Golden Mile Food Centre
    ## 1440                Golden Mile Food Centre
    ## 1441                Golden Mile Food Centre
    ## 1442                Golden Mile Food Centre
    ## 1443                Golden Mile Food Centre
    ## 1444                Golden Mile Food Centre
    ## 1445                Golden Mile Food Centre
    ## 1446                Golden Mile Food Centre
    ## 1447                Golden Mile Food Centre
    ## 1448                Golden Mile Food Centre
    ## 1449                Golden Mile Food Centre
    ## 1450                Golden Mile Food Centre
    ## 1451                Golden Mile Food Centre
    ## 1452                Golden Mile Food Centre
    ## 1453                Golden Mile Food Centre
    ## 1454                Golden Mile Food Centre
    ## 1455                Golden Mile Food Centre
    ## 1456                Golden Mile Food Centre
    ## 1457                Golden Mile Food Centre
    ## 1458                Golden Mile Food Centre
    ## 1459                Golden Mile Food Centre
    ## 1460                Golden Mile Food Centre
    ## 1461                Golden Mile Food Centre
    ## 1462                Golden Mile Food Centre
    ## 1463                Golden Mile Food Centre
    ## 1464                Golden Mile Food Centre
    ## 1465                Golden Mile Food Centre
    ## 1466                Golden Mile Food Centre
    ## 1467                Golden Mile Food Centre
    ## 1468                Golden Mile Food Centre
    ## 1469                Golden Mile Food Centre
    ## 1470                Golden Mile Food Centre
    ## 1471                Golden Mile Food Centre
    ## 1472                Golden Mile Food Centre
    ## 1473                Golden Mile Food Centre
    ## 1474                Golden Mile Food Centre
    ## 1475                Golden Mile Food Centre
    ## 1476                Golden Mile Food Centre
    ## 1477                Golden Mile Food Centre
    ## 1478                Golden Mile Food Centre
    ## 1479                Golden Mile Food Centre
    ## 1480                Golden Mile Food Centre
    ## 1481                Golden Mile Food Centre
    ## 1482                Golden Mile Food Centre
    ## 1483                Golden Mile Food Centre
    ## 1484                Golden Mile Food Centre
    ## 1485                Golden Mile Food Centre
    ## 1486                Golden Mile Food Centre
    ## 1487                Golden Mile Food Centre
    ## 1488                Golden Mile Food Centre
    ## 1489                Golden Mile Food Centre
    ## 1490                Golden Mile Food Centre
    ## 1491                Golden Mile Food Centre
    ## 1492                Golden Mile Food Centre
    ## 1493                Golden Mile Food Centre
    ## 1494                Golden Mile Food Centre
    ## 1495                Golden Mile Food Centre
    ## 1496                Golden Mile Food Centre
    ## 1497                Golden Mile Food Centre
    ## 1498                Golden Mile Food Centre
    ## 1499                Golden Mile Food Centre
    ## 1500                Golden Mile Food Centre
    ## 1501                Golden Mile Food Centre
    ## 1502                Golden Mile Food Centre
    ## 1503                Golden Mile Food Centre
    ## 1504                Golden Mile Food Centre
    ## 1505                Golden Mile Food Centre
    ## 1506                Golden Mile Food Centre
    ## 1507                Golden Mile Food Centre
    ## 1508                Golden Mile Food Centre
    ## 1509                Golden Mile Food Centre
    ## 1510                Golden Mile Food Centre
    ## 1511                Golden Mile Food Centre
    ## 1512                Golden Mile Food Centre
    ## 1513                Golden Mile Food Centre
    ## 1514                Golden Mile Food Centre
    ## 1515                Golden Mile Food Centre
    ## 1516                Golden Mile Food Centre
    ## 1517                Golden Mile Food Centre
    ## 1518                Golden Mile Food Centre
    ## 1519                Golden Mile Food Centre
    ## 1520   Holland Village Market & Food Centre
    ## 1521   Holland Village Market & Food Centre
    ## 1522   Holland Village Market & Food Centre
    ## 1523   Holland Village Market & Food Centre
    ## 1524   Holland Village Market & Food Centre
    ## 1525   Holland Village Market & Food Centre
    ## 1526   Holland Village Market & Food Centre
    ## 1527   Holland Village Market & Food Centre
    ## 1528   Holland Village Market & Food Centre
    ## 1529   Holland Village Market & Food Centre
    ## 1530   Holland Village Market & Food Centre
    ## 1531   Holland Village Market & Food Centre
    ## 1532   Holland Village Market & Food Centre
    ## 1533   Holland Village Market & Food Centre
    ## 1534   Holland Village Market & Food Centre
    ## 1535   Holland Village Market & Food Centre
    ## 1536   Holland Village Market & Food Centre
    ## 1537   Holland Village Market & Food Centre
    ## 1538   Holland Village Market & Food Centre
    ## 1539   Holland Village Market & Food Centre
    ## 1540   Holland Village Market & Food Centre
    ## 1541   Holland Village Market & Food Centre
    ## 1542   Holland Village Market & Food Centre
    ## 1543   Holland Village Market & Food Centre
    ## 1544   Holland Village Market & Food Centre
    ## 1545   Holland Village Market & Food Centre
    ## 1546   Holland Village Market & Food Centre
    ## 1547   Holland Village Market & Food Centre
    ## 1548   Holland Village Market & Food Centre
    ## 1549   Holland Village Market & Food Centre
    ## 1550   Holland Village Market & Food Centre
    ## 1551   Holland Village Market & Food Centre
    ## 1552   Holland Village Market & Food Centre
    ## 1553   Holland Village Market & Food Centre
    ## 1554   Holland Village Market & Food Centre
    ## 1555   Holland Village Market & Food Centre
    ## 1556   Holland Village Market & Food Centre
    ## 1557   Holland Village Market & Food Centre
    ## 1558   Holland Village Market & Food Centre
    ## 1559   Holland Village Market & Food Centre
    ## 1560   Holland Village Market & Food Centre
    ## 1561   Holland Village Market & Food Centre
    ## 1562   Holland Village Market & Food Centre
    ## 1563   Holland Village Market & Food Centre
    ## 1564   Holland Village Market & Food Centre
    ## 1565   Holland Village Market & Food Centre
    ## 1566   Holland Village Market & Food Centre
    ## 1567   Holland Village Market & Food Centre
    ## 1568   Holland Village Market & Food Centre
    ## 1569   Holland Village Market & Food Centre
    ## 1570   Holland Village Market & Food Centre
    ## 1571   Holland Village Market & Food Centre
    ## 1572   Holland Village Market & Food Centre
    ## 1573   Holland Village Market & Food Centre
    ## 1574   Holland Village Market & Food Centre
    ## 1575   Holland Village Market & Food Centre
    ## 1576   Holland Village Market & Food Centre
    ## 1577   Holland Village Market & Food Centre
    ## 1578   Holland Village Market & Food Centre
    ## 1579   Holland Village Market & Food Centre
    ## 1580   Holland Village Market & Food Centre
    ## 1581   Holland Village Market & Food Centre
    ## 1582   Holland Village Market & Food Centre
    ## 1583   Holland Village Market & Food Centre
    ## 1584   Holland Village Market & Food Centre
    ## 1585   Holland Village Market & Food Centre
    ## 1586   Holland Village Market & Food Centre
    ## 1587   Holland Village Market & Food Centre
    ## 1588   Holland Village Market & Food Centre
    ## 1589   Holland Village Market & Food Centre
    ## 1590   Holland Village Market & Food Centre
    ## 1591   Holland Village Market & Food Centre
    ## 1592   Holland Village Market & Food Centre
    ## 1593   Holland Village Market & Food Centre
    ## 1594   Holland Village Market & Food Centre
    ## 1595   Holland Village Market & Food Centre
    ## 1596   Holland Village Market & Food Centre
    ## 1597   Holland Village Market & Food Centre
    ## 1598   Holland Village Market & Food Centre
    ## 1599   Holland Village Market & Food Centre
    ## 1600   Holland Village Market & Food Centre
    ## 1601   Holland Village Market & Food Centre
    ## 1602   Holland Village Market & Food Centre
    ## 1603   Holland Village Market & Food Centre
    ## 1604   Holland Village Market & Food Centre
    ## 1605   Holland Village Market & Food Centre
    ## 1606   Holland Village Market & Food Centre
    ## 1607   Holland Village Market & Food Centre
    ## 1608   Holland Village Market & Food Centre
    ## 1609   Holland Village Market & Food Centre
    ## 1610   Holland Village Market & Food Centre
    ## 1611   Holland Village Market & Food Centre
    ## 1612   Holland Village Market & Food Centre
    ## 1613   Holland Village Market & Food Centre
    ## 1614   Holland Village Market & Food Centre
    ## 1615   Holland Village Market & Food Centre
    ## 1616   Holland Village Market & Food Centre
    ## 1617   Holland Village Market & Food Centre
    ## 1618   Holland Village Market & Food Centre
    ## 1619   Holland Village Market & Food Centre
    ## 1620   Holland Village Market & Food Centre
    ## 1621          Hong Lim Market & Food Centre
    ## 1622          Hong Lim Market & Food Centre
    ## 1623          Hong Lim Market & Food Centre
    ## 1624          Hong Lim Market & Food Centre
    ## 1625          Hong Lim Market & Food Centre
    ## 1626          Hong Lim Market & Food Centre
    ## 1627          Hong Lim Market & Food Centre
    ## 1628          Hong Lim Market & Food Centre
    ## 1629          Hong Lim Market & Food Centre
    ## 1630          Hong Lim Market & Food Centre
    ## 1631          Hong Lim Market & Food Centre
    ## 1632          Hong Lim Market & Food Centre
    ## 1633          Hong Lim Market & Food Centre
    ## 1634          Hong Lim Market & Food Centre
    ## 1635          Hong Lim Market & Food Centre
    ## 1636          Hong Lim Market & Food Centre
    ## 1637          Hong Lim Market & Food Centre
    ## 1638          Hong Lim Market & Food Centre
    ## 1639          Hong Lim Market & Food Centre
    ## 1640          Hong Lim Market & Food Centre
    ## 1641          Hong Lim Market & Food Centre
    ## 1642          Hong Lim Market & Food Centre
    ## 1643          Hong Lim Market & Food Centre
    ## 1644          Hong Lim Market & Food Centre
    ## 1645          Hong Lim Market & Food Centre
    ## 1646          Hong Lim Market & Food Centre
    ## 1647          Hong Lim Market & Food Centre
    ## 1648          Hong Lim Market & Food Centre
    ## 1649          Hong Lim Market & Food Centre
    ## 1650          Hong Lim Market & Food Centre
    ## 1651          Hong Lim Market & Food Centre
    ## 1652          Hong Lim Market & Food Centre
    ## 1653          Hong Lim Market & Food Centre
    ## 1654          Hong Lim Market & Food Centre
    ## 1655          Hong Lim Market & Food Centre
    ## 1656          Hong Lim Market & Food Centre
    ## 1657          Hong Lim Market & Food Centre
    ## 1658          Hong Lim Market & Food Centre
    ## 1659          Hong Lim Market & Food Centre
    ## 1660          Hong Lim Market & Food Centre
    ## 1661          Hong Lim Market & Food Centre
    ## 1662          Hong Lim Market & Food Centre
    ## 1663          Hong Lim Market & Food Centre
    ## 1664          Hong Lim Market & Food Centre
    ## 1665          Hong Lim Market & Food Centre
    ## 1666          Hong Lim Market & Food Centre
    ## 1667          Hong Lim Market & Food Centre
    ## 1668          Hong Lim Market & Food Centre
    ## 1669          Hong Lim Market & Food Centre
    ## 1670          Hong Lim Market & Food Centre
    ## 1671          Hong Lim Market & Food Centre
    ## 1672          Hong Lim Market & Food Centre
    ## 1673          Hong Lim Market & Food Centre
    ## 1674          Hong Lim Market & Food Centre
    ## 1675          Hong Lim Market & Food Centre
    ## 1676          Hong Lim Market & Food Centre
    ## 1677          Hong Lim Market & Food Centre
    ## 1678          Hong Lim Market & Food Centre
    ## 1679          Hong Lim Market & Food Centre
    ## 1680          Hong Lim Market & Food Centre
    ## 1681          Hong Lim Market & Food Centre
    ## 1682          Hong Lim Market & Food Centre
    ## 1683          Hong Lim Market & Food Centre
    ## 1684          Hong Lim Market & Food Centre
    ## 1685          Hong Lim Market & Food Centre
    ## 1686          Hong Lim Market & Food Centre
    ## 1687          Hong Lim Market & Food Centre
    ## 1688          Hong Lim Market & Food Centre
    ## 1689          Hong Lim Market & Food Centre
    ## 1690          Hong Lim Market & Food Centre
    ## 1691          Hong Lim Market & Food Centre
    ## 1692          Hong Lim Market & Food Centre
    ## 1693          Hong Lim Market & Food Centre
    ## 1694          Hong Lim Market & Food Centre
    ## 1695          Hong Lim Market & Food Centre
    ## 1696          Hong Lim Market & Food Centre
    ## 1697          Hong Lim Market & Food Centre
    ## 1698          Hong Lim Market & Food Centre
    ## 1699          Hong Lim Market & Food Centre
    ## 1700          Hong Lim Market & Food Centre
    ## 1701          Hong Lim Market & Food Centre
    ## 1702          Hong Lim Market & Food Centre
    ## 1703          Hong Lim Market & Food Centre
    ## 1704          Hong Lim Market & Food Centre
    ## 1705          Hong Lim Market & Food Centre
    ## 1706          Hong Lim Market & Food Centre
    ## 1707          Hong Lim Market & Food Centre
    ## 1708          Hong Lim Market & Food Centre
    ## 1709          Hong Lim Market & Food Centre
    ## 1710          Hong Lim Market & Food Centre
    ## 1711          Hong Lim Market & Food Centre
    ## 1712          Hong Lim Market & Food Centre
    ## 1713          Hong Lim Market & Food Centre
    ## 1714          Hong Lim Market & Food Centre
    ## 1715          Hong Lim Market & Food Centre
    ## 1716          Hong Lim Market & Food Centre
    ## 1717          Hong Lim Market & Food Centre
    ## 1718          Hong Lim Market & Food Centre
    ## 1719          Hong Lim Market & Food Centre
    ## 1720          Hong Lim Market & Food Centre
    ## 1721                  Kallang Estate Market
    ## 1722                  Kallang Estate Market
    ## 1723                  Kallang Estate Market
    ## 1724                  Kallang Estate Market
    ## 1725                  Kallang Estate Market
    ## 1726                  Kallang Estate Market
    ## 1727                  Kallang Estate Market
    ## 1728                  Kallang Estate Market
    ## 1729                  Kallang Estate Market
    ## 1730                  Kallang Estate Market
    ## 1731                  Kallang Estate Market
    ## 1732                  Kallang Estate Market
    ## 1733                  Kallang Estate Market
    ## 1734                  Kallang Estate Market
    ## 1735                  Kallang Estate Market
    ## 1736                  Kallang Estate Market
    ## 1737                  Kallang Estate Market
    ## 1738                  Kallang Estate Market
    ## 1739                  Kallang Estate Market
    ## 1740                  Kallang Estate Market
    ## 1741                  Kallang Estate Market
    ## 1742                  Kallang Estate Market
    ## 1743                  Kallang Estate Market
    ## 1744                  Kallang Estate Market
    ## 1745                  Kallang Estate Market
    ## 1746                  Kallang Estate Market
    ## 1747                  Kallang Estate Market
    ## 1748                  Kallang Estate Market
    ## 1749                  Kallang Estate Market
    ## 1750                  Kallang Estate Market
    ## 1751                  Kallang Estate Market
    ## 1752                  Kallang Estate Market
    ## 1753                  Kallang Estate Market
    ## 1754                  Kallang Estate Market
    ## 1755                  Kallang Estate Market
    ## 1756                  Kallang Estate Market
    ## 1757                  Kallang Estate Market
    ## 1758                  Kallang Estate Market
    ## 1759                  Kallang Estate Market
    ## 1760                  Kallang Estate Market
    ## 1761                  Kallang Estate Market
    ## 1762                  Kallang Estate Market
    ## 1763                  Kallang Estate Market
    ## 1764                  Kallang Estate Market
    ## 1765                  Kallang Estate Market
    ## 1766                  Kallang Estate Market
    ## 1767                  Kallang Estate Market
    ## 1768                  Kallang Estate Market
    ## 1769                  Kallang Estate Market
    ## 1770                  Kallang Estate Market
    ## 1771                  Kallang Estate Market
    ## 1772                  Kallang Estate Market
    ## 1773                  Kallang Estate Market
    ## 1774                  Kallang Estate Market
    ## 1775                  Kallang Estate Market
    ## 1776                  Kallang Estate Market
    ## 1777                  Kallang Estate Market
    ## 1778                  Kallang Estate Market
    ## 1779                  Kallang Estate Market
    ## 1780                  Kallang Estate Market
    ## 1781                  Kallang Estate Market
    ## 1782                  Kallang Estate Market
    ## 1783                  Kallang Estate Market
    ## 1784                  Kallang Estate Market
    ## 1785                  Kallang Estate Market
    ## 1786                  Kallang Estate Market
    ## 1787                  Kallang Estate Market
    ## 1788                  Kallang Estate Market
    ## 1789                  Kallang Estate Market
    ## 1790                  Kallang Estate Market
    ## 1791                  Kallang Estate Market
    ## 1792                  Kallang Estate Market
    ## 1793                  Kallang Estate Market
    ## 1794                  Kallang Estate Market
    ## 1795                  Kallang Estate Market
    ## 1796                  Kallang Estate Market
    ## 1797                  Kallang Estate Market
    ## 1798                  Kallang Estate Market
    ## 1799                  Kallang Estate Market
    ## 1800                  Kallang Estate Market
    ## 1801                  Kallang Estate Market
    ## 1802                  Kallang Estate Market
    ## 1803                  Kallang Estate Market
    ## 1804                  Kallang Estate Market
    ## 1805                  Kallang Estate Market
    ## 1806                  Kallang Estate Market
    ## 1807                  Kallang Estate Market
    ## 1808                  Kallang Estate Market
    ## 1809                  Kallang Estate Market
    ## 1810                  Kallang Estate Market
    ## 1811                  Kallang Estate Market
    ## 1812                  Kallang Estate Market
    ## 1813                  Kallang Estate Market
    ## 1814                  Kallang Estate Market
    ## 1815                  Kallang Estate Market
    ## 1816                  Kallang Estate Market
    ## 1817                  Kallang Estate Market
    ## 1818                  Kallang Estate Market
    ## 1819                  Kallang Estate Market
    ## 1820             Kovan Market & Food Centre
    ## 1821             Kovan Market & Food Centre
    ## 1822             Kovan Market & Food Centre
    ## 1823             Kovan Market & Food Centre
    ## 1824             Kovan Market & Food Centre
    ## 1825             Kovan Market & Food Centre
    ## 1826             Kovan Market & Food Centre
    ## 1827             Kovan Market & Food Centre
    ## 1828             Kovan Market & Food Centre
    ## 1829             Kovan Market & Food Centre
    ## 1830             Kovan Market & Food Centre
    ## 1831             Kovan Market & Food Centre
    ## 1832             Kovan Market & Food Centre
    ## 1833             Kovan Market & Food Centre
    ## 1834             Kovan Market & Food Centre
    ## 1835             Kovan Market & Food Centre
    ## 1836             Kovan Market & Food Centre
    ## 1837             Kovan Market & Food Centre
    ## 1838             Kovan Market & Food Centre
    ## 1839             Kovan Market & Food Centre
    ## 1840             Kovan Market & Food Centre
    ## 1841             Kovan Market & Food Centre
    ## 1842             Kovan Market & Food Centre
    ## 1843             Kovan Market & Food Centre
    ## 1844             Kovan Market & Food Centre
    ## 1845             Kovan Market & Food Centre
    ## 1846             Kovan Market & Food Centre
    ## 1847             Kovan Market & Food Centre
    ## 1848             Kovan Market & Food Centre
    ## 1849             Kovan Market & Food Centre
    ## 1850             Kovan Market & Food Centre
    ## 1851             Kovan Market & Food Centre
    ## 1852             Kovan Market & Food Centre
    ## 1853             Kovan Market & Food Centre
    ## 1854             Kovan Market & Food Centre
    ## 1855             Kovan Market & Food Centre
    ## 1856             Kovan Market & Food Centre
    ## 1857             Kovan Market & Food Centre
    ## 1858             Kovan Market & Food Centre
    ## 1859             Kovan Market & Food Centre
    ## 1860             Kovan Market & Food Centre
    ## 1861             Kovan Market & Food Centre
    ## 1862             Kovan Market & Food Centre
    ## 1863             Kovan Market & Food Centre
    ## 1864             Kovan Market & Food Centre
    ## 1865             Kovan Market & Food Centre
    ## 1866             Kovan Market & Food Centre
    ## 1867             Kovan Market & Food Centre
    ## 1868             Kovan Market & Food Centre
    ## 1869             Kovan Market & Food Centre
    ## 1870             Kovan Market & Food Centre
    ## 1871             Kovan Market & Food Centre
    ## 1872             Kovan Market & Food Centre
    ## 1873             Kovan Market & Food Centre
    ## 1874             Kovan Market & Food Centre
    ## 1875             Kovan Market & Food Centre
    ## 1876             Kovan Market & Food Centre
    ## 1877             Kovan Market & Food Centre
    ## 1878             Kovan Market & Food Centre
    ## 1879             Kovan Market & Food Centre
    ## 1880             Kovan Market & Food Centre
    ## 1881             Kovan Market & Food Centre
    ## 1882             Kovan Market & Food Centre
    ## 1883             Kovan Market & Food Centre
    ## 1884             Kovan Market & Food Centre
    ## 1885             Kovan Market & Food Centre
    ## 1886             Kovan Market & Food Centre
    ## 1887             Kovan Market & Food Centre
    ## 1888             Kovan Market & Food Centre
    ## 1889             Kovan Market & Food Centre
    ## 1890             Kovan Market & Food Centre
    ## 1891             Kovan Market & Food Centre
    ## 1892             Kovan Market & Food Centre
    ## 1893             Kovan Market & Food Centre
    ## 1894             Kovan Market & Food Centre
    ## 1895             Kovan Market & Food Centre
    ## 1896             Kovan Market & Food Centre
    ## 1897             Kovan Market & Food Centre
    ## 1898             Kovan Market & Food Centre
    ## 1899             Kovan Market & Food Centre
    ## 1900             Kovan Market & Food Centre
    ## 1901             Kovan Market & Food Centre
    ## 1902             Kovan Market & Food Centre
    ## 1903             Kovan Market & Food Centre
    ## 1904             Kovan Market & Food Centre
    ## 1905             Kovan Market & Food Centre
    ## 1906             Kovan Market & Food Centre
    ## 1907             Kovan Market & Food Centre
    ## 1908             Kovan Market & Food Centre
    ## 1909             Kovan Market & Food Centre
    ## 1910             Kovan Market & Food Centre
    ## 1911             Kovan Market & Food Centre
    ## 1912             Kovan Market & Food Centre
    ## 1913             Kovan Market & Food Centre
    ## 1914             Kovan Market & Food Centre
    ## 1915             Kovan Market & Food Centre
    ## 1916             Kovan Market & Food Centre
    ## 1917             Kovan Market & Food Centre
    ## 1918         Blks 2 & 3 Changi Village Road
    ## 1919         Blks 2 & 3 Changi Village Road
    ## 1920         Blks 2 & 3 Changi Village Road
    ## 1921         Blks 2 & 3 Changi Village Road
    ## 1922         Blks 2 & 3 Changi Village Road
    ## 1923         Blks 2 & 3 Changi Village Road
    ## 1924         Blks 2 & 3 Changi Village Road
    ## 1925         Blks 2 & 3 Changi Village Road
    ## 1926         Blks 2 & 3 Changi Village Road
    ## 1927         Blks 2 & 3 Changi Village Road
    ## 1928         Blks 2 & 3 Changi Village Road
    ## 1929         Blks 2 & 3 Changi Village Road
    ## 1930         Blks 2 & 3 Changi Village Road
    ## 1931         Blks 2 & 3 Changi Village Road
    ## 1932         Blks 2 & 3 Changi Village Road
    ## 1933         Blks 2 & 3 Changi Village Road
    ## 1934         Blks 2 & 3 Changi Village Road
    ## 1935         Blks 2 & 3 Changi Village Road
    ## 1936         Blks 2 & 3 Changi Village Road
    ## 1937         Blks 2 & 3 Changi Village Road
    ## 1938         Blks 2 & 3 Changi Village Road
    ## 1939         Blks 2 & 3 Changi Village Road
    ## 1940         Blks 2 & 3 Changi Village Road
    ## 1941         Blks 2 & 3 Changi Village Road
    ## 1942         Blks 2 & 3 Changi Village Road
    ## 1943         Blks 2 & 3 Changi Village Road
    ## 1944         Blks 2 & 3 Changi Village Road
    ## 1945         Blks 2 & 3 Changi Village Road
    ## 1946         Blks 2 & 3 Changi Village Road
    ## 1947         Blks 2 & 3 Changi Village Road
    ## 1948         Blks 2 & 3 Changi Village Road
    ## 1949         Blks 2 & 3 Changi Village Road
    ## 1950         Blks 2 & 3 Changi Village Road
    ## 1951         Blks 2 & 3 Changi Village Road
    ## 1952         Blks 2 & 3 Changi Village Road
    ## 1953         Blks 2 & 3 Changi Village Road
    ## 1954         Blks 2 & 3 Changi Village Road
    ## 1955         Blks 2 & 3 Changi Village Road
    ## 1956         Blks 2 & 3 Changi Village Road
    ## 1957         Blks 2 & 3 Changi Village Road
    ## 1958         Blks 2 & 3 Changi Village Road
    ## 1959         Blks 2 & 3 Changi Village Road
    ## 1960         Blks 2 & 3 Changi Village Road
    ## 1961         Blks 2 & 3 Changi Village Road
    ## 1962         Blks 2 & 3 Changi Village Road
    ## 1963         Blks 2 & 3 Changi Village Road
    ## 1964         Blks 2 & 3 Changi Village Road
    ## 1965         Blks 2 & 3 Changi Village Road
    ## 1966         Blks 2 & 3 Changi Village Road
    ## 1967         Blks 2 & 3 Changi Village Road
    ## 1968         Blks 2 & 3 Changi Village Road
    ## 1969         Blks 2 & 3 Changi Village Road
    ## 1970         Blks 2 & 3 Changi Village Road
    ## 1971         Blks 2 & 3 Changi Village Road
    ## 1972         Blks 2 & 3 Changi Village Road
    ## 1973         Blks 2 & 3 Changi Village Road
    ## 1974         Blks 2 & 3 Changi Village Road
    ## 1975         Blks 2 & 3 Changi Village Road
    ## 1976         Blks 2 & 3 Changi Village Road
    ## 1977         Blks 2 & 3 Changi Village Road
    ## 1978         Blks 2 & 3 Changi Village Road
    ## 1979         Blks 2 & 3 Changi Village Road
    ## 1980         Blks 2 & 3 Changi Village Road
    ## 1981         Blks 2 & 3 Changi Village Road
    ## 1982         Blks 2 & 3 Changi Village Road
    ## 1983         Blks 2 & 3 Changi Village Road
    ## 1984         Blks 2 & 3 Changi Village Road
    ## 1985         Blks 2 & 3 Changi Village Road
    ## 1986         Blks 2 & 3 Changi Village Road
    ## 1987         Blks 2 & 3 Changi Village Road
    ## 1988         Blks 2 & 3 Changi Village Road
    ## 1989         Blks 2 & 3 Changi Village Road
    ## 1990         Blks 2 & 3 Changi Village Road
    ## 1991         Blks 2 & 3 Changi Village Road
    ## 1992         Blks 2 & 3 Changi Village Road
    ## 1993         Blks 2 & 3 Changi Village Road
    ## 1994         Blks 2 & 3 Changi Village Road
    ## 1995         Blks 2 & 3 Changi Village Road
    ## 1996         Blks 2 & 3 Changi Village Road
    ## 1997         Blks 2 & 3 Changi Village Road
    ## 1998         Blks 2 & 3 Changi Village Road
    ## 1999         Blks 2 & 3 Changi Village Road
    ## 2000         Blks 2 & 3 Changi Village Road
    ## 2001         Blks 2 & 3 Changi Village Road
    ## 2002         Blks 2 & 3 Changi Village Road
    ## 2003         Blks 2 & 3 Changi Village Road
    ## 2004         Blks 2 & 3 Changi Village Road
    ## 2005         Blks 2 & 3 Changi Village Road
    ## 2006         Blks 2 & 3 Changi Village Road
    ## 2007         Blks 2 & 3 Changi Village Road
    ## 2008         Blks 2 & 3 Changi Village Road
    ## 2009         Blks 2 & 3 Changi Village Road
    ## 2010         Blks 2 & 3 Changi Village Road
    ## 2011         Blks 2 & 3 Changi Village Road
    ## 2012         Blks 2 & 3 Changi Village Road
    ## 2013         Blks 2 & 3 Changi Village Road
    ## 2014         Blks 2 & 3 Changi Village Road
    ## 2015           Commonwealth Crescent Market
    ## 2016           Commonwealth Crescent Market
    ## 2017           Commonwealth Crescent Market
    ## 2018           Commonwealth Crescent Market
    ## 2019           Commonwealth Crescent Market
    ## 2020           Commonwealth Crescent Market
    ## 2021           Commonwealth Crescent Market
    ## 2022           Commonwealth Crescent Market
    ## 2023           Commonwealth Crescent Market
    ## 2024           Commonwealth Crescent Market
    ## 2025           Commonwealth Crescent Market
    ## 2026           Commonwealth Crescent Market
    ## 2027           Commonwealth Crescent Market
    ## 2028           Commonwealth Crescent Market
    ## 2029           Commonwealth Crescent Market
    ## 2030           Commonwealth Crescent Market
    ## 2031           Commonwealth Crescent Market
    ## 2032           Commonwealth Crescent Market
    ## 2033           Commonwealth Crescent Market
    ## 2034           Commonwealth Crescent Market
    ## 2035           Commonwealth Crescent Market
    ## 2036           Commonwealth Crescent Market
    ## 2037           Commonwealth Crescent Market
    ## 2038           Commonwealth Crescent Market
    ## 2039           Commonwealth Crescent Market
    ## 2040           Commonwealth Crescent Market
    ## 2041           Commonwealth Crescent Market
    ## 2042           Commonwealth Crescent Market
    ## 2043           Commonwealth Crescent Market
    ## 2044           Commonwealth Crescent Market
    ## 2045           Commonwealth Crescent Market
    ## 2046           Commonwealth Crescent Market
    ## 2047           Commonwealth Crescent Market
    ## 2048           Commonwealth Crescent Market
    ## 2049           Commonwealth Crescent Market
    ## 2050           Commonwealth Crescent Market
    ## 2051           Commonwealth Crescent Market
    ## 2052           Commonwealth Crescent Market
    ## 2053           Commonwealth Crescent Market
    ## 2054           Commonwealth Crescent Market
    ## 2055           Commonwealth Crescent Market
    ## 2056           Commonwealth Crescent Market
    ## 2057           Commonwealth Crescent Market
    ## 2058           Commonwealth Crescent Market
    ## 2059           Commonwealth Crescent Market
    ## 2060           Commonwealth Crescent Market
    ## 2061           Commonwealth Crescent Market
    ## 2062           Commonwealth Crescent Market
    ## 2063           Commonwealth Crescent Market
    ## 2064           Commonwealth Crescent Market
    ## 2065           Commonwealth Crescent Market
    ## 2066           Commonwealth Crescent Market
    ## 2067           Commonwealth Crescent Market
    ## 2068           Commonwealth Crescent Market
    ## 2069           Commonwealth Crescent Market
    ## 2070           Commonwealth Crescent Market
    ## 2071           Commonwealth Crescent Market
    ## 2072           Commonwealth Crescent Market
    ## 2073           Commonwealth Crescent Market
    ## 2074           Commonwealth Crescent Market
    ## 2075           Commonwealth Crescent Market
    ## 2076           Commonwealth Crescent Market
    ## 2077           Commonwealth Crescent Market
    ## 2078           Commonwealth Crescent Market
    ## 2079           Commonwealth Crescent Market
    ## 2080           Commonwealth Crescent Market
    ## 2081           Commonwealth Crescent Market
    ## 2082           Commonwealth Crescent Market
    ## 2083           Commonwealth Crescent Market
    ## 2084           Commonwealth Crescent Market
    ## 2085           Commonwealth Crescent Market
    ## 2086           Commonwealth Crescent Market
    ## 2087           Commonwealth Crescent Market
    ## 2088           Commonwealth Crescent Market
    ## 2089           Commonwealth Crescent Market
    ## 2090           Commonwealth Crescent Market
    ## 2091           Commonwealth Crescent Market
    ## 2092           Commonwealth Crescent Market
    ## 2093           Commonwealth Crescent Market
    ## 2094           Commonwealth Crescent Market
    ## 2095           Commonwealth Crescent Market
    ## 2096           Commonwealth Crescent Market
    ## 2097           Commonwealth Crescent Market
    ## 2098           Commonwealth Crescent Market
    ## 2099           Commonwealth Crescent Market
    ## 2100           Commonwealth Crescent Market
    ## 2101           Commonwealth Crescent Market
    ## 2102           Commonwealth Crescent Market
    ## 2103           Commonwealth Crescent Market
    ## 2104           Commonwealth Crescent Market
    ## 2105           Commonwealth Crescent Market
    ## 2106           Commonwealth Crescent Market
    ## 2107           Commonwealth Crescent Market
    ## 2108           Commonwealth Crescent Market
    ## 2109           Commonwealth Crescent Market
    ## 2110           Commonwealth Crescent Market
    ## 2111    ABC Brickworks Market & Food Centre
    ## 2112    ABC Brickworks Market & Food Centre
    ## 2113    ABC Brickworks Market & Food Centre
    ## 2114    ABC Brickworks Market & Food Centre
    ## 2115    ABC Brickworks Market & Food Centre
    ## 2116    ABC Brickworks Market & Food Centre
    ## 2117    ABC Brickworks Market & Food Centre
    ## 2118    ABC Brickworks Market & Food Centre
    ## 2119    ABC Brickworks Market & Food Centre
    ## 2120    ABC Brickworks Market & Food Centre
    ## 2121    ABC Brickworks Market & Food Centre
    ## 2122    ABC Brickworks Market & Food Centre
    ## 2123    ABC Brickworks Market & Food Centre
    ## 2124    ABC Brickworks Market & Food Centre
    ## 2125    ABC Brickworks Market & Food Centre
    ## 2126    ABC Brickworks Market & Food Centre
    ## 2127    ABC Brickworks Market & Food Centre
    ## 2128    ABC Brickworks Market & Food Centre
    ## 2129    ABC Brickworks Market & Food Centre
    ## 2130    ABC Brickworks Market & Food Centre
    ## 2131    ABC Brickworks Market & Food Centre
    ## 2132    ABC Brickworks Market & Food Centre
    ## 2133    ABC Brickworks Market & Food Centre
    ## 2134    ABC Brickworks Market & Food Centre
    ## 2135    ABC Brickworks Market & Food Centre
    ## 2136    ABC Brickworks Market & Food Centre
    ## 2137    ABC Brickworks Market & Food Centre
    ## 2138    ABC Brickworks Market & Food Centre
    ## 2139    ABC Brickworks Market & Food Centre
    ## 2140    ABC Brickworks Market & Food Centre
    ## 2141    ABC Brickworks Market & Food Centre
    ## 2142    ABC Brickworks Market & Food Centre
    ## 2143    ABC Brickworks Market & Food Centre
    ## 2144    ABC Brickworks Market & Food Centre
    ## 2145    ABC Brickworks Market & Food Centre
    ## 2146    ABC Brickworks Market & Food Centre
    ## 2147    ABC Brickworks Market & Food Centre
    ## 2148    ABC Brickworks Market & Food Centre
    ## 2149    ABC Brickworks Market & Food Centre
    ## 2150    ABC Brickworks Market & Food Centre
    ## 2151    ABC Brickworks Market & Food Centre
    ## 2152    ABC Brickworks Market & Food Centre
    ## 2153    ABC Brickworks Market & Food Centre
    ## 2154    ABC Brickworks Market & Food Centre
    ## 2155    ABC Brickworks Market & Food Centre
    ## 2156    ABC Brickworks Market & Food Centre
    ## 2157    ABC Brickworks Market & Food Centre
    ## 2158    ABC Brickworks Market & Food Centre
    ## 2159    ABC Brickworks Market & Food Centre
    ## 2160    ABC Brickworks Market & Food Centre
    ## 2161    ABC Brickworks Market & Food Centre
    ## 2162    ABC Brickworks Market & Food Centre
    ## 2163    ABC Brickworks Market & Food Centre
    ## 2164    ABC Brickworks Market & Food Centre
    ## 2165    ABC Brickworks Market & Food Centre
    ## 2166    ABC Brickworks Market & Food Centre
    ## 2167    ABC Brickworks Market & Food Centre
    ## 2168    ABC Brickworks Market & Food Centre
    ## 2169    ABC Brickworks Market & Food Centre
    ## 2170    ABC Brickworks Market & Food Centre
    ## 2171    ABC Brickworks Market & Food Centre
    ## 2172    ABC Brickworks Market & Food Centre
    ## 2173    ABC Brickworks Market & Food Centre
    ## 2174    ABC Brickworks Market & Food Centre
    ## 2175    ABC Brickworks Market & Food Centre
    ## 2176    ABC Brickworks Market & Food Centre
    ## 2177    ABC Brickworks Market & Food Centre
    ## 2178    ABC Brickworks Market & Food Centre
    ## 2179    ABC Brickworks Market & Food Centre
    ## 2180    ABC Brickworks Market & Food Centre
    ## 2181    ABC Brickworks Market & Food Centre
    ## 2182    ABC Brickworks Market & Food Centre
    ## 2183    ABC Brickworks Market & Food Centre
    ## 2184    ABC Brickworks Market & Food Centre
    ## 2185    ABC Brickworks Market & Food Centre
    ## 2186    ABC Brickworks Market & Food Centre
    ## 2187    ABC Brickworks Market & Food Centre
    ## 2188    ABC Brickworks Market & Food Centre
    ## 2189    ABC Brickworks Market & Food Centre
    ## 2190    ABC Brickworks Market & Food Centre
    ## 2191    ABC Brickworks Market & Food Centre
    ## 2192    ABC Brickworks Market & Food Centre
    ## 2193    ABC Brickworks Market & Food Centre
    ## 2194    ABC Brickworks Market & Food Centre
    ## 2195    ABC Brickworks Market & Food Centre
    ## 2196    ABC Brickworks Market & Food Centre
    ## 2197    ABC Brickworks Market & Food Centre
    ## 2198    ABC Brickworks Market & Food Centre
    ## 2199    ABC Brickworks Market & Food Centre
    ## 2200    ABC Brickworks Market & Food Centre
    ## 2201    ABC Brickworks Market & Food Centre
    ## 2202    ABC Brickworks Market & Food Centre
    ## 2203    ABC Brickworks Market & Food Centre
    ## 2204    ABC Brickworks Market & Food Centre
    ## 2205    ABC Brickworks Market & Food Centre
    ## 2206                       Adam Food Centre
    ## 2207                       Adam Food Centre
    ## 2208                       Adam Food Centre
    ## 2209                       Adam Food Centre
    ## 2210                       Adam Food Centre
    ## 2211                       Adam Food Centre
    ## 2212                       Adam Food Centre
    ## 2213                       Adam Food Centre
    ## 2214                       Adam Food Centre
    ## 2215                       Adam Food Centre
    ## 2216                       Adam Food Centre
    ## 2217                       Adam Food Centre
    ## 2218                       Adam Food Centre
    ## 2219                       Adam Food Centre
    ## 2220                       Adam Food Centre
    ## 2221                       Adam Food Centre
    ## 2222                       Adam Food Centre
    ## 2223                       Adam Food Centre
    ## 2224                       Adam Food Centre
    ## 2225                       Adam Food Centre
    ## 2226                       Adam Food Centre
    ## 2227                       Adam Food Centre
    ## 2228                       Adam Food Centre
    ## 2229                       Adam Food Centre
    ## 2230                       Adam Food Centre
    ## 2231                       Adam Food Centre
    ## 2232                       Adam Food Centre
    ## 2233                       Adam Food Centre
    ## 2234                       Adam Food Centre
    ## 2235                       Adam Food Centre
    ## 2236                       Adam Food Centre
    ## 2237                       Adam Food Centre
    ## 2238                       Adam Food Centre
    ## 2239                       Adam Food Centre
    ## 2240                       Adam Food Centre
    ## 2241                       Adam Food Centre
    ## 2242                       Adam Food Centre
    ## 2243                       Adam Food Centre
    ## 2244                       Adam Food Centre
    ## 2245                       Adam Food Centre
    ## 2246                       Adam Food Centre
    ## 2247                       Adam Food Centre
    ## 2248                       Adam Food Centre
    ## 2249                       Adam Food Centre
    ## 2250                       Adam Food Centre
    ## 2251                       Adam Food Centre
    ## 2252                       Adam Food Centre
    ## 2253                       Adam Food Centre
    ## 2254                       Adam Food Centre
    ## 2255                       Adam Food Centre
    ## 2256                       Adam Food Centre
    ## 2257                       Adam Food Centre
    ## 2258                       Adam Food Centre
    ## 2259                       Adam Food Centre
    ## 2260                       Adam Food Centre
    ## 2261                       Adam Food Centre
    ## 2262                       Adam Food Centre
    ## 2263                       Adam Food Centre
    ## 2264                       Adam Food Centre
    ## 2265                       Adam Food Centre
    ## 2266                       Adam Food Centre
    ## 2267                       Adam Food Centre
    ## 2268                       Adam Food Centre
    ## 2269                       Adam Food Centre
    ## 2270                       Adam Food Centre
    ## 2271                       Adam Food Centre
    ## 2272                       Adam Food Centre
    ## 2273                       Adam Food Centre
    ## 2274                       Adam Food Centre
    ## 2275                       Adam Food Centre
    ## 2276                       Adam Food Centre
    ## 2277                       Adam Food Centre
    ## 2278                       Adam Food Centre
    ## 2279                       Adam Food Centre
    ## 2280                       Adam Food Centre
    ## 2281                       Adam Food Centre
    ## 2282                       Adam Food Centre
    ## 2283                       Adam Food Centre
    ## 2284                       Adam Food Centre
    ## 2285                       Adam Food Centre
    ## 2286                       Adam Food Centre
    ## 2287                       Adam Food Centre
    ## 2288                       Adam Food Centre
    ## 2289                       Adam Food Centre
    ## 2290                       Adam Food Centre
    ## 2291                       Adam Food Centre
    ## 2292                       Adam Food Centre
    ## 2293                       Adam Food Centre
    ## 2294                       Adam Food Centre
    ## 2295                       Adam Food Centre
    ## 2296                       Adam Food Centre
    ## 2297                       Adam Food Centre
    ## 2298                       Adam Food Centre
    ## 2299                       Adam Food Centre
    ## 2300     Albert Centre Market & Food Centre
    ## 2301     Albert Centre Market & Food Centre
    ## 2302     Albert Centre Market & Food Centre
    ## 2303     Albert Centre Market & Food Centre
    ## 2304     Albert Centre Market & Food Centre
    ## 2305     Albert Centre Market & Food Centre
    ## 2306     Albert Centre Market & Food Centre
    ## 2307     Albert Centre Market & Food Centre
    ## 2308     Albert Centre Market & Food Centre
    ## 2309     Albert Centre Market & Food Centre
    ## 2310     Albert Centre Market & Food Centre
    ## 2311     Albert Centre Market & Food Centre
    ## 2312     Albert Centre Market & Food Centre
    ## 2313     Albert Centre Market & Food Centre
    ## 2314     Albert Centre Market & Food Centre
    ## 2315     Albert Centre Market & Food Centre
    ## 2316     Albert Centre Market & Food Centre
    ## 2317     Albert Centre Market & Food Centre
    ## 2318     Albert Centre Market & Food Centre
    ## 2319     Albert Centre Market & Food Centre
    ## 2320     Albert Centre Market & Food Centre
    ## 2321     Albert Centre Market & Food Centre
    ## 2322     Albert Centre Market & Food Centre
    ## 2323     Albert Centre Market & Food Centre
    ## 2324     Albert Centre Market & Food Centre
    ## 2325     Albert Centre Market & Food Centre
    ## 2326     Albert Centre Market & Food Centre
    ## 2327     Albert Centre Market & Food Centre
    ## 2328     Albert Centre Market & Food Centre
    ## 2329     Albert Centre Market & Food Centre
    ## 2330     Albert Centre Market & Food Centre
    ## 2331     Albert Centre Market & Food Centre
    ## 2332     Albert Centre Market & Food Centre
    ## 2333     Albert Centre Market & Food Centre
    ## 2334     Albert Centre Market & Food Centre
    ## 2335     Albert Centre Market & Food Centre
    ## 2336     Albert Centre Market & Food Centre
    ## 2337     Albert Centre Market & Food Centre
    ## 2338     Albert Centre Market & Food Centre
    ## 2339     Albert Centre Market & Food Centre
    ## 2340     Albert Centre Market & Food Centre
    ## 2341     Albert Centre Market & Food Centre
    ## 2342     Albert Centre Market & Food Centre
    ## 2343     Albert Centre Market & Food Centre
    ## 2344     Albert Centre Market & Food Centre
    ## 2345     Albert Centre Market & Food Centre
    ## 2346     Albert Centre Market & Food Centre
    ## 2347     Albert Centre Market & Food Centre
    ## 2348     Albert Centre Market & Food Centre
    ## 2349     Albert Centre Market & Food Centre
    ## 2350     Albert Centre Market & Food Centre
    ## 2351     Albert Centre Market & Food Centre
    ## 2352     Albert Centre Market & Food Centre
    ## 2353     Albert Centre Market & Food Centre
    ## 2354     Albert Centre Market & Food Centre
    ## 2355     Albert Centre Market & Food Centre
    ## 2356     Albert Centre Market & Food Centre
    ## 2357     Albert Centre Market & Food Centre
    ## 2358     Albert Centre Market & Food Centre
    ## 2359     Albert Centre Market & Food Centre
    ## 2360     Albert Centre Market & Food Centre
    ## 2361     Albert Centre Market & Food Centre
    ## 2362     Albert Centre Market & Food Centre
    ## 2363     Albert Centre Market & Food Centre
    ## 2364     Albert Centre Market & Food Centre
    ## 2365     Albert Centre Market & Food Centre
    ## 2366     Albert Centre Market & Food Centre
    ## 2367     Albert Centre Market & Food Centre
    ## 2368     Albert Centre Market & Food Centre
    ## 2369     Albert Centre Market & Food Centre
    ## 2370     Albert Centre Market & Food Centre
    ## 2371     Albert Centre Market & Food Centre
    ## 2372     Albert Centre Market & Food Centre
    ## 2373     Albert Centre Market & Food Centre
    ## 2374     Albert Centre Market & Food Centre
    ## 2375     Albert Centre Market & Food Centre
    ## 2376     Albert Centre Market & Food Centre
    ## 2377     Albert Centre Market & Food Centre
    ## 2378     Albert Centre Market & Food Centre
    ## 2379     Albert Centre Market & Food Centre
    ## 2380     Albert Centre Market & Food Centre
    ## 2381     Albert Centre Market & Food Centre
    ## 2382     Albert Centre Market & Food Centre
    ## 2383     Albert Centre Market & Food Centre
    ## 2384     Albert Centre Market & Food Centre
    ## 2385     Albert Centre Market & Food Centre
    ## 2386     Albert Centre Market & Food Centre
    ## 2387     Albert Centre Market & Food Centre
    ## 2388     Albert Centre Market & Food Centre
    ## 2389     Albert Centre Market & Food Centre
    ## 2390     Albert Centre Market & Food Centre
    ## 2391     Albert Centre Market & Food Centre
    ## 2392     Albert Centre Market & Food Centre
    ## 2393          Alexandra Village Food Centre
    ## 2394          Alexandra Village Food Centre
    ## 2395          Alexandra Village Food Centre
    ## 2396          Alexandra Village Food Centre
    ## 2397          Alexandra Village Food Centre
    ## 2398          Alexandra Village Food Centre
    ## 2399          Alexandra Village Food Centre
    ## 2400          Alexandra Village Food Centre
    ## 2401          Alexandra Village Food Centre
    ## 2402          Alexandra Village Food Centre
    ## 2403          Alexandra Village Food Centre
    ## 2404          Alexandra Village Food Centre
    ## 2405          Alexandra Village Food Centre
    ## 2406          Alexandra Village Food Centre
    ## 2407          Alexandra Village Food Centre
    ## 2408          Alexandra Village Food Centre
    ## 2409          Alexandra Village Food Centre
    ## 2410          Alexandra Village Food Centre
    ## 2411          Alexandra Village Food Centre
    ## 2412          Alexandra Village Food Centre
    ## 2413          Alexandra Village Food Centre
    ## 2414          Alexandra Village Food Centre
    ## 2415          Alexandra Village Food Centre
    ## 2416          Alexandra Village Food Centre
    ## 2417          Alexandra Village Food Centre
    ## 2418          Alexandra Village Food Centre
    ## 2419          Alexandra Village Food Centre
    ## 2420          Alexandra Village Food Centre
    ## 2421          Alexandra Village Food Centre
    ## 2422          Alexandra Village Food Centre
    ## 2423          Alexandra Village Food Centre
    ## 2424          Alexandra Village Food Centre
    ## 2425          Alexandra Village Food Centre
    ## 2426          Alexandra Village Food Centre
    ## 2427          Alexandra Village Food Centre
    ## 2428          Alexandra Village Food Centre
    ## 2429          Alexandra Village Food Centre
    ## 2430          Alexandra Village Food Centre
    ## 2431          Alexandra Village Food Centre
    ## 2432          Alexandra Village Food Centre
    ## 2433          Alexandra Village Food Centre
    ## 2434          Alexandra Village Food Centre
    ## 2435          Alexandra Village Food Centre
    ## 2436          Alexandra Village Food Centre
    ## 2437          Alexandra Village Food Centre
    ## 2438          Alexandra Village Food Centre
    ## 2439          Alexandra Village Food Centre
    ## 2440          Alexandra Village Food Centre
    ## 2441          Alexandra Village Food Centre
    ## 2442          Alexandra Village Food Centre
    ## 2443          Alexandra Village Food Centre
    ## 2444          Alexandra Village Food Centre
    ## 2445          Alexandra Village Food Centre
    ## 2446          Alexandra Village Food Centre
    ## 2447          Alexandra Village Food Centre
    ## 2448          Alexandra Village Food Centre
    ## 2449          Alexandra Village Food Centre
    ## 2450          Alexandra Village Food Centre
    ## 2451          Alexandra Village Food Centre
    ## 2452          Alexandra Village Food Centre
    ## 2453          Alexandra Village Food Centre
    ## 2454          Alexandra Village Food Centre
    ## 2455          Alexandra Village Food Centre
    ## 2456          Alexandra Village Food Centre
    ## 2457          Alexandra Village Food Centre
    ## 2458          Alexandra Village Food Centre
    ## 2459          Alexandra Village Food Centre
    ## 2460          Alexandra Village Food Centre
    ## 2461          Alexandra Village Food Centre
    ## 2462          Alexandra Village Food Centre
    ## 2463          Alexandra Village Food Centre
    ## 2464          Alexandra Village Food Centre
    ## 2465          Alexandra Village Food Centre
    ## 2466          Alexandra Village Food Centre
    ## 2467          Alexandra Village Food Centre
    ## 2468          Alexandra Village Food Centre
    ## 2469          Alexandra Village Food Centre
    ## 2470          Alexandra Village Food Centre
    ## 2471          Alexandra Village Food Centre
    ## 2472          Alexandra Village Food Centre
    ## 2473          Alexandra Village Food Centre
    ## 2474          Alexandra Village Food Centre
    ## 2475          Alexandra Village Food Centre
    ## 2476          Alexandra Village Food Centre
    ## 2477          Alexandra Village Food Centre
    ## 2478          Alexandra Village Food Centre
    ## 2479          Alexandra Village Food Centre
    ## 2480          Alexandra Village Food Centre
    ## 2481          Alexandra Village Food Centre
    ## 2482          Alexandra Village Food Centre
    ## 2483          Alexandra Village Food Centre
    ## 2484          Alexandra Village Food Centre
    ## 2485                Amoy Street Food Centre
    ## 2486                Amoy Street Food Centre
    ## 2487                Amoy Street Food Centre
    ## 2488                Amoy Street Food Centre
    ## 2489                Amoy Street Food Centre
    ## 2490                Amoy Street Food Centre
    ## 2491                Amoy Street Food Centre
    ## 2492                Amoy Street Food Centre
    ## 2493                Amoy Street Food Centre
    ## 2494                Amoy Street Food Centre
    ## 2495                Amoy Street Food Centre
    ## 2496                Amoy Street Food Centre
    ## 2497                Amoy Street Food Centre
    ## 2498                Amoy Street Food Centre
    ## 2499                Amoy Street Food Centre
    ## 2500                Amoy Street Food Centre
    ## 2501                Amoy Street Food Centre
    ## 2502                Amoy Street Food Centre
    ## 2503                Amoy Street Food Centre
    ## 2504                Amoy Street Food Centre
    ## 2505                Amoy Street Food Centre
    ## 2506                Amoy Street Food Centre
    ## 2507                Amoy Street Food Centre
    ## 2508                Amoy Street Food Centre
    ## 2509                Amoy Street Food Centre
    ## 2510                Amoy Street Food Centre
    ## 2511                Amoy Street Food Centre
    ## 2512                Amoy Street Food Centre
    ## 2513                Amoy Street Food Centre
    ## 2514                Amoy Street Food Centre
    ## 2515                Amoy Street Food Centre
    ## 2516                Amoy Street Food Centre
    ## 2517                Amoy Street Food Centre
    ## 2518                Amoy Street Food Centre
    ## 2519                Amoy Street Food Centre
    ## 2520                Amoy Street Food Centre
    ## 2521                Amoy Street Food Centre
    ## 2522                Amoy Street Food Centre
    ## 2523                Amoy Street Food Centre
    ## 2524                Amoy Street Food Centre
    ## 2525                Amoy Street Food Centre
    ## 2526                Amoy Street Food Centre
    ## 2527                Amoy Street Food Centre
    ## 2528                Amoy Street Food Centre
    ## 2529                Amoy Street Food Centre
    ## 2530                Amoy Street Food Centre
    ## 2531                Amoy Street Food Centre
    ## 2532                Amoy Street Food Centre
    ## 2533                Amoy Street Food Centre
    ## 2534                Amoy Street Food Centre
    ## 2535                Amoy Street Food Centre
    ## 2536                Amoy Street Food Centre
    ## 2537                Amoy Street Food Centre
    ## 2538                Amoy Street Food Centre
    ## 2539                Amoy Street Food Centre
    ## 2540                Amoy Street Food Centre
    ## 2541                Amoy Street Food Centre
    ## 2542                Amoy Street Food Centre
    ## 2543                Amoy Street Food Centre
    ## 2544                Amoy Street Food Centre
    ## 2545                Amoy Street Food Centre
    ## 2546                Amoy Street Food Centre
    ## 2547                Amoy Street Food Centre
    ## 2548                Amoy Street Food Centre
    ## 2549                Amoy Street Food Centre
    ## 2550                Amoy Street Food Centre
    ## 2551                Amoy Street Food Centre
    ## 2552                Amoy Street Food Centre
    ## 2553                Amoy Street Food Centre
    ## 2554                Amoy Street Food Centre
    ## 2555                Amoy Street Food Centre
    ## 2556                Amoy Street Food Centre
    ## 2557                Amoy Street Food Centre
    ## 2558                Amoy Street Food Centre
    ## 2559                Amoy Street Food Centre
    ## 2560                Amoy Street Food Centre
    ## 2561                Amoy Street Food Centre
    ## 2562                Amoy Street Food Centre
    ## 2563                Amoy Street Food Centre
    ## 2564                Amoy Street Food Centre
    ## 2565                Amoy Street Food Centre
    ## 2566                Amoy Street Food Centre
    ## 2567                Amoy Street Food Centre
    ## 2568                Amoy Street Food Centre
    ## 2569                Amoy Street Food Centre
    ## 2570                Amoy Street Food Centre
    ## 2571                Amoy Street Food Centre
    ## 2572                Amoy Street Food Centre
    ## 2573                Amoy Street Food Centre
    ## 2574                Amoy Street Food Centre
    ## 2575                Amoy Street Food Centre
    ## 2576                      Bedok Food Centre
    ## 2577                      Bedok Food Centre
    ## 2578                      Bedok Food Centre
    ## 2579                      Bedok Food Centre
    ## 2580                      Bedok Food Centre
    ## 2581                      Bedok Food Centre
    ## 2582                      Bedok Food Centre
    ## 2583                      Bedok Food Centre
    ## 2584                      Bedok Food Centre
    ## 2585                      Bedok Food Centre
    ## 2586                      Bedok Food Centre
    ## 2587                      Bedok Food Centre
    ## 2588                      Bedok Food Centre
    ## 2589                      Bedok Food Centre
    ## 2590                      Bedok Food Centre
    ## 2591                      Bedok Food Centre
    ## 2592                      Bedok Food Centre
    ## 2593                      Bedok Food Centre
    ## 2594                      Bedok Food Centre
    ## 2595                      Bedok Food Centre
    ## 2596                      Bedok Food Centre
    ## 2597                      Bedok Food Centre
    ## 2598                      Bedok Food Centre
    ## 2599                      Bedok Food Centre
    ## 2600                      Bedok Food Centre
    ## 2601                      Bedok Food Centre
    ## 2602                      Bedok Food Centre
    ## 2603                      Bedok Food Centre
    ## 2604                      Bedok Food Centre
    ## 2605                      Bedok Food Centre
    ## 2606                      Bedok Food Centre
    ## 2607                      Bedok Food Centre
    ## 2608                      Bedok Food Centre
    ## 2609                      Bedok Food Centre
    ## 2610                      Bedok Food Centre
    ## 2611                      Bedok Food Centre
    ## 2612                      Bedok Food Centre
    ## 2613                      Bedok Food Centre
    ## 2614                      Bedok Food Centre
    ## 2615                      Bedok Food Centre
    ## 2616                      Bedok Food Centre
    ## 2617                      Bedok Food Centre
    ## 2618                      Bedok Food Centre
    ## 2619                      Bedok Food Centre
    ## 2620                      Bedok Food Centre
    ## 2621                      Bedok Food Centre
    ## 2622                      Bedok Food Centre
    ## 2623                      Bedok Food Centre
    ## 2624                      Bedok Food Centre
    ## 2625                      Bedok Food Centre
    ## 2626                      Bedok Food Centre
    ## 2627                      Bedok Food Centre
    ## 2628                      Bedok Food Centre
    ## 2629                      Bedok Food Centre
    ## 2630                      Bedok Food Centre
    ## 2631                      Bedok Food Centre
    ## 2632                      Bedok Food Centre
    ## 2633                      Bedok Food Centre
    ## 2634                      Bedok Food Centre
    ## 2635                      Bedok Food Centre
    ## 2636                      Bedok Food Centre
    ## 2637                      Bedok Food Centre
    ## 2638                      Bedok Food Centre
    ## 2639                      Bedok Food Centre
    ## 2640                      Bedok Food Centre
    ## 2641                      Bedok Food Centre
    ## 2642                      Bedok Food Centre
    ## 2643                      Bedok Food Centre
    ## 2644                      Bedok Food Centre
    ## 2645                      Bedok Food Centre
    ## 2646                      Bedok Food Centre
    ## 2647                      Bedok Food Centre
    ## 2648                      Bedok Food Centre
    ## 2649                      Bedok Food Centre
    ## 2650                      Bedok Food Centre
    ## 2651                      Bedok Food Centre
    ## 2652                      Bedok Food Centre
    ## 2653                      Bedok Food Centre
    ## 2654                      Bedok Food Centre
    ## 2655                      Bedok Food Centre
    ## 2656                      Bedok Food Centre
    ## 2657                      Bedok Food Centre
    ## 2658                      Bedok Food Centre
    ## 2659                      Bedok Food Centre
    ## 2660                      Bedok Food Centre
    ## 2661                      Bedok Food Centre
    ## 2662                      Bedok Food Centre
    ## 2663                      Bedok Food Centre
    ## 2664                      Bedok Food Centre
    ## 2665                      Bedok Food Centre
    ## 2666                    Beo Crescent Market
    ## 2667                    Beo Crescent Market
    ## 2668                    Beo Crescent Market
    ## 2669                    Beo Crescent Market
    ## 2670                    Beo Crescent Market
    ## 2671                    Beo Crescent Market
    ## 2672                    Beo Crescent Market
    ## 2673                    Beo Crescent Market
    ## 2674                    Beo Crescent Market
    ## 2675                    Beo Crescent Market
    ## 2676                    Beo Crescent Market
    ## 2677                    Beo Crescent Market
    ## 2678                    Beo Crescent Market
    ## 2679                    Beo Crescent Market
    ## 2680                    Beo Crescent Market
    ## 2681                    Beo Crescent Market
    ## 2682                    Beo Crescent Market
    ## 2683                    Beo Crescent Market
    ## 2684                    Beo Crescent Market
    ## 2685                    Beo Crescent Market
    ## 2686                    Beo Crescent Market
    ## 2687                    Beo Crescent Market
    ## 2688                    Beo Crescent Market
    ## 2689                    Beo Crescent Market
    ## 2690                    Beo Crescent Market
    ## 2691                    Beo Crescent Market
    ## 2692                    Beo Crescent Market
    ## 2693                    Beo Crescent Market
    ## 2694                    Beo Crescent Market
    ## 2695                    Beo Crescent Market
    ## 2696                    Beo Crescent Market
    ## 2697                    Beo Crescent Market
    ## 2698                    Beo Crescent Market
    ## 2699                    Beo Crescent Market
    ## 2700                    Beo Crescent Market
    ## 2701                    Beo Crescent Market
    ## 2702                    Beo Crescent Market
    ## 2703                    Beo Crescent Market
    ## 2704                    Beo Crescent Market
    ## 2705                    Beo Crescent Market
    ## 2706                    Beo Crescent Market
    ## 2707                    Beo Crescent Market
    ## 2708                    Beo Crescent Market
    ## 2709                    Beo Crescent Market
    ## 2710                    Beo Crescent Market
    ## 2711                    Beo Crescent Market
    ## 2712                    Beo Crescent Market
    ## 2713                    Beo Crescent Market
    ## 2714                    Beo Crescent Market
    ## 2715                    Beo Crescent Market
    ## 2716                    Beo Crescent Market
    ## 2717                    Beo Crescent Market
    ## 2718                    Beo Crescent Market
    ## 2719                    Beo Crescent Market
    ## 2720                    Beo Crescent Market
    ## 2721                    Beo Crescent Market
    ## 2722                    Beo Crescent Market
    ## 2723                    Beo Crescent Market
    ## 2724                    Beo Crescent Market
    ## 2725                    Beo Crescent Market
    ## 2726                    Beo Crescent Market
    ## 2727                    Beo Crescent Market
    ## 2728                    Beo Crescent Market
    ## 2729                    Beo Crescent Market
    ## 2730                    Beo Crescent Market
    ## 2731                    Beo Crescent Market
    ## 2732                    Beo Crescent Market
    ## 2733                    Beo Crescent Market
    ## 2734                    Beo Crescent Market
    ## 2735                    Beo Crescent Market
    ## 2736                    Beo Crescent Market
    ## 2737                    Beo Crescent Market
    ## 2738                    Beo Crescent Market
    ## 2739                    Beo Crescent Market
    ## 2740                    Beo Crescent Market
    ## 2741                    Beo Crescent Market
    ## 2742                    Beo Crescent Market
    ## 2743                    Beo Crescent Market
    ## 2744                    Beo Crescent Market
    ## 2745                    Beo Crescent Market
    ## 2746                    Beo Crescent Market
    ## 2747                    Beo Crescent Market
    ## 2748                    Beo Crescent Market
    ## 2749                    Beo Crescent Market
    ## 2750                    Beo Crescent Market
    ## 2751                    Beo Crescent Market
    ## 2752                    Beo Crescent Market
    ## 2753                    Beo Crescent Market
    ## 2754                    Beo Crescent Market
    ## 2755                     Berseh Food Centre
    ## 2756                     Berseh Food Centre
    ## 2757                     Berseh Food Centre
    ## 2758                     Berseh Food Centre
    ## 2759                     Berseh Food Centre
    ## 2760                     Berseh Food Centre
    ## 2761                     Berseh Food Centre
    ## 2762                     Berseh Food Centre
    ## 2763                     Berseh Food Centre
    ## 2764                     Berseh Food Centre
    ## 2765                     Berseh Food Centre
    ## 2766                     Berseh Food Centre
    ## 2767                     Berseh Food Centre
    ## 2768                     Berseh Food Centre
    ## 2769                     Berseh Food Centre
    ## 2770                     Berseh Food Centre
    ## 2771                     Berseh Food Centre
    ## 2772                     Berseh Food Centre
    ## 2773                     Berseh Food Centre
    ## 2774                     Berseh Food Centre
    ## 2775                     Berseh Food Centre
    ## 2776                     Berseh Food Centre
    ## 2777                     Berseh Food Centre
    ## 2778                     Berseh Food Centre
    ## 2779                     Berseh Food Centre
    ## 2780                     Berseh Food Centre
    ## 2781                     Berseh Food Centre
    ## 2782                     Berseh Food Centre
    ## 2783                     Berseh Food Centre
    ## 2784                     Berseh Food Centre
    ## 2785                     Berseh Food Centre
    ## 2786                     Berseh Food Centre
    ## 2787                     Berseh Food Centre
    ## 2788                     Berseh Food Centre
    ## 2789                     Berseh Food Centre
    ## 2790                     Berseh Food Centre
    ## 2791                     Berseh Food Centre
    ## 2792                     Berseh Food Centre
    ## 2793                     Berseh Food Centre
    ## 2794                     Berseh Food Centre
    ## 2795                     Berseh Food Centre
    ## 2796                     Berseh Food Centre
    ## 2797                     Berseh Food Centre
    ## 2798                     Berseh Food Centre
    ## 2799                     Berseh Food Centre
    ## 2800                     Berseh Food Centre
    ## 2801                     Berseh Food Centre
    ## 2802                     Berseh Food Centre
    ## 2803                     Berseh Food Centre
    ## 2804                     Berseh Food Centre
    ## 2805                     Berseh Food Centre
    ## 2806                     Berseh Food Centre
    ## 2807                     Berseh Food Centre
    ## 2808                     Berseh Food Centre
    ## 2809                     Berseh Food Centre
    ## 2810                     Berseh Food Centre
    ## 2811                     Berseh Food Centre
    ## 2812                     Berseh Food Centre
    ## 2813                     Berseh Food Centre
    ## 2814                     Berseh Food Centre
    ## 2815                     Berseh Food Centre
    ## 2816                     Berseh Food Centre
    ## 2817                     Berseh Food Centre
    ## 2818                     Berseh Food Centre
    ## 2819                     Berseh Food Centre
    ## 2820                     Berseh Food Centre
    ## 2821                     Berseh Food Centre
    ## 2822                     Berseh Food Centre
    ## 2823                     Berseh Food Centre
    ## 2824                     Berseh Food Centre
    ## 2825                     Berseh Food Centre
    ## 2826                     Berseh Food Centre
    ## 2827                     Berseh Food Centre
    ## 2828                     Berseh Food Centre
    ## 2829                     Berseh Food Centre
    ## 2830                     Berseh Food Centre
    ## 2831                     Berseh Food Centre
    ## 2832                     Berseh Food Centre
    ## 2833                     Berseh Food Centre
    ## 2834                     Berseh Food Centre
    ## 2835                     Berseh Food Centre
    ## 2836                     Berseh Food Centre
    ## 2837                     Berseh Food Centre
    ## 2838                     Berseh Food Centre
    ## 2839                     Berseh Food Centre
    ## 2840                     Berseh Food Centre
    ## 2841                     Berseh Food Centre
    ## 2842                     Berseh Food Centre
    ## 2843                      Blk 1 Jalan Kukoh
    ## 2844                      Blk 1 Jalan Kukoh
    ## 2845                      Blk 1 Jalan Kukoh
    ## 2846                      Blk 1 Jalan Kukoh
    ## 2847                      Blk 1 Jalan Kukoh
    ## 2848                      Blk 1 Jalan Kukoh
    ## 2849                      Blk 1 Jalan Kukoh
    ## 2850                      Blk 1 Jalan Kukoh
    ## 2851                      Blk 1 Jalan Kukoh
    ## 2852                      Blk 1 Jalan Kukoh
    ## 2853                      Blk 1 Jalan Kukoh
    ## 2854                      Blk 1 Jalan Kukoh
    ## 2855                      Blk 1 Jalan Kukoh
    ## 2856                      Blk 1 Jalan Kukoh
    ## 2857                      Blk 1 Jalan Kukoh
    ## 2858                      Blk 1 Jalan Kukoh
    ## 2859                      Blk 1 Jalan Kukoh
    ## 2860                      Blk 1 Jalan Kukoh
    ## 2861                      Blk 1 Jalan Kukoh
    ## 2862                      Blk 1 Jalan Kukoh
    ## 2863                      Blk 1 Jalan Kukoh
    ## 2864                      Blk 1 Jalan Kukoh
    ## 2865                      Blk 1 Jalan Kukoh
    ## 2866                      Blk 1 Jalan Kukoh
    ## 2867                      Blk 1 Jalan Kukoh
    ## 2868                      Blk 1 Jalan Kukoh
    ## 2869                      Blk 1 Jalan Kukoh
    ## 2870                      Blk 1 Jalan Kukoh
    ## 2871                      Blk 1 Jalan Kukoh
    ## 2872                      Blk 1 Jalan Kukoh
    ## 2873                      Blk 1 Jalan Kukoh
    ## 2874                      Blk 1 Jalan Kukoh
    ## 2875                      Blk 1 Jalan Kukoh
    ## 2876                      Blk 1 Jalan Kukoh
    ## 2877                      Blk 1 Jalan Kukoh
    ## 2878                      Blk 1 Jalan Kukoh
    ## 2879                      Blk 1 Jalan Kukoh
    ## 2880                      Blk 1 Jalan Kukoh
    ## 2881                      Blk 1 Jalan Kukoh
    ## 2882                      Blk 1 Jalan Kukoh
    ## 2883                      Blk 1 Jalan Kukoh
    ## 2884                      Blk 1 Jalan Kukoh
    ## 2885                      Blk 1 Jalan Kukoh
    ## 2886                      Blk 1 Jalan Kukoh
    ## 2887                      Blk 1 Jalan Kukoh
    ## 2888                      Blk 1 Jalan Kukoh
    ## 2889                      Blk 1 Jalan Kukoh
    ## 2890                      Blk 1 Jalan Kukoh
    ## 2891                      Blk 1 Jalan Kukoh
    ## 2892                      Blk 1 Jalan Kukoh
    ## 2893                      Blk 1 Jalan Kukoh
    ## 2894                      Blk 1 Jalan Kukoh
    ## 2895                      Blk 1 Jalan Kukoh
    ## 2896                      Blk 1 Jalan Kukoh
    ## 2897                      Blk 1 Jalan Kukoh
    ## 2898                      Blk 1 Jalan Kukoh
    ## 2899                      Blk 1 Jalan Kukoh
    ## 2900                      Blk 1 Jalan Kukoh
    ## 2901                      Blk 1 Jalan Kukoh
    ## 2902                      Blk 1 Jalan Kukoh
    ## 2903                      Blk 1 Jalan Kukoh
    ## 2904                      Blk 1 Jalan Kukoh
    ## 2905                      Blk 1 Jalan Kukoh
    ## 2906                      Blk 1 Jalan Kukoh
    ## 2907                      Blk 1 Jalan Kukoh
    ## 2908                      Blk 1 Jalan Kukoh
    ## 2909                      Blk 1 Jalan Kukoh
    ## 2910                      Blk 1 Jalan Kukoh
    ## 2911                      Blk 1 Jalan Kukoh
    ## 2912                      Blk 1 Jalan Kukoh
    ## 2913                      Blk 1 Jalan Kukoh
    ## 2914                      Blk 1 Jalan Kukoh
    ## 2915                      Blk 1 Jalan Kukoh
    ## 2916                      Blk 1 Jalan Kukoh
    ## 2917                      Blk 1 Jalan Kukoh
    ## 2918                      Blk 1 Jalan Kukoh
    ## 2919                      Blk 1 Jalan Kukoh
    ## 2920                      Blk 1 Jalan Kukoh
    ## 2921                      Blk 1 Jalan Kukoh
    ## 2922                      Blk 1 Jalan Kukoh
    ## 2923                      Blk 1 Jalan Kukoh
    ## 2924                      Blk 1 Jalan Kukoh
    ## 2925                      Blk 1 Jalan Kukoh
    ## 2926                      Blk 1 Jalan Kukoh
    ## 2927                      Blk 1 Jalan Kukoh
    ## 2928                      Blk 1 Jalan Kukoh
    ## 2929                      Blk 1 Jalan Kukoh
    ## 2930                  Blk 105 Hougang Ave 1
    ## 2931                  Blk 105 Hougang Ave 1
    ## 2932                  Blk 105 Hougang Ave 1
    ## 2933                  Blk 105 Hougang Ave 1
    ## 2934                  Blk 105 Hougang Ave 1
    ## 2935                  Blk 105 Hougang Ave 1
    ## 2936                  Blk 105 Hougang Ave 1
    ## 2937                  Blk 105 Hougang Ave 1
    ## 2938                  Blk 105 Hougang Ave 1
    ## 2939                  Blk 105 Hougang Ave 1
    ## 2940                  Blk 105 Hougang Ave 1
    ## 2941                  Blk 105 Hougang Ave 1
    ## 2942                  Blk 105 Hougang Ave 1
    ## 2943                  Blk 105 Hougang Ave 1
    ## 2944                  Blk 105 Hougang Ave 1
    ## 2945                  Blk 105 Hougang Ave 1
    ## 2946                  Blk 105 Hougang Ave 1
    ## 2947                  Blk 105 Hougang Ave 1
    ## 2948                  Blk 105 Hougang Ave 1
    ## 2949                  Blk 105 Hougang Ave 1
    ## 2950                  Blk 105 Hougang Ave 1
    ## 2951                  Blk 105 Hougang Ave 1
    ## 2952                  Blk 105 Hougang Ave 1
    ## 2953                  Blk 105 Hougang Ave 1
    ## 2954                  Blk 105 Hougang Ave 1
    ## 2955                  Blk 105 Hougang Ave 1
    ## 2956                  Blk 105 Hougang Ave 1
    ## 2957                  Blk 105 Hougang Ave 1
    ## 2958                  Blk 105 Hougang Ave 1
    ## 2959                  Blk 105 Hougang Ave 1
    ## 2960                  Blk 105 Hougang Ave 1
    ## 2961                  Blk 105 Hougang Ave 1
    ## 2962                  Blk 105 Hougang Ave 1
    ## 2963                  Blk 105 Hougang Ave 1
    ## 2964                  Blk 105 Hougang Ave 1
    ## 2965                  Blk 105 Hougang Ave 1
    ## 2966                  Blk 105 Hougang Ave 1
    ## 2967                  Blk 105 Hougang Ave 1
    ## 2968                  Blk 105 Hougang Ave 1
    ## 2969                  Blk 105 Hougang Ave 1
    ## 2970                  Blk 105 Hougang Ave 1
    ## 2971                  Blk 105 Hougang Ave 1
    ## 2972                  Blk 105 Hougang Ave 1
    ## 2973                  Blk 105 Hougang Ave 1
    ## 2974                  Blk 105 Hougang Ave 1
    ## 2975                  Blk 105 Hougang Ave 1
    ## 2976                  Blk 105 Hougang Ave 1
    ## 2977                  Blk 105 Hougang Ave 1
    ## 2978                  Blk 105 Hougang Ave 1
    ## 2979                  Blk 105 Hougang Ave 1
    ## 2980                  Blk 105 Hougang Ave 1
    ## 2981                  Blk 105 Hougang Ave 1
    ## 2982                  Blk 105 Hougang Ave 1
    ## 2983                  Blk 105 Hougang Ave 1
    ## 2984                  Blk 105 Hougang Ave 1
    ## 2985                  Blk 105 Hougang Ave 1
    ## 2986                  Blk 105 Hougang Ave 1
    ## 2987                  Blk 105 Hougang Ave 1
    ## 2988                  Blk 105 Hougang Ave 1
    ## 2989                  Blk 105 Hougang Ave 1
    ## 2990                  Blk 105 Hougang Ave 1
    ## 2991                  Blk 105 Hougang Ave 1
    ## 2992                  Blk 105 Hougang Ave 1
    ## 2993                  Blk 105 Hougang Ave 1
    ## 2994                  Blk 105 Hougang Ave 1
    ## 2995                  Blk 105 Hougang Ave 1
    ## 2996                  Blk 105 Hougang Ave 1
    ## 2997                  Blk 105 Hougang Ave 1
    ## 2998                  Blk 105 Hougang Ave 1
    ## 2999                  Blk 105 Hougang Ave 1
    ## 3000                  Blk 105 Hougang Ave 1
    ## 3001                  Blk 105 Hougang Ave 1
    ## 3002                  Blk 105 Hougang Ave 1
    ## 3003                  Blk 105 Hougang Ave 1
    ## 3004                  Blk 105 Hougang Ave 1
    ## 3005                  Blk 105 Hougang Ave 1
    ## 3006                  Blk 105 Hougang Ave 1
    ## 3007                  Blk 105 Hougang Ave 1
    ## 3008                  Blk 105 Hougang Ave 1
    ## 3009                  Blk 105 Hougang Ave 1
    ## 3010                  Blk 105 Hougang Ave 1
    ## 3011                  Blk 105 Hougang Ave 1
    ## 3012                  Blk 105 Hougang Ave 1
    ## 3013                  Blk 105 Hougang Ave 1
    ## 3014                  Blk 105 Hougang Ave 1
    ## 3015                  Blk 105 Hougang Ave 1
    ## 3016          Blk 11 Telok Blangah Crescent
    ## 3017          Blk 11 Telok Blangah Crescent
    ## 3018          Blk 11 Telok Blangah Crescent
    ## 3019          Blk 11 Telok Blangah Crescent
    ## 3020          Blk 11 Telok Blangah Crescent
    ## 3021          Blk 11 Telok Blangah Crescent
    ## 3022          Blk 11 Telok Blangah Crescent
    ## 3023          Blk 11 Telok Blangah Crescent
    ## 3024          Blk 11 Telok Blangah Crescent
    ## 3025          Blk 11 Telok Blangah Crescent
    ## 3026          Blk 11 Telok Blangah Crescent
    ## 3027          Blk 11 Telok Blangah Crescent
    ## 3028          Blk 11 Telok Blangah Crescent
    ## 3029          Blk 11 Telok Blangah Crescent
    ## 3030          Blk 11 Telok Blangah Crescent
    ## 3031          Blk 11 Telok Blangah Crescent
    ## 3032          Blk 11 Telok Blangah Crescent
    ## 3033          Blk 11 Telok Blangah Crescent
    ## 3034          Blk 11 Telok Blangah Crescent
    ## 3035          Blk 11 Telok Blangah Crescent
    ## 3036          Blk 11 Telok Blangah Crescent
    ## 3037          Blk 11 Telok Blangah Crescent
    ## 3038          Blk 11 Telok Blangah Crescent
    ## 3039          Blk 11 Telok Blangah Crescent
    ## 3040          Blk 11 Telok Blangah Crescent
    ## 3041          Blk 11 Telok Blangah Crescent
    ## 3042          Blk 11 Telok Blangah Crescent
    ## 3043          Blk 11 Telok Blangah Crescent
    ## 3044          Blk 11 Telok Blangah Crescent
    ## 3045          Blk 11 Telok Blangah Crescent
    ## 3046          Blk 11 Telok Blangah Crescent
    ## 3047          Blk 11 Telok Blangah Crescent
    ## 3048          Blk 11 Telok Blangah Crescent
    ## 3049          Blk 11 Telok Blangah Crescent
    ## 3050          Blk 11 Telok Blangah Crescent
    ## 3051          Blk 11 Telok Blangah Crescent
    ## 3052          Blk 11 Telok Blangah Crescent
    ## 3053          Blk 11 Telok Blangah Crescent
    ## 3054          Blk 11 Telok Blangah Crescent
    ## 3055          Blk 11 Telok Blangah Crescent
    ## 3056          Blk 11 Telok Blangah Crescent
    ## 3057          Blk 11 Telok Blangah Crescent
    ## 3058          Blk 11 Telok Blangah Crescent
    ## 3059          Blk 11 Telok Blangah Crescent
    ## 3060          Blk 11 Telok Blangah Crescent
    ## 3061          Blk 11 Telok Blangah Crescent
    ## 3062          Blk 11 Telok Blangah Crescent
    ## 3063          Blk 11 Telok Blangah Crescent
    ## 3064          Blk 11 Telok Blangah Crescent
    ## 3065          Blk 11 Telok Blangah Crescent
    ## 3066          Blk 11 Telok Blangah Crescent
    ## 3067          Blk 11 Telok Blangah Crescent
    ## 3068          Blk 11 Telok Blangah Crescent
    ## 3069          Blk 11 Telok Blangah Crescent
    ## 3070          Blk 11 Telok Blangah Crescent
    ## 3071          Blk 11 Telok Blangah Crescent
    ## 3072          Blk 11 Telok Blangah Crescent
    ## 3073          Blk 11 Telok Blangah Crescent
    ## 3074          Blk 11 Telok Blangah Crescent
    ## 3075          Blk 11 Telok Blangah Crescent
    ## 3076          Blk 11 Telok Blangah Crescent
    ## 3077          Blk 11 Telok Blangah Crescent
    ## 3078          Blk 11 Telok Blangah Crescent
    ## 3079          Blk 11 Telok Blangah Crescent
    ## 3080          Blk 11 Telok Blangah Crescent
    ## 3081          Blk 11 Telok Blangah Crescent
    ## 3082          Blk 11 Telok Blangah Crescent
    ## 3083          Blk 11 Telok Blangah Crescent
    ## 3084          Blk 11 Telok Blangah Crescent
    ## 3085          Blk 11 Telok Blangah Crescent
    ## 3086          Blk 11 Telok Blangah Crescent
    ## 3087          Blk 11 Telok Blangah Crescent
    ## 3088          Blk 11 Telok Blangah Crescent
    ## 3089          Blk 11 Telok Blangah Crescent
    ## 3090          Blk 11 Telok Blangah Crescent
    ## 3091          Blk 11 Telok Blangah Crescent
    ## 3092          Blk 11 Telok Blangah Crescent
    ## 3093          Blk 11 Telok Blangah Crescent
    ## 3094          Blk 11 Telok Blangah Crescent
    ## 3095          Blk 11 Telok Blangah Crescent
    ## 3096          Blk 11 Telok Blangah Crescent
    ## 3097          Blk 11 Telok Blangah Crescent
    ## 3098          Blk 11 Telok Blangah Crescent
    ## 3099          Blk 11 Telok Blangah Crescent
    ## 3100          Blk 11 Telok Blangah Crescent
    ## 3101              Blk 112 Jalan Bukit Merah
    ## 3102              Blk 112 Jalan Bukit Merah
    ## 3103              Blk 112 Jalan Bukit Merah
    ## 3104              Blk 112 Jalan Bukit Merah
    ## 3105              Blk 112 Jalan Bukit Merah
    ## 3106              Blk 112 Jalan Bukit Merah
    ## 3107              Blk 112 Jalan Bukit Merah
    ## 3108              Blk 112 Jalan Bukit Merah
    ## 3109              Blk 112 Jalan Bukit Merah
    ## 3110              Blk 112 Jalan Bukit Merah
    ## 3111              Blk 112 Jalan Bukit Merah
    ## 3112              Blk 112 Jalan Bukit Merah
    ## 3113              Blk 112 Jalan Bukit Merah
    ## 3114              Blk 112 Jalan Bukit Merah
    ## 3115              Blk 112 Jalan Bukit Merah
    ## 3116              Blk 112 Jalan Bukit Merah
    ## 3117              Blk 112 Jalan Bukit Merah
    ## 3118              Blk 112 Jalan Bukit Merah
    ## 3119              Blk 112 Jalan Bukit Merah
    ## 3120              Blk 112 Jalan Bukit Merah
    ## 3121              Blk 112 Jalan Bukit Merah
    ## 3122              Blk 112 Jalan Bukit Merah
    ## 3123              Blk 112 Jalan Bukit Merah
    ## 3124              Blk 112 Jalan Bukit Merah
    ## 3125              Blk 112 Jalan Bukit Merah
    ## 3126              Blk 112 Jalan Bukit Merah
    ## 3127              Blk 112 Jalan Bukit Merah
    ## 3128              Blk 112 Jalan Bukit Merah
    ## 3129              Blk 112 Jalan Bukit Merah
    ## 3130              Blk 112 Jalan Bukit Merah
    ## 3131              Blk 112 Jalan Bukit Merah
    ## 3132              Blk 112 Jalan Bukit Merah
    ## 3133              Blk 112 Jalan Bukit Merah
    ## 3134              Blk 112 Jalan Bukit Merah
    ## 3135              Blk 112 Jalan Bukit Merah
    ## 3136              Blk 112 Jalan Bukit Merah
    ## 3137              Blk 112 Jalan Bukit Merah
    ## 3138              Blk 112 Jalan Bukit Merah
    ## 3139              Blk 112 Jalan Bukit Merah
    ## 3140              Blk 112 Jalan Bukit Merah
    ## 3141              Blk 112 Jalan Bukit Merah
    ## 3142              Blk 112 Jalan Bukit Merah
    ## 3143              Blk 112 Jalan Bukit Merah
    ## 3144              Blk 112 Jalan Bukit Merah
    ## 3145              Blk 112 Jalan Bukit Merah
    ## 3146              Blk 112 Jalan Bukit Merah
    ## 3147              Blk 112 Jalan Bukit Merah
    ## 3148              Blk 112 Jalan Bukit Merah
    ## 3149              Blk 112 Jalan Bukit Merah
    ## 3150              Blk 112 Jalan Bukit Merah
    ## 3151              Blk 112 Jalan Bukit Merah
    ## 3152              Blk 112 Jalan Bukit Merah
    ## 3153              Blk 112 Jalan Bukit Merah
    ## 3154              Blk 112 Jalan Bukit Merah
    ## 3155              Blk 112 Jalan Bukit Merah
    ## 3156              Blk 112 Jalan Bukit Merah
    ## 3157              Blk 112 Jalan Bukit Merah
    ## 3158              Blk 112 Jalan Bukit Merah
    ## 3159              Blk 112 Jalan Bukit Merah
    ## 3160              Blk 112 Jalan Bukit Merah
    ## 3161              Blk 112 Jalan Bukit Merah
    ## 3162              Blk 112 Jalan Bukit Merah
    ## 3163              Blk 112 Jalan Bukit Merah
    ## 3164              Blk 112 Jalan Bukit Merah
    ## 3165              Blk 112 Jalan Bukit Merah
    ## 3166              Blk 112 Jalan Bukit Merah
    ## 3167              Blk 112 Jalan Bukit Merah
    ## 3168              Blk 112 Jalan Bukit Merah
    ## 3169              Blk 112 Jalan Bukit Merah
    ## 3170              Blk 112 Jalan Bukit Merah
    ## 3171              Blk 112 Jalan Bukit Merah
    ## 3172              Blk 112 Jalan Bukit Merah
    ## 3173              Blk 112 Jalan Bukit Merah
    ## 3174              Blk 112 Jalan Bukit Merah
    ## 3175              Blk 112 Jalan Bukit Merah
    ## 3176              Blk 112 Jalan Bukit Merah
    ## 3177              Blk 112 Jalan Bukit Merah
    ## 3178              Blk 112 Jalan Bukit Merah
    ## 3179              Blk 112 Jalan Bukit Merah
    ## 3180              Blk 112 Jalan Bukit Merah
    ## 3181              Blk 112 Jalan Bukit Merah
    ## 3182              Blk 112 Jalan Bukit Merah
    ## 3183              Blk 112 Jalan Bukit Merah
    ## 3184              Blk 112 Jalan Bukit Merah
    ## 3185               Blk 115 Bukit Merah View
    ## 3186               Blk 115 Bukit Merah View
    ## 3187               Blk 115 Bukit Merah View
    ## 3188               Blk 115 Bukit Merah View
    ## 3189               Blk 115 Bukit Merah View
    ## 3190               Blk 115 Bukit Merah View
    ## 3191               Blk 115 Bukit Merah View
    ## 3192               Blk 115 Bukit Merah View
    ## 3193               Blk 115 Bukit Merah View
    ## 3194               Blk 115 Bukit Merah View
    ## 3195               Blk 115 Bukit Merah View
    ## 3196               Blk 115 Bukit Merah View
    ## 3197               Blk 115 Bukit Merah View
    ## 3198               Blk 115 Bukit Merah View
    ## 3199               Blk 115 Bukit Merah View
    ## 3200               Blk 115 Bukit Merah View
    ## 3201               Blk 115 Bukit Merah View
    ## 3202               Blk 115 Bukit Merah View
    ## 3203               Blk 115 Bukit Merah View
    ## 3204               Blk 115 Bukit Merah View
    ## 3205               Blk 115 Bukit Merah View
    ## 3206               Blk 115 Bukit Merah View
    ## 3207               Blk 115 Bukit Merah View
    ## 3208               Blk 115 Bukit Merah View
    ## 3209               Blk 115 Bukit Merah View
    ## 3210               Blk 115 Bukit Merah View
    ## 3211               Blk 115 Bukit Merah View
    ## 3212               Blk 115 Bukit Merah View
    ## 3213               Blk 115 Bukit Merah View
    ## 3214               Blk 115 Bukit Merah View
    ## 3215               Blk 115 Bukit Merah View
    ## 3216               Blk 115 Bukit Merah View
    ## 3217               Blk 115 Bukit Merah View
    ## 3218               Blk 115 Bukit Merah View
    ## 3219               Blk 115 Bukit Merah View
    ## 3220               Blk 115 Bukit Merah View
    ## 3221               Blk 115 Bukit Merah View
    ## 3222               Blk 115 Bukit Merah View
    ## 3223               Blk 115 Bukit Merah View
    ## 3224               Blk 115 Bukit Merah View
    ## 3225               Blk 115 Bukit Merah View
    ## 3226               Blk 115 Bukit Merah View
    ## 3227               Blk 115 Bukit Merah View
    ## 3228               Blk 115 Bukit Merah View
    ## 3229               Blk 115 Bukit Merah View
    ## 3230               Blk 115 Bukit Merah View
    ## 3231               Blk 115 Bukit Merah View
    ## 3232               Blk 115 Bukit Merah View
    ## 3233               Blk 115 Bukit Merah View
    ## 3234               Blk 115 Bukit Merah View
    ## 3235               Blk 115 Bukit Merah View
    ## 3236               Blk 115 Bukit Merah View
    ## 3237               Blk 115 Bukit Merah View
    ## 3238               Blk 115 Bukit Merah View
    ## 3239               Blk 115 Bukit Merah View
    ## 3240               Blk 115 Bukit Merah View
    ## 3241               Blk 115 Bukit Merah View
    ## 3242               Blk 115 Bukit Merah View
    ## 3243               Blk 115 Bukit Merah View
    ## 3244               Blk 115 Bukit Merah View
    ## 3245               Blk 115 Bukit Merah View
    ## 3246               Blk 115 Bukit Merah View
    ## 3247               Blk 115 Bukit Merah View
    ## 3248               Blk 115 Bukit Merah View
    ## 3249               Blk 115 Bukit Merah View
    ## 3250               Blk 115 Bukit Merah View
    ## 3251               Blk 115 Bukit Merah View
    ## 3252               Blk 115 Bukit Merah View
    ## 3253               Blk 115 Bukit Merah View
    ## 3254               Blk 115 Bukit Merah View
    ## 3255               Blk 115 Bukit Merah View
    ## 3256               Blk 115 Bukit Merah View
    ## 3257               Blk 115 Bukit Merah View
    ## 3258               Blk 115 Bukit Merah View
    ## 3259               Blk 115 Bukit Merah View
    ## 3260               Blk 115 Bukit Merah View
    ## 3261               Blk 115 Bukit Merah View
    ## 3262               Blk 115 Bukit Merah View
    ## 3263               Blk 115 Bukit Merah View
    ## 3264               Blk 115 Bukit Merah View
    ## 3265               Blk 115 Bukit Merah View
    ## 3266               Blk 115 Bukit Merah View
    ## 3267               Blk 115 Bukit Merah View
    ## 3268                 Blk 117 Aljunied Ave 2
    ## 3269                 Blk 117 Aljunied Ave 2
    ## 3270                 Blk 117 Aljunied Ave 2
    ## 3271                 Blk 117 Aljunied Ave 2
    ## 3272                 Blk 117 Aljunied Ave 2
    ## 3273                 Blk 117 Aljunied Ave 2
    ## 3274                 Blk 117 Aljunied Ave 2
    ## 3275                 Blk 117 Aljunied Ave 2
    ## 3276                 Blk 117 Aljunied Ave 2
    ## 3277                 Blk 117 Aljunied Ave 2
    ## 3278                 Blk 117 Aljunied Ave 2
    ## 3279                 Blk 117 Aljunied Ave 2
    ## 3280                 Blk 117 Aljunied Ave 2
    ## 3281                 Blk 117 Aljunied Ave 2
    ## 3282                 Blk 117 Aljunied Ave 2
    ## 3283                 Blk 117 Aljunied Ave 2
    ## 3284                 Blk 117 Aljunied Ave 2
    ## 3285                 Blk 117 Aljunied Ave 2
    ## 3286                 Blk 117 Aljunied Ave 2
    ## 3287                 Blk 117 Aljunied Ave 2
    ## 3288                 Blk 117 Aljunied Ave 2
    ## 3289                 Blk 117 Aljunied Ave 2
    ## 3290                 Blk 117 Aljunied Ave 2
    ## 3291                 Blk 117 Aljunied Ave 2
    ## 3292                 Blk 117 Aljunied Ave 2
    ## 3293                 Blk 117 Aljunied Ave 2
    ## 3294                 Blk 117 Aljunied Ave 2
    ## 3295                 Blk 117 Aljunied Ave 2
    ## 3296                 Blk 117 Aljunied Ave 2
    ## 3297                 Blk 117 Aljunied Ave 2
    ## 3298                 Blk 117 Aljunied Ave 2
    ## 3299                 Blk 117 Aljunied Ave 2
    ## 3300                 Blk 117 Aljunied Ave 2
    ## 3301                 Blk 117 Aljunied Ave 2
    ## 3302                 Blk 117 Aljunied Ave 2
    ## 3303                 Blk 117 Aljunied Ave 2
    ## 3304                 Blk 117 Aljunied Ave 2
    ## 3305                 Blk 117 Aljunied Ave 2
    ## 3306                 Blk 117 Aljunied Ave 2
    ## 3307                 Blk 117 Aljunied Ave 2
    ## 3308                 Blk 117 Aljunied Ave 2
    ## 3309                 Blk 117 Aljunied Ave 2
    ## 3310                 Blk 117 Aljunied Ave 2
    ## 3311                 Blk 117 Aljunied Ave 2
    ## 3312                 Blk 117 Aljunied Ave 2
    ## 3313                 Blk 117 Aljunied Ave 2
    ## 3314                 Blk 117 Aljunied Ave 2
    ## 3315                 Blk 117 Aljunied Ave 2
    ## 3316                 Blk 117 Aljunied Ave 2
    ## 3317                 Blk 117 Aljunied Ave 2
    ## 3318                 Blk 117 Aljunied Ave 2
    ## 3319                 Blk 117 Aljunied Ave 2
    ## 3320                 Blk 117 Aljunied Ave 2
    ## 3321                 Blk 117 Aljunied Ave 2
    ## 3322                 Blk 117 Aljunied Ave 2
    ## 3323                 Blk 117 Aljunied Ave 2
    ## 3324                 Blk 117 Aljunied Ave 2
    ## 3325                 Blk 117 Aljunied Ave 2
    ## 3326                 Blk 117 Aljunied Ave 2
    ## 3327                 Blk 117 Aljunied Ave 2
    ## 3328                 Blk 117 Aljunied Ave 2
    ## 3329                 Blk 117 Aljunied Ave 2
    ## 3330                 Blk 117 Aljunied Ave 2
    ## 3331                 Blk 117 Aljunied Ave 2
    ## 3332                 Blk 117 Aljunied Ave 2
    ## 3333                 Blk 117 Aljunied Ave 2
    ## 3334                 Blk 117 Aljunied Ave 2
    ## 3335                 Blk 117 Aljunied Ave 2
    ## 3336                 Blk 117 Aljunied Ave 2
    ## 3337                 Blk 117 Aljunied Ave 2
    ## 3338                 Blk 117 Aljunied Ave 2
    ## 3339                 Blk 117 Aljunied Ave 2
    ## 3340                 Blk 117 Aljunied Ave 2
    ## 3341                 Blk 117 Aljunied Ave 2
    ## 3342                 Blk 117 Aljunied Ave 2
    ## 3343                 Blk 117 Aljunied Ave 2
    ## 3344                 Blk 117 Aljunied Ave 2
    ## 3345                 Blk 117 Aljunied Ave 2
    ## 3346                 Blk 117 Aljunied Ave 2
    ## 3347                 Blk 117 Aljunied Ave 2
    ## 3348                 Blk 117 Aljunied Ave 2
    ## 3349                 Blk 117 Aljunied Ave 2
    ## 3350             Blk 127 Toa Payoh Lorong 1
    ## 3351             Blk 127 Toa Payoh Lorong 1
    ## 3352             Blk 127 Toa Payoh Lorong 1
    ## 3353             Blk 127 Toa Payoh Lorong 1
    ## 3354             Blk 127 Toa Payoh Lorong 1
    ## 3355             Blk 127 Toa Payoh Lorong 1
    ## 3356             Blk 127 Toa Payoh Lorong 1
    ## 3357             Blk 127 Toa Payoh Lorong 1
    ## 3358             Blk 127 Toa Payoh Lorong 1
    ## 3359             Blk 127 Toa Payoh Lorong 1
    ## 3360             Blk 127 Toa Payoh Lorong 1
    ## 3361             Blk 127 Toa Payoh Lorong 1
    ## 3362             Blk 127 Toa Payoh Lorong 1
    ## 3363             Blk 127 Toa Payoh Lorong 1
    ## 3364             Blk 127 Toa Payoh Lorong 1
    ## 3365             Blk 127 Toa Payoh Lorong 1
    ## 3366             Blk 127 Toa Payoh Lorong 1
    ## 3367             Blk 127 Toa Payoh Lorong 1
    ## 3368             Blk 127 Toa Payoh Lorong 1
    ## 3369             Blk 127 Toa Payoh Lorong 1
    ## 3370             Blk 127 Toa Payoh Lorong 1
    ## 3371             Blk 127 Toa Payoh Lorong 1
    ## 3372             Blk 127 Toa Payoh Lorong 1
    ## 3373             Blk 127 Toa Payoh Lorong 1
    ## 3374             Blk 127 Toa Payoh Lorong 1
    ## 3375             Blk 127 Toa Payoh Lorong 1
    ## 3376             Blk 127 Toa Payoh Lorong 1
    ## 3377             Blk 127 Toa Payoh Lorong 1
    ## 3378             Blk 127 Toa Payoh Lorong 1
    ## 3379             Blk 127 Toa Payoh Lorong 1
    ## 3380             Blk 127 Toa Payoh Lorong 1
    ## 3381             Blk 127 Toa Payoh Lorong 1
    ## 3382             Blk 127 Toa Payoh Lorong 1
    ## 3383             Blk 127 Toa Payoh Lorong 1
    ## 3384             Blk 127 Toa Payoh Lorong 1
    ## 3385             Blk 127 Toa Payoh Lorong 1
    ## 3386             Blk 127 Toa Payoh Lorong 1
    ## 3387             Blk 127 Toa Payoh Lorong 1
    ## 3388             Blk 127 Toa Payoh Lorong 1
    ## 3389             Blk 127 Toa Payoh Lorong 1
    ## 3390             Blk 127 Toa Payoh Lorong 1
    ## 3391             Blk 127 Toa Payoh Lorong 1
    ## 3392             Blk 127 Toa Payoh Lorong 1
    ## 3393             Blk 127 Toa Payoh Lorong 1
    ## 3394             Blk 127 Toa Payoh Lorong 1
    ## 3395             Blk 127 Toa Payoh Lorong 1
    ## 3396             Blk 127 Toa Payoh Lorong 1
    ## 3397             Blk 127 Toa Payoh Lorong 1
    ## 3398             Blk 127 Toa Payoh Lorong 1
    ## 3399             Blk 127 Toa Payoh Lorong 1
    ## 3400             Blk 127 Toa Payoh Lorong 1
    ## 3401             Blk 127 Toa Payoh Lorong 1
    ## 3402             Blk 127 Toa Payoh Lorong 1
    ## 3403             Blk 127 Toa Payoh Lorong 1
    ## 3404             Blk 127 Toa Payoh Lorong 1
    ## 3405             Blk 127 Toa Payoh Lorong 1
    ## 3406             Blk 127 Toa Payoh Lorong 1
    ## 3407             Blk 127 Toa Payoh Lorong 1
    ## 3408             Blk 127 Toa Payoh Lorong 1
    ## 3409             Blk 127 Toa Payoh Lorong 1
    ## 3410             Blk 127 Toa Payoh Lorong 1
    ## 3411             Blk 127 Toa Payoh Lorong 1
    ## 3412             Blk 127 Toa Payoh Lorong 1
    ## 3413             Blk 127 Toa Payoh Lorong 1
    ## 3414             Blk 127 Toa Payoh Lorong 1
    ## 3415             Blk 127 Toa Payoh Lorong 1
    ## 3416             Blk 127 Toa Payoh Lorong 1
    ## 3417             Blk 127 Toa Payoh Lorong 1
    ## 3418             Blk 127 Toa Payoh Lorong 1
    ## 3419             Blk 127 Toa Payoh Lorong 1
    ## 3420             Blk 127 Toa Payoh Lorong 1
    ## 3421             Blk 127 Toa Payoh Lorong 1
    ## 3422             Blk 127 Toa Payoh Lorong 1
    ## 3423             Blk 127 Toa Payoh Lorong 1
    ## 3424             Blk 127 Toa Payoh Lorong 1
    ## 3425             Blk 127 Toa Payoh Lorong 1
    ## 3426             Blk 127 Toa Payoh Lorong 1
    ## 3427             Blk 127 Toa Payoh Lorong 1
    ## 3428             Blk 127 Toa Payoh Lorong 1
    ## 3429             Blk 127 Toa Payoh Lorong 1
    ## 3430             Blk 127 Toa Payoh Lorong 1
    ## 3431             Blk 137 Tampines Street 11
    ## 3432             Blk 137 Tampines Street 11
    ## 3433             Blk 137 Tampines Street 11
    ## 3434             Blk 137 Tampines Street 11
    ## 3435             Blk 137 Tampines Street 11
    ## 3436             Blk 137 Tampines Street 11
    ## 3437             Blk 137 Tampines Street 11
    ## 3438             Blk 137 Tampines Street 11
    ## 3439             Blk 137 Tampines Street 11
    ## 3440             Blk 137 Tampines Street 11
    ## 3441             Blk 137 Tampines Street 11
    ## 3442             Blk 137 Tampines Street 11
    ## 3443             Blk 137 Tampines Street 11
    ## 3444             Blk 137 Tampines Street 11
    ## 3445             Blk 137 Tampines Street 11
    ## 3446             Blk 137 Tampines Street 11
    ## 3447             Blk 137 Tampines Street 11
    ## 3448             Blk 137 Tampines Street 11
    ## 3449             Blk 137 Tampines Street 11
    ## 3450             Blk 137 Tampines Street 11
    ## 3451             Blk 137 Tampines Street 11
    ## 3452             Blk 137 Tampines Street 11
    ## 3453             Blk 137 Tampines Street 11
    ## 3454             Blk 137 Tampines Street 11
    ## 3455             Blk 137 Tampines Street 11
    ## 3456             Blk 137 Tampines Street 11
    ## 3457             Blk 137 Tampines Street 11
    ## 3458             Blk 137 Tampines Street 11
    ## 3459             Blk 137 Tampines Street 11
    ## 3460             Blk 137 Tampines Street 11
    ## 3461             Blk 137 Tampines Street 11
    ## 3462             Blk 137 Tampines Street 11
    ## 3463             Blk 137 Tampines Street 11
    ## 3464             Blk 137 Tampines Street 11
    ## 3465             Blk 137 Tampines Street 11
    ## 3466             Blk 137 Tampines Street 11
    ## 3467             Blk 137 Tampines Street 11
    ## 3468             Blk 137 Tampines Street 11
    ## 3469             Blk 137 Tampines Street 11
    ## 3470             Blk 137 Tampines Street 11
    ## 3471             Blk 137 Tampines Street 11
    ## 3472             Blk 137 Tampines Street 11
    ## 3473             Blk 137 Tampines Street 11
    ## 3474             Blk 137 Tampines Street 11
    ## 3475             Blk 137 Tampines Street 11
    ## 3476             Blk 137 Tampines Street 11
    ## 3477             Blk 137 Tampines Street 11
    ## 3478             Blk 137 Tampines Street 11
    ## 3479             Blk 137 Tampines Street 11
    ## 3480             Blk 137 Tampines Street 11
    ## 3481             Blk 137 Tampines Street 11
    ## 3482             Blk 137 Tampines Street 11
    ## 3483             Blk 137 Tampines Street 11
    ## 3484             Blk 137 Tampines Street 11
    ## 3485             Blk 137 Tampines Street 11
    ## 3486             Blk 137 Tampines Street 11
    ## 3487             Blk 137 Tampines Street 11
    ## 3488             Blk 137 Tampines Street 11
    ## 3489             Blk 137 Tampines Street 11
    ## 3490             Blk 137 Tampines Street 11
    ## 3491             Blk 137 Tampines Street 11
    ## 3492             Blk 137 Tampines Street 11
    ## 3493             Blk 137 Tampines Street 11
    ## 3494             Blk 137 Tampines Street 11
    ## 3495             Blk 137 Tampines Street 11
    ## 3496             Blk 137 Tampines Street 11
    ## 3497             Blk 137 Tampines Street 11
    ## 3498             Blk 137 Tampines Street 11
    ## 3499             Blk 137 Tampines Street 11
    ## 3500             Blk 137 Tampines Street 11
    ## 3501             Blk 137 Tampines Street 11
    ## 3502             Blk 137 Tampines Street 11
    ## 3503             Blk 137 Tampines Street 11
    ## 3504             Blk 137 Tampines Street 11
    ## 3505             Blk 137 Tampines Street 11
    ## 3506             Blk 137 Tampines Street 11
    ## 3507             Blk 137 Tampines Street 11
    ## 3508             Blk 137 Tampines Street 11
    ## 3509             Blk 137 Tampines Street 11
    ## 3510             Blk 137 Tampines Street 11
    ## 3511                  Blk 159 Mei Chin Road
    ## 3512                  Blk 159 Mei Chin Road
    ## 3513                  Blk 159 Mei Chin Road
    ## 3514                  Blk 159 Mei Chin Road
    ## 3515                  Blk 159 Mei Chin Road
    ## 3516                  Blk 159 Mei Chin Road
    ## 3517                  Blk 159 Mei Chin Road
    ## 3518                  Blk 159 Mei Chin Road
    ## 3519                  Blk 159 Mei Chin Road
    ## 3520                  Blk 159 Mei Chin Road
    ## 3521                  Blk 159 Mei Chin Road
    ## 3522                  Blk 159 Mei Chin Road
    ## 3523                  Blk 159 Mei Chin Road
    ## 3524                  Blk 159 Mei Chin Road
    ## 3525                  Blk 159 Mei Chin Road
    ## 3526                  Blk 159 Mei Chin Road
    ## 3527                  Blk 159 Mei Chin Road
    ## 3528                  Blk 159 Mei Chin Road
    ## 3529                  Blk 159 Mei Chin Road
    ## 3530                  Blk 159 Mei Chin Road
    ## 3531                  Blk 159 Mei Chin Road
    ## 3532                  Blk 159 Mei Chin Road
    ## 3533                  Blk 159 Mei Chin Road
    ## 3534                  Blk 159 Mei Chin Road
    ## 3535                  Blk 159 Mei Chin Road
    ## 3536                  Blk 159 Mei Chin Road
    ## 3537                  Blk 159 Mei Chin Road
    ## 3538                  Blk 159 Mei Chin Road
    ## 3539                  Blk 159 Mei Chin Road
    ## 3540                  Blk 159 Mei Chin Road
    ## 3541                  Blk 159 Mei Chin Road
    ## 3542                  Blk 159 Mei Chin Road
    ## 3543                  Blk 159 Mei Chin Road
    ## 3544                  Blk 159 Mei Chin Road
    ## 3545                  Blk 159 Mei Chin Road
    ## 3546                  Blk 159 Mei Chin Road
    ## 3547                  Blk 159 Mei Chin Road
    ## 3548                  Blk 159 Mei Chin Road
    ## 3549                  Blk 159 Mei Chin Road
    ## 3550                  Blk 159 Mei Chin Road
    ## 3551                  Blk 159 Mei Chin Road
    ## 3552                  Blk 159 Mei Chin Road
    ## 3553                  Blk 159 Mei Chin Road
    ## 3554                  Blk 159 Mei Chin Road
    ## 3555                  Blk 159 Mei Chin Road
    ## 3556                  Blk 159 Mei Chin Road
    ## 3557                  Blk 159 Mei Chin Road
    ## 3558                  Blk 159 Mei Chin Road
    ## 3559                  Blk 159 Mei Chin Road
    ## 3560                  Blk 159 Mei Chin Road
    ## 3561                  Blk 159 Mei Chin Road
    ## 3562                  Blk 159 Mei Chin Road
    ## 3563                  Blk 159 Mei Chin Road
    ## 3564                  Blk 159 Mei Chin Road
    ## 3565                  Blk 159 Mei Chin Road
    ## 3566                  Blk 159 Mei Chin Road
    ## 3567                  Blk 159 Mei Chin Road
    ## 3568                  Blk 159 Mei Chin Road
    ## 3569                  Blk 159 Mei Chin Road
    ## 3570                  Blk 159 Mei Chin Road
    ## 3571                  Blk 159 Mei Chin Road
    ## 3572                  Blk 159 Mei Chin Road
    ## 3573                  Blk 159 Mei Chin Road
    ## 3574                  Blk 159 Mei Chin Road
    ## 3575                  Blk 159 Mei Chin Road
    ## 3576                  Blk 159 Mei Chin Road
    ## 3577                  Blk 159 Mei Chin Road
    ## 3578                  Blk 159 Mei Chin Road
    ## 3579                  Blk 159 Mei Chin Road
    ## 3580                  Blk 159 Mei Chin Road
    ## 3581                  Blk 159 Mei Chin Road
    ## 3582                  Blk 159 Mei Chin Road
    ## 3583                  Blk 159 Mei Chin Road
    ## 3584                  Blk 159 Mei Chin Road
    ## 3585                  Blk 159 Mei Chin Road
    ## 3586                  Blk 159 Mei Chin Road
    ## 3587                  Blk 159 Mei Chin Road
    ## 3588                  Blk 159 Mei Chin Road
    ## 3589                  Blk 159 Mei Chin Road
    ## 3590                Blk 16 Bedok South Road
    ## 3591                Blk 16 Bedok South Road
    ## 3592                Blk 16 Bedok South Road
    ## 3593                Blk 16 Bedok South Road
    ## 3594                Blk 16 Bedok South Road
    ## 3595                Blk 16 Bedok South Road
    ## 3596                Blk 16 Bedok South Road
    ## 3597                Blk 16 Bedok South Road
    ## 3598                Blk 16 Bedok South Road
    ## 3599                Blk 16 Bedok South Road
    ## 3600                Blk 16 Bedok South Road
    ## 3601                Blk 16 Bedok South Road
    ## 3602                Blk 16 Bedok South Road
    ## 3603                Blk 16 Bedok South Road
    ## 3604                Blk 16 Bedok South Road
    ## 3605                Blk 16 Bedok South Road
    ## 3606                Blk 16 Bedok South Road
    ## 3607                Blk 16 Bedok South Road
    ## 3608                Blk 16 Bedok South Road
    ## 3609                Blk 16 Bedok South Road
    ## 3610                Blk 16 Bedok South Road
    ## 3611                Blk 16 Bedok South Road
    ## 3612                Blk 16 Bedok South Road
    ## 3613                Blk 16 Bedok South Road
    ## 3614                Blk 16 Bedok South Road
    ## 3615                Blk 16 Bedok South Road
    ## 3616                Blk 16 Bedok South Road
    ## 3617                Blk 16 Bedok South Road
    ## 3618                Blk 16 Bedok South Road
    ## 3619                Blk 16 Bedok South Road
    ## 3620                Blk 16 Bedok South Road
    ## 3621                Blk 16 Bedok South Road
    ## 3622                Blk 16 Bedok South Road
    ## 3623                Blk 16 Bedok South Road
    ## 3624                Blk 16 Bedok South Road
    ## 3625                Blk 16 Bedok South Road
    ## 3626                Blk 16 Bedok South Road
    ## 3627                Blk 16 Bedok South Road
    ## 3628                Blk 16 Bedok South Road
    ## 3629                Blk 16 Bedok South Road
    ## 3630                Blk 16 Bedok South Road
    ## 3631                Blk 16 Bedok South Road
    ## 3632                Blk 16 Bedok South Road
    ## 3633                Blk 16 Bedok South Road
    ## 3634                Blk 16 Bedok South Road
    ## 3635                Blk 16 Bedok South Road
    ## 3636                Blk 16 Bedok South Road
    ## 3637                Blk 16 Bedok South Road
    ## 3638                Blk 16 Bedok South Road
    ## 3639                Blk 16 Bedok South Road
    ## 3640                Blk 16 Bedok South Road
    ## 3641                Blk 16 Bedok South Road
    ## 3642                Blk 16 Bedok South Road
    ## 3643                Blk 16 Bedok South Road
    ## 3644                Blk 16 Bedok South Road
    ## 3645                Blk 16 Bedok South Road
    ## 3646                Blk 16 Bedok South Road
    ## 3647                Blk 16 Bedok South Road
    ## 3648                Blk 16 Bedok South Road
    ## 3649                Blk 16 Bedok South Road
    ## 3650                Blk 16 Bedok South Road
    ## 3651                Blk 16 Bedok South Road
    ## 3652                Blk 16 Bedok South Road
    ## 3653                Blk 16 Bedok South Road
    ## 3654                Blk 16 Bedok South Road
    ## 3655                Blk 16 Bedok South Road
    ## 3656                Blk 16 Bedok South Road
    ## 3657                Blk 16 Bedok South Road
    ## 3658                Blk 16 Bedok South Road
    ## 3659                Blk 16 Bedok South Road
    ## 3660                Blk 16 Bedok South Road
    ## 3661                Blk 16 Bedok South Road
    ## 3662                Blk 16 Bedok South Road
    ## 3663                Blk 16 Bedok South Road
    ## 3664                Blk 16 Bedok South Road
    ## 3665                Blk 16 Bedok South Road
    ## 3666                Blk 16 Bedok South Road
    ## 3667                Blk 16 Bedok South Road
    ## 3668            Blk 163 Bukit Merah Central
    ## 3669            Blk 163 Bukit Merah Central
    ## 3670            Blk 163 Bukit Merah Central
    ## 3671            Blk 163 Bukit Merah Central
    ## 3672            Blk 163 Bukit Merah Central
    ## 3673            Blk 163 Bukit Merah Central
    ## 3674            Blk 163 Bukit Merah Central
    ## 3675            Blk 163 Bukit Merah Central
    ## 3676            Blk 163 Bukit Merah Central
    ## 3677            Blk 163 Bukit Merah Central
    ## 3678            Blk 163 Bukit Merah Central
    ## 3679            Blk 163 Bukit Merah Central
    ## 3680            Blk 163 Bukit Merah Central
    ## 3681            Blk 163 Bukit Merah Central
    ## 3682            Blk 163 Bukit Merah Central
    ## 3683            Blk 163 Bukit Merah Central
    ## 3684            Blk 163 Bukit Merah Central
    ## 3685            Blk 163 Bukit Merah Central
    ## 3686            Blk 163 Bukit Merah Central
    ## 3687            Blk 163 Bukit Merah Central
    ## 3688            Blk 163 Bukit Merah Central
    ## 3689            Blk 163 Bukit Merah Central
    ## 3690            Blk 163 Bukit Merah Central
    ## 3691            Blk 163 Bukit Merah Central
    ## 3692            Blk 163 Bukit Merah Central
    ## 3693            Blk 163 Bukit Merah Central
    ## 3694            Blk 163 Bukit Merah Central
    ## 3695            Blk 163 Bukit Merah Central
    ## 3696            Blk 163 Bukit Merah Central
    ## 3697            Blk 163 Bukit Merah Central
    ## 3698            Blk 163 Bukit Merah Central
    ## 3699            Blk 163 Bukit Merah Central
    ## 3700            Blk 163 Bukit Merah Central
    ## 3701            Blk 163 Bukit Merah Central
    ## 3702            Blk 163 Bukit Merah Central
    ## 3703            Blk 163 Bukit Merah Central
    ## 3704            Blk 163 Bukit Merah Central
    ## 3705            Blk 163 Bukit Merah Central
    ## 3706            Blk 163 Bukit Merah Central
    ## 3707            Blk 163 Bukit Merah Central
    ## 3708            Blk 163 Bukit Merah Central
    ## 3709            Blk 163 Bukit Merah Central
    ## 3710            Blk 163 Bukit Merah Central
    ## 3711            Blk 163 Bukit Merah Central
    ## 3712            Blk 163 Bukit Merah Central
    ## 3713            Blk 163 Bukit Merah Central
    ## 3714            Blk 163 Bukit Merah Central
    ## 3715            Blk 163 Bukit Merah Central
    ## 3716            Blk 163 Bukit Merah Central
    ## 3717            Blk 163 Bukit Merah Central
    ## 3718            Blk 163 Bukit Merah Central
    ## 3719            Blk 163 Bukit Merah Central
    ## 3720            Blk 163 Bukit Merah Central
    ## 3721            Blk 163 Bukit Merah Central
    ## 3722            Blk 163 Bukit Merah Central
    ## 3723            Blk 163 Bukit Merah Central
    ## 3724            Blk 163 Bukit Merah Central
    ## 3725            Blk 163 Bukit Merah Central
    ## 3726            Blk 163 Bukit Merah Central
    ## 3727            Blk 163 Bukit Merah Central
    ## 3728            Blk 163 Bukit Merah Central
    ## 3729            Blk 163 Bukit Merah Central
    ## 3730            Blk 163 Bukit Merah Central
    ## 3731            Blk 163 Bukit Merah Central
    ## 3732            Blk 163 Bukit Merah Central
    ## 3733            Blk 163 Bukit Merah Central
    ## 3734            Blk 163 Bukit Merah Central
    ## 3735            Blk 163 Bukit Merah Central
    ## 3736            Blk 163 Bukit Merah Central
    ## 3737            Blk 163 Bukit Merah Central
    ## 3738            Blk 163 Bukit Merah Central
    ## 3739            Blk 163 Bukit Merah Central
    ## 3740            Blk 163 Bukit Merah Central
    ## 3741            Blk 163 Bukit Merah Central
    ## 3742            Blk 163 Bukit Merah Central
    ## 3743            Blk 163 Bukit Merah Central
    ## 3744            Blk 163 Bukit Merah Central
    ## 3745            Blk 17 Upper Boon Keng Road
    ## 3746            Blk 17 Upper Boon Keng Road
    ## 3747            Blk 17 Upper Boon Keng Road
    ## 3748            Blk 17 Upper Boon Keng Road
    ## 3749            Blk 17 Upper Boon Keng Road
    ## 3750            Blk 17 Upper Boon Keng Road
    ## 3751            Blk 17 Upper Boon Keng Road
    ## 3752            Blk 17 Upper Boon Keng Road
    ## 3753            Blk 17 Upper Boon Keng Road
    ## 3754            Blk 17 Upper Boon Keng Road
    ## 3755            Blk 17 Upper Boon Keng Road
    ## 3756            Blk 17 Upper Boon Keng Road
    ## 3757            Blk 17 Upper Boon Keng Road
    ## 3758            Blk 17 Upper Boon Keng Road
    ## 3759            Blk 17 Upper Boon Keng Road
    ## 3760            Blk 17 Upper Boon Keng Road
    ## 3761            Blk 17 Upper Boon Keng Road
    ## 3762            Blk 17 Upper Boon Keng Road
    ## 3763            Blk 17 Upper Boon Keng Road
    ## 3764            Blk 17 Upper Boon Keng Road
    ## 3765            Blk 17 Upper Boon Keng Road
    ## 3766            Blk 17 Upper Boon Keng Road
    ## 3767            Blk 17 Upper Boon Keng Road
    ## 3768            Blk 17 Upper Boon Keng Road
    ## 3769            Blk 17 Upper Boon Keng Road
    ## 3770            Blk 17 Upper Boon Keng Road
    ## 3771            Blk 17 Upper Boon Keng Road
    ## 3772            Blk 17 Upper Boon Keng Road
    ## 3773            Blk 17 Upper Boon Keng Road
    ## 3774            Blk 17 Upper Boon Keng Road
    ## 3775            Blk 17 Upper Boon Keng Road
    ## 3776            Blk 17 Upper Boon Keng Road
    ## 3777            Blk 17 Upper Boon Keng Road
    ## 3778            Blk 17 Upper Boon Keng Road
    ## 3779            Blk 17 Upper Boon Keng Road
    ## 3780            Blk 17 Upper Boon Keng Road
    ## 3781            Blk 17 Upper Boon Keng Road
    ## 3782            Blk 17 Upper Boon Keng Road
    ## 3783            Blk 17 Upper Boon Keng Road
    ## 3784            Blk 17 Upper Boon Keng Road
    ## 3785            Blk 17 Upper Boon Keng Road
    ## 3786            Blk 17 Upper Boon Keng Road
    ## 3787            Blk 17 Upper Boon Keng Road
    ## 3788            Blk 17 Upper Boon Keng Road
    ## 3789            Blk 17 Upper Boon Keng Road
    ## 3790            Blk 17 Upper Boon Keng Road
    ## 3791            Blk 17 Upper Boon Keng Road
    ## 3792            Blk 17 Upper Boon Keng Road
    ## 3793            Blk 17 Upper Boon Keng Road
    ## 3794            Blk 17 Upper Boon Keng Road
    ## 3795            Blk 17 Upper Boon Keng Road
    ## 3796            Blk 17 Upper Boon Keng Road
    ## 3797            Blk 17 Upper Boon Keng Road
    ## 3798            Blk 17 Upper Boon Keng Road
    ## 3799            Blk 17 Upper Boon Keng Road
    ## 3800            Blk 17 Upper Boon Keng Road
    ## 3801            Blk 17 Upper Boon Keng Road
    ## 3802            Blk 17 Upper Boon Keng Road
    ## 3803            Blk 17 Upper Boon Keng Road
    ## 3804            Blk 17 Upper Boon Keng Road
    ## 3805            Blk 17 Upper Boon Keng Road
    ## 3806            Blk 17 Upper Boon Keng Road
    ## 3807            Blk 17 Upper Boon Keng Road
    ## 3808            Blk 17 Upper Boon Keng Road
    ## 3809            Blk 17 Upper Boon Keng Road
    ## 3810            Blk 17 Upper Boon Keng Road
    ## 3811            Blk 17 Upper Boon Keng Road
    ## 3812            Blk 17 Upper Boon Keng Road
    ## 3813            Blk 17 Upper Boon Keng Road
    ## 3814            Blk 17 Upper Boon Keng Road
    ## 3815            Blk 17 Upper Boon Keng Road
    ## 3816            Blk 17 Upper Boon Keng Road
    ## 3817            Blk 17 Upper Boon Keng Road
    ## 3818            Blk 17 Upper Boon Keng Road
    ## 3819            Blk 17 Upper Boon Keng Road
    ## 3820            Blk 17 Upper Boon Keng Road
    ## 3821                   Blk 20 Ghim Moh Road
    ## 3822                   Blk 20 Ghim Moh Road
    ## 3823                   Blk 20 Ghim Moh Road
    ## 3824                   Blk 20 Ghim Moh Road
    ## 3825                   Blk 20 Ghim Moh Road
    ## 3826                   Blk 20 Ghim Moh Road
    ## 3827                   Blk 20 Ghim Moh Road
    ## 3828                   Blk 20 Ghim Moh Road
    ## 3829                   Blk 20 Ghim Moh Road
    ## 3830                   Blk 20 Ghim Moh Road
    ## 3831                   Blk 20 Ghim Moh Road
    ## 3832                   Blk 20 Ghim Moh Road
    ## 3833                   Blk 20 Ghim Moh Road
    ## 3834                   Blk 20 Ghim Moh Road
    ## 3835                   Blk 20 Ghim Moh Road
    ## 3836                   Blk 20 Ghim Moh Road
    ## 3837                   Blk 20 Ghim Moh Road
    ## 3838                   Blk 20 Ghim Moh Road
    ## 3839                   Blk 20 Ghim Moh Road
    ## 3840                   Blk 20 Ghim Moh Road
    ## 3841                   Blk 20 Ghim Moh Road
    ## 3842                   Blk 20 Ghim Moh Road
    ## 3843                   Blk 20 Ghim Moh Road
    ## 3844                   Blk 20 Ghim Moh Road
    ## 3845                   Blk 20 Ghim Moh Road
    ## 3846                   Blk 20 Ghim Moh Road
    ## 3847                   Blk 20 Ghim Moh Road
    ## 3848                   Blk 20 Ghim Moh Road
    ## 3849                   Blk 20 Ghim Moh Road
    ## 3850                   Blk 20 Ghim Moh Road
    ## 3851                   Blk 20 Ghim Moh Road
    ## 3852                   Blk 20 Ghim Moh Road
    ## 3853                   Blk 20 Ghim Moh Road
    ## 3854                   Blk 20 Ghim Moh Road
    ## 3855                   Blk 20 Ghim Moh Road
    ## 3856                   Blk 20 Ghim Moh Road
    ## 3857                   Blk 20 Ghim Moh Road
    ## 3858                   Blk 20 Ghim Moh Road
    ## 3859                   Blk 20 Ghim Moh Road
    ## 3860                   Blk 20 Ghim Moh Road
    ## 3861                   Blk 20 Ghim Moh Road
    ## 3862                   Blk 20 Ghim Moh Road
    ## 3863                   Blk 20 Ghim Moh Road
    ## 3864                   Blk 20 Ghim Moh Road
    ## 3865                   Blk 20 Ghim Moh Road
    ## 3866                   Blk 20 Ghim Moh Road
    ## 3867                   Blk 20 Ghim Moh Road
    ## 3868                   Blk 20 Ghim Moh Road
    ## 3869                   Blk 20 Ghim Moh Road
    ## 3870                   Blk 20 Ghim Moh Road
    ## 3871                   Blk 20 Ghim Moh Road
    ## 3872                   Blk 20 Ghim Moh Road
    ## 3873                   Blk 20 Ghim Moh Road
    ## 3874                   Blk 20 Ghim Moh Road
    ## 3875                   Blk 20 Ghim Moh Road
    ## 3876                   Blk 20 Ghim Moh Road
    ## 3877                   Blk 20 Ghim Moh Road
    ## 3878                   Blk 20 Ghim Moh Road
    ## 3879                   Blk 20 Ghim Moh Road
    ## 3880                   Blk 20 Ghim Moh Road
    ## 3881                   Blk 20 Ghim Moh Road
    ## 3882                   Blk 20 Ghim Moh Road
    ## 3883                   Blk 20 Ghim Moh Road
    ## 3884                   Blk 20 Ghim Moh Road
    ## 3885                   Blk 20 Ghim Moh Road
    ## 3886                   Blk 20 Ghim Moh Road
    ## 3887                   Blk 20 Ghim Moh Road
    ## 3888                   Blk 20 Ghim Moh Road
    ## 3889                   Blk 20 Ghim Moh Road
    ## 3890                   Blk 20 Ghim Moh Road
    ## 3891                   Blk 20 Ghim Moh Road
    ## 3892                   Blk 20 Ghim Moh Road
    ## 3893                   Blk 20 Ghim Moh Road
    ## 3894                   Blk 20 Ghim Moh Road
    ## 3895                   Blk 20 Ghim Moh Road
    ## 3896         Blk 208B New Upper Changi Road
    ## 3897         Blk 208B New Upper Changi Road
    ## 3898         Blk 208B New Upper Changi Road
    ## 3899         Blk 208B New Upper Changi Road
    ## 3900         Blk 208B New Upper Changi Road
    ## 3901         Blk 208B New Upper Changi Road
    ## 3902         Blk 208B New Upper Changi Road
    ## 3903         Blk 208B New Upper Changi Road
    ## 3904         Blk 208B New Upper Changi Road
    ## 3905         Blk 208B New Upper Changi Road
    ## 3906         Blk 208B New Upper Changi Road
    ## 3907         Blk 208B New Upper Changi Road
    ## 3908         Blk 208B New Upper Changi Road
    ## 3909         Blk 208B New Upper Changi Road
    ## 3910         Blk 208B New Upper Changi Road
    ## 3911         Blk 208B New Upper Changi Road
    ## 3912         Blk 208B New Upper Changi Road
    ## 3913         Blk 208B New Upper Changi Road
    ## 3914         Blk 208B New Upper Changi Road
    ## 3915         Blk 208B New Upper Changi Road
    ## 3916         Blk 208B New Upper Changi Road
    ## 3917         Blk 208B New Upper Changi Road
    ## 3918         Blk 208B New Upper Changi Road
    ## 3919         Blk 208B New Upper Changi Road
    ## 3920         Blk 208B New Upper Changi Road
    ## 3921         Blk 208B New Upper Changi Road
    ## 3922         Blk 208B New Upper Changi Road
    ## 3923         Blk 208B New Upper Changi Road
    ## 3924         Blk 208B New Upper Changi Road
    ## 3925         Blk 208B New Upper Changi Road
    ## 3926         Blk 208B New Upper Changi Road
    ## 3927         Blk 208B New Upper Changi Road
    ## 3928         Blk 208B New Upper Changi Road
    ## 3929         Blk 208B New Upper Changi Road
    ## 3930         Blk 208B New Upper Changi Road
    ## 3931         Blk 208B New Upper Changi Road
    ## 3932         Blk 208B New Upper Changi Road
    ## 3933         Blk 208B New Upper Changi Road
    ## 3934         Blk 208B New Upper Changi Road
    ## 3935         Blk 208B New Upper Changi Road
    ## 3936         Blk 208B New Upper Changi Road
    ## 3937         Blk 208B New Upper Changi Road
    ## 3938         Blk 208B New Upper Changi Road
    ## 3939         Blk 208B New Upper Changi Road
    ## 3940         Blk 208B New Upper Changi Road
    ## 3941         Blk 208B New Upper Changi Road
    ## 3942         Blk 208B New Upper Changi Road
    ## 3943         Blk 208B New Upper Changi Road
    ## 3944         Blk 208B New Upper Changi Road
    ## 3945         Blk 208B New Upper Changi Road
    ## 3946         Blk 208B New Upper Changi Road
    ## 3947         Blk 208B New Upper Changi Road
    ## 3948         Blk 208B New Upper Changi Road
    ## 3949         Blk 208B New Upper Changi Road
    ## 3950         Blk 208B New Upper Changi Road
    ## 3951         Blk 208B New Upper Changi Road
    ## 3952         Blk 208B New Upper Changi Road
    ## 3953         Blk 208B New Upper Changi Road
    ## 3954         Blk 208B New Upper Changi Road
    ## 3955         Blk 208B New Upper Changi Road
    ## 3956         Blk 208B New Upper Changi Road
    ## 3957         Blk 208B New Upper Changi Road
    ## 3958         Blk 208B New Upper Changi Road
    ## 3959         Blk 208B New Upper Changi Road
    ## 3960         Blk 208B New Upper Changi Road
    ## 3961         Blk 208B New Upper Changi Road
    ## 3962         Blk 208B New Upper Changi Road
    ## 3963         Blk 208B New Upper Changi Road
    ## 3964         Blk 208B New Upper Changi Road
    ## 3965         Blk 208B New Upper Changi Road
    ## 3966         Blk 208B New Upper Changi Road
    ## 3967         Blk 208B New Upper Changi Road
    ## 3968         Blk 208B New Upper Changi Road
    ## 3969         Blk 208B New Upper Changi Road
    ## 3970             Blk 210 Toa Payoh Lorong 8
    ## 3971             Blk 210 Toa Payoh Lorong 8
    ## 3972             Blk 210 Toa Payoh Lorong 8
    ## 3973             Blk 210 Toa Payoh Lorong 8
    ## 3974             Blk 210 Toa Payoh Lorong 8
    ## 3975             Blk 210 Toa Payoh Lorong 8
    ## 3976             Blk 210 Toa Payoh Lorong 8
    ## 3977             Blk 210 Toa Payoh Lorong 8
    ## 3978             Blk 210 Toa Payoh Lorong 8
    ## 3979             Blk 210 Toa Payoh Lorong 8
    ## 3980             Blk 210 Toa Payoh Lorong 8
    ## 3981             Blk 210 Toa Payoh Lorong 8
    ## 3982             Blk 210 Toa Payoh Lorong 8
    ## 3983             Blk 210 Toa Payoh Lorong 8
    ## 3984             Blk 210 Toa Payoh Lorong 8
    ## 3985             Blk 210 Toa Payoh Lorong 8
    ## 3986             Blk 210 Toa Payoh Lorong 8
    ## 3987             Blk 210 Toa Payoh Lorong 8
    ## 3988             Blk 210 Toa Payoh Lorong 8
    ## 3989             Blk 210 Toa Payoh Lorong 8
    ## 3990             Blk 210 Toa Payoh Lorong 8
    ## 3991             Blk 210 Toa Payoh Lorong 8
    ## 3992             Blk 210 Toa Payoh Lorong 8
    ## 3993             Blk 210 Toa Payoh Lorong 8
    ## 3994             Blk 210 Toa Payoh Lorong 8
    ## 3995             Blk 210 Toa Payoh Lorong 8
    ## 3996             Blk 210 Toa Payoh Lorong 8
    ## 3997             Blk 210 Toa Payoh Lorong 8
    ## 3998             Blk 210 Toa Payoh Lorong 8
    ## 3999             Blk 210 Toa Payoh Lorong 8
    ## 4000             Blk 210 Toa Payoh Lorong 8
    ## 4001             Blk 210 Toa Payoh Lorong 8
    ## 4002             Blk 210 Toa Payoh Lorong 8
    ## 4003             Blk 210 Toa Payoh Lorong 8
    ## 4004             Blk 210 Toa Payoh Lorong 8
    ## 4005             Blk 210 Toa Payoh Lorong 8
    ## 4006             Blk 210 Toa Payoh Lorong 8
    ## 4007             Blk 210 Toa Payoh Lorong 8
    ## 4008             Blk 210 Toa Payoh Lorong 8
    ## 4009             Blk 210 Toa Payoh Lorong 8
    ## 4010             Blk 210 Toa Payoh Lorong 8
    ## 4011             Blk 210 Toa Payoh Lorong 8
    ## 4012             Blk 210 Toa Payoh Lorong 8
    ## 4013             Blk 210 Toa Payoh Lorong 8
    ## 4014             Blk 210 Toa Payoh Lorong 8
    ## 4015             Blk 210 Toa Payoh Lorong 8
    ## 4016             Blk 210 Toa Payoh Lorong 8
    ## 4017             Blk 210 Toa Payoh Lorong 8
    ## 4018             Blk 210 Toa Payoh Lorong 8
    ## 4019             Blk 210 Toa Payoh Lorong 8
    ## 4020             Blk 210 Toa Payoh Lorong 8
    ## 4021             Blk 210 Toa Payoh Lorong 8
    ## 4022             Blk 210 Toa Payoh Lorong 8
    ## 4023             Blk 210 Toa Payoh Lorong 8
    ## 4024             Blk 210 Toa Payoh Lorong 8
    ## 4025             Blk 210 Toa Payoh Lorong 8
    ## 4026             Blk 210 Toa Payoh Lorong 8
    ## 4027             Blk 210 Toa Payoh Lorong 8
    ## 4028             Blk 210 Toa Payoh Lorong 8
    ## 4029             Blk 210 Toa Payoh Lorong 8
    ## 4030             Blk 210 Toa Payoh Lorong 8
    ## 4031             Blk 210 Toa Payoh Lorong 8
    ## 4032             Blk 210 Toa Payoh Lorong 8
    ## 4033             Blk 210 Toa Payoh Lorong 8
    ## 4034             Blk 210 Toa Payoh Lorong 8
    ## 4035             Blk 210 Toa Payoh Lorong 8
    ## 4036             Blk 210 Toa Payoh Lorong 8
    ## 4037             Blk 210 Toa Payoh Lorong 8
    ## 4038             Blk 210 Toa Payoh Lorong 8
    ## 4039             Blk 210 Toa Payoh Lorong 8
    ## 4040             Blk 210 Toa Payoh Lorong 8
    ## 4041             Blk 210 Toa Payoh Lorong 8
    ## 4042             Blk 210 Toa Payoh Lorong 8
    ## 4043           Blk 216 Bedok North Street 1
    ## 4044           Blk 216 Bedok North Street 1
    ## 4045           Blk 216 Bedok North Street 1
    ## 4046           Blk 216 Bedok North Street 1
    ## 4047           Blk 216 Bedok North Street 1
    ## 4048           Blk 216 Bedok North Street 1
    ## 4049           Blk 216 Bedok North Street 1
    ## 4050           Blk 216 Bedok North Street 1
    ## 4051           Blk 216 Bedok North Street 1
    ## 4052           Blk 216 Bedok North Street 1
    ## 4053           Blk 216 Bedok North Street 1
    ## 4054           Blk 216 Bedok North Street 1
    ## 4055           Blk 216 Bedok North Street 1
    ## 4056           Blk 216 Bedok North Street 1
    ## 4057           Blk 216 Bedok North Street 1
    ## 4058           Blk 216 Bedok North Street 1
    ## 4059           Blk 216 Bedok North Street 1
    ## 4060           Blk 216 Bedok North Street 1
    ## 4061           Blk 216 Bedok North Street 1
    ## 4062           Blk 216 Bedok North Street 1
    ## 4063           Blk 216 Bedok North Street 1
    ## 4064           Blk 216 Bedok North Street 1
    ## 4065           Blk 216 Bedok North Street 1
    ## 4066           Blk 216 Bedok North Street 1
    ## 4067           Blk 216 Bedok North Street 1
    ## 4068           Blk 216 Bedok North Street 1
    ## 4069           Blk 216 Bedok North Street 1
    ## 4070           Blk 216 Bedok North Street 1
    ## 4071           Blk 216 Bedok North Street 1
    ## 4072           Blk 216 Bedok North Street 1
    ## 4073           Blk 216 Bedok North Street 1
    ## 4074           Blk 216 Bedok North Street 1
    ## 4075           Blk 216 Bedok North Street 1
    ## 4076           Blk 216 Bedok North Street 1
    ## 4077           Blk 216 Bedok North Street 1
    ## 4078           Blk 216 Bedok North Street 1
    ## 4079           Blk 216 Bedok North Street 1
    ## 4080           Blk 216 Bedok North Street 1
    ## 4081           Blk 216 Bedok North Street 1
    ## 4082           Blk 216 Bedok North Street 1
    ## 4083           Blk 216 Bedok North Street 1
    ## 4084           Blk 216 Bedok North Street 1
    ## 4085           Blk 216 Bedok North Street 1
    ## 4086           Blk 216 Bedok North Street 1
    ## 4087           Blk 216 Bedok North Street 1
    ## 4088           Blk 216 Bedok North Street 1
    ## 4089           Blk 216 Bedok North Street 1
    ## 4090           Blk 216 Bedok North Street 1
    ## 4091           Blk 216 Bedok North Street 1
    ## 4092           Blk 216 Bedok North Street 1
    ## 4093           Blk 216 Bedok North Street 1
    ## 4094           Blk 216 Bedok North Street 1
    ## 4095           Blk 216 Bedok North Street 1
    ## 4096           Blk 216 Bedok North Street 1
    ## 4097           Blk 216 Bedok North Street 1
    ## 4098           Blk 216 Bedok North Street 1
    ## 4099           Blk 216 Bedok North Street 1
    ## 4100           Blk 216 Bedok North Street 1
    ## 4101           Blk 216 Bedok North Street 1
    ## 4102           Blk 216 Bedok North Street 1
    ## 4103           Blk 216 Bedok North Street 1
    ## 4104           Blk 216 Bedok North Street 1
    ## 4105           Blk 216 Bedok North Street 1
    ## 4106           Blk 216 Bedok North Street 1
    ## 4107           Blk 216 Bedok North Street 1
    ## 4108           Blk 216 Bedok North Street 1
    ## 4109           Blk 216 Bedok North Street 1
    ## 4110           Blk 216 Bedok North Street 1
    ## 4111           Blk 216 Bedok North Street 1
    ## 4112           Blk 216 Bedok North Street 1
    ## 4113           Blk 216 Bedok North Street 1
    ## 4114           Blk 216 Bedok North Street 1
    ## 4115              Blk 22 Toa Payoh Lorong 7
    ## 4116              Blk 22 Toa Payoh Lorong 7
    ## 4117              Blk 22 Toa Payoh Lorong 7
    ## 4118              Blk 22 Toa Payoh Lorong 7
    ## 4119              Blk 22 Toa Payoh Lorong 7
    ## 4120              Blk 22 Toa Payoh Lorong 7
    ## 4121              Blk 22 Toa Payoh Lorong 7
    ## 4122              Blk 22 Toa Payoh Lorong 7
    ## 4123              Blk 22 Toa Payoh Lorong 7
    ## 4124              Blk 22 Toa Payoh Lorong 7
    ## 4125              Blk 22 Toa Payoh Lorong 7
    ## 4126              Blk 22 Toa Payoh Lorong 7
    ## 4127              Blk 22 Toa Payoh Lorong 7
    ## 4128              Blk 22 Toa Payoh Lorong 7
    ## 4129              Blk 22 Toa Payoh Lorong 7
    ## 4130              Blk 22 Toa Payoh Lorong 7
    ## 4131              Blk 22 Toa Payoh Lorong 7
    ## 4132              Blk 22 Toa Payoh Lorong 7
    ## 4133              Blk 22 Toa Payoh Lorong 7
    ## 4134              Blk 22 Toa Payoh Lorong 7
    ## 4135              Blk 22 Toa Payoh Lorong 7
    ## 4136              Blk 22 Toa Payoh Lorong 7
    ## 4137              Blk 22 Toa Payoh Lorong 7
    ## 4138              Blk 22 Toa Payoh Lorong 7
    ## 4139              Blk 22 Toa Payoh Lorong 7
    ## 4140              Blk 22 Toa Payoh Lorong 7
    ## 4141              Blk 22 Toa Payoh Lorong 7
    ## 4142              Blk 22 Toa Payoh Lorong 7
    ## 4143              Blk 22 Toa Payoh Lorong 7
    ## 4144              Blk 22 Toa Payoh Lorong 7
    ## 4145              Blk 22 Toa Payoh Lorong 7
    ## 4146              Blk 22 Toa Payoh Lorong 7
    ## 4147              Blk 22 Toa Payoh Lorong 7
    ## 4148              Blk 22 Toa Payoh Lorong 7
    ## 4149              Blk 22 Toa Payoh Lorong 7
    ## 4150              Blk 22 Toa Payoh Lorong 7
    ## 4151              Blk 22 Toa Payoh Lorong 7
    ## 4152              Blk 22 Toa Payoh Lorong 7
    ## 4153              Blk 22 Toa Payoh Lorong 7
    ## 4154              Blk 22 Toa Payoh Lorong 7
    ## 4155              Blk 22 Toa Payoh Lorong 7
    ## 4156              Blk 22 Toa Payoh Lorong 7
    ## 4157              Blk 22 Toa Payoh Lorong 7
    ## 4158              Blk 22 Toa Payoh Lorong 7
    ## 4159              Blk 22 Toa Payoh Lorong 7
    ## 4160              Blk 22 Toa Payoh Lorong 7
    ## 4161              Blk 22 Toa Payoh Lorong 7
    ## 4162              Blk 22 Toa Payoh Lorong 7
    ## 4163              Blk 22 Toa Payoh Lorong 7
    ## 4164              Blk 22 Toa Payoh Lorong 7
    ## 4165              Blk 22 Toa Payoh Lorong 7
    ## 4166              Blk 22 Toa Payoh Lorong 7
    ## 4167              Blk 22 Toa Payoh Lorong 7
    ## 4168              Blk 22 Toa Payoh Lorong 7
    ## 4169              Blk 22 Toa Payoh Lorong 7
    ## 4170              Blk 22 Toa Payoh Lorong 7
    ## 4171              Blk 22 Toa Payoh Lorong 7
    ## 4172              Blk 22 Toa Payoh Lorong 7
    ## 4173              Blk 22 Toa Payoh Lorong 7
    ## 4174              Blk 22 Toa Payoh Lorong 7
    ## 4175              Blk 22 Toa Payoh Lorong 7
    ## 4176              Blk 22 Toa Payoh Lorong 7
    ## 4177              Blk 22 Toa Payoh Lorong 7
    ## 4178              Blk 22 Toa Payoh Lorong 7
    ## 4179              Blk 22 Toa Payoh Lorong 7
    ## 4180              Blk 22 Toa Payoh Lorong 7
    ## 4181              Blk 22 Toa Payoh Lorong 7
    ## 4182              Blk 22 Toa Payoh Lorong 7
    ## 4183              Blk 22 Toa Payoh Lorong 7
    ## 4184              Blk 22 Toa Payoh Lorong 7
    ## 4185              Blk 22 Toa Payoh Lorong 7
    ## 4186              Blk 226D Ang Mo Kio Ave 1
    ## 4187              Blk 226D Ang Mo Kio Ave 1
    ## 4188              Blk 226D Ang Mo Kio Ave 1
    ## 4189              Blk 226D Ang Mo Kio Ave 1
    ## 4190              Blk 226D Ang Mo Kio Ave 1
    ## 4191              Blk 226D Ang Mo Kio Ave 1
    ## 4192              Blk 226D Ang Mo Kio Ave 1
    ## 4193              Blk 226D Ang Mo Kio Ave 1
    ## 4194              Blk 226D Ang Mo Kio Ave 1
    ## 4195              Blk 226D Ang Mo Kio Ave 1
    ## 4196              Blk 226D Ang Mo Kio Ave 1
    ## 4197              Blk 226D Ang Mo Kio Ave 1
    ## 4198              Blk 226D Ang Mo Kio Ave 1
    ## 4199              Blk 226D Ang Mo Kio Ave 1
    ## 4200              Blk 226D Ang Mo Kio Ave 1
    ## 4201              Blk 226D Ang Mo Kio Ave 1
    ## 4202              Blk 226D Ang Mo Kio Ave 1
    ## 4203              Blk 226D Ang Mo Kio Ave 1
    ## 4204              Blk 226D Ang Mo Kio Ave 1
    ## 4205              Blk 226D Ang Mo Kio Ave 1
    ## 4206              Blk 226D Ang Mo Kio Ave 1
    ## 4207              Blk 226D Ang Mo Kio Ave 1
    ## 4208              Blk 226D Ang Mo Kio Ave 1
    ## 4209              Blk 226D Ang Mo Kio Ave 1
    ## 4210              Blk 226D Ang Mo Kio Ave 1
    ## 4211              Blk 226D Ang Mo Kio Ave 1
    ## 4212              Blk 226D Ang Mo Kio Ave 1
    ## 4213              Blk 226D Ang Mo Kio Ave 1
    ## 4214              Blk 226D Ang Mo Kio Ave 1
    ## 4215              Blk 226D Ang Mo Kio Ave 1
    ## 4216              Blk 226D Ang Mo Kio Ave 1
    ## 4217              Blk 226D Ang Mo Kio Ave 1
    ## 4218              Blk 226D Ang Mo Kio Ave 1
    ## 4219              Blk 226D Ang Mo Kio Ave 1
    ## 4220              Blk 226D Ang Mo Kio Ave 1
    ## 4221              Blk 226D Ang Mo Kio Ave 1
    ## 4222              Blk 226D Ang Mo Kio Ave 1
    ## 4223              Blk 226D Ang Mo Kio Ave 1
    ## 4224              Blk 226D Ang Mo Kio Ave 1
    ## 4225              Blk 226D Ang Mo Kio Ave 1
    ## 4226              Blk 226D Ang Mo Kio Ave 1
    ## 4227              Blk 226D Ang Mo Kio Ave 1
    ## 4228              Blk 226D Ang Mo Kio Ave 1
    ## 4229              Blk 226D Ang Mo Kio Ave 1
    ## 4230              Blk 226D Ang Mo Kio Ave 1
    ## 4231              Blk 226D Ang Mo Kio Ave 1
    ## 4232              Blk 226D Ang Mo Kio Ave 1
    ## 4233              Blk 226D Ang Mo Kio Ave 1
    ## 4234              Blk 226D Ang Mo Kio Ave 1
    ## 4235              Blk 226D Ang Mo Kio Ave 1
    ## 4236              Blk 226D Ang Mo Kio Ave 1
    ## 4237              Blk 226D Ang Mo Kio Ave 1
    ## 4238              Blk 226D Ang Mo Kio Ave 1
    ## 4239              Blk 226D Ang Mo Kio Ave 1
    ## 4240              Blk 226D Ang Mo Kio Ave 1
    ## 4241              Blk 226D Ang Mo Kio Ave 1
    ## 4242              Blk 226D Ang Mo Kio Ave 1
    ## 4243              Blk 226D Ang Mo Kio Ave 1
    ## 4244              Blk 226D Ang Mo Kio Ave 1
    ## 4245              Blk 226D Ang Mo Kio Ave 1
    ## 4246              Blk 226D Ang Mo Kio Ave 1
    ## 4247              Blk 226D Ang Mo Kio Ave 1
    ## 4248              Blk 226D Ang Mo Kio Ave 1
    ## 4249              Blk 226D Ang Mo Kio Ave 1
    ## 4250              Blk 226D Ang Mo Kio Ave 1
    ## 4251              Blk 226D Ang Mo Kio Ave 1
    ## 4252              Blk 226D Ang Mo Kio Ave 1
    ## 4253              Blk 226D Ang Mo Kio Ave 1
    ## 4254              Blk 226D Ang Mo Kio Ave 1
    ## 4255              Blk 226D Ang Mo Kio Ave 1
    ## 4256          Blk 226H Ang Mo Kio Street 22
    ## 4257          Blk 226H Ang Mo Kio Street 22
    ## 4258          Blk 226H Ang Mo Kio Street 22
    ## 4259          Blk 226H Ang Mo Kio Street 22
    ## 4260          Blk 226H Ang Mo Kio Street 22
    ## 4261          Blk 226H Ang Mo Kio Street 22
    ## 4262          Blk 226H Ang Mo Kio Street 22
    ## 4263          Blk 226H Ang Mo Kio Street 22
    ## 4264          Blk 226H Ang Mo Kio Street 22
    ## 4265          Blk 226H Ang Mo Kio Street 22
    ## 4266          Blk 226H Ang Mo Kio Street 22
    ## 4267          Blk 226H Ang Mo Kio Street 22
    ## 4268          Blk 226H Ang Mo Kio Street 22
    ## 4269          Blk 226H Ang Mo Kio Street 22
    ## 4270          Blk 226H Ang Mo Kio Street 22
    ## 4271          Blk 226H Ang Mo Kio Street 22
    ## 4272          Blk 226H Ang Mo Kio Street 22
    ## 4273          Blk 226H Ang Mo Kio Street 22
    ## 4274          Blk 226H Ang Mo Kio Street 22
    ## 4275          Blk 226H Ang Mo Kio Street 22
    ## 4276          Blk 226H Ang Mo Kio Street 22
    ## 4277          Blk 226H Ang Mo Kio Street 22
    ## 4278          Blk 226H Ang Mo Kio Street 22
    ## 4279          Blk 226H Ang Mo Kio Street 22
    ## 4280          Blk 226H Ang Mo Kio Street 22
    ## 4281          Blk 226H Ang Mo Kio Street 22
    ## 4282          Blk 226H Ang Mo Kio Street 22
    ## 4283          Blk 226H Ang Mo Kio Street 22
    ## 4284          Blk 226H Ang Mo Kio Street 22
    ## 4285          Blk 226H Ang Mo Kio Street 22
    ## 4286          Blk 226H Ang Mo Kio Street 22
    ## 4287          Blk 226H Ang Mo Kio Street 22
    ## 4288          Blk 226H Ang Mo Kio Street 22
    ## 4289          Blk 226H Ang Mo Kio Street 22
    ## 4290          Blk 226H Ang Mo Kio Street 22
    ## 4291          Blk 226H Ang Mo Kio Street 22
    ## 4292          Blk 226H Ang Mo Kio Street 22
    ## 4293          Blk 226H Ang Mo Kio Street 22
    ## 4294          Blk 226H Ang Mo Kio Street 22
    ## 4295          Blk 226H Ang Mo Kio Street 22
    ## 4296          Blk 226H Ang Mo Kio Street 22
    ## 4297          Blk 226H Ang Mo Kio Street 22
    ## 4298          Blk 226H Ang Mo Kio Street 22
    ## 4299          Blk 226H Ang Mo Kio Street 22
    ## 4300          Blk 226H Ang Mo Kio Street 22
    ## 4301          Blk 226H Ang Mo Kio Street 22
    ## 4302          Blk 226H Ang Mo Kio Street 22
    ## 4303          Blk 226H Ang Mo Kio Street 22
    ## 4304          Blk 226H Ang Mo Kio Street 22
    ## 4305          Blk 226H Ang Mo Kio Street 22
    ## 4306          Blk 226H Ang Mo Kio Street 22
    ## 4307          Blk 226H Ang Mo Kio Street 22
    ## 4308          Blk 226H Ang Mo Kio Street 22
    ## 4309          Blk 226H Ang Mo Kio Street 22
    ## 4310          Blk 226H Ang Mo Kio Street 22
    ## 4311          Blk 226H Ang Mo Kio Street 22
    ## 4312          Blk 226H Ang Mo Kio Street 22
    ## 4313          Blk 226H Ang Mo Kio Street 22
    ## 4314          Blk 226H Ang Mo Kio Street 22
    ## 4315          Blk 226H Ang Mo Kio Street 22
    ## 4316          Blk 226H Ang Mo Kio Street 22
    ## 4317          Blk 226H Ang Mo Kio Street 22
    ## 4318          Blk 226H Ang Mo Kio Street 22
    ## 4319          Blk 226H Ang Mo Kio Street 22
    ## 4320          Blk 226H Ang Mo Kio Street 22
    ## 4321          Blk 226H Ang Mo Kio Street 22
    ## 4322          Blk 226H Ang Mo Kio Street 22
    ## 4323          Blk 226H Ang Mo Kio Street 22
    ## 4324          Blk 226H Ang Mo Kio Street 22
    ## 4325          Blk 254 Jurong East Street 24
    ## 4326          Blk 254 Jurong East Street 24
    ## 4327          Blk 254 Jurong East Street 24
    ## 4328          Blk 254 Jurong East Street 24
    ## 4329          Blk 254 Jurong East Street 24
    ## 4330          Blk 254 Jurong East Street 24
    ## 4331          Blk 254 Jurong East Street 24
    ## 4332          Blk 254 Jurong East Street 24
    ## 4333          Blk 254 Jurong East Street 24
    ## 4334          Blk 254 Jurong East Street 24
    ## 4335          Blk 254 Jurong East Street 24
    ## 4336          Blk 254 Jurong East Street 24
    ## 4337          Blk 254 Jurong East Street 24
    ## 4338          Blk 254 Jurong East Street 24
    ## 4339          Blk 254 Jurong East Street 24
    ## 4340          Blk 254 Jurong East Street 24
    ## 4341          Blk 254 Jurong East Street 24
    ## 4342          Blk 254 Jurong East Street 24
    ## 4343          Blk 254 Jurong East Street 24
    ## 4344          Blk 254 Jurong East Street 24
    ## 4345          Blk 254 Jurong East Street 24
    ## 4346          Blk 254 Jurong East Street 24
    ## 4347          Blk 254 Jurong East Street 24
    ## 4348          Blk 254 Jurong East Street 24
    ## 4349          Blk 254 Jurong East Street 24
    ## 4350          Blk 254 Jurong East Street 24
    ## 4351          Blk 254 Jurong East Street 24
    ## 4352          Blk 254 Jurong East Street 24
    ## 4353          Blk 254 Jurong East Street 24
    ## 4354          Blk 254 Jurong East Street 24
    ## 4355          Blk 254 Jurong East Street 24
    ## 4356          Blk 254 Jurong East Street 24
    ## 4357          Blk 254 Jurong East Street 24
    ## 4358          Blk 254 Jurong East Street 24
    ## 4359          Blk 254 Jurong East Street 24
    ## 4360          Blk 254 Jurong East Street 24
    ## 4361          Blk 254 Jurong East Street 24
    ## 4362          Blk 254 Jurong East Street 24
    ## 4363          Blk 254 Jurong East Street 24
    ## 4364          Blk 254 Jurong East Street 24
    ## 4365          Blk 254 Jurong East Street 24
    ## 4366          Blk 254 Jurong East Street 24
    ## 4367          Blk 254 Jurong East Street 24
    ## 4368          Blk 254 Jurong East Street 24
    ## 4369          Blk 254 Jurong East Street 24
    ## 4370          Blk 254 Jurong East Street 24
    ## 4371          Blk 254 Jurong East Street 24
    ## 4372          Blk 254 Jurong East Street 24
    ## 4373          Blk 254 Jurong East Street 24
    ## 4374          Blk 254 Jurong East Street 24
    ## 4375          Blk 254 Jurong East Street 24
    ## 4376          Blk 254 Jurong East Street 24
    ## 4377          Blk 254 Jurong East Street 24
    ## 4378          Blk 254 Jurong East Street 24
    ## 4379          Blk 254 Jurong East Street 24
    ## 4380          Blk 254 Jurong East Street 24
    ## 4381          Blk 254 Jurong East Street 24
    ## 4382          Blk 254 Jurong East Street 24
    ## 4383          Blk 254 Jurong East Street 24
    ## 4384          Blk 254 Jurong East Street 24
    ## 4385          Blk 254 Jurong East Street 24
    ## 4386          Blk 254 Jurong East Street 24
    ## 4387          Blk 254 Jurong East Street 24
    ## 4388          Blk 254 Jurong East Street 24
    ## 4389          Blk 254 Jurong East Street 24
    ## 4390          Blk 254 Jurong East Street 24
    ## 4391          Blk 254 Jurong East Street 24
    ## 4392          Blk 254 Jurong East Street 24
    ## 4393                  Blk 29 Bendemeer Road
    ## 4394                  Blk 29 Bendemeer Road
    ## 4395                  Blk 29 Bendemeer Road
    ## 4396                  Blk 29 Bendemeer Road
    ## 4397                  Blk 29 Bendemeer Road
    ## 4398                  Blk 29 Bendemeer Road
    ## 4399                  Blk 29 Bendemeer Road
    ## 4400                  Blk 29 Bendemeer Road
    ## 4401                  Blk 29 Bendemeer Road
    ## 4402                  Blk 29 Bendemeer Road
    ## 4403                  Blk 29 Bendemeer Road
    ## 4404                  Blk 29 Bendemeer Road
    ## 4405                  Blk 29 Bendemeer Road
    ## 4406                  Blk 29 Bendemeer Road
    ## 4407                  Blk 29 Bendemeer Road
    ## 4408                  Blk 29 Bendemeer Road
    ## 4409                  Blk 29 Bendemeer Road
    ## 4410                  Blk 29 Bendemeer Road
    ## 4411                  Blk 29 Bendemeer Road
    ## 4412                  Blk 29 Bendemeer Road
    ## 4413                  Blk 29 Bendemeer Road
    ## 4414                  Blk 29 Bendemeer Road
    ## 4415                  Blk 29 Bendemeer Road
    ## 4416                  Blk 29 Bendemeer Road
    ## 4417                  Blk 29 Bendemeer Road
    ## 4418                  Blk 29 Bendemeer Road
    ## 4419                  Blk 29 Bendemeer Road
    ## 4420                  Blk 29 Bendemeer Road
    ## 4421                  Blk 29 Bendemeer Road
    ## 4422                  Blk 29 Bendemeer Road
    ## 4423                  Blk 29 Bendemeer Road
    ## 4424                  Blk 29 Bendemeer Road
    ## 4425                  Blk 29 Bendemeer Road
    ## 4426                  Blk 29 Bendemeer Road
    ## 4427                  Blk 29 Bendemeer Road
    ## 4428                  Blk 29 Bendemeer Road
    ## 4429                  Blk 29 Bendemeer Road
    ## 4430                  Blk 29 Bendemeer Road
    ## 4431                  Blk 29 Bendemeer Road
    ## 4432                  Blk 29 Bendemeer Road
    ## 4433                  Blk 29 Bendemeer Road
    ## 4434                  Blk 29 Bendemeer Road
    ## 4435                  Blk 29 Bendemeer Road
    ## 4436                  Blk 29 Bendemeer Road
    ## 4437                  Blk 29 Bendemeer Road
    ## 4438                  Blk 29 Bendemeer Road
    ## 4439                  Blk 29 Bendemeer Road
    ## 4440                  Blk 29 Bendemeer Road
    ## 4441                  Blk 29 Bendemeer Road
    ## 4442                  Blk 29 Bendemeer Road
    ## 4443                  Blk 29 Bendemeer Road
    ## 4444                  Blk 29 Bendemeer Road
    ## 4445                  Blk 29 Bendemeer Road
    ## 4446                  Blk 29 Bendemeer Road
    ## 4447                  Blk 29 Bendemeer Road
    ## 4448                  Blk 29 Bendemeer Road
    ## 4449                  Blk 29 Bendemeer Road
    ## 4450                  Blk 29 Bendemeer Road
    ## 4451                  Blk 29 Bendemeer Road
    ## 4452                  Blk 29 Bendemeer Road
    ## 4453                  Blk 29 Bendemeer Road
    ## 4454                  Blk 29 Bendemeer Road
    ## 4455                  Blk 29 Bendemeer Road
    ## 4456                  Blk 29 Bendemeer Road
    ## 4457                  Blk 29 Bendemeer Road
    ## 4458                  Blk 29 Bendemeer Road
    ## 4459                  Blk 29 Bendemeer Road
    ## 4460                    Blk 320 Shunfu Road
    ## 4461                    Blk 320 Shunfu Road
    ## 4462                    Blk 320 Shunfu Road
    ## 4463                    Blk 320 Shunfu Road
    ## 4464                    Blk 320 Shunfu Road
    ## 4465                    Blk 320 Shunfu Road
    ## 4466                    Blk 320 Shunfu Road
    ## 4467                    Blk 320 Shunfu Road
    ## 4468                    Blk 320 Shunfu Road
    ## 4469                    Blk 320 Shunfu Road
    ## 4470                    Blk 320 Shunfu Road
    ## 4471                    Blk 320 Shunfu Road
    ## 4472                    Blk 320 Shunfu Road
    ## 4473                    Blk 320 Shunfu Road
    ## 4474                    Blk 320 Shunfu Road
    ## 4475                    Blk 320 Shunfu Road
    ## 4476                    Blk 320 Shunfu Road
    ## 4477                    Blk 320 Shunfu Road
    ## 4478                    Blk 320 Shunfu Road
    ## 4479                    Blk 320 Shunfu Road
    ## 4480                    Blk 320 Shunfu Road
    ## 4481                    Blk 320 Shunfu Road
    ## 4482                    Blk 320 Shunfu Road
    ## 4483                    Blk 320 Shunfu Road
    ## 4484                    Blk 320 Shunfu Road
    ## 4485                    Blk 320 Shunfu Road
    ## 4486                    Blk 320 Shunfu Road
    ## 4487                    Blk 320 Shunfu Road
    ## 4488                    Blk 320 Shunfu Road
    ## 4489                    Blk 320 Shunfu Road
    ## 4490                    Blk 320 Shunfu Road
    ## 4491                    Blk 320 Shunfu Road
    ## 4492                    Blk 320 Shunfu Road
    ## 4493                    Blk 320 Shunfu Road
    ## 4494                    Blk 320 Shunfu Road
    ## 4495                    Blk 320 Shunfu Road
    ## 4496                    Blk 320 Shunfu Road
    ## 4497                    Blk 320 Shunfu Road
    ## 4498                    Blk 320 Shunfu Road
    ## 4499                    Blk 320 Shunfu Road
    ## 4500                    Blk 320 Shunfu Road
    ## 4501                    Blk 320 Shunfu Road
    ## 4502                    Blk 320 Shunfu Road
    ## 4503                    Blk 320 Shunfu Road
    ## 4504                    Blk 320 Shunfu Road
    ## 4505                    Blk 320 Shunfu Road
    ## 4506                    Blk 320 Shunfu Road
    ## 4507                    Blk 320 Shunfu Road
    ## 4508                    Blk 320 Shunfu Road
    ## 4509                    Blk 320 Shunfu Road
    ## 4510                    Blk 320 Shunfu Road
    ## 4511                    Blk 320 Shunfu Road
    ## 4512                    Blk 320 Shunfu Road
    ## 4513                    Blk 320 Shunfu Road
    ## 4514                    Blk 320 Shunfu Road
    ## 4515                    Blk 320 Shunfu Road
    ## 4516                    Blk 320 Shunfu Road
    ## 4517                    Blk 320 Shunfu Road
    ## 4518                    Blk 320 Shunfu Road
    ## 4519                    Blk 320 Shunfu Road
    ## 4520                    Blk 320 Shunfu Road
    ## 4521                    Blk 320 Shunfu Road
    ## 4522                    Blk 320 Shunfu Road
    ## 4523                    Blk 320 Shunfu Road
    ## 4524                    Blk 320 Shunfu Road
    ## 4525                    Blk 320 Shunfu Road
    ## 4526               Blk 341 Ang Mo Kio Ave 1
    ## 4527               Blk 341 Ang Mo Kio Ave 1
    ## 4528               Blk 341 Ang Mo Kio Ave 1
    ## 4529               Blk 341 Ang Mo Kio Ave 1
    ## 4530               Blk 341 Ang Mo Kio Ave 1
    ## 4531               Blk 341 Ang Mo Kio Ave 1
    ## 4532               Blk 341 Ang Mo Kio Ave 1
    ## 4533               Blk 341 Ang Mo Kio Ave 1
    ## 4534               Blk 341 Ang Mo Kio Ave 1
    ## 4535               Blk 341 Ang Mo Kio Ave 1
    ## 4536               Blk 341 Ang Mo Kio Ave 1
    ## 4537               Blk 341 Ang Mo Kio Ave 1
    ## 4538               Blk 341 Ang Mo Kio Ave 1
    ## 4539               Blk 341 Ang Mo Kio Ave 1
    ## 4540               Blk 341 Ang Mo Kio Ave 1
    ## 4541               Blk 341 Ang Mo Kio Ave 1
    ## 4542               Blk 341 Ang Mo Kio Ave 1
    ## 4543               Blk 341 Ang Mo Kio Ave 1
    ## 4544               Blk 341 Ang Mo Kio Ave 1
    ## 4545               Blk 341 Ang Mo Kio Ave 1
    ## 4546               Blk 341 Ang Mo Kio Ave 1
    ## 4547               Blk 341 Ang Mo Kio Ave 1
    ## 4548               Blk 341 Ang Mo Kio Ave 1
    ## 4549               Blk 341 Ang Mo Kio Ave 1
    ## 4550               Blk 341 Ang Mo Kio Ave 1
    ## 4551               Blk 341 Ang Mo Kio Ave 1
    ## 4552               Blk 341 Ang Mo Kio Ave 1
    ## 4553               Blk 341 Ang Mo Kio Ave 1
    ## 4554               Blk 341 Ang Mo Kio Ave 1
    ## 4555               Blk 341 Ang Mo Kio Ave 1
    ## 4556               Blk 341 Ang Mo Kio Ave 1
    ## 4557               Blk 341 Ang Mo Kio Ave 1
    ## 4558               Blk 341 Ang Mo Kio Ave 1
    ## 4559               Blk 341 Ang Mo Kio Ave 1
    ## 4560               Blk 341 Ang Mo Kio Ave 1
    ## 4561               Blk 341 Ang Mo Kio Ave 1
    ## 4562               Blk 341 Ang Mo Kio Ave 1
    ## 4563               Blk 341 Ang Mo Kio Ave 1
    ## 4564               Blk 341 Ang Mo Kio Ave 1
    ## 4565               Blk 341 Ang Mo Kio Ave 1
    ## 4566               Blk 341 Ang Mo Kio Ave 1
    ## 4567               Blk 341 Ang Mo Kio Ave 1
    ## 4568               Blk 341 Ang Mo Kio Ave 1
    ## 4569               Blk 341 Ang Mo Kio Ave 1
    ## 4570               Blk 341 Ang Mo Kio Ave 1
    ## 4571               Blk 341 Ang Mo Kio Ave 1
    ## 4572               Blk 341 Ang Mo Kio Ave 1
    ## 4573               Blk 341 Ang Mo Kio Ave 1
    ## 4574               Blk 341 Ang Mo Kio Ave 1
    ## 4575               Blk 341 Ang Mo Kio Ave 1
    ## 4576               Blk 341 Ang Mo Kio Ave 1
    ## 4577               Blk 341 Ang Mo Kio Ave 1
    ## 4578               Blk 341 Ang Mo Kio Ave 1
    ## 4579               Blk 341 Ang Mo Kio Ave 1
    ## 4580               Blk 341 Ang Mo Kio Ave 1
    ## 4581               Blk 341 Ang Mo Kio Ave 1
    ## 4582               Blk 341 Ang Mo Kio Ave 1
    ## 4583               Blk 341 Ang Mo Kio Ave 1
    ## 4584               Blk 341 Ang Mo Kio Ave 1
    ## 4585               Blk 341 Ang Mo Kio Ave 1
    ## 4586               Blk 341 Ang Mo Kio Ave 1
    ## 4587               Blk 341 Ang Mo Kio Ave 1
    ## 4588               Blk 341 Ang Mo Kio Ave 1
    ## 4589               Blk 341 Ang Mo Kio Ave 1
    ## 4590               Blk 341 Ang Mo Kio Ave 1
    ## 4591              Blk 347 Jurong East Ave 1
    ## 4592              Blk 347 Jurong East Ave 1
    ## 4593              Blk 347 Jurong East Ave 1
    ## 4594              Blk 347 Jurong East Ave 1
    ## 4595              Blk 347 Jurong East Ave 1
    ## 4596              Blk 347 Jurong East Ave 1
    ## 4597              Blk 347 Jurong East Ave 1
    ## 4598              Blk 347 Jurong East Ave 1
    ## 4599              Blk 347 Jurong East Ave 1
    ## 4600              Blk 347 Jurong East Ave 1
    ## 4601              Blk 347 Jurong East Ave 1
    ## 4602              Blk 347 Jurong East Ave 1
    ## 4603              Blk 347 Jurong East Ave 1
    ## 4604              Blk 347 Jurong East Ave 1
    ## 4605              Blk 347 Jurong East Ave 1
    ## 4606              Blk 347 Jurong East Ave 1
    ## 4607              Blk 347 Jurong East Ave 1
    ## 4608              Blk 347 Jurong East Ave 1
    ## 4609              Blk 347 Jurong East Ave 1
    ## 4610              Blk 347 Jurong East Ave 1
    ## 4611              Blk 347 Jurong East Ave 1
    ## 4612              Blk 347 Jurong East Ave 1
    ## 4613              Blk 347 Jurong East Ave 1
    ## 4614              Blk 347 Jurong East Ave 1
    ## 4615              Blk 347 Jurong East Ave 1
    ## 4616              Blk 347 Jurong East Ave 1
    ## 4617              Blk 347 Jurong East Ave 1
    ## 4618              Blk 347 Jurong East Ave 1
    ## 4619              Blk 347 Jurong East Ave 1
    ## 4620              Blk 347 Jurong East Ave 1
    ## 4621              Blk 347 Jurong East Ave 1
    ## 4622              Blk 347 Jurong East Ave 1
    ## 4623              Blk 347 Jurong East Ave 1
    ## 4624              Blk 347 Jurong East Ave 1
    ## 4625              Blk 347 Jurong East Ave 1
    ## 4626              Blk 347 Jurong East Ave 1
    ## 4627              Blk 347 Jurong East Ave 1
    ## 4628              Blk 347 Jurong East Ave 1
    ## 4629              Blk 347 Jurong East Ave 1
    ## 4630              Blk 347 Jurong East Ave 1
    ## 4631              Blk 347 Jurong East Ave 1
    ## 4632              Blk 347 Jurong East Ave 1
    ## 4633              Blk 347 Jurong East Ave 1
    ## 4634              Blk 347 Jurong East Ave 1
    ## 4635              Blk 347 Jurong East Ave 1
    ## 4636              Blk 347 Jurong East Ave 1
    ## 4637              Blk 347 Jurong East Ave 1
    ## 4638              Blk 347 Jurong East Ave 1
    ## 4639              Blk 347 Jurong East Ave 1
    ## 4640              Blk 347 Jurong East Ave 1
    ## 4641              Blk 347 Jurong East Ave 1
    ## 4642              Blk 347 Jurong East Ave 1
    ## 4643              Blk 347 Jurong East Ave 1
    ## 4644              Blk 347 Jurong East Ave 1
    ## 4645              Blk 347 Jurong East Ave 1
    ## 4646              Blk 347 Jurong East Ave 1
    ## 4647              Blk 347 Jurong East Ave 1
    ## 4648              Blk 347 Jurong East Ave 1
    ## 4649              Blk 347 Jurong East Ave 1
    ## 4650              Blk 347 Jurong East Ave 1
    ## 4651              Blk 347 Jurong East Ave 1
    ## 4652              Blk 347 Jurong East Ave 1
    ## 4653              Blk 347 Jurong East Ave 1
    ## 4654              Blk 347 Jurong East Ave 1
    ## 4655                 Blk 353 Clementi Ave 2
    ## 4656                 Blk 353 Clementi Ave 2
    ## 4657                 Blk 353 Clementi Ave 2
    ## 4658                 Blk 353 Clementi Ave 2
    ## 4659                 Blk 353 Clementi Ave 2
    ## 4660                 Blk 353 Clementi Ave 2
    ## 4661                 Blk 353 Clementi Ave 2
    ## 4662                 Blk 353 Clementi Ave 2
    ## 4663                 Blk 353 Clementi Ave 2
    ## 4664                 Blk 353 Clementi Ave 2
    ## 4665                 Blk 353 Clementi Ave 2
    ## 4666                 Blk 353 Clementi Ave 2
    ## 4667                 Blk 353 Clementi Ave 2
    ## 4668                 Blk 353 Clementi Ave 2
    ## 4669                 Blk 353 Clementi Ave 2
    ## 4670                 Blk 353 Clementi Ave 2
    ## 4671                 Blk 353 Clementi Ave 2
    ## 4672                 Blk 353 Clementi Ave 2
    ## 4673                 Blk 353 Clementi Ave 2
    ## 4674                 Blk 353 Clementi Ave 2
    ## 4675                 Blk 353 Clementi Ave 2
    ## 4676                 Blk 353 Clementi Ave 2
    ## 4677                 Blk 353 Clementi Ave 2
    ## 4678                 Blk 353 Clementi Ave 2
    ## 4679                 Blk 353 Clementi Ave 2
    ## 4680                 Blk 353 Clementi Ave 2
    ## 4681                 Blk 353 Clementi Ave 2
    ## 4682                 Blk 353 Clementi Ave 2
    ## 4683                 Blk 353 Clementi Ave 2
    ## 4684                 Blk 353 Clementi Ave 2
    ## 4685                 Blk 353 Clementi Ave 2
    ## 4686                 Blk 353 Clementi Ave 2
    ## 4687                 Blk 353 Clementi Ave 2
    ## 4688                 Blk 353 Clementi Ave 2
    ## 4689                 Blk 353 Clementi Ave 2
    ## 4690                 Blk 353 Clementi Ave 2
    ## 4691                 Blk 353 Clementi Ave 2
    ## 4692                 Blk 353 Clementi Ave 2
    ## 4693                 Blk 353 Clementi Ave 2
    ## 4694                 Blk 353 Clementi Ave 2
    ## 4695                 Blk 353 Clementi Ave 2
    ## 4696                 Blk 353 Clementi Ave 2
    ## 4697                 Blk 353 Clementi Ave 2
    ## 4698                 Blk 353 Clementi Ave 2
    ## 4699                 Blk 353 Clementi Ave 2
    ## 4700                 Blk 353 Clementi Ave 2
    ## 4701                 Blk 353 Clementi Ave 2
    ## 4702                 Blk 353 Clementi Ave 2
    ## 4703                 Blk 353 Clementi Ave 2
    ## 4704                 Blk 353 Clementi Ave 2
    ## 4705                 Blk 353 Clementi Ave 2
    ## 4706                 Blk 353 Clementi Ave 2
    ## 4707                 Blk 353 Clementi Ave 2
    ## 4708                 Blk 353 Clementi Ave 2
    ## 4709                 Blk 353 Clementi Ave 2
    ## 4710                 Blk 353 Clementi Ave 2
    ## 4711                 Blk 353 Clementi Ave 2
    ## 4712                 Blk 353 Clementi Ave 2
    ## 4713                 Blk 353 Clementi Ave 2
    ## 4714                 Blk 353 Clementi Ave 2
    ## 4715                 Blk 353 Clementi Ave 2
    ## 4716                 Blk 353 Clementi Ave 2
    ## 4717                 Blk 353 Clementi Ave 2
    ## 4718              Blk 36 Telok Blangah Rise
    ## 4719              Blk 36 Telok Blangah Rise
    ## 4720              Blk 36 Telok Blangah Rise
    ## 4721              Blk 36 Telok Blangah Rise
    ## 4722              Blk 36 Telok Blangah Rise
    ## 4723              Blk 36 Telok Blangah Rise
    ## 4724              Blk 36 Telok Blangah Rise
    ## 4725              Blk 36 Telok Blangah Rise
    ## 4726              Blk 36 Telok Blangah Rise
    ## 4727              Blk 36 Telok Blangah Rise
    ## 4728              Blk 36 Telok Blangah Rise
    ## 4729              Blk 36 Telok Blangah Rise
    ## 4730              Blk 36 Telok Blangah Rise
    ## 4731              Blk 36 Telok Blangah Rise
    ## 4732              Blk 36 Telok Blangah Rise
    ## 4733              Blk 36 Telok Blangah Rise
    ## 4734              Blk 36 Telok Blangah Rise
    ## 4735              Blk 36 Telok Blangah Rise
    ## 4736              Blk 36 Telok Blangah Rise
    ## 4737              Blk 36 Telok Blangah Rise
    ## 4738              Blk 36 Telok Blangah Rise
    ## 4739              Blk 36 Telok Blangah Rise
    ## 4740              Blk 36 Telok Blangah Rise
    ## 4741              Blk 36 Telok Blangah Rise
    ## 4742              Blk 36 Telok Blangah Rise
    ## 4743              Blk 36 Telok Blangah Rise
    ## 4744              Blk 36 Telok Blangah Rise
    ## 4745              Blk 36 Telok Blangah Rise
    ## 4746              Blk 36 Telok Blangah Rise
    ## 4747              Blk 36 Telok Blangah Rise
    ## 4748              Blk 36 Telok Blangah Rise
    ## 4749              Blk 36 Telok Blangah Rise
    ## 4750              Blk 36 Telok Blangah Rise
    ## 4751              Blk 36 Telok Blangah Rise
    ## 4752              Blk 36 Telok Blangah Rise
    ## 4753              Blk 36 Telok Blangah Rise
    ## 4754              Blk 36 Telok Blangah Rise
    ## 4755              Blk 36 Telok Blangah Rise
    ## 4756              Blk 36 Telok Blangah Rise
    ## 4757              Blk 36 Telok Blangah Rise
    ## 4758              Blk 36 Telok Blangah Rise
    ## 4759              Blk 36 Telok Blangah Rise
    ## 4760              Blk 36 Telok Blangah Rise
    ## 4761              Blk 36 Telok Blangah Rise
    ## 4762              Blk 36 Telok Blangah Rise
    ## 4763              Blk 36 Telok Blangah Rise
    ## 4764              Blk 36 Telok Blangah Rise
    ## 4765              Blk 36 Telok Blangah Rise
    ## 4766              Blk 36 Telok Blangah Rise
    ## 4767              Blk 36 Telok Blangah Rise
    ## 4768              Blk 36 Telok Blangah Rise
    ## 4769              Blk 36 Telok Blangah Rise
    ## 4770              Blk 36 Telok Blangah Rise
    ## 4771              Blk 36 Telok Blangah Rise
    ## 4772              Blk 36 Telok Blangah Rise
    ## 4773              Blk 36 Telok Blangah Rise
    ## 4774              Blk 36 Telok Blangah Rise
    ## 4775              Blk 36 Telok Blangah Rise
    ## 4776              Blk 36 Telok Blangah Rise
    ## 4777              Blk 36 Telok Blangah Rise
    ## 4778              Blk 36 Telok Blangah Rise
    ## 4779              Blk 36 Telok Blangah Rise
    ## 4780             Blk 37A Teban Gardens Road
    ## 4781             Blk 37A Teban Gardens Road
    ## 4782             Blk 37A Teban Gardens Road
    ## 4783             Blk 37A Teban Gardens Road
    ## 4784             Blk 37A Teban Gardens Road
    ## 4785             Blk 37A Teban Gardens Road
    ## 4786             Blk 37A Teban Gardens Road
    ## 4787             Blk 37A Teban Gardens Road
    ## 4788             Blk 37A Teban Gardens Road
    ## 4789             Blk 37A Teban Gardens Road
    ## 4790             Blk 37A Teban Gardens Road
    ## 4791             Blk 37A Teban Gardens Road
    ## 4792             Blk 37A Teban Gardens Road
    ## 4793             Blk 37A Teban Gardens Road
    ## 4794             Blk 37A Teban Gardens Road
    ## 4795             Blk 37A Teban Gardens Road
    ## 4796             Blk 37A Teban Gardens Road
    ## 4797             Blk 37A Teban Gardens Road
    ## 4798             Blk 37A Teban Gardens Road
    ## 4799             Blk 37A Teban Gardens Road
    ## 4800             Blk 37A Teban Gardens Road
    ## 4801             Blk 37A Teban Gardens Road
    ## 4802             Blk 37A Teban Gardens Road
    ## 4803             Blk 37A Teban Gardens Road
    ## 4804             Blk 37A Teban Gardens Road
    ## 4805             Blk 37A Teban Gardens Road
    ## 4806             Blk 37A Teban Gardens Road
    ## 4807             Blk 37A Teban Gardens Road
    ## 4808             Blk 37A Teban Gardens Road
    ## 4809             Blk 37A Teban Gardens Road
    ## 4810             Blk 37A Teban Gardens Road
    ## 4811             Blk 37A Teban Gardens Road
    ## 4812             Blk 37A Teban Gardens Road
    ## 4813             Blk 37A Teban Gardens Road
    ## 4814             Blk 37A Teban Gardens Road
    ## 4815             Blk 37A Teban Gardens Road
    ## 4816             Blk 37A Teban Gardens Road
    ## 4817             Blk 37A Teban Gardens Road
    ## 4818             Blk 37A Teban Gardens Road
    ## 4819             Blk 37A Teban Gardens Road
    ## 4820             Blk 37A Teban Gardens Road
    ## 4821             Blk 37A Teban Gardens Road
    ## 4822             Blk 37A Teban Gardens Road
    ## 4823             Blk 37A Teban Gardens Road
    ## 4824             Blk 37A Teban Gardens Road
    ## 4825             Blk 37A Teban Gardens Road
    ## 4826             Blk 37A Teban Gardens Road
    ## 4827             Blk 37A Teban Gardens Road
    ## 4828             Blk 37A Teban Gardens Road
    ## 4829             Blk 37A Teban Gardens Road
    ## 4830             Blk 37A Teban Gardens Road
    ## 4831             Blk 37A Teban Gardens Road
    ## 4832             Blk 37A Teban Gardens Road
    ## 4833             Blk 37A Teban Gardens Road
    ## 4834             Blk 37A Teban Gardens Road
    ## 4835             Blk 37A Teban Gardens Road
    ## 4836             Blk 37A Teban Gardens Road
    ## 4837             Blk 37A Teban Gardens Road
    ## 4838             Blk 37A Teban Gardens Road
    ## 4839             Blk 37A Teban Gardens Road
    ## 4840             Blk 37A Teban Gardens Road
    ## 4841              Blk 409 Ang Mo Kio Ave 10
    ## 4842              Blk 409 Ang Mo Kio Ave 10
    ## 4843              Blk 409 Ang Mo Kio Ave 10
    ## 4844              Blk 409 Ang Mo Kio Ave 10
    ## 4845              Blk 409 Ang Mo Kio Ave 10
    ## 4846              Blk 409 Ang Mo Kio Ave 10
    ## 4847              Blk 409 Ang Mo Kio Ave 10
    ## 4848              Blk 409 Ang Mo Kio Ave 10
    ## 4849              Blk 409 Ang Mo Kio Ave 10
    ## 4850              Blk 409 Ang Mo Kio Ave 10
    ## 4851              Blk 409 Ang Mo Kio Ave 10
    ## 4852              Blk 409 Ang Mo Kio Ave 10
    ## 4853              Blk 409 Ang Mo Kio Ave 10
    ## 4854              Blk 409 Ang Mo Kio Ave 10
    ## 4855              Blk 409 Ang Mo Kio Ave 10
    ## 4856              Blk 409 Ang Mo Kio Ave 10
    ## 4857              Blk 409 Ang Mo Kio Ave 10
    ## 4858              Blk 409 Ang Mo Kio Ave 10
    ## 4859              Blk 409 Ang Mo Kio Ave 10
    ## 4860              Blk 409 Ang Mo Kio Ave 10
    ## 4861              Blk 409 Ang Mo Kio Ave 10
    ## 4862              Blk 409 Ang Mo Kio Ave 10
    ## 4863              Blk 409 Ang Mo Kio Ave 10
    ## 4864              Blk 409 Ang Mo Kio Ave 10
    ## 4865              Blk 409 Ang Mo Kio Ave 10
    ## 4866              Blk 409 Ang Mo Kio Ave 10
    ## 4867              Blk 409 Ang Mo Kio Ave 10
    ## 4868              Blk 409 Ang Mo Kio Ave 10
    ## 4869              Blk 409 Ang Mo Kio Ave 10
    ## 4870              Blk 409 Ang Mo Kio Ave 10
    ## 4871              Blk 409 Ang Mo Kio Ave 10
    ## 4872              Blk 409 Ang Mo Kio Ave 10
    ## 4873              Blk 409 Ang Mo Kio Ave 10
    ## 4874              Blk 409 Ang Mo Kio Ave 10
    ## 4875              Blk 409 Ang Mo Kio Ave 10
    ## 4876              Blk 409 Ang Mo Kio Ave 10
    ## 4877              Blk 409 Ang Mo Kio Ave 10
    ## 4878              Blk 409 Ang Mo Kio Ave 10
    ## 4879              Blk 409 Ang Mo Kio Ave 10
    ## 4880              Blk 409 Ang Mo Kio Ave 10
    ## 4881              Blk 409 Ang Mo Kio Ave 10
    ## 4882              Blk 409 Ang Mo Kio Ave 10
    ## 4883              Blk 409 Ang Mo Kio Ave 10
    ## 4884              Blk 409 Ang Mo Kio Ave 10
    ## 4885              Blk 409 Ang Mo Kio Ave 10
    ## 4886              Blk 409 Ang Mo Kio Ave 10
    ## 4887              Blk 409 Ang Mo Kio Ave 10
    ## 4888              Blk 409 Ang Mo Kio Ave 10
    ## 4889              Blk 409 Ang Mo Kio Ave 10
    ## 4890              Blk 409 Ang Mo Kio Ave 10
    ## 4891              Blk 409 Ang Mo Kio Ave 10
    ## 4892              Blk 409 Ang Mo Kio Ave 10
    ## 4893              Blk 409 Ang Mo Kio Ave 10
    ## 4894              Blk 409 Ang Mo Kio Ave 10
    ## 4895              Blk 409 Ang Mo Kio Ave 10
    ## 4896              Blk 409 Ang Mo Kio Ave 10
    ## 4897              Blk 409 Ang Mo Kio Ave 10
    ## 4898              Blk 409 Ang Mo Kio Ave 10
    ## 4899              Blk 409 Ang Mo Kio Ave 10
    ## 4900              Blk 409 Ang Mo Kio Ave 10
    ## 4901                   Blk 44 Holland Drive
    ## 4902                   Blk 44 Holland Drive
    ## 4903                   Blk 44 Holland Drive
    ## 4904                   Blk 44 Holland Drive
    ## 4905                   Blk 44 Holland Drive
    ## 4906                   Blk 44 Holland Drive
    ## 4907                   Blk 44 Holland Drive
    ## 4908                   Blk 44 Holland Drive
    ## 4909                   Blk 44 Holland Drive
    ## 4910                   Blk 44 Holland Drive
    ## 4911                   Blk 44 Holland Drive
    ## 4912                   Blk 44 Holland Drive
    ## 4913                   Blk 44 Holland Drive
    ## 4914                   Blk 44 Holland Drive
    ## 4915                   Blk 44 Holland Drive
    ## 4916                   Blk 44 Holland Drive
    ## 4917                   Blk 44 Holland Drive
    ## 4918                   Blk 44 Holland Drive
    ## 4919                   Blk 44 Holland Drive
    ## 4920                   Blk 44 Holland Drive
    ## 4921                   Blk 44 Holland Drive
    ## 4922                   Blk 44 Holland Drive
    ## 4923                   Blk 44 Holland Drive
    ## 4924                   Blk 44 Holland Drive
    ## 4925                   Blk 44 Holland Drive
    ## 4926                   Blk 44 Holland Drive
    ## 4927                   Blk 44 Holland Drive
    ## 4928                   Blk 44 Holland Drive
    ## 4929                   Blk 44 Holland Drive
    ## 4930                   Blk 44 Holland Drive
    ## 4931                   Blk 44 Holland Drive
    ## 4932                   Blk 44 Holland Drive
    ## 4933                   Blk 44 Holland Drive
    ## 4934                   Blk 44 Holland Drive
    ## 4935                   Blk 44 Holland Drive
    ## 4936                   Blk 44 Holland Drive
    ## 4937                   Blk 44 Holland Drive
    ## 4938                   Blk 44 Holland Drive
    ## 4939                   Blk 44 Holland Drive
    ## 4940                   Blk 44 Holland Drive
    ## 4941                   Blk 44 Holland Drive
    ## 4942                   Blk 44 Holland Drive
    ## 4943                   Blk 44 Holland Drive
    ## 4944                   Blk 44 Holland Drive
    ## 4945                   Blk 44 Holland Drive
    ## 4946                   Blk 44 Holland Drive
    ## 4947                   Blk 44 Holland Drive
    ## 4948                   Blk 44 Holland Drive
    ## 4949                   Blk 44 Holland Drive
    ## 4950                   Blk 44 Holland Drive
    ## 4951                   Blk 44 Holland Drive
    ## 4952                   Blk 44 Holland Drive
    ## 4953                   Blk 44 Holland Drive
    ## 4954                   Blk 44 Holland Drive
    ## 4955                   Blk 44 Holland Drive
    ## 4956                   Blk 44 Holland Drive
    ## 4957                   Blk 44 Holland Drive
    ## 4958                   Blk 44 Holland Drive
    ## 4959                   Blk 44 Holland Drive
    ## 4960                 Blk 448 Clementi Ave 3
    ## 4961                 Blk 448 Clementi Ave 3
    ## 4962                 Blk 448 Clementi Ave 3
    ## 4963                 Blk 448 Clementi Ave 3
    ## 4964                 Blk 448 Clementi Ave 3
    ## 4965                 Blk 448 Clementi Ave 3
    ## 4966                 Blk 448 Clementi Ave 3
    ## 4967                 Blk 448 Clementi Ave 3
    ## 4968                 Blk 448 Clementi Ave 3
    ## 4969                 Blk 448 Clementi Ave 3
    ## 4970                 Blk 448 Clementi Ave 3
    ## 4971                 Blk 448 Clementi Ave 3
    ## 4972                 Blk 448 Clementi Ave 3
    ## 4973                 Blk 448 Clementi Ave 3
    ## 4974                 Blk 448 Clementi Ave 3
    ## 4975                 Blk 448 Clementi Ave 3
    ## 4976                 Blk 448 Clementi Ave 3
    ## 4977                 Blk 448 Clementi Ave 3
    ## 4978                 Blk 448 Clementi Ave 3
    ## 4979                 Blk 448 Clementi Ave 3
    ## 4980                 Blk 448 Clementi Ave 3
    ## 4981                 Blk 448 Clementi Ave 3
    ## 4982                 Blk 448 Clementi Ave 3
    ## 4983                 Blk 448 Clementi Ave 3
    ## 4984                 Blk 448 Clementi Ave 3
    ## 4985                 Blk 448 Clementi Ave 3
    ## 4986                 Blk 448 Clementi Ave 3
    ## 4987                 Blk 448 Clementi Ave 3
    ## 4988                 Blk 448 Clementi Ave 3
    ## 4989                 Blk 448 Clementi Ave 3
    ## 4990                 Blk 448 Clementi Ave 3
    ## 4991                 Blk 448 Clementi Ave 3
    ## 4992                 Blk 448 Clementi Ave 3
    ## 4993                 Blk 448 Clementi Ave 3
    ## 4994                 Blk 448 Clementi Ave 3
    ## 4995                 Blk 448 Clementi Ave 3
    ## 4996                 Blk 448 Clementi Ave 3
    ## 4997                 Blk 448 Clementi Ave 3
    ## 4998                 Blk 448 Clementi Ave 3
    ## 4999                 Blk 448 Clementi Ave 3
    ## 5000                 Blk 448 Clementi Ave 3
    ## 5001                 Blk 448 Clementi Ave 3
    ## 5002                 Blk 448 Clementi Ave 3
    ## 5003                 Blk 448 Clementi Ave 3
    ## 5004                 Blk 448 Clementi Ave 3
    ## 5005                 Blk 448 Clementi Ave 3
    ## 5006                 Blk 448 Clementi Ave 3
    ## 5007                 Blk 448 Clementi Ave 3
    ## 5008                 Blk 448 Clementi Ave 3
    ## 5009                 Blk 448 Clementi Ave 3
    ## 5010                 Blk 448 Clementi Ave 3
    ## 5011                 Blk 448 Clementi Ave 3
    ## 5012                 Blk 448 Clementi Ave 3
    ## 5013                 Blk 448 Clementi Ave 3
    ## 5014                 Blk 448 Clementi Ave 3
    ## 5015                 Blk 448 Clementi Ave 3
    ## 5016                 Blk 448 Clementi Ave 3
    ## 5017                 Blk 448 Clementi Ave 3
    ## 5018             Blk 453A Ang Mo Kio Ave 10
    ## 5019             Blk 453A Ang Mo Kio Ave 10
    ## 5020             Blk 453A Ang Mo Kio Ave 10
    ## 5021             Blk 453A Ang Mo Kio Ave 10
    ## 5022             Blk 453A Ang Mo Kio Ave 10
    ## 5023             Blk 453A Ang Mo Kio Ave 10
    ## 5024             Blk 453A Ang Mo Kio Ave 10
    ## 5025             Blk 453A Ang Mo Kio Ave 10
    ## 5026             Blk 453A Ang Mo Kio Ave 10
    ## 5027             Blk 453A Ang Mo Kio Ave 10
    ## 5028             Blk 453A Ang Mo Kio Ave 10
    ## 5029             Blk 453A Ang Mo Kio Ave 10
    ## 5030             Blk 453A Ang Mo Kio Ave 10
    ## 5031             Blk 453A Ang Mo Kio Ave 10
    ## 5032             Blk 453A Ang Mo Kio Ave 10
    ## 5033             Blk 453A Ang Mo Kio Ave 10
    ## 5034             Blk 453A Ang Mo Kio Ave 10
    ## 5035             Blk 453A Ang Mo Kio Ave 10
    ## 5036             Blk 453A Ang Mo Kio Ave 10
    ## 5037             Blk 453A Ang Mo Kio Ave 10
    ## 5038             Blk 453A Ang Mo Kio Ave 10
    ## 5039             Blk 453A Ang Mo Kio Ave 10
    ## 5040             Blk 453A Ang Mo Kio Ave 10
    ## 5041             Blk 453A Ang Mo Kio Ave 10
    ## 5042             Blk 453A Ang Mo Kio Ave 10
    ## 5043             Blk 453A Ang Mo Kio Ave 10
    ## 5044             Blk 453A Ang Mo Kio Ave 10
    ## 5045             Blk 453A Ang Mo Kio Ave 10
    ## 5046             Blk 453A Ang Mo Kio Ave 10
    ## 5047             Blk 453A Ang Mo Kio Ave 10
    ## 5048             Blk 453A Ang Mo Kio Ave 10
    ## 5049             Blk 453A Ang Mo Kio Ave 10
    ## 5050             Blk 453A Ang Mo Kio Ave 10
    ## 5051             Blk 453A Ang Mo Kio Ave 10
    ## 5052             Blk 453A Ang Mo Kio Ave 10
    ## 5053             Blk 453A Ang Mo Kio Ave 10
    ## 5054             Blk 453A Ang Mo Kio Ave 10
    ## 5055             Blk 453A Ang Mo Kio Ave 10
    ## 5056             Blk 453A Ang Mo Kio Ave 10
    ## 5057             Blk 453A Ang Mo Kio Ave 10
    ## 5058             Blk 453A Ang Mo Kio Ave 10
    ## 5059             Blk 453A Ang Mo Kio Ave 10
    ## 5060             Blk 453A Ang Mo Kio Ave 10
    ## 5061             Blk 453A Ang Mo Kio Ave 10
    ## 5062             Blk 453A Ang Mo Kio Ave 10
    ## 5063             Blk 453A Ang Mo Kio Ave 10
    ## 5064             Blk 453A Ang Mo Kio Ave 10
    ## 5065             Blk 453A Ang Mo Kio Ave 10
    ## 5066             Blk 453A Ang Mo Kio Ave 10
    ## 5067             Blk 453A Ang Mo Kio Ave 10
    ## 5068             Blk 453A Ang Mo Kio Ave 10
    ## 5069             Blk 453A Ang Mo Kio Ave 10
    ## 5070             Blk 453A Ang Mo Kio Ave 10
    ## 5071             Blk 453A Ang Mo Kio Ave 10
    ## 5072             Blk 453A Ang Mo Kio Ave 10
    ## 5073             Blk 453A Ang Mo Kio Ave 10
    ## 5074             Blk 453A Ang Mo Kio Ave 10
    ## 5075                      Blk 49 Sims Place
    ## 5076                      Blk 49 Sims Place
    ## 5077                      Blk 49 Sims Place
    ## 5078                      Blk 49 Sims Place
    ## 5079                      Blk 49 Sims Place
    ## 5080                      Blk 49 Sims Place
    ## 5081                      Blk 49 Sims Place
    ## 5082                      Blk 49 Sims Place
    ## 5083                      Blk 49 Sims Place
    ## 5084                      Blk 49 Sims Place
    ## 5085                      Blk 49 Sims Place
    ## 5086                      Blk 49 Sims Place
    ## 5087                      Blk 49 Sims Place
    ## 5088                      Blk 49 Sims Place
    ## 5089                      Blk 49 Sims Place
    ## 5090                      Blk 49 Sims Place
    ## 5091                      Blk 49 Sims Place
    ## 5092                      Blk 49 Sims Place
    ## 5093                      Blk 49 Sims Place
    ## 5094                      Blk 49 Sims Place
    ## 5095                      Blk 49 Sims Place
    ## 5096                      Blk 49 Sims Place
    ## 5097                      Blk 49 Sims Place
    ## 5098                      Blk 49 Sims Place
    ## 5099                      Blk 49 Sims Place
    ## 5100                      Blk 49 Sims Place
    ## 5101                      Blk 49 Sims Place
    ## 5102                      Blk 49 Sims Place
    ## 5103                      Blk 49 Sims Place
    ## 5104                      Blk 49 Sims Place
    ## 5105                      Blk 49 Sims Place
    ## 5106                      Blk 49 Sims Place
    ## 5107                      Blk 49 Sims Place
    ## 5108                      Blk 49 Sims Place
    ## 5109                      Blk 49 Sims Place
    ## 5110                      Blk 49 Sims Place
    ## 5111                      Blk 49 Sims Place
    ## 5112                      Blk 49 Sims Place
    ## 5113                      Blk 49 Sims Place
    ## 5114                      Blk 49 Sims Place
    ## 5115                      Blk 49 Sims Place
    ## 5116                      Blk 49 Sims Place
    ## 5117                      Blk 49 Sims Place
    ## 5118                      Blk 49 Sims Place
    ## 5119                      Blk 49 Sims Place
    ## 5120                      Blk 49 Sims Place
    ## 5121                      Blk 49 Sims Place
    ## 5122                      Blk 49 Sims Place
    ## 5123                      Blk 49 Sims Place
    ## 5124                      Blk 49 Sims Place
    ## 5125                      Blk 49 Sims Place
    ## 5126                      Blk 49 Sims Place
    ## 5127                      Blk 49 Sims Place
    ## 5128                      Blk 49 Sims Place
    ## 5129                      Blk 49 Sims Place
    ## 5130                      Blk 49 Sims Place
    ## 5131                  Blk 4A Eunos Crescent
    ## 5132                  Blk 4A Eunos Crescent
    ## 5133                  Blk 4A Eunos Crescent
    ## 5134                  Blk 4A Eunos Crescent
    ## 5135                  Blk 4A Eunos Crescent
    ## 5136                  Blk 4A Eunos Crescent
    ## 5137                  Blk 4A Eunos Crescent
    ## 5138                  Blk 4A Eunos Crescent
    ## 5139                  Blk 4A Eunos Crescent
    ## 5140                  Blk 4A Eunos Crescent
    ## 5141                  Blk 4A Eunos Crescent
    ## 5142                  Blk 4A Eunos Crescent
    ## 5143                  Blk 4A Eunos Crescent
    ## 5144                  Blk 4A Eunos Crescent
    ## 5145                  Blk 4A Eunos Crescent
    ## 5146                  Blk 4A Eunos Crescent
    ## 5147                  Blk 4A Eunos Crescent
    ## 5148                  Blk 4A Eunos Crescent
    ## 5149                  Blk 4A Eunos Crescent
    ## 5150                  Blk 4A Eunos Crescent
    ## 5151                  Blk 4A Eunos Crescent
    ## 5152                  Blk 4A Eunos Crescent
    ## 5153                  Blk 4A Eunos Crescent
    ## 5154                  Blk 4A Eunos Crescent
    ## 5155                  Blk 4A Eunos Crescent
    ## 5156                  Blk 4A Eunos Crescent
    ## 5157                  Blk 4A Eunos Crescent
    ## 5158                  Blk 4A Eunos Crescent
    ## 5159                  Blk 4A Eunos Crescent
    ## 5160                  Blk 4A Eunos Crescent
    ## 5161                  Blk 4A Eunos Crescent
    ## 5162                  Blk 4A Eunos Crescent
    ## 5163                  Blk 4A Eunos Crescent
    ## 5164                  Blk 4A Eunos Crescent
    ## 5165                  Blk 4A Eunos Crescent
    ## 5166                  Blk 4A Eunos Crescent
    ## 5167                  Blk 4A Eunos Crescent
    ## 5168                  Blk 4A Eunos Crescent
    ## 5169                  Blk 4A Eunos Crescent
    ## 5170                  Blk 4A Eunos Crescent
    ## 5171                  Blk 4A Eunos Crescent
    ## 5172                  Blk 4A Eunos Crescent
    ## 5173                  Blk 4A Eunos Crescent
    ## 5174                  Blk 4A Eunos Crescent
    ## 5175                  Blk 4A Eunos Crescent
    ## 5176                  Blk 4A Eunos Crescent
    ## 5177                  Blk 4A Eunos Crescent
    ## 5178                  Blk 4A Eunos Crescent
    ## 5179                  Blk 4A Eunos Crescent
    ## 5180                  Blk 4A Eunos Crescent
    ## 5181                  Blk 4A Eunos Crescent
    ## 5182                  Blk 4A Eunos Crescent
    ## 5183                  Blk 4A Eunos Crescent
    ## 5184                  Blk 4A Eunos Crescent
    ## 5185                  Blk 4A Eunos Crescent
    ## 5186                      Blk 4A Jalan Batu
    ## 5187                      Blk 4A Jalan Batu
    ## 5188                      Blk 4A Jalan Batu
    ## 5189                      Blk 4A Jalan Batu
    ## 5190                      Blk 4A Jalan Batu
    ## 5191                      Blk 4A Jalan Batu
    ## 5192                      Blk 4A Jalan Batu
    ## 5193                      Blk 4A Jalan Batu
    ## 5194                      Blk 4A Jalan Batu
    ## 5195                      Blk 4A Jalan Batu
    ## 5196                      Blk 4A Jalan Batu
    ## 5197                      Blk 4A Jalan Batu
    ## 5198                      Blk 4A Jalan Batu
    ## 5199                      Blk 4A Jalan Batu
    ## 5200                      Blk 4A Jalan Batu
    ## 5201                      Blk 4A Jalan Batu
    ## 5202                      Blk 4A Jalan Batu
    ## 5203                      Blk 4A Jalan Batu
    ## 5204                      Blk 4A Jalan Batu
    ## 5205                      Blk 4A Jalan Batu
    ## 5206                      Blk 4A Jalan Batu
    ## 5207                      Blk 4A Jalan Batu
    ## 5208                      Blk 4A Jalan Batu
    ## 5209                      Blk 4A Jalan Batu
    ## 5210                      Blk 4A Jalan Batu
    ## 5211                      Blk 4A Jalan Batu
    ## 5212                      Blk 4A Jalan Batu
    ## 5213                      Blk 4A Jalan Batu
    ## 5214                      Blk 4A Jalan Batu
    ## 5215                      Blk 4A Jalan Batu
    ## 5216                      Blk 4A Jalan Batu
    ## 5217                      Blk 4A Jalan Batu
    ## 5218                      Blk 4A Jalan Batu
    ## 5219                      Blk 4A Jalan Batu
    ## 5220                      Blk 4A Jalan Batu
    ## 5221                      Blk 4A Jalan Batu
    ## 5222                      Blk 4A Jalan Batu
    ## 5223                      Blk 4A Jalan Batu
    ## 5224                      Blk 4A Jalan Batu
    ## 5225                      Blk 4A Jalan Batu
    ## 5226                      Blk 4A Jalan Batu
    ## 5227                      Blk 4A Jalan Batu
    ## 5228                      Blk 4A Jalan Batu
    ## 5229                      Blk 4A Jalan Batu
    ## 5230                      Blk 4A Jalan Batu
    ## 5231                      Blk 4A Jalan Batu
    ## 5232                      Blk 4A Jalan Batu
    ## 5233                      Blk 4A Jalan Batu
    ## 5234                      Blk 4A Jalan Batu
    ## 5235                      Blk 4A Jalan Batu
    ## 5236                      Blk 4A Jalan Batu
    ## 5237                      Blk 4A Jalan Batu
    ## 5238                      Blk 4A Jalan Batu
    ## 5239                      Blk 4A Jalan Batu
    ## 5240           Blk 4A Woodlands Centre Road
    ## 5241           Blk 4A Woodlands Centre Road
    ## 5242           Blk 4A Woodlands Centre Road
    ## 5243           Blk 4A Woodlands Centre Road
    ## 5244           Blk 4A Woodlands Centre Road
    ## 5245           Blk 4A Woodlands Centre Road
    ## 5246           Blk 4A Woodlands Centre Road
    ## 5247           Blk 4A Woodlands Centre Road
    ## 5248           Blk 4A Woodlands Centre Road
    ## 5249           Blk 4A Woodlands Centre Road
    ## 5250           Blk 4A Woodlands Centre Road
    ## 5251           Blk 4A Woodlands Centre Road
    ## 5252           Blk 4A Woodlands Centre Road
    ## 5253           Blk 4A Woodlands Centre Road
    ## 5254           Blk 4A Woodlands Centre Road
    ## 5255           Blk 4A Woodlands Centre Road
    ## 5256           Blk 4A Woodlands Centre Road
    ## 5257           Blk 4A Woodlands Centre Road
    ## 5258           Blk 4A Woodlands Centre Road
    ## 5259           Blk 4A Woodlands Centre Road
    ## 5260           Blk 4A Woodlands Centre Road
    ## 5261           Blk 4A Woodlands Centre Road
    ## 5262           Blk 4A Woodlands Centre Road
    ## 5263           Blk 4A Woodlands Centre Road
    ## 5264           Blk 4A Woodlands Centre Road
    ## 5265           Blk 4A Woodlands Centre Road
    ## 5266           Blk 4A Woodlands Centre Road
    ## 5267           Blk 4A Woodlands Centre Road
    ## 5268           Blk 4A Woodlands Centre Road
    ## 5269           Blk 4A Woodlands Centre Road
    ## 5270           Blk 4A Woodlands Centre Road
    ## 5271           Blk 4A Woodlands Centre Road
    ## 5272           Blk 4A Woodlands Centre Road
    ## 5273           Blk 4A Woodlands Centre Road
    ## 5274           Blk 4A Woodlands Centre Road
    ## 5275           Blk 4A Woodlands Centre Road
    ## 5276           Blk 4A Woodlands Centre Road
    ## 5277           Blk 4A Woodlands Centre Road
    ## 5278           Blk 4A Woodlands Centre Road
    ## 5279           Blk 4A Woodlands Centre Road
    ## 5280           Blk 4A Woodlands Centre Road
    ## 5281           Blk 4A Woodlands Centre Road
    ## 5282           Blk 4A Woodlands Centre Road
    ## 5283           Blk 4A Woodlands Centre Road
    ## 5284           Blk 4A Woodlands Centre Road
    ## 5285           Blk 4A Woodlands Centre Road
    ## 5286           Blk 4A Woodlands Centre Road
    ## 5287           Blk 4A Woodlands Centre Road
    ## 5288           Blk 4A Woodlands Centre Road
    ## 5289           Blk 4A Woodlands Centre Road
    ## 5290           Blk 4A Woodlands Centre Road
    ## 5291           Blk 4A Woodlands Centre Road
    ## 5292           Blk 4A Woodlands Centre Road
    ## 5293               Blk 502 West Coast Drive
    ## 5294               Blk 502 West Coast Drive
    ## 5295               Blk 502 West Coast Drive
    ## 5296               Blk 502 West Coast Drive
    ## 5297               Blk 502 West Coast Drive
    ## 5298               Blk 502 West Coast Drive
    ## 5299               Blk 502 West Coast Drive
    ## 5300               Blk 502 West Coast Drive
    ## 5301               Blk 502 West Coast Drive
    ## 5302               Blk 502 West Coast Drive
    ## 5303               Blk 502 West Coast Drive
    ## 5304               Blk 502 West Coast Drive
    ## 5305               Blk 502 West Coast Drive
    ## 5306               Blk 502 West Coast Drive
    ## 5307               Blk 502 West Coast Drive
    ## 5308               Blk 502 West Coast Drive
    ## 5309               Blk 502 West Coast Drive
    ## 5310               Blk 502 West Coast Drive
    ## 5311               Blk 502 West Coast Drive
    ## 5312               Blk 502 West Coast Drive
    ## 5313               Blk 502 West Coast Drive
    ## 5314               Blk 502 West Coast Drive
    ## 5315               Blk 502 West Coast Drive
    ## 5316               Blk 502 West Coast Drive
    ## 5317               Blk 502 West Coast Drive
    ## 5318               Blk 502 West Coast Drive
    ## 5319               Blk 502 West Coast Drive
    ## 5320               Blk 502 West Coast Drive
    ## 5321               Blk 502 West Coast Drive
    ## 5322               Blk 502 West Coast Drive
    ## 5323               Blk 502 West Coast Drive
    ## 5324               Blk 502 West Coast Drive
    ## 5325               Blk 502 West Coast Drive
    ## 5326               Blk 502 West Coast Drive
    ## 5327               Blk 502 West Coast Drive
    ## 5328               Blk 502 West Coast Drive
    ## 5329               Blk 502 West Coast Drive
    ## 5330               Blk 502 West Coast Drive
    ## 5331               Blk 502 West Coast Drive
    ## 5332               Blk 502 West Coast Drive
    ## 5333               Blk 502 West Coast Drive
    ## 5334               Blk 502 West Coast Drive
    ## 5335               Blk 502 West Coast Drive
    ## 5336               Blk 502 West Coast Drive
    ## 5337               Blk 502 West Coast Drive
    ## 5338               Blk 502 West Coast Drive
    ## 5339               Blk 502 West Coast Drive
    ## 5340               Blk 502 West Coast Drive
    ## 5341               Blk 502 West Coast Drive
    ## 5342               Blk 502 West Coast Drive
    ## 5343               Blk 502 West Coast Drive
    ## 5344               Blk 502 West Coast Drive
    ## 5345               Blk 503 West Coast Drive
    ## 5346               Blk 503 West Coast Drive
    ## 5347               Blk 503 West Coast Drive
    ## 5348               Blk 503 West Coast Drive
    ## 5349               Blk 503 West Coast Drive
    ## 5350               Blk 503 West Coast Drive
    ## 5351               Blk 503 West Coast Drive
    ## 5352               Blk 503 West Coast Drive
    ## 5353               Blk 503 West Coast Drive
    ## 5354               Blk 503 West Coast Drive
    ## 5355               Blk 503 West Coast Drive
    ## 5356               Blk 503 West Coast Drive
    ## 5357               Blk 503 West Coast Drive
    ## 5358               Blk 503 West Coast Drive
    ## 5359               Blk 503 West Coast Drive
    ## 5360               Blk 503 West Coast Drive
    ## 5361               Blk 503 West Coast Drive
    ## 5362               Blk 503 West Coast Drive
    ## 5363               Blk 503 West Coast Drive
    ## 5364               Blk 503 West Coast Drive
    ## 5365               Blk 503 West Coast Drive
    ## 5366               Blk 503 West Coast Drive
    ## 5367               Blk 503 West Coast Drive
    ## 5368               Blk 503 West Coast Drive
    ## 5369               Blk 503 West Coast Drive
    ## 5370               Blk 503 West Coast Drive
    ## 5371               Blk 503 West Coast Drive
    ## 5372               Blk 503 West Coast Drive
    ## 5373               Blk 503 West Coast Drive
    ## 5374               Blk 503 West Coast Drive
    ## 5375               Blk 503 West Coast Drive
    ## 5376               Blk 503 West Coast Drive
    ## 5377               Blk 503 West Coast Drive
    ## 5378               Blk 503 West Coast Drive
    ## 5379               Blk 503 West Coast Drive
    ## 5380               Blk 503 West Coast Drive
    ## 5381               Blk 503 West Coast Drive
    ## 5382               Blk 503 West Coast Drive
    ## 5383               Blk 503 West Coast Drive
    ## 5384               Blk 503 West Coast Drive
    ## 5385               Blk 503 West Coast Drive
    ## 5386               Blk 503 West Coast Drive
    ## 5387               Blk 503 West Coast Drive
    ## 5388               Blk 503 West Coast Drive
    ## 5389               Blk 503 West Coast Drive
    ## 5390               Blk 503 West Coast Drive
    ## 5391               Blk 503 West Coast Drive
    ## 5392               Blk 503 West Coast Drive
    ## 5393               Blk 503 West Coast Drive
    ## 5394               Blk 503 West Coast Drive
    ## 5395               Blk 503 West Coast Drive
    ## 5396          Blk 505 Jurong West Street 52
    ## 5397          Blk 505 Jurong West Street 52
    ## 5398          Blk 505 Jurong West Street 52
    ## 5399          Blk 505 Jurong West Street 52
    ## 5400          Blk 505 Jurong West Street 52
    ## 5401          Blk 505 Jurong West Street 52
    ## 5402          Blk 505 Jurong West Street 52
    ## 5403          Blk 505 Jurong West Street 52
    ## 5404          Blk 505 Jurong West Street 52
    ## 5405          Blk 505 Jurong West Street 52
    ## 5406          Blk 505 Jurong West Street 52
    ## 5407          Blk 505 Jurong West Street 52
    ## 5408          Blk 505 Jurong West Street 52
    ## 5409          Blk 505 Jurong West Street 52
    ## 5410          Blk 505 Jurong West Street 52
    ## 5411          Blk 505 Jurong West Street 52
    ## 5412          Blk 505 Jurong West Street 52
    ## 5413          Blk 505 Jurong West Street 52
    ## 5414          Blk 505 Jurong West Street 52
    ## 5415          Blk 505 Jurong West Street 52
    ## 5416          Blk 505 Jurong West Street 52
    ## 5417          Blk 505 Jurong West Street 52
    ## 5418          Blk 505 Jurong West Street 52
    ## 5419          Blk 505 Jurong West Street 52
    ## 5420          Blk 505 Jurong West Street 52
    ## 5421          Blk 505 Jurong West Street 52
    ## 5422          Blk 505 Jurong West Street 52
    ## 5423          Blk 505 Jurong West Street 52
    ## 5424          Blk 505 Jurong West Street 52
    ## 5425          Blk 505 Jurong West Street 52
    ## 5426          Blk 505 Jurong West Street 52
    ## 5427          Blk 505 Jurong West Street 52
    ## 5428          Blk 505 Jurong West Street 52
    ## 5429          Blk 505 Jurong West Street 52
    ## 5430          Blk 505 Jurong West Street 52
    ## 5431          Blk 505 Jurong West Street 52
    ## 5432          Blk 505 Jurong West Street 52
    ## 5433          Blk 505 Jurong West Street 52
    ## 5434          Blk 505 Jurong West Street 52
    ## 5435          Blk 505 Jurong West Street 52
    ## 5436          Blk 505 Jurong West Street 52
    ## 5437          Blk 505 Jurong West Street 52
    ## 5438          Blk 505 Jurong West Street 52
    ## 5439          Blk 505 Jurong West Street 52
    ## 5440          Blk 505 Jurong West Street 52
    ## 5441          Blk 505 Jurong West Street 52
    ## 5442          Blk 505 Jurong West Street 52
    ## 5443          Blk 505 Jurong West Street 52
    ## 5444          Blk 505 Jurong West Street 52
    ## 5445          Blk 505 Jurong West Street 52
    ## 5446                 Blk 50A Marine Terrace
    ## 5447                 Blk 50A Marine Terrace
    ## 5448                 Blk 50A Marine Terrace
    ## 5449                 Blk 50A Marine Terrace
    ## 5450                 Blk 50A Marine Terrace
    ## 5451                 Blk 50A Marine Terrace
    ## 5452                 Blk 50A Marine Terrace
    ## 5453                 Blk 50A Marine Terrace
    ## 5454                 Blk 50A Marine Terrace
    ## 5455                 Blk 50A Marine Terrace
    ## 5456                 Blk 50A Marine Terrace
    ## 5457                 Blk 50A Marine Terrace
    ## 5458                 Blk 50A Marine Terrace
    ## 5459                 Blk 50A Marine Terrace
    ## 5460                 Blk 50A Marine Terrace
    ## 5461                 Blk 50A Marine Terrace
    ## 5462                 Blk 50A Marine Terrace
    ## 5463                 Blk 50A Marine Terrace
    ## 5464                 Blk 50A Marine Terrace
    ## 5465                 Blk 50A Marine Terrace
    ## 5466                 Blk 50A Marine Terrace
    ## 5467                 Blk 50A Marine Terrace
    ## 5468                 Blk 50A Marine Terrace
    ## 5469                 Blk 50A Marine Terrace
    ## 5470                 Blk 50A Marine Terrace
    ## 5471                 Blk 50A Marine Terrace
    ## 5472                 Blk 50A Marine Terrace
    ## 5473                 Blk 50A Marine Terrace
    ## 5474                 Blk 50A Marine Terrace
    ## 5475                 Blk 50A Marine Terrace
    ## 5476                 Blk 50A Marine Terrace
    ## 5477                 Blk 50A Marine Terrace
    ## 5478                 Blk 50A Marine Terrace
    ## 5479                 Blk 50A Marine Terrace
    ## 5480                 Blk 50A Marine Terrace
    ## 5481                 Blk 50A Marine Terrace
    ## 5482                 Blk 50A Marine Terrace
    ## 5483                 Blk 50A Marine Terrace
    ## 5484                 Blk 50A Marine Terrace
    ## 5485                 Blk 50A Marine Terrace
    ## 5486                 Blk 50A Marine Terrace
    ## 5487                 Blk 50A Marine Terrace
    ## 5488                 Blk 50A Marine Terrace
    ## 5489                 Blk 50A Marine Terrace
    ## 5490                 Blk 50A Marine Terrace
    ## 5491                 Blk 50A Marine Terrace
    ## 5492                 Blk 50A Marine Terrace
    ## 5493                 Blk 50A Marine Terrace
    ## 5494                 Blk 50A Marine Terrace
    ## 5495                Blk 51 Old Airport Road
    ## 5496                Blk 51 Old Airport Road
    ## 5497                Blk 51 Old Airport Road
    ## 5498                Blk 51 Old Airport Road
    ## 5499                Blk 51 Old Airport Road
    ## 5500                Blk 51 Old Airport Road
    ## 5501                Blk 51 Old Airport Road
    ## 5502                Blk 51 Old Airport Road
    ## 5503                Blk 51 Old Airport Road
    ## 5504                Blk 51 Old Airport Road
    ## 5505                Blk 51 Old Airport Road
    ## 5506                Blk 51 Old Airport Road
    ## 5507                Blk 51 Old Airport Road
    ## 5508                Blk 51 Old Airport Road
    ## 5509                Blk 51 Old Airport Road
    ## 5510                Blk 51 Old Airport Road
    ## 5511                Blk 51 Old Airport Road
    ## 5512                Blk 51 Old Airport Road
    ## 5513                Blk 51 Old Airport Road
    ## 5514                Blk 51 Old Airport Road
    ## 5515                Blk 51 Old Airport Road
    ## 5516                Blk 51 Old Airport Road
    ## 5517                Blk 51 Old Airport Road
    ## 5518                Blk 51 Old Airport Road
    ## 5519                Blk 51 Old Airport Road
    ## 5520                Blk 51 Old Airport Road
    ## 5521                Blk 51 Old Airport Road
    ## 5522                Blk 51 Old Airport Road
    ## 5523                Blk 51 Old Airport Road
    ## 5524                Blk 51 Old Airport Road
    ## 5525                Blk 51 Old Airport Road
    ## 5526                Blk 51 Old Airport Road
    ## 5527                Blk 51 Old Airport Road
    ## 5528                Blk 51 Old Airport Road
    ## 5529                Blk 51 Old Airport Road
    ## 5530                Blk 51 Old Airport Road
    ## 5531                Blk 51 Old Airport Road
    ## 5532                Blk 51 Old Airport Road
    ## 5533                Blk 51 Old Airport Road
    ## 5534                Blk 51 Old Airport Road
    ## 5535                Blk 51 Old Airport Road
    ## 5536                Blk 51 Old Airport Road
    ## 5537                Blk 51 Old Airport Road
    ## 5538                Blk 51 Old Airport Road
    ## 5539                Blk 51 Old Airport Road
    ## 5540                Blk 51 Old Airport Road
    ## 5541                Blk 51 Old Airport Road
    ## 5542                Blk 51 Old Airport Road
    ## 5543           Blk 511 Bedok North Street 3
    ## 5544           Blk 511 Bedok North Street 3
    ## 5545           Blk 511 Bedok North Street 3
    ## 5546           Blk 511 Bedok North Street 3
    ## 5547           Blk 511 Bedok North Street 3
    ## 5548           Blk 511 Bedok North Street 3
    ## 5549           Blk 511 Bedok North Street 3
    ## 5550           Blk 511 Bedok North Street 3
    ## 5551           Blk 511 Bedok North Street 3
    ## 5552           Blk 511 Bedok North Street 3
    ## 5553           Blk 511 Bedok North Street 3
    ## 5554           Blk 511 Bedok North Street 3
    ## 5555           Blk 511 Bedok North Street 3
    ## 5556           Blk 511 Bedok North Street 3
    ## 5557           Blk 511 Bedok North Street 3
    ## 5558           Blk 511 Bedok North Street 3
    ## 5559           Blk 511 Bedok North Street 3
    ## 5560           Blk 511 Bedok North Street 3
    ## 5561           Blk 511 Bedok North Street 3
    ## 5562           Blk 511 Bedok North Street 3
    ## 5563           Blk 511 Bedok North Street 3
    ## 5564           Blk 511 Bedok North Street 3
    ## 5565           Blk 511 Bedok North Street 3
    ## 5566           Blk 511 Bedok North Street 3
    ## 5567           Blk 511 Bedok North Street 3
    ## 5568           Blk 511 Bedok North Street 3
    ## 5569           Blk 511 Bedok North Street 3
    ## 5570           Blk 511 Bedok North Street 3
    ## 5571           Blk 511 Bedok North Street 3
    ## 5572           Blk 511 Bedok North Street 3
    ## 5573           Blk 511 Bedok North Street 3
    ## 5574           Blk 511 Bedok North Street 3
    ## 5575           Blk 511 Bedok North Street 3
    ## 5576           Blk 511 Bedok North Street 3
    ## 5577           Blk 511 Bedok North Street 3
    ## 5578           Blk 511 Bedok North Street 3
    ## 5579           Blk 511 Bedok North Street 3
    ## 5580           Blk 511 Bedok North Street 3
    ## 5581           Blk 511 Bedok North Street 3
    ## 5582           Blk 511 Bedok North Street 3
    ## 5583           Blk 511 Bedok North Street 3
    ## 5584           Blk 511 Bedok North Street 3
    ## 5585           Blk 511 Bedok North Street 3
    ## 5586           Blk 511 Bedok North Street 3
    ## 5587           Blk 511 Bedok North Street 3
    ## 5588           Blk 511 Bedok North Street 3
    ## 5589           Blk 511 Bedok North Street 3
    ## 5590 Bukit Panjang Hawker Centre and Market
    ## 5591 Bukit Panjang Hawker Centre and Market
    ## 5592 Bukit Panjang Hawker Centre and Market
    ## 5593 Bukit Panjang Hawker Centre and Market
    ## 5594 Bukit Panjang Hawker Centre and Market
    ## 5595 Bukit Panjang Hawker Centre and Market
    ## 5596 Bukit Panjang Hawker Centre and Market
    ## 5597 Bukit Panjang Hawker Centre and Market
    ## 5598 Bukit Panjang Hawker Centre and Market
    ## 5599 Bukit Panjang Hawker Centre and Market
    ## 5600 Bukit Panjang Hawker Centre and Market
    ## 5601 Bukit Panjang Hawker Centre and Market
    ## 5602 Bukit Panjang Hawker Centre and Market
    ## 5603 Bukit Panjang Hawker Centre and Market
    ## 5604 Bukit Panjang Hawker Centre and Market
    ## 5605 Bukit Panjang Hawker Centre and Market
    ## 5606 Bukit Panjang Hawker Centre and Market
    ## 5607 Bukit Panjang Hawker Centre and Market
    ## 5608 Bukit Panjang Hawker Centre and Market
    ## 5609 Bukit Panjang Hawker Centre and Market
    ## 5610 Bukit Panjang Hawker Centre and Market
    ## 5611 Bukit Panjang Hawker Centre and Market
    ## 5612 Bukit Panjang Hawker Centre and Market
    ## 5613 Bukit Panjang Hawker Centre and Market
    ## 5614 Bukit Panjang Hawker Centre and Market
    ## 5615 Bukit Panjang Hawker Centre and Market
    ## 5616 Bukit Panjang Hawker Centre and Market
    ## 5617 Bukit Panjang Hawker Centre and Market
    ## 5618 Bukit Panjang Hawker Centre and Market
    ## 5619 Bukit Panjang Hawker Centre and Market
    ## 5620 Bukit Panjang Hawker Centre and Market
    ## 5621 Bukit Panjang Hawker Centre and Market
    ## 5622 Bukit Panjang Hawker Centre and Market
    ## 5623 Bukit Panjang Hawker Centre and Market
    ## 5624 Bukit Panjang Hawker Centre and Market
    ## 5625 Bukit Panjang Hawker Centre and Market
    ## 5626 Bukit Panjang Hawker Centre and Market
    ## 5627 Bukit Panjang Hawker Centre and Market
    ## 5628 Bukit Panjang Hawker Centre and Market
    ## 5629 Bukit Panjang Hawker Centre and Market
    ## 5630 Bukit Panjang Hawker Centre and Market
    ## 5631 Bukit Panjang Hawker Centre and Market
    ## 5632 Bukit Panjang Hawker Centre and Market
    ## 5633 Bukit Panjang Hawker Centre and Market
    ## 5634 Bukit Panjang Hawker Centre and Market
    ## 5635 Bukit Panjang Hawker Centre and Market
    ## 5636         Our Tampines Hub Hawker Centre
    ## 5637         Our Tampines Hub Hawker Centre
    ## 5638         Our Tampines Hub Hawker Centre
    ## 5639         Our Tampines Hub Hawker Centre
    ## 5640         Our Tampines Hub Hawker Centre
    ## 5641         Our Tampines Hub Hawker Centre
    ## 5642         Our Tampines Hub Hawker Centre
    ## 5643         Our Tampines Hub Hawker Centre
    ## 5644         Our Tampines Hub Hawker Centre
    ## 5645         Our Tampines Hub Hawker Centre
    ## 5646         Our Tampines Hub Hawker Centre
    ## 5647         Our Tampines Hub Hawker Centre
    ## 5648         Our Tampines Hub Hawker Centre
    ## 5649         Our Tampines Hub Hawker Centre
    ## 5650         Our Tampines Hub Hawker Centre
    ## 5651         Our Tampines Hub Hawker Centre
    ## 5652         Our Tampines Hub Hawker Centre
    ## 5653         Our Tampines Hub Hawker Centre
    ## 5654         Our Tampines Hub Hawker Centre
    ## 5655         Our Tampines Hub Hawker Centre
    ## 5656         Our Tampines Hub Hawker Centre
    ## 5657         Our Tampines Hub Hawker Centre
    ## 5658         Our Tampines Hub Hawker Centre
    ## 5659         Our Tampines Hub Hawker Centre
    ## 5660         Our Tampines Hub Hawker Centre
    ## 5661         Our Tampines Hub Hawker Centre
    ## 5662         Our Tampines Hub Hawker Centre
    ## 5663         Our Tampines Hub Hawker Centre
    ## 5664         Our Tampines Hub Hawker Centre
    ## 5665         Our Tampines Hub Hawker Centre
    ## 5666         Our Tampines Hub Hawker Centre
    ## 5667         Our Tampines Hub Hawker Centre
    ## 5668         Our Tampines Hub Hawker Centre
    ## 5669         Our Tampines Hub Hawker Centre
    ## 5670         Our Tampines Hub Hawker Centre
    ## 5671         Our Tampines Hub Hawker Centre
    ## 5672         Our Tampines Hub Hawker Centre
    ## 5673         Our Tampines Hub Hawker Centre
    ## 5674         Our Tampines Hub Hawker Centre
    ## 5675         Our Tampines Hub Hawker Centre
    ## 5676         Our Tampines Hub Hawker Centre
    ## 5677         Our Tampines Hub Hawker Centre
    ## 5678         Our Tampines Hub Hawker Centre
    ## 5679         Our Tampines Hub Hawker Centre
    ## 5680         Our Tampines Hub Hawker Centre
    ## 5681        Kampung Admiralty Hawker Centre
    ## 5682        Kampung Admiralty Hawker Centre
    ## 5683        Kampung Admiralty Hawker Centre
    ## 5684        Kampung Admiralty Hawker Centre
    ## 5685        Kampung Admiralty Hawker Centre
    ## 5686        Kampung Admiralty Hawker Centre
    ## 5687        Kampung Admiralty Hawker Centre
    ## 5688        Kampung Admiralty Hawker Centre
    ## 5689        Kampung Admiralty Hawker Centre
    ## 5690        Kampung Admiralty Hawker Centre
    ## 5691        Kampung Admiralty Hawker Centre
    ## 5692        Kampung Admiralty Hawker Centre
    ## 5693        Kampung Admiralty Hawker Centre
    ## 5694        Kampung Admiralty Hawker Centre
    ## 5695        Kampung Admiralty Hawker Centre
    ## 5696        Kampung Admiralty Hawker Centre
    ## 5697        Kampung Admiralty Hawker Centre
    ## 5698        Kampung Admiralty Hawker Centre
    ## 5699        Kampung Admiralty Hawker Centre
    ## 5700        Kampung Admiralty Hawker Centre
    ## 5701        Kampung Admiralty Hawker Centre
    ## 5702        Kampung Admiralty Hawker Centre
    ## 5703        Kampung Admiralty Hawker Centre
    ## 5704        Kampung Admiralty Hawker Centre
    ## 5705        Kampung Admiralty Hawker Centre
    ## 5706        Kampung Admiralty Hawker Centre
    ## 5707        Kampung Admiralty Hawker Centre
    ## 5708        Kampung Admiralty Hawker Centre
    ## 5709        Kampung Admiralty Hawker Centre
    ## 5710        Kampung Admiralty Hawker Centre
    ## 5711        Kampung Admiralty Hawker Centre
    ## 5712        Kampung Admiralty Hawker Centre
    ## 5713        Kampung Admiralty Hawker Centre
    ## 5714        Kampung Admiralty Hawker Centre
    ## 5715        Kampung Admiralty Hawker Centre
    ## 5716        Kampung Admiralty Hawker Centre
    ## 5717        Kampung Admiralty Hawker Centre
    ## 5718        Kampung Admiralty Hawker Centre
    ## 5719        Kampung Admiralty Hawker Centre
    ## 5720        Kampung Admiralty Hawker Centre
    ## 5721        Kampung Admiralty Hawker Centre
    ## 5722        Kampung Admiralty Hawker Centre
    ## 5723        Kampung Admiralty Hawker Centre
    ## 5724        Kampung Admiralty Hawker Centre
    ## 5725                   Yishun Hawker Centre
    ## 5726                   Yishun Hawker Centre
    ## 5727                   Yishun Hawker Centre
    ## 5728                   Yishun Hawker Centre
    ## 5729                   Yishun Hawker Centre
    ## 5730                   Yishun Hawker Centre
    ## 5731                   Yishun Hawker Centre
    ## 5732                   Yishun Hawker Centre
    ## 5733                   Yishun Hawker Centre
    ## 5734                   Yishun Hawker Centre
    ## 5735                   Yishun Hawker Centre
    ## 5736                   Yishun Hawker Centre
    ## 5737                   Yishun Hawker Centre
    ## 5738                   Yishun Hawker Centre
    ## 5739                   Yishun Hawker Centre
    ## 5740                   Yishun Hawker Centre
    ## 5741                   Yishun Hawker Centre
    ## 5742                   Yishun Hawker Centre
    ## 5743                   Yishun Hawker Centre
    ## 5744                   Yishun Hawker Centre
    ## 5745                   Yishun Hawker Centre
    ## 5746                   Yishun Hawker Centre
    ## 5747                   Yishun Hawker Centre
    ## 5748                   Yishun Hawker Centre
    ## 5749                   Yishun Hawker Centre
    ## 5750                   Yishun Hawker Centre
    ## 5751                   Yishun Hawker Centre
    ## 5752                   Yishun Hawker Centre
    ## 5753                   Yishun Hawker Centre
    ## 5754                   Yishun Hawker Centre
    ## 5755                   Yishun Hawker Centre
    ## 5756                   Yishun Hawker Centre
    ## 5757                   Yishun Hawker Centre
    ## 5758                   Yishun Hawker Centre
    ## 5759                   Yishun Hawker Centre
    ## 5760                   Yishun Hawker Centre
    ## 5761                   Yishun Hawker Centre
    ## 5762                   Yishun Hawker Centre
    ## 5763                   Yishun Hawker Centre
    ## 5764                   Yishun Hawker Centre
    ## 5765                   Yishun Hawker Centre
    ## 5766                   Yishun Hawker Centre
    ## 5767                   Yishun Hawker Centre
    ## 5768   Jurong West Hawker Centre and Market
    ## 5769   Jurong West Hawker Centre and Market
    ## 5770   Jurong West Hawker Centre and Market
    ## 5771   Jurong West Hawker Centre and Market
    ## 5772   Jurong West Hawker Centre and Market
    ## 5773   Jurong West Hawker Centre and Market
    ## 5774   Jurong West Hawker Centre and Market
    ## 5775   Jurong West Hawker Centre and Market
    ## 5776   Jurong West Hawker Centre and Market
    ## 5777   Jurong West Hawker Centre and Market
    ## 5778   Jurong West Hawker Centre and Market
    ## 5779   Jurong West Hawker Centre and Market
    ## 5780   Jurong West Hawker Centre and Market
    ## 5781   Jurong West Hawker Centre and Market
    ## 5782   Jurong West Hawker Centre and Market
    ## 5783   Jurong West Hawker Centre and Market
    ## 5784   Jurong West Hawker Centre and Market
    ## 5785   Jurong West Hawker Centre and Market
    ## 5786   Jurong West Hawker Centre and Market
    ## 5787   Jurong West Hawker Centre and Market
    ## 5788   Jurong West Hawker Centre and Market
    ## 5789   Jurong West Hawker Centre and Market
    ## 5790   Jurong West Hawker Centre and Market
    ## 5791   Jurong West Hawker Centre and Market
    ## 5792   Jurong West Hawker Centre and Market
    ## 5793   Jurong West Hawker Centre and Market
    ## 5794   Jurong West Hawker Centre and Market
    ## 5795   Jurong West Hawker Centre and Market
    ## 5796   Jurong West Hawker Centre and Market
    ## 5797   Jurong West Hawker Centre and Market
    ## 5798   Jurong West Hawker Centre and Market
    ## 5799   Jurong West Hawker Centre and Market
    ## 5800   Jurong West Hawker Centre and Market
    ## 5801   Jurong West Hawker Centre and Market
    ## 5802   Jurong West Hawker Centre and Market
    ## 5803   Jurong West Hawker Centre and Market
    ## 5804   Jurong West Hawker Centre and Market
    ## 5805   Jurong West Hawker Centre and Market
    ## 5806   Jurong West Hawker Centre and Market
    ## 5807   Jurong West Hawker Centre and Market
    ## 5808   Jurong West Hawker Centre and Market
    ## 5809   Jurong West Hawker Centre and Market
    ## 5810        Pasir Ris Central Hawker Centre
    ## 5811        Pasir Ris Central Hawker Centre
    ## 5812        Pasir Ris Central Hawker Centre
    ## 5813        Pasir Ris Central Hawker Centre
    ## 5814        Pasir Ris Central Hawker Centre
    ## 5815        Pasir Ris Central Hawker Centre
    ## 5816        Pasir Ris Central Hawker Centre
    ## 5817        Pasir Ris Central Hawker Centre
    ## 5818        Pasir Ris Central Hawker Centre
    ## 5819        Pasir Ris Central Hawker Centre
    ## 5820        Pasir Ris Central Hawker Centre
    ## 5821        Pasir Ris Central Hawker Centre
    ## 5822        Pasir Ris Central Hawker Centre
    ## 5823        Pasir Ris Central Hawker Centre
    ## 5824        Pasir Ris Central Hawker Centre
    ## 5825        Pasir Ris Central Hawker Centre
    ## 5826        Pasir Ris Central Hawker Centre
    ## 5827        Pasir Ris Central Hawker Centre
    ## 5828        Pasir Ris Central Hawker Centre
    ## 5829        Pasir Ris Central Hawker Centre
    ## 5830        Pasir Ris Central Hawker Centre
    ## 5831        Pasir Ris Central Hawker Centre
    ## 5832        Pasir Ris Central Hawker Centre
    ## 5833        Pasir Ris Central Hawker Centre
    ## 5834        Pasir Ris Central Hawker Centre
    ## 5835        Pasir Ris Central Hawker Centre
    ## 5836        Pasir Ris Central Hawker Centre
    ## 5837        Pasir Ris Central Hawker Centre
    ## 5838        Pasir Ris Central Hawker Centre
    ## 5839        Pasir Ris Central Hawker Centre
    ## 5840        Pasir Ris Central Hawker Centre
    ## 5841        Pasir Ris Central Hawker Centre
    ## 5842        Pasir Ris Central Hawker Centre
    ## 5843        Pasir Ris Central Hawker Centre
    ## 5844        Pasir Ris Central Hawker Centre
    ## 5845        Pasir Ris Central Hawker Centre
    ## 5846        Pasir Ris Central Hawker Centre
    ## 5847        Pasir Ris Central Hawker Centre
    ## 5848        Pasir Ris Central Hawker Centre
    ## 5849        Pasir Ris Central Hawker Centre
    ## 5850        Pasir Ris Central Hawker Centre
    ## 5851                   Dawson Hawker Centre
    ## 5852                   Dawson Hawker Centre
    ## 5853                   Dawson Hawker Centre
    ## 5854                   Dawson Hawker Centre
    ## 5855                   Dawson Hawker Centre
    ## 5856                   Dawson Hawker Centre
    ## 5857                   Dawson Hawker Centre
    ## 5858                   Dawson Hawker Centre
    ## 5859                   Dawson Hawker Centre
    ## 5860                   Dawson Hawker Centre
    ## 5861                   Dawson Hawker Centre
    ## 5862                   Dawson Hawker Centre
    ## 5863                   Dawson Hawker Centre
    ## 5864                   Dawson Hawker Centre
    ## 5865                   Dawson Hawker Centre
    ## 5866                   Dawson Hawker Centre
    ## 5867                   Dawson Hawker Centre
    ## 5868                   Dawson Hawker Centre
    ## 5869                   Dawson Hawker Centre
    ## 5870                   Dawson Hawker Centre
    ## 5871                   Dawson Hawker Centre
    ## 5872                   Dawson Hawker Centre
    ## 5873                   Dawson Hawker Centre
    ## 5874                   Dawson Hawker Centre
    ## 5875                   Dawson Hawker Centre
    ## 5876                   Dawson Hawker Centre
    ## 5877                   Dawson Hawker Centre
    ## 5878                   Dawson Hawker Centre
    ## 5879                   Dawson Hawker Centre
    ## 5880                   Dawson Hawker Centre
    ## 5881                   Dawson Hawker Centre
    ## 5882                   Dawson Hawker Centre
    ## 5883                   Dawson Hawker Centre
    ## 5884                   Dawson Hawker Centre
    ## 5885                   Dawson Hawker Centre
    ## 5886                   Dawson Hawker Centre
    ## 5887                   Dawson Hawker Centre
    ## 5888                   Dawson Hawker Centre
    ## 5889                   Dawson Hawker Centre
    ## 5890                   Dawson Hawker Centre
    ## 5891      Woodlands Street 12 Hawker Centre
    ## 5892      Woodlands Street 12 Hawker Centre
    ## 5893      Woodlands Street 12 Hawker Centre
    ## 5894      Woodlands Street 12 Hawker Centre
    ## 5895      Woodlands Street 12 Hawker Centre
    ## 5896      Woodlands Street 12 Hawker Centre
    ## 5897      Woodlands Street 12 Hawker Centre
    ## 5898      Woodlands Street 12 Hawker Centre
    ## 5899      Woodlands Street 12 Hawker Centre
    ## 5900      Woodlands Street 12 Hawker Centre
    ## 5901      Woodlands Street 12 Hawker Centre
    ## 5902      Woodlands Street 12 Hawker Centre
    ## 5903      Woodlands Street 12 Hawker Centre
    ## 5904      Woodlands Street 12 Hawker Centre
    ## 5905      Woodlands Street 12 Hawker Centre
    ## 5906      Woodlands Street 12 Hawker Centre
    ## 5907      Woodlands Street 12 Hawker Centre
    ## 5908      Woodlands Street 12 Hawker Centre
    ## 5909      Woodlands Street 12 Hawker Centre
    ## 5910      Woodlands Street 12 Hawker Centre
    ## 5911      Woodlands Street 12 Hawker Centre
    ## 5912      Woodlands Street 12 Hawker Centre
    ## 5913      Woodlands Street 12 Hawker Centre
    ## 5914      Woodlands Street 12 Hawker Centre
    ## 5915      Woodlands Street 12 Hawker Centre
    ## 5916      Woodlands Street 12 Hawker Centre
    ## 5917      Woodlands Street 12 Hawker Centre
    ## 5918      Woodlands Street 12 Hawker Centre
    ## 5919      Woodlands Street 12 Hawker Centre
    ## 5920      Woodlands Street 12 Hawker Centre
    ## 5921      Woodlands Street 12 Hawker Centre
    ## 5922      Woodlands Street 12 Hawker Centre
    ## 5923      Woodlands Street 12 Hawker Centre
    ## 5924      Woodlands Street 12 Hawker Centre
    ## 5925      Woodlands Street 12 Hawker Centre
    ## 5926      Woodlands Street 12 Hawker Centre
    ## 5927      Woodlands Street 12 Hawker Centre
    ## 5928      Woodlands Street 12 Hawker Centre
    ## 5929      Woodlands Street 12 Hawker Centre
    ## 5930              Blk 527 Ang Mo Kio Ave 10
    ## 5931              Blk 527 Ang Mo Kio Ave 10
    ## 5932              Blk 527 Ang Mo Kio Ave 10
    ## 5933              Blk 527 Ang Mo Kio Ave 10
    ## 5934              Blk 527 Ang Mo Kio Ave 10
    ## 5935              Blk 527 Ang Mo Kio Ave 10
    ## 5936              Blk 527 Ang Mo Kio Ave 10
    ## 5937              Blk 527 Ang Mo Kio Ave 10
    ## 5938              Blk 527 Ang Mo Kio Ave 10
    ## 5939              Blk 527 Ang Mo Kio Ave 10
    ## 5940              Blk 527 Ang Mo Kio Ave 10
    ## 5941              Blk 527 Ang Mo Kio Ave 10
    ## 5942              Blk 527 Ang Mo Kio Ave 10
    ## 5943              Blk 527 Ang Mo Kio Ave 10
    ## 5944              Blk 527 Ang Mo Kio Ave 10
    ## 5945              Blk 527 Ang Mo Kio Ave 10
    ## 5946              Blk 527 Ang Mo Kio Ave 10
    ## 5947              Blk 527 Ang Mo Kio Ave 10
    ## 5948              Blk 527 Ang Mo Kio Ave 10
    ## 5949              Blk 527 Ang Mo Kio Ave 10
    ## 5950              Blk 527 Ang Mo Kio Ave 10
    ## 5951              Blk 527 Ang Mo Kio Ave 10
    ## 5952              Blk 527 Ang Mo Kio Ave 10
    ## 5953              Blk 527 Ang Mo Kio Ave 10
    ## 5954              Blk 527 Ang Mo Kio Ave 10
    ## 5955              Blk 527 Ang Mo Kio Ave 10
    ## 5956              Blk 527 Ang Mo Kio Ave 10
    ## 5957              Blk 527 Ang Mo Kio Ave 10
    ## 5958              Blk 527 Ang Mo Kio Ave 10
    ## 5959              Blk 527 Ang Mo Kio Ave 10
    ## 5960              Blk 527 Ang Mo Kio Ave 10
    ## 5961              Blk 527 Ang Mo Kio Ave 10
    ## 5962              Blk 527 Ang Mo Kio Ave 10
    ## 5963              Blk 527 Ang Mo Kio Ave 10
    ## 5964              Blk 527 Ang Mo Kio Ave 10
    ## 5965              Blk 527 Ang Mo Kio Ave 10
    ## 5966              Blk 527 Ang Mo Kio Ave 10
    ## 5967              Blk 527 Ang Mo Kio Ave 10
    ## 5968           Blk 538 Bedok North Street 3
    ## 5969           Blk 538 Bedok North Street 3
    ## 5970           Blk 538 Bedok North Street 3
    ## 5971           Blk 538 Bedok North Street 3
    ## 5972           Blk 538 Bedok North Street 3
    ## 5973           Blk 538 Bedok North Street 3
    ## 5974           Blk 538 Bedok North Street 3
    ## 5975           Blk 538 Bedok North Street 3
    ## 5976           Blk 538 Bedok North Street 3
    ## 5977           Blk 538 Bedok North Street 3
    ## 5978           Blk 538 Bedok North Street 3
    ## 5979           Blk 538 Bedok North Street 3
    ## 5980           Blk 538 Bedok North Street 3
    ## 5981           Blk 538 Bedok North Street 3
    ## 5982           Blk 538 Bedok North Street 3
    ## 5983           Blk 538 Bedok North Street 3
    ## 5984           Blk 538 Bedok North Street 3
    ## 5985           Blk 538 Bedok North Street 3
    ## 5986           Blk 538 Bedok North Street 3
    ## 5987           Blk 538 Bedok North Street 3
    ## 5988           Blk 538 Bedok North Street 3
    ## 5989           Blk 538 Bedok North Street 3
    ## 5990           Blk 538 Bedok North Street 3
    ## 5991           Blk 538 Bedok North Street 3
    ## 5992           Blk 538 Bedok North Street 3
    ## 5993           Blk 538 Bedok North Street 3
    ## 5994           Blk 538 Bedok North Street 3
    ## 5995           Blk 538 Bedok North Street 3
    ## 5996           Blk 538 Bedok North Street 3
    ## 5997           Blk 538 Bedok North Street 3
    ## 5998           Blk 538 Bedok North Street 3
    ## 5999           Blk 538 Bedok North Street 3
    ## 6000           Blk 538 Bedok North Street 3
    ## 6001           Blk 538 Bedok North Street 3
    ## 6002           Blk 538 Bedok North Street 3
    ## 6003           Blk 538 Bedok North Street 3
    ## 6004           Blk 538 Bedok North Street 3
    ## 6005           Blk 58 New Upper Changi Road
    ## 6006           Blk 58 New Upper Changi Road
    ## 6007           Blk 58 New Upper Changi Road
    ## 6008           Blk 58 New Upper Changi Road
    ## 6009           Blk 58 New Upper Changi Road
    ## 6010           Blk 58 New Upper Changi Road
    ## 6011           Blk 58 New Upper Changi Road
    ## 6012           Blk 58 New Upper Changi Road
    ## 6013           Blk 58 New Upper Changi Road
    ## 6014           Blk 58 New Upper Changi Road
    ## 6015           Blk 58 New Upper Changi Road
    ## 6016           Blk 58 New Upper Changi Road
    ## 6017           Blk 58 New Upper Changi Road
    ## 6018           Blk 58 New Upper Changi Road
    ## 6019           Blk 58 New Upper Changi Road
    ## 6020           Blk 58 New Upper Changi Road
    ## 6021           Blk 58 New Upper Changi Road
    ## 6022           Blk 58 New Upper Changi Road
    ## 6023           Blk 58 New Upper Changi Road
    ## 6024           Blk 58 New Upper Changi Road
    ## 6025           Blk 58 New Upper Changi Road
    ## 6026           Blk 58 New Upper Changi Road
    ## 6027           Blk 58 New Upper Changi Road
    ## 6028           Blk 58 New Upper Changi Road
    ## 6029           Blk 58 New Upper Changi Road
    ## 6030           Blk 58 New Upper Changi Road
    ## 6031           Blk 58 New Upper Changi Road
    ## 6032           Blk 58 New Upper Changi Road
    ## 6033           Blk 58 New Upper Changi Road
    ## 6034           Blk 58 New Upper Changi Road
    ## 6035           Blk 58 New Upper Changi Road
    ## 6036           Blk 58 New Upper Changi Road
    ## 6037           Blk 58 New Upper Changi Road
    ## 6038           Blk 58 New Upper Changi Road
    ## 6039           Blk 58 New Upper Changi Road
    ## 6040           Blk 58 New Upper Changi Road
    ## 6041              Blk 6 Tanjong Pagar Plaza
    ## 6042              Blk 6 Tanjong Pagar Plaza
    ## 6043              Blk 6 Tanjong Pagar Plaza
    ## 6044              Blk 6 Tanjong Pagar Plaza
    ## 6045              Blk 6 Tanjong Pagar Plaza
    ## 6046              Blk 6 Tanjong Pagar Plaza
    ## 6047              Blk 6 Tanjong Pagar Plaza
    ## 6048              Blk 6 Tanjong Pagar Plaza
    ## 6049              Blk 6 Tanjong Pagar Plaza
    ## 6050              Blk 6 Tanjong Pagar Plaza
    ## 6051              Blk 6 Tanjong Pagar Plaza
    ## 6052              Blk 6 Tanjong Pagar Plaza
    ## 6053              Blk 6 Tanjong Pagar Plaza
    ## 6054              Blk 6 Tanjong Pagar Plaza
    ## 6055              Blk 6 Tanjong Pagar Plaza
    ## 6056              Blk 6 Tanjong Pagar Plaza
    ## 6057              Blk 6 Tanjong Pagar Plaza
    ## 6058              Blk 6 Tanjong Pagar Plaza
    ## 6059              Blk 6 Tanjong Pagar Plaza
    ## 6060              Blk 6 Tanjong Pagar Plaza
    ## 6061              Blk 6 Tanjong Pagar Plaza
    ## 6062              Blk 6 Tanjong Pagar Plaza
    ## 6063              Blk 6 Tanjong Pagar Plaza
    ## 6064              Blk 6 Tanjong Pagar Plaza
    ## 6065              Blk 6 Tanjong Pagar Plaza
    ## 6066              Blk 6 Tanjong Pagar Plaza
    ## 6067              Blk 6 Tanjong Pagar Plaza
    ## 6068              Blk 6 Tanjong Pagar Plaza
    ## 6069              Blk 6 Tanjong Pagar Plaza
    ## 6070              Blk 6 Tanjong Pagar Plaza
    ## 6071              Blk 6 Tanjong Pagar Plaza
    ## 6072              Blk 6 Tanjong Pagar Plaza
    ## 6073              Blk 6 Tanjong Pagar Plaza
    ## 6074              Blk 6 Tanjong Pagar Plaza
    ## 6075              Blk 6 Tanjong Pagar Plaza
    ## 6076               Blk 628 Ang Mo Kio Ave 4
    ## 6077               Blk 628 Ang Mo Kio Ave 4
    ## 6078               Blk 628 Ang Mo Kio Ave 4
    ## 6079               Blk 628 Ang Mo Kio Ave 4
    ## 6080               Blk 628 Ang Mo Kio Ave 4
    ## 6081               Blk 628 Ang Mo Kio Ave 4
    ## 6082               Blk 628 Ang Mo Kio Ave 4
    ## 6083               Blk 628 Ang Mo Kio Ave 4
    ## 6084               Blk 628 Ang Mo Kio Ave 4
    ## 6085               Blk 628 Ang Mo Kio Ave 4
    ## 6086               Blk 628 Ang Mo Kio Ave 4
    ## 6087               Blk 628 Ang Mo Kio Ave 4
    ## 6088               Blk 628 Ang Mo Kio Ave 4
    ## 6089               Blk 628 Ang Mo Kio Ave 4
    ## 6090               Blk 628 Ang Mo Kio Ave 4
    ## 6091               Blk 628 Ang Mo Kio Ave 4
    ## 6092               Blk 628 Ang Mo Kio Ave 4
    ## 6093               Blk 628 Ang Mo Kio Ave 4
    ## 6094               Blk 628 Ang Mo Kio Ave 4
    ## 6095               Blk 628 Ang Mo Kio Ave 4
    ## 6096               Blk 628 Ang Mo Kio Ave 4
    ## 6097               Blk 628 Ang Mo Kio Ave 4
    ## 6098               Blk 628 Ang Mo Kio Ave 4
    ## 6099               Blk 628 Ang Mo Kio Ave 4
    ## 6100               Blk 628 Ang Mo Kio Ave 4
    ## 6101               Blk 628 Ang Mo Kio Ave 4
    ## 6102               Blk 628 Ang Mo Kio Ave 4
    ## 6103               Blk 628 Ang Mo Kio Ave 4
    ## 6104               Blk 628 Ang Mo Kio Ave 4
    ## 6105               Blk 628 Ang Mo Kio Ave 4
    ## 6106               Blk 628 Ang Mo Kio Ave 4
    ## 6107               Blk 628 Ang Mo Kio Ave 4
    ## 6108               Blk 628 Ang Mo Kio Ave 4
    ## 6109               Blk 628 Ang Mo Kio Ave 4
    ## 6110           Blk 630 Bedok Reservoir Road
    ## 6111           Blk 630 Bedok Reservoir Road
    ## 6112           Blk 630 Bedok Reservoir Road
    ## 6113           Blk 630 Bedok Reservoir Road
    ## 6114           Blk 630 Bedok Reservoir Road
    ## 6115           Blk 630 Bedok Reservoir Road
    ## 6116           Blk 630 Bedok Reservoir Road
    ## 6117           Blk 630 Bedok Reservoir Road
    ## 6118           Blk 630 Bedok Reservoir Road
    ## 6119           Blk 630 Bedok Reservoir Road
    ## 6120           Blk 630 Bedok Reservoir Road
    ## 6121           Blk 630 Bedok Reservoir Road
    ## 6122           Blk 630 Bedok Reservoir Road
    ## 6123           Blk 630 Bedok Reservoir Road
    ## 6124           Blk 630 Bedok Reservoir Road
    ## 6125           Blk 630 Bedok Reservoir Road
    ## 6126           Blk 630 Bedok Reservoir Road
    ## 6127           Blk 630 Bedok Reservoir Road
    ## 6128           Blk 630 Bedok Reservoir Road
    ## 6129           Blk 630 Bedok Reservoir Road
    ## 6130           Blk 630 Bedok Reservoir Road
    ## 6131           Blk 630 Bedok Reservoir Road
    ## 6132           Blk 630 Bedok Reservoir Road
    ## 6133           Blk 630 Bedok Reservoir Road
    ## 6134           Blk 630 Bedok Reservoir Road
    ## 6135           Blk 630 Bedok Reservoir Road
    ## 6136           Blk 630 Bedok Reservoir Road
    ## 6137           Blk 630 Bedok Reservoir Road
    ## 6138           Blk 630 Bedok Reservoir Road
    ## 6139           Blk 630 Bedok Reservoir Road
    ## 6140           Blk 630 Bedok Reservoir Road
    ## 6141           Blk 630 Bedok Reservoir Road
    ## 6142           Blk 630 Bedok Reservoir Road
    ## 6143                   Blk 69 Geylang Bahru
    ## 6144                   Blk 69 Geylang Bahru
    ## 6145                   Blk 69 Geylang Bahru
    ## 6146                   Blk 69 Geylang Bahru
    ## 6147                   Blk 69 Geylang Bahru
    ## 6148                   Blk 69 Geylang Bahru
    ## 6149                   Blk 69 Geylang Bahru
    ## 6150                   Blk 69 Geylang Bahru
    ## 6151                   Blk 69 Geylang Bahru
    ## 6152                   Blk 69 Geylang Bahru
    ## 6153                   Blk 69 Geylang Bahru
    ## 6154                   Blk 69 Geylang Bahru
    ## 6155                   Blk 69 Geylang Bahru
    ## 6156                   Blk 69 Geylang Bahru
    ## 6157                   Blk 69 Geylang Bahru
    ## 6158                   Blk 69 Geylang Bahru
    ## 6159                   Blk 69 Geylang Bahru
    ## 6160                   Blk 69 Geylang Bahru
    ## 6161                   Blk 69 Geylang Bahru
    ## 6162                   Blk 69 Geylang Bahru
    ## 6163                   Blk 69 Geylang Bahru
    ## 6164                   Blk 69 Geylang Bahru
    ## 6165                   Blk 69 Geylang Bahru
    ## 6166                   Blk 69 Geylang Bahru
    ## 6167                   Blk 69 Geylang Bahru
    ## 6168                   Blk 69 Geylang Bahru
    ## 6169                   Blk 69 Geylang Bahru
    ## 6170                   Blk 69 Geylang Bahru
    ## 6171                   Blk 69 Geylang Bahru
    ## 6172                   Blk 69 Geylang Bahru
    ## 6173                   Blk 69 Geylang Bahru
    ## 6174                   Blk 69 Geylang Bahru
    ## 6175                     Blk 7 Empress Road
    ## 6176                     Blk 7 Empress Road
    ## 6177                     Blk 7 Empress Road
    ## 6178                     Blk 7 Empress Road
    ## 6179                     Blk 7 Empress Road
    ## 6180                     Blk 7 Empress Road
    ## 6181                     Blk 7 Empress Road
    ## 6182                     Blk 7 Empress Road
    ## 6183                     Blk 7 Empress Road
    ## 6184                     Blk 7 Empress Road
    ## 6185                     Blk 7 Empress Road
    ## 6186                     Blk 7 Empress Road
    ## 6187                     Blk 7 Empress Road
    ## 6188                     Blk 7 Empress Road
    ## 6189                     Blk 7 Empress Road
    ## 6190                     Blk 7 Empress Road
    ## 6191                     Blk 7 Empress Road
    ## 6192                     Blk 7 Empress Road
    ## 6193                     Blk 7 Empress Road
    ## 6194                     Blk 7 Empress Road
    ## 6195                     Blk 7 Empress Road
    ## 6196                     Blk 7 Empress Road
    ## 6197                     Blk 7 Empress Road
    ## 6198                     Blk 7 Empress Road
    ## 6199                     Blk 7 Empress Road
    ## 6200                     Blk 7 Empress Road
    ## 6201                     Blk 7 Empress Road
    ## 6202                     Blk 7 Empress Road
    ## 6203                     Blk 7 Empress Road
    ## 6204                     Blk 7 Empress Road
    ## 6205                     Blk 7 Empress Road
    ## 6206               Blk 724 Ang Mo Kio Ave 6
    ## 6207               Blk 724 Ang Mo Kio Ave 6
    ## 6208               Blk 724 Ang Mo Kio Ave 6
    ## 6209               Blk 724 Ang Mo Kio Ave 6
    ## 6210               Blk 724 Ang Mo Kio Ave 6
    ## 6211               Blk 724 Ang Mo Kio Ave 6
    ## 6212               Blk 724 Ang Mo Kio Ave 6
    ## 6213               Blk 724 Ang Mo Kio Ave 6
    ## 6214               Blk 724 Ang Mo Kio Ave 6
    ## 6215               Blk 724 Ang Mo Kio Ave 6
    ## 6216               Blk 724 Ang Mo Kio Ave 6
    ## 6217               Blk 724 Ang Mo Kio Ave 6
    ## 6218               Blk 724 Ang Mo Kio Ave 6
    ## 6219               Blk 724 Ang Mo Kio Ave 6
    ## 6220               Blk 724 Ang Mo Kio Ave 6
    ## 6221               Blk 724 Ang Mo Kio Ave 6
    ## 6222               Blk 724 Ang Mo Kio Ave 6
    ## 6223               Blk 724 Ang Mo Kio Ave 6
    ## 6224               Blk 724 Ang Mo Kio Ave 6
    ## 6225               Blk 724 Ang Mo Kio Ave 6
    ## 6226               Blk 724 Ang Mo Kio Ave 6
    ## 6227               Blk 724 Ang Mo Kio Ave 6
    ## 6228               Blk 724 Ang Mo Kio Ave 6
    ## 6229               Blk 724 Ang Mo Kio Ave 6
    ## 6230               Blk 724 Ang Mo Kio Ave 6
    ## 6231               Blk 724 Ang Mo Kio Ave 6
    ## 6232               Blk 724 Ang Mo Kio Ave 6
    ## 6233               Blk 724 Ang Mo Kio Ave 6
    ## 6234               Blk 724 Ang Mo Kio Ave 6
    ## 6235               Blk 724 Ang Mo Kio Ave 6
    ## 6236         Blk 726 Clementi West Street 2
    ## 6237         Blk 726 Clementi West Street 2
    ## 6238         Blk 726 Clementi West Street 2
    ## 6239         Blk 726 Clementi West Street 2
    ## 6240         Blk 726 Clementi West Street 2
    ## 6241         Blk 726 Clementi West Street 2
    ## 6242         Blk 726 Clementi West Street 2
    ## 6243         Blk 726 Clementi West Street 2
    ## 6244         Blk 726 Clementi West Street 2
    ## 6245         Blk 726 Clementi West Street 2
    ## 6246         Blk 726 Clementi West Street 2
    ## 6247         Blk 726 Clementi West Street 2
    ## 6248         Blk 726 Clementi West Street 2
    ## 6249         Blk 726 Clementi West Street 2
    ## 6250         Blk 726 Clementi West Street 2
    ## 6251         Blk 726 Clementi West Street 2
    ## 6252         Blk 726 Clementi West Street 2
    ## 6253         Blk 726 Clementi West Street 2
    ## 6254         Blk 726 Clementi West Street 2
    ## 6255         Blk 726 Clementi West Street 2
    ## 6256         Blk 726 Clementi West Street 2
    ## 6257         Blk 726 Clementi West Street 2
    ## 6258         Blk 726 Clementi West Street 2
    ## 6259         Blk 726 Clementi West Street 2
    ## 6260         Blk 726 Clementi West Street 2
    ## 6261         Blk 726 Clementi West Street 2
    ## 6262         Blk 726 Clementi West Street 2
    ## 6263         Blk 726 Clementi West Street 2
    ## 6264         Blk 726 Clementi West Street 2
    ## 6265              Blk 74 Toa Payoh Lorong 4
    ## 6266              Blk 74 Toa Payoh Lorong 4
    ## 6267              Blk 74 Toa Payoh Lorong 4
    ## 6268              Blk 74 Toa Payoh Lorong 4
    ## 6269              Blk 74 Toa Payoh Lorong 4
    ## 6270              Blk 74 Toa Payoh Lorong 4
    ## 6271              Blk 74 Toa Payoh Lorong 4
    ## 6272              Blk 74 Toa Payoh Lorong 4
    ## 6273              Blk 74 Toa Payoh Lorong 4
    ## 6274              Blk 74 Toa Payoh Lorong 4
    ## 6275              Blk 74 Toa Payoh Lorong 4
    ## 6276              Blk 74 Toa Payoh Lorong 4
    ## 6277              Blk 74 Toa Payoh Lorong 4
    ## 6278              Blk 74 Toa Payoh Lorong 4
    ## 6279              Blk 74 Toa Payoh Lorong 4
    ## 6280              Blk 74 Toa Payoh Lorong 4
    ## 6281              Blk 74 Toa Payoh Lorong 4
    ## 6282              Blk 74 Toa Payoh Lorong 4
    ## 6283              Blk 74 Toa Payoh Lorong 4
    ## 6284              Blk 74 Toa Payoh Lorong 4
    ## 6285              Blk 74 Toa Payoh Lorong 4
    ## 6286              Blk 74 Toa Payoh Lorong 4
    ## 6287              Blk 74 Toa Payoh Lorong 4
    ## 6288              Blk 74 Toa Payoh Lorong 4
    ## 6289              Blk 74 Toa Payoh Lorong 4
    ## 6290              Blk 74 Toa Payoh Lorong 4
    ## 6291              Blk 74 Toa Payoh Lorong 4
    ## 6292              Blk 74 Toa Payoh Lorong 4
    ## 6293              Market Street Food Centre
    ## 6294              Market Street Food Centre
    ## 6295              Market Street Food Centre
    ## 6296              Market Street Food Centre
    ## 6297              Market Street Food Centre
    ## 6298              Market Street Food Centre
    ## 6299              Market Street Food Centre
    ## 6300              Market Street Food Centre
    ## 6301              Market Street Food Centre
    ## 6302              Market Street Food Centre
    ## 6303              Market Street Food Centre
    ## 6304              Market Street Food Centre
    ## 6305              Market Street Food Centre
    ## 6306              Market Street Food Centre
    ## 6307              Market Street Food Centre
    ## 6308              Market Street Food Centre
    ## 6309              Market Street Food Centre
    ## 6310              Market Street Food Centre
    ## 6311              Market Street Food Centre
    ## 6312              Market Street Food Centre
    ## 6313              Market Street Food Centre
    ## 6314              Market Street Food Centre
    ## 6315              Market Street Food Centre
    ## 6316              Market Street Food Centre
    ## 6317              Market Street Food Centre
    ## 6318              Market Street Food Centre
    ## 6319              Market Street Food Centre
    ## 6320                    Maxwell Food Centre
    ## 6321                    Maxwell Food Centre
    ## 6322                    Maxwell Food Centre
    ## 6323                    Maxwell Food Centre
    ## 6324                    Maxwell Food Centre
    ## 6325                    Maxwell Food Centre
    ## 6326                    Maxwell Food Centre
    ## 6327                    Maxwell Food Centre
    ## 6328                    Maxwell Food Centre
    ## 6329                    Maxwell Food Centre
    ## 6330                    Maxwell Food Centre
    ## 6331                    Maxwell Food Centre
    ## 6332                    Maxwell Food Centre
    ## 6333                    Maxwell Food Centre
    ## 6334                    Maxwell Food Centre
    ## 6335                    Maxwell Food Centre
    ## 6336                    Maxwell Food Centre
    ## 6337                    Maxwell Food Centre
    ## 6338                    Maxwell Food Centre
    ## 6339                    Maxwell Food Centre
    ## 6340                    Maxwell Food Centre
    ## 6341                    Maxwell Food Centre
    ## 6342                    Maxwell Food Centre
    ## 6343                    Maxwell Food Centre
    ## 6344                    Maxwell Food Centre
    ## 6345                    Maxwell Food Centre
    ## 6346                     Newton Food Centre
    ## 6347                     Newton Food Centre
    ## 6348                     Newton Food Centre
    ## 6349                     Newton Food Centre
    ## 6350                     Newton Food Centre
    ## 6351                     Newton Food Centre
    ## 6352                     Newton Food Centre
    ## 6353                     Newton Food Centre
    ## 6354                     Newton Food Centre
    ## 6355                     Newton Food Centre
    ## 6356                     Newton Food Centre
    ## 6357                     Newton Food Centre
    ## 6358                     Newton Food Centre
    ## 6359                     Newton Food Centre
    ## 6360                     Newton Food Centre
    ## 6361                     Newton Food Centre
    ## 6362                     Newton Food Centre
    ## 6363                     Newton Food Centre
    ## 6364                     Newton Food Centre
    ## 6365                     Newton Food Centre
    ## 6366                     Newton Food Centre
    ## 6367                     Newton Food Centre
    ## 6368                     Newton Food Centre
    ## 6369                     Newton Food Centre
    ## 6370                     Newton Food Centre
    ## 6371 North Bridge Road Market & Food Centre
    ## 6372 North Bridge Road Market & Food Centre
    ## 6373 North Bridge Road Market & Food Centre
    ## 6374 North Bridge Road Market & Food Centre
    ## 6375 North Bridge Road Market & Food Centre
    ## 6376 North Bridge Road Market & Food Centre
    ## 6377 North Bridge Road Market & Food Centre
    ## 6378 North Bridge Road Market & Food Centre
    ## 6379 North Bridge Road Market & Food Centre
    ## 6380 North Bridge Road Market & Food Centre
    ## 6381 North Bridge Road Market & Food Centre
    ## 6382 North Bridge Road Market & Food Centre
    ## 6383 North Bridge Road Market & Food Centre
    ## 6384 North Bridge Road Market & Food Centre
    ## 6385 North Bridge Road Market & Food Centre
    ## 6386 North Bridge Road Market & Food Centre
    ## 6387 North Bridge Road Market & Food Centre
    ## 6388 North Bridge Road Market & Food Centre
    ## 6389 North Bridge Road Market & Food Centre
    ## 6390 North Bridge Road Market & Food Centre
    ## 6391 North Bridge Road Market & Food Centre
    ## 6392 North Bridge Road Market & Food Centre
    ## 6393 North Bridge Road Market & Food Centre
    ## 6394 North Bridge Road Market & Food Centre
    ## 6395              Pasir Panjang Food Centre
    ## 6396              Pasir Panjang Food Centre
    ## 6397              Pasir Panjang Food Centre
    ## 6398              Pasir Panjang Food Centre
    ## 6399              Pasir Panjang Food Centre
    ## 6400              Pasir Panjang Food Centre
    ## 6401              Pasir Panjang Food Centre
    ## 6402              Pasir Panjang Food Centre
    ## 6403              Pasir Panjang Food Centre
    ## 6404              Pasir Panjang Food Centre
    ## 6405              Pasir Panjang Food Centre
    ## 6406              Pasir Panjang Food Centre
    ## 6407              Pasir Panjang Food Centre
    ## 6408              Pasir Panjang Food Centre
    ## 6409              Pasir Panjang Food Centre
    ## 6410              Pasir Panjang Food Centre
    ## 6411              Pasir Panjang Food Centre
    ## 6412              Pasir Panjang Food Centre
    ## 6413              Pasir Panjang Food Centre
    ## 6414              Pasir Panjang Food Centre
    ## 6415              Pasir Panjang Food Centre
    ## 6416              Pasir Panjang Food Centre
    ## 6417              Pasir Panjang Food Centre
    ## 6418           Pek Kio Market & Food Centre
    ## 6419           Pek Kio Market & Food Centre
    ## 6420           Pek Kio Market & Food Centre
    ## 6421           Pek Kio Market & Food Centre
    ## 6422           Pek Kio Market & Food Centre
    ## 6423           Pek Kio Market & Food Centre
    ## 6424           Pek Kio Market & Food Centre
    ## 6425           Pek Kio Market & Food Centre
    ## 6426           Pek Kio Market & Food Centre
    ## 6427           Pek Kio Market & Food Centre
    ## 6428           Pek Kio Market & Food Centre
    ## 6429           Pek Kio Market & Food Centre
    ## 6430           Pek Kio Market & Food Centre
    ## 6431           Pek Kio Market & Food Centre
    ## 6432           Pek Kio Market & Food Centre
    ## 6433           Pek Kio Market & Food Centre
    ## 6434           Pek Kio Market & Food Centre
    ## 6435           Pek Kio Market & Food Centre
    ## 6436           Pek Kio Market & Food Centre
    ## 6437           Pek Kio Market & Food Centre
    ## 6438           Pek Kio Market & Food Centre
    ## 6439           Pek Kio Market & Food Centre
    ## 6440              People's Park Food Centre
    ## 6441              People's Park Food Centre
    ## 6442              People's Park Food Centre
    ## 6443              People's Park Food Centre
    ## 6444              People's Park Food Centre
    ## 6445              People's Park Food Centre
    ## 6446              People's Park Food Centre
    ## 6447              People's Park Food Centre
    ## 6448              People's Park Food Centre
    ## 6449              People's Park Food Centre
    ## 6450              People's Park Food Centre
    ## 6451              People's Park Food Centre
    ## 6452              People's Park Food Centre
    ## 6453              People's Park Food Centre
    ## 6454              People's Park Food Centre
    ## 6455              People's Park Food Centre
    ## 6456              People's Park Food Centre
    ## 6457              People's Park Food Centre
    ## 6458              People's Park Food Centre
    ## 6459              People's Park Food Centre
    ## 6460              People's Park Food Centre
    ## 6461            Sembawang Hills Food Centre
    ## 6462            Sembawang Hills Food Centre
    ## 6463            Sembawang Hills Food Centre
    ## 6464            Sembawang Hills Food Centre
    ## 6465            Sembawang Hills Food Centre
    ## 6466            Sembawang Hills Food Centre
    ## 6467            Sembawang Hills Food Centre
    ## 6468            Sembawang Hills Food Centre
    ## 6469            Sembawang Hills Food Centre
    ## 6470            Sembawang Hills Food Centre
    ## 6471            Sembawang Hills Food Centre
    ## 6472            Sembawang Hills Food Centre
    ## 6473            Sembawang Hills Food Centre
    ## 6474            Sembawang Hills Food Centre
    ## 6475            Sembawang Hills Food Centre
    ## 6476            Sembawang Hills Food Centre
    ## 6477            Sembawang Hills Food Centre
    ## 6478            Sembawang Hills Food Centre
    ## 6479            Sembawang Hills Food Centre
    ## 6480            Sembawang Hills Food Centre
    ## 6481                Serangoon Garden Market
    ## 6482                Serangoon Garden Market
    ## 6483                Serangoon Garden Market
    ## 6484                Serangoon Garden Market
    ## 6485                Serangoon Garden Market
    ## 6486                Serangoon Garden Market
    ## 6487                Serangoon Garden Market
    ## 6488                Serangoon Garden Market
    ## 6489                Serangoon Garden Market
    ## 6490                Serangoon Garden Market
    ## 6491                Serangoon Garden Market
    ## 6492                Serangoon Garden Market
    ## 6493                Serangoon Garden Market
    ## 6494                Serangoon Garden Market
    ## 6495                Serangoon Garden Market
    ## 6496                Serangoon Garden Market
    ## 6497                Serangoon Garden Market
    ## 6498                Serangoon Garden Market
    ## 6499                Serangoon Garden Market
    ## 6500      Taman Jurong Market & Food Centre
    ## 6501      Taman Jurong Market & Food Centre
    ## 6502      Taman Jurong Market & Food Centre
    ## 6503      Taman Jurong Market & Food Centre
    ## 6504      Taman Jurong Market & Food Centre
    ## 6505      Taman Jurong Market & Food Centre
    ## 6506      Taman Jurong Market & Food Centre
    ## 6507      Taman Jurong Market & Food Centre
    ## 6508      Taman Jurong Market & Food Centre
    ## 6509      Taman Jurong Market & Food Centre
    ## 6510      Taman Jurong Market & Food Centre
    ## 6511      Taman Jurong Market & Food Centre
    ## 6512      Taman Jurong Market & Food Centre
    ## 6513      Taman Jurong Market & Food Centre
    ## 6514      Taman Jurong Market & Food Centre
    ## 6515      Taman Jurong Market & Food Centre
    ## 6516      Taman Jurong Market & Food Centre
    ## 6517      Taman Jurong Market & Food Centre
    ## 6518                    Tanglin Halt Market
    ## 6519                    Tanglin Halt Market
    ## 6520                    Tanglin Halt Market
    ## 6521                    Tanglin Halt Market
    ## 6522                    Tanglin Halt Market
    ## 6523                    Tanglin Halt Market
    ## 6524                    Tanglin Halt Market
    ## 6525                    Tanglin Halt Market
    ## 6526                    Tanglin Halt Market
    ## 6527                    Tanglin Halt Market
    ## 6528                    Tanglin Halt Market
    ## 6529                    Tanglin Halt Market
    ## 6530                    Tanglin Halt Market
    ## 6531                    Tanglin Halt Market
    ## 6532                    Tanglin Halt Market
    ## 6533                    Tanglin Halt Market
    ## 6534                    Tanglin Halt Market
    ## 6535                           Tekka Market
    ## 6536                           Tekka Market
    ## 6537                           Tekka Market
    ## 6538                           Tekka Market
    ## 6539                           Tekka Market
    ## 6540                           Tekka Market
    ## 6541                           Tekka Market
    ## 6542                           Tekka Market
    ## 6543                           Tekka Market
    ## 6544                           Tekka Market
    ## 6545                           Tekka Market
    ## 6546                           Tekka Market
    ## 6547                           Tekka Market
    ## 6548                           Tekka Market
    ## 6549                           Tekka Market
    ## 6550                           Tekka Market
    ## 6551                     Tiong Bahru Market
    ## 6552                     Tiong Bahru Market
    ## 6553                     Tiong Bahru Market
    ## 6554                     Tiong Bahru Market
    ## 6555                     Tiong Bahru Market
    ## 6556                     Tiong Bahru Market
    ## 6557                     Tiong Bahru Market
    ## 6558                     Tiong Bahru Market
    ## 6559                     Tiong Bahru Market
    ## 6560                     Tiong Bahru Market
    ## 6561                     Tiong Bahru Market
    ## 6562                     Tiong Bahru Market
    ## 6563                     Tiong Bahru Market
    ## 6564                     Tiong Bahru Market
    ## 6565                     Tiong Bahru Market
    ## 6566             Zion Riverside Food Centre
    ## 6567             Zion Riverside Food Centre
    ## 6568             Zion Riverside Food Centre
    ## 6569             Zion Riverside Food Centre
    ## 6570             Zion Riverside Food Centre
    ## 6571             Zion Riverside Food Centre
    ## 6572             Zion Riverside Food Centre
    ## 6573             Zion Riverside Food Centre
    ## 6574             Zion Riverside Food Centre
    ## 6575             Zion Riverside Food Centre
    ## 6576             Zion Riverside Food Centre
    ## 6577             Zion Riverside Food Centre
    ## 6578             Zion Riverside Food Centre
    ## 6579             Zion Riverside Food Centre
    ## 6580              Blk 75 Toa Payoh Lorong 5
    ## 6581              Blk 75 Toa Payoh Lorong 5
    ## 6582              Blk 75 Toa Payoh Lorong 5
    ## 6583              Blk 75 Toa Payoh Lorong 5
    ## 6584              Blk 75 Toa Payoh Lorong 5
    ## 6585              Blk 75 Toa Payoh Lorong 5
    ## 6586              Blk 75 Toa Payoh Lorong 5
    ## 6587              Blk 75 Toa Payoh Lorong 5
    ## 6588              Blk 75 Toa Payoh Lorong 5
    ## 6589              Blk 75 Toa Payoh Lorong 5
    ## 6590              Blk 75 Toa Payoh Lorong 5
    ## 6591              Blk 75 Toa Payoh Lorong 5
    ## 6592              Blk 75 Toa Payoh Lorong 5
    ## 6593                    Blk 79 Redhill Lane
    ## 6594                    Blk 79 Redhill Lane
    ## 6595                    Blk 79 Redhill Lane
    ## 6596                    Blk 79 Redhill Lane
    ## 6597                    Blk 79 Redhill Lane
    ## 6598                    Blk 79 Redhill Lane
    ## 6599                    Blk 79 Redhill Lane
    ## 6600                    Blk 79 Redhill Lane
    ## 6601                    Blk 79 Redhill Lane
    ## 6602                    Blk 79 Redhill Lane
    ## 6603                    Blk 79 Redhill Lane
    ## 6604                    Blk 79 Redhill Lane
    ## 6605             Blk 79 Telok Blangah Drive
    ## 6606             Blk 79 Telok Blangah Drive
    ## 6607             Blk 79 Telok Blangah Drive
    ## 6608             Blk 79 Telok Blangah Drive
    ## 6609             Blk 79 Telok Blangah Drive
    ## 6610             Blk 79 Telok Blangah Drive
    ## 6611             Blk 79 Telok Blangah Drive
    ## 6612             Blk 79 Telok Blangah Drive
    ## 6613             Blk 79 Telok Blangah Drive
    ## 6614             Blk 79 Telok Blangah Drive
    ## 6615             Blk 79 Telok Blangah Drive
    ## 6616                    Blk 80 Circuit Road
    ## 6617                    Blk 80 Circuit Road
    ## 6618                    Blk 80 Circuit Road
    ## 6619                    Blk 80 Circuit Road
    ## 6620                    Blk 80 Circuit Road
    ## 6621                    Blk 80 Circuit Road
    ## 6622                    Blk 80 Circuit Road
    ## 6623                    Blk 80 Circuit Road
    ## 6624                    Blk 80 Circuit Road
    ## 6625                    Blk 80 Circuit Road
    ## 6626             Blk 82 Telok Blangah Drive
    ## 6627             Blk 82 Telok Blangah Drive
    ## 6628             Blk 82 Telok Blangah Drive
    ## 6629             Blk 82 Telok Blangah Drive
    ## 6630             Blk 82 Telok Blangah Drive
    ## 6631             Blk 82 Telok Blangah Drive
    ## 6632             Blk 82 Telok Blangah Drive
    ## 6633             Blk 82 Telok Blangah Drive
    ## 6634             Blk 82 Telok Blangah Drive
    ## 6635           Blk 84 Marine Parade Central
    ## 6636           Blk 84 Marine Parade Central
    ## 6637           Blk 84 Marine Parade Central
    ## 6638           Blk 84 Marine Parade Central
    ## 6639           Blk 84 Marine Parade Central
    ## 6640           Blk 84 Marine Parade Central
    ## 6641           Blk 84 Marine Parade Central
    ## 6642           Blk 84 Marine Parade Central
    ## 6643            Blk 85 Bedok North Street 4
    ## 6644            Blk 85 Bedok North Street 4
    ## 6645            Blk 85 Bedok North Street 4
    ## 6646            Blk 85 Bedok North Street 4
    ## 6647            Blk 85 Bedok North Street 4
    ## 6648            Blk 85 Bedok North Street 4
    ## 6649            Blk 85 Bedok North Street 4
    ## 6650                    Blk 85 Redhill Lane
    ## 6651                    Blk 85 Redhill Lane
    ## 6652                    Blk 85 Redhill Lane
    ## 6653                    Blk 85 Redhill Lane
    ## 6654                    Blk 85 Redhill Lane
    ## 6655                    Blk 85 Redhill Lane
    ## 6656                    Blk 89 Circuit Road
    ## 6657                    Blk 89 Circuit Road
    ## 6658                    Blk 89 Circuit Road
    ## 6659                    Blk 89 Circuit Road
    ## 6660                    Blk 89 Circuit Road
    ## 6661                   Blk 90 Whampoa Drive
    ## 6662                   Blk 90 Whampoa Drive
    ## 6663                   Blk 90 Whampoa Drive
    ## 6664                   Blk 90 Whampoa Drive
    ## 6665              Blk 93 Toa Payoh Lorong 4
    ## 6666              Blk 93 Toa Payoh Lorong 4
    ## 6667              Blk 93 Toa Payoh Lorong 4
    ## 6668                   Blks 13/14 Haig Road
    ## 6669                   Blks 13/14 Haig Road
    ## 6670          Blks 160/162 Ang Mo Kio Ave 4
    ##                                     hawker2        dist
    ## 1                 Blks 20/21 Marsiling Lane 16106.57344
    ## 2                Blks 221A/B Boon Lay Place 10722.32517
    ## 3                  Blks 22A/B Havelock Road  3764.47329
    ## 4                  Blks 79/79A Circuit Road 10141.45466
    ## 5                  Blks 91/92 Whampoa Drive  6780.95570
    ## 6                        Bukit Timah Market  5016.36725
    ## 7                          Chinatown Market  5399.46800
    ## 8                   Chomp Chomp Food Centre 10453.64880
    ## 9           Chong Pang Market & Food Centre 14960.86381
    ## 10                       Dunman Food Centre 11618.27103
    ## 11           East Coast Lagoon Food Village 15282.03241
    ## 12                     Geylang Serai Market 11325.25975
    ## 13                  Golden Mile Food Centre  7354.38642
    ## 14     Holland Village Market & Food Centre  1282.45456
    ## 15            Hong Lim Market & Food Centre  5570.32498
    ## 16                    Kallang Estate Market  9636.88044
    ## 17               Kovan Market & Food Centre 11801.89840
    ## 18           Blks 2 & 3 Changi Village Road 23380.71892
    ## 19             Commonwealth Crescent Market   837.84483
    ## 20      ABC Brickworks Market & Food Centre  1814.61572
    ## 21                         Adam Food Centre  3252.43517
    ## 22       Albert Centre Market & Food Centre  6271.32737
    ## 23            Alexandra Village Food Centre  1655.99500
    ## 24                  Amoy Street Food Centre  5877.82153
    ## 25                        Bedok Food Centre 17687.16285
    ## 26                      Beo Crescent Market  3495.17753
    ## 27                       Berseh Food Centre  6617.67251
    ## 28                        Blk 1 Jalan Kukoh  4851.18480
    ## 29                    Blk 105 Hougang Ave 1 11879.25677
    ## 30            Blk 11 Telok Blangah Crescent  3383.31693
    ## 31                Blk 112 Jalan Bukit Merah  3815.16347
    ## 32                 Blk 115 Bukit Merah View  3115.49760
    ## 33                   Blk 117 Aljunied Ave 2 10187.33147
    ## 34               Blk 127 Toa Payoh Lorong 1  6723.25498
    ## 35               Blk 137 Tampines Street 11 17103.03197
    ## 36                    Blk 159 Mei Chin Road   908.91158
    ## 37                  Blk 16 Bedok South Road 15492.98365
    ## 38              Blk 163 Bukit Merah Central  2768.50710
    ## 39              Blk 17 Upper Boon Keng Road  8379.99758
    ## 40                     Blk 20 Ghim Moh Road  1645.80748
    ## 41           Blk 208B New Upper Changi Road 15012.49275
    ## 42               Blk 210 Toa Payoh Lorong 8  7724.46985
    ## 43             Blk 216 Bedok North Street 1 15363.30503
    ## 44                Blk 22 Toa Payoh Lorong 7  7666.38942
    ## 45                Blk 226D Ang Mo Kio Ave 1  8728.73274
    ## 46            Blk 226H Ang Mo Kio Street 22  8812.76709
    ## 47            Blk 254 Jurong East Street 24  8252.77112
    ## 48                    Blk 29 Bendemeer Road  7564.08288
    ## 49                      Blk 320 Shunfu Road  7240.57539
    ## 50                 Blk 341 Ang Mo Kio Ave 1  9062.58567
    ## 51                Blk 347 Jurong East Ave 1  8936.37484
    ## 52                   Blk 353 Clementi Ave 2  3417.71654
    ## 53                Blk 36 Telok Blangah Rise  4032.25779
    ## 54               Blk 37A Teban Gardens Road  6550.27242
    ## 55                Blk 409 Ang Mo Kio Ave 10  9463.34916
    ## 56                     Blk 44 Holland Drive  1089.03887
    ## 57                   Blk 448 Clementi Ave 3  4004.16378
    ## 58               Blk 453A Ang Mo Kio Ave 10  9988.38001
    ## 59                        Blk 49 Sims Place  9275.84504
    ## 60                    Blk 4A Eunos Crescent 12057.20877
    ## 61                        Blk 4A Jalan Batu  9582.94156
    ## 62             Blk 4A Woodlands Centre Road 15908.55426
    ## 63                 Blk 502 West Coast Drive  4513.49840
    ## 64                 Blk 503 West Coast Drive  4448.48509
    ## 65            Blk 505 Jurong West Street 52 10421.98021
    ## 66                   Blk 50A Marine Terrace 13136.80765
    ## 67                  Blk 51 Old Airport Road  9834.31669
    ## 68             Blk 511 Bedok North Street 3 15229.47155
    ## 69   Bukit Panjang Hawker Centre and Market  9063.91915
    ## 70           Our Tampines Hub Hawker Centre 16883.87485
    ## 71          Kampung Admiralty Hawker Centre 15507.94245
    ## 72                     Yishun Hawker Centre 14804.90535
    ## 73     Jurong West Hawker Centre and Market 12088.42504
    ## 74          Pasir Ris Central Hawker Centre 18945.02217
    ## 75                     Dawson Hawker Centre   800.06494
    ## 76        Woodlands Street 12 Hawker Centre 14934.07898
    ## 77                Blk 527 Ang Mo Kio Ave 10 10244.27680
    ## 78             Blk 538 Bedok North Street 3 14568.51695
    ## 79             Blk 58 New Upper Changi Road 16171.19589
    ## 80                Blk 6 Tanjong Pagar Plaza  5650.74124
    ## 81                 Blk 628 Ang Mo Kio Ave 4 10175.14137
    ## 82             Blk 630 Bedok Reservoir Road 13448.05312
    ## 83                     Blk 69 Geylang Bahru  8379.77076
    ## 84                       Blk 7 Empress Road  2021.84326
    ## 85                 Blk 724 Ang Mo Kio Ave 6  9671.88514
    ## 86           Blk 726 Clementi West Street 2  3767.56703
    ## 87                Blk 74 Toa Payoh Lorong 4  7149.72876
    ## 88                Market Street Food Centre  6062.92115
    ## 89                      Maxwell Food Centre  5657.06335
    ## 90                       Newton Food Centre  4830.90522
    ## 91   North Bridge Road Market & Food Centre  7377.34420
    ## 92                Pasir Panjang Food Centre  2729.11106
    ## 93             Pek Kio Market & Food Centre  6103.26028
    ## 94                People's Park Food Centre  5234.65468
    ## 95              Sembawang Hills Food Centre  8750.16848
    ## 96                  Serangoon Garden Market 10389.68313
    ## 97        Taman Jurong Market & Food Centre  9331.76946
    ## 98                      Tanglin Halt Market   122.18234
    ## 99                             Tekka Market  5903.88886
    ## 100                      Tiong Bahru Market  4163.48086
    ## 101              Zion Riverside Food Centre  3796.07039
    ## 102               Blk 75 Toa Payoh Lorong 5  7327.35058
    ## 103                     Blk 79 Redhill Lane  2633.88191
    ## 104              Blk 79 Telok Blangah Drive  3108.67988
    ## 105                     Blk 80 Circuit Road 10408.84498
    ## 106              Blk 82 Telok Blangah Drive  3064.93751
    ## 107            Blk 84 Marine Parade Central 12074.31151
    ## 108             Blk 85 Bedok North Street 4 16077.11536
    ## 109                     Blk 85 Redhill Lane  2657.30473
    ## 110                     Blk 89 Circuit Road 10093.22682
    ## 111                    Blk 90 Whampoa Drive  6870.93148
    ## 112               Blk 93 Toa Payoh Lorong 4  7169.09385
    ## 113                    Blks 13/14 Haig Road 11009.87100
    ## 114           Blks 160/162 Ang Mo Kio Ave 4  9462.05515
    ## 115                   Ci Yuan Hawker Centre 12640.98623
    ## 116              Blks 221A/B Boon Lay Place 13052.96471
    ## 117                Blks 22A/B Havelock Road 18199.77368
    ## 118                Blks 79/79A Circuit Road 17681.65116
    ## 119                Blks 91/92 Whampoa Drive 15830.06986
    ## 120                      Bukit Timah Market 11556.51539
    ## 121                        Chinatown Market 19323.29798
    ## 122                 Chomp Chomp Food Centre 13281.90411
    ## 123         Chong Pang Market & Food Centre  5875.86077
    ## 124                      Dunman Food Centre 20332.86739
    ## 125          East Coast Lagoon Food Village 23207.66910
    ## 126                    Geylang Serai Market 19469.83258
    ## 127                 Golden Mile Food Centre 18334.57734
    ## 128    Holland Village Market & Food Centre 14834.33361
    ## 129           Hong Lim Market & Food Centre 19131.13638
    ## 130                   Kallang Estate Market 19218.92145
    ## 131              Kovan Market & Food Centre 15309.45805
    ## 132          Blks 2 & 3 Changi Village Road 24253.21202
    ## 133            Commonwealth Crescent Market 15350.26802
    ## 134     ABC Brickworks Market & Food Centre 17690.41835
    ## 135                        Adam Food Centre 13865.28834
    ## 136      Albert Centre Market & Food Centre 17973.71798
    ## 137           Alexandra Village Food Centre 17687.46218
    ## 138                 Amoy Street Food Centre 19778.15255
    ## 139                       Bedok Food Centre 24086.56713
    ## 140                     Beo Crescent Market 18031.36863
    ## 141                      Berseh Food Centre 17508.20994
    ## 142                       Blk 1 Jalan Kukoh 18573.59633
    ## 143                   Blk 105 Hougang Ave 1 16029.14982
    ## 144           Blk 11 Telok Blangah Crescent 18981.11871
    ## 145               Blk 112 Jalan Bukit Merah 18917.34270
    ## 146                Blk 115 Bukit Merah View 18209.81193
    ## 147                  Blk 117 Aljunied Ave 2 18304.04737
    ## 148              Blk 127 Toa Payoh Lorong 1 13903.60422
    ## 149              Blk 137 Tampines Street 11 21567.35830
    ## 150                   Blk 159 Mei Chin Road 16901.39698
    ## 151                 Blk 16 Bedok South Road 22281.48357
    ## 152             Blk 163 Bukit Merah Central 18258.01395
    ## 153             Blk 17 Upper Boon Keng Road 17703.43699
    ## 154                    Blk 20 Ghim Moh Road 14744.97354
    ## 155          Blk 208B New Upper Changi Road 21504.01752
    ## 156              Blk 210 Toa Payoh Lorong 8 14320.58829
    ## 157            Blk 216 Bedok North Street 1 21640.82124
    ## 158               Blk 22 Toa Payoh Lorong 7 14942.15953
    ## 159               Blk 226D Ang Mo Kio Ave 1 10969.04933
    ## 160           Blk 226H Ang Mo Kio Street 22 10981.96967
    ## 161           Blk 254 Jurong East Street 24 11941.22892
    ## 162                   Blk 29 Bendemeer Road 16769.98826
    ## 163                     Blk 320 Shunfu Road 12146.43996
    ## 164                Blk 341 Ang Mo Kio Ave 1 11846.35440
    ## 165               Blk 347 Jurong East Ave 1 12023.88383
    ## 166                  Blk 353 Clementi Ave 2 14338.53282
    ## 167               Blk 36 Telok Blangah Rise 19591.69271
    ## 168              Blk 37A Teban Gardens Road 14130.14304
    ## 169               Blk 409 Ang Mo Kio Ave 10 12494.57525
    ## 170                    Blk 44 Holland Drive 15111.78144
    ## 171                  Blk 448 Clementi Ave 3 14511.87598
    ## 172              Blk 453A Ang Mo Kio Ave 10 12146.60098
    ## 173                       Blk 49 Sims Place 18054.75440
    ## 174                   Blk 4A Eunos Crescent 19661.50466
    ## 175                       Blk 4A Jalan Batu 19641.29927
    ## 176            Blk 4A Woodlands Centre Road   860.34019
    ## 177                Blk 502 West Coast Drive 14732.41354
    ## 178                Blk 503 West Coast Drive 14733.18536
    ## 179           Blk 505 Jurong West Street 52 12302.14171
    ## 180                  Blk 50A Marine Terrace 21702.08377
    ## 181                 Blk 51 Old Airport Road 19263.10967
    ## 182            Blk 511 Bedok North Street 3 21002.11816
    ## 183  Bukit Panjang Hawker Centre and Market  7347.80109
    ## 184          Our Tampines Hub Hawker Centre 20644.77518
    ## 185         Kampung Admiralty Hawker Centre  2650.65272
    ## 186                    Yishun Hawker Centre  7800.72678
    ## 187    Jurong West Hawker Centre and Market 14409.34854
    ## 188         Pasir Ris Central Hawker Centre 20907.00029
    ## 189                    Dawson Hawker Centre 16474.75242
    ## 190       Woodlands Street 12 Hawker Centre  1179.20902
    ## 191               Blk 527 Ang Mo Kio Ave 10 11652.99993
    ## 192            Blk 538 Bedok North Street 3 20581.82556
    ## 193            Blk 58 New Upper Changi Road 22538.52205
    ## 194               Blk 6 Tanjong Pagar Plaza 19899.14485
    ## 195                Blk 628 Ang Mo Kio Ave 4  9910.24427
    ## 196            Blk 630 Bedok Reservoir Road 19540.69234
    ## 197                    Blk 69 Geylang Bahru 17026.36422
    ## 198                      Blk 7 Empress Road 14463.72363
    ## 199                Blk 724 Ang Mo Kio Ave 6 11060.46290
    ## 200          Blk 726 Clementi West Street 2 15558.31362
    ## 201               Blk 74 Toa Payoh Lorong 4 14677.99265
    ## 202               Market Street Food Centre 19461.47742
    ## 203                     Maxwell Food Centre 19610.38224
    ## 204                      Newton Food Centre 16160.67281
    ## 205  North Bridge Road Market & Food Centre 18064.07207
    ## 206               Pasir Panjang Food Centre 18640.84935
    ## 207            Pek Kio Market & Food Centre 16293.96632
    ## 208               People's Park Food Centre 19016.17431
    ## 209             Sembawang Hills Food Centre  9783.68389
    ## 210                 Serangoon Garden Market 13377.44342
    ## 211       Taman Jurong Market & Food Centre 13581.17903
    ## 212                     Tanglin Halt Market 15984.84767
    ## 213                            Tekka Market 17272.77338
    ## 214                      Tiong Bahru Market 18600.19748
    ## 215              Zion Riverside Food Centre 17801.01019
    ## 216               Blk 75 Toa Payoh Lorong 5 14602.81319
    ## 217                     Blk 79 Redhill Lane 17851.17778
    ## 218              Blk 79 Telok Blangah Drive 19156.11291
    ## 219                     Blk 80 Circuit Road 17727.62606
    ## 220              Blk 82 Telok Blangah Drive 19103.02611
    ## 221            Blk 84 Marine Parade Central 21252.62103
    ## 222             Blk 85 Bedok North Street 4 21822.61807
    ## 223                     Blk 85 Redhill Lane 17905.72428
    ## 224                     Blk 89 Circuit Road 17957.32333
    ## 225                    Blk 90 Whampoa Drive 15939.09229
    ## 226               Blk 93 Toa Payoh Lorong 4 14163.51561
    ## 227                    Blks 13/14 Haig Road 19398.02369
    ## 228           Blks 160/162 Ang Mo Kio Ave 4 10319.87510
    ## 229                   Ci Yuan Hawker Centre 13994.81923
    ## 230                Blks 22A/B Havelock Road 14460.84099
    ## 231                Blks 79/79A Circuit Road 19280.99111
    ## 232                Blks 91/92 Whampoa Drive 15905.43723
    ## 233                      Bukit Timah Market  7062.61053
    ## 234                        Chinatown Market 16094.86010
    ## 235                 Chomp Chomp Food Centre 17237.69399
    ## 236         Chong Pang Market & Food Centre 16024.98683
    ## 237                      Dunman Food Centre 21406.01057
    ## 238          East Coast Lagoon Food Village 25094.98492
    ## 239                    Geylang Serai Market 20875.00639
    ## 240                 Golden Mile Food Centre 17453.94361
    ## 241    Holland Village Market & Food Centre  9889.66714
    ## 242           Hong Lim Market & Food Centre 16221.39347
    ## 243                   Kallang Estate Market 19520.73135
    ## 244              Kovan Market & Food Centre 19333.90566
    ## 245          Blks 2 & 3 Changi Village Road 31036.33134
    ## 246            Commonwealth Crescent Market 10589.13361
    ## 247     ABC Brickworks Market & Food Centre 12412.44778
    ## 248                        Adam Food Centre 11519.39329
    ## 249      Albert Centre Market & Food Centre 16479.88723
    ## 250           Alexandra Village Food Centre 12106.98137
    ## 251                 Amoy Street Food Centre 16583.82193
    ## 252                       Bedok Food Centre 27146.76566
    ## 253                     Beo Crescent Market 14192.86196
    ## 254                      Berseh Food Centre 16570.10562
    ## 255                       Blk 1 Jalan Kukoh 15490.03658
    ## 256                   Blk 105 Hougang Ave 1 19752.41739
    ## 257           Blk 11 Telok Blangah Crescent 13965.96001
    ## 258               Blk 112 Jalan Bukit Merah 14517.42982
    ## 259                Blk 115 Bukit Merah View 13835.89086
    ## 260                  Blk 117 Aljunied Ave 2 19578.29223
    ## 261              Blk 127 Toa Payoh Lorong 1 14703.75222
    ## 262              Blk 137 Tampines Street 11 25799.60089
    ## 263                   Blk 159 Mei Chin Road 11558.53797
    ## 264                 Blk 16 Bedok South Road 24938.23716
    ## 265             Blk 163 Bukit Merah Central 13444.32682
    ## 266             Blk 17 Upper Boon Keng Road 17987.68360
    ## 267                    Blk 20 Ghim Moh Road  9208.38874
    ## 268          Blk 208B New Upper Changi Road 24311.48432
    ## 269              Blk 210 Toa Payoh Lorong 8 15767.60155
    ## 270            Blk 216 Bedok North Street 1 24612.68585
    ## 271               Blk 22 Toa Payoh Lorong 7 16089.92893
    ## 272               Blk 226D Ang Mo Kio Ave 1 14271.01185
    ## 273           Blk 226H Ang Mo Kio Street 22 14358.42955
    ## 274           Blk 254 Jurong East Street 24  2787.13513
    ## 275                   Blk 29 Bendemeer Road 16963.62454
    ## 276                     Blk 320 Shunfu Road 13845.98899
    ## 277                Blk 341 Ang Mo Kio Ave 1 15218.44699
    ## 278               Blk 347 Jurong East Ave 1  2090.03288
    ## 279                  Blk 353 Clementi Ave 2  7304.68252
    ## 280               Blk 36 Telok Blangah Rise 14573.80214
    ## 281              Blk 37A Teban Gardens Road  4298.78915
    ## 282               Blk 409 Ang Mo Kio Ave 10 15984.60804
    ## 283                    Blk 44 Holland Drive  9800.48160
    ## 284                  Blk 448 Clementi Ave 3  6753.24916
    ## 285              Blk 453A Ang Mo Kio Ave 10 16177.12641
    ## 286                       Blk 49 Sims Place 18802.04114
    ## 287                   Blk 4A Eunos Crescent 21484.66746
    ## 288                       Blk 4A Jalan Batu 19625.55547
    ## 289            Blk 4A Woodlands Centre Road 12346.27240
    ## 290                Blk 502 West Coast Drive  6341.30515
    ## 291                Blk 503 West Coast Drive  6399.94686
    ## 292           Blk 505 Jurong West Street 52   796.13276
    ## 293                  Blk 50A Marine Terrace 23004.97428
    ## 294                 Blk 51 Old Airport Road 19686.03887
    ## 295            Blk 511 Bedok North Street 3 24275.23827
    ## 296  Bukit Panjang Hawker Centre and Market  7546.11999
    ## 297          Our Tampines Hub Hawker Centre 25272.44712
    ## 298         Kampung Admiralty Hawker Centre 14330.26410
    ## 299                    Yishun Hawker Centre 17131.06037
    ## 300    Jurong West Hawker Centre and Market  1768.52028
    ## 301         Pasir Ris Central Hawker Centre 26751.86356
    ## 302                    Dawson Hawker Centre 11510.93713
    ## 303       Woodlands Street 12 Hawker Centre 12285.60300
    ## 304               Blk 527 Ang Mo Kio Ave 10 16059.20250
    ## 305            Blk 538 Bedok North Street 3 23645.06108
    ## 306            Blk 58 New Upper Changi Road 25517.26766
    ## 307               Blk 6 Tanjong Pagar Plaza 16372.35952
    ## 308                Blk 628 Ang Mo Kio Ave 4 14772.16284
    ## 309            Blk 630 Bedok Reservoir Road 22437.13053
    ## 310                    Blk 69 Geylang Bahru 17691.42776
    ## 311                      Blk 7 Empress Road 10817.21593
    ## 312                Blk 724 Ang Mo Kio Ave 6 15177.39946
    ## 313          Blk 726 Clementi West Street 2  7335.00835
    ## 314               Blk 74 Toa Payoh Lorong 4 15536.31006
    ## 315               Market Street Food Centre 16711.40505
    ## 316                     Maxwell Food Centre 16362.55091
    ## 317                      Newton Food Centre 14580.90369
    ## 318  North Bridge Road Market & Food Centre 17371.76838
    ## 319               Pasir Panjang Food Centre 11630.52807
    ## 320            Pek Kio Market & Food Centre 15628.01790
    ## 321               People's Park Food Centre 15904.80002
    ## 322             Sembawang Hills Food Centre 13275.31189
    ## 323                 Serangoon Garden Market 17245.91553
    ## 324       Taman Jurong Market & Food Centre  1524.58831
    ## 325                     Tanglin Halt Market 10642.34055
    ## 326                            Tekka Market 15923.69871
    ## 327                      Tiong Bahru Market 14873.76082
    ## 328              Zion Riverside Food Centre 14414.84092
    ## 329               Blk 75 Toa Payoh Lorong 5 15629.31022
    ## 330                     Blk 79 Redhill Lane 13355.09732
    ## 331              Blk 79 Telok Blangah Drive 13209.93093
    ## 332                     Blk 80 Circuit Road 19495.83727
    ## 333              Blk 82 Telok Blangah Drive 13201.99188
    ## 334            Blk 84 Marine Parade Central 22056.02061
    ## 335             Blk 85 Bedok North Street 4 25186.48473
    ## 336                     Blk 85 Redhill Lane 13376.66620
    ## 337                     Blk 89 Circuit Road 19364.39814
    ## 338                    Blk 90 Whampoa Drive 16031.16163
    ## 339               Blk 93 Toa Payoh Lorong 4 15233.05369
    ## 340                    Blks 13/14 Haig Road 20614.96120
    ## 341           Blks 160/162 Ang Mo Kio Ave 4 14435.86427
    ## 342                   Ci Yuan Hawker Centre 19231.12343
    ## 343                Blks 79/79A Circuit Road  7492.36281
    ## 344                Blks 91/92 Whampoa Drive  4770.85988
    ## 345                      Bukit Timah Market  8241.89788
    ## 346                        Chinatown Market  1636.22062
    ## 347                 Chomp Chomp Food Centre  9379.00732
    ## 348         Chong Pang Market & Food Centre 15864.89288
    ## 349                      Dunman Food Centre  8377.43374
    ## 350          East Coast Lagoon Food Village 11910.01244
    ## 351                    Geylang Serai Market  8269.84473
    ## 352                 Golden Mile Food Centre  4153.21274
    ## 353    Holland Village Market & Food Centre  4625.86102
    ## 354           Hong Lim Market & Food Centre  1827.13090
    ## 355                   Kallang Estate Market  6434.64969
    ## 356              Kovan Market & Food Centre 10060.54124
    ## 357          Blks 2 & 3 Changi Village Road 20899.19409
    ## 358            Commonwealth Crescent Market  3906.77120
    ## 359     ABC Brickworks Market & Food Centre  2395.31011
    ## 360                        Adam Food Centre  4352.81849
    ## 361      Albert Centre Market & Food Centre  3092.04083
    ## 362           Alexandra Village Food Centre  2803.74245
    ## 363                 Amoy Street Food Centre  2123.36637
    ## 364                       Bedok Food Centre 14456.08767
    ## 365                     Beo Crescent Market   269.29711
    ## 366                      Berseh Food Centre  3710.11716
    ## 367                       Blk 1 Jalan Kukoh  1148.77652
    ## 368                   Blk 105 Hougang Ave 1  9914.77770
    ## 369           Blk 11 Telok Blangah Crescent  1694.30687
    ## 370               Blk 112 Jalan Bukit Merah   968.76756
    ## 371                Blk 115 Bukit Merah View   886.45610
    ## 372                  Blk 117 Aljunied Ave 2  7338.18470
    ## 373              Blk 127 Toa Payoh Lorong 1  5791.05721
    ## 374              Blk 137 Tampines Street 11 14298.53293
    ## 375                   Blk 159 Mei Chin Road  3030.39000
    ## 376                 Blk 16 Bedok South Road 12325.20402
    ## 377             Blk 163 Bukit Merah Central  1483.71614
    ## 378             Blk 17 Upper Boon Keng Road  5548.83922
    ## 379                    Blk 20 Ghim Moh Road  5263.49457
    ## 380          Blk 208B New Upper Changi Road 11945.45595
    ## 381              Blk 210 Toa Payoh Lorong 8  6404.95168
    ## 382            Blk 216 Bedok North Street 1 12312.63285
    ## 383               Blk 22 Toa Payoh Lorong 7  6050.52682
    ## 384               Blk 226D Ang Mo Kio Ave 1  8779.95600
    ## 385           Blk 226H Ang Mo Kio Street 22  8838.60951
    ## 386           Blk 254 Jurong East Street 24 11920.41039
    ## 387                   Blk 29 Bendemeer Road  5074.07262
    ## 388                     Blk 320 Shunfu Road  7128.12836
    ## 389                Blk 341 Ang Mo Kio Ave 1  8669.53333
    ## 390               Blk 347 Jurong East Ave 1 12619.87176
    ## 391                  Blk 353 Clementi Ave 2  7164.14370
    ## 392               Blk 36 Telok Blangah Rise  1903.47485
    ## 393              Blk 37A Teban Gardens Road 10314.54889
    ## 394               Blk 409 Ang Mo Kio Ave 10  8752.44809
    ## 395                    Blk 44 Holland Drive  4667.14182
    ## 396                  Blk 448 Clementi Ave 3  7767.67008
    ## 397              Blk 453A Ang Mo Kio Ave 10  9360.37580
    ## 398                       Blk 49 Sims Place  6408.14108
    ## 399                   Blk 4A Eunos Crescent  9043.87862
    ## 400                       Blk 4A Jalan Batu  6252.06972
    ## 401            Blk 4A Woodlands Centre Road 18155.31726
    ## 402                Blk 502 West Coast Drive  8275.50051
    ## 403                Blk 503 West Coast Drive  8210.72921
    ## 404           Blk 505 Jurong West Street 52 14127.68563
    ## 405                  Blk 50A Marine Terrace  9786.04916
    ## 406                 Blk 51 Old Airport Road  6644.93191
    ## 407            Blk 511 Bedok North Street 3 12302.11240
    ## 408  Bukit Panjang Hawker Centre and Market 11774.88259
    ## 409          Our Tampines Hub Hawker Centre 14255.26526
    ## 410         Kampung Admiralty Hawker Centre 17108.35404
    ## 411                    Yishun Hawker Centre 15246.57518
    ## 412    Jurong West Hawker Centre and Market 15850.84176
    ## 413         Pasir Ris Central Hawker Centre 16534.38836
    ## 414                    Dawson Hawker Centre  2965.18239
    ## 415       Woodlands Street 12 Hawker Centre 17024.61822
    ## 416               Blk 527 Ang Mo Kio Ave 10  9775.68101
    ## 417            Blk 538 Bedok North Street 3 11651.39089
    ## 418            Blk 58 New Upper Changi Road 13039.56430
    ## 419               Blk 6 Tanjong Pagar Plaza  1961.85281
    ## 420                Blk 628 Ang Mo Kio Ave 4 10362.23151
    ## 421            Blk 630 Bedok Reservoir Road 10641.69161
    ## 422                    Blk 69 Geylang Bahru  5822.68500
    ## 423                      Blk 7 Empress Road  4110.99033
    ## 424                Blk 724 Ang Mo Kio Ave 6  9500.69082
    ## 425          Blk 726 Clementi West Street 2  7481.29370
    ## 426               Blk 74 Toa Payoh Lorong 4  5720.08640
    ## 427               Market Street Food Centre  2315.55902
    ## 428                     Maxwell Food Centre  1902.10308
    ## 429                      Newton Food Centre  2866.47686
    ## 430  North Bridge Road Market & Food Centre  4290.78315
    ## 431               Pasir Panjang Food Centre  4458.41732
    ## 432            Pek Kio Market & Food Centre  3868.80144
    ## 433               People's Park Food Centre  1478.73242
    ## 434             Sembawang Hills Food Centre  9333.83532
    ## 435                 Serangoon Garden Market  9282.59709
    ## 436       Taman Jurong Market & Food Centre 13088.89055
    ## 437                     Tanglin Halt Market  3832.94211
    ## 438                            Tekka Market  3078.29403
    ## 439                      Tiong Bahru Market   440.09909
    ## 440              Zion Riverside Food Centre   513.20745
    ## 441               Blk 75 Toa Payoh Lorong 5  5914.17756
    ## 442                     Blk 79 Redhill Lane  1249.22723
    ## 443              Blk 79 Telok Blangah Drive  2933.18651
    ## 444                     Blk 80 Circuit Road  7771.24986
    ## 445              Blk 82 Telok Blangah Drive  2870.71351
    ## 446            Blk 84 Marine Parade Central  8682.73320
    ## 447             Blk 85 Bedok North Street 4 13081.38755
    ## 448                     Blk 85 Redhill Lane  1258.27060
    ## 449                     Blk 89 Circuit Road  7346.97298
    ## 450                    Blk 90 Whampoa Drive  4796.95260
    ## 451               Blk 93 Toa Payoh Lorong 4  6006.56094
    ## 452                    Blks 13/14 Haig Road  7933.93786
    ## 453           Blks 160/162 Ang Mo Kio Ave 4  9625.55933
    ## 454                   Ci Yuan Hawker Centre 11347.47374
    ## 455                Blks 91/92 Whampoa Drive  3463.47858
    ## 456                      Bukit Timah Market 12219.83848
    ## 457                        Chinatown Market  6753.64509
    ## 458                 Chomp Chomp Food Centre  4657.53326
    ## 459         Chong Pang Market & Food Centre 13203.74392
    ## 460                      Dunman Food Centre  2652.63608
    ## 461          East Coast Lagoon Food Village  5975.57718
    ## 462                    Geylang Serai Market  1818.61796
    ## 463                 Golden Mile Food Centre  3512.94867
    ## 464    Holland Village Market & Food Centre 10182.28825
    ## 465           Hong Lim Market & Food Centre  6309.83288
    ## 466                   Kallang Estate Market  2109.31822
    ## 467              Kovan Market & Food Centre  3613.92329
    ## 468          Blks 2 & 3 Changi Village Road 13415.68991
    ## 469            Commonwealth Crescent Market  9703.22489
    ## 470     ABC Brickworks Market & Food Centre  9609.75788
    ## 471                        Adam Food Centre  7892.21660
    ## 472      Albert Centre Market & Food Centre  4437.58135
    ## 473           Alexandra Village Food Centre 10004.10648
    ## 474                 Amoy Street Food Centre  6750.85603
    ## 475                       Bedok Food Centre  7867.14696
    ## 476                     Beo Crescent Market  7650.22422
    ## 477                      Berseh Food Centre  3782.70879
    ## 478                       Blk 1 Jalan Kukoh  6561.22589
    ## 479                   Blk 105 Hougang Ave 1  3079.82360
    ## 480           Blk 11 Telok Blangah Crescent  9168.81158
    ## 481               Blk 112 Jalan Bukit Merah  8338.68619
    ## 482                Blk 115 Bukit Merah View  8345.52171
    ## 483                  Blk 117 Aljunied Ave 2   678.14288
    ## 484              Blk 127 Toa Payoh Lorong 1  4667.35703
    ## 485              Blk 137 Tampines Street 11  6961.97775
    ## 486                   Blk 159 Mei Chin Road  9851.52599
    ## 487                 Blk 16 Bedok South Road  5657.37281
    ## 488             Blk 163 Bukit Merah Central  8928.06013
    ## 489             Blk 17 Upper Boon Keng Road  1956.20981
    ## 490                    Blk 20 Ghim Moh Road 10908.98579
    ## 491          Blk 208B New Upper Changi Road  5044.14584
    ## 492              Blk 210 Toa Payoh Lorong 8  3737.09802
    ## 493            Blk 216 Bedok North Street 1  5362.15979
    ## 494               Blk 22 Toa Payoh Lorong 7  3264.69334
    ## 495               Blk 226D Ang Mo Kio Ave 1  6774.08216
    ## 496           Blk 226H Ang Mo Kio Street 22  6746.43644
    ## 497           Blk 254 Jurong East Street 24 16496.10069
    ## 498                   Blk 29 Bendemeer Road  2578.03805
    ## 499                     Blk 320 Shunfu Road  6044.50581
    ## 500                Blk 341 Ang Mo Kio Ave 1  5839.47261
    ## 501               Blk 347 Jurong East Ave 1 17205.78744
    ## 502                  Blk 353 Clementi Ave 2 12780.10863
    ## 503               Blk 36 Telok Blangah Rise  9202.48246
    ## 504              Blk 37A Teban Gardens Road 15832.20587
    ## 505               Blk 409 Ang Mo Kio Ave 10  5199.38374
    ## 506                    Blk 44 Holland Drive 10466.88800
    ## 507                  Blk 448 Clementi Ave 3 13493.13141
    ## 508              Blk 453A Ang Mo Kio Ave 10  5616.89282
    ## 509                       Blk 49 Sims Place  1210.28090
    ## 510                   Blk 4A Eunos Crescent  2243.14975
    ## 511                       Blk 4A Jalan Batu  2660.07092
    ## 512            Blk 4A Woodlands Centre Road 17990.38038
    ## 513                Blk 502 West Coast Drive 14101.13449
    ## 514                Blk 503 West Coast Drive 14037.53551
    ## 515           Blk 505 Jurong West Street 52 18716.88224
    ## 516                  Blk 50A Marine Terrace  4112.61496
    ## 517                 Blk 51 Old Airport Road  2014.20011
    ## 518            Blk 511 Bedok North Street 3  5125.60431
    ## 519  Bukit Panjang Hawker Centre and Market 13743.73532
    ## 520          Our Tampines Hub Hawker Centre  6798.68300
    ## 521         Kampung Admiralty Hawker Centre 15670.75628
    ## 522                    Yishun Hawker Centre 11785.62556
    ## 523    Jurong West Hawker Centre and Market 20946.40973
    ## 524         Pasir Ris Central Hawker Centre  9042.16463
    ## 525                    Dawson Hawker Centre  9496.15606
    ## 526       Woodlands Street 12 Hawker Centre 16656.36316
    ## 527               Blk 527 Ang Mo Kio Ave 10  6148.20166
    ## 528            Blk 538 Bedok North Street 3  4468.47399
    ## 529            Blk 58 New Upper Changi Road  6247.14139
    ## 530               Blk 6 Tanjong Pagar Plaza  7217.06473
    ## 531                Blk 628 Ang Mo Kio Ave 4  7796.76782
    ## 532            Blk 630 Bedok Reservoir Road  3312.70134
    ## 533                    Blk 69 Geylang Bahru  1762.22439
    ## 534                      Blk 7 Empress Road  8908.96604
    ## 535                Blk 724 Ang Mo Kio Ave 6  6630.97525
    ## 536          Blk 726 Clementi West Street 2 13673.25406
    ## 537               Blk 74 Toa Payoh Lorong 4  3784.94902
    ## 538               Market Street Food Centre  6111.96559
    ## 539                     Maxwell Food Centre  6803.26856
    ## 540                      Newton Food Centre  5311.36662
    ## 541  North Bridge Road Market & Food Centre  3282.20639
    ## 542               Pasir Panjang Food Centre 11825.35283
    ## 543            Pek Kio Market & Food Centre  4038.54311
    ## 544               People's Park Food Centre  6584.10124
    ## 545             Sembawang Hills Food Centre  8044.08470
    ## 546                 Serangoon Garden Market  4541.48196
    ## 547       Taman Jurong Market & Food Centre 18220.74296
    ## 548                     Tanglin Halt Market 10134.08597
    ## 549                            Tekka Market  4445.60683
    ## 550                      Tiong Bahru Market  7437.00673
    ## 551              Zion Riverside Food Centre  7081.35337
    ## 552               Blk 75 Toa Payoh Lorong 5  3726.63309
    ## 553                     Blk 79 Redhill Lane  8556.53934
    ## 554              Blk 79 Telok Blangah Drive 10425.09696
    ## 555                     Blk 80 Circuit Road   278.91991
    ## 556              Blk 82 Telok Blangah Drive 10362.04696
    ## 557            Blk 84 Marine Parade Central  3571.59204
    ## 558             Blk 85 Bedok North Street 4  6006.33756
    ## 559                     Blk 85 Redhill Lane  8591.89391
    ## 560                     Blk 89 Circuit Road   335.29516
    ## 561                    Blk 90 Whampoa Drive  3350.72920
    ## 562               Blk 93 Toa Payoh Lorong 4  4171.69423
    ## 563                    Blks 13/14 Haig Road  1716.43161
    ## 564           Blks 160/162 Ang Mo Kio Ave 4  7364.06349
    ## 565                   Ci Yuan Hawker Centre  5420.73076
    ## 566                      Bukit Timah Market  8866.00719
    ## 567                        Chinatown Market  4712.71931
    ## 568                 Chomp Chomp Food Centre  4720.73235
    ## 569         Chong Pang Market & Food Centre 12278.21613
    ## 570                      Dunman Food Centre  5535.99404
    ## 571          East Coast Lagoon Food Village  9196.95508
    ## 572                    Geylang Serai Market  4969.58293
    ## 573                 Golden Mile Food Centre  2520.85979
    ## 574    Holland Village Market & Food Centre  6732.67883
    ## 575           Hong Lim Market & Food Centre  4319.91278
    ## 576                   Kallang Estate Market  3785.93274
    ## 577              Kovan Market & Food Centre  5309.60062
    ## 578          Blks 2 & 3 Changi Village Road 16607.33048
    ## 579            Commonwealth Crescent Market  6282.95260
    ## 580     ABC Brickworks Market & Food Centre  6511.62516
    ## 581                        Adam Food Centre  4441.50873
    ## 582      Albert Centre Market & Food Centre  2479.17153
    ## 583           Alexandra Village Food Centre  6877.16948
    ## 584                 Amoy Street Food Centre  4957.80767
    ## 585                       Bedok Food Centre 11290.59038
    ## 586                     Beo Crescent Market  4844.93320
    ## 587                      Berseh Food Centre  1805.44341
    ## 588                       Blk 1 Jalan Kukoh  4196.66945
    ## 589                   Blk 105 Hougang Ave 1  5232.02886
    ## 590           Blk 11 Telok Blangah Crescent  6440.34869
    ## 591               Blk 112 Jalan Bukit Merah  5726.80442
    ## 592                Blk 115 Bukit Merah View  5502.37755
    ## 593                  Blk 117 Aljunied Ave 2  3679.91642
    ## 594              Blk 127 Toa Payoh Lorong 1  1926.46720
    ## 595              Blk 137 Tampines Street 11 10373.10554
    ## 596                   Blk 159 Mei Chin Road  6600.25377
    ## 597                 Blk 16 Bedok South Road  9072.47252
    ## 598             Blk 163 Bukit Merah Central  6026.96016
    ## 599             Blk 17 Upper Boon Keng Road  2162.67421
    ## 600                    Blk 20 Ghim Moh Road  7454.76927
    ## 601          Blk 208B New Upper Changi Road  8492.32832
    ## 602              Blk 210 Toa Payoh Lorong 8  1862.33576
    ## 603            Blk 216 Bedok North Street 1  8818.18351
    ## 604               Blk 22 Toa Payoh Lorong 7  1346.98259
    ## 605               Blk 226D Ang Mo Kio Ave 1  5071.49722
    ## 606           Blk 226H Ang Mo Kio Street 22  5092.43628
    ## 607           Blk 254 Jurong East Street 24 13129.48893
    ## 608                   Blk 29 Bendemeer Road  1099.55105
    ## 609                     Blk 320 Shunfu Road  3685.87515
    ## 610                Blk 341 Ang Mo Kio Ave 1  4544.85597
    ## 611               Blk 347 Jurong East Ave 1 13844.91102
    ## 612                  Blk 353 Clementi Ave 2  9316.77765
    ## 613               Blk 36 Telok Blangah Rise  6659.57893
    ## 614              Blk 37A Teban Gardens Road 12376.10769
    ## 615               Blk 409 Ang Mo Kio Ave 10  4353.21989
    ## 616                    Blk 44 Holland Drive  7028.23074
    ## 617                  Blk 448 Clementi Ave 3 10029.95639
    ## 618              Blk 453A Ang Mo Kio Ave 10  4962.33842
    ## 619                       Blk 49 Sims Place  2906.84835
    ## 620                   Blk 4A Eunos Crescent  5596.40328
    ## 621                       Blk 4A Jalan Batu  4055.36781
    ## 622            Blk 4A Woodlands Centre Road 15997.47581
    ## 623                Blk 502 West Coast Drive 10638.36506
    ## 624                Blk 503 West Coast Drive 10574.86813
    ## 625           Blk 505 Jurong West Street 52 15369.08792
    ## 626                  Blk 50A Marine Terrace  7137.82805
    ## 627                 Blk 51 Old Airport Road  3912.94439
    ## 628            Blk 511 Bedok North Street 3  8586.46466
    ## 629  Bukit Panjang Hawker Centre and Market 10877.22023
    ## 630          Our Tampines Hub Hawker Centre 10109.62701
    ## 631         Kampung Admiralty Hawker Centre 14186.45884
    ## 632                    Yishun Hawker Centre 11281.74138
    ## 633    Jurong West Hawker Centre and Market 17546.28223
    ## 634         Pasir Ris Central Hawker Centre 12176.16127
    ## 635                    Dawson Hawker Centre  6196.15557
    ## 636       Woodlands Street 12 Hawker Centre 14717.31615
    ## 637               Blk 527 Ang Mo Kio Ave 10  5456.86482
    ## 638            Blk 538 Bedok North Street 3  7930.72324
    ## 639            Blk 58 New Upper Changi Road  9689.76237
    ## 640               Blk 6 Tanjong Pagar Plaza  5315.97845
    ## 641                Blk 628 Ang Mo Kio Ave 4  6541.34239
    ## 642            Blk 630 Bedok Reservoir Road  6762.08546
    ## 643                    Blk 69 Geylang Bahru  1786.74338
    ## 644                      Blk 7 Empress Road  5448.06268
    ## 645                Blk 724 Ang Mo Kio Ave 6  5458.40347
    ## 646          Blk 726 Clementi West Street 2 10227.50938
    ## 647               Blk 74 Toa Payoh Lorong 4  1251.66210
    ## 648               Market Street Food Centre  4396.79836
    ## 649                     Maxwell Food Centre  4898.11018
    ## 650                      Newton Food Centre  2057.00213
    ## 651  North Bridge Road Market & Food Centre  2237.57932
    ## 652               Pasir Panjang Food Centre  8731.62591
    ## 653            Pek Kio Market & Food Centre   909.77299
    ## 654               People's Park Food Centre  4437.67666
    ## 655             Sembawang Hills Food Centre  6090.89793
    ## 656                 Serangoon Garden Market  4614.42447
    ## 657       Taman Jurong Market & Food Centre 14802.85887
    ## 658                     Tanglin Halt Market  6761.66017
    ## 659                            Tekka Market  1943.92066
    ## 660                      Tiong Bahru Market  4879.48736
    ## 661              Zion Riverside Food Centre  4277.08518
    ## 662               Blk 75 Toa Payoh Lorong 5  1400.75270
    ## 663                     Blk 79 Redhill Lane  5587.39197
    ## 664              Blk 79 Telok Blangah Drive  7572.69713
    ## 665                     Blk 80 Circuit Road  3711.73030
    ## 666              Blk 82 Telok Blangah Drive  7504.56824
    ## 667            Blk 84 Marine Parade Central  6269.15411
    ## 668             Blk 85 Bedok North Street 4  9469.78525
    ## 669                     Blk 85 Redhill Lane  5633.58562
    ## 670                     Blk 89 Circuit Road  3492.41101
    ## 671                    Blk 90 Whampoa Drive   130.27704
    ## 672               Blk 93 Toa Payoh Lorong 4  1739.95148
    ## 673                    Blks 13/14 Haig Road  4712.93417
    ## 674           Blks 160/162 Ang Mo Kio Ave 4  5883.75773
    ## 675                   Ci Yuan Hawker Centre  6589.52478
    ## 676                        Chinatown Market  9788.96299
    ## 677                 Chomp Chomp Food Centre 10444.22431
    ## 678         Chong Pang Market & Food Centre 11740.54266
    ## 679                      Dunman Food Centre 14390.19921
    ## 680          East Coast Lagoon Food Village 18062.95063
    ## 681                    Geylang Serai Market 13831.47352
    ## 682                 Golden Mile Food Centre 10581.08467
    ## 683    Holland Village Market & Food Centre  3783.92642
    ## 684           Hong Lim Market & Food Centre  9808.61658
    ## 685                   Kallang Estate Market 12543.14114
    ## 686              Kovan Market & Food Centre 12433.00822
    ## 687          Blks 2 & 3 Changi Village Road 24253.77858
    ## 688            Commonwealth Crescent Market  4471.20509
    ## 689     ABC Brickworks Market & Food Centre  6811.02684
    ## 690                        Adam Food Centre  4570.04321
    ## 691      Albert Centre Market & Food Centre  9683.89079
    ## 692           Alexandra Village Food Centre  6671.03074
    ## 693                 Amoy Street Food Centre 10295.76694
    ## 694                       Bedok Food Centre 20084.60911
    ## 695                     Beo Crescent Market  7995.18776
    ## 696                      Berseh Food Centre  9671.06164
    ## 697                       Blk 1 Jalan Kukoh  9090.49104
    ## 698                   Blk 105 Hougang Ave 1 12796.82158
    ## 699           Blk 11 Telok Blangah Crescent  8339.05785
    ## 700               Blk 112 Jalan Bukit Merah  8607.28295
    ## 701                Blk 115 Bukit Merah View  7858.32737
    ## 702                  Blk 117 Aljunied Ave 2 12528.16150
    ## 703              Blk 127 Toa Payoh Lorong 1  7650.96730
    ## 704              Blk 137 Tampines Street 11 18779.41345
    ## 705                   Blk 159 Mei Chin Road  5915.87558
    ## 706                 Blk 16 Bedok South Road 17877.20546
    ## 707             Blk 163 Bukit Merah Central  7663.44619
    ## 708             Blk 17 Upper Boon Keng Road 10977.38762
    ## 709                    Blk 20 Ghim Moh Road  3420.78912
    ## 710          Blk 208B New Upper Changi Road 17248.87833
    ## 711              Blk 210 Toa Payoh Lorong 8  8725.65788
    ## 712            Blk 216 Bedok North Street 1 17550.57320
    ## 713               Blk 22 Toa Payoh Lorong 7  9030.63877
    ## 714               Blk 226D Ang Mo Kio Ave 1  7662.47120
    ## 715           Blk 226H Ang Mo Kio Street 22  7755.61546
    ## 716           Blk 254 Jurong East Street 24  4276.48544
    ## 717                   Blk 29 Bendemeer Road  9938.34806
    ## 718                     Blk 320 Shunfu Road  6934.83583
    ## 719                Blk 341 Ang Mo Kio Ave 1  8493.82244
    ## 720               Blk 347 Jurong East Ave 1  4986.33819
    ## 721                  Blk 353 Clementi Ave 2  2824.24210
    ## 722               Blk 36 Telok Blangah Rise  8992.90194
    ## 723              Blk 37A Teban Gardens Road  4215.38775
    ## 724               Blk 409 Ang Mo Kio Ave 10  9204.45434
    ## 725                    Blk 44 Holland Drive  3928.53093
    ## 726                  Blk 448 Clementi Ave 3  3157.47819
    ## 727              Blk 453A Ang Mo Kio Ave 10  9495.25226
    ## 728                       Blk 49 Sims Place 11770.81549
    ## 729                   Blk 4A Eunos Crescent 14429.16330
    ## 730                       Blk 4A Jalan Batu 12689.30831
    ## 731            Blk 4A Woodlands Centre Road 11238.63902
    ## 732                Blk 502 West Coast Drive  3570.01350
    ## 733                Blk 503 West Coast Drive  3544.52392
    ## 734           Blk 505 Jurong West Street 52  6504.23977
    ## 735                  Blk 50A Marine Terrace 15992.24994
    ## 736                 Blk 51 Old Airport Road 12699.13485
    ## 737            Blk 511 Bedok North Street 3 17219.48917
    ## 738  Bukit Panjang Hawker Centre and Market  4244.64549
    ## 739          Our Tampines Hub Hawker Centre 18291.74613
    ## 740         Kampung Admiralty Hawker Centre 11453.84156
    ## 741                    Yishun Hawker Centre 12174.55459
    ## 742    Jurong West Hawker Centre and Market  8748.79816
    ## 743         Pasir Ris Central Hawker Centre 19896.15481
    ## 744                    Dawson Hawker Centre  5627.76749
    ## 745       Woodlands Street 12 Hawker Centre 10420.59861
    ## 746               Blk 527 Ang Mo Kio Ave 10  9483.21751
    ## 747            Blk 538 Bedok North Street 3 16586.62707
    ## 748            Blk 58 New Upper Changi Road 18454.65803
    ## 749               Blk 6 Tanjong Pagar Plaza 10200.29697
    ## 750                Blk 628 Ang Mo Kio Ave 4  8545.98061
    ## 751            Blk 630 Bedok Reservoir Road 15379.86660
    ## 752                    Blk 69 Geylang Bahru 10648.13872
    ## 753                      Blk 7 Empress Road  4174.13615
    ## 754                Blk 724 Ang Mo Kio Ave 6  8647.01413
    ## 755          Blk 726 Clementi West Street 2  4146.67057
    ## 756               Blk 74 Toa Payoh Lorong 4  8475.28322
    ## 757               Market Street Food Centre 10274.18955
    ## 758                     Maxwell Food Centre 10080.93985
    ## 759                      Newton Food Centre  7699.55994
    ## 760  North Bridge Road Market & Food Centre 10464.16852
    ## 761               Pasir Panjang Food Centre  7224.46162
    ## 762            Pek Kio Market & Food Centre  8650.72424
    ## 763               People's Park Food Centre  9543.25064
    ## 764             Sembawang Hills Food Centre  6936.21290
    ## 765                 Serangoon Garden Market 10435.36576
    ## 766       Taman Jurong Market & Food Centre  6085.57198
    ## 767                     Tanglin Halt Market  4898.96937
    ## 768                            Tekka Market  9069.81123
    ## 769                      Tiong Bahru Market  8681.46064
    ## 770              Zion Riverside Food Centre  8048.55934
    ## 771               Blk 75 Toa Payoh Lorong 5  8571.27956
    ## 772                     Blk 79 Redhill Lane  7397.65589
    ## 773              Blk 79 Telok Blangah Drive  8103.39989
    ## 774                     Blk 80 Circuit Road 12433.49816
    ## 775              Blk 82 Telok Blangah Drive  8064.59012
    ## 776            Blk 84 Marine Parade Central 15074.59091
    ## 777             Blk 85 Bedok North Street 4 18129.40161
    ## 778                     Blk 85 Redhill Lane  7438.15732
    ## 779                     Blk 89 Circuit Road 12307.83260
    ## 780                    Blk 90 Whampoa Drive  8993.24312
    ## 781               Blk 93 Toa Payoh Lorong 4  8182.42767
    ## 782                    Blks 13/14 Haig Road 13578.90535
    ## 783           Blks 160/162 Ang Mo Kio Ave 4  8035.80862
    ## 784                   Ci Yuan Hawker Centre 12555.67044
    ## 785                 Chomp Chomp Food Centre  9431.72840
    ## 786         Chong Pang Market & Food Centre 16579.40994
    ## 787                      Dunman Food Centre  7185.61773
    ## 788          East Coast Lagoon Food Village 10572.39374
    ## 789                    Geylang Serai Market  7216.47094
    ## 790                 Golden Mile Food Centre  3243.29576
    ## 791    Holland Village Market & Food Centre  6242.12895
    ## 792           Hong Lim Market & Food Centre   444.15706
    ## 793                   Kallang Estate Market  5339.73735
    ## 794              Kovan Market & Food Centre  9744.90772
    ## 795          Blks 2 & 3 Changi Village Road 20010.34744
    ## 796            Commonwealth Crescent Market  5526.46928
    ## 797     ABC Brickworks Market & Food Centre  3935.45724
    ## 798                        Adam Food Centre  5647.11781
    ## 799      Albert Centre Market & Food Centre  2411.20994
    ## 800           Alexandra Village Food Centre  4329.06260
    ## 801                 Amoy Street Food Centre   506.87083
    ## 802                       Bedok Food Centre 13189.31501
    ## 803                     Beo Crescent Market  1905.30078
    ## 804                      Berseh Food Centre  3165.51817
    ## 805                       Blk 1 Jalan Kukoh   754.21789
    ## 806                   Blk 105 Hougang Ave 1  9483.28509
    ## 807           Blk 11 Telok Blangah Crescent  2783.20448
    ## 808               Blk 112 Jalan Bukit Merah  1926.10398
    ## 809                Blk 115 Bukit Merah View  2379.03196
    ## 810                  Blk 117 Aljunied Ave 2  6468.97501
    ## 811              Blk 127 Toa Payoh Lorong 1  6178.86002
    ## 812              Blk 137 Tampines Street 11 13289.80451
    ## 813                   Blk 159 Mei Chin Road  4644.26252
    ## 814                 Blk 16 Bedok South Road 11117.97007
    ## 815             Blk 163 Bukit Merah Central  2918.65280
    ## 816             Blk 17 Upper Boon Keng Road  4813.33886
    ## 817                    Blk 20 Ghim Moh Road  6892.65075
    ## 818          Blk 208B New Upper Changi Road 10807.37554
    ## 819              Blk 210 Toa Payoh Lorong 8  6536.50866
    ## 820            Blk 216 Bedok North Street 1 11179.95479
    ## 821               Blk 22 Toa Payoh Lorong 7  6059.62187
    ## 822               Blk 226D Ang Mo Kio Ave 1  9359.69070
    ## 823           Blk 226H Ang Mo Kio Street 22  9405.15953
    ## 824           Blk 254 Jurong East Street 24 13542.12539
    ## 825                   Blk 29 Bendemeer Road  4647.49828
    ## 826                     Blk 320 Shunfu Road  7745.32072
    ## 827                Blk 341 Ang Mo Kio Ave 1  9070.37975
    ## 828               Blk 347 Jurong East Ave 1 14244.23852
    ## 829                  Blk 353 Clementi Ave 2  8800.06537
    ## 830               Blk 36 Telok Blangah Rise  2584.57102
    ## 831              Blk 37A Teban Gardens Road 11949.73825
    ## 832               Blk 409 Ang Mo Kio Ave 10  9007.79178
    ## 833                    Blk 44 Holland Drive  6297.41392
    ## 834                  Blk 448 Clementi Ave 3  9403.27871
    ## 835              Blk 453A Ang Mo Kio Ave 10  9622.37816
    ## 836                       Blk 49 Sims Place  5578.01668
    ## 837                   Blk 4A Eunos Crescent  7997.40324
    ## 838                       Blk 4A Jalan Batu  5055.82762
    ## 839            Blk 4A Woodlands Centre Road 19332.23049
    ## 840                Blk 502 West Coast Drive  9908.06974
    ## 841                Blk 503 West Coast Drive  9843.44419
    ## 842           Blk 505 Jurong West Street 52 15755.77741
    ## 843                  Blk 50A Marine Terrace  8487.01734
    ## 844                 Blk 51 Old Airport Road  5550.82160
    ## 845            Blk 511 Bedok North Street 3 11246.32326
    ## 846  Bukit Panjang Hawker Centre and Market 13156.16700
    ## 847          Our Tampines Hub Hawker Centre 13348.48152
    ## 848         Kampung Admiralty Hawker Centre 18066.92788
    ## 849                    Yishun Hawker Centre 15789.12597
    ## 850    Jurong West Hawker Centre and Market 17486.83526
    ## 851         Pasir Ris Central Hawker Centre 15720.57789
    ## 852                    Dawson Hawker Centre  4600.72599
    ## 853       Woodlands Street 12 Hawker Centre 18157.01969
    ## 854               Blk 527 Ang Mo Kio Ave 10 10089.62372
    ## 855            Blk 538 Bedok North Street 3 10612.24704
    ## 856            Blk 58 New Upper Changi Road 11847.43702
    ## 857               Blk 6 Tanjong Pagar Plaza   621.49112
    ## 858                Blk 628 Ang Mo Kio Ave 4 10927.19027
    ## 859            Blk 630 Bedok Reservoir Road  9691.53574
    ## 860                    Blk 69 Geylang Bahru  5265.72647
    ## 861                      Blk 7 Empress Road  5621.27126
    ## 862                Blk 724 Ang Mo Kio Ave 6  9954.51726
    ## 863          Blk 726 Clementi West Street 2  9099.79635
    ## 864               Blk 74 Toa Payoh Lorong 4  5867.22508
    ## 865               Market Street Food Centre   783.53217
    ## 866                     Maxwell Food Centre   298.05004
    ## 867                      Newton Food Centre  3303.92679
    ## 868  North Bridge Road Market & Food Centre  3476.15787
    ## 869               Pasir Panjang Food Centre  5804.45991
    ## 870            Pek Kio Market & Food Centre  3832.34302
    ## 871               People's Park Food Centre   311.93364
    ## 872             Sembawang Hills Food Centre 10092.50227
    ## 873                 Serangoon Garden Market  9324.18069
    ## 874       Taman Jurong Market & Food Centre 14725.08138
    ## 875                     Tanglin Halt Market  5468.93964
    ## 876                            Tekka Market  2774.01605
    ## 877                      Tiong Bahru Market  1245.76814
    ## 878              Zion Riverside Food Centre  1740.42371
    ## 879               Blk 75 Toa Payoh Lorong 5  6047.75545
    ## 880                     Blk 79 Redhill Lane  2826.48422
    ## 881              Blk 79 Telok Blangah Drive  4076.18855
    ## 882                     Blk 80 Circuit Road  7026.03343
    ## 883              Blk 82 Telok Blangah Drive  4027.77599
    ## 884            Blk 84 Marine Parade Central  7370.60270
    ## 885             Blk 85 Bedok North Street 4 11973.08015
    ## 886                     Blk 85 Redhill Lane  2821.17918
    ## 887                     Blk 89 Circuit Road  6549.52345
    ## 888                    Blk 90 Whampoa Drive  4693.94663
    ## 889               Blk 93 Toa Payoh Lorong 4  6258.53465
    ## 890                    Blks 13/14 Haig Road  6876.73968
    ## 891           Blks 160/162 Ang Mo Kio Ave 4 10210.50268
    ## 892                   Ci Yuan Hawker Centre 11216.51675
    ## 893         Chong Pang Market & Food Centre  8551.48935
    ## 894                      Dunman Food Centre  7222.22058
    ## 895          East Coast Lagoon Food Village  9928.21429
    ## 896                    Geylang Serai Market  6326.43470
    ## 897                 Golden Mile Food Centre  6790.52737
    ## 898    Holland Village Market & Food Centre  9918.95711
    ## 899           Hong Lim Market & Food Centre  9032.65410
    ## 900                   Kallang Estate Market  6582.20953
    ## 901              Kovan Market & Food Centre  2236.14929
    ## 902          Blks 2 & 3 Changi Village Road 13822.30841
    ## 903            Commonwealth Crescent Market  9739.74767
    ## 904     ABC Brickworks Market & Food Centre 10736.19826
    ## 905                        Adam Food Centre  7322.88899
    ## 906      Albert Centre Market & Food Centre  7124.59644
    ## 907           Alexandra Village Food Centre 11042.53019
    ## 908                 Amoy Street Food Centre  9658.09806
    ## 909                       Bedok Food Centre 11022.77364
    ## 910                     Beo Crescent Market  9409.24425
    ## 911                      Berseh Food Centre  6382.44204
    ## 912                       Blk 1 Jalan Kukoh  8909.27685
    ## 913                   Blk 105 Hougang Ave 1  2861.12883
    ## 914           Blk 11 Telok Blangah Crescent 10984.59939
    ## 915               Blk 112 Jalan Bukit Merah 10347.49516
    ## 916                Blk 115 Bukit Merah View 10014.31088
    ## 917                  Blk 117 Aljunied Ave 2  5330.83869
    ## 918              Blk 127 Toa Payoh Lorong 1  3773.63012
    ## 919              Blk 137 Tampines Street 11  8930.12629
    ## 920                   Blk 159 Mei Chin Road 10572.19132
    ## 921                 Blk 16 Bedok South Road  9071.69394
    ## 922             Blk 163 Bukit Merah Central 10473.99685
    ## 923             Blk 17 Upper Boon Keng Road  5469.68218
    ## 924                    Blk 20 Ghim Moh Road 10516.19856
    ## 925          Blk 208B New Upper Changi Road  8304.93611
    ## 926              Blk 210 Toa Payoh Lorong 8  2974.77235
    ## 927            Blk 216 Bedok North Street 1  8483.26090
    ## 928               Blk 22 Toa Payoh Lorong 7  3376.16030
    ## 929               Blk 226D Ang Mo Kio Ave 1  3051.84507
    ## 930           Blk 226H Ang Mo Kio Street 22  2976.92751
    ## 931           Blk 254 Jurong East Street 24 14512.13274
    ## 932                   Blk 29 Bendemeer Road  4992.49181
    ## 933                     Blk 320 Shunfu Road  3550.69587
    ## 934                Blk 341 Ang Mo Kio Ave 1  2034.60576
    ## 935               Blk 347 Jurong East Ave 1 15164.05760
    ## 936                  Blk 353 Clementi Ave 2 11992.76113
    ## 937               Blk 36 Telok Blangah Rise 11282.45340
    ## 938              Blk 37A Teban Gardens Road 14572.39174
    ## 939               Blk 409 Ang Mo Kio Ave 10  1253.11873
    ## 940                    Blk 44 Holland Drive 10289.91266
    ## 941                  Blk 448 Clementi Ave 3 12675.88666
    ## 942              Blk 453A Ang Mo Kio Ave 10  1217.15888
    ## 943                       Blk 49 Sims Place  5406.57670
    ## 944                   Blk 4A Eunos Crescent  6417.65874
    ## 945                       Blk 4A Jalan Batu  7103.76739
    ## 946            Blk 4A Woodlands Centre Road 13673.68568
    ## 947                Blk 502 West Coast Drive 13276.87906
    ## 948                Blk 503 West Coast Drive 13222.49408
    ## 949           Blk 505 Jurong West Street 52 16559.29725
    ## 950                  Blk 50A Marine Terrace  8472.81110
    ## 951                 Blk 51 Old Airport Road  6551.62258
    ## 952            Blk 511 Bedok North Street 3  7910.19078
    ## 953  Bukit Panjang Hawker Centre and Market 10570.16712
    ## 954          Our Tampines Hub Hawker Centre  8225.46783
    ## 955         Kampung Admiralty Hawker Centre 11124.72237
    ## 956                    Yishun Hawker Centre  7145.73322
    ## 957    Jurong West Hawker Centre and Market 18993.05355
    ## 958         Pasir Ris Central Hawker Centre  9514.85764
    ## 959                    Dawson Hawker Centre 10089.61854
    ## 960       Woodlands Street 12 Hawker Centre 12326.44008
    ## 961               Blk 527 Ang Mo Kio Ave 10  1640.53019
    ## 962            Blk 538 Bedok North Street 3  7414.88090
    ## 963            Blk 58 New Upper Changi Road  9408.60471
    ## 964               Blk 6 Tanjong Pagar Plaza 10030.91613
    ## 965                Blk 628 Ang Mo Kio Ave 4  3424.58102
    ## 966            Blk 630 Bedok Reservoir Road  6306.15610
    ## 967                    Blk 69 Geylang Bahru  4744.60178
    ## 968                      Blk 7 Empress Road  8607.24615
    ## 969                Blk 724 Ang Mo Kio Ave 6  2395.58589
    ## 970          Blk 726 Clementi West Street 2 13199.19971
    ## 971               Blk 74 Toa Payoh Lorong 4  3658.98597
    ## 972               Market Street Food Centre  9073.87924
    ## 973                     Maxwell Food Centre  9609.78657
    ## 974                      Newton Food Centre  6519.30007
    ## 975  North Bridge Road Market & Food Centre  6471.25520
    ## 976               Pasir Panjang Food Centre 12856.63181
    ## 977            Pek Kio Market & Food Centre  5616.60622
    ## 978               People's Park Food Centre  9158.36715
    ## 979             Sembawang Hills Food Centre  4274.20189
    ## 980                 Serangoon Garden Market   120.57103
    ## 981       Taman Jurong Market & Food Centre 16468.10768
    ## 982                     Tanglin Halt Market 10393.30801
    ## 983                            Tekka Market  6658.51689
    ## 984                      Tiong Bahru Market  9542.24747
    ## 985              Zion Riverside Food Centre  8868.94202
    ## 986               Blk 75 Toa Payoh Lorong 5  3464.83563
    ## 987                     Blk 79 Redhill Lane 10000.35568
    ## 988              Blk 79 Telok Blangah Drive 11996.70216
    ## 989                     Blk 80 Circuit Road  4629.60536
    ## 990              Blk 82 Telok Blangah Drive 11926.92301
    ## 991            Blk 84 Marine Parade Central  8156.92165
    ## 992             Blk 85 Bedok North Street 4  8790.09833
    ## 993                     Blk 85 Redhill Lane 10053.42443
    ## 994                     Blk 89 Circuit Road  4976.62807
    ## 995                    Blk 90 Whampoa Drive  4738.10208
    ## 996               Blk 93 Toa Payoh Lorong 4  3420.28868
    ## 997                    Blks 13/14 Haig Road  6318.94748
    ## 998           Blks 160/162 Ang Mo Kio Ave 4  3245.37032
    ## 999                   Ci Yuan Hawker Centre  2209.51433
    ## 1000                     Dunman Food Centre 15767.19547
    ## 1001         East Coast Lagoon Food Village 18186.26788
    ## 1002                   Geylang Serai Market 14867.95788
    ## 1003                Golden Mile Food Centre 14750.99678
    ## 1004   Holland Village Market & Food Centre 13845.49153
    ## 1005          Hong Lim Market & Food Centre 16280.18782
    ## 1006                  Kallang Estate Market 15047.65842
    ## 1007             Kovan Market & Food Centre 10238.39518
    ## 1008         Blks 2 & 3 Changi Village Road 18378.34054
    ## 1009           Commonwealth Crescent Market 14124.62079
    ## 1010    ABC Brickworks Market & Food Centre 16138.15328
    ## 1011                       Adam Food Centre 11973.49367
    ## 1012     Albert Centre Market & Food Centre 14704.34392
    ## 1013          Alexandra Village Food Centre 16271.54272
    ## 1014                Amoy Street Food Centre 16952.42161
    ## 1015                      Bedok Food Centre 18720.38863
    ## 1016                    Beo Crescent Market 15771.35674
    ## 1017                     Berseh Food Centre 14080.73829
    ## 1018                      Blk 1 Jalan Kukoh 15887.03251
    ## 1019                  Blk 105 Hougang Ave 1 10979.75365
    ## 1020          Blk 11 Telok Blangah Crescent 17073.03146
    ## 1021              Blk 112 Jalan Bukit Merah 16748.13707
    ## 1022               Blk 115 Bukit Merah View 16153.76859
    ## 1023                 Blk 117 Aljunied Ave 2 13872.99455
    ## 1024             Blk 127 Toa Payoh Lorong 1 10478.85848
    ## 1025             Blk 137 Tampines Street 11 16028.92406
    ## 1026                  Blk 159 Mei Chin Road 15546.69156
    ## 1027                Blk 16 Bedok South Road 17092.75779
    ## 1028            Blk 163 Bukit Merah Central 16388.44908
    ## 1029            Blk 17 Upper Boon Keng Road 13736.97350
    ## 1030                   Blk 20 Ghim Moh Road 14053.98615
    ## 1031         Blk 208B New Upper Changi Road 16314.33755
    ## 1032             Blk 210 Toa Payoh Lorong 8 10485.57166
    ## 1033           Blk 216 Bedok North Street 1 16399.68842
    ## 1034              Blk 22 Toa Payoh Lorong 7 11101.75506
    ## 1035              Blk 226D Ang Mo Kio Ave 1  7248.14610
    ## 1036          Blk 226H Ang Mo Kio Street 22  7213.59734
    ## 1037          Blk 254 Jurong East Street 24 14025.22032
    ## 1038                  Blk 29 Bendemeer Road 12989.72329
    ## 1039                    Blk 320 Shunfu Road  8834.81669
    ## 1040               Blk 341 Ang Mo Kio Ave 1  7762.94294
    ## 1041              Blk 347 Jurong East Ave 1 14392.41844
    ## 1042                 Blk 353 Clementi Ave 2 14451.16340
    ## 1043              Blk 36 Telok Blangah Rise 17585.44854
    ## 1044             Blk 37A Teban Gardens Road 15505.08077
    ## 1045              Blk 409 Ang Mo Kio Ave 10  8160.12851
    ## 1046                   Blk 44 Holland Drive 14206.13264
    ## 1047                 Blk 448 Clementi Ave 3 14884.41879
    ## 1048             Blk 453A Ang Mo Kio Ave 10  7643.87778
    ## 1049                      Blk 49 Sims Place 13855.61525
    ## 1050                  Blk 4A Eunos Crescent 14898.88511
    ## 1051                      Blk 4A Jalan Batu 15542.62485
    ## 1052           Blk 4A Woodlands Centre Road  6587.00517
    ## 1053               Blk 502 West Coast Drive 15309.24624
    ## 1054               Blk 503 West Coast Drive 15284.80461
    ## 1055          Blk 505 Jurong West Street 52 15229.08699
    ## 1056                 Blk 50A Marine Terrace 16950.85685
    ## 1057                Blk 51 Old Airport Road 15040.84881
    ## 1058           Blk 511 Bedok North Street 3 15713.84473
    ## 1059 Bukit Panjang Hawker Centre and Market  8623.03537
    ## 1060         Our Tampines Hub Hawker Centre 15056.93351
    ## 1061        Kampung Admiralty Hawker Centre  3237.24443
    ## 1062                   Yishun Hawker Centre  1938.99048
    ## 1063   Jurong West Hawker Centre and Market 17675.42958
    ## 1064        Pasir Ris Central Hawker Centre 15117.35804
    ## 1065                   Dawson Hawker Centre 15048.21461
    ## 1066      Woodlands Street 12 Hawker Centre  5430.36830
    ## 1067              Blk 527 Ang Mo Kio Ave 10  7101.73811
    ## 1068           Blk 538 Bedok North Street 3 15372.80884
    ## 1069           Blk 58 New Upper Changi Road 17253.30547
    ## 1070              Blk 6 Tanjong Pagar Plaza 17197.88841
    ## 1071               Blk 628 Ang Mo Kio Ave 4  5736.97312
    ## 1072           Blk 630 Bedok Reservoir Road 14446.09362
    ## 1073                   Blk 69 Geylang Bahru 13007.73144
    ## 1074                     Blk 7 Empress Road 12990.90149
    ## 1075               Blk 724 Ang Mo Kio Ave 6  6850.38778
    ## 1076         Blk 726 Clementi West Street 2 15825.36765
    ## 1077              Blk 74 Toa Payoh Lorong 4 11028.19322
    ## 1078              Market Street Food Centre 16493.05209
    ## 1079                    Maxwell Food Centre 16831.81987
    ## 1080                     Newton Food Centre 13276.63631
    ## 1081 North Bridge Road Market & Food Centre 14443.50388
    ## 1082              Pasir Panjang Food Centre 17688.39330
    ## 1083           Pek Kio Market & Food Centre 12974.40125
    ## 1084              People's Park Food Centre 16269.64583
    ## 1085            Sembawang Hills Food Centre  6531.05767
    ## 1086                Serangoon Garden Market  8665.63059
    ## 1087      Taman Jurong Market & Food Centre 16017.11180
    ## 1088                    Tanglin Halt Market 14851.45772
    ## 1089                           Tekka Market 14063.23628
    ## 1090                     Tiong Bahru Market 16189.23643
    ## 1091             Zion Riverside Food Centre 15384.41014
    ## 1092              Blk 75 Toa Payoh Lorong 5 10893.79209
    ## 1093                    Blk 79 Redhill Lane 15917.41647
    ## 1094             Blk 79 Telok Blangah Drive 17633.84423
    ## 1095                    Blk 80 Circuit Road 13181.09417
    ## 1096             Blk 82 Telok Blangah Drive 17569.42052
    ## 1097           Blk 84 Marine Parade Central 16701.67329
    ## 1098            Blk 85 Bedok North Street 4 16472.45031
    ## 1099                    Blk 85 Redhill Lane 15976.08268
    ## 1100                    Blk 89 Circuit Road 13517.91061
    ## 1101                   Blk 90 Whampoa Drive 12357.69003
    ## 1102              Blk 93 Toa Payoh Lorong 4 10542.17002
    ## 1103                   Blks 13/14 Haig Road 14869.68735
    ## 1104          Blks 160/162 Ang Mo Kio Ave 4  6409.28596
    ## 1105                  Ci Yuan Hawker Centre  8664.52679
    ## 1106         East Coast Lagoon Food Village  3706.23267
    ## 1107                   Geylang Serai Market   902.58118
    ## 1108                Golden Mile Food Centre  4284.03667
    ## 1109   Holland Village Market & Food Centre 11905.03490
    ## 1110          Hong Lim Market & Food Centre  6781.72396
    ## 1111                  Kallang Estate Market  1981.88726
    ## 1112             Kovan Market & Food Centre  5772.19321
    ## 1113         Blks 2 & 3 Changi Village Road 13047.55770
    ## 1114           Commonwealth Crescent Market 11332.10545
    ## 1115    ABC Brickworks Market & Food Centre 10719.36274
    ## 1116                       Adam Food Centre  9890.57355
    ## 1117     Albert Centre Market & Food Centre  5379.97881
    ## 1118          Alexandra Village Food Centre 11130.10677
    ## 1119                Amoy Street Food Centre  6993.79699
    ## 1120                      Bedok Food Centre  6091.97962
    ## 1121                    Beo Crescent Market  8594.63231
    ## 1122                     Berseh Food Centre  5010.14426
    ## 1123                      Blk 1 Jalan Kukoh  7274.06750
    ## 1124                  Blk 105 Hougang Ave 1  5083.00070
    ## 1125          Blk 11 Telok Blangah Crescent  9912.64033
    ## 1126              Blk 112 Jalan Bukit Merah  9042.20283
    ## 1127               Blk 115 Bukit Merah View  9263.61099
    ## 1128                 Blk 117 Aljunied Ave 2  2063.55610
    ## 1129             Blk 127 Toa Payoh Lorong 1  7101.30656
    ## 1130             Blk 137 Tampines Street 11  6223.53810
    ## 1131                  Blk 159 Mei Chin Road 11153.43524
    ## 1132                Blk 16 Bedok South Road  3949.00155
    ## 1133            Blk 163 Bukit Merah Central  9860.24194
    ## 1134            Blk 17 Upper Boon Keng Road  3418.35148
    ## 1135                   Blk 20 Ghim Moh Road 12643.20952
    ## 1136         Blk 208B New Upper Changi Road  3628.32093
    ## 1137             Blk 210 Toa Payoh Lorong 8  6284.60255
    ## 1138           Blk 216 Bedok North Street 1  4002.12588
    ## 1139              Blk 22 Toa Payoh Lorong 7  5744.18502
    ## 1140              Blk 226D Ang Mo Kio Ave 1  9421.92417
    ## 1141          Blk 226H Ang Mo Kio Street 22  9395.91116
    ## 1142          Blk 254 Jurong East Street 24 18640.13720
    ## 1143                  Blk 29 Bendemeer Road  4452.44662
    ## 1144                    Blk 320 Shunfu Road  8612.65269
    ## 1145               Blk 341 Ang Mo Kio Ave 1  8492.09310
    ## 1146              Blk 347 Jurong East Ave 1 19357.93629
    ## 1147                 Blk 353 Clementi Ave 2 14588.05581
    ## 1148              Blk 36 Telok Blangah Rise  9770.17296
    ## 1149             Blk 37A Teban Gardens Road 17732.90548
    ## 1150              Blk 409 Ang Mo Kio Ave 10  7844.43828
    ## 1151                   Blk 44 Holland Drive 12137.24729
    ## 1152                 Blk 448 Clementi Ave 3 15287.75054
    ## 1153             Blk 453A Ang Mo Kio Ave 10  8241.50368
    ## 1154                      Blk 49 Sims Place  2634.05088
    ## 1155                  Blk 4A Eunos Crescent  1237.27233
    ## 1156                      Blk 4A Jalan Batu  2135.40815
    ## 1157           Blk 4A Woodlands Centre Road 20642.90225
    ## 1158               Blk 502 West Coast Drive 15878.96977
    ## 1159               Blk 503 West Coast Drive 15813.68639
    ## 1160          Blk 505 Jurong West Street 52 20887.20239
    ## 1161                 Blk 50A Marine Terrace  1602.38381
    ## 1162                Blk 51 Old Airport Road  1784.39010
    ## 1163           Blk 511 Bedok North Street 3  4147.41942
    ## 1164 Bukit Panjang Hawker Centre and Market 16248.81320
    ## 1165         Our Tampines Hub Hawker Centre  6476.65306
    ## 1166        Kampung Admiralty Hawker Centre 18301.53629
    ## 1167                   Yishun Hawker Centre 14274.09013
    ## 1168   Jurong West Hawker Centre and Market 23020.90566
    ## 1169        Pasir Ris Central Hawker Centre  8984.83349
    ## 1170                   Dawson Hawker Centre 10887.64112
    ## 1171      Woodlands Street 12 Hawker Centre 19308.99774
    ## 1172              Blk 527 Ang Mo Kio Ave 10  8765.66778
    ## 1173           Blk 538 Bedok North Street 3  3560.85968
    ## 1174           Blk 58 New Upper Changi Road  4669.58664
    ## 1175              Blk 6 Tanjong Pagar Plaza  7467.96058
    ## 1176               Blk 628 Ang Mo Kio Ave 4 10440.39199
    ## 1177           Blk 630 Bedok Reservoir Road  2957.35999
    ## 1178                   Blk 69 Geylang Bahru  3783.63561
    ## 1179                     Blk 7 Empress Road 10731.51318
    ## 1180               Blk 724 Ang Mo Kio Ave 6  9277.84092
    ## 1181         Blk 726 Clementi West Street 2 15321.90201
    ## 1182              Blk 74 Toa Payoh Lorong 4  6202.61818
    ## 1183              Market Street Food Centre  6419.44105
    ## 1184                    Maxwell Food Centre  7122.88266
    ## 1185                     Newton Food Centre  6934.64196
    ## 1186 North Bridge Road Market & Food Centre  4240.95367
    ## 1187              Pasir Panjang Food Centre 12835.42832
    ## 1188           Pek Kio Market & Food Centre  5790.83768
    ## 1189              People's Park Food Centre  7125.21990
    ## 1190            Sembawang Hills Food Centre 10685.45937
    ## 1191                Serangoon Garden Market  7110.75830
    ## 1192      Taman Jurong Market & Food Centre 20258.36590
    ## 1193                    Tanglin Halt Market 11635.58609
    ## 1194                           Tekka Market  5719.57881
    ## 1195                     Tiong Bahru Market  8186.61604
    ## 1196             Zion Riverside Food Centre  8084.61432
    ## 1197              Blk 75 Toa Payoh Lorong 5  6185.76081
    ## 1198                    Blk 79 Redhill Lane  9585.27308
    ## 1199             Blk 79 Telok Blangah Drive 11215.58658
    ## 1200                    Blk 80 Circuit Road  2613.54087
    ## 1201             Blk 82 Telok Blangah Drive 11161.28704
    ## 1202           Blk 84 Marine Parade Central   934.72501
    ## 1203            Blk 85 Bedok North Street 4  4806.35410
    ## 1204                    Blk 85 Redhill Lane  9606.79679
    ## 1205                    Blk 89 Circuit Road  2393.43781
    ## 1206                   Blk 90 Whampoa Drive  5406.92784
    ## 1207              Blk 93 Toa Payoh Lorong 4  6649.71356
    ## 1208                   Blks 13/14 Haig Road   937.89736
    ## 1209          Blks 160/162 Ang Mo Kio Ave 4 10016.44846
    ## 1210                  Ci Yuan Hawker Centre  7594.95318
    ## 1211                   Geylang Serai Market  4242.59457
    ## 1212                Golden Mile Food Centre  7927.70615
    ## 1213   Holland Village Market & Food Centre 15604.80466
    ## 1214          Hong Lim Market & Food Centre 10206.05018
    ## 1215                  Kallang Estate Market  5663.75459
    ## 1216             Kovan Market & Food Centre  7962.52262
    ## 1217         Blks 2 & 3 Changi Village Road 10874.04788
    ## 1218           Commonwealth Crescent Market 15022.85364
    ## 1219    ABC Brickworks Market & Food Centre 14289.15651
    ## 1220                       Adam Food Centre 13587.34050
    ## 1221     Albert Centre Market & Food Centre  9014.80276
    ## 1222          Alexandra Village Food Centre 14700.42918
    ## 1223                Amoy Street Food Centre 10297.32025
    ## 1224                      Bedok Food Centre  2732.91514
    ## 1225                    Beo Crescent Market 12143.17238
    ## 1226                     Berseh Food Centre  8699.09724
    ## 1227                      Blk 1 Jalan Kukoh 10775.51116
    ## 1228                  Blk 105 Hougang Ave 1  7224.54907
    ## 1229          Blk 11 Telok Blangah Crescent 13351.09199
    ## 1230              Blk 112 Jalan Bukit Merah 12484.69758
    ## 1231               Blk 115 Bukit Merah View 12788.54508
    ## 1232                 Blk 117 Aljunied Ave 2  5560.37892
    ## 1233             Blk 127 Toa Payoh Lorong 1 10631.11889
    ## 1234             Blk 137 Tampines Street 11  4438.19982
    ## 1235                  Blk 159 Mei Chin Road 14777.98653
    ## 1236                Blk 16 Bedok South Road  1534.53913
    ## 1237            Blk 163 Bukit Merah Central 13378.85399
    ## 1238            Blk 17 Upper Boon Keng Road  7115.50111
    ## 1239                   Blk 20 Ghim Moh Road 16342.88130
    ## 1240         Blk 208B New Upper Changi Road  2122.58671
    ## 1241             Blk 210 Toa Payoh Lorong 8  9710.18510
    ## 1242           Blk 216 Bedok North Street 1  2262.14795
    ## 1243              Blk 22 Toa Payoh Lorong 7  9234.73403
    ## 1244              Blk 226D Ang Mo Kio Ave 1 12561.92118
    ## 1245          Blk 226H Ang Mo Kio Street 22 12519.92075
    ## 1246          Blk 254 Jurong East Street 24 22323.47068
    ## 1247                  Blk 29 Bendemeer Road  8131.36249
    ## 1248                    Blk 320 Shunfu Road 12001.21442
    ## 1249               Blk 341 Ang Mo Kio Ave 1 11556.27166
    ## 1250              Blk 347 Jurong East Ave 1 23039.87581
    ## 1251                 Blk 353 Clementi Ave 2 18291.43478
    ## 1252              Blk 36 Telok Blangah Rise 13127.39846
    ## 1253             Blk 37A Teban Gardens Road 21438.98061
    ## 1254              Blk 409 Ang Mo Kio Ave 10 10817.92594
    ## 1255                   Blk 44 Holland Drive 15831.12156
    ## 1256                 Blk 448 Clementi Ave 3 18989.69124
    ## 1257             Blk 453A Ang Mo Kio Ave 10 11089.30784
    ## 1258                      Blk 49 Sims Place  6293.66047
    ## 1259                  Blk 4A Eunos Crescent  3740.73616
    ## 1260                      Blk 4A Jalan Batu  5703.84880
    ## 1261           Blk 4A Woodlands Centre Road 23599.72809
    ## 1262               Blk 502 West Coast Drive 19578.99929
    ## 1263               Blk 503 West Coast Drive 19513.61706
    ## 1264          Blk 505 Jurong West Street 52 24565.57240
    ## 1265                 Blk 50A Marine Terrace  2145.24056
    ## 1266                Blk 51 Old Airport Road  5476.41743
    ## 1267           Blk 511 Bedok North Street 3  2977.89765
    ## 1268 Bukit Panjang Hawker Centre and Market 19714.16397
    ## 1269         Our Tampines Hub Hawker Centre  5242.95003
    ## 1270        Kampung Admiralty Hawker Centre 20988.99852
    ## 1271                   Yishun Hawker Centre 16496.34182
    ## 1272   Jurong West Hawker Centre and Market 26718.84861
    ## 1273        Pasir Ris Central Hawker Centre  7606.56738
    ## 1274                   Dawson Hawker Centre 14537.01251
    ## 1275      Woodlands Street 12 Hawker Centre 22252.98302
    ## 1276              Blk 527 Ang Mo Kio Ave 10 11563.99210
    ## 1277           Blk 538 Bedok North Street 3  2997.13239
    ## 1278           Blk 58 New Upper Changi Road  2051.63451
    ## 1279              Blk 6 Tanjong Pagar Plaza 10749.25655
    ## 1280               Blk 628 Ang Mo Kio Ave 4 13333.72005
    ## 1281           Blk 630 Bedok Reservoir Road  3743.36344
    ## 1282                   Blk 69 Geylang Bahru  7417.48603
    ## 1283                     Blk 7 Empress Road 14437.57733
    ## 1284               Blk 724 Ang Mo Kio Ave 6 12226.06232
    ## 1285         Blk 726 Clementi West Street 2 19005.88180
    ## 1286              Blk 74 Toa Payoh Lorong 4  9739.23975
    ## 1287              Market Street Food Centre  9789.15232
    ## 1288                    Maxwell Food Centre 10458.45852
    ## 1289                     Newton Food Centre 10638.74529
    ## 1290 North Bridge Road Market & Food Centre  7916.29494
    ## 1291              Pasir Panjang Food Centre 16341.30680
    ## 1292           Pek Kio Market & Food Centre  9494.50512
    ## 1293              People's Park Food Centre 10563.49654
    ## 1294            Sembawang Hills Food Centre 13856.35273
    ## 1295                Serangoon Garden Market  9835.38502
    ## 1296      Taman Jurong Market & Food Centre 23959.62423
    ## 1297                    Tanglin Halt Market 15304.25643
    ## 1298                           Tekka Market  9402.64011
    ## 1299                     Tiong Bahru Market 11672.91834
    ## 1300             Zion Riverside Food Centre 11663.39119
    ## 1301              Blk 75 Toa Payoh Lorong 5  9693.73947
    ## 1302                    Blk 79 Redhill Lane 13144.01963
    ## 1303             Blk 79 Telok Blangah Drive 14648.22002
    ## 1304                    Blk 80 Circuit Road  5820.71807
    ## 1305             Blk 82 Telok Blangah Drive 14599.01621
    ## 1306           Blk 84 Marine Parade Central  3228.48342
    ## 1307            Blk 85 Bedok North Street 4  2828.24914
    ## 1308                    Blk 85 Redhill Lane 13160.37725
    ## 1309                    Blk 89 Circuit Road  5819.40372
    ## 1310                   Blk 90 Whampoa Drive  9069.74252
    ## 1311              Blk 93 Toa Payoh Lorong 4 10144.19652
    ## 1312                   Blks 13/14 Haig Road  4484.26513
    ## 1313          Blks 160/162 Ang Mo Kio Ave 4 13034.04715
    ## 1314                  Ci Yuan Hawker Centre  9556.23714
    ## 1315                Golden Mile Food Centre  4117.36980
    ## 1316   Holland Village Market & Food Centre 11521.12166
    ## 1317          Hong Lim Market & Food Centre  6792.14044
    ## 1318                  Kallang Estate Market  1877.52858
    ## 1319             Kovan Market & Food Centre  4881.53750
    ## 1320         Blks 2 & 3 Changi Village Road 12825.75537
    ## 1321           Commonwealth Crescent Market 10981.23636
    ## 1322    ABC Brickworks Market & Food Centre 10553.61601
    ## 1323                       Adam Food Centre  9390.74799
    ## 1324     Albert Centre Market & Food Centre  5197.73009
    ## 1325          Alexandra Village Food Centre 10961.10866
    ## 1326                Amoy Street Food Centre  7085.40579
    ## 1327                      Bedok Food Centre  6383.97112
    ## 1328                    Beo Crescent Market  8468.93323
    ## 1329                     Berseh Food Centre  4719.79192
    ## 1330                      Blk 1 Jalan Kukoh  7210.81515
    ## 1331                  Blk 105 Hougang Ave 1  4202.23816
    ## 1332          Blk 11 Telok Blangah Crescent  9868.47574
    ## 1333              Blk 112 Jalan Bukit Merah  9004.08669
    ## 1334               Blk 115 Bukit Merah View  9153.65330
    ## 1335                 Blk 117 Aljunied Ave 2  1320.22865
    ## 1336             Blk 127 Toa Payoh Lorong 1  6404.79194
    ## 1337             Blk 137 Tampines Street 11  6073.34389
    ## 1338                  Blk 159 Mei Chin Road 10921.69328
    ## 1339                Blk 16 Bedok South Road  4174.24575
    ## 1340            Blk 163 Bukit Merah Central  9750.50062
    ## 1341            Blk 17 Upper Boon Keng Road  2966.73579
    ## 1342                   Blk 20 Ghim Moh Road 12257.63936
    ## 1343         Blk 208B New Upper Changi Road  3699.07508
    ## 1344             Blk 210 Toa Payoh Lorong 8  5528.40400
    ## 1345           Blk 216 Bedok North Street 1  4059.01823
    ## 1346              Blk 22 Toa Payoh Lorong 7  5019.73541
    ## 1347              Blk 226D Ang Mo Kio Ave 1  8587.35827
    ## 1348          Blk 226H Ang Mo Kio Street 22  8557.94701
    ## 1349          Blk 254 Jurong East Street 24 18098.35375
    ## 1350                  Blk 29 Bendemeer Road  3927.39882
    ## 1351                    Blk 320 Shunfu Road  7848.25486
    ## 1352               Blk 341 Ang Mo Kio Ave 1  7640.04615
    ## 1353              Blk 347 Jurong East Ave 1 18813.23116
    ## 1354                 Blk 353 Clementi Ave 2 14179.41240
    ## 1355              Blk 36 Telok Blangah Rise  9785.11249
    ## 1356             Blk 37A Teban Gardens Road 17293.01465
    ## 1357              Blk 409 Ang Mo Kio Ave 10  6975.92714
    ## 1358                   Blk 44 Holland Drive 11774.20203
    ## 1359                 Blk 448 Clementi Ave 3 14885.98626
    ## 1360             Blk 453A Ang Mo Kio Ave 10  7359.32693
    ## 1361                      Blk 49 Sims Place  2093.43761
    ## 1362                  Blk 4A Eunos Crescent   781.09508
    ## 1363                      Blk 4A Jalan Batu  2242.87804
    ## 1364           Blk 4A Woodlands Centre Road 19791.09835
    ## 1365               Blk 502 West Coast Drive 15485.09392
    ## 1366               Blk 503 West Coast Drive 15420.33866
    ## 1367          Blk 505 Jurong West Street 52 20335.61711
    ## 1368                 Blk 50A Marine Terrace  2297.00962
    ## 1369                Blk 51 Old Airport Road  1667.39346
    ## 1370           Blk 511 Bedok North Street 3  4039.80934
    ## 1371 Bukit Panjang Hawker Centre and Market 15527.60942
    ## 1372         Our Tampines Hub Hawker Centre  6180.01385
    ## 1373        Kampung Admiralty Hawker Centre 17418.29480
    ## 1374                   Yishun Hawker Centre 13371.50955
    ## 1375   Jurong West Hawker Centre and Market 22513.24310
    ## 1376        Pasir Ris Central Hawker Centre  8632.85470
    ## 1377                   Dawson Hawker Centre 10621.98484
    ## 1378      Woodlands Street 12 Hawker Centre 18454.44107
    ## 1379              Blk 527 Ang Mo Kio Ave 10  7880.19471
    ## 1380           Blk 538 Bedok North Street 3  3398.10951
    ## 1381           Blk 58 New Upper Changi Road  4845.96575
    ## 1382              Blk 6 Tanjong Pagar Plaza  7564.32913
    ## 1383               Blk 628 Ang Mo Kio Ave 4  9568.43447
    ## 1384           Blk 630 Bedok Reservoir Road  2530.71209
    ## 1385                   Blk 69 Geylang Bahru  3183.92670
    ## 1386                     Blk 7 Empress Road 10304.27837
    ## 1387               Blk 724 Ang Mo Kio Ave 6  8410.16785
    ## 1388         Blk 726 Clementi West Street 2 14977.04890
    ## 1389              Blk 74 Toa Payoh Lorong 4  5508.85447
    ## 1390              Market Street Food Centre  6478.64480
    ## 1391                    Maxwell Food Centre  7190.73693
    ## 1392                     Newton Food Centre  6550.33900
    ## 1393 North Bridge Road Market & Food Centre  4008.65089
    ## 1394              Pasir Panjang Food Centre 12716.35719
    ## 1395           Pek Kio Market & Food Centre  5342.22044
    ## 1396              People's Park Food Centre  7118.06149
    ## 1397            Sembawang Hills Food Centre  9860.77496
    ## 1398                Serangoon Garden Market  6215.76803
    ## 1399      Taman Jurong Market & Food Centre 19764.35494
    ## 1400                    Tanglin Halt Market 11333.55332
    ## 1401                           Tekka Market  5433.84592
    ## 1402                     Tiong Bahru Market  8123.34885
    ## 1403             Zion Riverside Food Centre  7932.73663
    ## 1404              Blk 75 Toa Payoh Lorong 5  5473.21656
    ## 1405                    Blk 79 Redhill Lane  9439.60527
    ## 1406             Blk 79 Telok Blangah Drive 11164.60145
    ## 1407                    Blk 80 Circuit Road  1742.73905
    ## 1408             Blk 82 Telok Blangah Drive 11106.93297
    ## 1409           Blk 84 Marine Parade Central  1834.66471
    ## 1410            Blk 85 Bedok North Street 4  4813.56512
    ## 1411                    Blk 85 Redhill Lane  9466.14301
    ## 1412                    Blk 89 Circuit Road  1604.06334
    ## 1413                   Blk 90 Whampoa Drive  4844.11171
    ## 1414              Blk 93 Toa Payoh Lorong 4  5930.40492
    ## 1415                   Blks 13/14 Haig Road   339.76853
    ## 1416          Blks 160/162 Ang Mo Kio Ave 4  9160.80624
    ## 1417                  Ci Yuan Hawker Centre  6708.94572
    ## 1418   Holland Village Market & Food Centre  7732.50243
    ## 1419          Hong Lim Market & Food Centre  2800.29180
    ## 1420                  Kallang Estate Market  2308.07839
    ## 1421             Kovan Market & Food Centre  6687.60953
    ## 1422         Blks 2 & 3 Changi Village Road 16809.59860
    ## 1423           Commonwealth Crescent Market  7120.75403
    ## 1424    ABC Brickworks Market & Food Centre  6450.02460
    ## 1425                       Adam Food Centre  6011.79006
    ## 1426     Albert Centre Market & Food Centre  1096.12733
    ## 1427          Alexandra Village Food Centre  6859.49553
    ## 1428                Amoy Street Food Centre  3246.48360
    ## 1429                      Bedok Food Centre 10374.85634
    ## 1430                    Beo Crescent Market  4351.83637
    ## 1431                     Berseh Food Centre   924.82903
    ## 1432                      Blk 1 Jalan Kukoh  3117.57725
    ## 1433                  Blk 105 Hougang Ave 1  6344.67307
    ## 1434          Blk 11 Telok Blangah Crescent  5771.22851
    ## 1435              Blk 112 Jalan Bukit Merah  4915.11006
    ## 1436               Blk 115 Bukit Merah View  5036.39863
    ## 1437                 Blk 117 Aljunied Ave 2  3239.08555
    ## 1438             Blk 127 Toa Payoh Lorong 1  4439.27032
    ## 1439             Blk 137 Tampines Street 11 10153.94930
    ## 1440                  Blk 159 Mei Chin Road  6869.65043
    ## 1441                Blk 16 Bedok South Road  8210.66665
    ## 1442            Blk 163 Bukit Merah Central  5633.15893
    ## 1443            Blk 17 Upper Boon Keng Road  1595.97440
    ## 1444                   Blk 20 Ghim Moh Road  8467.18201
    ## 1445         Blk 208B New Upper Changi Road  7801.91902
    ## 1446             Blk 210 Toa Payoh Lorong 8  4266.60660
    ## 1447           Blk 216 Bedok North Street 1  8167.01193
    ## 1448              Blk 22 Toa Payoh Lorong 7  3658.01117
    ## 1449              Blk 226D Ang Mo Kio Ave 1  7580.92361
    ## 1450          Blk 226H Ang Mo Kio Street 22  7598.02968
    ## 1451          Blk 254 Jurong East Street 24 14733.34401
    ## 1452                  Blk 29 Bendemeer Road  1809.50507
    ## 1453                    Blk 320 Shunfu Road  6200.38548
    ## 1454               Blk 341 Ang Mo Kio Ave 1  6990.31752
    ## 1455              Blk 347 Jurong East Ave 1 15453.70917
    ## 1456                 Blk 353 Clementi Ave 2 10432.88793
    ## 1457              Blk 36 Telok Blangah Rise  5738.41621
    ## 1458             Blk 37A Teban Gardens Road 13611.09132
    ## 1459              Blk 409 Ang Mo Kio Ave 10  6690.13954
    ## 1460                   Blk 44 Holland Drive  7935.03208
    ## 1461                 Blk 448 Clementi Ave 3 11118.76605
    ## 1462             Blk 453A Ang Mo Kio Ave 10  7276.17105
    ## 1463                      Blk 49 Sims Place  2335.97321
    ## 1464                  Blk 4A Eunos Crescent  4890.67125
    ## 1465                      Blk 4A Jalan Batu  2232.77017
    ## 1466           Blk 4A Woodlands Centre Road 18484.76992
    ## 1467               Blk 502 West Coast Drive 11696.59601
    ## 1468               Blk 503 West Coast Drive 11630.88635
    ## 1469          Blk 505 Jurong West Street 52 16991.55634
    ## 1470                 Blk 50A Marine Terrace  5782.52323
    ## 1471                Blk 51 Old Airport Road  2514.11999
    ## 1472           Blk 511 Bedok North Street 3  8149.29029
    ## 1473 Bukit Panjang Hawker Centre and Market 13104.36569
    ## 1474         Our Tampines Hub Hawker Centre 10150.27731
    ## 1475        Kampung Admiralty Hawker Centre 16707.03796
    ## 1476                   Yishun Hawker Centre 13672.72080
    ## 1477   Jurong West Hawker Centre and Market 19007.40755
    ## 1478        Pasir Ris Central Hawker Centre 12491.73492
    ## 1479                   Dawson Hawker Centre  6611.78532
    ## 1480      Woodlands Street 12 Hawker Centre 17214.55060
    ## 1481              Blk 527 Ang Mo Kio Ave 10  7799.05617
    ## 1482           Blk 538 Bedok North Street 3  7499.57943
    ## 1483           Blk 58 New Upper Changi Road  8914.23945
    ## 1484              Blk 6 Tanjong Pagar Plaza  3707.76691
    ## 1485               Blk 628 Ang Mo Kio Ave 4  9018.86324
    ## 1486           Blk 630 Bedok Reservoir Road  6510.66948
    ## 1487                   Blk 69 Geylang Bahru  2165.18931
    ## 1488                     Blk 7 Empress Road  6648.78172
    ## 1489               Blk 724 Ang Mo Kio Ave 6  7905.10049
    ## 1490         Blk 726 Clementi West Street 2 11087.12387
    ## 1491              Blk 74 Toa Payoh Lorong 4  3743.61569
    ## 1492              Market Street Food Centre  2606.76447
    ## 1493                    Maxwell Food Centre  3291.33334
    ## 1494                     Newton Food Centre  2884.45094
    ## 1495 North Bridge Road Market & Food Centre   319.55106
    ## 1496              Pasir Panjang Food Centre  8600.01089
    ## 1497           Pek Kio Market & Food Centre  2113.81603
    ## 1498              People's Park Food Centre  3087.46754
    ## 1499            Sembawang Hills Food Centre  8611.07300
    ## 1500                Serangoon Garden Market  6673.20010
    ## 1501      Taman Jurong Market & Food Centre 16227.10243
    ## 1502                    Tanglin Halt Market  7377.46704
    ## 1503                           Tekka Market  1530.93663
    ## 1504                     Tiong Bahru Market  4023.06473
    ## 1505             Zion Riverside Food Centre  3820.41786
    ## 1506              Blk 75 Toa Payoh Lorong 5  3863.92629
    ## 1507                    Blk 79 Redhill Lane  5327.02101
    ## 1508             Blk 79 Telok Blangah Drive  7060.02626
    ## 1509                    Blk 80 Circuit Road  3783.96151
    ## 1510             Blk 82 Telok Blangah Drive  7000.98868
    ## 1511           Blk 84 Marine Parade Central  4725.33956
    ## 1512            Blk 85 Bedok North Street 4  8930.34565
    ## 1513                    Blk 85 Redhill Lane  5352.19884
    ## 1514                    Blk 89 Circuit Road  3306.62543
    ## 1515                   Blk 90 Whampoa Drive  2423.04248
    ## 1516              Blk 93 Toa Payoh Lorong 4  4248.83614
    ## 1517                   Blks 13/14 Haig Road  3782.39977
    ## 1518          Blks 160/162 Ang Mo Kio Ave 4  8381.08060
    ## 1519                  Ci Yuan Hawker Centre  8299.26702
    ## 1520          Hong Lim Market & Food Centre  6341.22959
    ## 1521                  Kallang Estate Market  9942.19557
    ## 1522             Kovan Market & Food Centre 11456.04575
    ## 1523         Blks 2 & 3 Changi Village Road 23195.31286
    ## 1524           Commonwealth Crescent Market   721.03635
    ## 1525    ABC Brickworks Market & Food Centre  3032.46200
    ## 1526                       Adam Food Centre  2598.71852
    ## 1527     Albert Centre Market & Food Centre  6695.18373
    ## 1528          Alexandra Village Food Centre  2924.56509
    ## 1529                Amoy Street Food Centre  6740.95100
    ## 1530                      Bedok Food Centre 17904.99149
    ## 1531                    Beo Crescent Market  4363.77349
    ## 1532                     Berseh Food Centre  6910.31498
    ## 1533                      Blk 1 Jalan Kukoh  5609.05586
    ## 1534                  Blk 105 Hougang Ave 1 11611.41970
    ## 1535          Blk 11 Telok Blangah Crescent  4556.15590
    ## 1536              Blk 112 Jalan Bukit Merah  4867.76448
    ## 1537               Blk 115 Bukit Merah View  4128.50568
    ## 1538                 Blk 117 Aljunied Ave 2 10312.47960
    ## 1539             Blk 127 Toa Payoh Lorong 1  6311.64677
    ## 1540             Blk 137 Tampines Street 11 17103.67529
    ## 1541                  Blk 159 Mei Chin Road  2149.22404
    ## 1542                Blk 16 Bedok South Road 15692.18035
    ## 1543            Blk 163 Bukit Merah Central  3888.64919
    ## 1544            Blk 17 Upper Boon Keng Road  8554.49931
    ## 1545                   Blk 20 Ghim Moh Road   738.17476
    ## 1546         Blk 208B New Upper Changi Road 15162.48454
    ## 1547             Blk 210 Toa Payoh Lorong 8  7377.64895
    ## 1548           Blk 216 Bedok North Street 1 15501.22563
    ## 1549              Blk 22 Toa Payoh Lorong 7  7423.58717
    ## 1550              Blk 226D Ang Mo Kio Ave 1  7909.47273
    ## 1551          Blk 226H Ang Mo Kio Street 22  7997.91045
    ## 1552          Blk 254 Jurong East Street 24  7300.51044
    ## 1553                  Blk 29 Bendemeer Road  7640.81735
    ## 1554                    Blk 320 Shunfu Road  6532.75661
    ## 1555               Blk 341 Ang Mo Kio Ave 1  8361.04311
    ## 1556              Blk 347 Jurong East Ave 1  8004.57328
    ## 1557                 Blk 353 Clementi Ave 2  2701.11546
    ## 1558              Blk 36 Telok Blangah Rise  5210.04283
    ## 1559             Blk 37A Teban Gardens Road  5887.42122
    ## 1560              Blk 409 Ang Mo Kio Ave 10  8844.67655
    ## 1561                   Blk 44 Holland Drive   386.33095
    ## 1562                 Blk 448 Clementi Ave 3  3387.97360
    ## 1563             Blk 453A Ang Mo Kio Ave 10  9328.26064
    ## 1564                      Blk 49 Sims Place  9434.80320
    ## 1565                  Blk 4A Eunos Crescent 12219.44809
    ## 1566                      Blk 4A Jalan Batu  9957.86012
    ## 1567           Blk 4A Woodlands Centre Road 14629.40610
    ## 1568               Blk 502 West Coast Drive  3974.20519
    ## 1569               Blk 503 West Coast Drive  3908.84517
    ## 1570          Blk 505 Jurong West Street 52  9521.90729
    ## 1571                 Blk 50A Marine Terrace 13466.90053
    ## 1572                Blk 51 Old Airport Road 10128.39939
    ## 1573           Blk 511 Bedok North Street 3 15307.03622
    ## 1574 Bukit Panjang Hawker Centre and Market  7783.52025
    ## 1575         Our Tampines Hub Hawker Centre 16806.86762
    ## 1576        Kampung Admiralty Hawker Centre 14281.67258
    ## 1577                   Yishun Hawker Centre 13784.96224
    ## 1578   Jurong West Hawker Centre and Market 11353.55645
    ## 1579        Pasir Ris Central Hawker Centre 18757.48055
    ## 1580                   Dawson Hawker Centre  1845.27024
    ## 1581      Woodlands Street 12 Hawker Centre 13663.32732
    ## 1582              Blk 527 Ang Mo Kio Ave 10  9529.66642
    ## 1583           Blk 538 Bedok North Street 3 14648.60767
    ## 1584           Blk 58 New Upper Changi Road 16344.46632
    ## 1585              Blk 6 Tanjong Pagar Plaza  6578.65028
    ## 1586               Blk 628 Ang Mo Kio Ave 4  9279.72386
    ## 1587           Blk 630 Bedok Reservoir Road 13491.02851
    ## 1588                   Blk 69 Geylang Bahru  8443.55958
    ## 1589                     Blk 7 Empress Road  1336.38063
    ## 1590               Blk 724 Ang Mo Kio Ave 6  8886.91194
    ## 1591         Blk 726 Clementi West Street 2  3495.30816
    ## 1592              Blk 74 Toa Payoh Lorong 4  6875.02419
    ## 1593              Market Street Food Centre  6828.86702
    ## 1594                    Maxwell Food Centre  6520.47385
    ## 1595                     Newton Food Centre  4975.84985
    ## 1596 North Bridge Road Market & Food Centre  7703.19818
    ## 1597              Pasir Panjang Food Centre  3889.28838
    ## 1598           Pek Kio Market & Food Centre  6188.58496
    ## 1599              People's Park Food Centre  6034.26995
    ## 1600            Sembawang Hills Food Centre  7787.05844
    ## 1601                Serangoon Garden Market  9867.15648
    ## 1602      Taman Jurong Market & Food Centre  8572.10492
    ## 1603                    Tanglin Halt Market  1160.35507
    ## 1604                           Tekka Market  6217.11207
    ## 1605                     Tiong Bahru Market  5055.23412
    ## 1606             Zion Riverside Food Centre  4534.16733
    ## 1607              Blk 75 Toa Payoh Lorong 5  7035.59891
    ## 1608                    Blk 79 Redhill Lane  3655.02576
    ## 1609             Blk 79 Telok Blangah Drive  4387.28364
    ## 1610                    Blk 80 Circuit Road 10437.73734
    ## 1611             Blk 82 Telok Blangah Drive  4341.94761
    ## 1612           Blk 84 Marine Parade Central 12442.78903
    ## 1613            Blk 85 Bedok North Street 4 16180.03423
    ## 1614                    Blk 85 Redhill Lane  3690.90143
    ## 1615                    Blk 89 Circuit Road 10176.24351
    ## 1616                   Blk 90 Whampoa Drive  6838.72500
    ## 1617              Blk 93 Toa Payoh Lorong 4  6803.15036
    ## 1618                   Blks 13/14 Haig Road 11221.90253
    ## 1619          Blks 160/162 Ang Mo Kio Ave 4  8590.10473
    ## 1620                  Ci Yuan Hawker Centre 12126.91616
    ## 1621                  Kallang Estate Market  4917.09701
    ## 1622             Kovan Market & Food Centre  9311.21708
    ## 1623         Blks 2 & 3 Changi Village Road 19576.69072
    ## 1624           Commonwealth Crescent Market  5633.87578
    ## 1625    ABC Brickworks Market & Food Centre  4198.92170
    ## 1626                       Adam Food Centre  5558.79043
    ## 1627     Albert Centre Market & Food Centre  1976.71310
    ## 1628          Alexandra Village Food Centre  4601.10921
    ## 1629                Amoy Street Food Centre   672.28342
    ## 1630                      Bedok Food Centre 12806.27730
    ## 1631                    Beo Crescent Market  2092.23803
    ## 1632                     Berseh Food Centre  2733.97110
    ## 1633                      Blk 1 Jalan Kukoh   732.17901
    ## 1634                  Blk 105 Hougang Ave 1  9043.71014
    ## 1635          Blk 11 Telok Blangah Crescent  3147.59168
    ## 1636              Blk 112 Jalan Bukit Merah  2279.12741
    ## 1637               Blk 115 Bukit Merah View  2645.50417
    ## 1638                 Blk 117 Aljunied Ave 2  6028.91798
    ## 1639             Blk 127 Toa Payoh Lorong 1  5844.72325
    ## 1640             Blk 137 Tampines Street 11 12865.06144
    ## 1641                  Blk 159 Mei Chin Road  4857.09772
    ## 1642                Blk 16 Bedok South Road 10721.09563
    ## 1643            Blk 163 Bukit Merah Central  3212.74718
    ## 1644            Blk 17 Upper Boon Keng Road  4369.18940
    ## 1645                   Blk 20 Ghim Moh Road  7013.06508
    ## 1646         Blk 208B New Upper Changi Road 10398.16719
    ## 1647             Blk 210 Toa Payoh Lorong 8  6157.63773
    ## 1648           Blk 216 Bedok North Street 1 10770.15726
    ## 1649              Blk 22 Toa Payoh Lorong 7  5666.31614
    ## 1650              Blk 226D Ang Mo Kio Ave 1  9046.16896
    ## 1651          Blk 226H Ang Mo Kio Street 22  9088.81610
    ## 1652          Blk 254 Jurong East Street 24 13637.26582
    ## 1653                  Blk 29 Bendemeer Road  4216.03885
    ## 1654                    Blk 320 Shunfu Road  7446.58450
    ## 1655               Blk 341 Ang Mo Kio Ave 1  8724.00953
    ## 1656              Blk 347 Jurong East Ave 1 14344.06450
    ## 1657                 Blk 353 Clementi Ave 2  8944.57085
    ## 1658              Blk 36 Telok Blangah Rise  2995.58821
    ## 1659             Blk 37A Teban Gardens Road 12112.45381
    ## 1660              Blk 409 Ang Mo Kio Ave 10  8637.69140
    ## 1661                   Blk 44 Holland Drive  6422.35128
    ## 1662                 Blk 448 Clementi Ave 3  9564.56777
    ## 1663             Blk 453A Ang Mo Kio Ave 10  9251.61783
    ## 1664                      Blk 49 Sims Place  5135.59926
    ## 1665                  Blk 4A Eunos Crescent  7573.23287
    ## 1666                      Blk 4A Jalan Batu  4648.15589
    ## 1667           Blk 4A Woodlands Centre Road 19158.16237
    ## 1668               Blk 502 West Coast Drive 10083.65402
    ## 1669               Blk 503 West Coast Drive 10018.56888
    ## 1670          Blk 505 Jurong West Street 52 15863.12362
    ## 1671                 Blk 50A Marine Terrace  8108.28053
    ## 1672                Blk 51 Old Airport Road  5128.57218
    ## 1673           Blk 511 Bedok North Street 3 10825.92868
    ## 1674 Bukit Panjang Hawker Centre and Market 13072.27826
    ## 1675         Our Tampines Hub Hawker Centre 12914.63899
    ## 1676        Kampung Admiralty Hawker Centre 17826.33494
    ## 1677                   Yishun Hawker Centre 15455.65054
    ## 1678   Jurong West Hawker Centre and Market 17640.22626
    ## 1679        Pasir Ris Central Hawker Centre 15281.62886
    ## 1680                   Dawson Hawker Centre  4770.47974
    ## 1681      Woodlands Street 12 Hawker Centre 17968.87946
    ## 1682              Blk 527 Ang Mo Kio Ave 10  9725.81658
    ## 1683           Blk 538 Bedok North Street 3 10189.47241
    ## 1684           Blk 58 New Upper Changi Road 11448.17533
    ## 1685              Blk 6 Tanjong Pagar Plaza   999.04725
    ## 1686               Blk 628 Ang Mo Kio Ave 4 10606.10891
    ## 1687           Blk 630 Bedok Reservoir Road  9259.37192
    ## 1688                   Blk 69 Geylang Bahru  4825.08016
    ## 1689                     Blk 7 Empress Road  5634.51735
    ## 1690               Blk 724 Ang Mo Kio Ave 6  9614.91560
    ## 1691         Blk 726 Clementi West Street 2  9304.83768
    ## 1692              Blk 74 Toa Payoh Lorong 4  5494.75360
    ## 1693              Market Street Food Centre   492.72044
    ## 1694                    Maxwell Food Centre   578.32380
    ## 1695                     Newton Food Centre  3027.52185
    ## 1696 North Bridge Road Market & Food Centre  3032.00161
    ## 1697              Pasir Panjang Food Centre  6143.49327
    ## 1698           Pek Kio Market & Food Centre  3453.25141
    ## 1699              People's Park Food Centre   361.70399
    ## 1700            Sembawang Hills Food Centre  9815.57833
    ## 1701                Serangoon Garden Market  8923.97915
    ## 1702      Taman Jurong Market & Food Centre 14870.42950
    ## 1703                    Tanglin Halt Market  5632.11067
    ## 1704                           Tekka Market  2376.17746
    ## 1705                     Tiong Bahru Market  1499.21192
    ## 1706             Zion Riverside Food Centre  1807.34200
    ## 1707              Blk 75 Toa Payoh Lorong 5  5672.02850
    ## 1708                    Blk 79 Redhill Lane  3064.83614
    ## 1709             Blk 79 Telok Blangah Drive  4448.76836
    ## 1710                    Blk 80 Circuit Road  6582.39553
    ## 1711             Blk 82 Telok Blangah Drive  4397.53492
    ## 1712           Blk 84 Marine Parade Central  6993.86124
    ## 1713            Blk 85 Bedok North Street 4 11560.11836
    ## 1714                    Blk 85 Redhill Lane  3066.96246
    ## 1715                    Blk 89 Circuit Road  6106.81821
    ## 1716                   Blk 90 Whampoa Drive  4295.38388
    ## 1717              Blk 93 Toa Payoh Lorong 4  5900.06238
    ## 1718                   Blks 13/14 Haig Road  6452.37519
    ## 1719          Blks 160/162 Ang Mo Kio Ave 4  9895.33916
    ## 1720                  Ci Yuan Hawker Centre 10795.41996
    ## 1721             Kovan Market & Food Centre  5722.98989
    ## 1722         Blks 2 & 3 Changi Village Road 14696.28243
    ## 1723           Commonwealth Crescent Market  9359.72675
    ## 1724    ABC Brickworks Market & Food Centre  8755.22876
    ## 1725                       Adam Food Centre  8003.39128
    ## 1726     Albert Centre Market & Food Centre  3403.91188
    ## 1727          Alexandra Village Food Centre  9165.22142
    ## 1728                Amoy Street Food Centre  5210.28810
    ## 1729                      Bedok Food Centre  8068.19388
    ## 1730                    Beo Crescent Market  6643.66832
    ## 1731                     Berseh Food Centre  3035.59736
    ## 1732                      Blk 1 Jalan Kukoh  5354.54307
    ## 1733                  Blk 105 Hougang Ave 1  5177.76480
    ## 1734          Blk 11 Telok Blangah Crescent  8009.05812
    ## 1735              Blk 112 Jalan Bukit Merah  7141.94684
    ## 1736               Blk 115 Bukit Merah View  7320.82209
    ## 1737                 Blk 117 Aljunied Ave 2  1500.20762
    ## 1738             Blk 127 Toa Payoh Lorong 1  5544.93847
    ## 1739             Blk 137 Tampines Street 11  7950.32886
    ## 1740                  Blk 159 Mei Chin Road  9174.35099
    ## 1741                Blk 16 Bedok South Road  5903.37000
    ## 1742            Blk 163 Bukit Merah Central  7918.22275
    ## 1743            Blk 17 Upper Boon Keng Road  1628.02840
    ## 1744                   Blk 20 Ghim Moh Road 10680.14110
    ## 1745         Blk 208B New Upper Changi Road  5511.89555
    ## 1746             Blk 210 Toa Payoh Lorong 8  4914.95678
    ## 1747           Blk 216 Bedok North Street 1  5880.26834
    ## 1748              Blk 22 Toa Payoh Lorong 7  4308.76955
    ## 1749              Blk 226D Ang Mo Kio Ave 1  8252.02210
    ## 1750          Blk 226H Ang Mo Kio Street 22  8242.85061
    ## 1751          Blk 254 Jurong East Street 24 16767.77390
    ## 1752                  Blk 29 Bendemeer Road  2688.35665
    ## 1753                    Blk 320 Shunfu Road  7197.73520
    ## 1754               Blk 341 Ang Mo Kio Ave 1  7433.79606
    ## 1755              Blk 347 Jurong East Ave 1 17487.34847
    ## 1756                 Blk 353 Clementi Ave 2 12632.03556
    ## 1757              Blk 36 Telok Blangah Rise  7911.64311
    ## 1758             Blk 37A Teban Gardens Road 15788.72641
    ## 1759              Blk 409 Ang Mo Kio Ave 10  6908.96554
    ## 1760                   Blk 44 Holland Drive 10167.43293
    ## 1761                 Blk 448 Clementi Ave 3 13328.34934
    ## 1762             Blk 453A Ang Mo Kio Ave 10  7404.14026
    ## 1763                      Blk 49 Sims Place  1192.69230
    ## 1764                  Blk 4A Eunos Crescent  2658.02672
    ## 1765                      Blk 4A Jalan Batu   550.77404
    ## 1766           Blk 4A Woodlands Centre Road 19462.83291
    ## 1767               Blk 502 West Coast Drive 13916.20556
    ## 1768               Blk 503 West Coast Drive 13850.77654
    ## 1769          Blk 505 Jurong West Street 52 19021.31999
    ## 1770                 Blk 50A Marine Terrace  3524.88553
    ## 1771                Blk 51 Old Airport Road   211.82269
    ## 1772           Blk 511 Bedok North Street 3  5909.19669
    ## 1773 Bukit Panjang Hawker Centre and Market 14650.15764
    ## 1774         Our Tampines Hub Hawker Centre  8042.44971
    ## 1775        Kampung Admiralty Hawker Centre 17349.05836
    ## 1776                   Yishun Hawker Centre 13724.60308
    ## 1777   Jurong West Hawker Centre and Market 21115.39537
    ## 1778        Pasir Ris Central Hawker Centre 10469.30734
    ## 1779                   Dawson Hawker Centre  8905.84342
    ## 1780      Woodlands Street 12 Hawker Centre 18149.37443
    ## 1781              Blk 527 Ang Mo Kio Ave 10  7946.08983
    ## 1782           Blk 538 Bedok North Street 3  5272.67742
    ## 1783           Blk 58 New Upper Changi Road  6609.93662
    ## 1784              Blk 6 Tanjong Pagar Plaza  5689.29340
    ## 1785               Blk 628 Ang Mo Kio Ave 4  9469.09106
    ## 1786           Blk 630 Bedok Reservoir Road  4382.43332
    ## 1787                   Blk 69 Geylang Bahru  2211.45172
    ## 1788                     Blk 7 Empress Road  8790.25136
    ## 1789               Blk 724 Ang Mo Kio Ave 6  8298.36943
    ## 1790         Blk 726 Clementi West Street 2 13345.94339
    ## 1791              Blk 74 Toa Payoh Lorong 4  4669.13456
    ## 1792              Market Street Food Centre  4601.35840
    ## 1793                    Maxwell Food Centre  5313.54142
    ## 1794                     Newton Food Centre  4984.79913
    ## 1795 North Bridge Road Market & Food Centre  2259.63960
    ## 1796              Pasir Panjang Food Centre 10891.33024
    ## 1797           Pek Kio Market & Food Centre  3895.76440
    ## 1798              People's Park Food Centre  5246.13875
    ## 1799            Sembawang Hills Food Centre  9449.41514
    ## 1800                Serangoon Garden Market  6462.45125
    ## 1801      Taman Jurong Market & Food Centre 18345.03044
    ## 1802                    Tanglin Halt Market  9654.83114
    ## 1803                           Tekka Market  3741.59751
    ## 1804                     Tiong Bahru Market  6268.54083
    ## 1805             Zion Riverside Food Centre  6122.47665
    ## 1806              Blk 75 Toa Payoh Lorong 5  4698.79080
    ## 1807                    Blk 79 Redhill Lane  7627.26032
    ## 1808             Blk 79 Telok Blangah Drive  9308.51415
    ## 1809                    Blk 80 Circuit Road  2286.11695
    ## 1810             Blk 82 Telok Blangah Drive  9251.98041
    ## 1811           Blk 84 Marine Parade Central  2535.37636
    ## 1812            Blk 85 Bedok North Street 4  6657.34090
    ## 1813                    Blk 85 Redhill Lane  7650.76671
    ## 1814                    Blk 89 Circuit Road  1780.68801
    ## 1815                   Blk 90 Whampoa Drive  3655.97383
    ## 1816              Blk 93 Toa Payoh Lorong 4  5162.49175
    ## 1817                   Blks 13/14 Haig Road  1538.08346
    ## 1818          Blks 160/162 Ang Mo Kio Ave 4  8945.67512
    ## 1819                  Ci Yuan Hawker Centre  7523.81526
    ## 1820         Blks 2 & 3 Changi Village Road 11855.78743
    ## 1821           Commonwealth Crescent Market 11168.62911
    ## 1822    ABC Brickworks Market & Food Centre 11778.81341
    ## 1823                       Adam Food Centre  8879.00357
    ## 1824     Albert Centre Market & Food Centre  7334.59428
    ## 1825          Alexandra Village Food Centre 12127.75759
    ## 1826                Amoy Street Food Centre  9861.85929
    ## 1827                      Bedok Food Centre  8842.53042
    ## 1828                    Beo Crescent Market 10147.95492
    ## 1829                     Berseh Food Centre  6579.58376
    ## 1830                      Blk 1 Jalan Kukoh  9364.09444
    ## 1831                  Blk 105 Hougang Ave 1   741.39794
    ## 1832          Blk 11 Telok Blangah Crescent 11742.02473
    ## 1833              Blk 112 Jalan Bukit Merah 11002.90238
    ## 1834               Blk 115 Bukit Merah View 10810.90599
    ## 1835                 Blk 117 Aljunied Ave 2  4255.52093
    ## 1836             Blk 127 Toa Payoh Lorong 1  5145.47246
    ## 1837             Blk 137 Tampines Street 11  6694.77717
    ## 1838                  Blk 159 Mei Chin Road 11770.57861
    ## 1839                Blk 16 Bedok South Road  6972.02675
    ## 1840            Blk 163 Bukit Merah Central 11335.91008
    ## 1841            Blk 17 Upper Boon Keng Road  5130.55148
    ## 1842                   Blk 20 Ghim Moh Road 12109.94319
    ## 1843         Blk 208B New Upper Changi Road  6194.81387
    ## 1844             Blk 210 Toa Payoh Lorong 8  4086.80428
    ## 1845           Blk 216 Bedok North Street 1  6341.03704
    ## 1846              Blk 22 Toa Payoh Lorong 7  4165.46757
    ## 1847              Blk 226D Ang Mo Kio Ave 1  5270.95656
    ## 1848          Blk 226H Ang Mo Kio Street 22  5199.90347
    ## 1849          Blk 254 Jurong East Street 24 16583.44815
    ## 1850                  Blk 29 Bendemeer Road  5098.47950
    ## 1851                    Blk 320 Shunfu Road  5503.58358
    ## 1852               Blk 341 Ang Mo Kio Ave 1  4234.21539
    ## 1853              Blk 347 Jurong East Ave 1 17250.43568
    ## 1854                 Blk 353 Clementi Ave 2 13736.24356
    ## 1855              Blk 36 Telok Blangah Rise 11927.56375
    ## 1856             Blk 37A Teban Gardens Road 16476.65322
    ## 1857              Blk 409 Ang Mo Kio Ave 10  3430.11417
    ## 1858                   Blk 44 Holland Drive 11806.57131
    ## 1859                 Blk 448 Clementi Ave 3 14437.62470
    ## 1860             Blk 453A Ang Mo Kio Ave 10  3447.85388
    ## 1861                      Blk 49 Sims Place  4705.12935
    ## 1862                  Blk 4A Eunos Crescent  4747.12022
    ## 1863                      Blk 4A Jalan Batu  6273.76250
    ## 1864           Blk 4A Woodlands Centre Road 15747.59949
    ## 1865               Blk 502 West Coast Drive 15048.07118
    ## 1866               Blk 503 West Coast Drive 14990.35057
    ## 1867          Blk 505 Jurong West Street 52 18673.62422
    ## 1868                 Blk 50A Marine Terrace  6766.08767
    ## 1869                Blk 51 Old Airport Road  5624.73251
    ## 1870           Blk 511 Bedok North Street 3  5731.33845
    ## 1871 Bukit Panjang Hawker Centre and Market 12794.11141
    ## 1872         Our Tampines Hub Hawker Centre  6009.78294
    ## 1873        Kampung Admiralty Hawker Centre 13035.46376
    ## 1874                   Yishun Hawker Centre  8610.97807
    ## 1875   Jurong West Hawker Centre and Market 21078.82964
    ## 1876        Pasir Ris Central Hawker Centre  7466.36529
    ## 1877                   Dawson Hawker Centre 11324.05563
    ## 1878      Woodlands Street 12 Hawker Centre 14400.56925
    ## 1879              Blk 527 Ang Mo Kio Ave 10  3816.18945
    ## 1880           Blk 538 Bedok North Street 3  5277.06196
    ## 1881           Blk 58 New Upper Changi Road  7252.31410
    ## 1882              Blk 6 Tanjong Pagar Plaza 10292.13055
    ## 1883               Blk 628 Ang Mo Kio Ave 4  5592.57382
    ## 1884           Blk 630 Bedok Reservoir Road  4242.99903
    ## 1885                   Blk 69 Geylang Bahru  4527.51592
    ## 1886                     Blk 7 Empress Road 10120.20015
    ## 1887               Blk 724 Ang Mo Kio Ave 6  4623.23299
    ## 1888         Blk 726 Clementi West Street 2 14863.36959
    ## 1889              Blk 74 Toa Payoh Lorong 4  4656.29753
    ## 1890              Market Street Food Centre  9232.84853
    ## 1891                    Maxwell Food Centre  9863.76065
    ## 1892                     Newton Food Centre  7345.26605
    ## 1893 North Bridge Road Market & Food Centre  6391.19001
    ## 1894              Pasir Panjang Food Centre 13980.07214
    ## 1895           Pek Kio Market & Food Centre  6196.47298
    ## 1896              People's Park Food Centre  9513.89033
    ## 1897            Sembawang Hills Food Centre  6509.00735
    ## 1898                Serangoon Garden Market  2187.25399
    ## 1899      Taman Jurong Market & Food Centre 18502.23559
    ## 1900                    Tanglin Halt Market 11759.31807
    ## 1901                           Tekka Market  7056.55578
    ## 1902                     Tiong Bahru Market 10130.72843
    ## 1903             Zion Riverside Food Centre  9576.00669
    ## 1904              Blk 75 Toa Payoh Lorong 5  4475.53330
    ## 1905                    Blk 79 Redhill Lane 10892.50140
    ## 1906             Blk 79 Telok Blangah Drive 12881.75837
    ## 1907                    Blk 80 Circuit Road  3460.03435
    ## 1908             Blk 82 Telok Blangah Drive 12813.50627
    ## 1909           Blk 84 Marine Parade Central  6680.48327
    ## 1910            Blk 85 Bedok North Street 4  6593.19520
    ## 1911                    Blk 85 Redhill Lane 10939.63517
    ## 1912                    Blk 89 Circuit Road  3945.10702
    ## 1913                   Blk 90 Whampoa Drive  5270.79132
    ## 1914              Blk 93 Toa Payoh Lorong 4  4656.05885
    ## 1915                   Blks 13/14 Haig Road  4979.53822
    ## 1916          Blks 160/162 Ang Mo Kio Ave 4  5474.66904
    ## 1917                  Ci Yuan Hawker Centre  1834.06274
    ## 1918           Commonwealth Crescent Market 22832.46573
    ## 1919    ABC Brickworks Market & Food Centre 23011.19095
    ## 1920                       Adam Food Centre 20664.15288
    ## 1921     Albert Centre Market & Food Centre 17818.85370
    ## 1922          Alexandra Village Food Centre 23399.90975
    ## 1923                Amoy Street Food Centre 19905.67499
    ## 1924                      Bedok Food Centre  8436.19827
    ## 1925                    Beo Crescent Market 21063.43667
    ## 1926                     Berseh Food Centre 17194.75502
    ## 1927                      Blk 1 Jalan Kukoh 19922.85110
    ## 1928                  Blk 105 Hougang Ave 1 11601.46519
    ## 1929          Blk 11 Telok Blangah Crescent 22562.29391
    ## 1930              Blk 112 Jalan Bukit Merah 21717.14041
    ## 1931               Blk 115 Bukit Merah View 21758.85421
    ## 1932                 Blk 117 Aljunied Ave 2 13575.21140
    ## 1933             Blk 127 Toa Payoh Lorong 1 16939.97557
    ## 1934             Blk 137 Tampines Street 11  6834.89705
    ## 1935                  Blk 159 Mei Chin Road 23193.51953
    ## 1936                Blk 16 Bedok South Road  9588.76551
    ## 1937            Blk 163 Bukit Merah Central 22343.15283
    ## 1938            Blk 17 Upper Boon Keng Road 15350.42998
    ## 1939                   Blk 20 Ghim Moh Road 23878.19781
    ## 1940         Blk 208B New Upper Changi Road  9558.37433
    ## 1941             Blk 210 Toa Payoh Lorong 8 15846.65681
    ## 1942           Blk 216 Bedok North Street 1  9199.65939
    ## 1943              Blk 22 Toa Payoh Lorong 7 15772.45155
    ## 1944              Blk 226D Ang Mo Kio Ave 1 16766.82884
    ## 1945          Blk 226H Ang Mo Kio Street 22 16680.19953
    ## 1946          Blk 254 Jurong East Street 24 28328.27861
    ## 1947                  Blk 29 Bendemeer Road 15937.81018
    ## 1948                    Blk 320 Shunfu Road 17322.26622
    ## 1949               Blk 341 Ang Mo Kio Ave 1 15823.59195
    ## 1950              Blk 347 Jurong East Ave 1 28972.28782
    ## 1951                 Blk 353 Clementi Ave 2 25569.27020
    ## 1952              Blk 36 Telok Blangah Rise 22547.10333
    ## 1953             Blk 37A Teban Gardens Road 28330.42133
    ## 1954              Blk 409 Ang Mo Kio Ave 10 15072.48313
    ## 1955                   Blk 44 Holland Drive 23526.97504
    ## 1956                 Blk 448 Clementi Ave 3 26275.15569
    ## 1957             Blk 453A Ang Mo Kio Ave 10 14859.21589
    ## 1958                      Blk 49 Sims Place 14497.96384
    ## 1959                  Blk 4A Eunos Crescent 12052.68123
    ## 1960                      Blk 4A Jalan Batu 15057.36251
    ## 1961           Blk 4A Woodlands Centre Road 24941.62384
    ## 1962               Blk 502 West Coast Drive 26887.39247
    ## 1963               Blk 503 West Coast Drive 26828.70187
    ## 1964          Blk 505 Jurong West Street 52 30341.47622
    ## 1965                 Blk 50A Marine Terrace 12250.22044
    ## 1966                Blk 51 Old Airport Road 14488.72976
    ## 1967           Blk 511 Bedok North Street 3  8908.83006
    ## 1968 Bukit Panjang Hawker Centre and Market 24044.88395
    ## 1969         Our Tampines Hub Hawker Centre  6662.06181
    ## 1970        Kampung Admiralty Hawker Centre 21613.72343
    ## 1971                   Yishun Hawker Centre 16454.46008
    ## 1972   Jurong West Hawker Centre and Market 32797.58803
    ## 1973        Pasir Ris Central Hawker Centre  4438.93987
    ## 1974                   Dawson Hawker Centre 22801.69299
    ## 1975      Woodlands Street 12 Hawker Centre 23712.29929
    ## 1976              Blk 527 Ang Mo Kio Ave 10 14995.95322
    ## 1977           Blk 538 Bedok North Street 3  9488.32836
    ## 1978           Blk 58 New Upper Changi Road  8893.38011
    ## 1979              Blk 6 Tanjong Pagar Plaza 20384.64926
    ## 1980               Blk 628 Ang Mo Kio Ave 4 16446.50937
    ## 1981           Blk 630 Bedok Reservoir Road 10320.18795
    ## 1982                   Blk 69 Geylang Bahru 15138.66279
    ## 1983                     Blk 7 Empress Road 21862.93586
    ## 1984               Blk 724 Ang Mo Kio Ave 6 15883.42118
    ## 1985         Blk 726 Clementi West Street 2 26654.22978
    ## 1986              Blk 74 Toa Payoh Lorong 4 16320.58328
    ## 1987              Market Street Food Centre 19290.98682
    ## 1988                    Maxwell Food Centre 20003.08368
    ## 1989                     Newton Food Centre 18621.97338
    ## 1990 North Bridge Road Market & Food Centre 16629.59908
    ## 1991              Pasir Panjang Food Centre 25232.89872
    ## 1992           Pek Kio Market & Food Centre 17351.56057
    ## 1993              People's Park Food Centre 19886.12006
    ## 1994            Sembawang Hills Food Centre 17819.41859
    ## 1995                Serangoon Garden Market 13824.49931
    ## 1996      Taman Jurong Market & Food Centre 30290.41539
    ## 1997                    Tanglin Halt Market 23355.87701
    ## 1998                           Tekka Market 17860.89753
    ## 1999                     Tiong Bahru Market 20818.71306
    ## 2000             Zion Riverside Food Centre 20496.24896
    ## 2001              Blk 75 Toa Payoh Lorong 5 16162.56579
    ## 2002                    Blk 79 Redhill Lane 21971.07401
    ## 2003             Blk 79 Telok Blangah Drive 23831.95273
    ## 2004                    Blk 80 Circuit Road 13137.05935
    ## 2005             Blk 82 Telok Blangah Drive 23769.83733
    ## 2006           Blk 84 Marine Parade Central 13242.04644
    ## 2007            Blk 85 Bedok North Street 4  8385.57179
    ## 2008                    Blk 85 Redhill Lane 22006.94987
    ## 2009                    Blk 89 Circuit Road 13552.23882
    ## 2010                   Blk 90 Whampoa Drive 16523.76471
    ## 2011              Blk 93 Toa Payoh Lorong 4 16424.73137
    ## 2012                   Blks 13/14 Haig Road 13162.64498
    ## 2013          Blks 160/162 Ang Mo Kio Ave 4 16664.39340
    ## 2014                  Ci Yuan Hawker Centre 11812.84771
    ## 2015    ABC Brickworks Market & Food Centre  2390.24069
    ## 2016                       Adam Food Centre  2464.82152
    ## 2017     Albert Centre Market & Food Centre  6066.01455
    ## 2018          Alexandra Village Food Centre  2339.48650
    ## 2019                Amoy Street Food Centre  6024.03388
    ## 2020                      Bedok Food Centre 17362.66789
    ## 2021                    Beo Crescent Market  3643.98460
    ## 2022                     Berseh Food Centre  6324.14710
    ## 2023                      Blk 1 Jalan Kukoh  4902.00260
    ## 2024                  Blk 105 Hougang Ave 1 11282.24250
    ## 2025          Blk 11 Telok Blangah Crescent  3874.03015
    ## 2026              Blk 112 Jalan Bukit Merah  4153.91663
    ## 2027               Blk 115 Bukit Merah View  3411.88129
    ## 2028                 Blk 117 Aljunied Ave 2  9798.45838
    ## 2029             Blk 127 Toa Payoh Lorong 1  6048.95322
    ## 2030             Blk 137 Tampines Street 11 16650.23918
    ## 2031                  Blk 159 Mei Chin Road  1552.33131
    ## 2032                Blk 16 Bedok South Road 15155.46742
    ## 2033            Blk 163 Bukit Merah Central  3192.30901
    ## 2034            Blk 17 Upper Boon Keng Road  8016.98380
    ## 2035                   Blk 20 Ghim Moh Road  1386.40646
    ## 2036         Blk 208B New Upper Changi Road 14644.61875
    ## 2037             Blk 210 Toa Payoh Lorong 8  7081.98288
    ## 2038           Blk 216 Bedok North Street 1 14988.48414
    ## 2039              Blk 22 Toa Payoh Lorong 7  7071.95664
    ## 2040              Blk 226D Ang Mo Kio Ave 1  7923.22113
    ## 2041          Blk 226H Ang Mo Kio Street 22  8008.39811
    ## 2042          Blk 254 Jurong East Street 24  8015.79948
    ## 2043                  Blk 29 Bendemeer Road  7139.90200
    ## 2044                    Blk 320 Shunfu Road  6461.73805
    ## 2045               Blk 341 Ang Mo Kio Ave 1  8289.26261
    ## 2046              Blk 347 Jurong East Ave 1  8717.93703
    ## 2047                 Blk 353 Clementi Ave 2  3350.19240
    ## 2048              Blk 36 Telok Blangah Rise  4527.12036
    ## 2049             Blk 37A Teban Gardens Road  6541.37484
    ## 2050              Blk 409 Ang Mo Kio Ave 10  8717.44899
    ## 2051                   Blk 44 Holland Drive   817.36143
    ## 2052                 Blk 448 Clementi Ave 3  4013.68854
    ## 2053             Blk 453A Ang Mo Kio Ave 10  9229.45621
    ## 2054                      Blk 49 Sims Place  8905.50300
    ## 2055                  Blk 4A Eunos Crescent 11693.60404
    ## 2056                      Blk 4A Jalan Batu  9352.08999
    ## 2057           Blk 4A Woodlands Centre Road 15172.70008
    ## 2058               Blk 502 West Coast Drive  4580.10437
    ## 2059               Blk 503 West Coast Drive  4514.29684
    ## 2060          Blk 505 Jurong West Street 52 10231.97188
    ## 2061                 Blk 50A Marine Terrace 12880.62049
    ## 2062                Blk 51 Old Airport Road  9550.18585
    ## 2063           Blk 511 Bedok North Street 3 14818.98235
    ## 2064 Bukit Panjang Hawker Centre and Market  8389.45271
    ## 2065         Our Tampines Hub Hawker Centre 16388.51998
    ## 2066        Kampung Admiralty Hawker Centre 14702.13830
    ## 2067                   Yishun Hawker Centre 13969.22691
    ## 2068   Jurong West Hawker Centre and Market 12033.97747
    ## 2069        Pasir Ris Central Hawker Centre 18393.66039
    ## 2070                   Dawson Hawker Centre  1171.58220
    ## 2071      Woodlands Street 12 Hawker Centre 14175.12964
    ## 2072              Blk 527 Ang Mo Kio Ave 10  9468.67953
    ## 2073           Blk 538 Bedok North Street 3 14158.90256
    ## 2074           Blk 58 New Upper Changi Road 15818.31956
    ## 2075              Blk 6 Tanjong Pagar Plaza  5858.06515
    ## 2076               Blk 628 Ang Mo Kio Ave 4  9354.94732
    ## 2077           Blk 630 Bedok Reservoir Road 13015.92089
    ## 2078                   Blk 69 Geylang Bahru  7950.60542
    ## 2079                     Blk 7 Empress Road  1200.20877
    ## 2080               Blk 724 Ang Mo Kio Ave 6  8875.95942
    ## 2081         Blk 726 Clementi West Street 2  3995.95823
    ## 2082              Blk 74 Toa Payoh Lorong 4  6538.17785
    ## 2083              Market Street Food Centre  6123.01254
    ## 2084                    Maxwell Food Centre  5803.34490
    ## 2085                     Newton Food Centre  4433.14694
    ## 2086 North Bridge Road Market & Food Centre  7108.43568
    ## 2087              Pasir Panjang Food Centre  3566.78532
    ## 2088           Pek Kio Market & Food Centre  5677.87569
    ## 2089              People's Park Food Centre  5322.77144
    ## 2090            Sembawang Hills Food Centre  7918.62589
    ## 2091                Serangoon Garden Market  9680.33570
    ## 2092      Taman Jurong Market & Food Centre  9255.59963
    ## 2093                    Tanglin Halt Market   734.30315
    ## 2094                           Tekka Market  5620.97151
    ## 2095                     Tiong Bahru Market  4335.14868
    ## 2096             Zion Riverside Food Centre  3826.63018
    ## 2097              Blk 75 Toa Payoh Lorong 5  6708.52223
    ## 2098                    Blk 79 Redhill Lane  2940.87283
    ## 2099             Blk 79 Telok Blangah Drive  3810.88757
    ## 2100                    Blk 80 Circuit Road  9964.32434
    ## 2101             Blk 82 Telok Blangah Drive  3760.19444
    ## 2102           Blk 84 Marine Parade Central 11842.55912
    ## 2103            Blk 85 Bedok North Street 4 15682.11414
    ## 2104                    Blk 85 Redhill Lane  2978.20267
    ## 2105                    Blk 89 Circuit Road  9679.28176
    ## 2106                   Blk 90 Whampoa Drive  6382.14214
    ## 2107              Blk 93 Toa Payoh Lorong 4  6516.17765
    ## 2108                   Blks 13/14 Haig Road 10675.41321
    ## 2109          Blks 160/162 Ang Mo Kio Ave 4  8645.85025
    ## 2110                  Ci Yuan Hawker Centre 11937.85918
    ## 2111                       Adam Food Centre  4165.87133
    ## 2112     Albert Centre Market & Food Centre  5358.80687
    ## 2113          Alexandra Village Food Centre   411.38456
    ## 2114                Amoy Street Food Centre  4366.93384
    ## 2115                      Bedok Food Centre 16809.11552
    ## 2116                    Beo Crescent Market  2150.09756
    ## 2117                     Berseh Food Centre  5872.53215
    ## 2118                      Blk 1 Jalan Kukoh  3543.88724
    ## 2119                  Blk 105 Hougang Ave 1 11742.31820
    ## 2120          Blk 11 Telok Blangah Crescent  1578.80455
    ## 2121              Blk 112 Jalan Bukit Merah  2133.60375
    ## 2122               Blk 115 Bukit Merah View  1557.58207
    ## 2123                 Blk 117 Aljunied Ave 2  9537.71300
    ## 2124             Blk 127 Toa Payoh Lorong 1  6969.30126
    ## 2125             Blk 137 Tampines Street 11 16518.76190
    ## 2126                  Blk 159 Mei Chin Road   905.95874
    ## 2127                Blk 16 Bedok South Road 14658.33907
    ## 2128            Blk 163 Bukit Merah Central  1050.33409
    ## 2129            Blk 17 Upper Boon Keng Road  7717.99570
    ## 2130                   Blk 20 Ghim Moh Road  3460.02265
    ## 2131         Blk 208B New Upper Changi Road 14248.05418
    ## 2132             Blk 210 Toa Payoh Lorong 8  7826.80117
    ## 2133           Blk 216 Bedok North Street 1 14611.14659
    ## 2134              Blk 22 Toa Payoh Lorong 7  7623.96684
    ## 2135              Blk 226D Ang Mo Kio Ave 1  9482.12838
    ## 2136          Blk 226H Ang Mo Kio Street 22  9557.17615
    ## 2137          Blk 254 Jurong East Street 24 10015.40823
    ## 2138                  Blk 29 Bendemeer Road  7074.55864
    ## 2139                    Blk 320 Shunfu Road  7879.87435
    ## 2140               Blk 341 Ang Mo Kio Ave 1  9629.22403
    ## 2141              Blk 347 Jurong East Ave 1 10687.26322
    ## 2142                 Blk 353 Clementi Ave 2  5139.40172
    ## 2143              Blk 36 Telok Blangah Rise  2221.80275
    ## 2144             Blk 37A Teban Gardens Road  8170.57317
    ## 2145              Blk 409 Ang Mo Kio Ave 10  9895.57730
    ## 2146                   Blk 44 Holland Drive  2897.76674
    ## 2147                 Blk 448 Clementi Ave 3  5659.37665
    ## 2148             Blk 453A Ang Mo Kio Ave 10 10468.68190
    ## 2149                      Blk 49 Sims Place  8604.98094
    ## 2150                  Blk 4A Eunos Crescent 11317.46813
    ## 2151                      Blk 4A Jalan Batu  8608.85051
    ## 2152           Blk 4A Woodlands Centre Road 17534.80142
    ## 2153               Blk 502 West Coast Drive  6106.97433
    ## 2154               Blk 503 West Coast Drive  6044.56760
    ## 2155          Blk 505 Jurong West Street 52 12151.65702
    ## 2156                 Blk 50A Marine Terrace 12158.17928
    ## 2157                Blk 51 Old Airport Road  8962.85548
    ## 2158           Blk 511 Bedok North Street 3 14559.74498
    ## 2159 Bukit Panjang Hawker Centre and Market 10779.54464
    ## 2160         Our Tampines Hub Hawker Centre 16408.25561
    ## 2161        Kampung Admiralty Hawker Centre 16935.96584
    ## 2162                   Yishun Hawker Centre 15801.09944
    ## 2163   Jurong West Hawker Centre and Market 13707.80915
    ## 2164        Pasir Ris Central Hawker Centre 18606.08050
    ## 2165                   Dawson Hawker Centre  1225.04185
    ## 2166      Woodlands Street 12 Hawker Centre 16512.80860
    ## 2167              Blk 527 Ang Mo Kio Ave 10 10800.86654
    ## 2168           Blk 538 Bedok North Street 3 13903.09733
    ## 2169           Blk 58 New Upper Changi Road 15364.11566
    ## 2170              Blk 6 Tanjong Pagar Plaza  4063.65369
    ## 2171               Blk 628 Ang Mo Kio Ave 4 11014.88359
    ## 2172           Blk 630 Bedok Reservoir Road 12848.14394
    ## 2173                   Blk 69 Geylang Bahru  7872.85247
    ## 2174                     Blk 7 Empress Road  3250.91488
    ## 2175               Blk 724 Ang Mo Kio Ave 6 10349.04023
    ## 2176         Blk 726 Clementi West Street 2  5224.29566
    ## 2177              Blk 74 Toa Payoh Lorong 4  7178.85145
    ## 2178              Market Street Food Centre  4675.66664
    ## 2179                    Maxwell Food Centre  4153.83941
    ## 2180                     Newton Food Centre  4454.62361
    ## 2181 North Bridge Road Market & Food Centre  6545.29137
    ## 2182              Pasir Panjang Food Centre  2226.30220
    ## 2183           Pek Kio Market & Food Centre  5690.31005
    ## 2184              People's Park Food Centre  3840.21198
    ## 2185            Sembawang Hills Food Centre  9727.89034
    ## 2186                Serangoon Garden Market 10655.85430
    ## 2187      Taman Jurong Market & Food Centre 10984.06642
    ## 2188                    Tanglin Halt Market  1925.34139
    ## 2189                           Tekka Market  5177.05817
    ## 2190                     Tiong Bahru Market  2703.44370
    ## 2191             Zion Riverside Food Centre  2634.80490
    ## 2192              Blk 75 Toa Payoh Lorong 5  7371.10376
    ## 2193                    Blk 79 Redhill Lane  1147.85300
    ## 2194             Blk 79 Telok Blangah Drive  1503.13570
    ## 2195                    Blk 80 Circuit Road  9886.84208
    ## 2196             Blk 82 Telok Blangah Drive  1441.92640
    ## 2197           Blk 84 Marine Parade Central 11060.68163
    ## 2198            Blk 85 Bedok North Street 4 15364.41793
    ## 2199                    Blk 85 Redhill Lane  1137.15804
    ## 2200                    Blk 89 Circuit Road  9504.25844
    ## 2201                   Blk 90 Whampoa Drive  6573.32356
    ## 2202              Blk 93 Toa Payoh Lorong 4  7325.96257
    ## 2203                   Blks 13/14 Haig Road 10222.70365
    ## 2204          Blks 160/162 Ang Mo Kio Ave 4 10279.00825
    ## 2205                  Ci Yuan Hawker Centre 12849.17616
    ## 2206     Albert Centre Market & Food Centre  5138.67763
    ## 2207          Alexandra Village Food Centre  4320.11476
    ## 2208                Amoy Street Food Centre  6140.21269
    ## 2209                      Bedok Food Centre 15731.85946
    ## 2210                    Beo Crescent Market  4171.56358
    ## 2211                     Berseh Food Centre  5101.04887
    ## 2212                      Blk 1 Jalan Kukoh  4898.30933
    ## 2213                  Blk 105 Hougang Ave 1  9064.60439
    ## 2214          Blk 11 Telok Blangah Crescent  5195.24090
    ## 2215              Blk 112 Jalan Bukit Merah  5054.62639
    ## 2216               Blk 115 Bukit Merah View  4360.63864
    ## 2217                 Blk 117 Aljunied Ave 2  8117.08458
    ## 2218             Blk 127 Toa Payoh Lorong 1  3735.52415
    ## 2219             Blk 137 Tampines Street 11 14712.22104
    ## 2220                  Blk 159 Mei Chin Road  3639.15014
    ## 2221                Blk 16 Bedok South Road 13513.53923
    ## 2222            Blk 163 Bukit Merah Central  4482.88474
    ## 2223            Blk 17 Upper Boon Keng Road  6473.00156
    ## 2224                   Blk 20 Ghim Moh Road  3231.22359
    ## 2225         Blk 208B New Upper Changi Road 12931.24186
    ## 2226             Blk 210 Toa Payoh Lorong 8  4819.25422
    ## 2227           Blk 216 Bedok North Street 1 13253.75083
    ## 2228              Blk 22 Toa Payoh Lorong 7  4926.36991
    ## 2229              Blk 226D Ang Mo Kio Ave 1  5479.62707
    ## 2230          Blk 226H Ang Mo Kio Street 22  5562.81288
    ## 2231          Blk 254 Jurong East Street 24  8764.72141
    ## 2232                  Blk 29 Bendemeer Road  5464.34714
    ## 2233                    Blk 320 Shunfu Road  3996.91958
    ## 2234               Blk 341 Ang Mo Kio Ave 1  5824.69478
    ## 2235              Blk 347 Jurong East Ave 1  9484.52335
    ## 2236                 Blk 353 Clementi Ave 2  4941.46227
    ## 2237              Blk 36 Telok Blangah Rise  5771.41107
    ## 2238             Blk 37A Teban Gardens Road  7940.33911
    ## 2239              Blk 409 Ang Mo Kio Ave 10  6267.24291
    ## 2240                   Blk 44 Holland Drive  2967.14469
    ## 2241                 Blk 448 Clementi Ave 3  5655.24850
    ## 2242             Blk 453A Ang Mo Kio Ave 10  6769.60220
    ## 2243                      Blk 49 Sims Place  7303.63795
    ## 2244                  Blk 4A Eunos Crescent 10035.58714
    ## 2245                      Blk 4A Jalan Batu  8128.12373
    ## 2246           Blk 4A Woodlands Centre Road 13805.58415
    ## 2247               Blk 502 West Coast Drive  6268.80197
    ## 2248               Blk 503 West Coast Drive  6207.32420
    ## 2249          Blk 505 Jurong West Street 52 11019.43722
    ## 2250                 Blk 50A Marine Terrace 11487.56000
    ## 2251                Blk 51 Old Airport Road  8167.04034
    ## 2252           Blk 511 Bedok North Street 3 12997.37833
    ## 2253 Bukit Panjang Hawker Centre and Market  7514.20339
    ## 2254         Our Tampines Hub Hawker Centre 14355.04941
    ## 2255        Kampung Admiralty Hawker Centre 12891.85422
    ## 2256                   Yishun Hawker Centre 11662.82526
    ## 2257   Jurong West Hawker Centre and Market 13131.52529
    ## 2258        Pasir Ris Central Hawker Centre 16231.44920
    ## 2259                   Dawson Hawker Centre  3128.70886
    ## 2260      Woodlands Street 12 Hawker Centre 12688.03425
    ## 2261              Blk 527 Ang Mo Kio Ave 10  7003.86685
    ## 2262           Blk 538 Bedok North Street 3 12346.46159
    ## 2263           Blk 58 New Upper Changi Road 14130.20125
    ## 2264              Blk 6 Tanjong Pagar Plaza  6166.62663
    ## 2265               Blk 628 Ang Mo Kio Ave 4  6948.48626
    ## 2266           Blk 630 Bedok Reservoir Road 11161.31530
    ## 2267                   Blk 69 Geylang Bahru  6221.38828
    ## 2268                     Blk 7 Empress Road  1289.08581
    ## 2269               Blk 724 Ang Mo Kio Ave 6  6419.46922
    ## 2270         Blk 726 Clementi West Street 2  5992.11460
    ## 2271              Blk 74 Toa Payoh Lorong 4  4365.17235
    ## 2272              Market Street Food Centre  5979.59773
    ## 2273                    Maxwell Food Centre  5944.73571
    ## 2274                     Newton Food Centre  3133.93157
    ## 2275 North Bridge Road Market & Food Centre  5894.99243
    ## 2276              Pasir Panjang Food Centre  5903.77878
    ## 2277           Pek Kio Market & Food Centre  4108.85788
    ## 2278              People's Park Food Centre  5360.33739
    ## 2279            Sembawang Hills Food Centre  5584.72800
    ## 2280                Serangoon Garden Market  7269.80093
    ## 2281      Taman Jurong Market & Food Centre 10375.07177
    ## 2282                    Tanglin Halt Market  3170.40311
    ## 2283                           Tekka Market  4506.62786
    ## 2284                     Tiong Bahru Market  4768.24212
    ## 2285             Zion Riverside Food Centre  3993.46094
    ## 2286              Blk 75 Toa Payoh Lorong 5  4511.73064
    ## 2287                    Blk 79 Redhill Lane  4038.85551
    ## 2288             Blk 79 Telok Blangah Drive  5660.40008
    ## 2289                    Blk 80 Circuit Road  8130.48869
    ## 2290             Blk 82 Telok Blangah Drive  5596.11516
    ## 2291           Blk 84 Marine Parade Central 10538.69661
    ## 2292            Blk 85 Bedok North Street 4 13890.27279
    ## 2293                    Blk 85 Redhill Lane  4095.96234
    ## 2294                    Blk 89 Circuit Road  7933.58528
    ## 2295                   Blk 90 Whampoa Drive  4561.75181
    ## 2296              Blk 93 Toa Payoh Lorong 4  4241.48214
    ## 2297                   Blks 13/14 Haig Road  9118.34551
    ## 2298          Blks 160/162 Ang Mo Kio Ave 4  6226.85980
    ## 2299                  Ci Yuan Hawker Centre  9529.81209
    ## 2300          Alexandra Village Food Centre  5767.57244
    ## 2301                Amoy Street Food Centre  2553.02456
    ## 2302                      Bedok Food Centre 11470.97987
    ## 2303                    Beo Crescent Market  3277.99636
    ## 2304                     Berseh Food Centre   759.71802
    ## 2305                      Blk 1 Jalan Kukoh  2124.79850
    ## 2306                  Blk 105 Hougang Ave 1  7075.87925
    ## 2307          Blk 11 Telok Blangah Crescent  4743.48095
    ## 2308              Blk 112 Jalan Bukit Merah  3903.69380
    ## 2309               Blk 115 Bukit Merah View  3968.40209
    ## 2310                 Blk 117 Aljunied Ave 2  4248.58018
    ## 2311             Blk 127 Toa Payoh Lorong 1  4234.88165
    ## 2312             Blk 137 Tampines Street 11 11207.47571
    ## 2313                  Blk 159 Mei Chin Road  5774.29543
    ## 2314                Blk 16 Bedok South Road  9305.57621
    ## 2315            Blk 163 Bukit Merah Central  4562.97964
    ## 2316            Blk 17 Upper Boon Keng Road  2481.76358
    ## 2317                   Blk 20 Ghim Moh Road  7425.77723
    ## 2318         Blk 208B New Upper Changi Road  8889.52692
    ## 2319             Blk 210 Toa Payoh Lorong 8  4341.14783
    ## 2320           Blk 216 Bedok North Street 1  9253.20728
    ## 2321              Blk 22 Toa Payoh Lorong 7  3798.00629
    ## 2322              Blk 226D Ang Mo Kio Ave 1  7462.89598
    ## 2323          Blk 226H Ang Mo Kio Street 22  7493.17530
    ## 2324          Blk 254 Jurong East Street 24 13781.18986
    ## 2325                  Blk 29 Bendemeer Road  2240.13038
    ## 2326                    Blk 320 Shunfu Road  5955.24062
    ## 2327               Blk 341 Ang Mo Kio Ave 1  7009.11208
    ## 2328              Blk 347 Jurong East Ave 1 14500.58353
    ## 2329                 Blk 353 Clementi Ave 2  9395.82373
    ## 2330              Blk 36 Telok Blangah Rise  4766.24168
    ## 2331             Blk 37A Teban Gardens Road 12581.74234
    ## 2332              Blk 409 Ang Mo Kio Ave 10  6831.14482
    ## 2333                   Blk 44 Holland Drive  6882.60295
    ## 2334                 Blk 448 Clementi Ave 3 10074.57959
    ## 2335             Blk 453A Ang Mo Kio Ave 10  7438.76047
    ## 2336                      Blk 49 Sims Place  3321.27994
    ## 2337                  Blk 4A Eunos Crescent  5966.04431
    ## 2338                      Blk 4A Jalan Batu  3312.27194
    ## 2339           Blk 4A Woodlands Centre Road 18075.96314
    ## 2340               Blk 502 West Coast Drive 10645.99093
    ## 2341               Blk 503 West Coast Drive 10580.19442
    ## 2342          Blk 505 Jurong West Street 52 16037.92834
    ## 2343                 Blk 50A Marine Terrace  6870.29835
    ## 2344                Blk 51 Old Airport Road  3609.34872
    ## 2345           Blk 511 Bedok North Street 3  9218.91993
    ## 2346 Bukit Panjang Hawker Centre and Market 12427.99258
    ## 2347         Our Tampines Hub Hawker Centre 11168.53525
    ## 2348        Kampung Admiralty Hawker Centre 16476.34869
    ## 2349                   Yishun Hawker Centre 13753.46477
    ## 2350   Jurong West Hawker Centre and Market 18009.94298
    ## 2351        Pasir Ris Central Hawker Centre 13471.42405
    ## 2352                   Dawson Hawker Centre  5522.21052
    ## 2353      Woodlands Street 12 Hawker Centre 16833.98312
    ## 2354              Blk 527 Ang Mo Kio Ave 10  7935.84702
    ## 2355           Blk 538 Bedok North Street 3  8565.90052
    ## 2356           Blk 58 New Upper Changi Road 10007.44615
    ## 2357              Blk 6 Tanjong Pagar Plaza  2963.50776
    ## 2358               Blk 628 Ang Mo Kio Ave 4  8975.76647
    ## 2359           Blk 630 Bedok Reservoir Road  7549.68851
    ## 2360                   Blk 69 Geylang Bahru  2864.92361
    ## 2361                     Blk 7 Empress Road  5662.67550
    ## 2362               Blk 724 Ang Mo Kio Ave 6  7918.89620
    ## 2363         Blk 726 Clementi West Street 2 10015.03336
    ## 2364              Blk 74 Toa Payoh Lorong 4  3717.58534
    ## 2365              Market Street Food Centre  1950.98966
    ## 2366                    Maxwell Food Centre  2534.93966
    ## 2367                     Newton Food Centre  2026.84389
    ## 2368 North Bridge Road Market & Food Centre  1199.12459
    ## 2369              Pasir Panjang Food Centre  7520.69851
    ## 2370           Pek Kio Market & Food Centre  1733.86771
    ## 2371              People's Park Food Centre  2193.22625
    ## 2372            Sembawang Hills Food Centre  8377.28955
    ## 2373                Serangoon Garden Market  7012.79925
    ## 2374      Taman Jurong Market & Food Centre 15227.18074
    ## 2375                    Tanglin Halt Market  6297.54362
    ## 2376                           Tekka Market   706.37275
    ## 2377                     Tiong Bahru Market  3002.73169
    ## 2378             Zion Riverside Food Centre  2735.24985
    ## 2379              Blk 75 Toa Payoh Lorong 5  3876.82878
    ## 2380                    Blk 79 Redhill Lane  4241.96019
    ## 2381             Blk 79 Telok Blangah Drive  6017.91524
    ## 2382                    Blk 80 Circuit Road  4715.22510
    ## 2383             Blk 82 Telok Blangah Drive  5957.01764
    ## 2384           Blk 84 Marine Parade Central  5802.98886
    ## 2385            Blk 85 Bedok North Street 4 10010.93403
    ## 2386                    Blk 85 Redhill Lane  4268.91972
    ## 2387                    Blk 89 Circuit Road  4271.37078
    ## 2388                   Blk 90 Whampoa Drive  2428.37696
    ## 2389              Blk 93 Toa Payoh Lorong 4  4176.46191
    ## 2390                   Blks 13/14 Haig Road  4865.29324
    ## 2391          Blks 160/162 Ang Mo Kio Ave 4  8295.52805
    ## 2392                  Ci Yuan Hawker Centre  8830.55624
    ## 2393                Amoy Street Food Centre  4752.24726
    ## 2394                      Bedok Food Centre 17220.10075
    ## 2395                    Beo Crescent Market  2560.35854
    ## 2396                     Berseh Food Centre  6274.89762
    ## 2397                      Blk 1 Jalan Kukoh  3951.81229
    ## 2398                  Blk 105 Hougang Ave 1 12104.10575
    ## 2399          Blk 11 Telok Blangah Crescent  1859.03367
    ## 2400              Blk 112 Jalan Bukit Merah  2494.46469
    ## 2401               Blk 115 Bukit Merah View  1956.25681
    ## 2402                 Blk 117 Aljunied Ave 2  9939.20671
    ## 2403             Blk 127 Toa Payoh Lorong 1  7270.40344
    ## 2404             Blk 137 Tampines Street 11 16919.86623
    ## 2405                  Blk 159 Mei Chin Road   787.24451
    ## 2406                Blk 16 Bedok South Road 15068.46589
    ## 2407            Blk 163 Bukit Merah Central  1420.99772
    ## 2408            Blk 17 Upper Boon Keng Road  8118.45587
    ## 2409                   Blk 20 Ghim Moh Road  3276.06729
    ## 2410         Blk 208B New Upper Changi Road 14656.30814
    ## 2411             Blk 210 Toa Payoh Lorong 8  8151.82094
    ## 2412           Blk 216 Bedok North Street 1 15019.07821
    ## 2413              Blk 22 Toa Payoh Lorong 7  7967.16496
    ## 2414              Blk 226D Ang Mo Kio Ave 1  9703.71872
    ## 2415          Blk 226H Ang Mo Kio Street 22  9780.75469
    ## 2416          Blk 254 Jurong East Street 24  9748.93353
    ## 2417                  Blk 29 Bendemeer Road  7462.12153
    ## 2418                    Blk 320 Shunfu Road  8118.42664
    ## 2419               Blk 341 Ang Mo Kio Ave 1  9886.53963
    ## 2420              Blk 347 Jurong East Ave 1 10412.58328
    ## 2421                 Blk 353 Clementi Ave 2  4864.20533
    ## 2422              Blk 36 Telok Blangah Rise  2475.72709
    ## 2423             Blk 37A Teban Gardens Road  7847.94646
    ## 2424              Blk 409 Ang Mo Kio Ave 10 10176.64581
    ## 2425                   Blk 44 Holland Drive  2742.49982
    ## 2426                 Blk 448 Clementi Ave 3  5356.09028
    ## 2427             Blk 453A Ang Mo Kio Ave 10 10742.95478
    ## 2428                      Blk 49 Sims Place  9006.84473
    ## 2429                  Blk 4A Eunos Crescent 11724.07773
    ## 2430                      Blk 4A Jalan Batu  9020.13100
    ## 2431           Blk 4A Woodlands Centre Road 17512.12739
    ## 2432               Blk 502 West Coast Drive  5783.64424
    ## 2433               Blk 503 West Coast Drive  5722.19921
    ## 2434          Blk 505 Jurong West Street 52 11863.92682
    ## 2435                 Blk 50A Marine Terrace 12569.55902
    ## 2436                Blk 51 Old Airport Road  9372.68563
    ## 2437           Blk 511 Bedok North Street 3 14964.49774
    ## 2438 Bukit Panjang Hawker Centre and Market 10705.48758
    ## 2439         Our Tampines Hub Hawker Centre 16802.77876
    ## 2440        Kampung Admiralty Hawker Centre 16993.42762
    ## 2441                   Yishun Hawker Centre 15979.85959
    ## 2442   Jurong West Hawker Centre and Market 13377.70549
    ## 2443        Pasir Ris Central Hawker Centre 18991.24141
    ## 2444                   Dawson Hawker Centre  1237.35582
    ## 2445      Woodlands Street 12 Hawker Centre 16511.67126
    ## 2446              Blk 527 Ang Mo Kio Ave 10 11062.57680
    ## 2447           Blk 538 Bedok North Street 3 14307.46933
    ## 2448           Blk 58 New Upper Changi Road 15773.71815
    ## 2449              Blk 6 Tanjong Pagar Plaza  4435.68126
    ## 2450               Blk 628 Ang Mo Kio Ave 4 11222.20368
    ## 2451           Blk 630 Bedok Reservoir Road 13248.84255
    ## 2452                   Blk 69 Geylang Bahru  8263.23580
    ## 2453                     Blk 7 Empress Road  3313.95131
    ## 2454               Blk 724 Ang Mo Kio Ave 6 10587.61350
    ## 2455         Blk 726 Clementi West Street 2  4876.30406
    ## 2456              Blk 74 Toa Payoh Lorong 4  7511.44530
    ## 2457              Market Street Food Centre  5075.28264
    ## 2458                    Maxwell Food Centre  4541.15527
    ## 2459                     Newton Food Centre  4821.38604
    ## 2460 North Bridge Road Market & Food Centre  6952.53053
    ## 2461              Pasir Panjang Food Centre  1855.03972
    ## 2462           Pek Kio Market & Food Centre  6067.21816
    ## 2463              People's Park Food Centre  4241.50248
    ## 2464            Sembawang Hills Food Centre  9902.02887
    ## 2465                Serangoon Garden Market 10964.47636
    ## 2466      Taman Jurong Market & Food Centre 10666.55918
    ## 2467                    Tanglin Halt Market  1775.14903
    ## 2468                           Tekka Market  5576.88046
    ## 2469                     Tiong Bahru Market  3103.41393
    ## 2470             Zion Riverside Food Centre  3045.72342
    ## 2471              Blk 75 Toa Payoh Lorong 5  7702.46009
    ## 2472                    Blk 79 Redhill Lane  1558.02105
    ## 2473             Blk 79 Telok Blangah Drive  1471.70126
    ## 2474                    Blk 80 Circuit Road 10280.79263
    ## 2475             Blk 82 Telok Blangah Drive  1422.29597
    ## 2476           Blk 84 Marine Parade Central 11471.96068
    ## 2477            Blk 85 Bedok North Street 4 15771.11421
    ## 2478                    Blk 85 Redhill Lane  1545.57543
    ## 2479                    Blk 89 Circuit Road  9902.22616
    ## 2480                   Blk 90 Whampoa Drive  6942.01174
    ## 2481              Blk 93 Toa Payoh Lorong 4  7641.62734
    ## 2482                   Blks 13/14 Haig Road 10630.60072
    ## 2483          Blks 160/162 Ang Mo Kio Ave 4 10489.57647
    ## 2484                  Ci Yuan Hawker Centre 13169.44169
    ## 2485                      Bedok Food Centre 12941.72398
    ## 2486                    Beo Crescent Market  2390.98801
    ## 2487                     Berseh Food Centre  3311.70467
    ## 2488                      Blk 1 Jalan Kukoh  1241.99745
    ## 2489                  Blk 105 Hougang Ave 1  9564.91090
    ## 2490          Blk 11 Telok Blangah Crescent  3119.01708
    ## 2491              Blk 112 Jalan Bukit Merah  2293.53876
    ## 2492               Blk 115 Bukit Merah View  2820.05060
    ## 2493                 Blk 117 Aljunied Ave 2  6419.82659
    ## 2494             Blk 127 Toa Payoh Lorong 1  6513.35497
    ## 2495             Blk 137 Tampines Street 11 13150.51048
    ## 2496                  Blk 159 Mei Chin Road  5104.77757
    ## 2497                Blk 16 Bedok South Road 10901.70854
    ## 2498            Blk 163 Bukit Merah Central  3332.76448
    ## 2499            Blk 17 Upper Boon Keng Road  4840.26798
    ## 2500                   Blk 20 Ghim Moh Road  7386.12102
    ## 2501         Blk 208B New Upper Changi Road 10622.04897
    ## 2502             Blk 210 Toa Payoh Lorong 8  6805.18162
    ## 2503           Blk 216 Bedok North Street 1 10995.74987
    ## 2504              Blk 22 Toa Payoh Lorong 7  6301.44733
    ## 2505              Blk 226D Ang Mo Kio Ave 1  9717.68047
    ## 2506          Blk 226H Ang Mo Kio Street 22  9760.01324
    ## 2507          Blk 254 Jurong East Street 24 14039.73935
    ## 2508                  Blk 29 Bendemeer Road  4785.25370
    ## 2509                    Blk 320 Shunfu Road  8118.86767
    ## 2510               Blk 341 Ang Mo Kio Ave 1  9388.12931
    ## 2511              Blk 347 Jurong East Ave 1 14740.69948
    ## 2512                 Blk 353 Clementi Ave 2  9285.12633
    ## 2513              Blk 36 Telok Blangah Rise  2831.49943
    ## 2514             Blk 37A Teban Gardens Road 12426.60846
    ## 2515              Blk 409 Ang Mo Kio Ave 10  9289.91388
    ## 2516                   Blk 44 Holland Drive  6790.08883
    ## 2517                 Blk 448 Clementi Ave 3  9881.80720
    ## 2518             Blk 453A Ang Mo Kio Ave 10  9903.16116
    ## 2519                      Blk 49 Sims Place  5555.49843
    ## 2520                  Blk 4A Eunos Crescent  7863.65425
    ## 2521                      Blk 4A Jalan Batu  4883.09571
    ## 2522           Blk 4A Woodlands Centre Road 19796.48108
    ## 2523               Blk 502 West Coast Drive 10380.31158
    ## 2524               Blk 503 West Coast Drive 10315.92486
    ## 2525          Blk 505 Jurong West Street 52 16250.21089
    ## 2526                 Blk 50A Marine Terrace  8237.55150
    ## 2527                Blk 51 Old Airport Road  5418.88992
    ## 2528           Blk 511 Bedok North Street 3 11093.85460
    ## 2529 Bukit Panjang Hawker Centre and Market 13652.15116
    ## 2530         Our Tampines Hub Hawker Centre 13248.45859
    ## 2531        Kampung Admiralty Hawker Centre 18490.87526
    ## 2532                   Yishun Hawker Centre 16123.05874
    ## 2533   Jurong West Hawker Centre and Market 17966.17665
    ## 2534        Pasir Ris Central Hawker Centre 15649.80570
    ## 2535                   Dawson Hawker Centre  5080.93156
    ## 2536      Woodlands Street 12 Hawker Centre 18613.98850
    ## 2537              Blk 527 Ang Mo Kio Ave 10 10381.51282
    ## 2538           Blk 538 Bedok North Street 3 10468.74502
    ## 2539           Blk 58 New Upper Changi Road 11635.84377
    ## 2540              Blk 6 Tanjong Pagar Plaza   479.00851
    ## 2541               Blk 628 Ang Mo Kio Ave 4 11276.52470
    ## 2542           Blk 630 Bedok Reservoir Road  9587.43542
    ## 2543                   Blk 69 Geylang Bahru  5347.87342
    ## 2544                     Blk 7 Empress Road  6128.09254
    ## 2545               Blk 724 Ang Mo Kio Ave 6 10281.14467
    ## 2546         Blk 726 Clementi West Street 2  9558.04075
    ## 2547              Blk 74 Toa Payoh Lorong 4  6147.43840
    ## 2548              Market Street Food Centre   639.72490
    ## 2549                    Maxwell Food Centre   221.27695
    ## 2550                     Newton Food Centre  3696.98687
    ## 2551 North Bridge Road Market & Food Centre  3509.54272
    ## 2552              Pasir Panjang Food Centre  6154.67637
    ## 2553           Pek Kio Market & Food Centre  4104.59817
    ## 2554              People's Park Food Centre   779.99411
    ## 2555            Sembawang Hills Food Centre 10486.68650
    ## 2556                Serangoon Garden Market  9548.06586
    ## 2557      Taman Jurong Market & Food Centre 15207.94156
    ## 2558                    Tanglin Halt Market  5950.16133
    ## 2559                           Tekka Market  3015.91570
    ## 2560                     Tiong Bahru Market  1714.40956
    ## 2561             Zion Riverside Food Centre  2247.20787
    ## 2562              Blk 75 Toa Payoh Lorong 5  6322.33554
    ## 2563                    Blk 79 Redhill Lane  3281.04950
    ## 2564             Blk 79 Telok Blangah Drive  4386.12277
    ## 2565                    Blk 80 Circuit Road  7018.09252
    ## 2566             Blk 82 Telok Blangah Drive  4342.70371
    ## 2567           Blk 84 Marine Parade Central  7120.15269
    ## 2568            Blk 85 Bedok North Street 4 11796.81987
    ## 2569                    Blk 85 Redhill Lane  3271.36600
    ## 2570                    Blk 89 Circuit Road  6527.05693
    ## 2571                   Blk 90 Whampoa Drive  4927.17931
    ## 2572              Blk 93 Toa Payoh Lorong 4  6560.23854
    ## 2573                   Blks 13/14 Haig Road  6746.86415
    ## 2574          Blks 160/162 Ang Mo Kio Ave 4 10566.61954
    ## 2575                  Ci Yuan Hawker Centre 11382.49763
    ## 2576                    Beo Crescent Market 14678.32890
    ## 2577                     Berseh Food Centre 11069.50579
    ## 2578                      Blk 1 Jalan Kukoh 13338.93813
    ## 2579                  Blk 105 Hougang Ave 1  8165.12571
    ## 2580          Blk 11 Telok Blangah Crescent 15952.90991
    ## 2581              Blk 112 Jalan Bukit Merah 15082.89896
    ## 2582               Blk 115 Bukit Merah View 15340.88809
    ## 2583                 Blk 117 Aljunied Ave 2  7618.45732
    ## 2584             Blk 127 Toa Payoh Lorong 1 12479.58942
    ## 2585             Blk 137 Tampines Street 11  3045.33157
    ## 2586                  Blk 159 Mei Chin Road 17242.43699
    ## 2587                Blk 16 Bedok South Road  2218.68505
    ## 2588            Blk 163 Bukit Merah Central 15936.06545
    ## 2589            Blk 17 Upper Boon Keng Road  9350.70664
    ## 2590                   Blk 20 Ghim Moh Road 18641.34802
    ## 2591         Blk 208B New Upper Changi Road  2848.59310
    ## 2592             Blk 210 Toa Payoh Lorong 8 11462.34417
    ## 2593           Blk 216 Bedok North Street 1  2585.13996
    ## 2594              Blk 22 Toa Payoh Lorong 7 11077.96562
    ## 2595              Blk 226D Ang Mo Kio Ave 1 13918.13637
    ## 2596          Blk 226H Ang Mo Kio Street 22 13861.72267
    ## 2597          Blk 254 Jurong East Street 24 24361.09417
    ## 2598                  Blk 29 Bendemeer Road 10289.86465
    ## 2599                    Blk 320 Shunfu Road 13638.11033
    ## 2600               Blk 341 Ang Mo Kio Ave 1 12875.74386
    ## 2601              Blk 347 Jurong East Ave 1 25069.47309
    ## 2602                 Blk 353 Clementi Ave 2 20559.02374
    ## 2603              Blk 36 Telok Blangah Rise 15762.62326
    ## 2604             Blk 37A Teban Gardens Road 23658.41219
    ## 2605              Blk 409 Ang Mo Kio Ave 10 12087.43030
    ## 2606                   Blk 44 Holland Drive 18157.49959
    ## 2607                 Blk 448 Clementi Ave 3 21266.86801
    ## 2608             Blk 453A Ang Mo Kio Ave 10 12236.19208
    ## 2609                      Blk 49 Sims Place  8472.15418
    ## 2610                  Blk 4A Eunos Crescent  5699.54074
    ## 2611                      Blk 4A Jalan Batu  8204.37147
    ## 2612           Blk 4A Woodlands Centre Road 24558.83904
    ## 2613               Blk 502 West Coast Drive 21867.24709
    ## 2614               Blk 503 West Coast Drive 21802.59098
    ## 2615          Blk 505 Jurong West Street 52 26576.61628
    ## 2616                 Blk 50A Marine Terrace  4704.49482
    ## 2617                Blk 51 Old Airport Road  7865.09924
    ## 2618           Blk 511 Bedok North Street 3  3113.43272
    ## 2619 Bukit Panjang Hawker Centre and Market 21325.22425
    ## 2620         Our Tampines Hub Hawker Centre  4098.55531
    ## 2621        Kampung Admiralty Hawker Centre 21714.53583
    ## 2622                   Yishun Hawker Centre 16904.96737
    ## 2623   Jurong West Hawker Centre and Market 28813.39821
    ## 2624        Pasir Ris Central Hawker Centre  5885.40459
    ## 2625                   Dawson Hawker Centre 16967.58097
    ## 2626      Woodlands Street 12 Hawker Centre 23214.48122
    ## 2627              Blk 527 Ang Mo Kio Ave 10 12645.55558
    ## 2628           Blk 538 Bedok North Street 3  3635.38173
    ## 2629           Blk 58 New Upper Changi Road  1651.38170
    ## 2630              Blk 6 Tanjong Pagar Plaza 13402.00022
    ## 2631               Blk 628 Ang Mo Kio Ave 4 14429.32342
    ## 2632           Blk 630 Bedok Reservoir Road  4822.16165
    ## 2633                   Blk 69 Geylang Bahru  9512.73261
    ## 2634                     Blk 7 Empress Road 16681.68103
    ## 2635               Blk 724 Ang Mo Kio Ave 6 13412.21662
    ## 2636         Blk 726 Clementi West Street 2 21358.61084
    ## 2637              Blk 74 Toa Payoh Lorong 4 11622.16088
    ## 2638              Market Street Food Centre 12409.37724
    ## 2639                    Maxwell Food Centre 13093.33420
    ## 2640                     Newton Food Centre 12933.73750
    ## 2641 North Bridge Road Market & Food Centre 10319.23962
    ## 2642              Pasir Panjang Food Centre 18909.60291
    ## 2643           Pek Kio Market & Food Centre 11722.38889
    ## 2644              People's Park Food Centre 13158.72507
    ## 2645            Sembawang Hills Food Centre 15206.15587
    ## 2646                Serangoon Garden Market 10950.93954
    ## 2647      Taman Jurong Market & Food Centre 26084.27107
    ## 2648                    Tanglin Halt Market 17699.78125
    ## 2649                           Tekka Market 11783.52540
    ## 2650                     Tiong Bahru Market 14246.86216
    ## 2651             Zion Riverside Food Centre 14174.37833
    ## 2652              Blk 75 Toa Payoh Lorong 5 11541.85735
    ## 2653                    Blk 79 Redhill Lane 15672.34326
    ## 2654             Blk 79 Telok Blangah Drive 17254.93115
    ## 2655                    Blk 80 Circuit Road  7651.19022
    ## 2656             Blk 82 Telok Blangah Drive 17203.21715
    ## 2657           Blk 84 Marine Parade Central  5821.98149
    ## 2658            Blk 85 Bedok North Street 4  2263.95083
    ## 2659                    Blk 85 Redhill Lane 15692.71220
    ## 2660                    Blk 89 Circuit Road  7800.57084
    ## 2661                   Blk 90 Whampoa Drive 11170.12522
    ## 2662              Blk 93 Toa Payoh Lorong 4 11961.32540
    ## 2663                   Blks 13/14 Haig Road  6687.26023
    ## 2664          Blks 160/162 Ang Mo Kio Ave 4 14255.97207
    ## 2665                  Ci Yuan Hawker Centre 10108.28419
    ## 2666                     Berseh Food Centre  3868.70156
    ## 2667                      Blk 1 Jalan Kukoh  1402.45628
    ## 2668                  Blk 105 Hougang Ave 1 10020.44393
    ## 2669          Blk 11 Telok Blangah Crescent  1595.44469
    ## 2670              Blk 112 Jalan Bukit Merah   986.38967
    ## 2671               Blk 115 Bukit Merah View   695.41780
    ## 2672                 Blk 117 Aljunied Ave 2  7514.68798
    ## 2673             Blk 127 Toa Payoh Lorong 1  5781.01600
    ## 2674             Blk 137 Tampines Street 11 14484.11609
    ## 2675                  Blk 159 Mei Chin Road  2764.95504
    ## 2676                Blk 16 Bedok South Road 12540.37868
    ## 2677            Blk 163 Bukit Merah Central  1285.50386
    ## 2678            Blk 17 Upper Boon Keng Road  5715.07974
    ## 2679                   Blk 20 Ghim Moh Road  4997.37221
    ## 2680         Blk 208B New Upper Changi Road 12151.32342
    ## 2681             Blk 210 Toa Payoh Lorong 8  6434.70473
    ## 2682           Blk 216 Bedok North Street 1 12517.44063
    ## 2683              Blk 22 Toa Payoh Lorong 7  6102.74004
    ## 2684              Blk 226D Ang Mo Kio Ave 1  8721.70874
    ## 2685          Blk 226H Ang Mo Kio Street 22  8782.53944
    ## 2686          Blk 254 Jurong East Street 24 11655.83419
    ## 2687                  Blk 29 Bendemeer Road  5200.83956
    ## 2688                    Blk 320 Shunfu Road  7069.17151
    ## 2689               Blk 341 Ang Mo Kio Ave 1  8643.14827
    ## 2690              Blk 347 Jurong East Ave 1 12354.60220
    ## 2691                 Blk 353 Clementi Ave 2  6895.42852
    ## 2692              Blk 36 Telok Blangah Rise  1894.46796
    ## 2693             Blk 37A Teban Gardens Road 10045.25537
    ## 2694              Blk 409 Ang Mo Kio Ave 10  8750.77800
    ## 2695                   Blk 44 Holland Drive  4400.73780
    ## 2696                 Blk 448 Clementi Ave 3  7498.39736
    ## 2697             Blk 453A Ang Mo Kio Ave 10  9355.70872
    ## 2698                      Blk 49 Sims Place  6582.73686
    ## 2699                  Blk 4A Eunos Crescent  9240.61061
    ## 2700                      Blk 4A Jalan Batu  6474.13226
    ## 2701           Blk 4A Woodlands Centre Road 17977.12743
    ## 2702               Blk 502 West Coast Drive  8006.32701
    ## 2703               Blk 503 West Coast Drive  7941.54418
    ## 2704          Blk 505 Jurong West Street 52 13861.41570
    ## 2705                 Blk 50A Marine Terrace 10015.57193
    ## 2706                Blk 51 Old Airport Road  6853.22799
    ## 2707           Blk 511 Bedok North Street 3 12496.08359
    ## 2708 Bukit Panjang Hawker Centre and Market 11560.86706
    ## 2709         Our Tampines Hub Hawker Centre 14425.97017
    ## 2710        Kampung Admiralty Hawker Centre 16970.45455
    ## 2711                   Yishun Hawker Centre 15183.64364
    ## 2712   Jurong West Hawker Centre and Market 15581.63595
    ## 2713        Pasir Ris Central Hawker Centre 16689.54341
    ## 2714                   Dawson Hawker Centre  2695.89357
    ## 2715      Woodlands Street 12 Hawker Centre 16855.12791
    ## 2716              Blk 527 Ang Mo Kio Ave 10  9761.03441
    ## 2717           Blk 538 Bedok North Street 3 11843.63892
    ## 2718           Blk 58 New Upper Changi Road 13252.16001
    ## 2719              Blk 6 Tanjong Pagar Plaza  2217.56427
    ## 2720               Blk 628 Ang Mo Kio Ave 4 10302.58183
    ## 2721           Blk 630 Bedok Reservoir Road 10822.54670
    ## 2722                   Blk 69 Geylang Bahru  5962.56216
    ## 2723                     Blk 7 Empress Road  3878.03447
    ## 2724               Blk 724 Ang Mo Kio Ave 6  9462.62529
    ## 2725         Blk 726 Clementi West Street 2  7213.97997
    ## 2726              Blk 74 Toa Payoh Lorong 4  5751.95790
    ## 2727              Market Street Food Centre  2581.86496
    ## 2728                    Maxwell Food Centre  2169.71201
    ## 2729                     Newton Food Centre  2890.06023
    ## 2730 North Bridge Road Market & Food Centre  4476.68689
    ## 2731              Pasir Panjang Food Centre  4248.71249
    ## 2732           Pek Kio Market & Food Centre  3951.48956
    ## 2733              People's Park Food Centre  1746.10621
    ## 2734            Sembawang Hills Food Centre  9241.74443
    ## 2735                Serangoon Garden Market  9314.94536
    ## 2736      Taman Jurong Market & Food Centre 12819.93322
    ## 2737                    Tanglin Halt Market  3563.72561
    ## 2738                           Tekka Market  3216.49245
    ## 2739                     Tiong Bahru Market   692.07236
    ## 2740             Zion Riverside Food Centre   577.32884
    ## 2741              Blk 75 Toa Payoh Lorong 5  5946.86717
    ## 2742                    Blk 79 Redhill Lane  1002.34344
    ## 2743             Blk 79 Telok Blangah Drive  2782.45774
    ## 2744                    Blk 80 Circuit Road  7929.12074
    ## 2745             Blk 82 Telok Blangah Drive  2717.71797
    ## 2746           Blk 84 Marine Parade Central  8915.01169
    ## 2747            Blk 85 Bedok North Street 4 13282.16261
    ## 2748                    Blk 85 Redhill Lane  1017.20503
    ## 2749                    Blk 89 Circuit Road  7513.55152
    ## 2750                   Blk 90 Whampoa Drive  4877.81240
    ## 2751              Blk 93 Toa Payoh Lorong 4  6018.58884
    ## 2752                   Blks 13/14 Haig Road  8134.23341
    ## 2753          Blks 160/162 Ang Mo Kio Ave 4  9564.07640
    ## 2754                  Ci Yuan Hawker Centre 11402.86754
    ## 2755                      Blk 1 Jalan Kukoh  2829.14916
    ## 2756                  Blk 105 Hougang Ave 1  6333.43413
    ## 2757          Blk 11 Telok Blangah Crescent  5391.41032
    ## 2758              Blk 112 Jalan Bukit Merah  4573.22292
    ## 2759               Blk 115 Bukit Merah View  4564.11816
    ## 2760                 Blk 117 Aljunied Ave 2  3665.60179
    ## 2761             Blk 127 Toa Payoh Lorong 1  3658.67524
    ## 2762             Blk 137 Tampines Street 11 10646.66919
    ## 2763                  Blk 159 Mei Chin Road  6203.29129
    ## 2764                Blk 16 Bedok South Road  8877.99927
    ## 2765            Blk 163 Bukit Merah Central  5149.31730
    ## 2766            Blk 17 Upper Boon Keng Road  1850.38747
    ## 2767                   Blk 20 Ghim Moh Road  7647.84576
    ## 2768         Blk 208B New Upper Changi Road  8417.43036
    ## 2769             Blk 210 Toa Payoh Lorong 8  3650.87077
    ## 2770           Blk 216 Bedok North Street 1  8774.40305
    ## 2771              Blk 22 Toa Payoh Lorong 7  3084.54634
    ## 2772              Blk 226D Ang Mo Kio Ave 1  6860.11773
    ## 2773          Blk 226H Ang Mo Kio Street 22  6884.64089
    ## 2774          Blk 254 Jurong East Street 24 13839.93948
    ## 2775                  Blk 29 Bendemeer Road  1482.18460
    ## 2776                    Blk 320 Shunfu Road  5409.70718
    ## 2777               Blk 341 Ang Mo Kio Ave 1  6349.72809
    ## 2778              Blk 347 Jurong East Ave 1 14560.41763
    ## 2779                 Blk 353 Clementi Ave 2  9604.42588
    ## 2780              Blk 36 Telok Blangah Rise  5459.66853
    ## 2781             Blk 37A Teban Gardens Road 12770.61874
    ## 2782              Blk 409 Ang Mo Kio Ave 10  6131.65653
    ## 2783                   Blk 44 Holland Drive  7132.05338
    ## 2784                 Blk 448 Clementi Ave 3 10297.84136
    ## 2785             Blk 453A Ang Mo Kio Ave 10  6734.68264
    ## 2786                      Blk 49 Sims Place  2732.57259
    ## 2787                  Blk 4A Eunos Crescent  5468.12025
    ## 2788                      Blk 4A Jalan Batu  3063.14425
    ## 2789           Blk 4A Woodlands Centre Road 17639.73428
    ## 2790               Blk 502 West Coast Drive 10883.39659
    ## 2791               Blk 503 West Coast Drive 10817.89460
    ## 2792          Blk 505 Jurong West Street 52 16097.90829
    ## 2793                 Blk 50A Marine Terrace  6558.18939
    ## 2794                Blk 51 Old Airport Road  3226.70831
    ## 2795           Blk 511 Bedok North Street 3  8693.11723
    ## 2796 Bukit Panjang Hawker Centre and Market 12186.65139
    ## 2797         Our Tampines Hub Hawker Centre 10560.63836
    ## 2798        Kampung Admiralty Hawker Centre 15938.23002
    ## 2799                   Yishun Hawker Centre 13082.14194
    ## 2800   Jurong West Hawker Centre and Market 18137.29065
    ## 2801        Pasir Ris Central Hawker Centre 12824.37925
    ## 2802                   Dawson Hawker Centre  5903.92099
    ## 2803      Woodlands Street 12 Hawker Centre 16379.84145
    ## 2804              Blk 527 Ang Mo Kio Ave 10  7240.10286
    ## 2805           Blk 538 Bedok North Street 3  8034.94279
    ## 2806           Blk 58 New Upper Changi Road  9562.67544
    ## 2807              Blk 6 Tanjong Pagar Plaza  3722.94933
    ## 2808               Blk 628 Ang Mo Kio Ave 4  8344.47436
    ## 2809           Blk 630 Bedok Reservoir Road  6976.83924
    ## 2810                   Blk 69 Geylang Bahru  2140.46316
    ## 2811                     Blk 7 Empress Road  5784.53470
    ## 2812               Blk 724 Ang Mo Kio Ave 6  7263.62561
    ## 2813         Blk 726 Clementi West Street 2 10311.85962
    ## 2814              Blk 74 Toa Payoh Lorong 4  3057.04844
    ## 2815              Market Street Food Centre  2704.01527
    ## 2816                    Maxwell Food Centre  3294.44195
    ## 2817                     Newton Food Centre  1989.20212
    ## 2818 North Bridge Road Market & Food Centre   802.19590
    ## 2819              Pasir Panjang Food Centre  8070.71884
    ## 2820           Pek Kio Market & Food Centre  1223.16539
    ## 2821              People's Park Food Centre  2937.53038
    ## 2822            Sembawang Hills Food Centre  7832.51552
    ## 2823                Serangoon Garden Market  6269.59665
    ## 2824      Taman Jurong Market & Food Centre 15359.86480
    ## 2825                    Tanglin Halt Market  6631.19529
    ## 2826                           Tekka Market   714.71111
    ## 2827                     Tiong Bahru Market  3672.84093
    ## 2828             Zion Riverside Food Centre  3302.59043
    ## 2829              Blk 75 Toa Payoh Lorong 5  3203.48201
    ## 2830                    Blk 79 Redhill Lane  4790.75210
    ## 2831             Blk 79 Telok Blangah Drive  6642.46791
    ## 2832                    Blk 80 Circuit Road  4061.62593
    ## 2833             Blk 82 Telok Blangah Drive  6579.35375
    ## 2834           Blk 84 Marine Parade Central  5535.43216
    ## 2835            Blk 85 Bedok North Street 4  9511.22850
    ## 2836                    Blk 85 Redhill Lane  4823.54821
    ## 2837                    Blk 89 Circuit Road  3646.95416
    ## 2838                   Blk 90 Whampoa Drive  1736.22989
    ## 2839              Blk 93 Toa Payoh Lorong 4  3538.69242
    ## 2840                   Blks 13/14 Haig Road  4398.63392
    ## 2841          Blks 160/162 Ang Mo Kio Ave 4  7680.50937
    ## 2842                  Ci Yuan Hawker Centre  8071.12948
    ## 2843                  Blk 105 Hougang Ave 1  9151.97319
    ## 2844          Blk 11 Telok Blangah Crescent  2657.93061
    ## 2845              Blk 112 Jalan Bukit Merah  1797.67315
    ## 2846               Blk 115 Bukit Merah View  2014.41559
    ## 2847                 Blk 117 Aljunied Ave 2  6347.78093
    ## 2848             Blk 127 Toa Payoh Lorong 1  5539.16273
    ## 2849             Blk 137 Tampines Street 11 13266.98074
    ## 2850                  Blk 159 Mei Chin Road  4159.45439
    ## 2851                Blk 16 Bedok South Road 11222.92313
    ## 2852            Blk 163 Bukit Merah Central  2603.35508
    ## 2853            Blk 17 Upper Boon Keng Road  4605.02163
    ## 2854                   Blk 20 Ghim Moh Road  6281.88483
    ## 2855         Blk 208B New Upper Changi Road 10864.31193
    ## 2856             Blk 210 Toa Payoh Lorong 8  5973.74909
    ## 2857           Blk 216 Bedok North Street 1 11233.70544
    ## 2858              Blk 22 Toa Payoh Lorong 7  5534.21169
    ## 2859              Blk 226D Ang Mo Kio Ave 1  8686.80425
    ## 2860          Blk 226H Ang Mo Kio Street 22  8735.37013
    ## 2861          Blk 254 Jurong East Street 24 12905.30798
    ## 2862                  Blk 29 Bendemeer Road  4281.90815
    ## 2863                    Blk 320 Shunfu Road  7059.57420
    ## 2864               Blk 341 Ang Mo Kio Ave 1  8440.62285
    ## 2865              Blk 347 Jurong East Ave 1 13611.96024
    ## 2866                 Blk 353 Clementi Ave 2  8216.31390
    ## 2867              Blk 36 Telok Blangah Rise  2641.51040
    ## 2868             Blk 37A Teban Gardens Road 11387.51850
    ## 2869              Blk 409 Ang Mo Kio Ave 10  8418.91632
    ## 2870                   Blk 44 Holland Drive  5691.79939
    ## 2871                 Blk 448 Clementi Ave 3  8839.88356
    ## 2872             Blk 453A Ang Mo Kio Ave 10  9033.51512
    ## 2873                      Blk 49 Sims Place  5429.09488
    ## 2874                  Blk 4A Eunos Crescent  7989.47747
    ## 2875                      Blk 4A Jalan Batu  5141.42815
    ## 2876           Blk 4A Woodlands Centre Road 18579.56072
    ## 2877               Blk 502 West Coast Drive  9362.98452
    ## 2878               Blk 503 West Coast Drive  9297.77345
    ## 2879          Blk 505 Jurong West Street 52 15130.94613
    ## 2880                 Blk 50A Marine Terrace  8657.80428
    ## 2881                Blk 51 Old Airport Road  5566.04659
    ## 2882           Blk 511 Bedok North Street 3 11250.25458
    ## 2883 Bukit Panjang Hawker Centre and Market 12410.21097
    ## 2884         Our Tampines Hub Hawker Centre 13265.67768
    ## 2885        Kampung Admiralty Hawker Centre 17333.84423
    ## 2886                   Yishun Hawker Centre 15134.65461
    ## 2887   Jurong West Hawker Centre and Market 16912.55779
    ## 2888        Pasir Ris Central Hawker Centre 15588.75184
    ## 2889                   Dawson Hawker Centre  4052.23017
    ## 2890      Woodlands Street 12 Hawker Centre 17406.58004
    ## 2891              Blk 527 Ang Mo Kio Ave 10  9486.89081
    ## 2892           Blk 538 Bedok North Street 3 10604.81563
    ## 2893           Blk 58 New Upper Changi Road 11942.41183
    ## 2894              Blk 6 Tanjong Pagar Plaza  1333.51716
    ## 2895               Blk 628 Ang Mo Kio Ave 4 10261.04711
    ## 2896           Blk 630 Bedok Reservoir Road  9628.20951
    ## 2897                   Blk 69 Geylang Bahru  4969.22481
    ## 2898                     Blk 7 Empress Road  4916.90426
    ## 2899               Blk 724 Ang Mo Kio Ave 6  9313.40155
    ## 2900         Blk 726 Clementi West Street 2  8596.43754
    ## 2901              Blk 74 Toa Payoh Lorong 4  5294.31588
    ## 2902              Market Street Food Centre  1221.46320
    ## 2903                    Maxwell Food Centre  1049.29773
    ## 2904                     Newton Food Centre  2616.74786
    ## 2905 North Bridge Road Market & Food Centre  3295.59218
    ## 2906              Pasir Panjang Food Centre  5571.66182
    ## 2907           Pek Kio Market & Food Centre  3293.35395
    ## 2908              People's Park Food Centre   462.78417
    ## 2909            Sembawang Hills Food Centre  9384.84134
    ## 2910                Serangoon Garden Market  8805.18346
    ## 2911      Taman Jurong Market & Food Centre 14141.49344
    ## 2912                    Tanglin Halt Market  4910.32555
    ## 2913                           Tekka Market  2310.48506
    ## 2914                     Tiong Bahru Market   914.11084
    ## 2915             Zion Riverside Food Centre  1075.37242
    ## 2916              Blk 75 Toa Payoh Lorong 5  5480.66547
    ## 2917                    Blk 79 Redhill Lane  2397.94362
    ## 2918             Blk 79 Telok Blangah Drive  3954.50368
    ## 2919                    Blk 80 Circuit Road  6838.48035
    ## 2920             Blk 82 Telok Blangah Drive  3897.58191
    ## 2921           Blk 84 Marine Parade Central  7550.57835
    ## 2922            Blk 85 Bedok North Street 4 12011.59945
    ## 2923                    Blk 85 Redhill Lane  2406.73734
    ## 2924                    Blk 89 Circuit Road  6388.05255
    ## 2925                   Blk 90 Whampoa Drive  4194.19275
    ## 2926              Blk 93 Toa Payoh Lorong 4  5656.85872
    ## 2927                   Blks 13/14 Haig Road  6872.49409
    ## 2928          Blks 160/162 Ang Mo Kio Ave 4  9538.37776
    ## 2929                  Ci Yuan Hawker Centre 10765.77985
    ## 2930          Blk 11 Telok Blangah Crescent 11606.45858
    ## 2931              Blk 112 Jalan Bukit Merah 10839.28039
    ## 2932               Blk 115 Bukit Merah View 10696.50164
    ## 2933                 Blk 117 Aljunied Ave 2  3685.66985
    ## 2934             Blk 127 Toa Payoh Lorong 1  5338.73734
    ## 2935             Blk 137 Tampines Street 11  6137.39040
    ## 2936                  Blk 159 Mei Chin Road 11791.12913
    ## 2937                Blk 16 Bedok South Road  6256.17072
    ## 2938            Blk 163 Bukit Merah Central 11239.31900
    ## 2939            Blk 17 Upper Boon Keng Road  4757.78764
    ## 2940                   Blk 20 Ghim Moh Road 12284.96560
    ## 2941         Blk 208B New Upper Changi Road  5481.44803
    ## 2942             Blk 210 Toa Payoh Lorong 8  4245.70001
    ## 2943           Blk 216 Bedok North Street 1  5640.54389
    ## 2944              Blk 22 Toa Payoh Lorong 7  4213.91606
    ## 2945              Blk 226D Ang Mo Kio Ave 1  5837.34686
    ## 2946          Blk 226H Ang Mo Kio Street 22  5771.86421
    ## 2947          Blk 254 Jurong East Street 24 16988.21252
    ## 2948                  Blk 29 Bendemeer Road  4870.68637
    ## 2949                    Blk 320 Shunfu Road  5907.24902
    ## 2950               Blk 341 Ang Mo Kio Ave 1  4791.30503
    ## 2951              Blk 347 Jurong East Ave 1 17664.60044
    ## 2952                 Blk 353 Clementi Ave 2 13969.88233
    ## 2953              Blk 36 Telok Blangah Rise 11752.99790
    ## 2954             Blk 37A Teban Gardens Road 16781.86645
    ## 2955              Blk 409 Ang Mo Kio Ave 10  3987.21065
    ## 2956                   Blk 44 Holland Drive 11951.19999
    ## 2957                 Blk 448 Clementi Ave 3 14677.15424
    ## 2958             Blk 453A Ang Mo Kio Ave 10  4077.59859
    ## 2959                      Blk 49 Sims Place  4233.81509
    ## 2960                  Blk 4A Eunos Crescent  4026.96268
    ## 2961                      Blk 4A Jalan Batu  5726.52656
    ## 2962           Blk 4A Woodlands Centre Road 16458.17257
    ## 2963               Blk 502 West Coast Drive 15289.99528
    ## 2964               Blk 503 West Coast Drive 15230.87097
    ## 2965          Blk 505 Jurong West Street 52 19106.51643
    ## 2966                 Blk 50A Marine Terrace  6034.23431
    ## 2967                Blk 51 Old Airport Road  5062.76924
    ## 2968           Blk 511 Bedok North Street 3  5051.75391
    ## 2969 Bukit Panjang Hawker Centre and Market 13347.72082
    ## 2970         Our Tampines Hub Hawker Centre  5524.94087
    ## 2971        Kampung Admiralty Hawker Centre 13768.78415
    ## 2972                   Yishun Hawker Centre  9348.85240
    ## 2973   Jurong West Hawker Centre and Market 21487.86285
    ## 2974        Pasir Ris Central Hawker Centre  7175.03237
    ## 2975                   Dawson Hawker Centre 11361.47807
    ## 2976      Woodlands Street 12 Hawker Centre 15110.66545
    ## 2977              Blk 527 Ang Mo Kio Ave 10  4480.77998
    ## 2978           Blk 538 Bedok North Street 3  4573.33319
    ## 2979           Blk 58 New Upper Changi Road  6559.54783
    ## 2980              Blk 6 Tanjong Pagar Plaza 10008.62310
    ## 2981               Blk 628 Ang Mo Kio Ave 4  6265.77634
    ## 2982           Blk 630 Bedok Reservoir Road  3513.65743
    ## 2983                   Blk 69 Geylang Bahru  4219.91709
    ## 2984                     Blk 7 Empress Road 10276.03219
    ## 2985               Blk 724 Ang Mo Kio Ave 6  5255.84281
    ## 2986         Blk 726 Clementi West Street 2 15056.35466
    ## 2987              Blk 74 Toa Payoh Lorong 4  4744.55451
    ## 2988              Market Street Food Centre  8929.12848
    ## 2989                    Maxwell Food Centre  9582.34294
    ## 2990                     Newton Food Centre  7288.37858
    ## 2991 North Bridge Road Market & Food Centre  6062.22283
    ## 2992              Pasir Panjang Food Centre 13959.13842
    ## 2993           Pek Kio Market & Food Centre  6085.32741
    ## 2994              People's Park Food Centre  9268.24737
    ## 2995            Sembawang Hills Food Centre  7100.86867
    ## 2996                Serangoon Garden Market  2794.80255
    ## 2997      Taman Jurong Market & Food Centre 18879.37861
    ## 2998                    Tanglin Halt Market 11843.89547
    ## 2999                           Tekka Market  6863.89973
    ## 3000                     Tiong Bahru Market  9953.41073
    ## 3001             Zion Riverside Food Centre  9444.22700
    ## 3002              Blk 75 Toa Payoh Lorong 5  4577.86345
    ## 3003                    Blk 79 Redhill Lane 10810.21257
    ## 3004             Blk 79 Telok Blangah Drive 12781.92645
    ## 3005                    Blk 80 Circuit Road  2892.32784
    ## 3006             Blk 82 Telok Blangah Drive 12714.64320
    ## 3007           Blk 84 Marine Parade Central  5980.63120
    ## 3008            Blk 85 Bedok North Street 4  5929.00969
    ## 3009                    Blk 85 Redhill Lane 10854.86483
    ## 3010                    Blk 89 Circuit Road  3399.91243
    ## 3011                   Blk 90 Whampoa Drive  5176.19111
    ## 3012              Blk 93 Toa Payoh Lorong 4  4823.73330
    ## 3013                   Blks 13/14 Haig Road  4322.74366
    ## 3014          Blks 160/162 Ang Mo Kio Ave 4  6104.10631
    ## 3015                  Ci Yuan Hawker Centre  2514.94848
    ## 3016              Blk 112 Jalan Bukit Merah   870.71190
    ## 3017               Blk 115 Bukit Merah View   976.50427
    ## 3018                 Blk 117 Aljunied Ave 2  8989.61261
    ## 3019             Blk 127 Toa Payoh Lorong 1  7315.63984
    ## 3020             Blk 137 Tampines Street 11 15923.91509
    ## 3021                  Blk 159 Mei Chin Road  2479.28718
    ## 3022                Blk 16 Bedok South Road 13858.33597
    ## 3023            Blk 163 Bukit Merah Central   723.15214
    ## 3024            Blk 17 Upper Boon Keng Road  7218.41260
    ## 3025                   Blk 20 Ghim Moh Road  5028.17016
    ## 3026         Blk 208B New Upper Changi Road 13515.04910
    ## 3027             Blk 210 Toa Payoh Lorong 8  8012.34984
    ## 3028           Blk 216 Bedok North Street 1 13885.31169
    ## 3029              Blk 22 Toa Payoh Lorong 7  7693.96603
    ## 3030              Blk 226D Ang Mo Kio Ave 1 10149.73450
    ## 3031          Blk 226H Ang Mo Kio Street 22 10215.75800
    ## 3032          Blk 254 Jurong East Street 24 11591.54031
    ## 3033                  Blk 29 Bendemeer Road  6768.22009
    ## 3034                    Blk 320 Shunfu Road  8503.34847
    ## 3035               Blk 341 Ang Mo Kio Ave 1 10141.48229
    ## 3036              Blk 347 Jurong East Ave 1 12260.77280
    ## 3037                 Blk 353 Clementi Ave 2  6711.52728
    ## 3038              Blk 36 Telok Blangah Rise   653.88832
    ## 3039             Blk 37A Teban Gardens Road  9704.98078
    ## 3040              Blk 409 Ang Mo Kio Ave 10 10289.98594
    ## 3041                   Blk 44 Holland Drive  4455.31464
    ## 3042                 Blk 448 Clementi Ave 3  7214.48526
    ## 3043             Blk 453A Ang Mo Kio Ave 10 10889.51921
    ## 3044                      Blk 49 Sims Place  8064.42702
    ## 3045                  Blk 4A Eunos Crescent 10647.33501
    ## 3046                      Blk 4A Jalan Batu  7777.26757
    ## 3047           Blk 4A Woodlands Centre Road 18866.40864
    ## 3048               Blk 502 West Coast Drive  7641.00630
    ## 3049               Blk 503 West Coast Drive  7579.83472
    ## 3050          Blk 505 Jurong West Street 52 13718.84984
    ## 3051                 Blk 50A Marine Terrace 11255.85989
    ## 3052                Blk 51 Old Airport Road  8220.75146
    ## 3053           Blk 511 Bedok North Street 3 13908.06427
    ## 3054 Bukit Panjang Hawker Centre and Market 12215.62105
    ## 3055         Our Tampines Hub Hawker Centre 15910.23704
    ## 3056        Kampung Admiralty Hawker Centre 18086.31863
    ## 3057                   Yishun Hawker Centre 16582.95491
    ## 3058   Jurong West Hawker Centre and Market 15230.36112
    ## 3059        Pasir Ris Central Hawker Centre 18209.93734
    ## 3060                   Dawson Hawker Centre  2711.30892
    ## 3061      Woodlands Street 12 Hawker Centre 17801.91000
    ## 3062              Blk 527 Ang Mo Kio Ave 10 11278.09631
    ## 3063           Blk 538 Bedok North Street 3 13262.73997
    ## 3064           Blk 58 New Upper Changi Road 14582.12148
    ## 3065              Blk 6 Tanjong Pagar Plaza  2730.73705
    ## 3066               Blk 628 Ang Mo Kio Ave 4 11721.49846
    ## 3067           Blk 630 Bedok Reservoir Road 12280.55757
    ## 3068                   Blk 69 Geylang Bahru  7513.25071
    ## 3069                     Blk 7 Empress Road  4536.13700
    ## 3070               Blk 724 Ang Mo Kio Ave 6 10935.22408
    ## 3071         Blk 726 Clementi West Street 2  6719.73067
    ## 3072              Blk 74 Toa Payoh Lorong 4  7332.10372
    ## 3073              Market Street Food Centre  3566.30106
    ## 3074                    Maxwell Food Centre  2930.07799
    ## 3075                     Newton Food Centre  4471.64096
    ## 3076 North Bridge Road Market & Food Centre  5934.68741
    ## 3077              Pasir Panjang Food Centre  3035.77990
    ## 3078           Pek Kio Market & Food Centre  5545.72157
    ## 3079              People's Park Food Centre  2794.21055
    ## 3080            Sembawang Hills Food Centre 10569.59552
    ## 3081                Serangoon Garden Market 10892.15814
    ## 3082      Taman Jurong Market & Food Centre 12524.58415
    ## 3083                    Tanglin Halt Market  3489.72867
    ## 3084                           Tekka Market  4771.19414
    ## 3085                     Tiong Bahru Market  1748.16397
    ## 3086             Zion Riverside Food Centre  2166.11279
    ## 3087              Blk 75 Toa Payoh Lorong 5  7527.35022
    ## 3088                    Blk 79 Redhill Lane  1160.37502
    ## 3089             Blk 79 Telok Blangah Drive  1303.16388
    ## 3090                    Blk 80 Circuit Road  9447.45458
    ## 3091             Blk 82 Telok Blangah Drive  1250.34190
    ## 3092           Blk 84 Marine Parade Central 10141.38861
    ## 3093            Blk 85 Bedok North Street 4 14666.31686
    ## 3094                    Blk 85 Redhill Lane  1102.27497
    ## 3095                    Blk 89 Circuit Road  9013.35549
    ## 3096                   Blk 90 Whampoa Drive  6472.60616
    ## 3097              Blk 93 Toa Payoh Lorong 4  7580.32706
    ## 3098                   Blks 13/14 Haig Road  9529.99699
    ## 3099          Blks 160/162 Ang Mo Kio Ave 4 10980.62513
    ## 3100                  Ci Yuan Hawker Centre 12993.86222
    ## 3101               Blk 115 Bukit Merah View   751.31346
    ## 3102                 Blk 117 Aljunied Ave 2  8142.06349
    ## 3103             Blk 127 Toa Payoh Lorong 1  6752.42897
    ## 3104             Blk 137 Tampines Street 11 15063.86583
    ## 3105                  Blk 159 Mei Chin Road  2958.90493
    ## 3106                Blk 16 Bedok South Road 12987.67235
    ## 3107            Blk 163 Bukit Merah Central  1083.60519
    ## 3108            Blk 17 Upper Boon Keng Road  6384.35432
    ## 3109                   Blk 20 Ghim Moh Road  5424.43186
    ## 3110         Blk 208B New Upper Changi Road 12646.08955
    ## 3111             Blk 210 Toa Payoh Lorong 8  7373.60024
    ## 3112           Blk 216 Bedok North Street 1 13016.57905
    ## 3113              Blk 22 Toa Payoh Lorong 7  7015.09503
    ## 3114              Blk 226D Ang Mo Kio Ave 1  9708.09839
    ## 3115          Blk 226H Ang Mo Kio Street 22  9768.90092
    ## 3116          Blk 254 Jurong East Street 24 12067.51053
    ## 3117                  Blk 29 Bendemeer Road  5980.24296
    ## 3118                    Blk 320 Shunfu Road  8055.56104
    ## 3119               Blk 341 Ang Mo Kio Ave 1  9623.15803
    ## 3120              Blk 347 Jurong East Ave 1 12751.44248
    ## 3121                 Blk 353 Clementi Ave 2  7221.16805
    ## 3122              Blk 36 Telok Blangah Rise   935.67138
    ## 3123             Blk 37A Teban Gardens Road 10294.27544
    ## 3124              Blk 409 Ang Mo Kio Ave 10  9717.31422
    ## 3125                   Blk 44 Holland Drive  4831.96601
    ## 3126                 Blk 448 Clementi Ave 3  7770.18687
    ## 3127             Blk 453A Ang Mo Kio Ave 10 10324.33126
    ## 3128                      Blk 49 Sims Place  7220.33035
    ## 3129                  Blk 4A Eunos Crescent  9783.54466
    ## 3130                      Blk 4A Jalan Batu  6906.80557
    ## 3131           Blk 4A Woodlands Centre Road 18841.97127
    ## 3132               Blk 502 West Coast Drive  8232.65523
    ## 3133               Blk 503 West Coast Drive  8169.71594
    ## 3134          Blk 505 Jurong West Street 52 14233.69888
    ## 3135                 Blk 50A Marine Terrace 10386.77667
    ## 3136                Blk 51 Old Airport Road  7353.70550
    ## 3137           Blk 511 Bedok North Street 3 13043.89367
    ## 3138 Bukit Panjang Hawker Centre and Market 12326.43475
    ## 3139         Our Tampines Hub Hawker Centre 15061.49847
    ## 3140        Kampung Admiralty Hawker Centre 17907.06341
    ## 3141                   Yishun Hawker Centre 16169.75511
    ## 3142   Jurong West Hawker Centre and Market 15834.72648
    ## 3143        Pasir Ris Central Hawker Centre 17375.11777
    ## 3144                   Dawson Hawker Centre  3058.52285
    ## 3145      Woodlands Street 12 Hawker Centre 17739.41425
    ## 3146              Blk 527 Ang Mo Kio Ave 10 10735.67346
    ## 3147           Blk 538 Bedok North Street 3 12399.80280
    ## 3148           Blk 58 New Upper Changi Road 13711.62096
    ## 3149              Blk 6 Tanjong Pagar Plaza  1945.52700
    ## 3150               Blk 628 Ang Mo Kio Ave 4 11288.94355
    ## 3151           Blk 630 Bedok Reservoir Road 11425.77948
    ## 3152                   Blk 69 Geylang Bahru  6706.97144
    ## 3153                     Blk 7 Empress Road  4604.50861
    ## 3154               Blk 724 Ang Mo Kio Ave 6 10446.50404
    ## 3155         Blk 726 Clementi West Street 2  7357.88639
    ## 3156              Blk 74 Toa Payoh Lorong 4  6688.64865
    ## 3157              Market Street Food Centre  2706.62539
    ## 3158                    Maxwell Food Centre  2093.94360
    ## 3159                     Newton Food Centre  3834.87255
    ## 3160 North Bridge Road Market & Food Centre  5087.62353
    ## 3161              Pasir Panjang Food Centre  3878.43841
    ## 3162           Pek Kio Market & Food Centre  4821.14148
    ## 3163              People's Park Food Centre  1924.27854
    ## 3164            Sembawang Hills Food Centre 10220.30635
    ## 3165                Serangoon Garden Market 10250.88699
    ## 3166      Taman Jurong Market & Food Centre 13102.16357
    ## 3167                    Tanglin Halt Market  3906.79311
    ## 3168                           Tekka Market  3979.81287
    ## 3169                     Tiong Bahru Market   901.72525
    ## 3170             Zion Riverside Food Centre  1480.83609
    ## 3171              Blk 75 Toa Payoh Lorong 5  6882.66166
    ## 3172                    Blk 79 Redhill Lane  1213.05111
    ## 3173             Blk 79 Telok Blangah Drive  2173.87041
    ## 3174                    Blk 80 Circuit Road  8616.84158
    ## 3175             Blk 82 Telok Blangah Drive  2120.80261
    ## 3176           Blk 84 Marine Parade Central  9272.77825
    ## 3177            Blk 85 Bedok North Street 4 13798.77913
    ## 3178                    Blk 85 Redhill Lane  1177.15050
    ## 3179                    Blk 89 Circuit Road  8174.77109
    ## 3180                   Blk 90 Whampoa Drive  5748.77253
    ## 3181              Blk 93 Toa Payoh Lorong 4  6974.25528
    ## 3182                   Blks 13/14 Haig Road  8665.29850
    ## 3183          Blks 160/162 Ang Mo Kio Ave 4 10550.37957
    ## 3184                  Ci Yuan Hawker Centre 12309.54466
    ## 3185                 Blk 117 Aljunied Ave 2  8208.62447
    ## 3186             Blk 127 Toa Payoh Lorong 1  6339.18398
    ## 3187             Blk 137 Tampines Street 11 15175.74295
    ## 3188                  Blk 159 Mei Chin Road  2295.83447
    ## 3189                Blk 16 Bedok South Road 13211.54518
    ## 3190            Blk 163 Bukit Merah Central   597.40769
    ## 3191            Blk 17 Upper Boon Keng Road  6410.32086
    ## 3192                   Blk 20 Ghim Moh Road  4702.20917
    ## 3193         Blk 208B New Upper Changi Road 12831.26884
    ## 3194             Blk 210 Toa Payoh Lorong 8  7043.94795
    ## 3195           Blk 216 Bedok North Street 1 13198.23676
    ## 3196              Blk 22 Toa Payoh Lorong 7  6737.54401
    ## 3197              Blk 226D Ang Mo Kio Ave 1  9189.00093
    ## 3198          Blk 226H Ang Mo Kio Street 22  9253.82890
    ## 3199          Blk 254 Jurong East Street 24 11357.53233
    ## 3200                  Blk 29 Bendemeer Road  5889.63672
    ## 3201                    Blk 320 Shunfu Road  7540.19323
    ## 3202               Blk 341 Ang Mo Kio Ave 1  9167.46746
    ## 3203              Blk 347 Jurong East Ave 1 12046.05797
    ## 3204                 Blk 353 Clementi Ave 2  6532.23970
    ## 3205              Blk 36 Telok Blangah Rise  1432.97004
    ## 3206             Blk 37A Teban Gardens Road  9637.49537
    ## 3207              Blk 409 Ang Mo Kio Ave 10  9313.53166
    ## 3208                   Blk 44 Holland Drive  4107.00026
    ## 3209                 Blk 448 Clementi Ave 3  7101.25441
    ## 3210             Blk 453A Ang Mo Kio Ave 10  9913.02057
    ## 3211                      Blk 49 Sims Place  7276.98634
    ## 3212                  Blk 4A Eunos Crescent  9926.81095
    ## 3213                      Blk 4A Jalan Batu  7137.35336
    ## 3214           Blk 4A Woodlands Centre Road 18123.51164
    ## 3215               Blk 502 West Coast Drive  7582.13481
    ## 3216               Blk 503 West Coast Drive  7518.34712
    ## 3217          Blk 505 Jurong West Street 52 13536.41619
    ## 3218                 Blk 50A Marine Terrace 10667.52324
    ## 3219                Blk 51 Old Airport Road  7531.01498
    ## 3220           Blk 511 Bedok North Street 3 13183.94390
    ## 3221 Bukit Panjang Hawker Centre and Market 11581.18602
    ## 3222         Our Tampines Hub Hawker Centre 15121.01144
    ## 3223        Kampung Admiralty Hawker Centre 17240.42362
    ## 3224                   Yishun Hawker Centre 15632.06755
    ## 3225   Jurong West Hawker Centre and Market 15180.11561
    ## 3226        Pasir Ris Central Hawker Centre 17384.44037
    ## 3227                   Dawson Hawker Centre  2340.22175
    ## 3228      Woodlands Street 12 Hawker Centre 17031.22902
    ## 3229              Blk 527 Ang Mo Kio Ave 10 10302.33859
    ## 3230           Blk 538 Bedok North Street 3 12532.35905
    ## 3231           Blk 58 New Upper Changi Road 13926.01987
    ## 3232              Blk 6 Tanjong Pagar Plaza  2547.52456
    ## 3233               Blk 628 Ang Mo Kio Ave 4 10763.75617
    ## 3234           Blk 630 Bedok Reservoir Road 11515.57857
    ## 3235                   Blk 69 Geylang Bahru  6655.19822
    ## 3236                     Blk 7 Empress Road  3858.87605
    ## 3237               Blk 724 Ang Mo Kio Ave 6  9965.34039
    ## 3238         Blk 726 Clementi West Street 2  6741.04638
    ## 3239              Blk 74 Toa Payoh Lorong 4  6365.39815
    ## 3240              Market Street Food Centre  3119.13128
    ## 3241                    Maxwell Food Centre  2604.24239
    ## 3242                     Newton Food Centre  3508.91298
    ## 3243 North Bridge Road Market & Food Centre  5167.49467
    ## 3244              Pasir Panjang Food Centre  3572.00938
    ## 3245           Pek Kio Market & Food Centre  4617.64155
    ## 3246              People's Park Food Centre  2285.45617
    ## 3247            Sembawang Hills Food Centre  9637.80281
    ## 3248                Serangoon Garden Market  9922.71739
    ## 3249      Taman Jurong Market & Food Centre 12435.00010
    ## 3250                    Tanglin Halt Market  3200.72624
    ## 3251                           Tekka Market  3909.85014
    ## 3252                     Tiong Bahru Market  1147.16505
    ## 3253             Zion Riverside Food Centre  1267.39594
    ## 3254              Blk 75 Toa Payoh Lorong 5  6560.74340
    ## 3255                    Blk 79 Redhill Lane   482.10862
    ## 3256             Blk 79 Telok Blangah Drive  2092.20464
    ## 3257                    Blk 80 Circuit Road  8624.41204
    ## 3258             Blk 82 Telok Blangah Drive  2026.60903
    ## 3259           Blk 84 Marine Parade Central  9562.39881
    ## 3260            Blk 85 Bedok North Street 4 13965.92898
    ## 3261                    Blk 85 Redhill Lane   459.65985
    ## 3262                    Blk 89 Circuit Road  8208.82954
    ## 3263                   Blk 90 Whampoa Drive  5540.54729
    ## 3264              Blk 93 Toa Payoh Lorong 4  6606.68883
    ## 3265                   Blks 13/14 Haig Road  8818.15008
    ## 3266          Blks 160/162 Ang Mo Kio Ave 4 10023.15273
    ## 3267                  Ci Yuan Hawker Centre 12035.30709
    ## 3268             Blk 127 Toa Payoh Lorong 1  5086.11759
    ## 3269             Blk 137 Tampines Street 11  6981.11383
    ## 3270                  Blk 159 Mei Chin Road  9839.72508
    ## 3271                Blk 16 Bedok South Road  5399.82003
    ## 3272            Blk 163 Bukit Merah Central  8799.68380
    ## 3273            Blk 17 Upper Boon Keng Road  1822.93340
    ## 3274                   Blk 20 Ghim Moh Road 11045.87362
    ## 3275         Blk 208B New Upper Changi Road  4850.00914
    ## 3276             Blk 210 Toa Payoh Lorong 8  4229.06056
    ## 3277           Blk 216 Bedok North Street 1  5190.56573
    ## 3278              Blk 22 Toa Payoh Lorong 7  3706.01217
    ## 3279              Blk 226D Ang Mo Kio Ave 1  7369.71387
    ## 3280          Blk 226H Ang Mo Kio Street 22  7346.17942
    ## 3281          Blk 254 Jurong East Street 24 16798.77576
    ## 3282                  Blk 29 Bendemeer Road  2675.38803
    ## 3283                    Blk 320 Shunfu Road  6555.25122
    ## 3284               Blk 341 Ang Mo Kio Ave 1  6457.69438
    ## 3285              Blk 347 Jurong East Ave 1 17512.43892
    ## 3286                 Blk 353 Clementi Ave 2 12948.59554
    ## 3287              Blk 36 Telok Blangah Rise  8976.70401
    ## 3288             Blk 37A Teban Gardens Road 16039.97542
    ## 3289              Blk 409 Ang Mo Kio Ave 10  5839.36470
    ## 3290                   Blk 44 Holland Drive 10579.71549
    ## 3291                 Blk 448 Clementi Ave 3 13658.42982
    ## 3292             Blk 453A Ang Mo Kio Ave 10  6272.34320
    ## 3293                      Blk 49 Sims Place   933.35503
    ## 3294                  Blk 4A Eunos Crescent  1919.16816
    ## 3295                      Blk 4A Jalan Batu  2044.63084
    ## 3296           Blk 4A Woodlands Centre Road 18600.21141
    ## 3297               Blk 502 West Coast Drive 14261.71752
    ## 3298               Blk 503 West Coast Drive 14197.39594
    ## 3299          Blk 505 Jurong West Street 52 19032.21171
    ## 3300                 Blk 50A Marine Terrace  3596.82060
    ## 3301                Blk 51 Old Airport Road  1377.10515
    ## 3302           Blk 511 Bedok North Street 3  5046.43704
    ## 3303 Bukit Panjang Hawker Centre and Market 14216.71791
    ## 3304         Our Tampines Hub Hawker Centre  6920.63209
    ## 3305        Kampung Admiralty Hawker Centre 16317.47492
    ## 3306                   Yishun Hawker Centre 12462.60834
    ## 3307   Jurong West Hawker Centre and Market 21224.89694
    ## 3308        Pasir Ris Central Hawker Centre  9253.77289
    ## 3309                   Dawson Hawker Centre  9510.98435
    ## 3310      Woodlands Street 12 Hawker Centre 17269.57160
    ## 3311              Blk 527 Ang Mo Kio Ave 10  6806.41050
    ## 3312           Blk 538 Bedok North Street 3  4385.87199
    ## 3313           Blk 58 New Upper Changi Road  6034.98713
    ## 3314              Blk 6 Tanjong Pagar Plaza  6893.84319
    ## 3315               Blk 628 Ang Mo Kio Ave 4  8435.81113
    ## 3316           Blk 630 Bedok Reservoir Road  3311.51205
    ## 3317                   Blk 69 Geylang Bahru  1895.69630
    ## 3318                     Blk 7 Empress Road  9070.16604
    ## 3319               Blk 724 Ang Mo Kio Ave 6  7266.71623
    ## 3320         Blk 726 Clementi West Street 2 13788.13650
    ## 3321              Blk 74 Toa Payoh Lorong 4  4189.47419
    ## 3322              Market Street Food Centre  5786.90514
    ## 3323                    Maxwell Food Centre  6491.41279
    ## 3324                     Newton Food Centre  5369.44180
    ## 3325 North Bridge Road Market & Food Centre  3055.05423
    ## 3326              Pasir Panjang Food Centre 11735.03628
    ## 3327           Pek Kio Market & Food Centre  4124.77397
    ## 3328              People's Park Food Centre  6325.07613
    ## 3329            Sembawang Hills Food Centre  8627.10039
    ## 3330                Serangoon Garden Market  5214.25253
    ## 3331      Taman Jurong Market & Food Centre 18482.77527
    ## 3332                    Tanglin Halt Market 10188.09071
    ## 3333                           Tekka Market  4363.50723
    ## 3334                     Tiong Bahru Market  7244.28801
    ## 3335             Zion Riverside Food Centre  6957.15887
    ## 3336              Blk 75 Toa Payoh Lorong 5  4156.80278
    ## 3337                    Blk 79 Redhill Lane  8453.43034
    ## 3338             Blk 79 Telok Blangah Drive 10266.47352
    ## 3339                    Blk 80 Circuit Road   796.37290
    ## 3340             Blk 82 Telok Blangah Drive 10205.47597
    ## 3341           Blk 84 Marine Parade Central  2957.32755
    ## 3342            Blk 85 Bedok North Street 4  5890.18738
    ## 3343                    Blk 85 Redhill Lane  8484.99056
    ## 3344                    Blk 89 Circuit Road   355.20621
    ## 3345                   Blk 90 Whampoa Drive  3557.13793
    ## 3346              Blk 93 Toa Payoh Lorong 4  4616.01498
    ## 3347                   Blks 13/14 Haig Road  1133.44845
    ## 3348          Blks 160/162 Ang Mo Kio Ave 4  7984.19480
    ## 3349                  Ci Yuan Hawker Centre  6074.04099
    ## 3350             Blk 137 Tampines Street 11 11148.27713
    ## 3351                  Blk 159 Mei Chin Road  6802.26426
    ## 3352                Blk 16 Bedok South Road 10290.38168
    ## 3353            Blk 163 Bukit Merah Central  6761.20567
    ## 3354            Blk 17 Upper Boon Keng Road  3931.79997
    ## 3355                   Blk 20 Ghim Moh Road  6965.33746
    ## 3356         Blk 208B New Upper Changi Road  9633.28103
    ## 3357             Blk 210 Toa Payoh Lorong 8  1101.85946
    ## 3358           Blk 216 Bedok North Street 1  9923.25299
    ## 3359              Blk 22 Toa Payoh Lorong 7  1405.42141
    ## 3360              Blk 226D Ang Mo Kio Ave 1  3231.44728
    ## 3361          Blk 226H Ang Mo Kio Street 22  3265.46448
    ## 3362          Blk 254 Jurong East Street 24 11917.11939
    ## 3363                  Blk 29 Bendemeer Road  2915.37567
    ## 3364                    Blk 320 Shunfu Road  1761.14922
    ## 3365               Blk 341 Ang Mo Kio Ave 1  2902.92027
    ## 3366              Blk 347 Jurong East Ave 1 12617.86501
    ## 3367                 Blk 353 Clementi Ave 2  8632.04842
    ## 3368              Blk 36 Telok Blangah Rise  7675.08896
    ## 3369             Blk 37A Teban Gardens Road 11493.02964
    ## 3370              Blk 409 Ang Mo Kio Ave 10  2974.34772
    ## 3371                   Blk 44 Holland Drive  6664.96641
    ## 3372                 Blk 448 Clementi Ave 3  9340.36596
    ## 3373             Blk 453A Ang Mo Kio Ave 10  3575.62221
    ## 3374                      Blk 49 Sims Place  4505.26827
    ## 3375                  Blk 4A Eunos Crescent  6909.95884
    ## 3376                      Blk 4A Jalan Batu  5882.25212
    ## 3377           Blk 4A Woodlands Centre Road 14074.22771
    ## 3378               Blk 502 West Coast Drive  9953.61565
    ## 3379               Blk 503 West Coast Drive  9894.13723
    ## 3380          Blk 505 Jurong West Street 52 14111.95261
    ## 3381                 Blk 50A Marine Terrace  8675.16907
    ## 3382                Blk 51 Old Airport Road  5640.13849
    ## 3383           Blk 511 Bedok North Street 3  9571.50172
    ## 3384 Bukit Panjang Hawker Centre and Market  9149.93569
    ## 3385         Our Tampines Hub Hawker Centre 10714.68915
    ## 3386        Kampung Admiralty Hawker Centre 12281.07081
    ## 3387                   Yishun Hawker Centre  9611.72026
    ## 3388   Jurong West Hawker Centre and Market 16399.70828
    ## 3389        Pasir Ris Central Hawker Centre 12512.78564
    ## 3390                   Dawson Hawker Centre  6324.09788
    ## 3391      Woodlands Street 12 Hawker Centre 12791.64257
    ## 3392              Blk 527 Ang Mo Kio Ave 10  3985.03752
    ## 3393           Blk 538 Bedok North Street 3  8942.96763
    ## 3394           Blk 58 New Upper Changi Road 10838.02731
    ## 3395              Blk 6 Tanjong Pagar Plaza  6800.15895
    ## 3396               Blk 628 Ang Mo Kio Ave 4  4768.56300
    ## 3397           Blk 630 Bedok Reservoir Road  7734.10233
    ## 3398                   Blk 69 Geylang Bahru  3360.40325
    ## 3399                     Blk 7 Empress Road  4976.55224
    ## 3400               Blk 724 Ang Mo Kio Ave 6  3776.37278
    ## 3401         Blk 726 Clementi West Street 2  9725.70924
    ## 3402              Blk 74 Toa Payoh Lorong 4   898.75830
    ## 3403              Market Street Food Centre  6024.90376
    ## 3404                    Maxwell Food Centre  6411.18890
    ## 3405                     Newton Food Centre  2953.65687
    ## 3406 North Bridge Road Market & Food Centre  4162.23116
    ## 3407              Pasir Panjang Food Centre  9083.24912
    ## 3408           Pek Kio Market & Food Centre  2501.08548
    ## 3409              People's Park Food Centre  5876.59518
    ## 3410            Sembawang Hills Food Centre  4176.79353
    ## 3411                Serangoon Garden Market  3698.48965
    ## 3412      Taman Jurong Market & Food Centre 13717.64300
    ## 3413                    Tanglin Halt Market  6670.61233
    ## 3414                           Tekka Market  3584.46774
    ## 3415                     Tiong Bahru Market  6020.79359
    ## 3416             Zion Riverside Food Centre  5278.58638
    ## 3417              Blk 75 Toa Payoh Lorong 5   941.48049
    ## 3418                    Blk 79 Redhill Lane  6280.15645
    ## 3419             Blk 79 Telok Blangah Drive  8262.76011
    ## 3420                    Blk 80 Circuit Road  4852.37483
    ## 3421             Blk 82 Telok Blangah Drive  8192.89665
    ## 3422           Blk 84 Marine Parade Central  7917.38862
    ## 3423            Blk 85 Bedok North Street 4 10483.08626
    ## 3424                    Blk 85 Redhill Lane  6335.07329
    ## 3425                    Blk 89 Circuit Road  4812.19905
    ## 3426                   Blk 90 Whampoa Drive  2036.50599
    ## 3427              Blk 93 Toa Payoh Lorong 4   533.84452
    ## 3428                   Blks 13/14 Haig Road  6204.89055
    ## 3429          Blks 160/162 Ang Mo Kio Ave 4  4071.10238
    ## 3430                  Ci Yuan Hawker Centre  5929.66609
    ## 3431                  Blk 159 Mei Chin Road 16802.84914
    ## 3432                Blk 16 Bedok South Road  2952.56171
    ## 3433            Blk 163 Bukit Merah Central 15769.57950
    ## 3434            Blk 17 Upper Boon Keng Road  8801.52222
    ## 3435                   Blk 20 Ghim Moh Road 17821.60342
    ## 3436         Blk 208B New Upper Changi Road  2753.51893
    ## 3437             Blk 210 Toa Payoh Lorong 8 10059.13370
    ## 3438           Blk 216 Bedok North Street 1  2414.54506
    ## 3439              Blk 22 Toa Payoh Lorong 7  9815.36610
    ## 3440              Blk 226D Ang Mo Kio Ave 1 11959.89383
    ## 3441          Blk 226H Ang Mo Kio Street 22 11890.85036
    ## 3442          Blk 254 Jurong East Street 24 23020.68298
    ## 3443                  Blk 29 Bendemeer Road  9539.98518
    ## 3444                    Blk 320 Shunfu Road 11994.55895
    ## 3445               Blk 341 Ang Mo Kio Ave 1 10917.55223
    ## 3446              Blk 347 Jurong East Ave 1 23709.57755
    ## 3447                 Blk 353 Clementi Ave 2 19647.47416
    ## 3448              Blk 36 Telok Blangah Rise 15856.58599
    ## 3449             Blk 37A Teban Gardens Road 22617.51056
    ## 3450              Blk 409 Ang Mo Kio Ave 10 10112.01223
    ## 3451                   Blk 44 Holland Drive 17401.30230
    ## 3452                 Blk 448 Clementi Ave 3 20361.99410
    ## 3453             Blk 453A Ang Mo Kio Ave 10 10136.67491
    ## 3454                      Blk 49 Sims Place  7914.12199
    ## 3455                  Blk 4A Eunos Crescent  5292.41044
    ## 3456                      Blk 4A Jalan Batu  8273.45500
    ## 3457           Blk 4A Woodlands Centre Road 22092.82123
    ## 3458               Blk 502 West Coast Drive 20974.29870
    ## 3459               Blk 503 West Coast Drive 20911.92497
    ## 3460          Blk 505 Jurong West Street 52 25174.89133
    ## 3461                 Blk 50A Marine Terrace  5454.18804
    ## 3462                Blk 51 Old Airport Road  7739.54951
    ## 3463           Blk 511 Bedok North Street 3  2076.83183
    ## 3464 Bukit Panjang Hawker Centre and Market 19479.86732
    ## 3465         Our Tampines Hub Hawker Centre  1060.11172
    ## 3466        Kampung Admiralty Hawker Centre 19111.91795
    ## 3467                   Yishun Hawker Centre 14162.75523
    ## 3468   Jurong West Hawker Centre and Market 27517.19958
    ## 3469        Pasir Ris Central Hawker Centre  3168.37185
    ## 3470                   Dawson Hawker Centre 16455.77487
    ## 3471      Woodlands Street 12 Hawker Centre 20760.27224
    ## 3472              Blk 527 Ang Mo Kio Ave 10 10470.68431
    ## 3473           Blk 538 Bedok North Street 3  2682.17322
    ## 3474           Blk 58 New Upper Changi Road  2403.64907
    ## 3475              Blk 6 Tanjong Pagar Plaza 13628.88826
    ## 3476               Blk 628 Ang Mo Kio Ave 4 12211.37006
    ## 3477           Blk 630 Bedok Reservoir Road  3671.41308
    ## 3478                   Blk 69 Geylang Bahru  8724.15032
    ## 3479                     Blk 7 Empress Road 15805.73432
    ## 3480               Blk 724 Ang Mo Kio Ave 6 11306.02683
    ## 3481         Blk 726 Clementi West Street 2 20598.98232
    ## 3482              Blk 74 Toa Payoh Lorong 4 10380.91352
    ## 3483              Market Street Food Centre 12549.34510
    ## 3484                    Maxwell Food Centre 13260.93348
    ## 3485                     Newton Food Centre 12273.33936
    ## 3486 North Bridge Road Market & Food Centre 10008.35218
    ## 3487              Pasir Panjang Food Centre 18714.41359
    ## 3488           Pek Kio Market & Food Centre 10999.77719
    ## 3489              People's Park Food Centre 13188.93848
    ## 3490            Sembawang Hills Food Centre 13203.77153
    ## 3491                Serangoon Garden Market  8881.73545
    ## 3492      Taman Jurong Market & Food Centre 24859.20659
    ## 3493                    Tanglin Halt Market 17094.67326
    ## 3494                           Tekka Market 11343.21933
    ## 3495                     Tiong Bahru Market 14176.01216
    ## 3496             Zion Riverside Food Centre 13932.07708
    ## 3497              Blk 75 Toa Payoh Lorong 5 10258.40614
    ## 3498                    Blk 79 Redhill Lane 15432.02807
    ## 3499             Blk 79 Telok Blangah Drive 17213.94167
    ## 3500                    Blk 80 Circuit Road  6694.24314
    ## 3501             Blk 82 Telok Blangah Drive 17154.75693
    ## 3502           Blk 84 Marine Parade Central  6413.76755
    ## 3503            Blk 85 Bedok North Street 4  1645.87560
    ## 3504                    Blk 85 Redhill Lane 15462.70581
    ## 3505                    Blk 89 Circuit Road  7027.99933
    ## 3506                   Blk 90 Whampoa Drive 10270.29880
    ## 3507              Blk 93 Toa Payoh Lorong 4 10614.43774
    ## 3508                   Blks 13/14 Haig Road  6413.08942
    ## 3509          Blks 160/162 Ang Mo Kio Ave 4 12156.93885
    ## 3510                  Ci Yuan Hawker Centre  7610.33772
    ## 3511                Blk 16 Bedok South Road 15066.93237
    ## 3512            Blk 163 Bukit Merah Central  1889.98175
    ## 3513            Blk 17 Upper Boon Keng Road  8018.14325
    ## 3514                   Blk 20 Ghim Moh Road  2554.06713
    ## 3515         Blk 208B New Upper Changi Road 14620.27307
    ## 3516             Blk 210 Toa Payoh Lorong 8  7737.94691
    ## 3517           Blk 216 Bedok North Street 1 14977.67989
    ## 3518              Blk 22 Toa Payoh Lorong 7  7606.33878
    ## 3519              Blk 226D Ang Mo Kio Ave 1  9082.37349
    ## 3520          Blk 226H Ang Mo Kio Street 22  9162.08373
    ## 3521          Blk 254 Jurong East Street 24  9129.39707
    ## 3522                  Blk 29 Bendemeer Road  7281.21908
    ## 3523                    Blk 320 Shunfu Road  7527.12394
    ## 3524               Blk 341 Ang Mo Kio Ave 1  9320.96927
    ## 3525              Blk 347 Jurong East Ave 1  9806.38397
    ## 3526                 Blk 353 Clementi Ave 2  4265.67452
    ## 3527              Blk 36 Telok Blangah Rise  3125.97562
    ## 3528             Blk 37A Teban Gardens Road  7343.61198
    ## 3529              Blk 409 Ang Mo Kio Ave 10  9654.38027
    ## 3530                   Blk 44 Holland Drive  1994.68427
    ## 3531                 Blk 448 Clementi Ave 3  4812.67617
    ## 3532             Blk 453A Ang Mo Kio Ave 10 10206.08620
    ## 3533                      Blk 49 Sims Place  8914.14474
    ## 3534                  Blk 4A Eunos Crescent 11671.20697
    ## 3535                      Blk 4A Jalan Batu  9075.64122
    ## 3536           Blk 4A Woodlands Centre Road 16724.89603
    ## 3537               Blk 502 West Coast Drive  5286.59702
    ## 3538               Blk 503 West Coast Drive  5222.91376
    ## 3539          Blk 505 Jurong West Street 52 11280.31971
    ## 3540                 Blk 50A Marine Terrace 12636.38892
    ## 3541                Blk 51 Old Airport Road  9377.44118
    ## 3542           Blk 511 Bedok North Street 3 14884.29711
    ## 3543 Bukit Panjang Hawker Centre and Market  9923.66751
    ## 3544         Our Tampines Hub Hawker Centre 16637.30068
    ## 3545        Kampung Admiralty Hawker Centre 16222.82703
    ## 3546                   Yishun Hawker Centre 15297.71060
    ## 3547   Jurong West Hawker Centre and Market 12886.09982
    ## 3548        Pasir Ris Central Hawker Centre 18769.03722
    ## 3549                   Dawson Hawker Centre   511.29819
    ## 3550      Woodlands Street 12 Hawker Centre 15725.85347
    ## 3551              Blk 527 Ang Mo Kio Ave 10 10501.99191
    ## 3552           Blk 538 Bedok North Street 3 14224.35542
    ## 3553           Blk 58 New Upper Changi Road 15759.65437
    ## 3554              Blk 6 Tanjong Pagar Plaza  4843.23056
    ## 3555               Blk 628 Ang Mo Kio Ave 4 10578.58899
    ## 3556           Blk 630 Bedok Reservoir Road 13134.03826
    ## 3557                   Blk 69 Geylang Bahru  8093.65246
    ## 3558                     Blk 7 Empress Road  2562.38276
    ## 3559               Blk 724 Ang Mo Kio Ave 6  9989.65521
    ## 3560         Blk 726 Clementi West Street 2  4455.53387
    ## 3561              Blk 74 Toa Payoh Lorong 4  7121.84170
    ## 3562              Market Street Food Centre  5345.93687
    ## 3563                    Maxwell Food Centre  4886.25452
    ## 3564                     Newton Food Centre  4571.67829
    ## 3565 North Bridge Road Market & Food Centre  6927.27019
    ## 3566              Pasir Panjang Food Centre  2304.32000
    ## 3567           Pek Kio Market & Food Centre  5844.67613
    ## 3568              People's Park Food Centre  4508.61694
    ## 3569            Sembawang Hills Food Centre  9220.94511
    ## 3570                Serangoon Garden Market 10499.67576
    ## 3571      Taman Jurong Market & Food Centre 10146.08393
    ## 3572                    Tanglin Halt Market  1021.06869
    ## 3573                           Tekka Market  5491.04956
    ## 3574                     Tiong Bahru Market  3398.56317
    ## 3575             Zion Riverside Food Centre  3149.47258
    ## 3576              Blk 75 Toa Payoh Lorong 5  7308.26905
    ## 3577                    Blk 79 Redhill Lane  1824.51212
    ## 3578             Blk 79 Telok Blangah Drive  2258.89896
    ## 3579                    Blk 80 Circuit Road 10124.69789
    ## 3580             Blk 82 Telok Blangah Drive  2208.96510
    ## 3581           Blk 84 Marine Parade Central 11554.76263
    ## 3582            Blk 85 Bedok North Street 4 15712.00265
    ## 3583                    Blk 85 Redhill Lane  1837.71321
    ## 3584                    Blk 89 Circuit Road  9774.98021
    ## 3585                   Blk 90 Whampoa Drive  6677.02322
    ## 3586              Blk 93 Toa Payoh Lorong 4  7206.20931
    ## 3587                   Blks 13/14 Haig Road 10597.81090
    ## 3588          Blks 160/162 Ang Mo Kio Ave 4  9851.45582
    ## 3589                  Ci Yuan Hawker Centre 12728.43925
    ## 3590            Blk 163 Bukit Merah Central 13808.39163
    ## 3591            Blk 17 Upper Boon Keng Road  7139.72446
    ## 3592                   Blk 20 Ghim Moh Road 16428.02866
    ## 3593         Blk 208B New Upper Changi Road   780.00042
    ## 3594             Blk 210 Toa Payoh Lorong 8  9289.71328
    ## 3595           Blk 216 Bedok North Street 1   764.68079
    ## 3596              Blk 22 Toa Payoh Lorong 7  8886.10214
    ## 3597              Blk 226D Ang Mo Kio Ave 1 11874.89774
    ## 3598          Blk 226H Ang Mo Kio Street 22 11823.72006
    ## 3599          Blk 254 Jurong East Street 24 22153.46970
    ## 3600                  Blk 29 Bendemeer Road  8071.86418
    ## 3601                    Blk 320 Shunfu Road 11501.35522
    ## 3602               Blk 341 Ang Mo Kio Ave 1 10842.13052
    ## 3603              Blk 347 Jurong East Ave 1 22863.08278
    ## 3604                 Blk 353 Clementi Ave 2 18342.52727
    ## 3605              Blk 36 Telok Blangah Rise 13701.14780
    ## 3606             Blk 37A Teban Gardens Road 21439.79484
    ## 3607              Blk 409 Ang Mo Kio Ave 10 10069.41228
    ## 3608                   Blk 44 Holland Drive 15947.79714
    ## 3609                 Blk 448 Clementi Ave 3 19050.87418
    ## 3610             Blk 453A Ang Mo Kio Ave 10 10269.86268
    ## 3611                      Blk 49 Sims Place  6257.60789
    ## 3612                  Blk 4A Eunos Crescent  3481.04615
    ## 3613                      Blk 4A Jalan Batu  6082.42723
    ## 3614           Blk 4A Woodlands Centre Road 22714.29490
    ## 3615               Blk 502 West Coast Drive 19651.97226
    ## 3616               Blk 503 West Coast Drive 19587.39132
    ## 3617          Blk 505 Jurong West Street 52 24373.20748
    ## 3618                 Blk 50A Marine Terrace  2741.22227
    ## 3619                Blk 51 Old Airport Road  5696.55168
    ## 3620           Blk 511 Bedok North Street 3  1507.35417
    ## 3621 Bukit Panjang Hawker Centre and Market 19210.80675
    ## 3622         Our Tampines Hub Hawker Centre  3712.60919
    ## 3623        Kampung Admiralty Hawker Centre 19986.05418
    ## 3624                   Yishun Hawker Centre 15346.73798
    ## 3625   Jurong West Hawker Centre and Market 26600.69702
    ## 3626        Pasir Ris Central Hawker Centre  6112.66091
    ## 3627                   Dawson Hawker Centre 14781.19817
    ## 3628      Woodlands Street 12 Hawker Centre 21366.75042
    ## 3629              Blk 527 Ang Mo Kio Ave 10 10710.33990
    ## 3630           Blk 538 Bedok North Street 3  1719.98892
    ## 3631           Blk 58 New Upper Changi Road   741.00789
    ## 3632              Blk 6 Tanjong Pagar Plaza 11370.41942
    ## 3633               Blk 628 Ang Mo Kio Ave 4 12496.10107
    ## 3634           Blk 630 Bedok Reservoir Road  2767.78233
    ## 3635                   Blk 69 Geylang Bahru  7294.06143
    ## 3636                     Blk 7 Empress Road 14464.74720
    ## 3637               Blk 724 Ang Mo Kio Ave 6 11434.99716
    ## 3638         Blk 726 Clementi West Street 2 19151.24015
    ## 3639              Blk 74 Toa Payoh Lorong 4  9425.07959
    ## 3640              Market Street Food Centre 10345.01237
    ## 3641                    Maxwell Food Centre 11041.36282
    ## 3642                     Newton Food Centre 10724.08661
    ## 3643 North Bridge Road Market & Food Centre  8139.66378
    ## 3644              Pasir Panjang Food Centre 16783.53309
    ## 3645           Pek Kio Market & Food Centre  9507.03507
    ## 3646              People's Park Food Centre 11067.71786
    ## 3647            Sembawang Hills Food Centre 13169.81415
    ## 3648                Serangoon Garden Market  8990.09018
    ## 3649      Taman Jurong Market & Food Centre 23868.69453
    ## 3650                    Tanglin Halt Market 15503.24556
    ## 3651                           Tekka Market  9592.67417
    ## 3652                     Tiong Bahru Market 12135.20496
    ## 3653             Zion Riverside Food Centre 12024.80408
    ## 3654              Blk 75 Toa Payoh Lorong 5  9350.41308
    ## 3655                    Blk 79 Redhill Lane 13528.20625
    ## 3656             Blk 79 Telok Blangah Drive 15161.48080
    ## 3657                    Blk 80 Circuit Road  5447.70531
    ## 3658             Blk 82 Telok Blangah Drive 15107.69131
    ## 3659           Blk 84 Marine Parade Central  3826.32704
    ## 3660            Blk 85 Bedok North Street 4  1313.41432
    ## 3661                    Blk 85 Redhill Lane 13550.73043
    ## 3662                    Blk 89 Circuit Road  5583.68153
    ## 3663                   Blk 90 Whampoa Drive  8951.78803
    ## 3664              Blk 93 Toa Payoh Lorong 4  9776.73808
    ## 3665                   Blks 13/14 Haig Road  4483.86546
    ## 3666          Blks 160/162 Ang Mo Kio Ave 4 12267.28227
    ## 3667                  Ci Yuan Hawker Centre  8428.34269
    ## 3668            Blk 17 Upper Boon Keng Road  6997.64971
    ## 3669                   Blk 20 Ghim Moh Road  4400.10263
    ## 3670         Blk 208B New Upper Changi Road 13428.65829
    ## 3671             Blk 210 Toa Payoh Lorong 8  7513.48596
    ## 3672           Blk 216 Bedok North Street 1 13795.59167
    ## 3673              Blk 22 Toa Payoh Lorong 7  7234.47994
    ## 3674              Blk 226D Ang Mo Kio Ave 1  9515.72172
    ## 3675          Blk 226H Ang Mo Kio Street 22  9584.08055
    ## 3676          Blk 254 Jurong East Street 24 11016.44058
    ## 3677                  Blk 29 Bendemeer Road  6455.31995
    ## 3678                    Blk 320 Shunfu Road  7876.24542
    ## 3679               Blk 341 Ang Mo Kio Ave 1  9548.04023
    ## 3680              Blk 347 Jurong East Ave 1 11695.38350
    ## 3681                 Blk 353 Clementi Ave 2  6155.55069
    ## 3682              Blk 36 Telok Blangah Rise  1354.73732
    ## 3683             Blk 37A Teban Gardens Road  9213.49935
    ## 3684              Blk 409 Ang Mo Kio Ave 10  9731.21844
    ## 3685                   Blk 44 Holland Drive  3815.64213
    ## 3686                 Blk 448 Clementi Ave 3  6693.81204
    ## 3687             Blk 453A Ang Mo Kio Ave 10 10324.15350
    ## 3688                      Blk 49 Sims Place  7867.47646
    ## 3689                  Blk 4A Eunos Crescent 10523.32390
    ## 3690                      Blk 4A Jalan Batu  7733.13152
    ## 3691           Blk 4A Woodlands Centre Road 18143.68687
    ## 3692               Blk 502 West Coast Drive  7151.05913
    ## 3693               Blk 503 West Coast Drive  7088.29238
    ## 3694          Blk 505 Jurong West Street 52 13170.26703
    ## 3695                 Blk 50A Marine Terrace 11260.13120
    ## 3696                Blk 51 Old Airport Road  8128.42203
    ## 3697           Blk 511 Bedok North Street 3 13779.95439
    ## 3698 Bukit Panjang Hawker Centre and Market 11504.51478
    ## 3699         Our Tampines Hub Hawker Centre 15708.98387
    ## 3700        Kampung Admiralty Hawker Centre 17371.17773
    ## 3701                   Yishun Hawker Centre 15929.34784
    ## 3702   Jurong West Hawker Centre and Market 14752.95822
    ## 3703        Pasir Ris Central Hawker Centre 17963.66989
    ## 3704                   Dawson Hawker Centre  2047.68936
    ## 3705      Woodlands Street 12 Hawker Centre 17078.80575
    ## 3706              Blk 527 Ang Mo Kio Ave 10 10696.96153
    ## 3707           Blk 538 Bedok North Street 3 13128.00766
    ## 3708           Blk 58 New Upper Changi Road 14523.17270
    ## 3709              Blk 6 Tanjong Pagar Plaza  3016.32657
    ## 3710               Blk 628 Ang Mo Kio Ave 4 11080.71873
    ## 3711           Blk 630 Bedok Reservoir Road 12107.95075
    ## 3712                   Blk 69 Geylang Bahru  7228.36412
    ## 3713                     Blk 7 Empress Road  3814.97960
    ## 3714               Blk 724 Ang Mo Kio Ave 6 10323.36571
    ## 3715         Blk 726 Clementi West Street 2  6274.54565
    ## 3716              Blk 74 Toa Payoh Lorong 4  6841.07140
    ## 3717              Market Street Food Centre  3676.72060
    ## 3718                    Maxwell Food Centre  3123.05504
    ## 3719                     Newton Food Centre  4004.07138
    ## 3720 North Bridge Road Market & Food Centre  5761.87471
    ## 3721              Pasir Panjang Food Centre  2975.18733
    ## 3722           Pek Kio Market & Food Centre  5154.36755
    ## 3723              People's Park Food Centre  2851.16701
    ## 3724            Sembawang Hills Food Centre  9897.68361
    ## 3725                Serangoon Garden Market 10385.38659
    ## 3726      Taman Jurong Market & Food Centre 12023.65636
    ## 3727                    Tanglin Halt Market  2866.94205
    ## 3728                           Tekka Market  4486.54008
    ## 3729                     Tiong Bahru Market  1715.99312
    ## 3730             Zion Riverside Food Centre  1846.92697
    ## 3731              Blk 75 Toa Payoh Lorong 5  7036.29365
    ## 3732                    Blk 79 Redhill Lane   486.60627
    ## 3733             Blk 79 Telok Blangah Drive  1545.85948
    ## 3734                    Blk 80 Circuit Road  9206.85711
    ## 3735             Blk 82 Telok Blangah Drive  1477.61857
    ## 3736           Blk 84 Marine Parade Central 10153.76058
    ## 3737            Blk 85 Bedok North Street 4 14562.98872
    ## 3738                    Blk 85 Redhill Lane   429.29611
    ## 3739                    Blk 89 Circuit Road  8795.74279
    ## 3740                   Blk 90 Whampoa Drive  6071.12293
    ## 3741              Blk 93 Toa Payoh Lorong 4  7056.41046
    ## 3742                   Blks 13/14 Haig Road  9415.13810
    ## 3743          Blks 160/162 Ang Mo Kio Ave 4 10339.92055
    ## 3744                  Ci Yuan Hawker Centre 12522.88830
    ## 3745                   Blk 20 Ghim Moh Road  9291.17123
    ## 3746         Blk 208B New Upper Changi Road  6634.32567
    ## 3747             Blk 210 Toa Payoh Lorong 8  3385.18703
    ## 3748           Blk 216 Bedok North Street 1  6983.56969
    ## 3749              Blk 22 Toa Payoh Lorong 7  2761.35990
    ## 3750              Blk 226D Ang Mo Kio Ave 1  6763.80098
    ## 3751          Blk 226H Ang Mo Kio Street 22  6763.04803
    ## 3752          Blk 254 Jurong East Street 24 15222.68018
    ## 3753                  Blk 29 Bendemeer Road  1063.17761
    ## 3754                    Blk 320 Shunfu Road  5615.80307
    ## 3755               Blk 341 Ang Mo Kio Ave 1  6017.56491
    ## 3756              Blk 347 Jurong East Ave 1 15940.77451
    ## 3757                 Blk 353 Clementi Ave 2 11216.47729
    ## 3758              Blk 36 Telok Blangah Rise  7246.28182
    ## 3759             Blk 37A Teban Gardens Road 14340.69528
    ## 3760              Blk 409 Ang Mo Kio Ave 10  5580.13636
    ## 3761                   Blk 44 Holland Drive  8808.07540
    ## 3762                 Blk 448 Clementi Ave 3 11921.88306
    ## 3763             Blk 453A Ang Mo Kio Ave 10  6124.41608
    ## 3764                      Blk 49 Sims Place   896.97230
    ## 3765                  Blk 4A Eunos Crescent  3679.61864
    ## 3766                      Blk 4A Jalan Batu  1955.50112
    ## 3767           Blk 4A Woodlands Centre Road 17922.31109
    ## 3768               Blk 502 West Coast Drive 12519.88769
    ## 3769               Blk 503 West Coast Drive 12455.05365
    ## 3770          Blk 505 Jurong West Street 52 17470.94736
    ## 3771                 Blk 50A Marine Terrace  5017.96891
    ## 3772                Blk 51 Old Airport Road  1750.27084
    ## 3773           Blk 511 Bedok North Street 3  6866.25636
    ## 3774 Bukit Panjang Hawker Centre and Market 13022.29752
    ## 3775         Our Tampines Hub Hawker Centre  8711.34661
    ## 3776        Kampung Admiralty Hawker Centre 15907.59249
    ## 3777                   Yishun Hawker Centre 12524.38311
    ## 3778   Jurong West Hawker Centre and Market 19604.24293
    ## 3779        Pasir Ris Central Hawker Centre 10991.89912
    ## 3780                   Dawson Hawker Centre  7694.42943
    ## 3781      Woodlands Street 12 Hawker Centre 16618.50466
    ## 3782              Blk 527 Ang Mo Kio Ave 10  6663.57042
    ## 3783           Blk 538 Bedok North Street 3  6206.21994
    ## 3784           Blk 58 New Upper Changi Road  7802.04145
    ## 3785              Blk 6 Tanjong Pagar Plaza  5298.03009
    ## 3786               Blk 628 Ang Mo Kio Ave 4  8069.04324
    ## 3787           Blk 630 Bedok Reservoir Road  5130.38716
    ## 3788                   Blk 69 Geylang Bahru   733.83781
    ## 3789                     Blk 7 Empress Road  7344.60402
    ## 3790               Blk 724 Ang Mo Kio Ave 6  6911.21190
    ## 3791         Blk 726 Clementi West Street 2 12012.24237
    ## 3792              Blk 74 Toa Payoh Lorong 4  3070.35912
    ## 3793              Market Street Food Centre  4200.77140
    ## 3794                    Maxwell Food Centre  4878.19019
    ## 3795                     Newton Food Centre  3584.51192
    ## 3796 North Bridge Road Market & Food Centre  1337.51893
    ## 3797              Pasir Panjang Food Centre  9920.31460
    ## 3798           Pek Kio Market & Food Centre  2384.36564
    ## 3799              People's Park Food Centre  4634.38409
    ## 3800            Sembawang Hills Food Centre  7920.08261
    ## 3801                Serangoon Garden Market  5349.53872
    ## 3802      Taman Jurong Market & Food Centre 16844.12683
    ## 3803                    Tanglin Halt Market  8384.06050
    ## 3804                           Tekka Market  2541.69784
    ## 3805                     Tiong Bahru Market  5482.86640
    ## 3806             Zion Riverside Food Centre  5151.59124
    ## 3807              Blk 75 Toa Payoh Lorong 5  3117.49432
    ## 3808                    Blk 79 Redhill Lane  6641.05446
    ## 3809             Blk 79 Telok Blangah Drive  8481.84352
    ## 3810                    Blk 80 Circuit Road  2233.55503
    ## 3811             Blk 82 Telok Blangah Drive  8419.54420
    ## 3812           Blk 84 Marine Parade Central  4112.72474
    ## 3813            Blk 85 Bedok North Street 4  7700.74795
    ## 3814                    Blk 85 Redhill Lane  6673.93478
    ## 3815                    Blk 89 Circuit Road  1798.57331
    ## 3816                   Blk 90 Whampoa Drive  2032.44105
    ## 3817              Blk 93 Toa Payoh Lorong 4  3574.15022
    ## 3818                   Blks 13/14 Haig Road  2669.79992
    ## 3819          Blks 160/162 Ang Mo Kio Ave 4  7499.04565
    ## 3820                  Ci Yuan Hawker Centre  6797.38219
    ## 3821         Blk 208B New Upper Changi Road 15895.73216
    ## 3822             Blk 210 Toa Payoh Lorong 8  8043.11358
    ## 3823           Blk 216 Bedok North Street 1 16233.56832
    ## 3824              Blk 22 Toa Payoh Lorong 7  8112.72651
    ## 3825              Blk 226D Ang Mo Kio Ave 1  8382.90891
    ## 3826          Blk 226H Ang Mo Kio Street 22  8473.34663
    ## 3827          Blk 254 Jurong East Street 24  6662.90767
    ## 3828                  Blk 29 Bendemeer Road  8372.97620
    ## 3829                    Blk 320 Shunfu Road  7075.37260
    ## 3830               Blk 341 Ang Mo Kio Ave 1  8894.30000
    ## 3831              Blk 347 Jurong East Ave 1  7358.47976
    ## 3832                 Blk 353 Clementi Ave 2  1971.34848
    ## 3833              Blk 36 Telok Blangah Rise  5677.83660
    ## 3834             Blk 37A Teban Gardens Road  5162.06061
    ## 3835              Blk 409 Ang Mo Kio Ave 10  9412.84814
    ## 3836                   Blk 44 Holland Drive   597.44982
    ## 3837                 Blk 448 Clementi Ave 3  2651.71736
    ## 3838             Blk 453A Ang Mo Kio Ave 10  9876.19834
    ## 3839                      Blk 49 Sims Place 10170.46661
    ## 3840                  Blk 4A Eunos Crescent 12954.23977
    ## 3841                      Blk 4A Jalan Batu 10693.77202
    ## 3842           Blk 4A Woodlands Centre Road 14498.75500
    ## 3843               Blk 502 West Coast Drive  3236.12059
    ## 3844               Blk 503 West Coast Drive  3170.73710
    ## 3845          Blk 505 Jurong West Street 52  8864.19601
    ## 3846                 Blk 50A Marine Terrace 14204.78278
    ## 3847                Blk 51 Old Airport Road 10866.48863
    ## 3848           Blk 511 Bedok North Street 3 16034.41188
    ## 3849 Bukit Panjang Hawker Centre and Market  7570.24150
    ## 3850         Our Tampines Hub Hawker Centre 17513.48288
    ## 3851        Kampung Admiralty Hawker Centre 14324.48850
    ## 3852                   Yishun Hawker Centre 14089.67585
    ## 3853   Jurong West Hawker Centre and Market 10647.60005
    ## 3854        Pasir Ris Central Hawker Centre 19442.04904
    ## 3855                   Dawson Hawker Centre  2365.90933
    ## 3856      Woodlands Street 12 Hawker Centre 13582.70005
    ## 3857              Blk 527 Ang Mo Kio Ave 10 10050.90085
    ## 3858           Blk 538 Bedok North Street 3 15376.47332
    ## 3859           Blk 58 New Upper Changi Road 17078.88207
    ## 3860              Blk 6 Tanjong Pagar Plaza  7199.54873
    ## 3861               Blk 628 Ang Mo Kio Ave 4  9697.12750
    ## 3862           Blk 630 Bedok Reservoir Road 14215.70839
    ## 3863                   Blk 69 Geylang Bahru  9173.91505
    ## 3864                     Blk 7 Empress Road  2022.19087
    ## 3865               Blk 724 Ang Mo Kio Ave 6  9374.23099
    ## 3866         Blk 726 Clementi West Street 2  2783.95941
    ## 3867              Blk 74 Toa Payoh Lorong 4  7559.53860
    ## 3868              Market Street Food Centre  7503.32813
    ## 3869                    Maxwell Food Centre  7164.96679
    ## 3870                     Newton Food Centre  5713.62855
    ## 3871 North Bridge Road Market & Food Centre  8440.13418
    ## 3872              Pasir Panjang Food Centre  3895.96049
    ## 3873           Pek Kio Market & Food Centre  6922.97226
    ## 3874              People's Park Food Centre  6697.20314
    ## 3875            Sembawang Hills Food Centre  8164.42130
    ## 3876                Serangoon Garden Market 10469.04466
    ## 3877      Taman Jurong Market & Food Centre  7869.26874
    ## 3878                    Tanglin Halt Market  1538.52768
    ## 3879                           Tekka Market  6953.64767
    ## 3880                     Tiong Bahru Market  5684.59380
    ## 3881             Zion Riverside Food Centre  5206.89193
    ## 3882              Blk 75 Toa Payoh Lorong 5  7715.69332
    ## 3883                    Blk 79 Redhill Lane  4221.51828
    ## 3884             Blk 79 Telok Blangah Drive  4687.34268
    ## 3885                    Blk 80 Circuit Road 11163.01285
    ## 3886             Blk 82 Telok Blangah Drive  4650.40760
    ## 3887           Blk 84 Marine Parade Central 13179.56099
    ## 3888            Blk 85 Bedok North Street 4 16909.43456
    ## 3889                    Blk 85 Redhill Lane  4251.52084
    ## 3890                    Blk 89 Circuit Road 10906.69827
    ## 3891                   Blk 90 Whampoa Drive  7562.37686
    ## 3892              Blk 93 Toa Payoh Lorong 4  7466.36929
    ## 3893                   Blks 13/14 Haig Road 11959.03097
    ## 3894          Blks 160/162 Ang Mo Kio Ave 4  9026.84094
    ## 3895                  Ci Yuan Hawker Centre 12725.71199
    ## 3896             Blk 210 Toa Payoh Lorong 8  8613.75117
    ## 3897           Blk 216 Bedok North Street 1   374.12522
    ## 3898              Blk 22 Toa Payoh Lorong 7  8233.07788
    ## 3899              Blk 226D Ang Mo Kio Ave 1 11130.36798
    ## 3900          Blk 226H Ang Mo Kio Street 22 11077.61830
    ## 3901          Blk 254 Jurong East Street 24 21524.97837
    ## 3902                  Blk 29 Bendemeer Road  7523.49115
    ## 3903                    Blk 320 Shunfu Road 10797.48018
    ## 3904               Blk 341 Ang Mo Kio Ave 1 10094.25631
    ## 3905              Blk 347 Jurong East Ave 1 22231.85427
    ## 3906                 Blk 353 Clementi Ave 2 17793.72933
    ## 3907              Blk 36 Telok Blangah Rise 13391.07974
    ## 3908             Blk 37A Teban Gardens Road 20868.43057
    ## 3909              Blk 409 Ang Mo Kio Ave 10  9316.88682
    ## 3910                   Blk 44 Holland Drive 15428.67580
    ## 3911                 Blk 448 Clementi Ave 3 18504.58285
    ## 3912             Blk 453A Ang Mo Kio Ave 10  9506.44945
    ## 3913                      Blk 49 Sims Place  5741.02160
    ## 3914                  Blk 4A Eunos Crescent  2955.29708
    ## 3915                      Blk 4A Jalan Batu  5751.55202
    ## 3916           Blk 4A Woodlands Centre Road 21939.37864
    ## 3917               Blk 502 West Coast Drive 19109.07611
    ## 3918               Blk 503 West Coast Drive 19044.89214
    ## 3919          Blk 505 Jurong West Street 52 23736.01959
    ## 3920                 Blk 50A Marine Terrace  2701.28867
    ## 3921                Blk 51 Old Airport Road  5301.07762
    ## 3922           Blk 511 Bedok North Street 3   879.36481
    ## 3923 Bukit Panjang Hawker Centre and Market 18495.91183
    ## 3924         Our Tampines Hub Hawker Centre  3326.30877
    ## 3925        Kampung Admiralty Hawker Centre 19206.16277
    ## 3926                   Yishun Hawker Centre 14572.09562
    ## 3927   Jurong West Hawker Centre and Market 25984.83740
    ## 3928        Pasir Ris Central Hawker Centre  5821.69970
    ## 3929                   Dawson Hawker Centre 14316.54120
    ## 3930      Woodlands Street 12 Hawker Centre 20591.94168
    ## 3931              Blk 527 Ang Mo Kio Ave 10  9942.24606
    ## 3932           Blk 538 Bedok North Street 3   943.34543
    ## 3933           Blk 58 New Upper Changi Road  1205.85463
    ## 3934              Blk 6 Tanjong Pagar Plaza 11096.27752
    ## 3935               Blk 628 Ang Mo Kio Ave 4 11728.54504
    ## 3936           Blk 630 Bedok Reservoir Road  2011.51042
    ## 3937                   Blk 69 Geylang Bahru  6729.61751
    ## 3938                     Blk 7 Empress Road 13915.81614
    ## 3939               Blk 724 Ang Mo Kio Ave 6 10674.24438
    ## 3940         Blk 726 Clementi West Street 2 18636.59030
    ## 3941              Blk 74 Toa Payoh Lorong 4  8779.81471
    ## 3942              Market Street Food Centre 10044.90558
    ## 3943                    Maxwell Food Centre 10749.88984
    ## 3944                     Newton Food Centre 10212.26769
    ## 3945 North Bridge Road Market & Food Centre  7705.49654
    ## 3946              Pasir Panjang Food Centre 16400.00522
    ## 3947           Pek Kio Market & Food Centre  8974.55200
    ## 3948              People's Park Food Centre 10737.21424
    ## 3949            Sembawang Hills Food Centre 12423.82264
    ## 3950                Serangoon Garden Market  8224.85527
    ## 3951      Taman Jurong Market & Food Centre 23263.73265
    ## 3952                    Tanglin Halt Market 15017.85883
    ## 3953                           Tekka Market  9130.86041
    ## 3954                     Tiong Bahru Market 11778.41884
    ## 3955             Zion Riverside Food Centre 11622.09929
    ## 3956              Blk 75 Toa Payoh Lorong 5  8696.62774
    ## 3957                    Blk 79 Redhill Lane 13128.87501
    ## 3958             Blk 79 Telok Blangah Drive 14816.29341
    ## 3959                    Blk 80 Circuit Road  4818.76091
    ## 3960             Blk 82 Telok Blangah Drive 14760.48020
    ## 3961           Blk 84 Marine Parade Central  3690.24623
    ## 3962            Blk 85 Bedok North Street 4  1190.26807
    ## 3963                    Blk 85 Redhill Lane 13154.09809
    ## 3964                    Blk 89 Circuit Road  5001.58171
    ## 3965                   Blk 90 Whampoa Drive  8374.80399
    ## 3966              Blk 93 Toa Payoh Lorong 4  9113.87359
    ## 3967                   Blks 13/14 Haig Road  4026.41742
    ## 3968          Blks 160/162 Ang Mo Kio Ave 4 11509.22489
    ## 3969                  Ci Yuan Hawker Centre  7650.20558
    ## 3970           Blk 216 Bedok North Street 1  8893.11105
    ## 3971              Blk 22 Toa Payoh Lorong 7   628.99974
    ## 3972              Blk 226D Ang Mo Kio Ave 1  3384.01595
    ## 3973          Blk 226H Ang Mo Kio Street 22  3387.67255
    ## 3974          Blk 254 Jurong East Street 24 12982.65197
    ## 3975                  Blk 29 Bendemeer Road  2517.17070
    ## 3976                    Blk 320 Shunfu Road  2328.32931
    ## 3977               Blk 341 Ang Mo Kio Ave 1  2723.72135
    ## 3978              Blk 347 Jurong East Ave 1 13679.44856
    ## 3979                 Blk 353 Clementi Ave 2  9730.00099
    ## 3980              Blk 36 Telok Blangah Rise  8308.27322
    ## 3981             Blk 37A Teban Gardens Road 12593.49428
    ## 3982              Blk 409 Ang Mo Kio Ave 10  2491.19085
    ## 3983                   Blk 44 Holland Drive  7724.15994
    ## 3984                 Blk 448 Clementi Ave 3 10439.15305
    ## 3985             Blk 453A Ang Mo Kio Ave 10  3101.49771
    ## 3986                      Blk 49 Sims Place  3784.51432
    ## 3987                  Blk 4A Eunos Crescent  5971.04331
    ## 3988                      Blk 4A Jalan Batu  5321.71148
    ## 3989           Blk 4A Woodlands Centre Road 14550.51410
    ## 3990               Blk 502 West Coast Drive 11052.61026
    ## 3991               Blk 503 West Coast Drive 10992.91543
    ## 3992          Blk 505 Jurong West Street 52 15165.02110
    ## 3993                 Blk 50A Marine Terrace  7821.95436
    ## 3994                Blk 51 Old Airport Road  4976.80621
    ## 3995           Blk 511 Bedok North Street 3  8516.41539
    ## 3996 Bukit Panjang Hawker Centre and Market 10006.66330
    ## 3997         Our Tampines Hub Hawker Centre  9614.74448
    ## 3998        Kampung Admiralty Hawker Centre 12537.13331
    ## 3999                   Yishun Hawker Centre  9433.27726
    ## 4000   Jurong West Hawker Centre and Market 17471.77171
    ## 4001        Pasir Ris Central Hawker Centre 11416.75884
    ## 4002                   Dawson Hawker Centre  7276.01030
    ## 4003      Woodlands Street 12 Hawker Centre 13241.05875
    ## 4004              Blk 527 Ang Mo Kio Ave 10  3594.70311
    ## 4005           Blk 538 Bedok North Street 3  7896.09602
    ## 4006           Blk 58 New Upper Changi Road  9815.19301
    ## 4007              Blk 6 Tanjong Pagar Plaza  7147.75061
    ## 4008               Blk 628 Ang Mo Kio Ave 4  4758.81592
    ## 4009           Blk 630 Bedok Reservoir Road  6685.64977
    ## 4010                   Blk 69 Geylang Bahru  2708.95308
    ## 4011                     Blk 7 Empress Road  6041.27128
    ## 4012               Blk 724 Ang Mo Kio Ave 6  3638.53924
    ## 4013         Blk 726 Clementi West Street 2 10811.32128
    ## 4014              Blk 74 Toa Payoh Lorong 4   685.65812
    ## 4015              Market Street Food Centre  6254.64323
    ## 4016                    Maxwell Food Centre  6735.61411
    ## 4017                     Newton Food Centre  3544.66328
    ## 4018 North Bridge Road Market & Food Centre  3958.01786
    ## 4019              Pasir Panjang Food Centre  9989.11551
    ## 4020           Pek Kio Market & Food Centre  2705.06590
    ## 4021              People's Park Food Centre  6251.33954
    ## 4022            Sembawang Hills Food Centre  4539.67211
    ## 4023                Serangoon Garden Market  2880.35002
    ## 4024      Taman Jurong Market & Food Centre 14800.92026
    ## 4025                    Tanglin Halt Market  7678.87159
    ## 4026                           Tekka Market  3790.10479
    ## 4027                     Tiong Bahru Market  6575.60034
    ## 4028             Zion Riverside Food Centre  5894.48779
    ## 4029              Blk 75 Toa Payoh Lorong 5   494.08663
    ## 4030                    Blk 79 Redhill Lane  7044.18126
    ## 4031             Blk 79 Telok Blangah Drive  9044.39704
    ## 4032                    Blk 80 Circuit Road  3892.80230
    ## 4033             Blk 82 Telok Blangah Drive  8974.78276
    ## 4034           Blk 84 Marine Parade Central  7144.94554
    ## 4035            Blk 85 Bedok North Street 4  9430.33887
    ## 4036                    Blk 85 Redhill Lane  7096.23056
    ## 4037                    Blk 89 Circuit Road  3924.42520
    ## 4038                   Blk 90 Whampoa Drive  1916.69238
    ## 4039              Blk 93 Toa Payoh Lorong 4   578.07463
    ## 4040                   Blks 13/14 Haig Road  5362.14276
    ## 4041          Blks 160/162 Ang Mo Kio Ave 4  4144.74802
    ## 4042                  Ci Yuan Hawker Centre  5023.98132
    ## 4043              Blk 22 Toa Payoh Lorong 7  8526.64867
    ## 4044              Blk 226D Ang Mo Kio Ave 1 11344.96574
    ## 4045          Blk 226H Ang Mo Kio Street 22 11290.10203
    ## 4046          Blk 254 Jurong East Street 24 21825.73818
    ## 4047                  Blk 29 Bendemeer Road  7860.59897
    ## 4048                    Blk 320 Shunfu Road 11054.54226
    ## 4049               Blk 341 Ang Mo Kio Ave 1 10304.94621
    ## 4050              Blk 347 Jurong East Ave 1 22531.16379
    ## 4051                 Blk 353 Clementi Ave 2 18126.26473
    ## 4052              Blk 36 Telok Blangah Rise 13763.45630
    ## 4053             Blk 37A Teban Gardens Road 21192.96316
    ## 4054              Blk 409 Ang Mo Kio Ave 10  9521.26228
    ## 4055                   Blk 44 Holland Drive 15770.22353
    ## 4056                 Blk 448 Clementi Ave 3 18837.76258
    ## 4057             Blk 453A Ang Mo Kio Ave 10  9691.06540
    ## 4058                      Blk 49 Sims Place  6088.73249
    ## 4059                  Blk 4A Eunos Crescent  3307.84689
    ## 4060                      Blk 4A Jalan Batu  6124.14875
    ## 4061           Blk 4A Woodlands Centre Road 22087.90399
    ## 4062               Blk 502 West Coast Drive 19443.18772
    ## 4063               Blk 503 West Coast Drive 19379.13417
    ## 4064          Blk 505 Jurong West Street 52 24032.11782
    ## 4065                 Blk 50A Marine Terrace  3052.23546
    ## 4066                Blk 51 Old Airport Road  5669.22170
    ## 4067           Blk 511 Bedok North Street 3   743.32832
    ## 4068 Bukit Panjang Hawker Centre and Market 18740.24502
    ## 4069         Our Tampines Hub Hawker Centre  3050.51501
    ## 4070        Kampung Admiralty Hawker Centre 19320.46893
    ## 4071                   Yishun Hawker Centre 14637.76165
    ## 4072   Jurong West Hawker Centre and Market 26290.92285
    ## 4073        Pasir Ris Central Hawker Centre  5518.08808
    ## 4074                   Dawson Hawker Centre 14670.71122
    ## 4075      Woodlands Street 12 Hawker Centre 20741.10519
    ## 4076              Blk 527 Ang Mo Kio Ave 10 10115.81404
    ## 4077           Blk 538 Bedok North Street 3  1068.39239
    ## 4078           Blk 58 New Upper Changi Road   936.01877
    ## 4079              Blk 6 Tanjong Pagar Plaza 11470.07983
    ## 4080               Blk 628 Ang Mo Kio Ave 4 11902.28434
    ## 4081           Blk 630 Bedok Reservoir Road  2237.15968
    ## 4082                   Blk 69 Geylang Bahru  7062.63616
    ## 4083                     Blk 7 Empress Road 14249.25328
    ## 4084               Blk 724 Ang Mo Kio Ave 6 10863.30546
    ## 4085         Blk 726 Clementi West Street 2 18978.69839
    ## 4086              Blk 74 Toa Payoh Lorong 4  9077.03967
    ## 4087              Market Street Food Centre 10418.01192
    ## 4088                    Maxwell Food Centre 11123.24295
    ## 4089                     Newton Food Centre 10557.42750
    ## 4090 North Bridge Road Market & Food Centre  8067.18939
    ## 4091              Pasir Panjang Food Centre 16766.14782
    ## 4092           Pek Kio Market & Food Centre  9315.02792
    ## 4093              People's Park Food Centre 11108.70823
    ## 4094            Sembawang Hills Food Centre 12635.67312
    ## 4095                Serangoon Garden Market  8406.77888
    ## 4096      Taman Jurong Market & Food Centre 23574.91840
    ## 4097                    Tanglin Halt Market 15367.61488
    ## 4098                           Tekka Market  9487.23617
    ## 4099                     Tiong Bahru Market 12147.81401
    ## 4100             Zion Riverside Food Centre 11986.68961
    ## 4101              Blk 75 Toa Payoh Lorong 5  8989.30182
    ## 4102                    Blk 79 Redhill Lane 13493.58625
    ## 4103             Blk 79 Telok Blangah Drive 15186.27743
    ## 4104                    Blk 80 Circuit Road  5130.42568
    ## 4105             Blk 82 Telok Blangah Drive 15130.30735
    ## 4106           Blk 84 Marine Parade Central  4056.75072
    ## 4107            Blk 85 Bedok North Street 4   820.17887
    ## 4108                    Blk 85 Redhill Lane 13519.13804
    ## 4109                    Blk 89 Circuit Road  5331.60582
    ## 4110                   Blk 90 Whampoa Drive  8701.88077
    ## 4111              Blk 93 Toa Payoh Lorong 4  9400.92126
    ## 4112                   Blks 13/14 Haig Road  4388.47770
    ## 4113          Blks 160/162 Ang Mo Kio Ave 4 11702.74472
    ## 4114                  Ci Yuan Hawker Centre  7737.43471
    ## 4115              Blk 226D Ang Mo Kio Ave 1  4012.67329
    ## 4116          Blk 226H Ang Mo Kio Street 22  4016.65458
    ## 4117          Blk 254 Jurong East Street 24 13302.80070
    ## 4118                  Blk 29 Bendemeer Road  1891.93727
    ## 4119                    Blk 320 Shunfu Road  2897.79597
    ## 4120               Blk 341 Ang Mo Kio Ave 1  3338.94087
    ## 4121              Blk 347 Jurong East Ave 1 14006.36566
    ## 4122                 Blk 353 Clementi Ave 2  9866.61387
    ## 4123              Blk 36 Telok Blangah Rise  7950.55122
    ## 4124             Blk 37A Teban Gardens Road 12802.28007
    ## 4125              Blk 409 Ang Mo Kio Ave 10  3050.59262
    ## 4126                   Blk 44 Holland Drive  7754.74660
    ## 4127                 Blk 448 Clementi Ave 3 10579.75791
    ## 4128             Blk 453A Ang Mo Kio Ave 10  3650.76564
    ## 4129                      Blk 49 Sims Place  3198.58392
    ## 4130                  Blk 4A Eunos Crescent  5507.83050
    ## 4131                      Blk 4A Jalan Batu  4704.20671
    ## 4132           Blk 4A Woodlands Centre Road 15165.67591
    ## 4133               Blk 502 West Coast Drive 11193.57205
    ## 4134               Blk 503 West Coast Drive 11132.43472
    ## 4135          Blk 505 Jurong West Street 52 15505.49632
    ## 4136                 Blk 50A Marine Terrace  7302.27551
    ## 4137                Blk 51 Old Airport Road  4378.48145
    ## 4138           Blk 511 Bedok North Street 3  8190.08681
    ## 4139 Bukit Panjang Hawker Centre and Market 10510.89788
    ## 4140         Our Tampines Hub Hawker Centre  9430.28259
    ## 4141        Kampung Admiralty Hawker Centre 13166.09882
    ## 4142                   Yishun Hawker Centre 10021.84393
    ## 4143   Jurong West Hawker Centre and Market 17777.82612
    ## 4144        Pasir Ris Central Hawker Centre 11334.17557
    ## 4145                   Dawson Hawker Centre  7164.14838
    ## 4146      Woodlands Street 12 Hawker Centre 13859.06257
    ## 4147              Blk 527 Ang Mo Kio Ave 10  4160.52654
    ## 4148           Blk 538 Bedok North Street 3  7555.98847
    ## 4149           Blk 58 New Upper Changi Road  9438.39752
    ## 4150              Blk 6 Tanjong Pagar Plaza  6662.79658
    ## 4151               Blk 628 Ang Mo Kio Ave 4  5380.74159
    ## 4152           Blk 630 Bedok Reservoir Road  6349.63080
    ## 4153                   Blk 69 Geylang Bahru  2097.87147
    ## 4154                     Blk 7 Empress Road  6093.32445
    ## 4155               Blk 724 Ang Mo Kio Ave 6  4251.77171
    ## 4156         Blk 726 Clementi West Street 2 10894.48872
    ## 4157              Blk 74 Toa Payoh Lorong 4   565.61967
    ## 4158              Market Street Food Centre  5732.90263
    ## 4159                    Maxwell Food Centre  6244.39992
    ## 4160                     Newton Food Centre  3231.34610
    ## 4161 North Bridge Road Market & Food Centre  3346.33082
    ## 4162              Pasir Panjang Food Centre  9817.54570
    ## 4163           Pek Kio Market & Food Centre  2240.92050
    ## 4164              People's Park Food Centre  5783.63433
    ## 4165            Sembawang Hills Food Centre  5159.13108
    ## 4166                Serangoon Garden Market  3271.18706
    ## 4167      Taman Jurong Market & Food Centre 15081.69596
    ## 4168                    Tanglin Halt Market  7630.19200
    ## 4169                           Tekka Market  3290.13900
    ## 4170                     Tiong Bahru Market  6187.96007
    ## 4171             Zion Riverside Food Centre  5546.64683
    ## 4172              Blk 75 Toa Payoh Lorong 5   464.33208
    ## 4173                    Blk 79 Redhill Lane  6776.85501
    ## 4174             Blk 79 Telok Blangah Drive  8776.63196
    ## 4175                    Blk 80 Circuit Road  3447.02533
    ## 4176             Blk 82 Telok Blangah Drive  8707.53967
    ## 4177           Blk 84 Marine Parade Central  6586.17256
    ## 4178            Blk 85 Bedok North Street 4  9099.05553
    ## 4179                    Blk 85 Redhill Lane  6826.49801
    ## 4180                    Blk 89 Circuit Road  3420.14027
    ## 4181                   Blk 90 Whampoa Drive  1375.11349
    ## 4182              Blk 93 Toa Payoh Lorong 4   910.75715
    ## 4183                   Blks 13/14 Haig Road  4833.53245
    ## 4184          Blks 160/162 Ang Mo Kio Ave 4  4772.54735
    ## 4185                  Ci Yuan Hawker Centre  5300.15196
    ## 4186          Blk 226H Ang Mo Kio Street 22    94.05883
    ## 4187          Blk 254 Jurong East Street 24 11581.59498
    ## 4188                  Blk 29 Bendemeer Road  5888.84460
    ## 4189                    Blk 320 Shunfu Road  1652.54916
    ## 4190               Blk 341 Ang Mo Kio Ave 1  1047.03144
    ## 4191              Blk 347 Jurong East Ave 1 12213.46061
    ## 4192                 Blk 353 Clementi Ave 2  9567.31996
    ## 4193              Blk 36 Telok Blangah Rise 10594.91194
    ## 4194             Blk 37A Teban Gardens Road 11864.04669
    ## 4195              Blk 409 Ang Mo Kio Ave 10  1850.17405
    ## 4196                   Blk 44 Holland Drive  8295.65502
    ## 4197                 Blk 448 Clementi Ave 3 10206.64494
    ## 4198             Blk 453A Ang Mo Kio Ave 10  1912.02188
    ## 4199                      Blk 49 Sims Place  7085.71760
    ## 4200                  Blk 4A Eunos Crescent  8874.82854
    ## 4201                      Blk 4A Jalan Batu  8684.30648
    ## 4202           Blk 4A Woodlands Centre Road 11236.48066
    ## 4203               Blk 502 West Coast Drive 10781.66399
    ## 4204               Blk 503 West Coast Drive 10733.32933
    ## 4205          Blk 505 Jurong West Street 52 13574.88549
    ## 4206                 Blk 50A Marine Terrace 10865.30022
    ## 4207                Blk 51 Old Airport Road  8294.12234
    ## 4208           Blk 511 Bedok North Street 3 10823.81170
    ## 4209 Bukit Panjang Hawker Centre and Market  7523.16069
    ## 4210         Our Tampines Hub Hawker Centre 11274.86141
    ## 4211        Kampung Admiralty Hawker Centre  9154.59366
    ## 4212                   Yishun Hawker Centre  6467.97598
    ## 4213   Jurong West Hawker Centre and Market 16034.17703
    ## 4214        Pasir Ris Central Hawker Centre 12520.52759
    ## 4215                   Dawson Hawker Centre  8571.92622
    ## 4216      Woodlands Street 12 Hawker Centre  9911.87745
    ## 4217              Blk 527 Ang Mo Kio Ave 10  1821.65032
    ## 4218           Blk 538 Bedok North Street 3 10284.04542
    ## 4219           Blk 58 New Upper Changi Road 12279.85472
    ## 4220              Blk 6 Tanjong Pagar Plaza  9980.48680
    ## 4221               Blk 628 Ang Mo Kio Ave 4  1582.53923
    ## 4222           Blk 630 Bedok Reservoir Road  9119.45317
    ## 4223                   Blk 69 Geylang Bahru  6070.19368
    ## 4224                     Blk 7 Empress Road  6723.94846
    ## 4225               Blk 724 Ang Mo Kio Ave 6  1007.78652
    ## 4226         Blk 726 Clementi West Street 2 10869.36587
    ## 4227              Blk 74 Toa Payoh Lorong 4  3837.62163
    ## 4228              Market Street Food Centre  9247.34083
    ## 4229                    Maxwell Food Centre  9604.33867
    ## 4230                     Newton Food Centre  6070.15200
    ## 4231 North Bridge Road Market & Food Centre  7284.27604
    ## 4232              Pasir Panjang Food Centre 11371.73631
    ## 4233           Pek Kio Market & Food Centre  5730.29312
    ## 4234              People's Park Food Centre  9052.31975
    ## 4235            Sembawang Hills Food Centre  1296.53302
    ## 4236                Serangoon Garden Market  3087.31800
    ## 4237      Taman Jurong Market & Food Centre 13575.16829
    ## 4238                    Tanglin Halt Market  8642.72130
    ## 4239                           Tekka Market  6815.83784
    ## 4240                     Tiong Bahru Market  9066.31274
    ## 4241             Zion Riverside Food Centre  8280.31017
    ## 4242              Blk 75 Toa Payoh Lorong 5  3726.71005
    ## 4243                    Blk 79 Redhill Lane  9030.20809
    ## 4244             Blk 79 Telok Blangah Drive 10912.22471
    ## 4245                    Blk 80 Circuit Road  6852.11168
    ## 4246             Blk 82 Telok Blangah Drive 10843.77109
    ## 4247           Blk 84 Marine Parade Central 10326.97649
    ## 4248            Blk 85 Bedok North Street 4 11722.94579
    ## 4249                    Blk 85 Redhill Lane  9088.38795
    ## 4250                    Blk 89 Circuit Road  7030.28235
    ## 4251                   Blk 90 Whampoa Drive  5161.18089
    ## 4252              Blk 93 Toa Payoh Lorong 4  3333.26306
    ## 4253                   Blks 13/14 Haig Road  8484.03047
    ## 4254          Blks 160/162 Ang Mo Kio Ave 4   851.59212
    ## 4255                  Ci Yuan Hawker Centre  4961.35241
    ## 4256          Blk 254 Jurong East Street 24 11670.84770
    ## 4257                  Blk 29 Bendemeer Road  5897.17960
    ## 4258                    Blk 320 Shunfu Road  1714.35907
    ## 4259               Blk 341 Ang Mo Kio Ave 1   986.31793
    ## 4260              Blk 347 Jurong East Ave 1 12301.81295
    ## 4261                 Blk 353 Clementi Ave 2  9660.87817
    ## 4262              Blk 36 Telok Blangah Rise 10657.77191
    ## 4263             Blk 37A Teban Gardens Road 11957.59495
    ## 4264              Blk 409 Ang Mo Kio Ave 10  1785.19681
    ## 4265                   Blk 44 Holland Drive  8384.04752
    ## 4266                 Blk 448 Clementi Ave 3 10300.46609
    ## 4267             Blk 453A Ang Mo Kio Ave 10  1828.34719
    ## 4268                      Blk 49 Sims Place  7073.58201
    ## 4269                  Blk 4A Eunos Crescent  8838.06478
    ## 4270                      Blk 4A Jalan Batu  8678.89071
    ## 4271           Blk 4A Woodlands Centre Road 11256.20931
    ## 4272               Blk 502 West Coast Drive 10875.60191
    ## 4273               Blk 503 West Coast Drive 10827.24672
    ## 4274          Blk 505 Jurong West Street 52 13661.31166
    ## 4275                 Blk 50A Marine Terrace 10833.07074
    ## 4276                Blk 51 Old Airport Road  8282.60082
    ## 4277           Blk 511 Bedok North Street 3 10765.36023
    ## 4278 Bukit Panjang Hawker Centre and Market  7594.93827
    ## 4279         Our Tampines Hub Hawker Centre 11201.53450
    ## 4280        Kampung Admiralty Hawker Centre  9149.47549
    ## 4281                   Yishun Hawker Centre  6412.08525
    ## 4282   Jurong West Hawker Centre and Market 16121.90137
    ## 4283        Pasir Ris Central Hawker Centre 12437.54111
    ## 4284                   Dawson Hawker Centre  8651.81017
    ## 4285      Woodlands Street 12 Hawker Centre  9929.48616
    ## 4286              Blk 527 Ang Mo Kio Ave 10  1729.11370
    ## 4287           Blk 538 Bedok North Street 3 10228.31736
    ## 4288           Blk 58 New Upper Changi Road 12224.67356
    ## 4289              Blk 6 Tanjong Pagar Plaza 10026.18279
    ## 4290               Blk 628 Ang Mo Kio Ave 4  1527.50658
    ## 4291           Blk 630 Bedok Reservoir Road  9066.38490
    ## 4292                   Blk 69 Geylang Bahru  6065.99833
    ## 4293                     Blk 7 Empress Road  6809.39856
    ## 4294               Blk 724 Ang Mo Kio Ave 6   914.13260
    ## 4295         Blk 726 Clementi West Street 2 10962.39635
    ## 4296              Blk 74 Toa Payoh Lorong 4  3854.44666
    ## 4297              Market Street Food Centre  9285.81557
    ## 4298                    Maxwell Food Centre  9648.31602
    ## 4299                     Newton Food Centre  6119.12419
    ## 4300 North Bridge Road Market & Food Centre  7299.92605
    ## 4301              Pasir Panjang Food Centre 11452.67719
    ## 4302           Pek Kio Market & Food Centre  5761.62939
    ## 4303              People's Park Food Centre  9098.27432
    ## 4304            Sembawang Hills Food Centre  1346.22153
    ## 4305                Serangoon Garden Market  3014.60854
    ## 4306      Taman Jurong Market & Food Centre 13665.54358
    ## 4307                    Tanglin Halt Market  8727.17259
    ## 4308                           Tekka Market  6849.67158
    ## 4309                     Tiong Bahru Market  9122.17027
    ## 4310             Zion Riverside Food Centre  8337.96967
    ## 4311              Blk 75 Toa Payoh Lorong 5  3739.62656
    ## 4312                    Blk 79 Redhill Lane  9098.35194
    ## 4313             Blk 79 Telok Blangah Drive 10984.60124
    ## 4314                    Blk 80 Circuit Road  6820.94016
    ## 4315             Blk 82 Telok Blangah Drive 10916.07171
    ## 4316           Blk 84 Marine Parade Central 10303.07592
    ## 4317            Blk 85 Bedok North Street 4 11663.35243
    ## 4318                    Blk 85 Redhill Lane  9156.47262
    ## 4319                    Blk 89 Circuit Road  7005.45096
    ## 4320                   Blk 90 Whampoa Drive  5180.41177
    ## 4321              Blk 93 Toa Payoh Lorong 4  3352.91162
    ## 4322                   Blks 13/14 Haig Road  8458.03901
    ## 4323          Blks 160/162 Ang Mo Kio Ave 4   806.86100
    ## 4324                  Ci Yuan Hawker Centre  4873.21103
    ## 4325                  Blk 29 Bendemeer Road 14193.00716
    ## 4326                    Blk 320 Shunfu Road 11086.02322
    ## 4327               Blk 341 Ang Mo Kio Ave 1 12504.89710
    ## 4328              Blk 347 Jurong East Ave 1   720.47887
    ## 4329                 Blk 353 Clementi Ave 2  4885.56505
    ## 4330              Blk 36 Telok Blangah Rise 12222.77035
    ## 4331             Blk 37A Teban Gardens Road  2562.22642
    ## 4332              Blk 409 Ang Mo Kio Ave 10 13259.62592
    ## 4333                   Blk 44 Holland Drive  7260.34492
    ## 4334                 Blk 448 Clementi Ave 3  4470.28205
    ## 4335             Blk 453A Ang Mo Kio Ave 10 13477.40499
    ## 4336                      Blk 49 Sims Place 16029.81186
    ## 4337                  Blk 4A Eunos Crescent 18702.86294
    ## 4338                      Blk 4A Jalan Batu 16886.74251
    ## 4339           Blk 4A Woodlands Centre Road 11354.03272
    ## 4340               Blk 502 West Coast Drive  4223.32798
    ## 4341               Blk 503 West Coast Drive  4268.22501
    ## 4342          Blk 505 Jurong West Street 52  2258.21309
    ## 4343                 Blk 50A Marine Terrace 20240.63643
    ## 4344                Blk 51 Old Airport Road 16929.93941
    ## 4345           Blk 511 Bedok North Street 3 21488.51503
    ## 4346 Bukit Panjang Hawker Centre and Market  5402.42444
    ## 4347         Our Tampines Hub Hawker Centre 22503.74793
    ## 4348        Kampung Admiralty Hawker Centre 12761.67489
    ## 4349                   Yishun Hawker Centre 14935.50082
    ## 4350   Jurong West Hawker Centre and Market  4500.00398
    ## 4351        Pasir Ris Central Hawker Centre 24019.27459
    ## 4352                   Dawson Hawker Centre  9017.31937
    ## 4353      Woodlands Street 12 Hawker Centre 11004.58933
    ## 4354              Blk 527 Ang Mo Kio Ave 10 13386.28874
    ## 4355           Blk 538 Bedok North Street 3 20858.02341
    ## 4356           Blk 58 New Upper Changi Road 22730.79125
    ## 4357              Blk 6 Tanjong Pagar Plaza 13862.19372
    ## 4358               Blk 628 Ang Mo Kio Ave 4 12182.40483
    ## 4359           Blk 630 Bedok Reservoir Road 19650.19735
    ## 4360                   Blk 69 Geylang Bahru 14914.42978
    ## 4361                     Blk 7 Empress Road  8128.34168
    ## 4362               Blk 724 Ang Mo Kio Ave 6 12512.03731
    ## 4363         Blk 726 Clementi West Street 2  5283.80513
    ## 4364              Blk 74 Toa Payoh Lorong 4 12749.22277
    ## 4365              Market Street Food Centre 14122.57904
    ## 4366                    Maxwell Food Centre 13818.97200
    ## 4367                     Newton Food Centre 11852.29394
    ## 4368 North Bridge Road Market & Food Centre 14640.04011
    ## 4369              Pasir Panjang Food Centre  9559.15386
    ## 4370           Pek Kio Market & Food Centre 12872.33689
    ## 4371              People's Park Food Centre 13334.09155
    ## 4372            Sembawang Hills Food Centre 10643.50048
    ## 4373                Serangoon Garden Market 14515.99830
    ## 4374      Taman Jurong Market & Food Centre  2057.50339
    ## 4375                    Tanglin Halt Market  8160.93133
    ## 4376                           Tekka Market 13206.17921
    ## 4377                     Tiong Bahru Market 12344.99136
    ## 4378             Zion Riverside Food Centre 11831.79261
    ## 4379              Blk 75 Toa Payoh Lorong 5 12842.23055
    ## 4380                    Blk 79 Redhill Lane 10875.63805
    ## 4381             Blk 79 Telok Blangah Drive 10975.42801
    ## 4382                    Blk 80 Circuit Road 16709.97222
    ## 4383             Blk 82 Telok Blangah Drive 10958.01563
    ## 4384           Blk 84 Marine Parade Central 19302.89445
    ## 4385            Blk 85 Bedok North Street 4 22399.59903
    ## 4386                    Blk 85 Redhill Lane 10902.86749
    ## 4387                    Blk 89 Circuit Road 16581.99592
    ## 4388                   Blk 90 Whampoa Drive 13255.78847
    ## 4389              Blk 93 Toa Payoh Lorong 4 12446.72430
    ## 4390                   Blks 13/14 Haig Road 17841.24633
    ## 4391          Blks 160/162 Ang Mo Kio Ave 4 11798.81064
    ## 4392                  Ci Yuan Hawker Centre 16541.28424
    ## 4393                    Blk 320 Shunfu Road  4638.12469
    ## 4394               Blk 341 Ang Mo Kio Ave 1  5228.50134
    ## 4395              Blk 347 Jurong East Ave 1 14909.97064
    ## 4396                 Blk 353 Clementi Ave 2 10273.25945
    ## 4397              Blk 36 Telok Blangah Rise  6886.97666
    ## 4398             Blk 37A Teban Gardens Road 13370.47900
    ## 4399              Blk 409 Ang Mo Kio Ave 10  4890.45572
    ## 4400                   Blk 44 Holland Drive  7913.63497
    ## 4401                 Blk 448 Clementi Ave 3 10983.04239
    ## 4402             Blk 453A Ang Mo Kio Ave 10  5471.35471
    ## 4403                      Blk 49 Sims Place  1839.86573
    ## 4404                  Blk 4A Eunos Crescent  4591.20757
    ## 4405                      Blk 4A Jalan Batu  2978.29564
    ## 4406           Blk 4A Woodlands Centre Road 16965.43505
    ## 4407               Blk 502 West Coast Drive 11586.45551
    ## 4408               Blk 503 West Coast Drive 11522.16408
    ## 4409          Blk 505 Jurong West Street 52 16437.52438
    ## 4410                 Blk 50A Marine Terrace  6054.79363
    ## 4411                Blk 51 Old Airport Road  2813.42520
    ## 4412           Blk 511 Bedok North Street 3  7679.37718
    ## 4413 Bukit Panjang Hawker Centre and Market 11966.03760
    ## 4414         Our Tampines Hub Hawker Centre  9356.62401
    ## 4415        Kampung Admiralty Hawker Centre 15042.63240
    ## 4416                   Yishun Hawker Centre 11874.33934
    ## 4417   Jurong West Hawker Centre and Market 18591.95254
    ## 4418        Pasir Ris Central Hawker Centre 11532.24408
    ## 4419                   Dawson Hawker Centre  6919.56150
    ## 4420      Woodlands Street 12 Hawker Centre 15671.78432
    ## 4421              Blk 527 Ang Mo Kio Ave 10  5997.36905
    ## 4422           Blk 538 Bedok North Street 3  7019.12855
    ## 4423           Blk 58 New Upper Changi Road  8710.34480
    ## 4424              Blk 6 Tanjong Pagar Plaza  5203.22946
    ## 4425               Blk 628 Ang Mo Kio Ave 4  7272.52659
    ## 4426           Blk 630 Bedok Reservoir Road  5884.92663
    ## 4427                   Blk 69 Geylang Bahru   815.83674
    ## 4428                     Blk 7 Empress Road  6394.83549
    ## 4429               Blk 724 Ang Mo Kio Ave 6  6139.42303
    ## 4430         Blk 726 Clementi West Street 2 11122.44413
    ## 4431              Blk 74 Toa Payoh Lorong 4  2092.49819
    ## 4432              Market Street Food Centre  4167.69304
    ## 4433                    Maxwell Food Centre  4774.59110
    ## 4434                     Newton Food Centre  2733.47051
    ## 4435 North Bridge Road Market & Food Centre  1490.55652
    ## 4436              Pasir Panjang Food Centre  9298.56240
    ## 4437           Pek Kio Market & Food Centre  1463.67287
    ## 4438              People's Park Food Centre  4415.72984
    ## 4439            Sembawang Hills Food Centre  6993.09199
    ## 4440                Serangoon Garden Market  4876.24380
    ## 4441      Taman Jurong Market & Food Centre 15839.16033
    ## 4442                    Tanglin Halt Market  7557.78100
    ## 4443                           Tekka Market  2000.53835
    ## 4444                     Tiong Bahru Market  5088.61051
    ## 4445             Zion Riverside Food Centre  4623.96089
    ## 4446              Blk 75 Toa Payoh Lorong 5  2172.15529
    ## 4447                    Blk 79 Redhill Lane  6056.51494
    ## 4448             Blk 79 Telok Blangah Drive  7981.74776
    ## 4449                    Blk 80 Circuit Road  2847.63657
    ## 4450             Blk 82 Telok Blangah Drive  7916.24541
    ## 4451           Blk 84 Marine Parade Central  5171.58686
    ## 4452            Blk 85 Bedok North Street 4  8543.39824
    ## 4453                    Blk 85 Redhill Lane  6095.86912
    ## 4454                    Blk 89 Circuit Road  2539.40891
    ## 4455                   Blk 90 Whampoa Drive   969.29845
    ## 4456              Blk 93 Toa Payoh Lorong 4  2606.59310
    ## 4457                   Blks 13/14 Haig Road  3654.62498
    ## 4458          Blks 160/162 Ang Mo Kio Ave 4  6661.28046
    ## 4459                  Ci Yuan Hawker Centre  6600.50973
    ## 4460               Blk 341 Ang Mo Kio Ave 1  1830.10080
    ## 4461              Blk 347 Jurong East Ave 1 11758.97123
    ## 4462                 Blk 353 Clementi Ave 2  8461.22637
    ## 4463              Blk 36 Telok Blangah Rise  8943.42015
    ## 4464             Blk 37A Teban Gardens Road 11028.79933
    ## 4465              Blk 409 Ang Mo Kio Ave 10  2362.38634
    ## 4466                   Blk 44 Holland Drive  6914.67581
    ## 4467                 Blk 448 Clementi Ave 3  9137.34761
    ## 4468             Blk 453A Ang Mo Kio Ave 10  2800.82574
    ## 4469                      Blk 49 Sims Place  6096.17270
    ## 4470                  Blk 4A Eunos Crescent  8261.13615
    ## 4471                      Blk 4A Jalan Batu  7570.95871
    ## 4472           Blk 4A Woodlands Centre Road 12328.32010
    ## 4473               Blk 502 West Coast Drive  9735.07318
    ## 4474               Blk 503 West Coast Drive  9681.54269
    ## 4475          Blk 505 Jurong West Street 52 13199.69530
    ## 4476                 Blk 50A Marine Terrace 10144.16617
    ## 4477                Blk 51 Old Airport Road  7273.64802
    ## 4478           Blk 511 Bedok North Street 3 10617.99549
    ## 4479 Bukit Panjang Hawker Centre and Market  7720.16991
    ## 4480         Our Tampines Hub Hawker Centre 11430.56452
    ## 4481        Kampung Admiralty Hawker Centre 10529.72068
    ## 4482                   Yishun Hawker Centre  8118.63326
    ## 4483   Jurong West Hawker Centre and Market 15584.39742
    ## 4484        Pasir Ris Central Hawker Centre 12962.12942
    ## 4485                   Dawson Hawker Centre  7020.01174
    ## 4486      Woodlands Street 12 Hawker Centre 11039.18644
    ## 4487              Blk 527 Ang Mo Kio Ave 10  3007.06896
    ## 4488           Blk 538 Bedok North Street 3 10025.22961
    ## 4489           Blk 58 New Upper Changi Road 11986.72863
    ## 4490              Blk 6 Tanjong Pagar Plaza  8364.46623
    ## 4491               Blk 628 Ang Mo Kio Ave 4  3234.21598
    ## 4492           Blk 630 Bedok Reservoir Road  8821.45588
    ## 4493                   Blk 69 Geylang Bahru  4987.27941
    ## 4494                     Blk 7 Empress Road  5276.81360
    ## 4495               Blk 724 Ang Mo Kio Ave 6  2469.59063
    ## 4496         Blk 726 Clementi West Street 2  9696.27835
    ## 4497              Blk 74 Toa Payoh Lorong 4  2549.82609
    ## 4498              Market Street Food Centre  7672.19629
    ## 4499                    Maxwell Food Centre  7997.00796
    ## 4500                     Newton Food Centre  4444.65408
    ## 4501 North Bridge Road Market & Food Centre  5922.83456
    ## 4502              Pasir Panjang Food Centre  9828.26466
    ## 4503           Pek Kio Market & Food Centre  4227.03487
    ## 4504              People's Park Food Centre  7436.00023
    ## 4505            Sembawang Hills Food Centre  2424.29421
    ## 4506                Serangoon Garden Market  3528.30699
    ## 4507      Taman Jurong Market & Food Centre 12998.70635
    ## 4508                    Tanglin Halt Market  7162.89650
    ## 4509                           Tekka Market  5282.63057
    ## 4510                     Tiong Bahru Market  7417.91092
    ## 4511             Zion Riverside Food Centre  6629.44307
    ## 4512              Blk 75 Toa Payoh Lorong 5  2500.55115
    ## 4513                    Blk 79 Redhill Lane  7390.08200
    ## 4514             Blk 79 Telok Blangah Drive  9290.91884
    ## 4515                    Blk 80 Circuit Road  6182.13399
    ## 4516             Blk 82 Telok Blangah Drive  9222.05602
    ## 4517           Blk 84 Marine Parade Central  9472.11102
    ## 4518            Blk 85 Bedok North Street 4 11532.66514
    ## 4519                    Blk 85 Redhill Lane  7448.04562
    ## 4520                    Blk 89 Circuit Road  6245.70788
    ## 4521                   Blk 90 Whampoa Drive  3793.23712
    ## 4522              Blk 93 Toa Payoh Lorong 4  2041.73131
    ## 4523                   Blks 13/14 Haig Road  7688.65442
    ## 4524          Blks 160/162 Ang Mo Kio Ave 4  2497.74673
    ## 4525                  Ci Yuan Hawker Centre  5731.73211
    ## 4526              Blk 347 Jurong East Ave 1 13149.76979
    ## 4527                 Blk 353 Clementi Ave 2 10221.43661
    ## 4528              Blk 36 Telok Blangah Rise 10536.03717
    ## 4529             Blk 37A Teban Gardens Road 12663.75399
    ## 4530              Blk 409 Ang Mo Kio Ave 10   805.54578
    ## 4531                   Blk 44 Holland Drive  8743.51727
    ## 4532                 Blk 448 Clementi Ave 3 10883.99534
    ## 4533             Blk 453A Ang Mo Kio Ave 10  1010.50141
    ## 4534                      Blk 49 Sims Place  6249.93761
    ## 4535                  Blk 4A Eunos Crescent  7891.60279
    ## 4536                      Blk 4A Jalan Batu  7892.93978
    ## 4537           Blk 4A Woodlands Centre Road 12151.76417
    ## 4538               Blk 502 West Coast Drive 11473.20112
    ## 4539               Blk 503 West Coast Drive 11421.81840
    ## 4540          Blk 505 Jurong West Street 52 14534.24902
    ## 4541                 Blk 50A Marine Terrace  9901.72306
    ## 4542                Blk 51 Old Airport Road  7458.78034
    ## 4543           Blk 511 Bedok North Street 3  9779.04333
    ## 4544 Bukit Panjang Hawker Centre and Market  8562.71615
    ## 4545         Our Tampines Hub Hawker Centre 10242.58706
    ## 4546        Kampung Admiralty Hawker Centre  9915.29532
    ## 4547                   Yishun Hawker Centre  6746.68795
    ## 4548   Jurong West Hawker Centre and Market 16976.90190
    ## 4549        Pasir Ris Central Hawker Centre 11541.00183
    ## 4550                   Dawson Hawker Centre  8817.18618
    ## 4551      Woodlands Street 12 Hawker Centre 10816.93077
    ## 4552              Blk 527 Ang Mo Kio Ave 10  1182.55890
    ## 4553           Blk 538 Bedok North Street 3  9242.68096
    ## 4554           Blk 58 New Upper Changi Road 11239.29953
    ## 4555              Blk 6 Tanjong Pagar Plaza  9690.80932
    ## 4556               Blk 628 Ang Mo Kio Ave 4  2051.90704
    ## 4557           Blk 630 Bedok Reservoir Road  8082.82227
    ## 4558                   Blk 69 Geylang Bahru  5300.24119
    ## 4559                     Blk 7 Empress Road  7106.40390
    ## 4560               Blk 724 Ang Mo Kio Ave 6   915.39662
    ## 4561         Blk 726 Clementi West Street 2 11483.11379
    ## 4562              Blk 74 Toa Payoh Lorong 4  3294.72862
    ## 4563              Market Street Food Centre  8874.51882
    ## 4564                    Maxwell Food Centre  9295.33254
    ## 4565                     Newton Food Centre  5853.89318
    ## 4566 North Bridge Road Market & Food Centre  6681.51556
    ## 4567              Pasir Panjang Food Centre 11625.02976
    ## 4568           Pek Kio Market & Food Centre  5306.07656
    ## 4569              People's Park Food Centre  8770.80928
    ## 4570            Sembawang Hills Food Centre  2330.75130
    ## 4571                Serangoon Garden Market  2059.63839
    ## 4572      Taman Jurong Market & Food Centre 14476.81433
    ## 4573                    Tanglin Halt Market  8986.80737
    ## 4574                           Tekka Market  6407.22353
    ## 4575                     Tiong Bahru Market  8914.78847
    ## 4576             Zion Riverside Food Centre  8159.28511
    ## 4577              Blk 75 Toa Payoh Lorong 5  3146.96945
    ## 4578                    Blk 79 Redhill Lane  9061.94283
    ## 4579             Blk 79 Telok Blangah Drive 11004.94484
    ## 4580                    Blk 80 Circuit Road  5898.17743
    ## 4581             Blk 82 Telok Blangah Drive 10935.47097
    ## 4582           Blk 84 Marine Parade Central  9408.13534
    ## 4583            Blk 85 Bedok North Street 4 10677.19525
    ## 4584                    Blk 85 Redhill Lane  9118.80439
    ## 4585                    Blk 89 Circuit Road  6111.32848
    ## 4586                   Blk 90 Whampoa Drive  4615.51244
    ## 4587              Blk 93 Toa Payoh Lorong 4  2837.54370
    ## 4588                   Blks 13/14 Haig Road  7555.57549
    ## 4589          Blks 160/162 Ang Mo Kio Ave 4  1526.59088
    ## 4590                  Ci Yuan Hawker Centre  4062.13291
    ## 4591                 Blk 353 Clementi Ave 2  5549.36500
    ## 4592              Blk 36 Telok Blangah Rise 12888.05681
    ## 4593             Blk 37A Teban Gardens Road  2983.70525
    ## 4594              Blk 409 Ang Mo Kio Ave 10 13910.99662
    ## 4595                   Blk 44 Holland Drive  7955.81851
    ## 4596                 Blk 448 Clementi Ave 3  5097.91796
    ## 4597             Blk 453A Ang Mo Kio Ave 10 14115.20782
    ## 4598                      Blk 49 Sims Place 16746.24833
    ## 4599                  Blk 4A Eunos Crescent 19415.11727
    ## 4600                      Blk 4A Jalan Batu 17607.09385
    ## 4601           Blk 4A Woodlands Centre Road 11399.16620
    ## 4602               Blk 502 West Coast Drive  4804.55538
    ## 4603               Blk 503 West Coast Drive  4853.74742
    ## 4604          Blk 505 Jurong West Street 52  1537.89290
    ## 4605                 Blk 50A Marine Terrace 20958.66647
    ## 4606                Blk 51 Old Airport Road 17649.20512
    ## 4607           Blk 511 Bedok North Street 3 22189.22032
    ## 4608 Bukit Panjang Hawker Centre and Market  5785.72405
    ## 4609         Our Tampines Hub Hawker Centre 23183.66412
    ## 4610        Kampung Admiralty Hawker Centre 12985.78208
    ## 4611                   Yishun Hawker Centre 15371.66807
    ## 4612   Jurong West Hawker Centre and Market  3829.67816
    ## 4613        Pasir Ris Central Hawker Centre 24676.08006
    ## 4614                   Dawson Hawker Centre  9706.45141
    ## 4615      Woodlands Street 12 Hawker Centre 11133.34948
    ## 4616              Blk 527 Ang Mo Kio Ave 10 14010.60806
    ## 4617           Blk 538 Bedok North Street 3 21560.26212
    ## 4618           Blk 58 New Upper Changi Road 23437.70618
    ## 4619              Blk 6 Tanjong Pagar Plaza 14557.64593
    ## 4620               Blk 628 Ang Mo Kio Ave 4 12767.84611
    ## 4621           Blk 630 Bedok Reservoir Road 20351.84156
    ## 4622                   Blk 69 Geylang Bahru 15629.33030
    ## 4623                     Blk 7 Empress Road  8846.26990
    ## 4624               Blk 724 Ang Mo Kio Ave 6 13132.34133
    ## 4625         Blk 726 Clementi West Street 2  5858.31607
    ## 4626              Blk 74 Toa Payoh Lorong 4 13454.14376
    ## 4627              Market Street Food Centre 14830.17865
    ## 4628                    Maxwell Food Centre 14519.77174
    ## 4629                     Newton Food Centre 12572.75321
    ## 4630 North Bridge Road Market & Food Centre 15360.51664
    ## 4631              Pasir Panjang Food Centre 10159.84935
    ## 4632           Pek Kio Market & Food Centre 13591.79858
    ## 4633              People's Park Food Centre 14038.84293
    ## 4634            Sembawang Hills Food Centre 11246.81338
    ## 4635                Serangoon Garden Market 15170.39202
    ## 4636      Taman Jurong Market & Food Centre  1630.74106
    ## 4637                    Tanglin Halt Market  8846.68222
    ## 4638                           Tekka Market 13926.42902
    ## 4639                     Tiong Bahru Market 13042.77549
    ## 4640             Zion Riverside Food Centre 12537.80566
    ## 4641              Blk 75 Toa Payoh Lorong 5 13545.13815
    ## 4642                    Blk 79 Redhill Lane 11563.97995
    ## 4643             Blk 79 Telok Blangah Drive 11615.23116
    ## 4644                    Blk 80 Circuit Road 17418.30114
    ## 4645             Blk 82 Telok Blangah Drive 11599.80103
    ## 4646           Blk 84 Marine Parade Central 20022.39159
    ## 4647            Blk 85 Bedok North Street 4 23100.94785
    ## 4648                    Blk 85 Redhill Lane 11590.21069
    ## 4649                    Blk 89 Circuit Road 17293.93383
    ## 4650                   Blk 90 Whampoa Drive 13971.40711
    ## 4651              Blk 93 Toa Payoh Lorong 4 13146.51304
    ## 4652                   Blks 13/14 Haig Road 18557.16079
    ## 4653          Blks 160/162 Ang Mo Kio Ave 4 12405.33362
    ## 4654                  Ci Yuan Hawker Centre 17174.80706
    ## 4655              Blk 36 Telok Blangah Rise  7339.16283
    ## 4656             Blk 37A Teban Gardens Road  3191.51133
    ## 4657              Blk 409 Ang Mo Kio Ave 10 10823.44092
    ## 4658                   Blk 44 Holland Drive  2537.31393
    ## 4659                 Blk 448 Clementi Ave 3   714.53016
    ## 4660             Blk 453A Ang Mo Kio Ave 10 11227.47667
    ## 4661                      Blk 49 Sims Place 12087.66923
    ## 4662                  Blk 4A Eunos Crescent 14863.44831
    ## 4663                      Blk 4A Jalan Batu 12656.55630
    ## 4664           Blk 4A Woodlands Centre Road 13984.92642
    ## 4665               Blk 502 West Coast Drive  1327.42040
    ## 4666               Blk 503 West Coast Drive  1265.87781
    ## 4667          Blk 505 Jurong West Street 52  7014.77598
    ## 4668                 Blk 50A Marine Terrace 16156.77717
    ## 4669                Blk 51 Old Airport Road 12815.87773
    ## 4670           Blk 511 Bedok North Street 3 17902.41643
    ## 4671 Bukit Panjang Hawker Centre and Market  6994.38371
    ## 4672         Our Tampines Hub Hawker Centre 19296.29921
    ## 4673        Kampung Admiralty Hawker Centre 14276.47594
    ## 4674                   Yishun Hawker Centre 14742.95518
    ## 4675   Jurong West Hawker Centre and Market  8696.47435
    ## 4676        Pasir Ris Central Hawker Centre 21144.63331
    ## 4677                   Dawson Hawker Centre  4207.17608
    ## 4678      Woodlands Street 12 Hawker Centre 13214.91788
    ## 4679              Blk 527 Ang Mo Kio Ave 10 11329.56110
    ## 4680           Blk 538 Bedok North Street 3 17247.32016
    ## 4681           Blk 58 New Upper Changi Road 18983.32108
    ## 4682              Blk 6 Tanjong Pagar Plaza  9067.79367
    ## 4683               Blk 628 Ang Mo Kio Ave 4 10711.95405
    ## 4684           Blk 630 Bedok Reservoir Road 16073.50986
    ## 4685                   Blk 69 Geylang Bahru 11064.24139
    ## 4686                     Blk 7 Empress Road  3878.42953
    ## 4687               Blk 724 Ang Mo Kio Ave 6 10574.99931
    ## 4688         Blk 726 Clementi West Street 2  1380.26034
    ## 4689              Blk 74 Toa Payoh Lorong 4  9304.11272
    ## 4690              Market Street Food Centre  9436.62144
    ## 4691                    Maxwell Food Centre  9063.95901
    ## 4692                     Newton Food Centre  7653.43913
    ## 4693 North Bridge Road Market & Food Centre 10399.65824
    ## 4694              Pasir Panjang Food Centre  4829.53180
    ## 4695           Pek Kio Market & Food Centre  8837.38037
    ## 4696              People's Park Food Centre  8619.73183
    ## 4697            Sembawang Hills Food Centre  9112.95668
    ## 4698                Serangoon Garden Market 11958.75770
    ## 4699      Taman Jurong Market & Food Centre  5926.47670
    ## 4700                    Tanglin Halt Market  3338.09233
    ## 4701                           Tekka Market  8915.01748
    ## 4702                     Tiong Bahru Market  7572.71753
    ## 4703             Zion Riverside Food Centre  7144.61154
    ## 4704              Blk 75 Toa Payoh Lorong 5  9446.42523
    ## 4705                    Blk 79 Redhill Lane  6051.05739
    ## 4706             Blk 79 Telok Blangah Drive  6108.76155
    ## 4707                    Blk 80 Circuit Road 13027.70143
    ## 4708             Blk 82 Telok Blangah Drive  6087.98567
    ## 4709           Blk 84 Marine Parade Central 15139.15488
    ## 4710            Blk 85 Bedok North Street 4 18786.43260
    ## 4711                    Blk 85 Redhill Lane  6073.34665
    ## 4712                    Blk 89 Circuit Road 12794.68078
    ## 4713                   Blk 90 Whampoa Drive  9429.66816
    ## 4714              Blk 93 Toa Payoh Lorong 4  9152.69450
    ## 4715                   Blks 13/14 Haig Road 13886.27386
    ## 4716          Blks 160/162 Ang Mo Kio Ave 4 10104.55033
    ## 4717                  Ci Yuan Hawker Centre 14189.43560
    ## 4718             Blk 37A Teban Gardens Road 10301.67677
    ## 4719              Blk 409 Ang Mo Kio Ave 10 10643.54484
    ## 4720                   Blk 44 Holland Drive  5107.14374
    ## 4721                 Blk 448 Clementi Ave 3  7827.55635
    ## 4722             Blk 453A Ang Mo Kio Ave 10 11249.23287
    ## 4723                      Blk 49 Sims Place  8063.70765
    ## 4724                  Blk 4A Eunos Crescent 10566.15010
    ## 4725                      Blk 4A Jalan Batu  7639.64132
    ## 4726           Blk 4A Woodlands Centre Road 19486.76413
    ## 4727               Blk 502 West Coast Drive  8239.84865
    ## 4728               Blk 503 West Coast Drive  8179.51580
    ## 4729          Blk 505 Jurong West Street 52 14338.74177
    ## 4730                 Blk 50A Marine Terrace 11058.33239
    ## 4731                Blk 51 Old Airport Road  8123.24906
    ## 4732           Blk 511 Bedok North Street 3 13820.81505
    ## 4733 Bukit Panjang Hawker Centre and Market 12859.07436
    ## 4734         Our Tampines Hub Hawker Centre 15886.41335
    ## 4735        Kampung Admiralty Hawker Centre 18660.68297
    ## 4736                   Yishun Hawker Centre 17047.87043
    ## 4737   Jurong West Hawker Centre and Market 15815.87226
    ## 4738        Pasir Ris Central Hawker Centre 18225.15151
    ## 4739                   Dawson Hawker Centre  3365.17572
    ## 4740      Woodlands Street 12 Hawker Centre 18412.56577
    ## 4741              Blk 527 Ang Mo Kio Ave 10 11655.49480
    ## 4742           Blk 538 Bedok North Street 3 13183.07355
    ## 4743           Blk 58 New Upper Changi Road 14431.33955
    ## 4744              Blk 6 Tanjong Pagar Plaza  2393.52068
    ## 4745               Blk 628 Ang Mo Kio Ave 4 12173.12052
    ## 4746           Blk 630 Bedok Reservoir Road 12238.34181
    ## 4747                   Blk 69 Geylang Bahru  7599.13319
    ## 4748                     Blk 7 Empress Road  5165.64615
    ## 4749               Blk 724 Ang Mo Kio Ave 6 11350.67526
    ## 4750         Blk 726 Clementi West Street 2  7299.27104
    ## 4751              Blk 74 Toa Payoh Lorong 4  7623.50329
    ## 4752              Market Street Food Centre  3356.68192
    ## 4753                    Maxwell Food Centre  2670.16356
    ## 4754                     Newton Food Centre  4767.65862
    ## 4755 North Bridge Road Market & Food Centre  5931.51941
    ## 4756              Pasir Panjang Food Centre  3430.31757
    ## 4757           Pek Kio Market & Food Centre  5752.79196
    ## 4758              People's Park Food Centre  2667.72901
    ## 4759            Sembawang Hills Food Centre 11066.79790
    ## 4760                Serangoon Garden Market 11186.06958
    ## 4761      Taman Jurong Market & Food Centre 13123.42495
    ## 4762                    Tanglin Halt Market  4139.85273
    ## 4763                           Tekka Market  4889.22478
    ## 4764                     Tiong Bahru Market  1799.67501
    ## 4765             Zion Riverside Food Centre  2414.07181
    ## 4766              Blk 75 Toa Payoh Lorong 5  7817.63312
    ## 4767                    Blk 79 Redhill Lane  1741.00420
    ## 4768             Blk 79 Telok Blangah Drive  1607.06793
    ## 4769                    Blk 80 Circuit Road  9479.57213
    ## 4770             Blk 82 Telok Blangah Drive  1576.51796
    ## 4771           Blk 84 Marine Parade Central  9940.96232
    ## 4772            Blk 85 Bedok North Street 4 14554.99658
    ## 4773                    Blk 85 Redhill Lane  1686.06996
    ## 4774                    Blk 89 Circuit Road  9026.11270
    ## 4775                   Blk 90 Whampoa Drive  6679.81683
    ## 4776              Blk 93 Toa Payoh Lorong 4  7905.17747
    ## 4777                   Blks 13/14 Haig Road  9445.41183
    ## 4778          Blks 160/162 Ang Mo Kio Ave 4 11433.22416
    ## 4779                  Ci Yuan Hawker Centre 13244.16015
    ## 4780              Blk 409 Ang Mo Kio Ave 10 13348.29756
    ## 4781                   Blk 44 Holland Drive  5727.10173
    ## 4782                 Blk 448 Clementi Ave 3  2547.91661
    ## 4783             Blk 453A Ang Mo Kio Ave 10 13671.08832
    ## 4784                      Blk 49 Sims Place 15199.64606
    ## 4785                  Blk 4A Eunos Crescent 17958.90096
    ## 4786                      Blk 4A Jalan Batu 15829.14040
    ## 4787           Blk 4A Woodlands Centre Road 13608.90025
    ## 4788               Blk 502 West Coast Drive  2064.39041
    ## 4789               Blk 503 West Coast Drive  2126.25592
    ## 4790          Blk 505 Jurong West Street 52  4187.78727
    ## 4791                 Blk 50A Marine Terrace 19310.88425
    ## 4792                Blk 51 Old Airport Road 15968.89395
    ## 4793           Blk 511 Bedok North Street 3 20935.21549
    ## 4794 Bukit Panjang Hawker Centre and Market  7087.32946
    ## 4795         Our Tampines Hub Hawker Centre 22207.69089
    ## 4796        Kampung Admiralty Hawker Centre 14655.13255
    ## 4797                   Yishun Hawker Centre 16160.62222
    ## 4798   Jurong West Hawker Centre and Market  5542.62646
    ## 4799        Pasir Ris Central Hawker Centre 23937.56225
    ## 4800                   Dawson Hawker Centre  7349.37831
    ## 4801      Woodlands Street 12 Hawker Centre 13119.74399
    ## 4802              Blk 527 Ang Mo Kio Ave 10 13681.73867
    ## 4803           Blk 538 Bedok North Street 3 20285.54433
    ## 4804           Blk 58 New Upper Changi Road 22065.32121
    ## 4805              Blk 6 Tanjong Pagar Plaza 12184.55854
    ## 4806               Blk 628 Ang Mo Kio Ave 4 12756.00857
    ## 4807           Blk 630 Bedok Reservoir Road 19097.20606
    ## 4808                   Blk 69 Geylang Bahru 14146.56204
    ## 4809                     Blk 7 Empress Road  7001.40388
    ## 4810               Blk 724 Ang Mo Kio Ave 6 12854.56731
    ## 4811         Blk 726 Clementi West Street 2  3036.26268
    ## 4812              Blk 74 Toa Payoh Lorong 4 12236.68683
    ## 4813              Market Street Food Centre 12605.11118
    ## 4814                    Maxwell Food Centre 12206.11661
    ## 4815                     Newton Food Centre 10804.03875
    ## 4816 North Bridge Road Market & Food Centre 13568.96680
    ## 4817              Pasir Panjang Food Centre  7337.69784
    ## 4818           Pek Kio Market & Food Centre 11956.41223
    ## 4819              People's Park Food Centre 11781.49029
    ## 4820            Sembawang Hills Food Centre 11148.74727
    ## 4821                Serangoon Garden Market 14555.04620
    ## 4822      Taman Jurong Market & Food Centre  2822.35676
    ## 4823                    Tanglin Halt Market  6482.64957
    ## 4824                           Tekka Market 12088.15567
    ## 4825                     Tiong Bahru Market 10712.27550
    ## 4826             Zion Riverside Food Centre 10319.88017
    ## 4827              Blk 75 Toa Payoh Lorong 5 12362.08034
    ## 4828                    Blk 79 Redhill Lane  9161.28134
    ## 4829             Blk 79 Telok Blangah Drive  8914.47418
    ## 4830                    Blk 80 Circuit Road 16070.78142
    ## 4831             Blk 82 Telok Blangah Drive  8907.70708
    ## 4832           Blk 84 Marine Parade Central 18305.54316
    ## 4833            Blk 85 Bedok North Street 4 21829.89727
    ## 4834                    Blk 85 Redhill Lane  9178.16975
    ## 4835                    Blk 89 Circuit Road 15867.84622
    ## 4836                   Blk 90 Whampoa Drive 12494.02397
    ## 4837              Blk 93 Toa Payoh Lorong 4 12024.92513
    ## 4838                   Blks 13/14 Haig Road 17007.72183
    ## 4839          Blks 160/162 Ang Mo Kio Ave 4 12251.18201
    ## 4840                  Ci Yuan Hawker Centre 16717.48193
    ## 4841                   Blk 44 Holland Drive  9222.11609
    ## 4842                 Blk 448 Clementi Ave 3 11498.23801
    ## 4843             Blk 453A Ang Mo Kio Ave 10   614.80288
    ## 4844                      Blk 49 Sims Place  5716.77245
    ## 4845                  Blk 4A Eunos Crescent  7185.01444
    ## 4846                      Blk 4A Jalan Batu  7392.00559
    ## 4847           Blk 4A Woodlands Centre Road 12829.59220
    ## 4848               Blk 502 West Coast Drive 12094.60803
    ## 4849               Blk 503 West Coast Drive 12041.47629
    ## 4850          Blk 505 Jurong West Street 52 15306.85651
    ## 4851                 Blk 50A Marine Terrace  9213.48489
    ## 4852                Blk 51 Old Airport Road  6916.18872
    ## 4853           Blk 511 Bedok North Street 3  8986.43371
    ## 4854 Bukit Panjang Hawker Centre and Market  9368.23020
    ## 4855         Our Tampines Hub Hawker Centre  9439.41303
    ## 4856        Kampung Admiralty Hawker Centre 10479.58502
    ## 4857                   Yishun Hawker Centre  6983.91868
    ## 4858   Jurong West Hawker Centre and Market 17739.95117
    ## 4859        Pasir Ris Central Hawker Centre 10767.70969
    ## 4860                   Dawson Hawker Centre  9159.09066
    ## 4861      Woodlands Street 12 Hawker Centre 11488.74884
    ## 4862              Blk 527 Ang Mo Kio Ave 10  1110.25762
    ## 4863           Blk 538 Bedok North Street 3  8456.97549
    ## 4864           Blk 58 New Upper Changi Road 10454.47746
    ## 4865              Blk 6 Tanjong Pagar Plaza  9622.81289
    ## 4866               Blk 628 Ang Mo Kio Ave 4  2597.43365
    ## 4867           Blk 630 Bedok Reservoir Road  7305.62704
    ## 4868                   Blk 69 Geylang Bahru  4849.17014
    ## 4869                     Blk 7 Empress Road  7556.30397
    ## 4870               Blk 724 Ang Mo Kio Ave 6  1434.25124
    ## 4871         Blk 726 Clementi West Street 2 12055.85333
    ## 4872              Blk 74 Toa Payoh Lorong 4  3142.97225
    ## 4873              Market Street Food Centre  8744.79055
    ## 4874                    Maxwell Food Centre  9214.74320
    ## 4875                     Newton Food Centre  5894.30341
    ## 4876 North Bridge Road Market & Food Centre  6374.09941
    ## 4877              Pasir Panjang Food Centre 11955.28938
    ## 4878           Pek Kio Market & Food Centre  5185.49111
    ## 4879              People's Park Food Centre  8717.29458
    ## 4880            Sembawang Hills Food Centre  3121.93718
    ## 4881                Serangoon Garden Market  1265.81843
    ## 4882      Taman Jurong Market & Food Centre 15218.42192
    ## 4883                    Tanglin Halt Market  9395.01976
    ## 4884                           Tekka Market  6277.59570
    ## 4885                     Tiong Bahru Market  8963.47511
    ## 4886             Zion Riverside Food Centre  8239.28122
    ## 4887              Blk 75 Toa Payoh Lorong 5  2968.00558
    ## 4888                    Blk 79 Redhill Lane  9248.66932
    ## 4889             Blk 79 Telok Blangah Drive 11222.91375
    ## 4890                    Blk 80 Circuit Road  5234.58564
    ## 4891             Blk 82 Telok Blangah Drive 11153.09394
    ## 4892           Blk 84 Marine Parade Central  8769.49749
    ## 4893            Blk 85 Bedok North Street 4  9882.03510
    ## 4894                    Blk 85 Redhill Lane  9304.06729
    ## 4895                    Blk 89 Circuit Road  5487.51194
    ## 4896                   Blk 90 Whampoa Drive  4403.81552
    ## 4897              Blk 93 Toa Payoh Lorong 4  2764.25538
    ## 4898                   Blks 13/14 Haig Road  6912.96041
    ## 4899          Blks 160/162 Ang Mo Kio Ave 4  2216.30384
    ## 4900                  Ci Yuan Hawker Centre  3376.34293
    ## 4901                 Blk 448 Clementi Ave 3  3196.50855
    ## 4902             Blk 453A Ang Mo Kio Ave 10  9708.74352
    ## 4903                      Blk 49 Sims Place  9693.39922
    ## 4904                  Blk 4A Eunos Crescent 12480.73689
    ## 4905                      Blk 4A Jalan Batu 10165.57914
    ## 4906           Blk 4A Woodlands Centre Road 14891.34776
    ## 4907               Blk 502 West Coast Drive  3763.40294
    ## 4908               Blk 503 West Coast Drive  3697.61199
    ## 4909          Blk 505 Jurong West Street 52  9460.67843
    ## 4910                 Blk 50A Marine Terrace 13689.89872
    ## 4911                Blk 51 Old Airport Road 10356.66050
    ## 4912           Blk 511 Bedok North Street 3 15588.57489
    ## 4913 Bukit Panjang Hawker Centre and Market  8007.82128
    ## 4914         Our Tampines Hub Hawker Centre 17118.64254
    ## 4915        Kampung Admiralty Hawker Centre 14602.08693
    ## 4916                   Yishun Hawker Centre 14161.28125
    ## 4917   Jurong West Hawker Centre and Market 11228.53661
    ## 4918        Pasir Ris Central Hawker Centre 19088.40374
    ## 4919                   Dawson Hawker Centre  1774.34808
    ## 4920      Woodlands Street 12 Hawker Centre 13943.56016
    ## 4921              Blk 527 Ang Mo Kio Ave 10  9913.12962
    ## 4922           Blk 538 Bedok North Street 3 14929.23507
    ## 4923           Blk 58 New Upper Changi Road 16606.22859
    ## 4924              Blk 6 Tanjong Pagar Plaza  6602.10894
    ## 4925               Blk 628 Ang Mo Kio Ave 4  9665.29462
    ## 4926           Blk 630 Bedok Reservoir Road 13778.38075
    ## 4927                   Blk 69 Geylang Bahru  8720.76925
    ## 4928                     Blk 7 Empress Road  1690.23900
    ## 4929               Blk 724 Ang Mo Kio Ave 6  9272.62696
    ## 4930         Blk 726 Clementi West Street 2  3208.84709
    ## 4931              Blk 74 Toa Payoh Lorong 4  7210.07032
    ## 4932              Market Street Food Centre  6913.21815
    ## 4933                    Maxwell Food Centre  6568.89522
    ## 4934                     Newton Food Centre  5223.86754
    ## 4935 North Bridge Road Market & Food Centre  7918.96356
    ## 4936              Pasir Panjang Food Centre  3564.90985
    ## 4937           Pek Kio Market & Food Centre  6455.32962
    ## 4938              People's Park Food Centre  6104.31856
    ## 4939            Sembawang Hills Food Centre  8169.97796
    ## 4940                Serangoon Garden Market 10236.92764
    ## 4941      Taman Jurong Market & Food Centre  8452.84410
    ## 4942                    Tanglin Halt Market   973.77345
    ## 4943                           Tekka Market  6431.31666
    ## 4944                     Tiong Bahru Market  5087.52266
    ## 4945             Zion Riverside Food Centre  4617.47829
    ## 4946              Blk 75 Toa Payoh Lorong 5  7373.55339
    ## 4947                    Blk 79 Redhill Lane  3626.83723
    ## 4948             Blk 79 Telok Blangah Drive  4182.90031
    ## 4949                    Blk 80 Circuit Road 10725.04158
    ## 4950             Blk 82 Telok Blangah Drive  4141.80145
    ## 4951           Blk 84 Marine Parade Central 12655.16051
    ## 4952            Blk 85 Bedok North Street 4 16456.81291
    ## 4953                    Blk 85 Redhill Lane  3657.62362
    ## 4954                    Blk 89 Circuit Road 10452.20889
    ## 4955                   Blk 90 Whampoa Drive  7131.47374
    ## 4956              Blk 93 Toa Payoh Lorong 4  7151.31370
    ## 4957                   Blks 13/14 Haig Road 11470.96938
    ## 4958          Blks 160/162 Ang Mo Kio Ave 4  8976.22588
    ## 4959                  Ci Yuan Hawker Centre 12496.95179
    ## 4960             Blk 453A Ang Mo Kio Ave 10 11892.02243
    ## 4961                      Blk 49 Sims Place 12794.95655
    ## 4962                  Blk 4A Eunos Crescent 15572.47842
    ## 4963                      Blk 4A Jalan Batu 13345.45169
    ## 4964           Blk 4A Woodlands Centre Road 14120.27360
    ## 4965               Blk 502 West Coast Drive   613.91471
    ## 4966               Blk 503 West Coast Drive   553.77246
    ## 4967          Blk 505 Jurong West Street 52  6516.07991
    ## 4968                 Blk 50A Marine Terrace 16853.22852
    ## 4969                Blk 51 Old Airport Road 13513.44663
    ## 4970           Blk 511 Bedok North Street 3 18615.92241
    ## 4971 Bukit Panjang Hawker Centre and Market  7169.11967
    ## 4972         Our Tampines Hub Hawker Centre 20009.71734
    ## 4973        Kampung Admiralty Hawker Centre 14573.73216
    ## 4974                   Yishun Hawker Centre 15245.69630
    ## 4975   Jurong West Hawker Centre and Market  8084.37944
    ## 4976        Pasir Ris Central Hawker Centre 21852.15179
    ## 4977                   Dawson Hawker Centre  4802.56675
    ## 4978      Woodlands Street 12 Hawker Centre 13407.48657
    ## 4979              Blk 527 Ang Mo Kio Ave 10 11981.32824
    ## 4980           Blk 538 Bedok North Street 3 17960.63193
    ## 4981           Blk 58 New Upper Changi Road 19693.36201
    ## 4982              Blk 6 Tanjong Pagar Plaza  9646.35494
    ## 4983               Blk 628 Ang Mo Kio Ave 4 11313.67401
    ## 4984           Blk 630 Bedok Reservoir Road 16787.44120
    ## 4985                   Blk 69 Geylang Bahru 11775.38256
    ## 4986                     Blk 7 Empress Road  4588.76674
    ## 4987               Blk 724 Ang Mo Kio Ave 6 11214.29956
    ## 4988         Blk 726 Clementi West Street 2  1048.92835
    ## 4989              Blk 74 Toa Payoh Lorong 4 10016.98483
    ## 4990              Market Street Food Centre 10057.23434
    ## 4991                    Maxwell Food Centre  9661.11901
    ## 4992                     Newton Food Centre  8353.97056
    ## 4993 North Bridge Road Market & Food Centre 11091.16026
    ## 4994              Pasir Panjang Food Centre  5101.03846
    ## 4995           Pek Kio Market & Food Centre  9544.57649
    ## 4996              People's Park Food Centre  9233.67415
    ## 4997            Sembawang Hills Food Centre  9707.45555
    ## 4998                Serangoon Garden Market 12643.82593
    ## 4999      Taman Jurong Market & Food Centre  5333.92758
    ## 5000                    Tanglin Halt Market  3935.13468
    ## 5001                           Tekka Market  9605.03625
    ## 5002                     Tiong Bahru Market  8167.40460
    ## 5003             Zion Riverside Food Centre  7773.01873
    ## 5004              Blk 75 Toa Payoh Lorong 5 10158.55788
    ## 5005                    Blk 79 Redhill Lane  6623.06079
    ## 5006             Blk 79 Telok Blangah Drive  6518.22073
    ## 5007                    Blk 80 Circuit Road 13741.13899
    ## 5008             Blk 82 Telok Blangah Drive  6503.70267
    ## 5009           Blk 84 Marine Parade Central 15830.76250
    ## 5010            Blk 85 Bedok North Street 4 19499.39135
    ## 5011                    Blk 85 Redhill Lane  6641.60938
    ## 5012                    Blk 89 Circuit Road 13506.28907
    ## 5013                   Blk 90 Whampoa Drive 10142.53586
    ## 5014              Blk 93 Toa Payoh Lorong 4  9862.13186
    ## 5015                   Blks 13/14 Haig Road 14591.53607
    ## 5016          Blks 160/162 Ang Mo Kio Ave 4 10721.42345
    ## 5017                  Ci Yuan Hawker Centre 14868.43558
    ## 5018                      Blk 49 Sims Place  6211.87933
    ## 5019                  Blk 4A Eunos Crescent  7516.08956
    ## 5020                      Blk 4A Jalan Batu  7899.35518
    ## 5021           Blk 4A Woodlands Centre Road 12514.14844
    ## 5022               Blk 502 West Coast Drive 12482.15477
    ## 5023               Blk 503 West Coast Drive 12430.57401
    ## 5024          Blk 505 Jurong West Street 52 15484.28285
    ## 5025                 Blk 50A Marine Terrace  9561.56101
    ## 5026                Blk 51 Old Airport Road  7400.35732
    ## 5027           Blk 511 Bedok North Street 3  9124.39646
    ## 5028 Bukit Panjang Hawker Centre and Market  9390.01163
    ## 5029         Our Tampines Hub Hawker Centre  9413.86518
    ## 5030        Kampung Admiralty Hawker Centre 10060.04187
    ## 5031                   Yishun Hawker Centre  6409.56365
    ## 5032   Jurong West Hawker Centre and Market 17938.84429
    ## 5033        Pasir Ris Central Hawker Centre 10609.23171
    ## 5034                   Dawson Hawker Centre  9707.39212
    ## 5035      Woodlands Street 12 Hawker Centre 11168.61621
    ## 5036              Blk 527 Ang Mo Kio Ave 10   542.29887
    ## 5037           Blk 538 Bedok North Street 3  8622.94061
    ## 5038           Blk 58 New Upper Changi Road 10618.41020
    ## 5039              Blk 6 Tanjong Pagar Plaza 10237.21946
    ## 5040               Blk 628 Ang Mo Kio Ave 4  2244.79962
    ## 5041           Blk 630 Bedok Reservoir Road  7502.32599
    ## 5042                   Blk 69 Geylang Bahru  5391.04794
    ## 5043                     Blk 7 Empress Road  8056.65885
    ## 5044               Blk 724 Ang Mo Kio Ave 6  1178.44309
    ## 5045         Blk 726 Clementi West Street 2 12483.15353
    ## 5046              Blk 74 Toa Payoh Lorong 4  3756.88017
    ## 5047              Market Street Food Centre  9356.08221
    ## 5048                    Maxwell Food Centre  9828.79747
    ## 5049                     Newton Food Centre  6504.80036
    ## 5050 North Bridge Road Market & Food Centre  6958.95001
    ## 5051              Pasir Panjang Food Centre 12509.36840
    ## 5052           Pek Kio Market & Food Centre  5799.02358
    ## 5053              People's Park Food Centre  9332.03620
    ## 5054            Sembawang Hills Food Centre  3078.82529
    ## 5055                Serangoon Garden Market  1285.27153
    ## 5056      Taman Jurong Market & Food Centre 15459.71989
    ## 5057                    Tanglin Halt Market  9916.71594
    ## 5058                           Tekka Market  6889.99128
    ## 5059                     Tiong Bahru Market  9574.72165
    ## 5060             Zion Riverside Food Centre  8847.30373
    ## 5061              Blk 75 Toa Payoh Lorong 5  3580.86108
    ## 5062                    Blk 79 Redhill Lane  9840.61901
    ## 5063             Blk 79 Telok Blangah Drive 11809.08269
    ## 5064                    Blk 80 Circuit Road  5628.39289
    ## 5065             Blk 82 Telok Blangah Drive 11739.31235
    ## 5066           Blk 84 Marine Parade Central  9172.53295
    ## 5067            Blk 85 Bedok North Street 4 10006.19737
    ## 5068                    Blk 85 Redhill Lane  9896.34645
    ## 5069                    Blk 89 Circuit Road  5917.84066
    ## 5070                   Blk 90 Whampoa Drive  5010.57979
    ## 5071              Blk 93 Toa Payoh Lorong 4  3378.17183
    ## 5072                   Blks 13/14 Haig Road  7318.53548
    ## 5073          Blks 160/162 Ang Mo Kio Ave 4  2028.45841
    ## 5074                  Ci Yuan Hawker Centre  3067.96939
    ## 5075                  Blk 4A Eunos Crescent  2788.11297
    ## 5076                      Blk 4A Jalan Batu  1697.88076
    ## 5077           Blk 4A Woodlands Centre Road 18311.16644
    ## 5078               Blk 502 West Coast Drive 13395.09294
    ## 5079               Blk 503 West Coast Drive 13330.43965
    ## 5080          Blk 505 Jurong West Street 52 18272.41325
    ## 5081                 Blk 50A Marine Terrace  4233.71053
    ## 5082                Blk 51 Old Airport Road  1209.78489
    ## 5083           Blk 511 Bedok North Street 3  5970.17740
    ## 5084 Bukit Panjang Hawker Centre and Market 13652.28297
    ## 5085         Our Tampines Hub Hawker Centre  7847.73213
    ## 5086        Kampung Admiralty Hawker Centre 16164.01571
    ## 5087                   Yishun Hawker Centre 12542.98442
    ## 5088   Jurong West Hawker Centre and Market 20431.81110
    ## 5089        Pasir Ris Central Hawker Centre 10161.46609
    ## 5090                   Dawson Hawker Centre  8591.40102
    ## 5091      Woodlands Street 12 Hawker Centre 16992.89552
    ## 5092              Blk 527 Ang Mo Kio Ave 10  6753.91555
    ## 5093           Blk 538 Bedok North Street 3  5310.40321
    ## 5094           Blk 58 New Upper Changi Road  6913.09436
    ## 5095              Blk 6 Tanjong Pagar Plaza  6024.92443
    ## 5096               Blk 628 Ang Mo Kio Ave 4  8280.82296
    ## 5097           Blk 630 Bedok Reservoir Road  4244.85781
    ## 5098                   Blk 69 Geylang Bahru  1154.02358
    ## 5099                     Blk 7 Empress Road  8211.59259
    ## 5100               Blk 724 Ang Mo Kio Ave 6  7109.41844
    ## 5101         Blk 726 Clementi West Street 2 12899.52739
    ## 5102              Blk 74 Toa Payoh Lorong 4  3611.10242
    ## 5103              Market Street Food Centre  4918.05896
    ## 5104                    Maxwell Food Centre  5615.22561
    ## 5105                     Newton Food Centre  4472.39243
    ## 5106 North Bridge Road Market & Food Centre  2133.50570
    ## 5107              Pasir Panjang Food Centre 10801.68163
    ## 5108           Pek Kio Market & Food Centre  3250.38543
    ## 5109              People's Park Food Centre  5422.24136
    ## 5110            Sembawang Hills Food Centre  8299.50909
    ## 5111                Serangoon Garden Market  5286.41413
    ## 5112      Taman Jurong Market & Food Centre 17678.66156
    ## 5113                    Tanglin Halt Market  9279.20016
    ## 5114                           Tekka Market  3432.38460
    ## 5115                     Tiong Bahru Market  6321.06739
    ## 5116             Zion Riverside Food Centre  6024.30864
    ## 5117              Blk 75 Toa Payoh Lorong 5  3615.90826
    ## 5118                    Blk 79 Redhill Lane  7520.10288
    ## 5119             Blk 79 Telok Blangah Drive  9338.06840
    ## 5120                    Blk 80 Circuit Road  1467.56829
    ## 5121             Blk 82 Telok Blangah Drive  9276.74429
    ## 5122           Blk 84 Marine Parade Central  3413.82431
    ## 5123            Blk 85 Bedok North Street 4  6803.82807
    ## 5124                    Blk 85 Redhill Lane  7551.72550
    ## 5125                    Blk 89 Circuit Road   972.38951
    ## 5126                   Blk 90 Whampoa Drive  2778.65113
    ## 5127              Blk 93 Toa Payoh Lorong 4  4082.91663
    ## 5128                   Blks 13/14 Haig Road  1814.80522
    ## 5129          Blks 160/162 Ang Mo Kio Ave 4  7765.96897
    ## 5130                  Ci Yuan Hawker Centre  6463.72232
    ## 5131                      Blk 4A Jalan Batu  3006.23768
    ## 5132           Blk 4A Woodlands Centre Road 20012.99501
    ## 5133               Blk 502 West Coast Drive 16174.60861
    ## 5134               Blk 503 West Coast Drive 16110.14614
    ## 5135          Blk 505 Jurong West Street 52 20931.51742
    ## 5136                 Blk 50A Marine Terrace  2056.47982
    ## 5137                Blk 51 Old Airport Road  2447.47925
    ## 5138           Blk 511 Bedok North Street 3  3260.83481
    ## 5139 Bukit Panjang Hawker Centre and Market 15973.51490
    ## 5140         Our Tampines Hub Hawker Centre  5415.21536
    ## 5141        Kampung Admiralty Hawker Centre 17542.31112
    ## 5142                   Yishun Hawker Centre 13336.44503
    ## 5143   Jurong West Hawker Centre and Market 23137.03674
    ## 5144        Pasir Ris Central Hawker Centre  7884.57376
    ## 5145                   Dawson Hawker Centre 11362.87413
    ## 5146      Woodlands Street 12 Hawker Centre 18670.62616
    ## 5147              Blk 527 Ang Mo Kio Ave 10  8019.32875
    ## 5148           Blk 538 Bedok North Street 3  2617.29336
    ## 5149           Blk 58 New Upper Changi Road  4125.52827
    ## 5150              Blk 6 Tanjong Pagar Plaza  8342.48054
    ## 5151               Blk 628 Ang Mo Kio Ave 4  9752.08578
    ## 5152           Blk 630 Bedok Reservoir Road  1788.75697
    ## 5153                   Blk 69 Geylang Bahru  3814.45606
    ## 5154                     Blk 7 Empress Road 10985.23916
    ## 5155               Blk 724 Ang Mo Kio Ave 6  8613.03144
    ## 5156         Blk 726 Clementi West Street 2 15687.38416
    ## 5157              Blk 74 Toa Payoh Lorong 4  6024.89050
    ## 5158              Market Street Food Centre  7258.48294
    ## 5159                    Maxwell Food Centre  7970.44673
    ## 5160                     Newton Food Centre  7260.50518
    ## 5161 North Bridge Road Market & Food Centre  4772.77518
    ## 5162              Pasir Panjang Food Centre 13486.52500
    ## 5163           Pek Kio Market & Food Centre  6031.32525
    ## 5164              People's Park Food Centre  7899.01844
    ## 5165            Sembawang Hills Food Centre 10163.27697
    ## 5166                Serangoon Garden Market  6313.68737
    ## 5167      Taman Jurong Market & Food Centre 20398.23751
    ## 5168                    Tanglin Halt Market 12062.61662
    ## 5169                           Tekka Market  6180.26770
    ## 5170                     Tiong Bahru Market  8901.55178
    ## 5171             Zion Riverside Food Centre  8701.22166
    ## 5172              Blk 75 Toa Payoh Lorong 5  5969.55983
    ## 5173                    Blk 79 Redhill Lane 10207.65714
    ## 5174             Blk 79 Telok Blangah Drive 11942.64220
    ## 5175                    Blk 80 Circuit Road  2079.99312
    ## 5176             Blk 82 Telok Blangah Drive 11884.73397
    ## 5177           Blk 84 Marine Parade Central  2008.85807
    ## 5178            Blk 85 Bedok North Street 4  4047.04707
    ## 5179                    Blk 85 Redhill Lane 10234.93910
    ## 5180                    Blk 89 Circuit Road  2121.66925
    ## 5181                   Blk 90 Whampoa Drive  5474.54432
    ## 5182              Blk 93 Toa Payoh Lorong 4  6414.60661
    ## 5183                   Blks 13/14 Haig Road  1120.85775
    ## 5184          Blks 160/162 Ang Mo Kio Ave 4  9394.89666
    ## 5185                  Ci Yuan Hawker Centre  6536.58773
    ## 5186           Blk 4A Woodlands Centre Road 19869.87563
    ## 5187               Blk 502 West Coast Drive 13925.67833
    ## 5188               Blk 503 West Coast Drive 13860.01233
    ## 5189          Blk 505 Jurong West Street 52 19143.62648
    ## 5190                 Blk 50A Marine Terrace  3560.82304
    ## 5191                Blk 51 Old Airport Road   678.47941
    ## 5192           Blk 511 Bedok North Street 3  6211.81162
    ## 5193 Bukit Panjang Hawker Centre and Market 14931.55423
    ## 5194         Our Tampines Hub Hawker Centre  8420.27985
    ## 5195        Kampung Admiralty Hawker Centre 17805.42317
    ## 5196                   Yishun Hawker Centre 14240.76391
    ## 5197   Jurong West Hawker Centre and Market 21199.26449
    ## 5198        Pasir Ris Central Hawker Centre 10874.93649
    ## 5199                   Dawson Hawker Centre  8833.92968
    ## 5200      Woodlands Street 12 Hawker Centre 18562.50819
    ## 5201              Blk 527 Ang Mo Kio Ave 10  8441.64592
    ## 5202           Blk 538 Bedok North Street 3  5591.40006
    ## 5203           Blk 58 New Upper Changi Road  6804.86612
    ## 5204              Blk 6 Tanjong Pagar Plaza  5360.27130
    ## 5205               Blk 628 Ang Mo Kio Ave 4  9936.02938
    ## 5206           Blk 630 Bedok Reservoir Road  4773.40261
    ## 5207                   Blk 69 Geylang Bahru  2615.25791
    ## 5208                     Blk 7 Empress Road  8847.62540
    ## 5209               Blk 724 Ang Mo Kio Ave 6  8767.94774
    ## 5210         Blk 726 Clementi West Street 2 13319.72335
    ## 5211              Blk 74 Toa Payoh Lorong 4  5025.74047
    ## 5212              Market Street Food Centre  4295.56592
    ## 5213                    Maxwell Food Centre  5003.16693
    ## 5214                     Newton Food Centre  5048.71782
    ## 5215 North Bridge Road Market & Food Centre  2261.81025
    ## 5216              Pasir Panjang Food Centre 10707.77764
    ## 5217           Pek Kio Market & Food Centre  4048.18351
    ## 5218              People's Park Food Centre  4990.30930
    ## 5219            Sembawang Hills Food Centre  9861.33724
    ## 5220                Serangoon Garden Market  6983.68424
    ## 5221      Taman Jurong Market & Food Centre 18422.54625
    ## 5222                    Tanglin Halt Market  9607.75012
    ## 5223                           Tekka Market  3741.69025
    ## 5224                     Tiong Bahru Market  6053.06779
    ## 5225             Zion Riverside Food Centre  5975.55074
    ## 5226              Blk 75 Toa Payoh Lorong 5  5071.66083
    ## 5227                    Blk 79 Redhill Lane  7469.39039
    ## 5228             Blk 79 Telok Blangah Drive  9080.24984
    ## 5229                    Blk 80 Circuit Road  2834.29598
    ## 5230             Blk 82 Telok Blangah Drive  9026.04710
    ## 5231           Blk 84 Marine Parade Central  2492.73356
    ## 5232            Blk 85 Bedok North Street 4  6919.04106
    ## 5233                    Blk 85 Redhill Lane  7489.08811
    ## 5234                    Blk 89 Circuit Road  2331.05098
    ## 5235                   Blk 90 Whampoa Drive  3927.54694
    ## 5236              Blk 93 Toa Payoh Lorong 4  5529.45041
    ## 5237                   Blks 13/14 Haig Road  1914.61636
    ## 5238          Blks 160/162 Ang Mo Kio Ave 4  9395.94129
    ## 5239                  Ci Yuan Hawker Centre  8073.80290
    ## 5240               Blk 502 West Coast Drive 14310.39195
    ## 5241               Blk 503 West Coast Drive 14314.56461
    ## 5242          Blk 505 Jurong West Street 52 11606.78216
    ## 5243                 Blk 50A Marine Terrace 22042.46398
    ## 5244                Blk 51 Old Airport Road 19515.79136
    ## 5245           Blk 511 Bedok North Street 3 21462.46787
    ## 5246 Bukit Panjang Hawker Centre and Market  6997.82096
    ## 5247         Our Tampines Hub Hawker Centre 21187.18213
    ## 5248        Kampung Admiralty Hawker Centre  3407.78911
    ## 5249                   Yishun Hawker Centre  8490.75856
    ## 5250   Jurong West Hawker Centre and Market 13662.78032
    ## 5251        Pasir Ris Central Hawker Centre 21525.12423
    ## 5252                   Dawson Hawker Centre 16313.11513
    ## 5253      Woodlands Street 12 Hawker Centre  1347.74502
    ## 5254              Blk 527 Ang Mo Kio Ave 10 12035.94324
    ## 5255           Blk 538 Bedok North Street 3 21024.65849
    ## 5256           Blk 58 New Upper Changi Road 22993.82340
    ## 5257              Blk 6 Tanjong Pagar Plaza 19897.26989
    ## 5258               Blk 628 Ang Mo Kio Ave 4 10269.66649
    ## 5259           Blk 630 Bedok Reservoir Road 19961.56225
    ## 5260                   Blk 69 Geylang Bahru 17259.44278
    ## 5261                     Blk 7 Empress Road 14333.78092
    ## 5262               Blk 724 Ang Mo Kio Ave 6 11399.96357
    ## 5263         Blk 726 Clementi West Street 2 15169.04638
    ## 5264              Blk 74 Toa Payoh Lorong 4 14873.95244
    ## 5265              Market Street Food Centre 19504.44416
    ## 5266                    Maxwell Food Centre 19622.57273
    ## 5267                     Newton Food Centre 16219.78032
    ## 5268 North Bridge Road Market & Food Centre 18222.43114
    ## 5269              Pasir Panjang Food Centre 18391.03318
    ## 5270           Pek Kio Market & Food Centre 16419.62241
    ## 5271              People's Park Food Centre 19027.86358
    ## 5272            Sembawang Hills Food Centre 10013.53609
    ## 5273                Serangoon Garden Market 13764.90101
    ## 5274      Taman Jurong Market & Food Centre 12932.32057
    ## 5275                    Tanglin Halt Market 15786.44241
    ## 5276                           Tekka Market 17371.61288
    ## 5277                     Tiong Bahru Market 18563.84005
    ## 5278             Zion Riverside Food Centre 17772.44865
    ## 5279              Blk 75 Toa Payoh Lorong 5 14809.12293
    ## 5280                    Blk 79 Redhill Lane 17749.96443
    ## 5281             Blk 79 Telok Blangah Drive 18983.57951
    ## 5282                    Blk 80 Circuit Road 18048.63774
    ## 5283             Blk 82 Telok Blangah Drive 18932.55494
    ## 5284           Blk 84 Marine Parade Central 21555.58265
    ## 5285            Blk 85 Bedok North Street 4 22296.06776
    ## 5286                    Blk 85 Redhill Lane 17803.40417
    ## 5287                    Blk 89 Circuit Road 18257.23156
    ## 5288                   Blk 90 Whampoa Drive 16110.11209
    ## 5289              Blk 93 Toa Payoh Lorong 4 14361.41005
    ## 5290                   Blks 13/14 Haig Road 19705.77753
    ## 5291          Blks 160/162 Ang Mo Kio Ave 4 10630.52591
    ## 5292                  Ci Yuan Hawker Centre 14496.43569
    ## 5293               Blk 503 West Coast Drive    65.83225
    ## 5294          Blk 505 Jurong West Street 52  6161.28014
    ## 5295                 Blk 50A Marine Terrace 17440.76921
    ## 5296                Blk 51 Old Airport Road 14102.60063
    ## 5297           Blk 511 Bedok North Street 3 19224.72177
    ## 5298 Bukit Panjang Hawker Centre and Market  7415.72031
    ## 5299         Our Tampines Hub Hawker Centre 20623.46074
    ## 5300        Kampung Admiralty Hawker Centre 14893.63663
    ## 5301                   Yishun Hawker Centre 15722.21587
    ## 5302   Jurong West Hawker Centre and Market  7602.07384
    ## 5303        Pasir Ris Central Hawker Centre 22465.02300
    ## 5304                   Dawson Hawker Centre  5313.52914
    ## 5305      Woodlands Street 12 Hawker Centre 13645.66351
    ## 5306              Blk 527 Ang Mo Kio Ave 10 12563.48387
    ## 5307           Blk 538 Bedok North Street 3 18569.06356
    ## 5308           Blk 58 New Upper Changi Road 20296.68525
    ## 5309              Blk 6 Tanjong Pagar Plaza 10129.65808
    ## 5310               Blk 628 Ang Mo Kio Ave 4 11863.33366
    ## 5311           Blk 630 Bedok Reservoir Road 17397.17809
    ## 5312                   Blk 69 Geylang Bahru 12380.56270
    ## 5313                     Blk 7 Empress Road  5194.08167
    ## 5314               Blk 724 Ang Mo Kio Ave 6 11788.80395
    ## 5315         Blk 726 Clementi West Street 2  1060.97821
    ## 5316              Blk 74 Toa Payoh Lorong 4 10630.84981
    ## 5317              Market Street Food Centre 10576.29653
    ## 5318                    Maxwell Food Centre 10160.34027
    ## 5319                     Newton Food Centre  8947.38000
    ## 5320 North Bridge Road Market & Food Centre 11674.56325
    ## 5321              Pasir Panjang Food Centre  5357.37957
    ## 5322           Pek Kio Market & Food Centre 10144.98875
    ## 5323              People's Park Food Centre  9747.93879
    ## 5324            Sembawang Hills Food Centre 10253.60030
    ## 5325                Serangoon Garden Market 13245.91949
    ## 5326      Taman Jurong Market & Food Centre  4883.99092
    ## 5327                    Tanglin Halt Market  4452.72346
    ## 5328                           Tekka Market 10187.46246
    ## 5329                     Tiong Bahru Market  8667.00584
    ## 5330             Zion Riverside Food Centre  8301.84989
    ## 5331              Blk 75 Toa Payoh Lorong 5 10772.47258
    ## 5332                    Blk 79 Redhill Lane  7107.89877
    ## 5333             Blk 79 Telok Blangah Drive  6872.45719
    ## 5334                    Blk 80 Circuit Road 14349.89172
    ## 5335             Blk 82 Telok Blangah Drive  6863.08164
    ## 5336           Blk 84 Marine Parade Central 16413.32555
    ## 5337            Blk 85 Bedok North Street 4 20107.15481
    ## 5338                    Blk 85 Redhill Lane  7123.34898
    ## 5339                    Blk 89 Circuit Road 14112.04936
    ## 5340                   Blk 90 Whampoa Drive 10750.41080
    ## 5341              Blk 93 Toa Payoh Lorong 4 10475.66605
    ## 5342                   Blks 13/14 Haig Road 15189.10700
    ## 5343          Blks 160/162 Ang Mo Kio Ave 4 11281.59074
    ## 5344                  Ci Yuan Hawker Centre 15466.77233
    ## 5345          Blk 505 Jurong West Street 52  6215.78933
    ## 5346                 Blk 50A Marine Terrace 17375.31785
    ## 5347                Blk 51 Old Airport Road 14037.22567
    ## 5348           Blk 511 Bedok North Street 3 19161.27110
    ## 5349 Bukit Panjang Hawker Centre and Market  7412.38367
    ## 5350         Our Tampines Hub Hawker Centre 20562.17365
    ## 5351        Kampung Admiralty Hawker Centre 14882.68197
    ## 5352                   Yishun Hawker Centre 15690.49669
    ## 5353   Jurong West Hawker Centre and Market  7665.08161
    ## 5354        Pasir Ris Central Hawker Centre 22405.87716
    ## 5355                   Dawson Hawker Centre  5248.52982
    ## 5356      Woodlands Street 12 Hawker Centre 13644.32183
    ## 5357              Blk 527 Ang Mo Kio Ave 10 12513.70466
    ## 5358           Blk 538 Bedok North Street 3 18505.53609
    ## 5359           Blk 58 New Upper Changi Road 20232.34901
    ## 5360              Blk 6 Tanjong Pagar Plaza 10065.87074
    ## 5361               Blk 628 Ang Mo Kio Ave 4 11819.81080
    ## 5362           Blk 630 Bedok Reservoir Road 17333.95212
    ## 5363                   Blk 69 Geylang Bahru 12316.50024
    ## 5364                     Blk 7 Empress Road  5130.14499
    ## 5365               Blk 724 Ang Mo Kio Ave 6 11740.60495
    ## 5366         Blk 726 Clementi West Street 2  1019.26816
    ## 5367              Blk 74 Toa Payoh Lorong 4 10569.84311
    ## 5368              Market Street Food Centre 10511.21960
    ## 5369                    Maxwell Food Centre 10095.92176
    ## 5370                     Newton Food Centre  8882.25536
    ## 5371 North Bridge Road Market & Food Centre 11608.98803
    ## 5372              Pasir Panjang Food Centre  5306.80889
    ## 5373           Pek Kio Market & Food Centre 10080.38342
    ## 5374              People's Park Food Centre  9682.99461
    ## 5375            Sembawang Hills Food Centre 10210.61562
    ## 5376                Serangoon Garden Market 13191.21009
    ## 5377      Taman Jurong Market & Food Centre  4944.85372
    ## 5378                    Tanglin Halt Market  4387.48219
    ## 5379                           Tekka Market 10121.86634
    ## 5380                     Tiong Bahru Market  8602.53293
    ## 5381             Zion Riverside Food Centre  8236.47298
    ## 5382              Blk 75 Toa Payoh Lorong 5 10711.78234
    ## 5383                    Blk 79 Redhill Lane  7043.92342
    ## 5384             Blk 79 Telok Blangah Drive  6815.48141
    ## 5385                    Blk 80 Circuit Road 14286.44618
    ## 5386             Blk 82 Telok Blangah Drive  6805.77503
    ## 5387           Blk 84 Marine Parade Central 16347.71239
    ## 5388            Blk 85 Bedok North Street 4 20043.49080
    ## 5389                    Blk 85 Redhill Lane  7059.49985
    ## 5390                    Blk 89 Circuit Road 14048.06362
    ## 5391                   Blk 90 Whampoa Drive 10686.80662
    ## 5392              Blk 93 Toa Payoh Lorong 4 10415.88000
    ## 5393                   Blks 13/14 Haig Road 15124.22638
    ## 5394          Blks 160/162 Ang Mo Kio Ave 4 11236.14483
    ## 5395                  Ci Yuan Hawker Centre 15413.15310
    ## 5396                 Blk 50A Marine Terrace 22488.44361
    ## 5397                Blk 51 Old Airport Road 19182.31193
    ## 5398           Blk 511 Bedok North Street 3 23680.03130
    ## 5399 Bukit Panjang Hawker Centre and Market  6762.28342
    ## 5400         Our Tampines Hub Hawker Centre 24630.24961
    ## 5401        Kampung Admiralty Hawker Centre 13544.71787
    ## 5402                   Yishun Hawker Centre 16340.90464
    ## 5403   Jurong West Hawker Centre and Market  2518.65120
    ## 5404        Pasir Ris Central Hawker Centre 26073.87179
    ## 5405                   Dawson Hawker Centre 11199.77175
    ## 5406      Woodlands Street 12 Hawker Centre 11517.60940
    ## 5407              Blk 527 Ang Mo Kio Ave 10 15353.61785
    ## 5408           Blk 538 Bedok North Street 3 23054.48940
    ## 5409           Blk 58 New Upper Changi Road 24941.79485
    ## 5410              Blk 6 Tanjong Pagar Plaza 16058.11235
    ## 5411               Blk 628 Ang Mo Kio Ave 4 14038.91471
    ## 5412           Blk 630 Bedok Reservoir Road 21845.04885
    ## 5413                   Blk 69 Geylang Bahru 17152.10435
    ## 5414                     Blk 7 Empress Road 10382.16419
    ## 5415               Blk 724 Ang Mo Kio Ave 6 14469.45198
    ## 5416         Blk 726 Clementi West Street 2  7191.27420
    ## 5417              Blk 74 Toa Payoh Lorong 4 14956.27128
    ## 5418              Market Street Food Centre 16350.41906
    ## 5419                    Maxwell Food Centre 16029.08552
    ## 5420                     Newton Food Centre 14110.41742
    ## 5421 North Bridge Road Market & Food Centre 16897.86450
    ## 5422              Pasir Panjang Food Centre 11506.37155
    ## 5423           Pek Kio Market & Food Centre 15125.58121
    ## 5424              People's Park Food Centre 15554.69904
    ## 5425            Sembawang Hills Food Centre 12556.95107
    ## 5426                Serangoon Garden Market 16570.49888
    ## 5427      Taman Jurong Market & Food Centre  1699.22969
    ## 5428                    Tanglin Halt Market 10335.73861
    ## 5429                           Tekka Market 15464.32139
    ## 5430                     Tiong Bahru Market 14547.69735
    ## 5431             Zion Riverside Food Centre 14056.05799
    ## 5432              Blk 75 Toa Payoh Lorong 5 15043.12256
    ## 5433                    Blk 79 Redhill Lane 13054.39472
    ## 5434             Blk 79 Telok Blangah Drive 13024.36949
    ## 5435                    Blk 80 Circuit Road 18926.43091
    ## 5436             Blk 82 Telok Blangah Drive 13012.35013
    ## 5437           Blk 84 Marine Parade Central 21556.08562
    ## 5438            Blk 85 Bedok North Street 4 24592.97133
    ## 5439                    Blk 85 Redhill Lane 13078.97897
    ## 5440                    Blk 89 Circuit Road 18809.91435
    ## 5441                   Blk 90 Whampoa Drive 15496.00432
    ## 5442              Blk 93 Toa Payoh Lorong 4 14638.39174
    ## 5443                   Blks 13/14 Haig Road 20081.96388
    ## 5444          Blks 160/162 Ang Mo Kio Ave 4 13717.87562
    ## 5445                  Ci Yuan Hawker Centre 18530.99580
    ## 5446                Blk 51 Old Airport Road  3342.02976
    ## 5447           Blk 511 Bedok North Street 3  3457.57428
    ## 5448 Bukit Panjang Hawker Centre and Market 17813.17316
    ## 5449         Our Tampines Hub Hawker Centre  5948.57581
    ## 5450        Kampung Admiralty Hawker Centre 19596.92676
    ## 5451                   Yishun Hawker Centre 15372.33376
    ## 5452   Jurong West Hawker Centre and Market 24615.77016
    ## 5453        Pasir Ris Central Hawker Centre  8475.14443
    ## 5454                   Dawson Hawker Centre 12392.44477
    ## 5455      Woodlands Street 12 Hawker Centre 20702.19481
    ## 5456              Blk 527 Ang Mo Kio Ave 10 10069.06779
    ## 5457           Blk 538 Bedok North Street 3  3048.09355
    ## 5458           Blk 58 New Upper Changi Road  3480.99209
    ## 5459              Blk 6 Tanjong Pagar Plaza  8698.87099
    ## 5460               Blk 628 Ang Mo Kio Ave 4 11791.93682
    ## 5461           Blk 630 Bedok Reservoir Road  3033.95710
    ## 5462                   Blk 69 Geylang Bahru  5379.69919
    ## 5463                     Blk 7 Empress Road 12310.50218
    ## 5464               Blk 724 Ang Mo Kio Ave 6 10645.46001
    ## 5465         Blk 726 Clementi West Street 2 16861.73101
    ## 5466              Blk 74 Toa Payoh Lorong 4  7776.76790
    ## 5467              Market Street Food Centre  7706.20079
    ## 5468                    Maxwell Food Centre  8388.87337
    ## 5469                     Newton Food Centre  8507.20145
    ## 5470 North Bridge Road Market & Food Centre  5772.76023
    ## 5471              Pasir Panjang Food Centre 14228.71501
    ## 5472           Pek Kio Market & Food Centre  7383.47646
    ## 5473              People's Park Food Centre  8462.68780
    ## 5474            Sembawang Hills Food Centre 12144.68187
    ## 5475                Serangoon Garden Market  8368.15946
    ## 5476      Taman Jurong Market & Food Centre 21850.77705
    ## 5477                    Tanglin Halt Market 13159.03892
    ## 5478                           Tekka Market  7259.76526
    ## 5479                     Tiong Bahru Market  9560.82793
    ## 5480             Zion Riverside Food Centre  9528.66549
    ## 5481              Blk 75 Toa Payoh Lorong 5  7750.90773
    ## 5482                    Blk 79 Redhill Lane 11014.87198
    ## 5483             Blk 79 Telok Blangah Drive 12556.68445
    ## 5484                    Blk 80 Circuit Road  4014.03145
    ## 5485             Blk 82 Telok Blangah Drive 12505.77705
    ## 5486           Blk 84 Marine Parade Central  1117.49018
    ## 5487            Blk 85 Bedok North Street 4  3864.69352
    ## 5488                    Blk 85 Redhill Lane 11032.47762
    ## 5489                    Blk 89 Circuit Road  3898.49079
    ## 5490                   Blk 90 Whampoa Drive  7008.87739
    ## 5491              Blk 93 Toa Payoh Lorong 4  8211.68965
    ## 5492                   Blks 13/14 Haig Road  2470.39070
    ## 5493          Blks 160/162 Ang Mo Kio Ave 4 11415.01137
    ## 5494                  Ci Yuan Hawker Centre  8522.43420
    ## 5495           Blk 511 Bedok North Street 3  5697.56790
    ## 5496 Bukit Panjang Hawker Centre and Market 14761.87614
    ## 5497         Our Tampines Hub Hawker Centre  7836.36053
    ## 5498        Kampung Admiralty Hawker Centre 17371.87008
    ## 5499                   Yishun Hawker Centre 13697.10000
    ## 5500   Jurong West Hawker Centre and Market 21285.52236
    ## 5501        Pasir Ris Central Hawker Centre 10267.71923
    ## 5502                   Dawson Hawker Centre  9105.84579
    ## 5503      Woodlands Street 12 Hawker Centre 18199.11289
    ## 5504              Blk 527 Ang Mo Kio Ave 10  7941.56278
    ## 5505           Blk 538 Bedok North Street 3  5061.45733
    ## 5506           Blk 58 New Upper Changi Road  6401.57623
    ## 5507              Blk 6 Tanjong Pagar Plaza  5897.87297
    ## 5508               Blk 628 Ang Mo Kio Ave 4  9486.89902
    ## 5509           Blk 630 Bedok Reservoir Road  4177.23890
    ## 5510                   Blk 69 Geylang Bahru  2288.46376
    ## 5511                     Blk 7 Empress Road  8968.81250
    ## 5512               Blk 724 Ang Mo Kio Ave 6  8314.98225
    ## 5513         Blk 726 Clementi West Street 2 13538.51213
    ## 5514              Blk 74 Toa Payoh Lorong 4  4757.43616
    ## 5515              Market Street Food Centre  4811.25403
    ## 5516                    Maxwell Food Centre  5523.34481
    ## 5517                     Newton Food Centre  5165.18786
    ## 5518 North Bridge Road Market & Food Centre  2457.39012
    ## 5519              Pasir Panjang Food Centre 11101.19153
    ## 5520           Pek Kio Market & Food Centre  4058.18327
    ## 5521              People's Park Food Centre  5457.87621
    ## 5522            Sembawang Hills Food Centre  9502.77893
    ## 5523                Serangoon Garden Market  6432.37668
    ## 5524      Taman Jurong Market & Food Centre 18516.90928
    ## 5525                    Tanglin Halt Market  9851.29990
    ## 5526                           Tekka Market  3935.31966
    ## 5527                     Tiong Bahru Market  6480.01349
    ## 5528             Zion Riverside Food Centre  6330.70799
    ## 5529              Blk 75 Toa Payoh Lorong 5  4779.12941
    ## 5530                    Blk 79 Redhill Lane  7835.90860
    ## 5531             Blk 79 Telok Blangah Drive  9520.11022
    ## 5532                    Blk 80 Circuit Road  2171.83632
    ## 5533             Blk 82 Telok Blangah Drive  9463.53164
    ## 5534           Blk 84 Marine Parade Central  2375.50450
    ## 5535            Blk 85 Bedok North Street 4  6445.71235
    ## 5536                    Blk 85 Redhill Lane  7859.66370
    ## 5537                    Blk 89 Circuit Road  1680.58422
    ## 5538                   Blk 90 Whampoa Drive  3782.70826
    ## 5539              Blk 93 Toa Payoh Lorong 4  5244.65666
    ## 5540                   Blks 13/14 Haig Road  1328.26007
    ## 5541          Blks 160/162 Ang Mo Kio Ave 4  8975.59560
    ## 5542                  Ci Yuan Hawker Centre  7434.92976
    ## 5543 Bukit Panjang Hawker Centre and Market 18266.83871
    ## 5544         Our Tampines Hub Hawker Centre  2495.51857
    ## 5545        Kampung Admiralty Hawker Centre 18659.20639
    ## 5546                   Yishun Hawker Centre 13937.83909
    ## 5547   Jurong West Hawker Centre and Market 25967.58922
    ## 5548        Pasir Ris Central Hawker Centre  5017.80041
    ## 5549                   Dawson Hawker Centre 14557.33961
    ## 5550      Woodlands Street 12 Hawker Centre 20116.87855
    ## 5551              Blk 527 Ang Mo Kio Ave 10  9532.12358
    ## 5552           Blk 538 Bedok North Street 3   660.95560
    ## 5553           Blk 58 New Upper Changi Road  1541.63845
    ## 5554              Blk 6 Tanjong Pagar Plaza 11571.53202
    ## 5555               Blk 628 Ang Mo Kio Ave 4 11316.01890
    ## 5556           Blk 630 Bedok Reservoir Road  1841.67549
    ## 5557                   Blk 69 Geylang Bahru  6868.64245
    ## 5558                     Blk 7 Empress Road 14033.90771
    ## 5559               Blk 724 Ang Mo Kio Ave 6 10301.11236
    ## 5560         Blk 726 Clementi West Street 2 18796.28245
    ## 5561              Blk 74 Toa Payoh Lorong 4  8748.88103
    ## 5562              Market Street Food Centre 10498.62747
    ## 5563                    Maxwell Food Centre 11209.06045
    ## 5564                     Newton Food Centre 10403.29082
    ## 5565 North Bridge Road Market & Food Centre  8022.08808
    ## 5566              Pasir Panjang Food Centre 16738.85025
    ## 5567           Pek Kio Market & Food Centre  9142.51035
    ## 5568              People's Park Food Centre 11155.09765
    ## 5569            Sembawang Hills Food Centre 12107.69194
    ## 5570                Serangoon Garden Market  7839.28652
    ## 5571      Taman Jurong Market & Food Centre 23269.15447
    ## 5572                    Tanglin Halt Market 15227.94588
    ## 5573                           Tekka Market  9399.38639
    ## 5574                     Tiong Bahru Market 12162.38656
    ## 5575             Zion Riverside Food Centre 11952.52462
    ## 5576              Blk 75 Toa Payoh Lorong 5  8648.32696
    ## 5577                    Blk 79 Redhill Lane 13457.61081
    ## 5578             Blk 79 Telok Blangah Drive 15203.46155
    ## 5579                    Blk 80 Circuit Road  4874.83058
    ## 5580             Blk 82 Telok Blangah Drive 15145.51040
    ## 5581           Blk 84 Marine Parade Central  4360.29815
    ## 5582            Blk 85 Bedok North Street 4   915.48461
    ## 5583                    Blk 85 Redhill Lane 13486.03423
    ## 5584                    Blk 89 Circuit Road  5140.59976
    ## 5585                   Blk 90 Whampoa Drive  8475.34881
    ## 5586              Blk 93 Toa Payoh Lorong 4  9042.81676
    ## 5587                   Blks 13/14 Haig Road  4378.73767
    ## 5588          Blks 160/162 Ang Mo Kio Ave 4 11146.08428
    ## 5589                  Ci Yuan Hawker Centre  7060.20322
    ## 5590         Our Tampines Hub Hawker Centre 18795.63420
    ## 5591        Kampung Admiralty Hawker Centre  7573.54623
    ## 5592                   Yishun Hawker Centre  9599.66826
    ## 5593   Jurong West Hawker Centre and Market  9272.13435
    ## 5594        Pasir Ris Central Hawker Centre 19931.76944
    ## 5595                   Dawson Hawker Centre  9557.11929
    ## 5596      Woodlands Street 12 Hawker Centre  6238.43367
    ## 5597              Blk 527 Ang Mo Kio Ave 10  9140.45478
    ## 5598           Blk 538 Bedok North Street 3 17696.11750
    ## 5599           Blk 58 New Upper Changi Road 19675.41505
    ## 5600              Blk 6 Tanjong Pagar Plaza 13658.68817
    ## 5601               Blk 628 Ang Mo Kio Ave 4  7598.43594
    ## 5602           Blk 630 Bedok Reservoir Road 16503.09896
    ## 5603                   Blk 69 Geylang Bahru 12502.22103
    ## 5604                     Blk 7 Empress Road  7722.34106
    ## 5605               Blk 724 Ang Mo Kio Ave 6  8260.22048
    ## 5606         Blk 726 Clementi West Street 2  8213.65063
    ## 5607              Blk 74 Toa Payoh Lorong 4 10047.80874
    ## 5608              Market Street Food Centre 13486.63171
    ## 5609                    Maxwell Food Centre 13454.14014
    ## 5610                     Newton Food Centre 10416.75731
    ## 5611 North Bridge Road Market & Food Centre 12905.93910
    ## 5612              Pasir Panjang Food Centre 11439.99643
    ## 5613           Pek Kio Market & Food Centre 10999.87097
    ## 5614              People's Park Food Centre 12872.70677
    ## 5615            Sembawang Hills Food Centre  6314.65851
    ## 5616                Serangoon Garden Market 10609.54396
    ## 5617      Taman Jurong Market & Food Centre  7402.34669
    ## 5618                    Tanglin Halt Market  8942.21063
    ## 5619                           Tekka Market 11736.88837
    ## 5620                     Tiong Bahru Market 12207.44737
    ## 5621             Zion Riverside Food Centre 11469.62119
    ## 5622              Blk 75 Toa Payoh Lorong 5 10063.96076
    ## 5623                    Blk 79 Redhill Lane 11161.52153
    ## 5624             Blk 79 Telok Blangah Drive 12170.78019
    ## 5625                    Blk 80 Circuit Road 13893.53629
    ## 5626             Blk 82 Telok Blangah Drive 12125.09807
    ## 5627           Blk 84 Marine Parade Central 17065.89694
    ## 5628            Blk 85 Bedok North Street 4 19176.13871
    ## 5629                    Blk 85 Redhill Lane 11210.16599
    ## 5630                    Blk 89 Circuit Road 13924.52253
    ## 5631                   Blk 90 Whampoa Drive 11005.75073
    ## 5632              Blk 93 Toa Payoh Lorong 4  9601.77551
    ## 5633                   Blks 13/14 Haig Road 15344.03199
    ## 5634          Blks 160/162 Ang Mo Kio Ave 4  7433.59982
    ## 5635                  Ci Yuan Hawker Centre 12298.33493
    ## 5636        Kampung Admiralty Hawker Centre 18164.96428
    ## 5637                   Yishun Hawker Centre 13176.80692
    ## 5638   Jurong West Hawker Centre and Market 27003.74128
    ## 5639        Pasir Ris Central Hawker Centre  2532.41134
    ## 5640                   Dawson Hawker Centre 16267.20450
    ## 5641      Woodlands Street 12 Hawker Centre 19860.45265
    ## 5642              Blk 527 Ang Mo Kio Ave 10  9714.23928
    ## 5643           Blk 538 Bedok North Street 3  2950.31706
    ## 5644           Blk 58 New Upper Changi Road  3284.96672
    ## 5645              Blk 6 Tanjong Pagar Plaza 13727.28920
    ## 5646               Blk 628 Ang Mo Kio Ave 4 11424.29059
    ## 5647           Blk 630 Bedok Reservoir Road  3661.04525
    ## 5648                   Blk 69 Geylang Bahru  8546.44800
    ## 5649                     Blk 7 Empress Road 15491.62441
    ## 5650               Blk 724 Ang Mo Kio Ave 6 10569.11847
    ## 5651         Blk 726 Clementi West Street 2 20297.11453
    ## 5652              Blk 74 Toa Payoh Lorong 4  9993.91767
    ## 5653              Market Street Food Centre 12631.40302
    ## 5654                    Maxwell Food Centre 13343.25710
    ## 5655                     Newton Food Centre 12074.60814
    ## 5656 North Bridge Road Market & Food Centre  9975.59175
    ## 5657              Pasir Panjang Food Centre 18622.07969
    ## 5658           Pek Kio Market & Food Centre 10797.78423
    ## 5659              People's Park Food Centre 13224.38532
    ## 5660            Sembawang Hills Food Centre 12492.21352
    ## 5661                Serangoon Garden Market  8187.56668
    ## 5662      Taman Jurong Market & Food Centre 24377.31716
    ## 5663                    Tanglin Halt Market 16868.09798
    ## 5664                           Tekka Market 11238.51563
    ## 5665                     Tiong Bahru Market 14164.55715
    ## 5666             Zion Riverside Food Centre 13862.86081
    ## 5667              Blk 75 Toa Payoh Lorong 5  9857.20816
    ## 5668                    Blk 79 Redhill Lane 15348.40740
    ## 5669             Blk 79 Telok Blangah Drive 17185.79949
    ## 5670                    Blk 80 Circuit Road  6522.07171
    ## 5671             Blk 82 Telok Blangah Drive 17124.43726
    ## 5672           Blk 84 Marine Parade Central  6806.76978
    ## 5673            Blk 85 Bedok North Street 4  2421.36130
    ## 5674                    Blk 85 Redhill Lane 15382.44746
    ## 5675                    Blk 89 Circuit Road  6913.68999
    ## 5676                   Blk 90 Whampoa Drive 10015.40405
    ## 5677              Blk 93 Toa Payoh Lorong 4 10182.83794
    ## 5678                   Blks 13/14 Haig Road  6514.32405
    ## 5679          Blks 160/162 Ang Mo Kio Ave 4 11415.95443
    ## 5680                  Ci Yuan Hawker Centre  6750.65670
    ## 5681                   Yishun Hawker Centre  5172.21920
    ## 5682   Jurong West Hawker Centre and Market 15848.72943
    ## 5683        Pasir Ris Central Hawker Centre 18326.92802
    ## 5684                   Dawson Hawker Centre 15757.43256
    ## 5685      Woodlands Street 12 Hawker Centre  2432.16659
    ## 5686              Blk 527 Ang Mo Kio Ave 10  9538.43656
    ## 5687           Blk 538 Bedok North Street 3 18272.86647
    ## 5688           Blk 58 New Upper Changi Road 20200.47577
    ## 5689              Blk 6 Tanjong Pagar Plaza 18667.17314
    ## 5690               Blk 628 Ang Mo Kio Ave 4  7888.61120
    ## 5691           Blk 630 Bedok Reservoir Road 17278.22155
    ## 5692                   Blk 69 Geylang Bahru 15201.92943
    ## 5693                     Blk 7 Empress Road 13687.73762
    ## 5694               Blk 724 Ang Mo Kio Ave 6  9061.01458
    ## 5695         Blk 726 Clementi West Street 2 15591.27258
    ## 5696              Blk 74 Toa Payoh Lorong 4 12976.38381
    ## 5697              Market Street Food Centre 18109.22145
    ## 5698                    Maxwell Food Centre 18341.91298
    ## 5699                     Newton Food Centre 14804.02986
    ## 5700 North Bridge Road Market & Food Centre 16418.09051
    ## 5701              Pasir Panjang Food Centre 18166.55298
    ## 5702           Pek Kio Market & Food Centre 14754.41566
    ## 5703              People's Park Food Centre 17755.38185
    ## 5704            Sembawang Hills Food Centre  8105.84186
    ## 5705                Serangoon Garden Market 11228.79381
    ## 5706      Taman Jurong Market & Food Centre 14605.61136
    ## 5707                    Tanglin Halt Market 15389.57946
    ## 5708                           Tekka Market 15792.68036
    ## 5709                     Tiong Bahru Market 17480.51899
    ## 5710             Zion Riverside Food Centre 16668.90710
    ## 5711              Blk 75 Toa Payoh Lorong 5 12875.58871
    ## 5712                    Blk 79 Redhill Lane 16930.70933
    ## 5713             Blk 79 Telok Blangah Drive 18434.00264
    ## 5714                    Blk 80 Circuit Road 15688.28910
    ## 5715             Blk 82 Telok Blangah Drive 18375.24872
    ## 5716           Blk 84 Marine Parade Central 19232.25348
    ## 5717            Blk 85 Bedok North Street 4 19453.81232
    ## 5718                    Blk 85 Redhill Lane 16987.80374
    ## 5719                    Blk 89 Circuit Road 15964.67837
    ## 5720                   Blk 90 Whampoa Drive 14284.62433
    ## 5721              Blk 93 Toa Payoh Lorong 4 12464.90771
    ## 5722                   Blks 13/14 Haig Road 17377.40905
    ## 5723          Blks 160/162 Ang Mo Kio Ave 4  8410.71306
    ## 5724                  Ci Yuan Hawker Centre 11606.87834
    ## 5725   Jurong West Hawker Centre and Market 18832.21693
    ## 5726        Pasir Ris Central Hawker Centre 13182.74607
    ## 5727                   Dawson Hawker Centre 14788.84564
    ## 5728      Woodlands Street 12 Hawker Centre  7290.52259
    ## 5729              Blk 527 Ang Mo Kio Ave 10  5877.99352
    ## 5730           Blk 538 Bedok North Street 3 13628.76304
    ## 5731           Blk 58 New Upper Changi Road 15471.34448
    ## 5732              Blk 6 Tanjong Pagar Plaza 16410.59199
    ## 5733               Blk 628 Ang Mo Kio Ave 4  4885.49044
    ## 5734           Blk 630 Bedok Reservoir Road 12753.77195
    ## 5735                   Blk 69 Geylang Bahru 11790.56160
    ## 5736                     Blk 7 Empress Road 12790.79929
    ## 5737               Blk 724 Ang Mo Kio Ave 6  5843.91782
    ## 5738         Blk 726 Clementi West Street 2 16122.51393
    ## 5739              Blk 74 Toa Payoh Lorong 4 10036.15342
    ## 5740              Market Street Food Centre 15618.77547
    ## 5741                    Maxwell Food Centre 16022.90253
    ## 5742                     Newton Food Centre 12522.11931
    ## 5743 North Bridge Road Market & Food Centre 13357.30100
    ## 5744              Pasir Panjang Food Centre 17524.06995
    ## 5745           Pek Kio Market & Food Centre 12052.35898
    ## 5746              People's Park Food Centre 15485.31014
    ## 5747            Sembawang Hills Food Centre  6078.17703
    ## 5748                Serangoon Garden Market  7264.80351
    ## 5749      Taman Jurong Market & Food Centre 16971.96892
    ## 5750                    Tanglin Halt Market 14703.50383
    ## 5751                           Tekka Market 13153.36016
    ## 5752                     Tiong Bahru Market 15534.08194
    ## 5753             Zion Riverside Food Centre 14747.96751
    ## 5754              Blk 75 Toa Payoh Lorong 5  9880.99347
    ## 5755                    Blk 79 Redhill Lane 15447.19152
    ## 5756             Blk 79 Telok Blangah Drive 17269.98873
    ## 5757                    Blk 80 Circuit Road 11733.97950
    ## 5758             Blk 82 Telok Blangah Drive 17202.99927
    ## 5759           Blk 84 Marine Parade Central 15203.87622
    ## 5760            Blk 85 Bedok North Street 4 14670.29594
    ## 5761                    Blk 85 Redhill Lane 15505.78795
    ## 5762                    Blk 89 Circuit Road 12110.35290
    ## 5763                   Blk 90 Whampoa Drive 11345.96202
    ## 5764              Blk 93 Toa Payoh Lorong 4  9584.16009
    ## 5765                   Blks 13/14 Haig Road 13403.32722
    ## 5766          Blks 160/162 Ang Mo Kio Ave 4  5621.01890
    ## 5767                  Ci Yuan Hawker Centre  6940.26044
    ## 5768        Pasir Ris Central Hawker Centre 28505.62041
    ## 5769                   Dawson Hawker Centre 12886.27244
    ## 5770      Woodlands Street 12 Hawker Centre 13717.42197
    ## 5771              Blk 527 Ang Mo Kio Ave 10 17824.89769
    ## 5772           Blk 538 Bedok North Street 3 25333.36914
    ## 5773           Blk 58 New Upper Changi Road 27190.02948
    ## 5774              Blk 6 Tanjong Pagar Plaza 17727.10809
    ## 5775               Blk 628 Ang Mo Kio Ave 4 16540.33774
    ## 5776           Blk 630 Bedok Reservoir Road 24127.45198
    ## 5777                   Blk 69 Geylang Bahru 19332.96169
    ## 5778                     Blk 7 Empress Road 12358.63080
    ## 5779               Blk 724 Ang Mo Kio Ave 6 16943.73040
    ## 5780         Blk 726 Clementi West Street 2  8517.05928
    ## 5781              Blk 74 Toa Payoh Lorong 4 17220.77618
    ## 5782              Market Street Food Centre 18132.48139
    ## 5783                    Maxwell Food Centre 17745.46715
    ## 5784                     Newton Food Centre 16150.62850
    ## 5785 North Bridge Road Market & Food Centre 18939.44856
    ## 5786              Pasir Panjang Food Centre 12714.38064
    ## 5787           Pek Kio Market & Food Centre 17232.37621
    ## 5788              People's Park Food Centre 17313.18003
    ## 5789            Sembawang Hills Food Centre 15043.44838
    ## 5790                Serangoon Garden Market 18999.80434
    ## 5791      Taman Jurong Market & Food Centre  2783.32363
    ## 5792                    Tanglin Halt Market 12017.91216
    ## 5793                           Tekka Market 17476.73159
    ## 5794                     Tiong Bahru Market 16251.77899
    ## 5795             Zion Riverside Food Centre 15841.08333
    ## 5796              Blk 75 Toa Payoh Lorong 5 17319.27786
    ## 5797                    Blk 79 Redhill Lane 14703.73929
    ## 5798             Blk 79 Telok Blangah Drive 14381.58258
    ## 5799                    Blk 80 Circuit Road 21166.33907
    ## 5800             Blk 82 Telok Blangah Drive 14380.32983
    ## 5801           Blk 84 Marine Parade Central 23648.32729
    ## 5802            Blk 85 Bedok North Street 4 26876.87096
    ## 5803                    Blk 85 Redhill Lane 14720.77199
    ## 5804                    Blk 89 Circuit Road 21019.96610
    ## 5805                   Blk 90 Whampoa Drive 17670.56215
    ## 5806              Blk 93 Toa Payoh Lorong 4 16930.93828
    ## 5807                   Blks 13/14 Haig Road 22246.48197
    ## 5808          Blks 160/162 Ang Mo Kio Ave 4 16203.91936
    ## 5809                  Ci Yuan Hawker Centre 20994.99013
    ## 5810                   Dawson Hawker Centre 18372.12253
    ## 5811      Woodlands Street 12 Hawker Centre 20236.80707
    ## 5812              Blk 527 Ang Mo Kio Ave 10 10801.87184
    ## 5813           Blk 538 Bedok North Street 3  5481.02465
    ## 5814           Blk 58 New Upper Changi Road  5567.30593
    ## 5815              Blk 6 Tanjong Pagar Plaza 16127.19100
    ## 5816               Blk 628 Ang Mo Kio Ave 4 12365.94627
    ## 5817           Blk 630 Bedok Reservoir Road  6102.40683
    ## 5818                   Blk 69 Geylang Bahru 10741.01911
    ## 5819                     Blk 7 Empress Road 17425.69192
    ## 5820               Blk 724 Ang Mo Kio Ave 6 11688.67372
    ## 5821         Blk 726 Clementi West Street 2 22219.61825
    ## 5822              Blk 74 Toa Payoh Lorong 4 11883.30680
    ## 5823              Market Street Food Centre 15024.00124
    ## 5824                    Maxwell Food Centre 15733.09154
    ## 5825                     Newton Food Centre 14199.09503
    ## 5826 North Bridge Road Market & Food Centre 12293.99511
    ## 5827              Pasir Panjang Food Centre 20830.72342
    ## 5828           Pek Kio Market & Food Centre 12932.39935
    ## 5829              People's Park Food Centre 15578.67923
    ## 5830            Sembawang Hills Food Centre 13638.68621
    ## 5831                Serangoon Garden Market  9506.01395
    ## 5832      Taman Jurong Market & Food Centre 25956.82708
    ## 5833                    Tanglin Halt Market 18919.28774
    ## 5834                           Tekka Market 13477.15549
    ## 5835                     Tiong Bahru Market 16474.01782
    ## 5836             Zion Riverside Food Centre 16118.46736
    ## 5837              Blk 75 Toa Payoh Lorong 5 11726.45936
    ## 5838                    Blk 79 Redhill Lane 17580.91015
    ## 5839             Blk 79 Telok Blangah Drive 19466.78245
    ## 5840                    Blk 80 Circuit Road  8763.32672
    ## 5841             Blk 82 Telok Blangah Drive 19403.51686
    ## 5842           Blk 84 Marine Parade Central  9338.26993
    ## 5843            Blk 85 Bedok North Street 4  4799.63853
    ## 5844                    Blk 85 Redhill Lane 17618.32916
    ## 5845                    Blk 89 Circuit Road  9200.92849
    ## 5846                   Blk 90 Whampoa Drive 12094.57313
    ## 5847              Blk 93 Toa Payoh Lorong 4 11994.70573
    ## 5848                   Blks 13/14 Haig Road  8960.72081
    ## 5849          Blks 160/162 Ang Mo Kio Ave 4 12501.36126
    ## 5850                  Ci Yuan Hawker Centre  7633.63978
    ## 5851      Woodlands Street 12 Hawker Centre 15297.76697
    ## 5852              Blk 527 Ang Mo Kio Ave 10  9998.71372
    ## 5853           Blk 538 Bedok North Street 3 13896.64481
    ## 5854           Blk 58 New Upper Changi Road 15466.39226
    ## 5855              Blk 6 Tanjong Pagar Plaza  4861.98672
    ## 5856               Blk 628 Ang Mo Kio Ave 4 10067.29400
    ## 5857           Blk 630 Bedok Reservoir Road 12791.73247
    ## 5858                   Blk 69 Geylang Bahru  7734.64539
    ## 5859                     Blk 7 Empress Road  2076.92280
    ## 5860               Blk 724 Ang Mo Kio Ave 6  9481.16384
    ## 5861         Blk 726 Clementi West Street 2  4555.33061
    ## 5862              Blk 74 Toa Payoh Lorong 4  6669.80303
    ## 5863              Market Street Food Centre  5263.03658
    ## 5864                    Maxwell Food Centre  4859.99381
    ## 5865                     Newton Food Centre  4192.63185
    ## 5866 North Bridge Road Market & Food Centre  6649.06259
    ## 5867              Pasir Panjang Food Centre  2808.57073
    ## 5868           Pek Kio Market & Food Centre  5469.57710
    ## 5869              People's Park Food Centre  4434.59478
    ## 5870            Sembawang Hills Food Centre  8711.59767
    ## 5871                Serangoon Garden Market 10018.71559
    ## 5872      Taman Jurong Market & Food Centre 10127.12655
    ## 5873                    Tanglin Halt Market   869.45766
    ## 5874                           Tekka Market  5189.24826
    ## 5875                     Tiong Bahru Market  3367.06961
    ## 5876             Zion Riverside Food Centre  3000.72465
    ## 5877              Blk 75 Toa Payoh Lorong 5  6854.14133
    ## 5878                    Blk 79 Redhill Lane  1858.41667
    ## 5879             Blk 79 Telok Blangah Drive  2686.39127
    ## 5880                    Blk 80 Circuit Road  9767.03242
    ## 5881             Blk 82 Telok Blangah Drive  2631.10281
    ## 5882           Blk 84 Marine Parade Central 11322.20914
    ## 5883            Blk 85 Bedok North Street 4 15395.02726
    ## 5884                    Blk 85 Redhill Lane  1886.72043
    ## 5885                    Blk 89 Circuit Road  9432.32060
    ## 5886                   Blk 90 Whampoa Drive  6278.00765
    ## 5887              Blk 93 Toa Payoh Lorong 4  6737.84850
    ## 5888                   Blks 13/14 Haig Road 10302.37466
    ## 5889          Blks 160/162 Ang Mo Kio Ave 4  9340.23920
    ## 5890                  Ci Yuan Hawker Centre 12253.19159
    ## 5891              Blk 527 Ang Mo Kio Ave 10 10689.03958
    ## 5892           Blk 538 Bedok North Street 3 19677.62570
    ## 5893           Blk 58 New Upper Changi Road 21647.64328
    ## 5894              Blk 6 Tanjong Pagar Plaza 18730.79261
    ## 5895               Blk 628 Ang Mo Kio Ave 4  8924.48230
    ## 5896           Blk 630 Bedok Reservoir Road 18613.82461
    ## 5897                   Blk 69 Geylang Bahru 15949.63287
    ## 5898                     Blk 7 Empress Road 13284.93394
    ## 5899               Blk 724 Ang Mo Kio Ave 6 10057.90188
    ## 5900         Blk 726 Clementi West Street 2 14451.45570
    ## 5901              Blk 74 Toa Payoh Lorong 4 13579.32946
    ## 5902              Market Street Food Centre 18303.31369
    ## 5903                    Maxwell Food Centre 18444.84081
    ## 5904                     Newton Food Centre 15006.23383
    ## 5905 North Bridge Road Market & Food Centre 16947.71837
    ## 5906              Pasir Panjang Food Centre 17477.96181
    ## 5907           Pek Kio Market & Food Centre 15162.80465
    ## 5908              People's Park Food Centre 17850.42651
    ## 5909            Sembawang Hills Food Centre  8701.49490
    ## 5910                Serangoon Garden Market 12417.90163
    ## 5911      Taman Jurong Market & Food Centre 12718.38683
    ## 5912                    Tanglin Halt Market 14812.44340
    ## 5913                           Tekka Market 16131.60916
    ## 5914                     Tiong Bahru Market 17426.09877
    ## 5915             Zion Riverside Food Centre 16627.87927
    ## 5916              Blk 75 Toa Payoh Lorong 5 13509.76089
    ## 5917                    Blk 79 Redhill Lane 16672.08630
    ## 5918             Blk 79 Telok Blangah Drive 17979.84480
    ## 5919                    Blk 80 Circuit Road 16711.76428
    ## 5920             Blk 82 Telok Blangah Drive 17926.55806
    ## 5921           Blk 84 Marine Parade Central 20223.60267
    ## 5922            Blk 85 Bedok North Street 4 20952.04548
    ## 5923                    Blk 85 Redhill Lane 16726.61271
    ## 5924                    Blk 89 Circuit Road 16925.51941
    ## 5925                   Blk 90 Whampoa Drive 14828.13139
    ## 5926              Blk 93 Toa Payoh Lorong 4 13065.67875
    ## 5927                   Blks 13/14 Haig Road 18372.29857
    ## 5928          Blks 160/162 Ang Mo Kio Ave 4  9293.76556
    ## 5929                  Ci Yuan Hawker Centre 13159.35971
    ## 5930           Blk 538 Bedok North Street 3  9047.52169
    ## 5931           Blk 58 New Upper Changi Road 11038.48334
    ## 5932              Blk 6 Tanjong Pagar Plaza 10706.68253
    ## 5933               Blk 628 Ang Mo Kio Ave 4  1786.60049
    ## 5934           Blk 630 Bedok Reservoir Road  7945.77823
    ## 5935                   Blk 69 Geylang Bahru  5930.48412
    ## 5936                     Blk 7 Empress Road  8283.43766
    ## 5937               Blk 724 Ang Mo Kio Ave 6   888.50842
    ## 5938         Blk 726 Clementi West Street 2 12609.52330
    ## 5939              Blk 74 Toa Payoh Lorong 4  4235.39643
    ## 5940              Market Street Food Centre  9842.91589
    ## 5941                    Maxwell Food Centre 10301.80213
    ## 5942                     Newton Food Centre  6933.29581
    ## 5943 North Bridge Road Market & Food Centre  7482.63779
    ## 5944              Pasir Panjang Food Centre 12806.20433
    ## 5945           Pek Kio Market & Food Centre  6277.55477
    ## 5946              People's Park Food Centre  9796.13607
    ## 5947            Sembawang Hills Food Centre  2836.88957
    ## 5948                Serangoon Garden Market  1729.09698
    ## 5949      Taman Jurong Market & Food Centre 15387.75387
    ## 5950                    Tanglin Halt Market 10167.98844
    ## 5951                           Tekka Market  7373.44426
    ## 5952                     Tiong Bahru Market 10003.41840
    ## 5953             Zion Riverside Food Centre  9263.40923
    ## 5954              Blk 75 Toa Payoh Lorong 5  4065.15869
    ## 5955                    Blk 79 Redhill Lane 10211.68585
    ## 5956             Blk 79 Telok Blangah Drive 12165.57728
    ## 5957                    Blk 80 Circuit Road  6153.95688
    ## 5958             Blk 82 Telok Blangah Drive 12095.97169
    ## 5959           Blk 84 Marine Parade Central  9697.85742
    ## 5960            Blk 85 Bedok North Street 4 10404.15926
    ## 5961                    Blk 85 Redhill Lane 10268.10000
    ## 5962                    Blk 89 Circuit Road  6451.61359
    ## 5963                   Blk 90 Whampoa Drive  5510.08266
    ## 5964              Blk 93 Toa Payoh Lorong 4  3832.29879
    ## 5965                   Blks 13/14 Haig Road  7845.59681
    ## 5966          Blks 160/162 Ang Mo Kio Ave 4  1709.79965
    ## 5967                  Ci Yuan Hawker Centre  3184.76575
    ## 5968           Blk 58 New Upper Changi Road  1997.72464
    ## 5969              Blk 6 Tanjong Pagar Plaza 10947.02945
    ## 5970               Blk 628 Ang Mo Kio Ave 4 10834.02237
    ## 5971           Blk 630 Bedok Reservoir Road  1210.48303
    ## 5972                   Blk 69 Geylang Bahru  6208.86468
    ## 5973                     Blk 7 Empress Road 13377.39753
    ## 5974               Blk 724 Ang Mo Kio Ave 6  9795.55356
    ## 5975         Blk 726 Clementi West Street 2 18137.17306
    ## 5976              Blk 74 Toa Payoh Lorong 4  8112.67435
    ## 5977              Market Street Food Centre  9868.98264
    ## 5978                    Maxwell Food Centre 10580.28101
    ## 5979                     Newton Food Centre  9742.45822
    ## 5980 North Bridge Road Market & Food Centre  7368.26340
    ## 5981              Pasir Panjang Food Centre 16084.70557
    ## 5982           Pek Kio Market & Food Centre  8482.10985
    ## 5983              People's Park Food Centre 10516.15573
    ## 5984            Sembawang Hills Food Centre 11573.39120
    ## 5985                Serangoon Garden Market  7338.53425
    ## 5986      Taman Jurong Market & Food Centre 22630.02171
    ## 5987                    Tanglin Halt Market 14567.02294
    ## 5988                           Tekka Market  8740.48817
    ## 5989                     Tiong Bahru Market 11516.13505
    ## 5990             Zion Riverside Food Centre 11298.45572
    ## 5991              Blk 75 Toa Payoh Lorong 5  8015.85607
    ## 5992                    Blk 79 Redhill Lane 12802.93434
    ## 5993             Blk 79 Telok Blangah Drive 14556.68042
    ## 5994                    Blk 80 Circuit Road  4219.82736
    ## 5995             Blk 82 Telok Blangah Drive 14498.37607
    ## 5996           Blk 84 Marine Parade Central  3857.35180
    ## 5997            Blk 85 Bedok North Street 4  1545.79845
    ## 5998                    Blk 85 Redhill Lane 12831.71762
    ## 5999                    Blk 89 Circuit Road  4480.11421
    ## 6000                   Blk 90 Whampoa Drive  7818.94757
    ## 6001              Blk 93 Toa Payoh Lorong 4  8416.13146
    ## 6002                   Blks 13/14 Haig Road  3737.73450
    ## 6003          Blks 160/162 Ang Mo Kio Ave 4 10635.57237
    ## 6004                  Ci Yuan Hawker Centre  6708.36224
    ## 6005              Blk 6 Tanjong Pagar Plaza 12105.37650
    ## 6006               Blk 628 Ang Mo Kio Ave 4 12824.48099
    ## 6007           Blk 630 Bedok Reservoir Road  3172.64085
    ## 6008                   Blk 69 Geylang Bahru  7921.57391
    ## 6009                     Blk 7 Empress Road 15104.92475
    ## 6010               Blk 724 Ang Mo Kio Ave 6 11791.95109
    ## 6011         Blk 726 Clementi West Street 2 19812.62147
    ## 6012              Blk 74 Toa Payoh Lorong 4  9985.55246
    ## 6013              Market Street Food Centre 11075.98646
    ## 6014                    Maxwell Food Centre 11773.88238
    ## 6015                     Newton Food Centre 11385.18963
    ## 6016 North Bridge Road Market & Food Centre  8834.29286
    ## 6017              Pasir Panjang Food Centre 17497.87012
    ## 6018           Pek Kio Market & Food Centre 10155.96085
    ## 6019              People's Park Food Centre 11793.42319
    ## 6020            Sembawang Hills Food Centre 13570.05081
    ## 6021                Serangoon Garden Market  9333.55741
    ## 6022      Taman Jurong Market & Food Centre 24467.67839
    ## 6023                    Tanglin Halt Market 16179.14040
    ## 6024                           Tekka Market 10277.24963
    ## 6025                     Tiong Bahru Market 12855.44093
    ## 6026             Zion Riverside Food Centre 12732.27265
    ## 6027              Blk 75 Toa Payoh Lorong 5  9901.83082
    ## 6028                    Blk 79 Redhill Lane 14237.18654
    ## 6029             Blk 79 Telok Blangah Drive 15885.11595
    ## 6030                    Blk 80 Circuit Road  6023.71005
    ## 6031             Blk 82 Telok Blangah Drive 15830.86732
    ## 6032           Blk 84 Marine Parade Central  4567.24170
    ## 6033            Blk 85 Bedok North Street 4   902.80907
    ## 6034                    Blk 85 Redhill Lane 14260.54431
    ## 6035                    Blk 89 Circuit Road  6197.52651
    ## 6036                   Blk 90 Whampoa Drive  9571.34321
    ## 6037              Blk 93 Toa Payoh Lorong 4 10317.89741
    ## 6038                   Blks 13/14 Haig Road  5164.04931
    ## 6039          Blks 160/162 Ang Mo Kio Ave 4 12632.71421
    ## 6040                  Ci Yuan Hawker Centre  8601.70901
    ## 6041               Blk 628 Ang Mo Kio Ave 4 11548.46197
    ## 6042           Blk 630 Bedok Reservoir Road 10066.28234
    ## 6043                   Blk 69 Geylang Bahru  5789.01315
    ## 6044                     Blk 7 Empress Road  6053.89074
    ## 6045               Blk 724 Ang Mo Kio Ave 6 10575.64056
    ## 6046         Blk 726 Clementi West Street 2  9280.80765
    ## 6047              Blk 74 Toa Payoh Lorong 4  6480.96664
    ## 6048              Market Street Food Centre  1107.37034
    ## 6049                    Maxwell Food Centre   428.65066
    ## 6050                     Newton Food Centre  3921.40127
    ## 6051 North Bridge Road Market & Food Centre  3963.51331
    ## 6052              Pasir Panjang Food Centre  5761.49005
    ## 6053           Pek Kio Market & Food Centre  4442.70854
    ## 6054              People's Park Food Centre   928.53099
    ## 6055            Sembawang Hills Food Centre 10706.61128
    ## 6056                Serangoon Garden Market  9922.44527
    ## 6057      Taman Jurong Market & Food Centre 14979.26405
    ## 6058                    Tanglin Halt Market  5730.07389
    ## 6059                           Tekka Market  3373.12033
    ## 6060                     Tiong Bahru Market  1525.60820
    ## 6061             Zion Riverside Food Centre  2191.18838
    ## 6062              Blk 75 Toa Payoh Lorong 5  6660.26069
    ## 6063                    Blk 79 Redhill Lane  3023.50655
    ## 6064             Blk 79 Telok Blangah Drive  3971.59399
    ## 6065                    Blk 80 Circuit Road  7485.47732
    ## 6066             Blk 82 Telok Blangah Drive  3931.44818
    ## 6067           Blk 84 Marine Parade Central  7581.85244
    ## 6068            Blk 85 Bedok North Street 4 12272.09548
    ## 6069                    Blk 85 Redhill Lane  3006.51555
    ## 6070                    Blk 89 Circuit Road  6996.97775
    ## 6071                   Blk 90 Whampoa Drive  5293.15866
    ## 6072              Blk 93 Toa Payoh Lorong 4  6876.54554
    ## 6073                   Blks 13/14 Haig Road  7225.82023
    ## 6074          Blks 160/162 Ang Mo Kio Ave 4 10831.39772
    ## 6075                  Ci Yuan Hawker Centre 11790.49616
    ## 6076           Blk 630 Bedok Reservoir Road  9730.72775
    ## 6077                   Blk 69 Geylang Bahru  7350.21829
    ## 6078                     Blk 7 Empress Road  8155.94975
    ## 6079               Blk 724 Ang Mo Kio Ave 6  1172.47851
    ## 6080         Blk 726 Clementi West Street 2 12053.09395
    ## 6081              Blk 74 Toa Payoh Lorong 4  5291.22880
    ## 6082              Market Street Food Centre 10793.45172
    ## 6083                    Maxwell Food Centre 11167.93537
    ## 6084                     Newton Food Centre  7645.33508
    ## 6085 North Bridge Road Market & Food Centre  8713.42464
    ## 6086              Pasir Panjang Food Centre 12852.22742
    ## 6087           Pek Kio Market & Food Centre  7250.63315
    ## 6088              People's Park Food Centre 10621.02788
    ## 6089            Sembawang Hills Food Centre  1613.88893
    ## 6090                Serangoon Garden Market  3508.74656
    ## 6091      Taman Jurong Market & Food Centre 14220.72984
    ## 6092                    Tanglin Halt Market 10083.33858
    ## 6093                           Tekka Market  8345.39375
    ## 6094                     Tiong Bahru Market 10648.64267
    ## 6095             Zion Riverside Food Centre  9862.82478
    ## 6096              Blk 75 Toa Payoh Lorong 5  5158.03093
    ## 6097                    Blk 79 Redhill Lane 10596.08954
    ## 6098             Blk 79 Telok Blangah Drive 12459.57540
    ## 6099                    Blk 80 Circuit Road  7828.83107
    ## 6100             Blk 82 Telok Blangah Drive 12391.54703
    ## 6101           Blk 84 Marine Parade Central 11366.41109
    ## 6102            Blk 85 Bedok North Street 4 12184.86407
    ## 6103                    Blk 85 Redhill Lane 10654.44196
    ## 6104                    Blk 89 Circuit Road  8084.41481
    ## 6105                   Blk 90 Whampoa Drive  6620.80621
    ## 6106              Blk 93 Toa Payoh Lorong 4  4806.52665
    ## 6107                   Blks 13/14 Haig Road  9509.84705
    ## 6108          Blks 160/162 Ang Mo Kio Ave 4   740.92560
    ## 6109                  Ci Yuan Hawker Centre  4747.47902
    ## 6110                   Blk 69 Geylang Bahru  5070.01263
    ## 6111                     Blk 7 Empress Road 12209.86225
    ## 6112               Blk 724 Ang Mo Kio Ave 6  8667.25885
    ## 6113         Blk 726 Clementi West Street 2 16983.65483
    ## 6114              Blk 74 Toa Payoh Lorong 4  6907.75437
    ## 6115              Market Street Food Centre  8971.01776
    ## 6116                    Maxwell Food Centre  9683.02842
    ## 6117                     Newton Food Centre  8617.20524
    ## 6118 North Bridge Road Market & Food Centre  6351.46772
    ## 6119              Pasir Panjang Food Centre 15046.52572
    ## 6120           Pek Kio Market & Food Centre  7347.99867
    ## 6121              People's Park Food Centre  9573.01913
    ## 6122            Sembawang Hills Food Centre 10412.56223
    ## 6123                Serangoon Garden Market  6223.43527
    ## 6124      Taman Jurong Market & Food Centre 21427.61462
    ## 6125                    Tanglin Halt Market 13442.60435
    ## 6126                           Tekka Market  7672.03617
    ## 6127                     Tiong Bahru Market 10532.65745
    ## 6128             Zion Riverside Food Centre 10267.24513
    ## 6129              Blk 75 Toa Payoh Lorong 5  6808.61287
    ## 6130                    Blk 79 Redhill Lane 11764.76253
    ## 6131             Blk 79 Telok Blangah Drive 13564.63306
    ## 6132                    Blk 80 Circuit Road  3053.49246
    ## 6133             Blk 82 Telok Blangah Drive 13504.49990
    ## 6134           Blk 84 Marine Parade Central  3520.22854
    ## 6135            Blk 85 Bedok North Street 4  2749.54181
    ## 6136                    Blk 85 Redhill Lane 11796.11051
    ## 6137                    Blk 89 Circuit Road  3359.73742
    ## 6138                   Blk 90 Whampoa Drive  6653.37480
    ## 6139              Blk 93 Toa Payoh Lorong 4  7206.67670
    ## 6140                   Blks 13/14 Haig Road  2860.51541
    ## 6141          Blks 160/162 Ang Mo Kio Ave 4  9500.22248
    ## 6142                  Ci Yuan Hawker Centre  5814.33693
    ## 6143                     Blk 7 Empress Road  7186.71646
    ## 6144               Blk 724 Ang Mo Kio Ave 6  6188.99341
    ## 6145         Blk 726 Clementi West Street 2 11929.14390
    ## 6146              Blk 74 Toa Payoh Lorong 4  2471.63074
    ## 6147              Market Street Food Centre  4714.19178
    ## 6148                    Maxwell Food Centre  5362.50490
    ## 6149                     Newton Food Centre  3549.28504
    ## 6150 North Bridge Road Market & Food Centre  1864.26464
    ## 6151              Pasir Panjang Food Centre 10094.31970
    ## 6152           Pek Kio Market & Food Centre  2277.99033
    ## 6153              People's Park Food Centre  5057.62602
    ## 6154            Sembawang Hills Food Centre  7248.13835
    ## 6155                Serangoon Garden Market  4624.71851
    ## 6156      Taman Jurong Market & Food Centre 16588.45021
    ## 6157                    Tanglin Halt Market  8373.13409
    ## 6158                           Tekka Market  2746.39419
    ## 6159                     Tiong Bahru Market  5808.53623
    ## 6160             Zion Riverside Food Centre  5387.80326
    ## 6161              Blk 75 Toa Payoh Lorong 5  2490.74437
    ## 6162                    Blk 79 Redhill Lane  6840.08812
    ## 6163             Blk 79 Telok Blangah Drive  8744.90250
    ## 6164                    Blk 80 Circuit Road  2032.40766
    ## 6165             Blk 82 Telok Blangah Drive  8680.27782
    ## 6166           Blk 84 Marine Parade Central  4565.91837
    ## 6167            Blk 85 Bedok North Street 4  7736.78326
    ## 6168                    Blk 85 Redhill Lane  6877.86501
    ## 6169                    Blk 89 Circuit Road  1732.78334
    ## 6170                   Blk 90 Whampoa Drive  1662.60046
    ## 6171              Blk 93 Toa Payoh Lorong 4  2956.19713
    ## 6172                   Blks 13/14 Haig Road  2934.04134
    ## 6173          Blks 160/162 Ang Mo Kio Ave 4  6791.28156
    ## 6174                  Ci Yuan Hawker Centre  6139.54730
    ## 6175               Blk 724 Ang Mo Kio Ave 6  7679.70977
    ## 6176         Blk 726 Clementi West Street 2  4806.15011
    ## 6177              Blk 74 Toa Payoh Lorong 4  5542.40857
    ## 6178              Market Street Food Centre  6101.35646
    ## 6179                    Maxwell Food Centre  5915.04610
    ## 6180                     Newton Food Centre  3806.23440
    ## 6181 North Bridge Road Market & Food Centre  6585.94777
    ## 6182              Pasir Panjang Food Centre  4733.33212
    ## 6183           Pek Kio Market & Food Centre  4962.29909
    ## 6184              People's Park Food Centre  5370.94522
    ## 6185            Sembawang Hills Food Centre  6729.88680
    ## 6186                Serangoon Garden Market  8552.53510
    ## 6187      Taman Jurong Market & Food Centre  9578.55085
    ## 6188                    Tanglin Halt Market  1927.63591
    ## 6189                           Tekka Market  5118.19214
    ## 6190                     Tiong Bahru Market  4550.54566
    ## 6191             Zion Riverside Food Centre  3882.44555
    ## 6192              Blk 75 Toa Payoh Lorong 5  5701.39646
    ## 6193                    Blk 79 Redhill Lane  3445.52388
    ## 6194             Blk 79 Telok Blangah Drive  4746.33985
    ## 6195                    Blk 80 Circuit Road  9159.58105
    ## 6196             Blk 82 Telok Blangah Drive  4687.88468
    ## 6197           Blk 84 Marine Parade Central 11313.02127
    ## 6198            Blk 85 Bedok North Street 4 14914.19589
    ## 6199                    Blk 85 Redhill Lane  3495.85556
    ## 6200                    Blk 89 Circuit Road  8917.98729
    ## 6201                   Blk 90 Whampoa Drive  5558.56144
    ## 6202              Blk 93 Toa Payoh Lorong 4  5466.81234
    ## 6203                   Blks 13/14 Haig Road 10013.88273
    ## 6204          Blks 160/162 Ang Mo Kio Ave 4  7445.86879
    ## 6205                  Ci Yuan Hawker Centre 10812.55726
    ## 6206         Blk 726 Clementi West Street 2 11875.47870
    ## 6207              Blk 74 Toa Payoh Lorong 4  4207.49595
    ## 6208              Market Street Food Centre  9774.92848
    ## 6209                    Maxwell Food Centre 10184.04874
    ## 6210                     Newton Food Centre  6713.01833
    ## 6211 North Bridge Road Market & Food Centre  7595.86667
    ## 6212              Pasir Panjang Food Centre 12287.74979
    ## 6213           Pek Kio Market & Food Centre  6210.05255
    ## 6214              People's Park Food Centre  9652.80461
    ## 6215            Sembawang Hills Food Centre  1950.43017
    ## 6216                Serangoon Garden Market  2462.11086
    ## 6217      Taman Jurong Market & Food Centre 14519.28313
    ## 6218                    Tanglin Halt Market  9589.26229
    ## 6219                           Tekka Market  7310.65839
    ## 6220                     Tiong Bahru Market  9760.33231
    ## 6221             Zion Riverside Food Centre  8993.29172
    ## 6222              Blk 75 Toa Payoh Lorong 5  4061.61606
    ## 6223                    Blk 79 Redhill Lane  9836.76453
    ## 6224             Blk 79 Telok Blangah Drive 11753.98243
    ## 6225                    Blk 80 Circuit Road  6668.69719
    ## 6226             Blk 82 Telok Blangah Drive 11684.92540
    ## 6227           Blk 84 Marine Parade Central 10201.97969
    ## 6228            Blk 85 Bedok North Street 4 11184.10788
    ## 6229                    Blk 85 Redhill Lane  9894.30825
    ## 6230                    Blk 89 Circuit Road  6915.97751
    ## 6231                   Blk 90 Whampoa Drive  5530.10869
    ## 6232              Blk 93 Toa Payoh Lorong 4  3743.74176
    ## 6233                   Blks 13/14 Haig Road  8345.58798
    ## 6234          Blks 160/162 Ang Mo Kio Ave 4   851.52758
    ## 6235                  Ci Yuan Hawker Centre  4073.11874
    ## 6236              Blk 74 Toa Payoh Lorong 4 10339.69980
    ## 6237              Market Street Food Centre  9795.55247
    ## 6238                    Maxwell Food Centre  9340.21696
    ## 6239                     Newton Food Centre  8427.84898
    ## 6240 North Bridge Road Market & Food Centre 11089.69707
    ## 6241              Pasir Panjang Food Centre  4315.32127
    ## 6242           Pek Kio Market & Food Centre  9664.02857
    ## 6243              People's Park Food Centre  8959.95468
    ## 6244            Sembawang Hills Food Centre 10461.24710
    ## 6245                Serangoon Garden Market 13159.02295
    ## 6246      Taman Jurong Market & Food Centre  5850.98187
    ## 6247                    Tanglin Halt Market  3727.53437
    ## 6248                           Tekka Market  9604.41761
    ## 6249                     Tiong Bahru Market  7854.08529
    ## 6250             Zion Riverside Food Centre  7554.80094
    ## 6251              Blk 75 Toa Payoh Lorong 5 10493.67087
    ## 6252                    Blk 79 Redhill Lane  6277.12413
    ## 6253             Blk 79 Telok Blangah Drive  5882.07015
    ## 6254                    Blk 80 Circuit Road 13930.16445
    ## 6255             Blk 82 Telok Blangah Drive  5876.91489
    ## 6256           Blk 84 Marine Parade Central 15812.42612
    ## 6257            Blk 85 Bedok North Street 4 19665.49336
    ## 6258                    Blk 85 Redhill Lane  6286.94322
    ## 6259                    Blk 89 Circuit Road 13660.85094
    ## 6260                   Blk 90 Whampoa Drive 10332.88359
    ## 6261              Blk 93 Toa Payoh Lorong 4 10233.59258
    ## 6262                   Blks 13/14 Haig Road 14671.35154
    ## 6263          Blks 160/162 Ang Mo Kio Ave 4 11432.50775
    ## 6264                  Ci Yuan Hawker Centre 15404.83172
    ## 6265              Market Street Food Centre  5608.34737
    ## 6266                    Maxwell Food Centre  6071.92478
    ## 6267                     Newton Food Centre  2862.06978
    ## 6268 North Bridge Road Market & Food Centre  3446.85321
    ## 6269              Pasir Panjang Food Centre  9355.41830
    ## 6270           Pek Kio Market & Food Centre  2042.84170
    ## 6271              People's Park Food Centre  5579.16378
    ## 6272            Sembawang Hills Food Centre  4904.96911
    ## 6273                Serangoon Garden Market  3563.36995
    ## 6274      Taman Jurong Market & Food Centre 14520.98090
    ## 6275                    Tanglin Halt Market  7109.97534
    ## 6276                           Tekka Market  3138.20528
    ## 6277                     Tiong Bahru Market  5890.05376
    ## 6278             Zion Riverside Food Centre  5209.96363
    ## 6279              Blk 75 Toa Payoh Lorong 5   195.36822
    ## 6280                    Blk 79 Redhill Lane  6374.47129
    ## 6281             Blk 79 Telok Blangah Drive  8375.60522
    ## 6282                    Blk 80 Circuit Road  3979.08645
    ## 6283             Blk 82 Telok Blangah Drive  8306.10473
    ## 6284           Blk 84 Marine Parade Central  7021.28948
    ## 6285            Blk 85 Bedok North Street 4  9656.68624
    ## 6286                    Blk 85 Redhill Lane  6425.91899
    ## 6287                    Blk 89 Circuit Road  3919.87203
    ## 6288                   Blk 90 Whampoa Drive  1329.92104
    ## 6289              Blk 93 Toa Payoh Lorong 4   514.81680
    ## 6290                   Blks 13/14 Haig Road  5306.60201
    ## 6291          Blks 160/162 Ang Mo Kio Ave 4  4639.99604
    ## 6292                  Ci Yuan Hawker Centre  5685.05409
    ## 6293                    Maxwell Food Centre   712.32281
    ## 6294                     Newton Food Centre  3310.56726
    ## 6295 North Bridge Road Market & Food Centre  2871.11309
    ## 6296              Pasir Panjang Food Centre  6583.69922
    ## 6297           Pek Kio Market & Food Centre  3570.84683
    ## 6298              People's Park Food Centre   838.38268
    ## 6299            Sembawang Hills Food Centre 10063.89857
    ## 6300                Serangoon Garden Market  8962.51701
    ## 6301      Taman Jurong Market & Food Centre 15362.26324
    ## 6302                    Tanglin Halt Market  6124.82630
    ## 6303                           Tekka Market  2470.74209
    ## 6304                     Tiong Bahru Market  1972.25783
    ## 6305             Zion Riverside Food Centre  2296.57090
    ## 6306              Blk 75 Toa Payoh Lorong 5  5777.98868
    ## 6307                    Blk 79 Redhill Lane  3547.27832
    ## 6308             Blk 79 Telok Blangah Drive  4859.68435
    ## 6309                    Blk 80 Circuit Road  6379.64290
    ## 6310             Blk 82 Telok Blangah Drive  4811.30697
    ## 6311           Blk 84 Marine Parade Central  6589.41480
    ## 6312            Blk 85 Bedok North Street 4 11214.56421
    ## 6313                    Blk 85 Redhill Lane  3547.54542
    ## 6314                    Blk 89 Circuit Road  5889.96586
    ## 6315                   Blk 90 Whampoa Drive  4358.07538
    ## 6316              Blk 93 Toa Payoh Lorong 4  6038.51263
    ## 6317                   Blks 13/14 Haig Road  6139.38027
    ## 6318          Blks 160/162 Ang Mo Kio Ave 4 10092.54308
    ## 6319                  Ci Yuan Hawker Centre 10768.04420
    ## 6320                     Newton Food Centre  3562.60167
    ## 6321 North Bridge Road Market & Food Centre  3542.21508
    ## 6322              Pasir Panjang Food Centre  5963.94750
    ## 6323           Pek Kio Market & Food Centre  4030.97077
    ## 6324              People's Park Food Centre   594.71247
    ## 6325            Sembawang Hills Food Centre 10354.08906
    ## 6326                Serangoon Garden Market  9500.89426
    ## 6327      Taman Jurong Market & Food Centre 14986.93008
    ## 6328                    Tanglin Halt Market  5729.17780
    ## 6329                           Tekka Market  2954.28293
    ## 6330                     Tiong Bahru Market  1493.84161
    ## 6331             Zion Riverside Food Centre  2032.89647
    ## 6332              Blk 75 Toa Payoh Lorong 5  6249.65169
    ## 6333                    Blk 79 Redhill Lane  3063.09374
    ## 6334             Blk 79 Telok Blangah Drive  4206.79903
    ## 6335                    Blk 80 Circuit Road  7072.85765
    ## 6336             Blk 82 Telok Blangah Drive  4161.75388
    ## 6337           Blk 84 Marine Parade Central  7271.41977
    ## 6338            Blk 85 Bedok North Street 4 11921.41687
    ## 6339                    Blk 85 Redhill Lane  3054.14654
    ## 6340                    Blk 89 Circuit Road  6587.59670
    ## 6341                   Blk 90 Whampoa Drive  4873.01957
    ## 6342              Blk 93 Toa Payoh Lorong 4  6474.46112
    ## 6343                   Blks 13/14 Haig Road  6851.52111
    ## 6344          Blks 160/162 Ang Mo Kio Ave 4 10454.39518
    ## 6345                  Ci Yuan Hawker Centre 11362.66247
    ## 6346 North Bridge Road Market & Food Centre  2790.99082
    ## 6347              Pasir Panjang Food Centre  6675.32427
    ## 6348           Pek Kio Market & Food Centre  1277.00712
    ## 6349              People's Park Food Centre  2993.48944
    ## 6350            Sembawang Hills Food Centre  6791.57838
    ## 6351                Serangoon Garden Market  6424.88580
    ## 6352      Taman Jurong Market & Food Centre 13374.57645
    ## 6353                    Tanglin Halt Market  4825.96919
    ## 6354                           Tekka Market  1373.09852
    ## 6355                     Tiong Bahru Market  3070.67064
    ## 6356             Zion Riverside Food Centre  2354.06173
    ## 6357              Blk 75 Toa Payoh Lorong 5  3057.08730
    ## 6358                    Blk 79 Redhill Lane  3550.83466
    ## 6359             Blk 79 Telok Blangah Drive  5547.79806
    ## 6360                    Blk 80 Circuit Road  5579.93377
    ## 6361             Blk 82 Telok Blangah Drive  5478.91177
    ## 6362           Blk 84 Marine Parade Central  7507.07694
    ## 6363            Blk 85 Bedok North Street 4 11258.07976
    ## 6364                    Blk 85 Redhill Lane  3599.33299
    ## 6365                    Blk 89 Circuit Road  5264.08056
    ## 6366                   Blk 90 Whampoa Drive  2120.75568
    ## 6367              Blk 93 Toa Payoh Lorong 4  3141.04039
    ## 6368                   Blks 13/14 Haig Road  6247.81670
    ## 6369          Blks 160/162 Ang Mo Kio Ave 4  6921.73859
    ## 6370                  Ci Yuan Hawker Centre  8526.62085
    ## 6371              Pasir Panjang Food Centre  8716.76218
    ## 6372           Pek Kio Market & Food Centre  1906.34864
    ## 6373              People's Park Food Centre  3301.89698
    ## 6374            Sembawang Hills Food Centre  8327.50026
    ## 6375                Serangoon Garden Market  6353.98750
    ## 6376      Taman Jurong Market & Food Centre 16161.89588
    ## 6377                    Tanglin Halt Market  7395.21707
    ## 6378                           Tekka Market  1487.64692
    ## 6379                     Tiong Bahru Market  4189.29653
    ## 6380             Zion Riverside Food Centre  3930.43867
    ## 6381              Blk 75 Toa Payoh Lorong 5  3562.37867
    ## 6382                    Blk 79 Redhill Lane  5435.92073
    ## 6383             Blk 79 Telok Blangah Drive  7214.01971
    ## 6384                    Blk 80 Circuit Road  3556.84166
    ## 6385             Blk 82 Telok Blangah Drive  7153.55443
    ## 6386           Blk 84 Marine Parade Central  4739.67061
    ## 6387            Blk 85 Bedok North Street 4  8819.37894
    ## 6388                    Blk 85 Redhill Lane  5464.00878
    ## 6389                    Blk 89 Circuit Road  3094.85041
    ## 6390                   Blk 90 Whampoa Drive  2134.66270
    ## 6391              Blk 93 Toa Payoh Lorong 4  3954.96379
    ## 6392                   Blks 13/14 Haig Road  3679.08794
    ## 6393          Blks 160/162 Ang Mo Kio Ave 4  8080.13236
    ## 6394                  Ci Yuan Hawker Centre  7990.55882
    ## 6395           Pek Kio Market & Food Centre  7916.19370
    ## 6396              People's Park Food Centre  5783.36985
    ## 6397            Sembawang Hills Food Centre 11458.96835
    ## 6398                Serangoon Garden Market 12781.56143
    ## 6399      Taman Jurong Market & Food Centre 10128.69479
    ## 6400                    Tanglin Halt Market  2837.13232
    ## 6401                           Tekka Market  7384.21903
    ## 6402                     Tiong Bahru Market  4668.67609
    ## 6403             Zion Riverside Food Centre  4786.32870
    ## 6404              Blk 75 Toa Payoh Lorong 5  9545.37466
    ## 6405                    Blk 79 Redhill Lane  3282.60000
    ## 6406             Blk 79 Telok Blangah Drive  1827.32387
    ## 6407                    Blk 80 Circuit Road 12103.01838
    ## 6408             Blk 82 Telok Blangah Drive  1854.02647
    ## 6409           Blk 84 Marine Parade Central 13119.70865
    ## 6410            Blk 85 Bedok North Street 4 17529.88360
    ## 6411                    Blk 85 Redhill Lane  3253.02954
    ## 6412                    Blk 89 Circuit Road 11712.34582
    ## 6413                   Blk 90 Whampoa Drive  8795.59487
    ## 6414              Blk 93 Toa Payoh Lorong 4  9470.39361
    ## 6415                   Blks 13/14 Haig Road 12382.19895
    ## 6416          Blks 160/162 Ang Mo Kio Ave 4 12129.99009
    ## 6417                  Ci Yuan Hawker Centre 14998.84773
    ## 6418              People's Park Food Centre  3549.99692
    ## 6419            Sembawang Hills Food Centre  6650.91629
    ## 6420                Serangoon Garden Market  5512.05724
    ## 6421      Taman Jurong Market & Food Centre 14467.83558
    ## 6422                    Tanglin Halt Market  6095.55418
    ## 6423                           Tekka Market  1101.19655
    ## 6424                     Tiong Bahru Market  3970.17999
    ## 6425             Zion Riverside Food Centre  3379.96369
    ## 6426              Blk 75 Toa Payoh Lorong 5  2218.79987
    ## 6427                    Blk 79 Redhill Lane  4726.11457
    ## 6428             Blk 79 Telok Blangah Drive  6698.06504
    ## 6429                    Blk 80 Circuit Road  4305.59177
    ## 6430             Blk 82 Telok Blangah Drive  6630.52794
    ## 6431           Blk 84 Marine Parade Central  6430.59739
    ## 6432            Blk 85 Bedok North Street 4 10004.26744
    ## 6433                    Blk 85 Redhill Lane  4770.19795
    ## 6434                    Blk 89 Circuit Road  4002.01763
    ## 6435                   Blk 90 Whampoa Drive   928.15818
    ## 6436              Blk 93 Toa Payoh Lorong 4  2468.73097
    ## 6437                   Blks 13/14 Haig Road  5052.10196
    ## 6438          Blks 160/162 Ang Mo Kio Ave 4  6565.11801
    ## 6439                  Ci Yuan Hawker Centre  7499.12695
    ## 6440            Sembawang Hills Food Centre  9781.11513
    ## 6441                Serangoon Garden Market  9051.72490
    ## 6442      Taman Jurong Market & Food Centre 14546.20809
    ## 6443                    Tanglin Halt Market  5299.09782
    ## 6444                           Tekka Market  2507.21826
    ## 6445                     Tiong Bahru Market  1138.56075
    ## 6446             Zion Riverside Food Centre  1505.79153
    ## 6447              Blk 75 Toa Payoh Lorong 5  5761.21581
    ## 6448                    Blk 79 Redhill Lane  2709.21324
    ## 6449             Blk 79 Telok Blangah Drive  4096.67555
    ## 6450                    Blk 80 Circuit Road  6858.60173
    ## 6451             Blk 82 Telok Blangah Drive  4044.54609
    ## 6452           Blk 84 Marine Parade Central  7349.06967
    ## 6453            Blk 85 Bedok North Street 4 11896.07149
    ## 6454                    Blk 85 Redhill Lane  2710.06787
    ## 6455                    Blk 89 Circuit Road  6390.88659
    ## 6456                   Blk 90 Whampoa Drive  4423.17900
    ## 6457              Blk 93 Toa Payoh Lorong 4  5964.10130
    ## 6458                   Blks 13/14 Haig Road  6778.42151
    ## 6459          Blks 160/162 Ang Mo Kio Ave 4  9903.32189
    ## 6460                  Ci Yuan Hawker Centre 10964.37480
    ## 6461                Serangoon Garden Market  4322.53225
    ## 6462      Taman Jurong Market & Food Centre 12669.80530
    ## 6463                    Tanglin Halt Market  8652.20815
    ## 6464                           Tekka Market  7700.26696
    ## 6465                     Tiong Bahru Market  9660.03622
    ## 6466             Zion Riverside Food Centre  8854.09032
    ## 6467              Blk 75 Toa Payoh Lorong 5  4822.31170
    ## 6468                    Blk 79 Redhill Lane  9420.57890
    ## 6469             Blk 79 Telok Blangah Drive 11204.01623
    ## 6470                    Blk 80 Circuit Road  8130.66270
    ## 6471             Blk 82 Telok Blangah Drive 11137.60994
    ## 6472           Blk 84 Marine Parade Central 11583.31348
    ## 6473            Blk 85 Bedok North Street 4 13003.95262
    ## 6474                    Blk 85 Redhill Lane  9479.31789
    ## 6475                    Blk 89 Circuit Road  8292.05272
    ## 6476                   Blk 90 Whampoa Drive  6193.02010
    ## 6477              Blk 93 Toa Payoh Lorong 4  4390.24682
    ## 6478                   Blks 13/14 Haig Road  9748.07589
    ## 6479          Blks 160/162 Ang Mo Kio Ave 4  1160.92457
    ## 6480                  Ci Yuan Hawker Centre  6017.15489
    ## 6481      Taman Jurong Market & Food Centre 16466.93573
    ## 6482                    Tanglin Halt Market 10330.39352
    ## 6483                           Tekka Market  6550.57537
    ## 6484                     Tiong Bahru Market  9442.78014
    ## 6485             Zion Riverside Food Centre  8773.00267
    ## 6486              Blk 75 Toa Payoh Lorong 5  3368.80796
    ## 6487                    Blk 79 Redhill Lane  9912.66152
    ## 6488             Blk 79 Telok Blangah Drive 11910.02657
    ## 6489                    Blk 80 Circuit Road  4515.74572
    ## 6490             Blk 82 Telok Blangah Drive 11840.27344
    ## 6491           Blk 84 Marine Parade Central  8045.40211
    ## 6492            Blk 85 Bedok North Street 4  8722.13200
    ## 6493                    Blk 85 Redhill Lane  9965.52002
    ## 6494                    Blk 89 Circuit Road  4859.88932
    ## 6495                   Blk 90 Whampoa Drive  4630.26284
    ## 6496              Blk 93 Toa Payoh Lorong 4  3335.12742
    ## 6497                   Blks 13/14 Haig Road  6205.85040
    ## 6498          Blks 160/162 Ang Mo Kio Ave 4  3309.39023
    ## 6499                  Ci Yuan Hawker Centre  2260.07439
    ## 6500                    Tanglin Halt Market  9257.79800
    ## 6501                           Tekka Market 14696.69717
    ## 6502                     Tiong Bahru Market 13494.19018
    ## 6503             Zion Riverside Food Centre 13068.62019
    ## 6504              Blk 75 Toa Payoh Lorong 5 14626.73524
    ## 6505                    Blk 79 Redhill Lane 11956.40518
    ## 6506             Blk 79 Telok Blangah Drive 11732.80994
    ## 6507                    Blk 80 Circuit Road 18445.60016
    ## 6508             Blk 82 Telok Blangah Drive 11727.00253
    ## 6509           Blk 84 Marine Parade Central 20876.38373
    ## 6510            Blk 85 Bedok North Street 4 24175.23302
    ## 6511                    Blk 85 Redhill Lane 11975.34251
    ## 6512                    Blk 89 Circuit Road 18285.23383
    ## 6513                   Blk 90 Whampoa Drive 14925.96158
    ## 6514              Blk 93 Toa Payoh Lorong 4 14250.81931
    ## 6515                   Blks 13/14 Haig Road 19493.40291
    ## 6516          Blks 160/162 Ang Mo Kio Ave 4 13821.14374
    ## 6517                  Ci Yuan Hawker Centre 18527.22559
    ## 6518                           Tekka Market  5918.25197
    ## 6519                     Tiong Bahru Market  4236.45659
    ## 6520             Zion Riverside Food Centre  3850.33794
    ## 6521              Blk 75 Toa Payoh Lorong 5  7286.27482
    ## 6522                    Blk 79 Redhill Lane  2718.67210
    ## 6523             Blk 79 Telok Blangah Drive  3229.91312
    ## 6524                    Blk 80 Circuit Road 10400.51524
    ## 6525             Blk 82 Telok Blangah Drive  3185.89008
    ## 6526           Blk 84 Marine Parade Central 12099.88176
    ## 6527            Blk 85 Bedok North Street 4 16078.26437
    ## 6528                    Blk 85 Redhill Lane  2743.82833
    ## 6529                    Blk 89 Circuit Road 10089.86587
    ## 6530                   Blk 90 Whampoa Drive  6853.29110
    ## 6531              Blk 93 Toa Payoh Lorong 4  7121.11945
    ## 6532                   Blks 13/14 Haig Road 11019.56187
    ## 6533          Blks 160/162 Ang Mo Kio Ave 4  9371.95785
    ## 6534                  Ci Yuan Hawker Centre 12583.46497
    ## 6535                     Tiong Bahru Market  3089.85296
    ## 6536             Zion Riverside Food Centre  2642.63111
    ## 6537              Blk 75 Toa Payoh Lorong 5  3309.66880
    ## 6538                    Blk 79 Redhill Lane  4111.07587
    ## 6539             Blk 79 Telok Blangah Drive  5998.62855
    ## 6540                    Blk 80 Circuit Road  4724.04589
    ## 6541             Blk 82 Telok Blangah Drive  5934.11780
    ## 6542           Blk 84 Marine Parade Central  6225.98452
    ## 6543            Blk 85 Bedok North Street 4 10221.56006
    ## 6544                    Blk 85 Redhill Lane  4146.28712
    ## 6545                    Blk 89 Circuit Road  4328.42768
    ## 6546                   Blk 90 Whampoa Drive  1920.41524
    ## 6547              Blk 93 Toa Payoh Lorong 4  3569.92700
    ## 6548                   Blks 13/14 Haig Road  5113.20061
    ## 6549          Blks 160/162 Ang Mo Kio Ave 4  7654.73247
    ## 6550                  Ci Yuan Hawker Centre  8463.34943
    ## 6551             Zion Riverside Food Centre   812.41352
    ## 6552              Blk 75 Toa Payoh Lorong 5  6082.14949
    ## 6553                    Blk 79 Redhill Lane  1582.02111
    ## 6554             Blk 79 Telok Blangah Drive  3041.28106
    ## 6555                    Blk 80 Circuit Road  7715.18825
    ## 6556             Blk 82 Telok Blangah Drive  2983.95059
    ## 6557           Blk 84 Marine Parade Central  8451.12356
    ## 6558            Blk 85 Bedok North Street 4 12925.45132
    ## 6559                    Blk 85 Redhill Lane  1578.77537
    ## 6560                    Blk 89 Circuit Road  7274.06933
    ## 6561                   Blk 90 Whampoa Drive  4894.19348
    ## 6562              Blk 93 Toa Payoh Lorong 4  6204.59041
    ## 6563                   Blks 13/14 Haig Road  7785.25230
    ## 6564          Blks 160/162 Ang Mo Kio Ave 4  9914.97267
    ## 6565                  Ci Yuan Hawker Centre 11468.91821
    ## 6566              Blk 75 Toa Payoh Lorong 5  5404.27399
    ## 6567                    Blk 79 Redhill Lane  1506.95347
    ## 6568             Blk 79 Telok Blangah Drive  3358.31060
    ## 6569                    Blk 80 Circuit Road  7360.17861
    ## 6570             Blk 82 Telok Blangah Drive  3293.19797
    ## 6571           Blk 84 Marine Parade Central  8435.40121
    ## 6572            Blk 85 Bedok North Street 4 12746.10981
    ## 6573                    Blk 85 Redhill Lane  1533.74081
    ## 6574                    Blk 89 Circuit Road  6949.33201
    ## 6575                   Blk 90 Whampoa Drive  4307.29877
    ## 6576              Blk 93 Toa Payoh Lorong 4  5493.63504
    ## 6577                   Blks 13/14 Haig Road  7599.73201
    ## 6578          Blks 160/162 Ang Mo Kio Ave 4  9127.18915
    ## 6579                  Ci Yuan Hawker Centre 10845.51159
    ## 6580                    Blk 79 Redhill Lane  6569.49705
    ## 6581             Blk 79 Telok Blangah Drive  8570.58400
    ## 6582                    Blk 80 Circuit Road  3910.93210
    ## 6583             Blk 82 Telok Blangah Drive  8501.07311
    ## 6584           Blk 84 Marine Parade Central  7017.41891
    ## 6585            Blk 85 Bedok North Street 4  9558.15455
    ## 6586                    Blk 85 Redhill Lane  6620.99327
    ## 6587                    Blk 89 Circuit Road  3876.51974
    ## 6588                   Blk 90 Whampoa Drive  1468.54322
    ## 6589              Blk 93 Toa Payoh Lorong 4   467.01097
    ## 6590                   Blks 13/14 Haig Road  5280.86225
    ## 6591          Blks 160/162 Ang Mo Kio Ave 4  4517.77098
    ## 6592                  Ci Yuan Hawker Centre  5490.41407
    ## 6593             Blk 79 Telok Blangah Drive  2001.32291
    ## 6594                    Blk 80 Circuit Road  8834.86922
    ## 6595             Blk 82 Telok Blangah Drive  1931.92567
    ## 6596           Blk 84 Marine Parade Central  9915.66579
    ## 6597            Blk 85 Bedok North Street 4 14252.89403
    ## 6598                    Blk 85 Redhill Lane    58.73957
    ## 6599                    Blk 89 Circuit Road  8435.70286
    ## 6600                   Blk 90 Whampoa Drive  5636.01155
    ## 6601              Blk 93 Toa Payoh Lorong 4  6581.29338
    ## 6602                   Blks 13/14 Haig Road  9106.68024
    ## 6603          Blks 160/162 Ang Mo Kio Ave 4  9855.24058
    ## 6604                  Ci Yuan Hawker Centre 12058.60477
    ## 6605                    Blk 80 Circuit Road 10704.00381
    ## 6606             Blk 82 Telok Blangah Drive    69.86453
    ## 6607           Blk 84 Marine Parade Central 11441.64415
    ## 6608            Blk 85 Bedok North Street 4 15965.81301
    ## 6609                    Blk 85 Redhill Lane  1950.61245
    ## 6610                    Blk 89 Circuit Road 10279.73989
    ## 6611                   Blk 90 Whampoa Drive  7616.25448
    ## 6612              Blk 93 Toa Payoh Lorong 4  8576.60067
    ## 6613                   Blks 13/14 Haig Road 10826.53260
    ## 6614          Blks 160/162 Ang Mo Kio Ave 4 11721.00827
    ## 6615                  Ci Yuan Hawker Centre 14059.91336
    ## 6616             Blk 82 Telok Blangah Drive 10640.96039
    ## 6617           Blk 84 Marine Parade Central  3544.18889
    ## 6618            Blk 85 Bedok North Street 4  5761.19904
    ## 6619                    Blk 85 Redhill Lane  8870.31732
    ## 6620                    Blk 89 Circuit Road   521.63481
    ## 6621                   Blk 90 Whampoa Drive  3601.35863
    ## 6622              Blk 93 Toa Payoh Lorong 4  4347.90812
    ## 6623                   Blks 13/14 Haig Road  1692.70290
    ## 6624          Blks 160/162 Ang Mo Kio Ave 4  7418.21790
    ## 6625                  Ci Yuan Hawker Centre  5281.32798
    ## 6626           Blk 84 Marine Parade Central 11391.04259
    ## 6627            Blk 85 Bedok North Street 4 15909.16179
    ## 6628                    Blk 85 Redhill Lane  1881.34705
    ## 6629                    Blk 89 Circuit Road 10217.59856
    ## 6630                   Blk 90 Whampoa Drive  7548.38580
    ## 6631              Blk 93 Toa Payoh Lorong 4  8506.80992
    ## 6632                   Blks 13/14 Haig Road 10768.98867
    ## 6633          Blks 160/162 Ang Mo Kio Ave 4 11652.90335
    ## 6634                  Ci Yuan Hawker Centre 13990.47464
    ## 6635            Blk 85 Bedok North Street 4  4876.84899
    ## 6636                    Blk 85 Redhill Lane  9932.20551
    ## 6637                    Blk 89 Circuit Road  3298.35872
    ## 6638                   Blk 90 Whampoa Drive  6138.96508
    ## 6639              Blk 93 Toa Payoh Lorong 4  7483.79238
    ## 6640                   Blks 13/14 Haig Road  1856.56422
    ## 6641          Blks 160/162 Ang Mo Kio Ave 4 10933.77883
    ## 6642                  Ci Yuan Hawker Centre  8495.44378
    ## 6643                    Blk 85 Redhill Lane 14279.61540
    ## 6644                    Blk 89 Circuit Road  6004.64957
    ## 6645                   Blk 90 Whampoa Drive  9356.86031
    ## 6646              Blk 93 Toa Payoh Lorong 4  9955.03351
    ## 6647                   Blks 13/14 Haig Road  5147.95897
    ## 6648          Blks 160/162 Ang Mo Kio Ave 4 12031.13769
    ## 6649                  Ci Yuan Hawker Centre  7846.95505
    ## 6650                    Blk 89 Circuit Road  8469.31241
    ## 6651                   Blk 90 Whampoa Drive  5681.41527
    ## 6652              Blk 93 Toa Payoh Lorong 4  6634.65784
    ## 6653                   Blks 13/14 Haig Road  9132.83967
    ## 6654          Blks 160/162 Ang Mo Kio Ave 4  9913.58612
    ## 6655                  Ci Yuan Hawker Centre 12109.68872
    ## 6656                   Blk 90 Whampoa Drive  3373.82784
    ## 6657              Blk 93 Toa Payoh Lorong 4  4330.82896
    ## 6658                   Blks 13/14 Haig Road  1456.54570
    ## 6659          Blks 160/162 Ang Mo Kio Ave 4  7637.54856
    ## 6660                  Ci Yuan Hawker Centre  5754.67120
    ## 6661              Blk 93 Toa Payoh Lorong 4  1827.91834
    ## 6662                   Blks 13/14 Haig Road  4585.97064
    ## 6663          Blks 160/162 Ang Mo Kio Ave 4  5968.82826
    ## 6664                  Ci Yuan Hawker Centre  6578.87639
    ## 6665                   Blks 13/14 Haig Road  5742.26026
    ## 6666          Blks 160/162 Ang Mo Kio Ave 4  4143.95082
    ## 6667                  Ci Yuan Hawker Centre  5528.73299
    ## 6668          Blks 160/162 Ang Mo Kio Ave 4  9080.42054
    ## 6669                  Ci Yuan Hawker Centre  6812.82924
    ## 6670                  Ci Yuan Hawker Centre  4871.46984

TIDY DATA

``` r
unicef_hiv_tidy = read_excel("../data/unicef_hiv_data.xlsx", 
                             range = "A1:O219", na = c("-", "<0.01")) %>%
  filter(!(row_number() %in% c(1,2))) %>%
  gather(`2021`:`2010`, key = "years", value = "values") %>%
  mutate(values = as.numeric(values)) %>%
  spread(key = indicator, value = values) %>%
  rename(incidence = 4,
         deaths = 5,
         country = 1)



global_evolution = unicef_hiv_tidy %>%
  group_by(years) %>%
  summarize(total = sum(incidence, na.rm = TRUE)) %>%
  ungroup()

ggplot(data = global_evolution, aes(years, total)) + 
  geom_line(aes(group = 1))
```

![](EDA_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
iso_unicef = read.csv("../data/iso_unicef_region.csv")

overall = unicef_hiv_tidy %>% left_join(iso_unicef, by = c("country" = "Country.Region")) %>% 
  filter(!(is.na(UNICEF.Region)))

global_average = overall %>%
  group_by(years) %>%
  summarize(avg = mean(incidence, na.rm = TRUE)) %>%
  ungroup()


africa = overall %>% filter(UNICEF.Region == "Eastern and Southern Africa") %>%
  group_by(years) %>%
  summarize(avg = mean(incidence)) %>% 
  ungroup()

ggplot(data = africa) + 
  geom_line(aes(years, avg, group = 1), color = "indianred4", linewidth = 1.5) + 
  geom_line(data = global_average, aes(years, avg, group = 1), color = "grey", linewidth = 1.5) +
  annotate(geom = "text", x = 10, y = 1.2, label = "Africa") +
  annotate(geom = "text", x = 10, y = 0.63, label = "World") + 
  labs(y = "Average incidence rate",
  title = "HIV incidence rates in Eastern and Southern Africa to the global average")
```

![](EDA_files/figure-gfm/unnamed-chunk-36-2.png)<!-- -->

JOINS

``` r
themes = read.csv("../data/archive/themes.csv")
sets = read.csv("../data/archive/sets.csv")
inventory_sets = read.csv("../data/archive/inventory_sets.csv")
colors = read.csv("../data/archive/colors.csv")
inventories = read.csv("../data/archive/inventories.csv")
inventory_parts = read.csv("../data/archive/inventory_parts.csv")
parts = read.csv("../data/archive/parts.csv")
part_categories = read.csv("../data/archive/part_categories.csv")

part_cat = part_categories %>% 
  filter(str_detect(name, "Bricks"))

themes %>% anti_join(sets, by = c("id" = "theme_id")) %>% count()
```

    ##    n
    ## 1 39

``` r
brick_parts = inventory_parts %>% 
  left_join(colors, by = c("color_id" = "id")) %>%
  left_join(parts, by = c("part_num" = "part_num")) %>%
  left_join(part_cat, by = c("part_cat_id" = "id")) %>%
  rename(part_name = 2,
         part_cat_name = 3,
         color_name = 4) %>%
  na.omit()



inv_parts_col = inventory_parts %>% left_join(colors, by = c("color_id" = "id"))
parts_cat = parts %>% left_join(part_categories, by = c("part_cat_id" = "id"), 
                                suffix = c("_parts", "_cats")) %>%
  filter(str_detect(name_cats, "Bricks"))

table = inv_parts_col %>% left_join(parts_cat, by = c("part_num" = "part_num")) %>%
  filter(!(is.na(name_cats))) %>%
  select(part_num, name_parts, name_cats, name, rgb)


pop_themes = themes %>% 
  left_join(sets, by = c("id" = "theme_id"), suffix = c("_themes", "_sets")) %>% 
  filter(str_detect(name_themes, "Star Wars")) %>%
  group_by(year) %>%
  summarize(numSets = n_distinct(set_num),
            avg = ceiling(mean(num_parts)))

nrow(sets)
```

    ## [1] 11673

``` r
nrow(inventories)
```

    ## [1] 11681

``` r
sets %>% left_join(inventories) %>%
  group_by(set_num) %>%
  count() %>%
  filter(n > 1)
```

    ## Joining, by = "set_num"

    ## # A tibble: 8 × 2
    ## # Groups:   set_num [8]
    ##   set_num     n
    ##   <chr>   <int>
    ## 1 11905-1     2
    ## 2 214.6-1     2
    ## 3 31015-1     2
    ## 4 421-2       5
    ## 5 6515-1      2
    ## 6 75053-1     2
    ## 7 8030-1      2
    ## 8 8880-1      2

MIDTERMS

``` r
tset = tibble::tribble(
  ~abb, ~full, ~number, 
  "Jan", "January", 1,
  "Feb", "February", 2,
  "Mar", "March", 3,
  "Apr", "April", 4,
  "May", "May", 5
)

tset %>% mutate(a = match(abb, month.abb))
```

    ## # A tibble: 5 × 4
    ##   abb   full     number     a
    ##   <chr> <chr>     <dbl> <int>
    ## 1 Jan   January       1     1
    ## 2 Feb   February      2     2
    ## 3 Mar   March         3     3
    ## 4 Apr   April         4     4
    ## 5 May   May           5     5

``` r
tset %>% mutate(a = match(full, month.name))
```

    ## # A tibble: 5 × 4
    ##   abb   full     number     a
    ##   <chr> <chr>     <dbl> <int>
    ## 1 Jan   January       1     1
    ## 2 Feb   February      2     2
    ## 3 Mar   March         3     3
    ## 4 Apr   April         4     4
    ## 5 May   May           5     5
