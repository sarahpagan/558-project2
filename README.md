National Park Service API Vignette
================
by Sarah Pagan

- [Overview](#overview)
- [Requirements](#requirements)
- [API Interaction Functions](#api-interaction-functions)
  - [get_NPS_parks](#get_nps_parks)
  - [get_NPS_activities](#get_nps_activities)
  - [get_NPS_campgrounds](#get_nps_campgrounds)
  - [get_NPS_fees](#get_nps_fees)
- [Exploratory Data Analysis](#exploratory-data-analysis)
  - [NPS parks in NC](#nps-parks-in-nc)
  - [NPS parks with Climbing and
    Swimming](#nps-parks-with-climbing-and-swimming)
  - [Campground Amenities at grsm](#campground-amenities-at-grsm)
  - [Entrance Fees](#entrance-fees)

![](parks2.jpg)

# Overview

This document demonstrates how to retrieve and summarize data from the
United States National Parks Service (NPS)
[API](https://www.nps.gov/subjects/developer/api-documentation.htm). The
NPS API provides access to data about official NPS parks and their
activities, campgrounds, events, photos, and more. In this vignette, I
build functions to interact with four of the API’s endpoints. I then use
these functions to perform some exploratory data analysis.

# Requirements

To retrieve data from the NPS API you will need to sign up for a free
API key at the [Get Started with the NPS
API](https://www.nps.gov/subjects/developer/get-started.htm) web page.

Additional, helpful documentation about the NPS API is located
[here](https://www.nps.gov/subjects/developer/api-documentation.htm).

To develop my API interaction functions, I used the following packages:

`httr`

`jsonlite`

`dplyr`

In addition, I used the following packages to manipulate and visualize
data:

`stringr`

`tidyr`

`ggplot2`

`sf`

# API Interaction Functions

![](bear.jpg)

## get_NPS_parks

This function interacts with the parks endpoint to retrieve NPS parks
located in your state. The required input is `key` and the optional
input is `states`, a character vector of two-letter state codes
(e.g. `c("NC", "MI")`).

The output is a tibble of NPS parks. If no state(s) is supplied, the
function will return parks in all states. There are numerous site
designations within NPS (other than “National Park”) and descriptions
for each designation can be found
[here](https://www.nps.gov/goga/planyourvisit/designations.htm). 3

``` r
get_NPS_parks <- function(key, states = NULL){
  if(is.null(states)){
    url <- paste0("https://developer.nps.gov/api/v1/parks?api_key=",
                  key,
                  "&limit=10000")
  }
  
  else{
    url <- paste0("https://developer.nps.gov/api/v1/parks?api_key=",
                  key,
                  "&stateCode=",
                  paste(states, collapse = ","),
                  "&limit=10000")
  }

query <- GET(url)
results <- fromJSON(rawToChar(query$content))$data |>
  select(fullName, parkCode, states, designation, latitude, longitude) |>
  mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude)) |>
  as_tibble()

return(results)
}
```

## get_NPS_activities

This function interacts with the activities/parks endpoint to retrieve
parks associated with your activities. The required inputs are `key` and
`activities`, a character vector of activities
(e.g. `c("Hiking", "Horse Trekking")`). The output is a tibble of NPS
parks related to the input activities.

``` r
get_NPS_activities <- function(key, activities){
  acts <- paste(activities, collapse = ",")
  acts <- sub(" ", "%20", acts)
  
  url <- paste0("https://developer.nps.gov/api/v1/activities/parks?api_key=",
                key,
                "&q=",
                acts,
                "&limit=10000")
  
  query <- GET(url)
  results <- fromJSON(rawToChar(query$content))$data$parks
  
  activity <- c()
  for (i in 1:length(activities)){
    y <- rep(activities[i],
             nrow(results[[i]]))
    activity <- append(activity, y)
  }
  
  results <- results |>
    bind_rows() |>
    cbind(activity) |>
    select(fullName, parkCode, states, activity) |>
    as_tibble()
  
  return(results)
}
```

For reference, here are the categories of activities defined by NPS:

``` r
get_activities <- GET(paste0("https://developer.nps.gov/api/v1/activities?api_key=",
                             my_key))
NPS_activities <- fromJSON(rawToChar(get_activities$content))$data$name
NPS_activities
```

    ##  [1] "Arts and Culture"      "Astronomy"             "Auto and ATV"         
    ##  [4] "Biking"                "Boating"               "Camping"              
    ##  [7] "Canyoneering"          "Caving"                "Climbing"             
    ## [10] "Compass and GPS"       "Dog Sledding"          "Fishing"              
    ## [13] "Flying"                "Food"                  "Golfing"              
    ## [16] "Guided Tours"          "Hands-On"              "Hiking"               
    ## [19] "Horse Trekking"        "Hunting and Gathering" "Ice Skating"          
    ## [22] "Junior Ranger Program" "Living History"        "Museum Exhibits"      
    ## [25] "Paddling"              "Park Film"             "Playground"           
    ## [28] "SCUBA Diving"          "Shopping"              "Skiing"               
    ## [31] "Snorkeling"            "Snow Play"             "Snowmobiling"         
    ## [34] "Snowshoeing"           "Surfing"               "Swimming"             
    ## [37] "Team Sports"           "Tubing"                "Water Skiing"         
    ## [40] "Wildlife Watching"

## get_NPS_campgrounds

This function interacts with the campgrounds endpoint to retrieve data
about amenities at campgrounds in your park.

The preceding function, `get_NPS_codes`, retrieves park codes for all
parks. It can be used to look up the official code for your park and is
used in the main campgrounds function to join park names by `parkCode`.

Below, I look up the `parkCode` for each of my parks of interest –
Joshua Tree and the New River Gorge.

``` r
get_NPS_codes <- function(key) {
  url <- paste0("https://developer.nps.gov/api/v1/parks?api_key=",
                key,
                "&limit=10000")
  query <- GET(url)
  results <- fromJSON(rawToChar(query$content))$data
  results |>
    select(fullName, parkCode, states) |>
    as_tibble()
}
```

``` r
library(stringr)
all_codes <- get_NPS_codes(my_key) 
my_codes <- all_codes |>
  filter(str_detect(fullName, "Joshua Tree|New River"))
my_codes
```

    ## # A tibble: 2 × 3
    ##   fullName                                 parkCode states
    ##   <chr>                                    <chr>    <chr> 
    ## 1 Joshua Tree National Park                jotr     CA    
    ## 2 New River Gorge National Park & Preserve neri     WV

Now for the `get_NPS_campgrounds` function, the required input is `key`
and the optional input is `park_codes`, a character vector of park codes
(e.g. `c("jotr", "neri")`).

``` r
get_NPS_campgrounds <- function(key, park_codes = NULL){
  parks <- get_NPS_codes(key)
  
  if(is.null(park_codes)){
    url <- paste0("https://developer.nps.gov/api/v1/campgrounds?api_key=",
                  key,
                  "&limit=10000")
  }
  
  if(!is.null(park_codes)){
    url <- paste0("https://developer.nps.gov/api/v1/campgrounds?api_key=",
                  key,
                  "&parkCode=",
                  paste(park_codes, collapse = ","),
                  "&limit=10000")
  }
  
  query <- GET(url)
  camps <- fromJSON(rawToChar(query$content))$data |>
    select(name, parkCode) |>
    left_join(parks, by = "parkCode")
  results <- cbind(camps, fromJSON(rawToChar(GET(url)$content))$data$amenities)|>
    as_tibble()
  
  return(results)
}
```

## get_NPS_fees

This function interacts with the feespasses endpoint to retrieve data on
fees associated with your park. The required inputs are key an
park_codes, a character vector of park codes (e.g. `c("jotr", "neri")`).

``` r
get_NPS_fees <- function(key, park_codes){
  url <- paste0("https://developer.nps.gov/api/v1/feespasses?api_key=",
                key,
                "&parkCode=",
                paste(park_codes, collapse = ","),
                "&limit=10000")
  query <- GET(url)
  results <- fromJSON(rawToChar(query$content))$data$fees |>
    bind_rows()
  
  if(length(results) > 0){
    results <- results |>
      select(entranceFeeType, cost, description) |>
      mutate(cost = as.numeric(cost))
  }
  
  else{
    stop(message = "No fees!")
  }
  
  return(results)
}
```

# Exploratory Data Analysis

![](bears.jpeg)

## NPS parks in NC

First, let’s pull data on all NPS parks in North Carolina using the
`get_NPS_parks` function.

``` r
NC_parks <- get_NPS_parks(my_key, "NC")
NC_parks
```

    ## # A tibble: 12 × 6
    ##    fullName                                parkC…¹ states desig…² latit…³ longi…⁴
    ##    <chr>                                   <chr>   <chr>  <chr>     <dbl>   <dbl>
    ##  1 Appalachian National Scenic Trail       appa    CT,GA… Nation…    40.4   -76.4
    ##  2 Blue Ridge Parkway                      blri    NC,VA  Parkway    35.6   -82.5
    ##  3 Cape Hatteras National Seashore         caha    NC     Nation…    35.4   -75.7
    ##  4 Cape Lookout National Seashore          calo    NC     Nation…    34.8   -76.3
    ##  5 Carl Sandburg Home National Historic S… carl    NC     Nation…    35.3   -82.5
    ##  6 Fort Raleigh National Historic Site     fora    NC     Nation…    35.9   -75.7
    ##  7 Great Smoky Mountains National Park     grsm    NC,TN  Nation…    35.6   -83.5
    ##  8 Guilford Courthouse National Military … guco    NC     Nation…    36.1   -79.8
    ##  9 Moores Creek National Battlefield       mocr    NC     Nation…    34.5   -78.1
    ## 10 Overmountain Victory National Historic… ovvi    NC,SC… Nation…    35.1   -81.4
    ## 11 Trail Of Tears National Historic Trail  trte    AL,AR… Nation…    36.1   -89.7
    ## 12 Wright Brothers National Memorial       wrbr    NC     Nation…    36.0   -75.7
    ## # … with abbreviated variable names ¹​parkCode, ²​designation, ³​latitude,
    ## #   ⁴​longitude

### Which designation categories are represented by NC’s NPS parks?

North Carolina has twelve NPS parks in total. It turns out it’s a pretty
diverse set of parks as well, including nine unique designations.

``` r
NC_parks |>
  group_by(designation) |>
  summarise(count = n()) |>
  arrange(desc(count))
```

    ## # A tibble: 9 × 2
    ##   designation             count
    ##   <chr>                   <int>
    ## 1 National Historic Site      2
    ## 2 National Historic Trail     2
    ## 3 National Seashore           2
    ## 4 National Battlefield        1
    ## 5 National Memorial           1
    ## 6 National Military Park      1
    ## 7 National Park               1
    ## 8 National Scenic Trail       1
    ## 9 Parkway                     1

### Where are NC’s NPS parks located?

The data pull from `get_NPS_parks` includes information on latitude and
longitude coordinates for each park. I use this data to plot the parks
on a map of NC below. I exclude the Trail Of Tears National Historic
Trail and the Appalachian National Scenic Trail since their recorded
geographic coordinates lie outside the state.

``` r
NC <- NC_parks |>
  filter(parkCode != "trte", parkCode != "appa")

NC_map <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
ggplot(NC_map) +
  geom_sf(color = "seagreen3", fill = "honeydew", linewidth = 0.3) +
  geom_point(data = NC, mapping = aes(x = longitude,
                                      y = latitude),
             color = "grey20",
             shape = 17,
             size = 3) +
  ggrepel::geom_text_repel(data = NC, aes(x = longitude,
                                          y = latitude,
                                          label = parkCode),
                           size = 5) +
  labs(title = "NPS Parks in North Carolina") +
  xlab(NULL) + ylab(NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 20))
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- --> \## National
NPS Statistics

Let’s use the `get_NPS_parks` function again to pull data on all NPS
parks. This time, I don’t specify a state and retrieve data on all NPS
parks in the nation.

``` r
all_parks <- get_NPS_parks(my_key)
all_parks
```

    ## # A tibble: 471 × 6
    ##    fullName                                parkC…¹ states desig…² latit…³ longi…⁴
    ##    <chr>                                   <chr>   <chr>  <chr>     <dbl>   <dbl>
    ##  1 Abraham Lincoln Birthplace National Hi… abli    KY     "Natio…    37.6   -85.7
    ##  2 Acadia National Park                    acad    ME     "Natio…    44.4   -68.2
    ##  3 Adams National Historical Park          adam    MA     "Natio…    42.3   -71.0
    ##  4 African American Civil War Memorial     afam    DC     ""         38.9   -77.0
    ##  5 African Burial Ground National Monument afbg    NY     "Natio…    40.7   -74.0
    ##  6 Agate Fossil Beds National Monument     agfo    NE     "Natio…    42.4  -104. 
    ##  7 Ala Kahakai National Historic Trail     alka    HI     "Natio…    19.1  -156. 
    ##  8 Alagnak Wild River                      alag    AK     "Wild …    59.1  -156. 
    ##  9 Alaska Public Lands                     anch    AK     ""         61.2  -150. 
    ## 10 Alcatraz Island                         alca    CA     ""         37.8  -122. 
    ## # … with 461 more rows, and abbreviated variable names ¹​parkCode, ²​designation,
    ## #   ³​latitude, ⁴​longitude

### Nationally, which NPS designation categories have the most parks?

The National Monument and National Historic Site categories have the
most NPS units, followed by National Historical Park, National Park, and
National Historic Trail. I plot counts for these top five designations
below.

``` r
all_des <- all_parks |>
  group_by(designation) |>
  filter(!designation == "") |>
  summarise(count = n()) |>
  arrange(desc(count))

all_des
```

    ## # A tibble: 46 × 2
    ##    designation              count
    ##    <chr>                    <int>
    ##  1 National Monument           80
    ##  2 National Historic Site      78
    ##  3 National Historical Park    63
    ##  4 National Park               51
    ##  5 National Historic Trail     18
    ##  6 National Memorial           17
    ##  7 National Recreation Area    16
    ##  8 National Battlefield        11
    ##  9 Park                        11
    ## 10 National Seashore           10
    ## # … with 36 more rows

``` r
all_des_5 <- all_des |>
  slice(1:5)

ggplot(data = all_des_5, aes(x = reorder(designation, count))) +
         geom_col(aes(y = count), color = "seagreen1", linewidth = 1.5, fill = "seagreen4") +
  xlab(NULL) +
  ylab("count") +
  labs(title = "NPS parks by Designation: Top 5") +
  coord_flip()
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- --> \### Which
states have the most National Monuments?

The National Monument designation category has the most NPS units,
therefore I break down the total count of National Monuments by state in
the summary table below. The southwest region of the country has the
highest concentration of monuments. Arizona leads the count with
thirteen monuments, and is followed by New Mexico with nine monuments.

``` r
all_parks |>
  filter(designation == "National Monument") |>
  separate_rows(states) |>
  group_by(states) |>
  summarise(count = n()) |>
  arrange(desc(count)) |>
  slice(1:10)
```

    ## # A tibble: 10 × 2
    ##    states count
    ##    <chr>  <int>
    ##  1 AZ        13
    ##  2 NM         9
    ##  3 CA         7
    ##  4 NY         6
    ##  5 UT         6
    ##  6 CO         5
    ##  7 AL         3
    ##  8 VA         3
    ##  9 FL         2
    ## 10 GA         2

## NPS parks with Climbing and Swimming

Next, let’s use the `get_NPS_activities` function to pull data on all
parks with “Climbing” and/or “Swimming” recorded as possible activities.

``` r
climb_swim <- get_NPS_activities(my_key, c("Climbing", "Swimming"))
climb_swim
```

    ## # A tibble: 91 × 4
    ##    fullName                                         parkCode states activity
    ##    <chr>                                            <chr>    <chr>  <chr>   
    ##  1 Acadia National Park                             acad     ME     Climbing
    ##  2 Aniakchak National Monument & Preserve           ania     AK     Climbing
    ##  3 Arches National Park                             arch     UT     Climbing
    ##  4 Big South Fork National River & Recreation Area  biso     KY,TN  Climbing
    ##  5 Black Canyon Of The Gunnison National Park       blca     CO     Climbing
    ##  6 Canyonlands National Park                        cany     UT     Climbing
    ##  7 Capitol Reef National Park                       care     UT     Climbing
    ##  8 Catoctin Mountain Park                           cato     MD     Climbing
    ##  9 Chickamauga & Chattanooga National Military Park chch     GA,TN  Climbing
    ## 10 City Of Rocks National Reserve                   ciro     ID     Climbing
    ## # … with 81 more rows

### How many parks have climbing, and how many parks have swimming?

52 NPS parks have swimming listed as a possible activity, and 39 parks
have climbing listed as a possible activity.

``` r
climb_swim |>
  group_by(activity) |>
  summarise(count = n())
```

    ## # A tibble: 2 × 2
    ##   activity count
    ##   <chr>    <int>
    ## 1 Climbing    39
    ## 2 Swimming    52

### Which states have the most NPS parks with climbing availability? And swimming?

Alaska is unsurprisingly dominated by climbing destinations. California
has the highest number of total parks, with an even mix of climbing and
swimming availability. Unfortunately, North Carolina has no parks with
climbing availability, but only swimming. It’s a good thing we have have
great state parks for climbing!

``` r
climb_swim_states <- climb_swim |>
  separate_rows(states, sep = ",")

ggplot(data = climb_swim_states, aes(x = states)) + 
  geom_bar(aes(fill = activity)) +
  scale_fill_manual(values = c("grey20", "turquoise3")) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("state") +
  labs(title = "NPS parks with Climbing and/or Swimming by State")
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

### Which NPS parks have BOTH climbing and swimming recorded as possible activities?

Only nine parks have both climbing and swimming!

``` r
climb_and_swim <- climb_swim |>
  group_by(fullName) |>
  filter(n() > 1) |>
  select(-activity) |>
  distinct(fullName, states)

climb_and_swim
```

    ## # A tibble: 9 × 2
    ## # Groups:   fullName [9]
    ##   fullName                                        states
    ##   <chr>                                           <chr> 
    ## 1 Acadia National Park                            ME    
    ## 2 Big South Fork National River & Recreation Area KY,TN 
    ## 3 Delaware Water Gap National Recreation Area     NJ,PA 
    ## 4 Glacier National Park                           MT    
    ## 5 Great Sand Dunes National Park & Preserve       CO    
    ## 6 Little River Canyon National Preserve           AL    
    ## 7 Olympic National Park                           WA    
    ## 8 Pictured Rocks National Lakeshore               MI    
    ## 9 Yosemite National Park                          CA

## Campground Amenities at grsm

I want to visit Great Smoky Mountains National Park. Let’s use the
`get_NPS_camgrounds` function to see how many campgrounds the park has
available.

``` r
get_NPS_codes(my_key) |>
  filter(str_detect(fullName, "Great Smoky"))
```

    ## # A tibble: 1 × 3
    ##   fullName                            parkCode states
    ##   <chr>                               <chr>    <chr> 
    ## 1 Great Smoky Mountains National Park grsm     NC,TN

Great Smoky Mountains National Park has 13 campgrounds recorded.

``` r
my_camps <- get_NPS_campgrounds(my_key, "grsm")
my_camps |>
  select(name)
```

    ## # A tibble: 13 × 1
    ##    name                       
    ##    <chr>                      
    ##  1 Abrams Creek Campground    
    ##  2 Balsam Mountain Campground 
    ##  3 Big Creek Campground       
    ##  4 Cades Cove Campground      
    ##  5 Cades Cove Group Campground
    ##  6 Cataloochee Campground     
    ##  7 Cosby Campground           
    ##  8 Deep Creek Campground      
    ##  9 Elkmont Campground         
    ## 10 Elkmont Group Campground   
    ## 11 Look Rock Campground       
    ## 12 Smokemont Campground       
    ## 13 Smokemont Group Campground

### Which of the Great Smoky Mountains campgrounds have the best amenities?

I choose the following amenities to investigate: cell phone reception,
potable water, bathrooms, trash/recycling collection, camp store, food
storage lockers, and firewood. I code no availability as 0, seasonal
availability as 0.5, and year round availability as 1 and create a heat
map with `geom_tile()`. Based on the results, it seems Cades Cove
Campground/Group Campground offer the most amenities at any time of
year.

``` r
my_camps_heat <- my_camps |>
  slice(1:12) |>
  select(name,
         cellPhoneReception,
         potableWater,
         toilets,
         trashRecyclingCollection,
         campStore,
         foodStorageLockers,
         firewoodForSale) |>
  mutate(potableWater = unlist(potableWater)) |>
  mutate(toilets = unlist(toilets)) |>
  pivot_longer(!name, names_to = "var", values_to = "value") |>
  mutate(value = recode(value, 
                        "No" = "0",
                        "No water" = "0",
                        "Yes - year round" = "1",
                        "Yes - seasonal" = "0.5",
                        "Flush Toilets - year round" = "1",
                        "Flush Toilets - seasonal" = "0.5")) |>
  mutate(value = as.numeric(value))

ggplot(my_camps_heat, aes(x = name, y = var, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "turquoise1", name = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab(NULL) + ylab(NULL) +
  labs(title = "Campground Amenities:
Great Smoky Mountains National Park")
```

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

## Entrance Fees

### Does Great Smoky Mountains National Park have entrance fees?

Finally, let’s use the `get_NPS_fees` to pull entrance fees for grsm.
Good news…no fees!

``` r
get_NPS_fees(my_key, "grsm")
```

    ## Error in get_NPS_fees(my_key, "grsm"): No fees!

### Does Yosemite National Park have entrance fees?

Let’s try another park. It looks like Yosemite has entrance fees for all
visitors with the exception of Education/Academic Groups.

``` r
get_NPS_fees(my_key, "yose")
```

    ##                         entranceFeeType cost
    ## 1            Entrance - Private Vehicle   35
    ## 2                 Entrance - Motorcycle   30
    ## 3                 Entrance - Per Person   20
    ## 4           Commercial Entrance - Sedan   25
    ## 5             Commercial Entrance - Van  125
    ## 6        Commercial Entrance - Mini-bus  200
    ## 7     Commercial Entrance - Motor Coach  300
    ## 8      Entrance - Non-commercial Groups   20
    ## 9  Entrance - Education/Academic Groups    0
    ## 10     Commercial Entrance - Per Person   20
    ##                                                                                                                                                                                                                                               description
    ## 1                                                                This fee is valid for seven consecutive days for a non-commercial car, pickup truck, RV, or van with 15 or fewer passenger seats. This fee covers the vehicle and everyone inside of it.
    ## 2                                                                                                                                      This fee is valid for seven consecutive days for a non-commercial motorcycle (cost is per motorcycle, not person).
    ## 3                                                                                     This fee is valid for seven consecutive days for people entering on foot, a bicycle, a horse, or a non-commercial bus or van. People 15 years and younger are free.
    ## 4                                                                                                       The fee is $25 plus $20 per person, not to exceed $105. This fee is valid for seven consecutive days. A commercial use authorization is required.
    ## 5                                                                                                                                                               This fee is valid for seven consecutive days. A commercial use authorization is required.
    ## 6                                                                                                                                                               This fee is valid for seven consecutive days. A commercial use authorization is required.
    ## 7                                                                                                                                                               This fee is valid for seven consecutive days. A commercial use authorization is required.
    ## 8                                                                                                                                                                      This fee is valid for seven consecutive days. People 15 years and younger are free
    ## 9  An educational fee waiver is available for educational and scientific groups that are accredited or tax-exempt for educational purposes planning a trip for educational or scientific purposes related to Yosemite that is not primarily recreational.
    ## 10                                                                                                      The fee is $25 plus $20 per person, not to exceed $105. This fee is valid for seven consecutive days. A commercial use authorization is required.

### On avergae, what is the cost of non-commerical, non-group entrance to NPS parks?

I use the `get_NPS_fees` function to pull data on fees for all park
codes. The function will exclude parks that don’t have any fees listed.

``` r
all_codes <- as.vector(get_NPS_codes(my_key)$parkCode)

fees <- get_NPS_fees(my_key, all_codes) |>
  select(entranceFeeType, cost) |>
  filter(entranceFeeType == "Entrance - Private Vehicle"|
           entranceFeeType == "Entrance - Motorcycle"|
           entranceFeeType == "Entrance - Per Person"
  )
```

The average entrance fee, among all parks that have fees, is highest for
private vehicles at 25.8 dollars.

``` r
fees |>
  group_by(entranceFeeType) |>
  summarise(averageCost = mean(cost), sdCost = sd(cost))
```

    ## # A tibble: 3 × 3
    ##   entranceFeeType            averageCost sdCost
    ##   <chr>                            <dbl>  <dbl>
    ## 1 Entrance - Motorcycle             21.3   5.24
    ## 2 Entrance - Per Person             13.5   3.98
    ## 3 Entrance - Private Vehicle        25.8   6.15

Per person entrance is typically between 10 and 15 dollars and has the
smallest variance among the three individual entrance types, with a few
outliers at 0 and 30 dollars.

``` r
ggplot(data = fees, aes(x = entranceFeeType, y = cost)) +
  geom_boxplot(aes(fill = entranceFeeType)) +
  xlab(NULL) +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("seagreen2", "deeppink2", "turquoise2"), name = "Fee Type") +
  labs(title = "NPS Fees by Type of Entrance")
```

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->
