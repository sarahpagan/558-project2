NPS API Vignette
================
Sarah Pagan

- [Requirements](#requirements)
- [API Interaction Functions](#api-interaction-functions)
  - [get_NPS_parks](#get_nps_parks)
  - [get_NPS_activities](#get_nps_activities)
  - [get_NPS_campgrounds](#get_nps_campgrounds)
- [get_NPS_fees](#get_nps_fees)
- [Exploratory Data Analysis](#exploratory-data-analysis)
  - [NPS Sites in NC](#nps-sites-in-nc)
  - [NPS Sites with Climbing and
    Swimming](#nps-sites-with-climbing-and-swimming)
  - [Campground Amenities at grsm](#campground-amenities-at-grsm)

This document demonstrates how to retrieve data from the National Parks
Service (NPS)
[API](https://www.nps.gov/subjects/developer/api-documentation.htm). The
NPS API provides access to data about NPS sites and their activities,
campgrounds, events, photos, and more. In this vignette, I build
functions interacting with four of the API’s endpoints. I then use these
functions to perform some exploratory data analysis.

![](parks.png)

# Requirements

# API Interaction Functions

## get_NPS_parks

This function interacts with the parks endpoint to retrieve parks
located in your state. The required input is `key` and the optional
input is `states`, a character vector of two-letter state codes
(e.g. `c("NC", "MI")`).

The output is a tibble of NPS sites. If no state(s) is supplied, the
function will return sites in all states. There are numerous site
designations within NPS (other than “National Park”) and descriptions
for each designation can be found
[here](https://www.nps.gov/goga/planyourvisit/designations.htm).

``` r
get_NPS_parks <- function(key, states = NULL){
  if(is.null(states)){
    url <- paste0("https://developer.nps.gov/api/v1/parks?api_key=",
                  key,
                  "&limit=10000")
  }
  
  if(!is.null(states)){
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
`activities`, a character vector of activities (e.g. \`c(“Hiking”,
“Horse Trekking”)). The output is a tibble of NPS sites related to the
input activities.

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

These are the categories of activities defined by NPS:

``` r
get_activities <- GET(paste0("https://developer.nps.gov/api/v1/activities?api_key=",
                             my_key))
NPS_activities <- fromJSON(rawToChar(get_activities$content))$data$name
NPS_activities
```

    ##  [1] "Arts and Culture"      "Astronomy"            
    ##  [3] "Auto and ATV"          "Biking"               
    ##  [5] "Boating"               "Camping"              
    ##  [7] "Canyoneering"          "Caving"               
    ##  [9] "Climbing"              "Compass and GPS"      
    ## [11] "Dog Sledding"          "Fishing"              
    ## [13] "Flying"                "Food"                 
    ## [15] "Golfing"               "Guided Tours"         
    ## [17] "Hands-On"              "Hiking"               
    ## [19] "Horse Trekking"        "Hunting and Gathering"
    ## [21] "Ice Skating"           "Junior Ranger Program"
    ## [23] "Living History"        "Museum Exhibits"      
    ## [25] "Paddling"              "Park Film"            
    ## [27] "Playground"            "SCUBA Diving"         
    ## [29] "Shopping"              "Skiing"               
    ## [31] "Snorkeling"            "Snow Play"            
    ## [33] "Snowmobiling"          "Snowshoeing"          
    ## [35] "Surfing"               "Swimming"             
    ## [37] "Team Sports"           "Tubing"               
    ## [39] "Water Skiing"          "Wildlife Watching"

## get_NPS_campgrounds

This function interacts with the campgrounds endpoint to retrieve data
about amenities at campgrounds in your park.

The preceding function, `get_NPS_codes`, retrieves park codes for all
sites. It can be used to look up the official code for your park and is
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
all_codes <- get_NPS_codes(my_key) 
my_codes <- all_codes |>
  filter(grepl("Joshua Tree|New River", all_codes$fullName))
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
    left_join(parks)
  results <- cbind(camps, fromJSON(rawToChar(GET(url)$content))$data$amenities)|>
    as_tibble()
  
  return(results)
}
```

# get_NPS_fees

``` r
get_NPS_fees <- function(key, park_codes){
  url <- paste0("https://developer.nps.gov/api/v1/feespasses?api_key=",
                key,
                "&parkCode=",
                paste(park_codes, collapse = ","),
                "&limit=10000")
  query <- GET(url)
  fromJSON(rawToChar(query$content))$data$fees
}
```

# Exploratory Data Analysis

![](bear.jpg)

## NPS Sites in NC

First, let’s pull data on all NPS sites in North Carolina using the
`get_NPS_parks` function.

``` r
NC_parks <- get_NPS_parks(my_key, "NC")
NC_parks
```

    ## # A tibble: 12 × 6
    ##    fullName                   parkC…¹ states desig…² latit…³ longi…⁴
    ##    <chr>                      <chr>   <chr>  <chr>     <dbl>   <dbl>
    ##  1 Appalachian National Scen… appa    CT,GA… Nation…    40.4   -76.4
    ##  2 Blue Ridge Parkway         blri    NC,VA  Parkway    35.6   -82.5
    ##  3 Cape Hatteras National Se… caha    NC     Nation…    35.4   -75.7
    ##  4 Cape Lookout National Sea… calo    NC     Nation…    34.8   -76.3
    ##  5 Carl Sandburg Home Nation… carl    NC     Nation…    35.3   -82.5
    ##  6 Fort Raleigh National His… fora    NC     Nation…    35.9   -75.7
    ##  7 Great Smoky Mountains Nat… grsm    NC,TN  Nation…    35.6   -83.5
    ##  8 Guilford Courthouse Natio… guco    NC     Nation…    36.1   -79.8
    ##  9 Moores Creek National Bat… mocr    NC     Nation…    34.5   -78.1
    ## 10 Overmountain Victory Nati… ovvi    NC,SC… Nation…    35.1   -81.4
    ## 11 Trail Of Tears National H… trte    AL,AR… Nation…    36.1   -89.7
    ## 12 Wright Brothers National … wrbr    NC     Nation…    36.0   -75.7
    ## # … with abbreviated variable names ¹​parkCode, ²​designation,
    ## #   ³​latitude, ⁴​longitude

### What types (designations) of NPS sites are present in North Carolina?

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

``` r
NC <- NC_parks |>
  filter(parkCode != "trte", parkCode != "appa")

NC_map <- st_read(system.file("shape/nc.shp", package="sf"))
```

    ## Reading layer `nc' from data source 
    ##   `/Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library/sf/shape/nc.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 100 features and 14 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
    ## Geodetic CRS:  NAD27

``` r
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
  theme_void() +
  labs(title = "NPS Sites in North Carolina") +
  theme(plot.title = element_text(hjust = 0.5, size = 20))
```

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
all_parks <- get_NPS_parks(my_key)
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

ggplot(data = all_des_5, aes(x = reorder(designation, -count))) +
         geom_col(aes(y = count), color = "seagreen1", linewidth = 2, fill = "seagreen4") +
  xlab(NULL) +
  ylab("count") +
  labs(title = "NPS Sites by Designation: Top 5")
```

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

## NPS Sites with Climbing and Swimming

Next, let’s use the `get_NPS_activities` function to pull data on all
parks with “Climbing” and/or “Swimming” recorded as possible activities.

``` r
climb_swim <- get_NPS_activities(my_key, c("Climbing", "Swimming"))
climb_swim
```

    ## # A tibble: 91 × 4
    ##    fullName                                   parkC…¹ states activ…²
    ##    <chr>                                      <chr>   <chr>  <chr>  
    ##  1 Acadia National Park                       acad    ME     Climbi…
    ##  2 Aniakchak National Monument & Preserve     ania    AK     Climbi…
    ##  3 Arches National Park                       arch    UT     Climbi…
    ##  4 Big South Fork National River & Recreatio… biso    KY,TN  Climbi…
    ##  5 Black Canyon Of The Gunnison National Park blca    CO     Climbi…
    ##  6 Canyonlands National Park                  cany    UT     Climbi…
    ##  7 Capitol Reef National Park                 care    UT     Climbi…
    ##  8 Catoctin Mountain Park                     cato    MD     Climbi…
    ##  9 Chickamauga & Chattanooga National Milita… chch    GA,TN  Climbi…
    ## 10 City Of Rocks National Reserve             ciro    ID     Climbi…
    ## # … with 81 more rows, and abbreviated variable names ¹​parkCode,
    ## #   ²​activity

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

``` r
climb_swim_states <- climb_swim |>
  separate_rows(states, sep = ",")

ggplot(data = climb_swim_states, aes(x = states)) + 
  geom_bar(aes(fill = activity)) +
  scale_fill_manual(values = c("grey10", "turquoise3")) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("state") +
  labs(title = "NPS Sites with Climbing and/or Swimming by State")
```

![](README_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

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

``` r
my_camps <- get_NPS_campgrounds(my_key, "grsm")
my_camps
```

    ## # A tibble: 13 × 18
    ##    name       parkC…¹ fullN…² states trash…³ toilets inter…⁴ showers
    ##    <chr>      <chr>   <chr>   <chr>  <chr>   <list>  <chr>   <list> 
    ##  1 Abrams Cr… grsm    Great … NC,TN  "Yes -… <chr>   "No"    <chr>  
    ##  2 Balsam Mo… grsm    Great … NC,TN  "Yes -… <chr>   "No"    <chr>  
    ##  3 Big Creek… grsm    Great … NC,TN  "Yes -… <chr>   "No"    <chr>  
    ##  4 Cades Cov… grsm    Great … NC,TN  "Yes -… <chr>   "No"    <chr>  
    ##  5 Cades Cov… grsm    Great … NC,TN  "Yes -… <chr>   "No"    <chr>  
    ##  6 Catalooch… grsm    Great … NC,TN  "Yes -… <chr>   "No"    <chr>  
    ##  7 Cosby Cam… grsm    Great … NC,TN  "Yes -… <chr>   "No"    <chr>  
    ##  8 Deep Cree… grsm    Great … NC,TN  "Yes -… <chr>   "No"    <chr>  
    ##  9 Elkmont C… grsm    Great … NC,TN  "Yes -… <chr>   "No"    <chr>  
    ## 10 Elkmont G… grsm    Great … NC,TN  "Yes -… <chr>   "No"    <chr>  
    ## 11 Look Rock… grsm    Great … NC,TN  "Yes -… <chr>   "No"    <chr>  
    ## 12 Smokemont… grsm    Great … NC,TN  "Yes -… <chr>   "No"    <chr>  
    ## 13 Smokemont… grsm    Great … NC,TN  ""      <chr>   ""      <chr>  
    ## # … with 10 more variables: cellPhoneReception <chr>,
    ## #   laundry <chr>, amphitheater <chr>, dumpStation <chr>,
    ## #   campStore <chr>, staffOrVolunteerHostOnsite <chr>,
    ## #   potableWater <list>, iceAvailableForSale <chr>,
    ## #   firewoodForSale <chr>, foodStorageLockers <chr>, and
    ## #   abbreviated variable names ¹​parkCode, ²​fullName,
    ## #   ³​trashRecyclingCollection, ⁴​internetConnectivity

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
  scale_fill_gradient(low = "grey1", high = "turquoise1", name = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab(NULL) + ylab(NULL) +
  labs(title = "Campground Amenities:
Great Smoky Mountains National Park")
```

![](README_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
all_codes <- as.vector(get_NPS_codes(my_key)$parkCode)
fees <- bind_rows(get_NPS_fees(my_key, all_codes)) |>
  as_tibble() |>
  select(entranceFeeType, cost) |>
  mutate(cost = as.numeric(cost)) |>
  filter(entranceFeeType == "Entrance - Private Vehicle"|
         entranceFeeType == "Entrance - Motorcycle"|
         entranceFeeType == "Entrance - Per Person")
```

``` r
fees |>
  group_by(entranceFeeType) |>
  summarise(averageCost= mean(cost))
```

    ## # A tibble: 3 × 2
    ##   entranceFeeType            averageCost
    ##   <chr>                            <dbl>
    ## 1 Entrance - Motorcycle             21.3
    ## 2 Entrance - Per Person             13.5
    ## 3 Entrance - Private Vehicle        25.8

``` r
ggplot(data = fees, aes(x = entranceFeeType, y = cost)) +
  geom_boxplot(aes(fill = entranceFeeType)) +
  xlab(NULL) +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("turquoise4", "seagreen3", "turquoise2"), name = "Fee Type") +
  labs(title = "NPS Fees by Type of Entrance")
```

![](README_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->
