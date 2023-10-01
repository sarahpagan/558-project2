NPS API
================
Sarah Pagan
2023-10-01

``` r
get_NPS_activities <- function(key, activities){
  acts <- paste(activities, collapse = ",")
  url <- paste0("https://developer.nps.gov/api/v1/activities/parks?api_key=",
                key,
                "&q=",
                acts,
                "&limit=100000")
  
  query <- GET(url)
  results <- fromJSON(rawToChar(query$content))$data$parks
  
  activity <- c()
  for (i in 1:length(activities)){
    y <- rep(activities[i],
             nrow(results[[i]]))
    activity <- append(activity, y)
  }
  
  results |>
    bind_rows() |>
    cbind(activity) |>
    arrange(fullName) |>
    select(fullName, activity, states)
}
```

``` r
a <- GET(paste0("https://developer.nps.gov/api/v1/activities?api_key=", myKey))
fromJSON(rawToChar(a$content))$data$name
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

``` r
get_NPS_activities(myKey, c("climbing", "swimming")) |>
  group_by(fullName) |>
  filter(n() > 1) |>
  select(-activity) |>
  distinct(fullName, states)
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
