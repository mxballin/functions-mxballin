STAT 545 Assignment B-1
================

STAT 545 @ UBC Assignment B-1: Making a function Total Points: 100

This assignment covers making a function in R, documenting it, and
testing it.

# Loading packages

``` r
suppressPackageStartupMessages(library(datateachr)) # <- contains the data you picked!
suppressPackageStartupMessages(library(tidyverse))
```

# Assignment Exercises

## **Exercise 1: Make a Function (25 points)**

*In this exercise, you’ll be making a function and fortifying it. The
function need not be complicated. The function need not be “serious”,
but shouldn’t be nonsense.*

## **Exercise 2: Document your Function (20 points)**

*In the same code chunk where you made your function, document the
function using roxygen2 tags.*

One of the chunks of code that I that repeated frequently for the
tidying process in Milestone 2 of the mini data analysis was the
unnesting, grouping, and factoring of a variable. It would be great to
make this a function to not have to copy, paste, and modify the whole
chunk over and over if a lot of variables have nested observations.

``` r
#' @title A Function to Unnest and Factor a Variable in a Tibble
#'
#' @description 'unnest_and_factor' takes a designated variable from a designated dataframe and helps to tidy
#' the data by making sure that if there are multiple values included in a single observation with a standardized
#' separator, these values are unnested--separated--and then factored for sorting, counting, and other grouping purposes.
#'
#' @param df The name of the stored tibble you want to work with - called 'df' for dataframe based on common stackoverflow usage
#' @param var The target variable from that tibble - called 'var' as short for variable
#' @param sep The separator string that the function should use to identify nested values; include quotations for proper usage - called 'sep' to mirror dplyr language
#'
#'@return The function returns the unnested tibble.
#'
#'@examples
#' unnest_and_factor(steam_games, game_details,",")
#' unnest_and_factor(apt_buildings, parking_type, ",")
#' unnest_and_factor(apt_buildings, bike_parking, " and ")
#'
#' @references
#' Syntax suggested on StackOverflow by LyzandeR
#' link: \url{https://stackoverflow.com/questions/48062213/dplyr-using-column-names-as-function-arguments}
#' More information can aso be found in the book 'Tidy Evaluation' by Lionel Henry and Hadley Wickham
#' link: \url{https://tidyeval.tidyverse.org/dplyr.html}

unnest_and_factor <- function(df,var,sep) { #where df is the dataset, var is the variable to be unnested and sep is the separator used in the observations of the nested variable
    check <- dplyr::summarise(df, is.character({{var}})) #checking if the class of the variable is a character
  if (!check[[1]])
    warning("The variable you have selected to unnest is not a character. It will be converted to a character as part of the evaluation of this function.") #warning that the function converts the existing non-character variable, if applicable, to a character in order to work
   unnested_factored <- df %>%
    mutate({{var}}:=strsplit(as.character({{var}}), sep))%>%  #separating the observations on the given separator 
    unnest({{var}}) %>% #unnesting the separated observations
    mutate({{var}}:=as.factor({{var}})) #factoring the observations for grouping capabilities
   return(unnested_factored) #returning the unnested and factored tibble
}
```

## **Exercise 3: Include examples (15 points)**

*Demonstrate the usage of your function with a few examples. Use one or
more new code chunks, describing what you’re doing.*

### Example 1

Here is an unnesting that I had tried in my STAT545A assignment that
places each detail about a game is its own observation. To show what has
changed, after unnesting the variable using the function and getting the
return of the entire tibble, I asked r to print the head of the original
and tidied tibble and selected just the unnested variable and the id
variable.

``` r
#using function with `steam_games` dataset variable `game_details`

unnest_and_factor(steam_games, game_details,",")
```

    ## # A tibble: 179,228 × 21
    ##       id url    types name  desc_snippet recent_reviews all_reviews release_date
    ##    <dbl> <chr>  <chr> <chr> <chr>        <chr>          <chr>       <chr>       
    ##  1     1 https… app   DOOM  Now include… Very Positive… Very Posit… May 12, 2016
    ##  2     1 https… app   DOOM  Now include… Very Positive… Very Posit… May 12, 2016
    ##  3     1 https… app   DOOM  Now include… Very Positive… Very Posit… May 12, 2016
    ##  4     1 https… app   DOOM  Now include… Very Positive… Very Posit… May 12, 2016
    ##  5     1 https… app   DOOM  Now include… Very Positive… Very Posit… May 12, 2016
    ##  6     1 https… app   DOOM  Now include… Very Positive… Very Posit… May 12, 2016
    ##  7     1 https… app   DOOM  Now include… Very Positive… Very Posit… May 12, 2016
    ##  8     2 https… app   PLAY… PLAYERUNKNO… Mixed,(6,214)… Mixed,(836… Dec 21, 2017
    ##  9     2 https… app   PLAY… PLAYERUNKNO… Mixed,(6,214)… Mixed,(836… Dec 21, 2017
    ## 10     2 https… app   PLAY… PLAYERUNKNO… Mixed,(6,214)… Mixed,(836… Dec 21, 2017
    ## # … with 179,218 more rows, and 13 more variables: developer <chr>,
    ## #   publisher <chr>, popular_tags <chr>, game_details <fct>, languages <chr>,
    ## #   achievements <dbl>, genre <chr>, game_description <chr>,
    ## #   mature_content <chr>, minimum_requirements <chr>,
    ## #   recommended_requirements <chr>, original_price <dbl>, discount_price <dbl>

``` r
details<-unnest_and_factor(steam_games, game_details,",") #storing the function evaluating `game_details`

head(steam_games)%>%
  select(id, game_details) #the head of the original tibble's `id` and `game_details` columns
```

    ## # A tibble: 6 × 2
    ##      id game_details                                                            
    ##   <dbl> <chr>                                                                   
    ## 1     1 Single-player,Multi-player,Co-op,Steam Achievements,Steam Trading Cards…
    ## 2     2 Multi-player,Online Multi-Player,Stats                                  
    ## 3     3 Single-player,Multi-player,Online Multi-Player,Cross-Platform Multiplay…
    ## 4     4 Multi-player,Online Multi-Player,Steam Workshop,Steam Cloud,Valve Anti-…
    ## 5     5 Multi-player,Online Multi-Player,MMO,Co-op,Online Co-op,Steam Trading C…
    ## 6     6 Single-player,Multi-player,Downloadable Content,Steam Achievements,Full…

``` r
head(details)%>%
  select(id, game_details) #the head of the unnested and factored tibble's `id` and `game_details` columns
```

    ## # A tibble: 6 × 2
    ##      id game_details              
    ##   <dbl> <fct>                     
    ## 1     1 Single-player             
    ## 2     1 Multi-player              
    ## 3     1 Co-op                     
    ## 4     1 Steam Achievements        
    ## 5     1 Steam Trading Cards       
    ## 6     1 Partial Controller Support

### Example 2

I was also interested in testing if and how the function would work for
other datasets and not just the one that I had successfully unnested
before. I took a look at the other datasets in the `datateachr` package
and saw that `apt_buildings` also had a nested column in the
`parking_type` column, so I tried the function with that variable. To
show what has changed, after unnesting the variable using the function
and getting the return of the entire tibble, I asked r to print the head
of the original and tidied tibble and selected just the unnested
variable and the id variable.

``` r
#using function with `apt_buildings` dataset variable `parking_type`
unnest_and_factor(apt_buildings, parking_type, ",")
```

    ## # A tibble: 6,978 × 37
    ##       id air_conditioning amenities  balconies barrier_free_acce… bike_parking  
    ##    <dbl> <chr>            <chr>      <chr>     <chr>              <chr>         
    ##  1 10359 NONE             Outdoor r… YES       YES                0 indoor park…
    ##  2 10359 NONE             Outdoor r… YES       YES                0 indoor park…
    ##  3 10360 NONE             Outdoor p… YES       NO                 0 indoor park…
    ##  4 10360 NONE             Outdoor p… YES       NO                 0 indoor park…
    ##  5 10360 NONE             Outdoor p… YES       NO                 0 indoor park…
    ##  6 10361 NONE             <NA>       YES       NO                 Not Available 
    ##  7 10361 NONE             <NA>       YES       NO                 Not Available 
    ##  8 10361 NONE             <NA>       YES       NO                 Not Available 
    ##  9 10362 NONE             <NA>       YES       YES                Not Available 
    ## 10 10362 NONE             <NA>       YES       YES                Not Available 
    ## # … with 6,968 more rows, and 31 more variables: exterior_fire_escape <chr>,
    ## #   fire_alarm <chr>, garbage_chutes <chr>, heating_type <chr>, intercom <chr>,
    ## #   laundry_room <chr>, locker_or_storage_room <chr>, no_of_elevators <dbl>,
    ## #   parking_type <fct>, pets_allowed <chr>, prop_management_company_name <chr>,
    ## #   property_type <chr>, rsn <dbl>, separate_gas_meters <chr>,
    ## #   separate_hydro_meters <chr>, separate_water_meters <chr>,
    ## #   site_address <chr>, sprinkler_system <chr>, visitor_parking <chr>, …

``` r
parking<-unnest_and_factor(apt_buildings, parking_type, ",") #storing the function evaluating `parking_type`


head(apt_buildings)%>%
  select(id, parking_type) #the head of the original tibble's `id` and `parking_type` columns
```

    ## # A tibble: 6 × 2
    ##      id parking_type                                                        
    ##   <dbl> <chr>                                                               
    ## 1 10359 Underground Garage , Garage accessible thru buildg                  
    ## 2 10360 Underground Garage , Garage accessible thru buildg , Surface Parking
    ## 3 10361 Underground Garage , Garage accessible thru buildg , Surface Parking
    ## 4 10362 Ground Level Garage , Surface Parking                               
    ## 5 10363 Underground Garage , Garage accessible thru buildg , Surface Parking
    ## 6 10364 Ground Level Garage , Surface Parking

``` r
head(parking)%>%
  select(id, parking_type) #the head of the unnested and factored tibble's `id` and `parking_type` columns
```

    ## # A tibble: 6 × 2
    ##      id parking_type                     
    ##   <dbl> <fct>                            
    ## 1 10359 "Underground Garage "            
    ## 2 10359 " Garage accessible thru buildg" 
    ## 3 10360 "Underground Garage "            
    ## 4 10360 " Garage accessible thru buildg "
    ## 5 10360 " Surface Parking"               
    ## 6 10361 "Underground Garage "

### Example 3

Another column with two pieces of information in `apt_buildings`is the
`bike_parking` variable. I thought this might be a good variable to try
using the function on because the separator is not a comma, but the word
“and.” This shows the function can be used with any separator the user
sets, not just a single character. To show what has changed, after
unnesting the variable using the function and getting the return of the
entire tibble, I asked r to print the head of the original and tidied
tibble and selected just the unnested variable and the id variable.

``` r
#using function with `apt_buildings` dataset variable `bike_parking`
unnest_and_factor(apt_buildings, bike_parking, " and ")
```

    ## # A tibble: 4,650 × 37
    ##       id air_conditioning amenities   balconies barrier_free_acce… bike_parking 
    ##    <dbl> <chr>            <chr>       <chr>     <chr>              <fct>        
    ##  1 10359 NONE             Outdoor re… YES       YES                0 indoor par…
    ##  2 10359 NONE             Outdoor re… YES       YES                10 outdoor p…
    ##  3 10360 NONE             Outdoor po… YES       NO                 0 indoor par…
    ##  4 10360 NONE             Outdoor po… YES       NO                 34 outdoor p…
    ##  5 10361 NONE             <NA>        YES       NO                 Not Available
    ##  6 10362 NONE             <NA>        YES       YES                Not Available
    ##  7 10363 NONE             <NA>        NO        NO                 12 indoor pa…
    ##  8 10363 NONE             <NA>        NO        NO                 0 outdoor pa…
    ##  9 10364 NONE             <NA>        NO        NO                 Not Available
    ## 10 10365 NONE             <NA>        NO        YES                Not Available
    ## # … with 4,640 more rows, and 31 more variables: exterior_fire_escape <chr>,
    ## #   fire_alarm <chr>, garbage_chutes <chr>, heating_type <chr>, intercom <chr>,
    ## #   laundry_room <chr>, locker_or_storage_room <chr>, no_of_elevators <dbl>,
    ## #   parking_type <chr>, pets_allowed <chr>, prop_management_company_name <chr>,
    ## #   property_type <chr>, rsn <dbl>, separate_gas_meters <chr>,
    ## #   separate_hydro_meters <chr>, separate_water_meters <chr>,
    ## #   site_address <chr>, sprinkler_system <chr>, visitor_parking <chr>, …

``` r
bike<-unnest_and_factor(apt_buildings, bike_parking, " and ") #storing the function evaluating `bike_parking`

head(apt_buildings)%>%
  select(id, bike_parking) #the head of the original tibble's `id` and `bike_parking` columns
```

    ## # A tibble: 6 × 2
    ##      id bike_parking                                       
    ##   <dbl> <chr>                                              
    ## 1 10359 0 indoor parking spots and 10 outdoor parking spots
    ## 2 10360 0 indoor parking spots and 34 outdoor parking spots
    ## 3 10361 Not Available                                      
    ## 4 10362 Not Available                                      
    ## 5 10363 12 indoor parking spots and 0 outdoor parking spots
    ## 6 10364 Not Available

``` r
head(bike)%>%
  select(id, bike_parking) #the head of the unnested and factored tibble's `id` and `bike_parking` columns
```

    ## # A tibble: 6 × 2
    ##      id bike_parking            
    ##   <dbl> <fct>                   
    ## 1 10359 0 indoor parking spots  
    ## 2 10359 10 outdoor parking spots
    ## 3 10360 0 indoor parking spots  
    ## 4 10360 34 outdoor parking spots
    ## 5 10361 Not Available           
    ## 6 10362 Not Available

## **Exercise 4: Test the Function (25 points)**

*Running examples is a good way of checking by-eye whether your function
is working as expected. But, having a formal “yes or no” check is useful
when you move on to other parts of your analysis.*

*Write formal tests for your function. You should use at least three
non-redundant uses of an expect\_() function from the testthat package,
and they should be contained in a test\_that() function (or more than
one). They should all pass.*

### Test 1

I first decided to test to make sure that the function would produce the
same tibble as if I had done the unnesting manually. To do this, I
created a test that would check that the class for the languages had
been converted to being “factor” and that the overall tibble was the
same as the manually performed version.

``` r
#testing that using the function and doing the specific unnesting action produce the same results and that the unnested column is factored
#using the `steam_games` dataset and the `languages` variable, which has NAs

#count NAS
steam_games %>%
    count(is.na(languages))
```

    ## # A tibble: 2 × 2
    ##   `is.na(languages)`     n
    ##   <lgl>              <int>
    ## 1 FALSE              40797
    ## 2 TRUE                  36

``` r
#unnesting manually
unnested_languages <- steam_games %>%
  mutate(languages=strsplit(as.character(languages), ",")) %>%
  unnest(languages)%>%
  mutate(languages=as.factor(languages))

#unnesting using the custom function
function_languages <- unnest_and_factor(steam_games,languages,",")

#testing that the two unnested tibbles match
testthat::test_that("factorandmatched", {
testthat::expect_s3_class(function_languages$languages, "factor") #testing that the variable `languages` has the s3 class "factor"
testthat::expect_identical(unnested_languages, function_languages)}) #testing that the output is the same as if the unnesting had been done manually
```

    ## Test passed 🎉

### Test 2

I then created a test to make sure that the unnesting was occurring and
that it wasn’t somehow creating new or differently named columns. To do
this, I tested to make sure that the number of rows in the new tibble
was greater than the original and that the number of columns and the
names of the columns had remained the same.

``` r
#testing that the unnesting is occurring by asking r to check whether the new tibble has a higher number of rows compared to the original tibble but has the same number of columns with the same names.
#using the `apt_buildings` dataset and the `facilities_available` variable, which 

#checking NA count
apt_buildings %>%
count(is.na(facilities_available))
```

    ## # A tibble: 1 × 2
    ##   `is.na(facilities_available)`     n
    ##   <lgl>                         <int>
    ## 1 FALSE                          3455

``` r
#unnesting manually
unnested_facilities <- apt_buildings %>%
  mutate(facilities_available=strsplit(as.character(facilities_available), "/")) %>%
  unnest(facilities_available)%>%
  mutate(facilities_available=as.factor(facilities_available))

#unnesting using the custom function
function_facilities <- unnest_and_factor(apt_buildings,facilities_available,"/")

#testing that the number of rows has increased from the original tibble but that the number and names of columns has stayed the same

testthat::test_that("rowcolcount", {
  testthat::expect_gt(nrow(function_facilities), nrow(apt_buildings)) #inequality of row count
  testthat::expect_equal(ncol(function_facilities), ncol(apt_buildings)) #matching column count
  testthat::expect_equal(colnames(function_facilities),colnames(apt_buildings)) #matching column names
                    })
```

    ## Test passed 🌈

### Test 3

Lastly, I wanted to test to make sure that the warning was working by
attempting to unnest a column that was not classed as “character.” To do
so, I tested the `steam_games` dataset variable `id`, which is classed
as a double.

``` r
#testing that evaluating the `id` variable of `steam_games` would generate a warning.
testthat::test_that("warning", testthat::expect_warning(unnest_and_factor(steam_games, id, ",")))
```

    ## Test passed 🥇

Powered by the Academic theme for Hugo.
