#Code written by Eric Sun, heavily inspired by Tomas Westlake
#https://r-mageddon.netlify.com/post/the-burger-king-pandemic/
#8/16/2018

library(tidyverse)
library(rvest)
library(sf)
library(glue)
library(gifski)
library(maptools)

#URL for wikipedia link
url = "https://en.wikipedia.org/wiki/List_of_countries_with_McDonald%27s_restaurants"

#Function that gathers all data into a table containing the Country, Year,
#Number of Outlets, and Number of People. Returns that table
#Precondition: url is correct and the html table at that url is the 2nd table
gatheringdata = function(){
  table = url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
    html_table(fill = TRUE)

  table = table[[1]][c(1,2,3,5,7)]

  colnames(table) = c("Num","country","year","Outlets","People")
  
  return(table)
}

#the table with all the raw data
data = gatheringdata()

#cleandata takes in a raw data table and cleans it by removing unnecessary parts of country names
#and cleaning the years to be numbers only
#Preconditions: the raw data table must have columns labeled country and year
cleandata = function(data){
  clean = data %>%
    mutate(country = ifelse(grepl("\\(",country), str_sub(country, end = (regexpr("\\(",country)) -1), country)) %>%
    mutate(year = ifelse(grepl("\\[",year), str_sub(year, end = (regexpr("\\[",year)) -1), year)) %>%
    mutate(country = ifelse(endsWith(country, " "),str_sub(country, end = nchar(country)-1), country)) %>%
    mutate(year = ifelse(country=="United Kingdom",", 1974",year)) %>%
    mutate(year = as.numeric(str_sub(year, start = (regexpr(", ",year)+2),end=(regexpr(", ",year) + 5)))) %>%
    filter(!is.na(year))
  return(clean)
}

cleaned_data = cleandata(data)

#tfarrays is a helper function that takes in 2 vectors and combines them into a single logical
# vector with either TRUE or FALSE values. 
#Precondition: vector1 and vector2 are both vectors
#Postcondition: a vector with the length of vector 2 is returned that has TRUE values if a value
# in vector 2 is in vector 1, and FALSE value if a value in vector 2 is not in vector 1
tfarrays = function(vector1, vector2){
  result = logical(length(vector2))
  i = 1
  for (v in vector2){
    if(v %in% vector1){
      result[i] = TRUE
    } else {
      result[i] = FALSE
      # print(v)
    }
    i = i + 1
  }
  return(result)
}

#Collecting all the graph country names into a single vector to calculate mismatches
wrld_countries = wrld_simpl@data %>%
  mutate(NAME = as.vector(NAME)) %>%
  data.frame()

#Finds any mismatches for countries in our data but not in the graph country names
mismatches = cleaned_data %>% 
  anti_join(wrld_countries, by= c("country" = "NAME"))
mismatches

country_match = tribble(
  ~country, ~fix_country,
  "U.S. Virgin Islands", "United States Virgin Islands",
  "Brunei", "Brunei Darussalam",
  "South Korea", "Korea, Republic of",
  "Northern Marianas", "Northern Mariana Islands",
  "Vietnam", "Viet Nam",
  "PR of China", "China"
)

join_data = cleaned_data %>%
  left_join(country_match) %>%
  mutate(country = ifelse(is.na(fix_country),country,fix_country))

#plotting function to make the graphs
plot_fun = function(open_year){
  final_data = filter(join_data, year <= open_year)
  data(wrld_simpl)
  p = plot(wrld_simpl,
       col = c(gray(.80), "blue")[tfarrays(final_data$country, wrld_simpl@data$NAME) + 1],
       main = glue( "Countries with McDonald's in year: {open_year}"))
  # print(p)
}

save_gif(walk(min(join_data$year):max(join_data$year), plot_fun), delay = 0.5, gif_file = "animation.gif")
