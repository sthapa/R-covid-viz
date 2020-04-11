if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidr", repos = "http://cran.us.r-project.org")
if(!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")

download_data <- T
json_uri <- 'https://covid19-hawaii.herokuapp.com/hawaii_daily.sqlite/hawaii_daily?_format=json'

remove_comma <- function(x) {
  gsub(',', '', x)  
}

if (download_data) {
  resp <- fromJSON(json_uri)
  cv_data <- as_tibble(resp$rows)
  data_cols <- gsub(' ', '.', tolower(resp$columns))
  colnames(cv_data) <- data_cols
  cv_data$date <- as.Date(cv_data$date, format="%m/%d/%y")
  cv_data <- cv_data %>% mutate_at(data_cols[3:length(data_cols)], remove_comma)
  cv_data <- cv_data %>% mutate_at(data_cols[3:length(data_cols)], as.integer)
  test_data <- cv_data %>% select('date', 
                                  'total.tests',
                                  'daily.tests',
                                  'total.private.tests',
                                  'daily.private.tests',
                                  'total.state.tests',
                                  'daily.state.tests',
                                  'negative.tests',
                                  'positive.tests',
                                  'inconcl.tests')
  case_data <- cv_data %>% select('date', 
                                  'total.cases', 
                                  'new.cases', 
                                  'total.released',
                                  'daily.released', 
                                  'total.deaths',
                                  'total.hosp',
                                  'new.hosp')
  geo_data <- cv_data %>% select('date', 
                                 'total.oahu.res',
                                 'new.oahu.res',
                                 'total.kauai.res',
                                 'new.kauai.res',
                                 'total.hisland.res',
                                 'new.hisland.res',
                                 'total.maui.res',
                                 'new.maui.res')
  colnames(geo_data) <- c('date', 
                          'total.oahu.res',
                          'new.oahu.res',
                          'total.kauai.res',
                          'new.kauai.res',
                          'total.hawaii.res',
                          'new.hawaii.res',
                          'total.maui.res',
                          'new.maui.res') 
} else {
  cv_cases <- read_csv('covid_data.csv')
  cv_cases <- select(cv_cases, -c(6, 7, 8))
  colnames(cv_cases) <- c('date', 'total.infected', 'increase', 'released', 'total.recovered')
  cv_cases$date <- as.Date(cv_cases$date, format="%m/%d/%Y")
  
  
  total_cases <- cv_cases %>% pivot_longer(c(total.infected, total.recovered), names_prefix = 'total.', values_to='total')
}