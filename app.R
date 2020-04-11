

## Adapted from 
## Edward Parker's COVID-2019 interactive mapping tool (first github link)

## includes code adapted from the following sources:
# https://github.com/eparker12/nCoV_tracker (Edward Parker's repo)
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example

# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")

# load and import data
source("load_data.R")


### MAP FUNCTIONS ###
# function to plot cumulative COVID cases by date
# cumulative_plot = function(cv_aggregated, plot_date) {
#   plot_df = subset(cv_aggregated, date<=plot_date)
#   g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
#     ylab("cumulative cases") + theme_bw() + 
#     scale_colour_manual(values=c(covid_col)) +
#     scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
#     theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
#           plot.margin = margin(5, 12, 5, 5))
#   g1
# }
# 


# function to remove NAs embedded in totals data
clean_totals <- function(total_vec) {
  prior_val <- NA
  for (index in 1:length(total_vec)) {
    if (is.na(total_vec[[index]])) {
      if (!is.na(prior_val)) {
        total_vec[[index]] <- prior_val
      }
    } else {
      prior_val <- total_vec[[index]]
    }
    
    index <- index + 1
  }
  total_vec
}


# function to plot new COVID cases by date
new_cases_plot <- function(case_data) {
  g1 <- ggplot(case_data) + 
    geom_step(data=case_data, mapping=aes(x=date, y=new.cases, group=1), color=economist_pal(3)(1)) + 
    geom_step(data=case_data, mapping=aes(x=date, y=daily.released, group=1), color=economist_pal(3)(2)[[1]]) + 
    scale_x_date(date_breaks = "1 week", date_labels =  "%b %d") +
    ylab("Daily Increase") +
    xlab("Date") +
    ggtitle("Daily new cases and released") + 
    scale_color_economist() + 
    theme_economist_white(gray_bg = F) +
    theme(legend.title = element_text(), legend.position = "top", plot.title = element_text(size=10),
          plot.margin = margin(5, 12, 5, 5))
  g1
}

new_cases_by_county_plot = function(case_data, county) {
  selection_cols = c('date', 
                     paste('total.', county, '.res', sep=''), 
                     paste('new.', county, '.res', sep=''))
  county_data <- case_data %>% select(all_of(selection_cols))
  colnames(county_data) <- c('date', 'total.cases', 'new.cases')
  g1 = ggplot(county_data, aes(x=date, y=new.cases)) + 
    geom_bar(fill=economist_pal(3)(1), stat='identity') + 
    scale_x_date(date_breaks = "1 week", date_labels =  "%b %d") +
    ylab("Daily Increase") +
    xlab("Date") +
    ggtitle("Daily new cases") + 
    theme_economist_white(gray_bg = F) +
    theme(legend.title = element_text(), legend.position = "bottom", plot.title = element_text(size=10),
          plot.margin = margin(5, 12, 5, 5))
  g1
}


cummulative_cases_plot = function(case_data) {
  case_data <- case_data %>% select(c('date', 'total.cases', 'total.released', 'total.deaths'))
  case_data <- case_data %>% mutate_at(c('total.cases', 'total.released', 'total.deaths'), clean_totals)
  case_data$total.cases <- case_data$total.cases - 
    (if_else(is.na(case_data$total.deaths), as.integer(0), case_data$total.deaths) + 
        if_else(is.na(case_data$total.released), as.integer(0), case_data$total.released))
  case_data$total.cases <- if_else(case_data$total.cases >= 0, case_data$total.cases, as.integer(0))
  colnames(case_data) <- c('date', 'Active Cases', 'Recovered', 'Deaths')
  total_data <- case_data %>% pivot_longer(c('Active Cases', 'Recovered', 'Deaths'), 
                                           names_prefix = 'total.', 
                                           values_to='total')
  g1 = ggplot(total_data, aes(x=date, y=total, fill=name)) + 
    geom_bar(stat="identity") + 
    scale_x_date(date_breaks = "1 week", date_labels =  "%b %d") +
    ylab("Total") +
    xlab("Date") +
    ggtitle("Daily case information") + 
    scale_fill_economist(
      breaks = c('Active Cases', 'Recovered', 'Deaths'),
      labels = c('Active Cases', 'Recovered', 'Deaths')) +
    theme_economist_white(gray_bg = F) +
    theme(plot.title = element_text(size=10),
          plot.margin = margin(5, 12, 5, 5))
  
  ggplotly(g1) %>% layout(legend = list(x=0.5,
                                        y=-0.15,
                                        orientation="h",
                                        xanchor="center",
                                        font = list(
                                          family = "sans-serif",
                                          size = 9,
                                          color = "#000")))
}

log_cases_plot = function(case_data) {
  case_data <- case_data %>% select(c('date', 'total.cases', 'total.released', 'total.deaths'))
  case_data <- case_data %>% mutate_at(c('total.cases', 'total.released', 'total.deaths'), clean_totals)
  
  predict_dates <- case_data$date
  predict_dates[1:11] <- NA
  data_length <- length(case_data$total.cases)
  # fit to exponential model
  model <- lm(log10(total.cases[11:data_length]) ~ predict_dates[11:data_length], data=case_data)
  case_data$prediction <- double(data_length)
  case_data$prediction[11:data_length] <- 10^(predict(model,case_data$date))
  
  case_data <- case_data %>% rename(c('Cases' = total.cases,
                                      'Recovered' = total.released,
                                      'Deaths' = total.deaths,
                                      'Predicted Cases' = prediction))
  total_data <- case_data %>% pivot_longer(c('Cases', 'Recovered', 'Deaths', 'Predicted Cases'), 
                                           names_prefix = 'total.', 
                                           values_to='total')
  g1 = ggplot(total_data, aes(x=date, y=total, color=name)) + 
    geom_line() + 
    geom_point(data = total_data %>% filter(name %in% c('Cases','Recovered', 'Deaths'))) + 
    scale_x_date(date_breaks = "1 week", date_labels =  "%b %d") +
    scale_y_log10() +
    annotation_logticks() + 
    ylab("Log(Total)") +
    xlab("Date") +
    ggtitle("Case Curves") + 
    scale_color_economist(
      breaks = c('Active Cases', 'Recovered', 'Deaths'),
      labels = c('Active Cases', 'Recovered', 'Deaths')) +
    theme_economist_white(gray_bg = F) +
    theme(plot.title = element_text(size=10),
          plot.margin = margin(5, 12, 5, 5))
  
  ggplotly(g1) %>% layout(legend = list(x=0.5,
                                        y=-0.15,
                                        orientation="h",
                                        xanchor="center",
                                        font = list(
                                          family = "sans-serif",
                                          size = 9,
                                          color = "#000")))
}


cummulative_cases_by_county_plot = function(case_data, county) {
  selection_cols = c('date', 
                     paste('total.', county, '.res', sep=''), 
                     paste('new.', county, '.res', sep=''))
  county_data <- case_data %>% select(all_of(selection_cols))
  colnames(county_data) <- c('date', 'total.cases', 'new.cases')
  county_data <- county_data %>% mutate_at(c('total.cases'), clean_totals)
  g1 = ggplot(county_data, aes(x=date, y=total.cases)) + 
    geom_bar(fill=economist_pal(3)(1),stat="identity") + 
    scale_x_date(date_breaks = "1 week", date_labels =  "%b %d") +
    ylab("Total") +
    xlab("Date") +
    ggtitle("Total cases over time") + 
    scale_fill_economist() + 
    theme_economist_white(gray_bg = F) +
    theme(legend.title = element_text(), legend.position = "left", plot.title = element_text(size=10),
          plot.margin = margin(5, 12, 5, 5))
  
  g1
}

### plotting test data ###
new_tests_plot <- function(test_data) {
  selection_cols = c('date', 'daily.tests', 'daily.private.tests', 'daily.state.tests')
  test_data <- test_data %>% select(all_of(selection_cols))
  
  test_data$daily.tests <- test_data$daily.tests - 
    (if_else(is.na(test_data$daily.private.tests), as.integer(0), test_data$daily.private.tests) + 
       if_else(is.na(test_data$daily.state.tests), as.integer(0), test_data$daily.state.tests))
  # make sure unknown is greater than 0
  test_data$daily.tests <- if_else(test_data$daily.tests >= 0, test_data$daily.tests, as.integer(0))
  colnames(test_data) <- c('date', 'Unknown', 'Private Lab', 'State Lab')
  test_data <- test_data %>% pivot_longer(c('Unknown', 'Private Lab', 'State Lab'), 
                                          values_to='total')
  g1 <- ggplot(test_data, aes(x=date, y=total, fill=name)) + 
    geom_bar(stat="identity") + 
    scale_x_date(date_breaks = "1 week", date_labels =  "%b %d") +
    ylab("Total") +
    xlab("Date") +
    ggtitle("Daily Testing") + 
    scale_fill_economist() + 
    theme_economist_white(gray_bg = F) +
    theme(legend.title = element_text(), legend.position = "left", plot.title = element_text(size=10),
          plot.margin = margin(5, 12, 5, 5))
  ggplotly(g1) %>% layout(legend = list(x=0.5,
                                        y=-0.15,
                                        orientation="h",
                                        xanchor="center",
                                        font = list(
                                          family = "sans-serif",
                                          size = 9,
                                          color = "#000")))
  
}

cummulative_tests_plot = function(test_data) {
  selection_cols = c('date', 'total.tests', 'total.private.tests', 'total.state.tests')
  test_data <- test_data %>% select(all_of(selection_cols))

  test_data <- test_data %>% mutate_at(c('total.tests', 'total.private.tests', 'total.state.tests'), clean_totals)
  test_data$total.tests <- test_data$total.tests - 
    (if_else(is.na(test_data$total.state.tests), as.integer(0), test_data$total.state.tests) + 
       if_else(is.na(test_data$total.private.tests), as.integer(0), test_data$total.private.tests))
  # make sure unknown is greater than 0
  test_data$total.tests <- if_else(test_data$total.tests >= 0, test_data$total.tests, as.integer(0))
  test_data <-
    test_data %>% rename(
      c(
        "Unknown" = "total.tests",
        "Private Lab" = "total.private.tests",
        "State Lab" = "total.state.tests"
      )
    )
  
  test_data <- test_data %>% pivot_longer(c('Unknown', 'Private Lab', 'State Lab'), 
                                          values_to='total')

  g1 = ggplot(test_data, aes(x=date, y=total, fill=name)) + 
    geom_bar(stat="identity") + 
    scale_x_date(date_breaks = "1 week", date_labels =  "%b %d") +
    ylab("Total") +
    xlab("Date") +
    ggtitle("Total Testing") + 
    scale_fill_economist(breaks = c('Private Lab', 'State Lab', 'Unknown'),
                          labels = c('Private Lab', 'State Lab', 'Unknown')) + 
    theme_economist_white(gray_bg = F) +
    theme(
       plot.title = element_text(size=10),
          plot.margin = margin(5, 12, 5, 5))
  ggplotly(g1) %>% layout(legend = list(x=0.5,
                                        y=-0.15,
                                        orientation="h",
                                        xanchor="center",
                                        font = list(
                                          family = "sans-serif",
                                          size = 9,
                                          color = "#000")))
}

test_results_plot <- function(test_data) {
  selection_cols = c('date', 'positive.tests', 'negative.tests', 'inconcl.tests')
  test_data <- test_data %>% select(all_of(selection_cols))
  test_data <-
    test_data %>% rename(
      c(
        "Positive" = "positive.tests",
        "Negative" = "negative.tests",
        "Inconclusive" = "inconcl.tests"
      )
    )
  
  test_data <- test_data %>% pivot_longer(c(Positive, Negative, Inconclusive), 
                                          values_to='total')
  g1 <- ggplot(test_data, aes(x=date, y=total, fill=name)) + 
    geom_bar(stat="identity") + 
    scale_x_date(date_breaks = "1 week", date_labels =  "%b %d") +
    scale_fill_economist(
      breaks = c('Positive', 'Negative', 'Inconclusive'),
      labels = c('Positive', 'Negative', 'Inconclusive')) +
    ylab("Total") +
    xlab("Date") +
    ggtitle("Test Results") + 
    theme_economist_white(gray_bg = F) +
    theme(plot.title = element_text(size=10),
          plot.margin = margin(5, 12, 5, 5))
  ggplotly(g1) %>% layout(legend = list(x=0.5,
                                        y=-0.15,
                                        orientation="h",
                                        xanchor="center",
                                        font = list(
                                          family = "sans-serif",
                                          size = 9,
                                          color = "#000")))
}


### DATA PROCESSING: COVID-19 ###

# extract time stamp from cv_cases
update <- tail(case_data$date,1) 

# extract dates from cv data
cv_min_date <- as.Date(min(case_data$date),"%Y-%m-%d")
current_date <- as.Date(max(case_data$date),"%Y-%m-%d")
last_update <- format(as.Date(max(case_data$date),"%Y-%m-%d"), "%b %d %Y")
cv_max_date_clean <- format(as.POSIXct(current_date),"%d %B %Y")



### UI Elements ###

ui <- navbarPage(
  theme = shinytheme("sandstone"),
  collapsible = TRUE,
  "Hawaii COVID-19 tracker",
  id = "nav",
  
  tabPanel(
    "COVID-19 Case Information",
    tabsetPanel(
      tabPanel(
        "Statewide",
        class = "panel panel-default",
        top = 80,
        left = 20,
        width = 500,
        fixed = TRUE,
        draggable = TRUE,
        height = "auto",
        tags$i(h5(paste("Last updated on: ", last_update))),
        tags$i(h5("Updated once daily at noon")),
        tags$i(h2("Summary")),
        tags$i(h4(paste("Total Cases:", tail(case_data$total.cases,1) ))),
        tags$i(h5(paste("Increase: +", 
                        tail(case_data$new.cases,1), 
                        " (↗",
                        format(
                          tail(case_data$new.cases,1) / tail(case_data$total.cases, 2)[1] * 100,
                          digits = 3,
                          nsmall = 1
                        ),
                        "% )", 
                        sep=''))),
        tags$i(h2("Case Information")),
        fluidRow(column(
          6,
          plotlyOutput("log_cases_plot", 
                       height ="500px", 
                       width = "auto")
        ),
        column(
          6,
          plotlyOutput(
            "cummulative_cases_plot",
            height = "500px",
            width = "auto"
          )
        )),
        fluidRow(column(
          6,
          plotlyOutput("new_cases_plot", height =
                         "500px", width = "auto")
        )),
        tags$i(h2("Testing Information")),
        fluidRow(column(
          6,
          plotlyOutput("new_tests_plot",
                       height = "500px",
                       width = "auto"),
        ),
        column(
          6,
          plotlyOutput(
            "cummulative_tests_plot",
            height = "500px",
            width = "auto"
          ),
        )),
        fluidRow(column(
          6,
          plotlyOutput("test_results_plot",
                       height = "500px",
                       width = "auto"),
        )),
        
      ),
      tabPanel(
        "Kauai",
        class = "panel panel-default",
        top = 80,
        left = 20,
        width = 500,
        fixed = TRUE,
        draggable = TRUE,
        height = "auto",
        tags$i(h5(paste("Last updated on: ", last_update))),
        tags$i(h5("Updated once daily at noon")),
        tags$i(h2("Summary")),
        tags$i(h4(paste("Total Cases:", tail(geo_data$total.kauai.res,1) ))),
        tags$i(h5(paste("Increase: +", 
                        tail(geo_data$new.kauai.res,1), 
                        " (↗",
                        format(
                          tail(geo_data$new.kauai.res,1) / tail(geo_data$total.kauai.res, 2)[1] * 100,
                          digits = 3,
                          nsmall = 1
                        ),
                        "% )", 
                        sep=''))),
        tags$i(h2("Testing Information")),
        fluidRow(column(
          6,
          plotlyOutput("kauai_new_cases_plot",
                       height = "500px",
                       width = "auto"),
        ),
        column(
          6,
          plotlyOutput(
            "kauai_cummulative_cases_plot",
            height = "500px",
            width = "auto"
          ),
        )),
      ),
      tabPanel(
        "Oahu",
        class = "panel panel-default",
        top = 80,
        left = 20,
        width = 500,
        fixed = TRUE,
        draggable = TRUE,
        height = "auto",
        tags$i(h5(paste("Last updated on: ", last_update))),
        tags$i(h5("Updated once daily at noon")),
        tags$i(h2("Summary")),
        tags$i(h4(paste("Total Cases:", tail(geo_data$total.oahu.res,1) ))),
        tags$i(h5(paste("Increase: +", 
                        tail(geo_data$new.oahu.res,1), 
                        " (↗",
                        format(
                          tail(geo_data$new.oahu.res,1) / tail(geo_data$total.oahu.res, 2)[1] * 100,
                          digits = 3,
                          nsmall = 1
                        ),
                        "% )", 
                        sep=''))),
        tags$i(h2("Testing Information")),
        fluidRow(column(
          6,
          plotlyOutput("oahu_new_cases_plot",
                       height = "500px",
                       width = "auto"),
        ),
        column(
          6,
          plotlyOutput(
            "oahu_cummulative_cases_plot",
            height = "500px",
            width = "auto"
          ),
        )),
      ),
      tabPanel(
        "Maui",
        class = "panel panel-default",
        top = 80,
        left = 20,
        width = 500,
        fixed = TRUE,
        draggable = TRUE,
        height = "auto",
        tags$i(h5(paste("Last updated on: ", last_update))),
        tags$i(h5("Updated once daily at noon")),
        tags$i(h2("Summary")),
        tags$i(h4(paste("Total Cases:", tail(geo_data$total.maui.res,1) ))),
        tags$i(h5(paste("Increase: +", 
                        tail(geo_data$new.maui.res,1), 
                        " (↗",
                        format(
                          tail(geo_data$new.maui.res,1) / tail(geo_data$total.maui.res, 2)[1] * 100,
                          digits = 3,
                          nsmall = 1
                        ),
                        "% )", 
                        sep=''))),
        tags$i(h2("Testing Information")),
        fluidRow(column(
          6,
          plotlyOutput("maui_new_cases_plot",
                       height = "500px",
                       width = "auto"),
        ),
        column(
          6,
          plotlyOutput(
            "maui_cummulative_cases_plot",
            height = "500px",
            width = "auto"
          ),
        )),
      ),
      tabPanel(
        "Hawaii",
        class = "panel panel-default",
        top = 80,
        left = 20,
        width = 500,
        fixed = TRUE,
        draggable = TRUE,
        height = "auto",
        tags$i(h5(paste("Last updated on: ", last_update))),
        tags$i(h5("Updated once daily at noon")),
        tags$i(h2("Summary")),
        tags$i(h4(paste("Total Cases:", tail(geo_data$total.hawaii.res,1) ))),
        tags$i(h5(paste("Increase: +", 
                        tail(geo_data$new.hawaii.res,1), 
                        " (↗",
                        format(
                          tail(geo_data$new.hawaii.res,1) / tail(geo_data$total.hawaii.res, 2)[1] * 100,
                          digits = 3,
                          nsmall = 1
                        ),
                        "% )", 
                        sep=''))),
        tags$i(h2("Testing Information")),
        fluidRow(column(
          6,
          plotlyOutput("hawaii_new_cases_plot",
                       height = "500px",
                       width = "auto"),
        ),
        column(
          6,
          plotlyOutput(
            "hawaii_cummulative_cases_plot",
            height = "500px",
            width = "auto"
          ),
        )),
      )
    )
    # div(class="outer",
    #     tags$head(includeCSS("styles.css")),
    #     leafletOutput("mymap", width="100%", height="100%"),
    #
    #
    #     absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
    #                   tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
    #
    #     absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
    #                   actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
    #                                onclick = sprintf("window.open('%s')",
    #                                                  "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus"))),
    #
    #
    # )
  ),
  
  
  tabPanel(
    "Data",
    numericInput("maxrows", "Rows to show", 25),
    verbatimTextOutput("rawtable"),
    downloadButton("downloadCsv", "Download as CSV"),
    tags$br(),
    tags$br()
  ),
  
  tabPanel(
    "About this site",
    tags$div(
      tags$h4("Last update"),
      h6(paste0(update)),
      "This site is updated once daily. At this time of rapid escalation of the COVID-19 pandemic, the following resources offer the latest numbers of known cases:",
      tags$br(),
      tags$a(href = "https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "WHO COVID-19 dashboard"),
      tags$br(),
      tags$a(href = "https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University COVID-19 dashboard"),
      tags$br(),
      "The aim of this site is to complement the above resources by providing several interactive features not currently available elsewhere, including the timeline function,
                        the ability to overlay past outbreaks, and an emphasis on normalised counts (per 100,000 individuals).",
      tags$br(),
      tags$br(),
      tags$h4("Background"),
      "In December 2019, cases of severe respiratory illness began to be reported across the city of Wuhan in China.
                        These were caused by a new type of coronavirus, and the disease is now commonly referred to as COVID-19.
                        The number of COVID-19 cases started to escalate more quickly in mid-January and the virus soon spread beyond China's borders.
                        This story has been rapidly evolving ever since, and each day we are faced by worrying headlines regarding the current state of the outbreak.",
      tags$br(),
      tags$br(),
      "In isolation, these headlines can be hard to interpret.
                        How fast is the virus spreading? Are efforts to control the disease working? How does the situation compare with previous epidemics?
                        This site is updated daily based on data published by Johns Hopkins University.
                        By looking beyond the headlines, we hope it is possible to get a deeper understanding of this unfolding pandemic.",
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$h4("Code"),
      "Code and input data used to generate this Shiny mapping tool are available on ",
      tags$a(href = "https://github.com/eparker12/nCoV_tracker", "Github."),
      tags$br(),
      tags$br(),
      tags$h4("Sources"),
      tags$b("2019-COVID cases: "),
      tags$a(
        href = "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series",
        "Johns Hopkins Center for Systems Science and Engineering github page,"
      ),
      " with additional information from the ",
      tags$a(href = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports", "WHO's COVID-19 situation reports."),
      " In previous versions of this site (up to 17th March 2020), updates were based solely on the WHO's situation reports.",
      tags$br(),
      tags$br(),
      tags$br(),
      tags$h4("Authors"),
      "Suchandra Thapa, Code for Hawaiʻi",
      tags$br(),
      tags$br(),
      tags$br(),
      tags$h4("Contact"),
      "sthapa@codeforhawaii.org",
      tags$br(),
      tags$br()
      
    )
  )
  
)




### SHINY SERVER ###
server = function(input, output, session) {

  
  # covid tab 
  # output$clean_date_reactive <- renderText({
  #   format(as.POSIXct(input$plot_date),"%d %B %Y")
  # })
  
  reactive_db = reactive({
    case_data %>% filter(date == input$plot_date)
  })

  # reactive_db_last24h = reactive({
  #   cv_cases %>% filter(date == input$plot_date & new_cases>0)
  # })
  # 
  # 
  # output$cumulative_plot <- renderPlot({
  #   cumulative_plot(cv_aggregated, input$plot_date)
  # })
  # 
  output$new_cases_plot <- renderPlotly({
    new_cases_plot(case_data)
  })

  output$cummulative_cases_plot <- renderPlotly({
    cummulative_cases_plot(case_data)
  })

  output$log_cases_plot <- renderPlotly({
    log_cases_plot(case_data)
  })
  
  output$new_tests_plot <- renderPlotly({
    new_tests_plot(test_data)
  })
  
  output$cummulative_tests_plot <- renderPlotly({
    cummulative_tests_plot(test_data)
  })

  output$test_results_plot <- renderPlotly({
    test_results_plot(test_data)
  })
  
  output$kauai_new_cases_plot <- renderPlotly({
    new_cases_by_county_plot(geo_data, 'kauai')
  })
  
  output$kauai_cummulative_cases_plot <- renderPlotly({
    cummulative_cases_by_county_plot(geo_data, 'kauai')
  })

  output$oahu_new_cases_plot <- renderPlotly({
    new_cases_by_county_plot(geo_data, 'oahu')
  })
  
  output$oahu_cummulative_cases_plot <- renderPlotly({
    cummulative_cases_by_county_plot(geo_data, 'oahu')
  })

  output$maui_new_cases_plot <- renderPlotly({
    new_cases_by_county_plot(geo_data, 'maui')
  })
  
  output$maui_cummulative_cases_plot <- renderPlotly({
    cummulative_cases_by_county_plot(geo_data, 'maui')
  })

  output$hawaii_new_cases_plot <- renderPlotly({
    new_cases_by_county_plot(geo_data, 'hawaii')
  })
  
  output$hawaii_cummulative_cases_plot <- renderPlotly({
    cummulative_cases_by_county_plot(geo_data, 'hawaii')
  })
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(as.data.frame(case_data), 
               input$maxrows), row.names = FALSE)
    options(orig)
  })
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("COVID_data_", cv_data$date[1], ".csv", sep="")
    },
    content = function(file) {
      write.csv(cv_data, file)
    }
  )  
}


#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
# library(rsconnect)
# deployApp(account="ssthapa")
