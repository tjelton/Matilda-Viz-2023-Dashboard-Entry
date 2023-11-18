library(shiny)
library(shinyWidgets)
library(bslib)
library(tidyverse)
library(pushbar)
library(scroller)
library(scrollrevealR)
library(shinydashboard)
library(shinydashboardPlus)
library(scrollytell)
library(plotly)
library(DT)
library(formattable)
library(sf)
library(leaflet)
library(shinyalert)


options(shiny.fullstacktrace=TRUE)

###################################################################################
##### WHO Mortality Dataset
###################################################################################

# Load in data for plotting.
WHO_deaths = read.csv("www/data/world_wide_self_harm_and_substance_deaths.csv")

# Get rates of death
WHO_deaths = WHO_deaths %>%
  mutate(
    Rate_per_100_000 = (Deaths/Age_and_Sex_Population) * 100000
  )

self_harm_deaths = WHO_deaths %>% filter(Sex == "All", Cause == "Intentional self-harm", Age_Range == "All", Year == 2017)
substance_abuse_deaths = WHO_deaths %>% filter(Sex == "All", Cause == "Mental and behavioural disorders due to psychoactive substance use", Age_Range == "All", Year == 2017)

age_labels = unique(WHO_deaths$Age_Range)[order(unique(WHO_deaths$Age_Range))]
values_to_remvoes = c("0", "1", "1-4", "2", "3", "4", "5-9", "5-14", "15-24","25-34", "35-44", "45-54", "55-64", "65-74", "75+", "85+", "Unknown")
age_labels = age_labels[!(age_labels %in% values_to_remvoes)]

###################################################################################
##### Australia Deaths
###################################################################################

# Load in data for plotting.
australia_deaths = read.csv("www/data/death_data_cleaned.csv")

australia_deaths = australia_deaths %>% 
  select(-X) %>%
  rename(
    "Age Group" = "Age_Group",
    "Cause of Death" = "Cause_of_Death",
    "Deaths" = "Count",
    "Percentage of Deaths" = "Percentage_of_Deaths",
    "Rate per 100k" = "Rate_Per_100000_Age_Specific"
  ) %>%
  
  # Filter rows that are about all causes (this mucks up the ranking).
  filter(`Cause of Death` != "All causes") %>%
  
  # Add a rank column to rank causes within an age group and sex.
  group_by(`Age Group`, Sex) %>%
  mutate(Rank = dense_rank(desc(Deaths))) %>%
  
  # Re-order columns to make Rank, Deaths, Percentage of Deaths and Rate per 100k the first columns.
  select(Rank, Deaths,`Percentage of Deaths`, `Rate per 100k`, `Cause of Death`, `Age Group`, Sex)

# Data for the tables in the discussion part of chapter 2.
chapter_2_part_2 = australia_deaths %>% filter(`Cause of Death` == "Suicide ", Sex == "All")
chapter_2_part_2$Deaths = as.character(chapter_2_part_2$Deaths)
chapter_2_part_3 = australia_deaths %>% filter(`Cause of Death` == "Suicide ", Sex == "Female")
chapter_2_part_4 = australia_deaths %>% filter(`Cause of Death` == "Suicide ", Sex == "Male")

age_labels_australia_deaths = unique(australia_deaths$`Age Group`)[order(unique(australia_deaths$`Age Group`))]

###################################################################################
##### Mental Health Hospitalisations 
###################################################################################

hospital_data = read.csv("www/data/cleaned_mental_health_emergency_department.csv")

# Australia states spatial data.
australia_sf <- sf::st_read("www/data/Australia_states.geojson")

# Code for scatter plot:

# Get the type here because of weird string formatting.
# This is for "Mental health-related presentations" which contains characters that are atypical.
type = hospital_data$Type[1]

# Get scatter data.
scatter_data_hospitalisations = hospital_data %>% 
  filter(Count == "Number", Type == type) %>%
  
  # Filter to below 2022 data as 2021 data is incomplete.
  filter(Year <= 2021) %>%
  group_by(Year, Location) %>%
  summarise(Hospitalisations = sum(Value))
  
scatter_data_hospitalisations$Year = as.numeric(scatter_data_hospitalisations$Year)

###################################################################################
##### Lifeline Data
###################################################################################

# Annual reports from: https://www.lifeline.org.au/about/governance/annual-reports/
# Data table made in R:

year = c("21-22", "20-21", "19-20", "18-19", "17-18", "16-17", "15-16", "14-15")
calls_answered = c(1142234,1070860,835867,731646, 732365,793397, 831744, 821804)
call_answer_rate = c(87,90.34,84.5,80,82,85,85,85)/100
lifeline_data = tibble(year, calls_answered, call_answer_rate)



# Set the light and dark themes.
light <- bs_theme(bootswatch = "flatly")

library(thematic)
thematic_shiny(font = "auto")


linebreaks <- function(n){HTML(strrep(br(), n))}

ui <- fluidPage(
  
  # Initialise different packages.
  pushbar::pushbar_deps(),
  scroller::use_scroller(),

  # Theme.
  theme = light,
  
  # Title Section
  column(
    ######################
    id = "title_section",
    width = 12,
    ######################
    
    linebreaks(13),
    
    HTML('<h1 id = "beggining" style="text-align: center;">The Suicide Epidemic</h1>'),
    HTML('<h4>A Data Story</h4>'),
    linebreaks(1),
    HTML('<h5>Please wait 10 seconds for the dahsboard to fully load.</h5>'),
    
    linebreaks(3), 
    
    p("Scroll to begin..."),
    icon("angle-down", "fa-3x"),
    ),
  tags$head(tags$style(".col-sm-1 {width: 50%; margin: 0px;}
                          #title_section {margin: 0 auto; width: 100%; text-align: center}")),
  
  
  linebreaks(15), 
  
  # Warning
  column(
    ######################
    id = "warning",
    width = 12,
    ######################
    card(
      card_body(
        HTML(
          '<div class="h4 pb-2 mb-4 text-danger border-bottom border-danger">Warning!</div>',
          '<h5 class="text-danger"><br>
          The contents of the following dashboard discusses topics such as suicide, self-harm, and substance abuse. 
          It may be distressing for some users.<br><br>
          
          (Australia) If this dashboard causes you significant distress, please call "000" for emergency services if in immediate danger. Otherwise, please reach out to
          Lifeline on "13 11 14".<br><br></h5>',
        ),
      ),
      class = "border-danger"
    )
  ),
  tags$head(tags$style(".col-sm-1 {width: 50%; margin: 0px;}
                          #warning {margin: 0 auto; width: 50%; text-align: center}")),
  
  linebreaks(15),

  # Chapter 1 Introduction
  column(
    ######################
    id = "chapter_1_intro",
    width = 12,
    ######################
    
    HTML('<h1>Chapter 1</h1>'),
    HTML('<h4>A World Wide Epidemic</h4>'),
    
    linebreaks(3), 
    
    p("The Story Starts Below..."),
    icon("angle-down", "fa-3x"),
  ),
  tags$head(tags$style(".col-sm-1 {width: 50%; margin: 0px;}
                          #chapter_1_intro{margin: 0 auto; width: 100%; text-align: center}")),
  
  linebreaks(10),

  # Chapter 1 Part 1
  column(
    ######################
    id = "chapter_1_part_1",
    width = 12,
    ######################
    
    HTML('<h4 style="margin-right: 375px;"><em>"Suicide is an action taken to deliberately end one’s own life."</em></h4>'),
    HTML('<h5 style="margin-left: 550px;"><a href="https://www.aihw.gov.au/suicide-self-harm-monitoring/summary/suicide-and-intentional-self-harm">- Australian Insitute of Health and Welfare</a></h5>'),
  ),
  tags$head(tags$style(".col-sm-1 {width: 50%; margin: 0px;}
                          #chapter_1_part_1{margin: 0 auto; width: 100%; text-align: center}")),
  
  linebreaks(8),
  
  
  # Chapter 1 Part 2
  scrolly_container("suicide_deaths_scrolly", 
                    
                    # Graph part
                    scrolly_graph(
                      
                      linebreaks(7),
                      plotlyOutput("deaths_map"),
                      
                    ),
                    scrolly_sections(

                      scrolly_section(id = 0,
                                      
                                      linebreaks(1)
                                      
                                      
                      ),
                      scrolly_section(id = 1,
                                      
                                      HTML("<h4 style='padding-left: 8px;'>A Sad Reality</h4>"),
                                      br(),
                                      HTML("<p style='padding-left: 8px;'>Suicide is an international epidemic. It is a serious cause of death which 
                                           affects not only Australia, but the entire world.<br><br>If you look to the left, you will see a world map 
                                           with different countries shaded different colours (this map is interactive). The colour coding represents the number 
                                           of deaths from suicide in each country during the year 2017. Darker colours represent a lower number of deaths by suicide
                                           as compared to other countries, with lighter and warmer colours reprenting a high relative death count. 
                                           Whilst countries deviate significantly in the number of deaths, each death from suicide is a tragedy. 
                                           It is clear that suicide is an issue that the international community is facing together.</p>"),
                                      
                      ),
                      scrolly_section(id = 2,
                                      
                                      HTML("<h4 style='padding-left: 8px;'>Data Source</h4>"),
                                      br(),
                                      HTML("<p style='padding-left: 8px;'>International data on the number of deaths as a result of suicide is not readily available.
                                      However, data was found from the World Health Organisation who provide a publicly accessible database collating the number of deaths
                                      from a wide range of conditions for most countries.<br>
                                           
                                           The world map plots (known in data science as chloropleth maps) that we are exploring in this chapter were a result
                                           of extensive data cleaning where four data sets were combined:</p><br>
                                           
                                           <ul>
                                              <li style='padding-left: 12px;'>World Health Organisation Mortality Database</li>
                                              <li style='padding-left: 12px;'>World Health Organisation Population Data</li>
                                              <li style='padding-left: 12px;'>World Health Organisation Country Codes</li>
                                              <li style='padding-left: 12px;'>Our World in Data Population Data by Year</li>
                                              <li style='padding-left: 12px;'>Country Codes to ISO Label Dataset</li></ul><br>
                                           
                                           <p style='padding-left: 12px;'>Information on where to access these data sets can be found by pressing the rectangular
                                           \"+\" button in the bottom right corner.</p>"),
                                      
                      ),
                      scrolly_section(id = 4,
                                      
                                      HTML("<h4 style='padding-left: 8px;'>Rate per 100k Deaths</h4>"),
                                      br(),
                                      HTML("<p style='padding-left: 8px;'>Whilst looking at the total number of deaths offers insight into the extent of lives lost because
                                           of suicide, it is a poor way to compare between countries.<br><br>
                                           
                                           For example, in the \"Deaths From Suicide By Country\" plot, the United States of America had by far the highest number of lives
                                           lost because of suicide in 2017, with 47,107 deaths. On the other hand, Australia had 3,286 deaths. However, this large difference
                                           is largely attributed to population size - the USA has roughly 13 times the number of people as Australia.<br><br>
                                           
                                           A fairer way to compare countries is to look at the number of deaths per 100,000 people in a country. Once this has been done 
                                           we see that Australia has one of the higher suicide rates in the world - only slightly lower than the USA.<br><br>
                                           
                                           <b>The plots in this dashboard are all interactive. Feel free to click on the plots to interact with them.</b></p>"),
                                      
                      ),
                      scrolly_section(id = 5,
                                      
                                      HTML("<h4 style='padding-left: 8px;'>Substance Abuse</h4>"),
                                      br(),
                                      HTML("<p style='padding-left: 8px;'>Whilst this data story will be exploring suicide, substance use is another concerning cause of death.<br><br>
                                      
                                           More specifically, the map to the right showcases the number of deaths in the World Health Organisation's mortality database caused
                                           by \"mental and behavioural disorders due to psychoactive substance use.\" Whilst substance abuse can have links to suicide, it will
                                           not be explored in depth here.
                                           </p>"),
                                      
                      ),
                      
                      scrolly_section(id = "buffer", br()),
                      
                    )),
  
  # Removes excess whitespace from scrolly.
  div(style = 'margin-top: -300px;'),
  
  # Chapter 1 Part 3
  fluidRow(
    id = "chapter_1_part_3",
    
    column(
      ######################
      width = 3,
      ######################
      
      # Card for choosing the demographic inputs
      card(
        card_header(
          HTML('<center>'),
          HTML('<h7 style="font-weight: bold; text-decoration: underline;">Demographic Controls</h7>'),
          HTML('</center>')
        ),
        
        card_body(
          # Choose Age.
          selectizeInput(
            inputId = "age_select",
            label = "Age:",
            choices = c(age_labels),
            selected = "All"
          ),
          
          # Choose Sex
          radioButtons(
            inputId = "sex_select",
            label = "Sex:",
            choices = c("Male", 
                        "Female",
                        "All"),
            selected = "All"
          )
        )
      ),
      
      # Card for choosing the time inputs
      card(
        card_header(
          HTML('<center>'),
          HTML('<h7 style="font-weight: bold; text-decoration: underline;">Time Controls</h7>'),
          HTML('</center>')
        ),
        card_body(
          
          # Choose Year
          sliderInput(
            inputId = "year_select",
            label = "Year:",
            min = 2017,
            max = 2021,
            step = 1,
            value = 2017,
            sep = ""
          )
        )
      ),
      
      # Card for choosing what variables to fill the countries on the map.
      card(
        card_header(
          HTML('<center>'),
          HTML('<h7 style="font-weight: bold; text-decoration: underline;">Country Fill Controls</h7>'),
          HTML('</center>')
        ),
        card_body(
          
          # Whether to plot suicides or substance abuse related deaths.
          radioButtons(
            inputId = "death_cause_select",
            label = "Deaths type:",
            choices = c("Suicide" = "Suicide",
                        "Substance Abuse" = "Abuse"),
            selected = "Suicide"
          ),
          
          br(),
          
          # Whether to plot rate per 100k people or total number of deaths
          radioButtons(
            inputId = "death_type_select",
            label = "Unit for Deaths:",
            choices = c("Deaths per 100k people" = "Rate",
                       "Total number of deaths" = "Absolute"),
            selected = "Rate"
          )
          
        )
      ),
      
    ),
    
    column(
      ######################
      width = 9,
      ######################

      HTML('<center><h5 style="font-weight: bold; text-decoration: underline;">Data Explorer</h5></center>'),

      # Format and display world map for data exploration.
      linebreaks(2),
      plotlyOutput("deaths_map_exploration"),
      
      linebreaks(7),
      
      # Display message to explain the rate option if the user selects it.
      conditionalPanel(
        condition = "input.death_type_select == 'Rate'",
        HTML('<p style="color: red;">"Deaths per 100k people" is a rate based off the population of people within a country that match the variables in the "Demographic Controls" for a given year. Population demographic data was not sourced for most countries, and hence why there are less countries showing on the map.</p>'),
      ),
      
    ),

  ),
  
  linebreaks(15),
  
  # Chapter 2 Introduction
  column(
    ######################
    id = "chapter_2_intro",
    width = 12,
    ######################
    
    HTML('<h1>Chapter 2</h1>'),
    HTML('<h4>Suicides in Australia</h4>'),
    
    linebreaks(3), 
    
    p("The Story Continues Below..."),
    icon("angle-down", "fa-3x"),
  ),
  tags$head(tags$style("#chapter_2_intro{margin: 0 auto; width: 100%; text-align: center}")),
  
  linebreaks(15),
  
  # Chapter 2 part 1
  fluidRow(
    ######################
    id = "chapter_2_part_1",
    ######################
    
    column(
      width = 6,
      HTML("<center>"),
      HTML("<h3 style='padding-left: 8px;'><b>Suicide in Australia</b></h3>"),
      br(),
      HTML("<p style='padding-left: 8px;'>In the previous chapter, we explored how suicide is a global issue affecting the entire world. Now we turn our attention 
                                          specifically to Australia.<br><br>
                                      
                                          Suicide is a huge issue in Australia. There were 3,144 recorded deaths by suicide during 2021 (per the World Health
                                          Organisation data from chapter 1). Below, we will look at some data which breaks down the leading causes of death
                                          amongst different age group in Australia from 2019-2021.<br><br>
                                          
                                          At this point, you may be thinking that 3,144 is a low number of deaths when compared to the total number of deaths caused by 
                                          illnesses such as cancer. However, our investigation is going to yield a disturbing truth - that suicide is one of the leading
                                          causes of death amonst young Australians.</p>"),
      HTML("</center>")
    ),
    column(
      width = 6,
      linebreaks(1),
      HTML("<center>"),
      plotlyOutput("australia_map"),
      HTML("</center>")
    )
  ),
  
  linebreaks(8),
  
  fluidRow(
    ######################
    id = "chapter_2_part_2",
    ######################
    
    column(
      width = 6,
      HTML("<h4 style='padding-left: 8px;'>Suicide as a Leading Cause of Death</h4>"),
      br(),
      HTML("<p style='padding-left: 8px;'>The data source used in Chapter 2 comes from the Australian Institute of Health and Welfare
                                           (a department of the Australian Government). This data provides the leading causes of death for different age groups and 
                                           sex in Australia from 2019 to 2020.<br><br>
                                           
                                           The table on the right shares the different age groups (with the male and female sexes combined) in
                                           which sucicide was a leading cause of death. The rank column reveals how suicide ranked when compared to other causes of death
                                           (with a rank of 1 indicating that suicide was the leading cause of death within an age group).<br><br>
                
                                           What is evident from this table is that suicide is a leading cause of death for multiple age groups. What is quite concerning is
                                           that it is the 12th leading cause of death amongst all Australians, and is the number one leading cause of death for Australians
                                           aged 15 to 44.</p>"),

    ),
    
    column(
      width = 6,
      linebreaks(2),
      DTOutput("australia_deaths_chapter_2_part_2"),
    )
    
  ),
  
  linebreaks(8),
  
  fluidRow(
    ######################
    id = "chapter_2_part_3",
    ######################
    
    column(
      width = 6,
      HTML("<h4 style='padding-left: 8px;'>Suicide in Females</h4>"),
      br(),
      HTML("<p style='padding-left: 8px;'>Now we turn our attention to suicide in females as a leading cause of death. The table to the right summarises all female age
                                          groupswhere suicide was a leading cause of death. Only the top 20 leading causes of death for each age group
                                          were included in the original data set, meaning that if a particular age group is not listed, suicide is not a top-20 leading cause
                                          of death.<br><br>
                                           
                                          It is of particular concern that suicide is a leading cause of death amongst younger Australian females. In particular, it is striking
                                          that suicide is the leading cause of death amongst females aged between 15 and 44.</p>"),
    ),
    
    column(
      width = 6,
      DTOutput("australia_deaths_chapter_2_part_3"),
    )
    
  ),
  
  linebreaks(8),
  
  fluidRow(
    ######################
    id = "chapter_2_part_4",
    ######################
    
    column(
      width = 6,
      HTML("<h4 style='padding-left: 8px;'>Suicide in Males</h4>"),
      br(),
      HTML("<p style='padding-left: 8px;'>Similar to females, suicide is the number one leading cause of death
                                          amongst males aged between 15 and 24. However, a substantial difference betweent the sexes is that suicide is ranked much higher
                                          (ranked 3) for males aged between 45-64.<br><br>
           
                                          Suicide seems to be quite prevalent in men, and accounts for the 9th leading cause of death amongst all ages of men (see the last row
                                          in the table).</p>"),
    ),
    
    column(
      width = 6,
      DTOutput("australia_deaths_chapter_2_part_4"),
    )
    
  ),
  
  linebreaks(8),
  
  fluidRow(
    id = "chapter_2_part_5",
    
    column(
      ######################
      width = 7,
      ######################
      
      HTML('<center><h5 style="font-weight: bold; text-decoration: underline; padding-left: 8px;">Data Explorer: Chapter 2</h5></center>'),
      linebreaks(1),
      HTML("<p style='padding-left: 8px;'>In this section we explored how suicide is a leading cause of death for many age groups and sexes.<br><br>
           
                                          Let's look at the different leading causes of death. To the right in the \"Demographic
                                          Controls\" section, you can change the age group and sex that you are looking at. The table below will
                                          update with data corresponding to reflect the selected demographic.<br><br>
                                          
                                          If suicide is one of the twenty leading causes within the demographic selected, the corresponding row will
                                          be highlighted red. The column graph also illustrates the leading causes of death that are
                                          similarly ranked to suicide for the given demographic. If suicide is not a leading cause within the demographic,
                                          then the top 5 causes of death are plotted.</p>"),
      
    ),
    
    column(
      ######################
      width = 5,
      ######################
      
      # Card for choosing the demographic inputs
      card(
        card_header(
          HTML('<center>'),
          HTML('<h7 style="font-weight: bold; text-decoration: underline;">Demographic Controls</h7>'),
          HTML('</center>')
        ),
        
        card_body(
          # Choose Age.
          selectizeInput(
            inputId = "age_select_chapter_2",
            label = "Age:",
            choices = c(age_labels_australia_deaths),
            selected = "All ages"
          ),
          
          # Choose Sex
          radioButtons(
            inputId = "sex_select_chapter_2",
            label = "Sex:",
            choices = c("Male", 
                        "Female",
                        "All"),
            selected = "All"
          )
        )
      )
    )
  ),
  
  fluidRow(
    ######################
    id = "chapter_2_part_6",
    ######################
    
    column(
      ######################
      width = 7,
      ######################
      
      DTOutput("australia_deaths_exploration_table")
    ),
    column(
      ######################
      width = 5,
      ######################
      linebreaks(5),
      plotlyOutput("australia_deaths_exploration_graph")
    )
  ),
  
  linebreaks(15),
  
  # Chapter 3 Introduction
  column(
    ######################
    id = "chapter_3_intro",
    width = 12,
    ######################
    
    HTML('<h1>Chapter 3</h1>'),
    HTML('<h4>Mental Health Hospitalisations</h4>'),
    
    linebreaks(3), 
    
    p("The Story Continues Below..."),
    icon("angle-down", "fa-3x"),
  ),
  tags$head(tags$style("#chapter_3_intro{margin: 0 auto; width: 100%; text-align: center}")),
  
  linebreaks(15),
  
  # Chapter 3 part 1
  fluidRow(
    ######################
    id = "chapter_3_part_1",
    ######################
    
    column(
      width = 6,
      HTML("<center>"),
      HTML("<h3 style='padding-left: 8px;'><b>Mental Health-Related Hospital Presentations</b></h3>"),
      br(),
      HTML("<p style='padding-left: 8px;'>It goes without saying that every death caused by suicide is a tragedy. In the previous section, we found that between 2019 to
                                          2021, that 9641 Australians died by suicide (see the table to the right as a recap).<br><br> 
                                          
                                          This chapter focuses on the number of mental health-related hospital presentations across years and state/territory.</p>"),
      HTML("</center>")
    ),
    column(
      width = 6,
      linebreaks(1),
      HTML("<center>"),
      DTOutput("chapter_3_table"),
      HTML("</center>")
    )
  ),
  
  linebreaks(8),
  
  fluidRow(
    ######################
    id = "chapter_3_part_2",
    ######################
    
    column(
      width = 6,
      HTML("<h4 style='padding-left: 8px;'>Australian Hospitalisations</h4>"),
      br(),
      HTML("<p style='padding-left: 8px;'>Firstly, it is important to note that there are a lot of reasons why someone might be admitted to hospital because of 
                                          mental health. It would not be correct to say that every hospitalisation is a result of a suicide attempt. This begs the question
                                          - why are we looking at this data? The reason is to highlight how widespread and severe mental health illnesses are in Australia.<br><br>
           
                                          The plot to the right showcases the number of mental health related hospital presentations in Australia from 2014 to 2021.<br><br>
           
                                          The plot indicates an increasing number of hospital presentations from 2014 to 2019, whilst there is a rapid decline 
                                          from 2020 to 2021 (this coincides with the COVID pandemic). Regardless, with around 280,000 presentations in recent time, it is clear
                                          that many Australians suffer from sever mental health problems which require hospital attention.</p>"),
    ),
    column(
      width = 6,
      linebreaks(1),
      plotlyOutput("australian_all_hospitalisations_line_graph")
    )
  ),
  
  linebreaks(8),
  
  fluidRow(
    ######################
    id = "chapter_3_part_3",
    ######################
    
    column(
      width = 4,
      HTML("<h4 style='padding-left: 8px;'>Hospitalisation by Region</h4>"),
      br(),
      HTML("<p style='padding-left: 8px;'>The following looks at data from Australia's States and Territories.<br><br>
                                          
                                          Here we find that New South Wales had the most mental health related hospital presentations by a large margin. This is followed by
                                          Queensland and Victoria which are quite evenly matched. There is then another substantial gap before considering the other regions.
                                          The plot here is interactive. Hover over the lines and points to see the specific number of hospitalisations
                                          for each region!</p>"),
    ),
    column(
      width = 8,
      linebreaks(3),
      plotlyOutput("states_hospitalisations_line_graph")
    )
  ),
  
  linebreaks(8),
  
  fluidRow(
    ######################
    id = "chapter_3_part_4",
    ######################
    
    column(
      ######################
      width = 9,
      ######################
      
      HTML('<center><h5 style="font-weight: bold; text-decoration: underline; padding-left: 8px;">Data Explorer: Chapter 3</h5></center>'),
      linebreaks(1),
      HTML("<p style='padding-left: 8px;'>In this chapter we explored the number of mental health related presentations in Australia.<br><br>
           
                                          Whilst we preivously visualised this by using a line graph, the Chpater 3 data explorer offers an alternate way of visualising
                                          the data - by having the data overlaid on an interactive map of Australia!<br><br>
                                          
                                          On the right you can change the year that the data focuses on. A corresponding map of Australia is shown below, where 
                                          darker colours represent a larger number of mental health related hospital presentations.</p>"),
      
    ),
    
    column(
      width = 3,
      
      # Card for choosing the demographic inputs
      linebreaks(2),
      card(
        card_header(
          HTML('<center>'),
          HTML('<h7 style="font-weight: bold; text-decoration: underline;">Time Control</h7>'),
          HTML('</center>')
        ),
        card_body(
          # Choose Year
          sliderInput(
            inputId = "year_select_chapter_3",
            label = "Year:",
            min = 2014,
            max = 2021,
            step = 1,
            value = 2014,
            sep = ""
          )
        )
      )
    )
    
  ),
  
  fluidRow(
    ######################
    id = "chapter_3_part_5",
    ######################
    
    column(
      width = 2
    ),
    column(
      width = 8,
      leafletOutput("australia_hospitalisation_map")
    ),
    column(
      width = 2
    ),
  ),
  
  linebreaks(15),
  
  # Chapter 4 Introduction
  column(
    ######################
    id = "chapter_4_intro",
    width = 12,
    ######################
    
    HTML('<h1>Chapter 4</h1>'),
    HTML('<h4>Case Study - Lifeline</h4>'),
    
    linebreaks(3), 
    
    p("Last Chapter Below..."),
    icon("angle-down", "fa-3x"),
  ),
  tags$head(tags$style("#chapter_4_intro{margin: 0 auto; width: 100%; text-align: center}")),
  
  linebreaks(15),
  
  fluidRow(
    ######################
    id = "chapter_4_part_2",
    ######################
    
    column(
      ######################
      width = 8,
      ######################
      
      HTML('<center><h5 style="font-weight: bold; text-decoration: underline; padding-left: 8px;">Case Study - Lifeline </h5></center>'),
      linebreaks(1),
      HTML("<p style='padding-left: 8px;'>Lastly, we share a data stroy from one of Australia's leading crisis support organisations. Founded in 1963, Lifeline is a 24-hour 
                                          crisis support line, and is accessible to Australians by calling 13 11 14 or via their online website.<br><br>
                                          
                                          From <a href='https://www.lifeline.org.au/get-help/'>Lifeline's website</a>, \"If you are thinking about suicide or
                                          experiencing emotional distress, help is available. There is hope. Please contact Lifeline, we are here to listen. We are here for you.\"<br><br>
                                          
                                          More information about Lifeline can be found 
                                          <a href='https://www.lifeline.org.au/about/#:~:text=Lifeline%20is%20Australia's%20leading%20suicide,to%2024%2Dhour%20crisis%20support.'>
                                          here</a>. Additional resources regarding mental health can also be found <a href='https://www.lifeline.org.au/resources/'>here</a>.<br><br>
           
                                          Lifeline answers approximately a million calls a year from Austrlians that need help or someone to talk to. Some data
                                          that highlights the great work they are doing in the Australian community is shown below.</p>")
      
    ),
    column(
      ######################
      width = 4,
      ######################
      linebreaks(4),
      HTML("<center>"),
      imageOutput("lifeline_image"),
      HTML("</center>")
      
    )
  ),
  
  fluidRow(
    ######################
    id = "chapter_4_part_3",
    ######################
    
    column(
      width = 5,
      HTML("<h4 style='padding-left: 8px;'>Crisis Line</h4>"),
      br(),
      HTML("<p style='padding-left: 8px;'>One of the main ways that Lifeline is positively working in the Australian community is through their crisis support line, whereby
                                          people in Australia can receive help in times of distress by calling 13 11 14.<br><br>
                                          
                                          The plot to the right shows the total number of calls Lifeline answered during different financial year periods. This data
                                          was taken from lifeline's annual reports.<br><br>
                                          
                                          It is evident that Lifeline contributes to mental health services in Australia. For the last eight years, they have 
                                          consistently answered over 700,000 phone calls from Australians in distress. In fact, over the last two financial years, they have answered
                                          over a million phone calls.</p>"),
    ),
    column(
      width = 7,
      plotlyOutput("lifeline_calls_answered")
    )
  ),
  
  linebreaks(2),
  
  fluidRow(
    ######################
    id = "chapter_4_part_4",
    ######################
    
    column(
      width = 6,
      plotlyOutput("lifeline_call_answer_rate")
    ),
    column(
      width = 6,
      linebreaks(2),
      HTML("<p style='padding-left: 8px;'>Additionally, the plot to the left shows the call answer rate for different financial years.<br><br>
      
                                          Lifeline has consistently answered 80% or more phone calls in the last eight years.<br><br> 
                                          
                                          Also, over the last two periods, the call rate has grown (the most recent period's rate is 87%).</p>"),
    )
  ),
  
  linebreaks(15),
  
  column(
    ######################
    id = "end",
    width = 12,
    ######################
    
    HTML('<h1>The End</h1>'),
    HTML('<h4>Thank You for Engaging!</h4>'),
    
    linebreaks(3), 
    
    a("Return to Start", type = "button", class = "btn btn-info", href = "#beggining"), ##plot
    
  
  ),
  tags$head(tags$style("#end{margin: 0 auto; width: 100%; text-align: center}")),
  
  
  linebreaks(15),
  
  fixedPanel(
    actionButton("extra_info_open", label = "+", width = 37, height = 45),
    right = 20,
    bottom = 20,
  ),
  

  pushbar(
    
    ######################
    id = "myPushbar",
    ######################
    
    fluidRow(
      
      column(
        width = 10,
        HTML("<h5><u><b>Supplementary Material</b></u></h5>"),
        tabsetPanel(
          tabPanel("Chapter 1",
                   linebreaks(1),
                   HTML("<h5>Chapter 1 Datasets</h5>"),
                   linebreaks(1),
                   HTML("<p><b>World Health Organisation Mortality Database, World Health Organisation Population Data, World Health Organisation Country Codes:</b></p>
                   
                        These datasets formed the basis of the analysis in chapter 1. Data here came from the <a href='https://www.who.int/data/data-collection-tools/who-mortality-database'>World Health Organisation</a>.
                        Visiting this link will allow you to download the three datasets, as well as accompanying documentation. For the mortality data \"Mortality, 
                        ICD-10 (part 5/5)\" was utilised (this incldues mortality data from 2017 onwards).<br><br>
                        
                        Cleaning this data was a challenge, as these three data sets had to be merged, and additionally they were in a wide format which needed to be converted
                        into a long format.<br><br>
                        
                        <p><b>Our World in Data Population Data by Year:</b></p>
                        
                        This data set was used as there was some missing data in the World Health Organisation Population Data. This allowed for more countries to be plotted
                        when considering the suicide and substance abuse rates. This data can be found <a href='https://ourworldindata.org/grapher/population'>here</a>.<br><br>
                        
                        <p><b>Country Names to ISO Label Dataset:</b></p>

                        For creating the world plot, there was a need to map the country names to their ISO-3 code. Data for this was accessed <a href='https://ourworldindata.org/grapher/population'>here</a>.<br><br></p>")
          ),
          tabPanel("Chapter 2", 
                   linebreaks(1),
                   HTML("<h5>Chapter 2 Dataset</h5>"),
                   linebreaks(1),
                   HTML("<p><b>Deaths in Australia:</b></p>
                   
                        <p>Data here was sourced from Australian Government: Australian Institute of Health and Welfare. The original data can be downloaded <a href='https://www.aihw.gov.au/reports/life-expectancy-deaths/deaths-in-australia/data'>here</a>
                        and was last retrieved on the 28th of September 2023. Within this spreadsheet file, the page named \"Table S3.2\" was used. Once again, extensive data cleaning
                        was performed to put the data in a format appropriate for plotting.</p>")

          ),
          tabPanel("Chapter 3", 
                   linebreaks(1),
                   HTML("<h5>Chapter 3 Dataset</h5>"),
                   linebreaks(1),
                   HTML("<p><b>Emergency Department Mental Health Visits:</b></p>
                   
                        <p>Data here was sourced from Australian Government: Australian Institute of Health and Welfare. The original data can be downloaded <a href='https://www.aihw.gov.au/mental-health/topic-areas/emergency-departments'>here</a>
                        and was last retrieved on the 28th of September 2023. Within this spreadsheet file, the page named \"Table ED.4\" was used. Once again, extensive data cleaning
                        was performed to put the data in a format appropriate for plotting.</p>")
          ),
          tabPanel("Chapter 4", 
                   linebreaks(1),
                   HTML("<h5>Chapter 4 Dataset</h5>"),
                   linebreaks(1),
                   HTML("<p><b>Lifeline Data</b></p>
                   
                        <p>Data from Lifeline was collated by looking through their annual reports which can be found <a href='https://www.lifeline.org.au/about/governance/annual-reports/'>here</a>.</p>")
          ),
          tabPanel("R Packages Used", 
                   linebreaks(1),
                   HTML("<h5>The following R pacakges were used throught this data story:</h5>"),
                   linebreaks(1),
                   HTML("
                   <ul>
                    <li>shiny</li>
                    <li>shinyWidgets</li>
                    <li>bslib</li>
                    <li>tidyverse</li>
                    <li>pushbar</li>
                    <li>scroller</li>
                    <li>scrollrevealR</li>
                    <li>shinydashboard</li>
                    <li>shinydashboardPlus</li>
                    <li>scrollytell</li>
                    <li>DT</li>
                    <li>formattable</li>
                    <li>sf</li>
                    <li>leaflet</li>
                   </ul>")
          )
        ),
      ),
    
      column(
        width = 2,
        HTML("<center>"),
        HTML("<h5><u><b>Chapter Navigation</b></u></h5>"),
        linebreaks(1),
        a("Beggining", type = "button", class = "btn btn-info", href = "#beggining"),
        linebreaks(2),
        a("Chapter 1", type = "button", class = "btn btn-info", href = "#chapter_1_intro"),
        linebreaks(2),
        a("Chapter 2", type = "button", class = "btn btn-info", href = "#chapter_2_intro"),
        linebreaks(2),
        a("Chapter 3", type = "button", class = "btn btn-info", href = "#chapter_3_intro"),
        linebreaks(2),
        a("Chapter 4", type = "button", class = "btn btn-info", href = "#chapter_4_intro"),
        linebreaks(2),
        actionButton("extra_info_close", "Return to Data Story"),
        HTML("</center>")
      ),
      
      
      
    ),
    
    
    from = "bottom"
  ),

  scroll_reveal(target = c("#title_section", "#warning", "#chapter_1_intro", 
                           "#chapter_1_part_1", "#chapter_1_part_3", "#chapter_2_intro",
                           "#chapter_2_part_1", "#chapter_2_part_2", "#chapter_2_part_3",
                           "#chapter_2_part_4", "#chapter_2_part_5", "#chapter_2_part_6",
                           "#chapter_3_intro", "#chapter_3_part_1", "#chapter_3_part_2",
                           "#chapter_3_part_3", "#chapter_3_part_4", "#chapter_3_part_5",
                           "#chapter_4_intro", "#chapter_4_part_2", "#chapter_4_part_3",
                           "#chapter_4_part_4", "#chapter_4_part_5", "#end"), duration = 2000, distance = "20px"),
  
)

server <- function(input, output, session) {

  output$deaths_map <- plotly::renderPlotly({
    
    # Data to plot.
    subset = self_harm_deaths
    if (input$suicide_deaths_scrolly >= 5) {
      subset = substance_abuse_deaths
    }


    # Change what is plotted depending on scrolly index.
    # Deaths per 100k people plot.
    legend_text = "Deaths per 100k People"
    if (input$suicide_deaths_scrolly == 4) {
      
      title_for_plot = "Country Suicide Deaths per 100,000 People"
      subset$hover = with(subset, paste('(per 100k people)', '<br><br>', Country, '<br>Year: ', Year, '<br>Total Deaths: ', Deaths, sep = ""))

      fig = plot_geo(subset)
      fig = fig %>% add_trace(
        z = ~Rate_per_100_000, color = ~Rate_per_100_000,
        text = ~hover, locations = ~ISO_Code
      )
      
    } else if (input$suicide_deaths_scrolly >= 5) {
        
        title_for_plot = "Country Substance Abuse Deaths per 100,000 People"
        subset$hover = with(subset, paste('(per 100k people)', '<br><br>', Country, '<br>Year: ', Year, '<br>Total Deaths: ', Deaths, sep = ""))
        
        fig = plot_geo(subset)
        fig = fig %>% add_trace(
          z = ~Rate_per_100_000, color = ~Rate_per_100_000,
          text = ~hover, locations = ~ISO_Code
        )
        
    # Default - Plot with deaths (not rates).
    } else {
      
      title_for_plot = "Deaths From Suicide by Country"
      legend_text = "Deaths"
      subset$hover = with(subset, paste('(Deaths)', '<br><br>', Country, '<br>Year: ', Year, '<br>Death Rate per 100k: ', Rate_per_100_000, sep = ""))
      
      fig = plot_geo(subset)
      fig = fig %>% add_trace(
        z = ~Deaths, color = ~Deaths,
        text = ~hover, locations = ~ISO_Code
      )
    }
    
    # Customisation parameters available here: https://plotly.com/r/reference/layout/geo/
    g <- list(
      showframe = FALSE
    )
    
    # Layout for the plot.
    fig = fig %>% layout(
      title = title_for_plot,
      geo = g
    )
    
    # Layout for the colorbar.
    # Customisation parameters available here: https://plotly.com/r/reference/#scatter-marker-colorbar
    fig = fig %>% colorbar(
      title = list(font = list(color = '#8D23C2'), text = legend_text),
      orientation = "h",
      thickness = 15,
      yanchor = "bottom",
      xanchor = "left",
      yref = "paper",
      y = 0,
      ypad = -4,
      tickcolor = "#8D23C2",
      tickfont = list(color = "#8D23C2"),
      outlinecolor = "#8D23C2"
    )
    
    return(fig)
  })
  
  output$deaths_map_exploration <- plotly::renderPlotly({
    
    # Filter based on user input.
    plotting_data = WHO_deaths %>%
      filter(Sex == input$sex_select, Age_Range == input$age_select, Year == input$year_select)
    
    # Filter death cause based off user input.
    if (input$death_cause_select == "Abuse") {
      plotting_data = plotting_data %>%
        filter(Cause == "Mental and behavioural disorders due to psychoactive substance use")
    } else {
      plotting_data = plotting_data %>%
        filter(Cause == "Intentional self-harm")
    }
    
    # Make the hover text.
    unit = "(deaths)"
    if (input$death_type_select == "Rate") {
      unit = "(deaths per 100k people)"
    }
    plotting_data$hover = with(plotting_data, paste(unit, '<br><br>', Country, '<br>Year: ', Year, sep = ""))
    
    # Initialise plot.
    fig = plot_geo(plotting_data)
    
    # Plot data.
    legend_text = "Number of Deaths"
    if (input$death_type_select == "Rate") {
      legend_text = "Deaths per 100k People"
      fig = fig %>% add_trace(
        z = ~Rate_per_100_000, color = ~Rate_per_100_000,
        text = ~hover, locations = ~ISO_Code
      )
    } else {
      plotting_data$hover = with(plotting_data, paste('(deaths)', '<br><br>', Country, '<br>Year: ', Year, sep = ""))
      fig = fig %>% add_trace(
        z = ~Deaths, color = ~Deaths,
        text = ~hover, locations = ~ISO_Code
      )
    }
    
    # Customisation parameters available here: https://plotly.com/r/reference/layout/geo/
    g <- list(
      showframe = FALSE
    )
    
    # Layout for the plot.
    fig = fig %>% layout(
      geo = g,
      width = 1000,
      height = 500
    )
    
    # Layout for the colorbar.
    # Customisation parameters available here: https://plotly.com/r/reference/#scatter-marker-colorbar
    fig = fig %>% colorbar(
      title = list(font = list(color = '#8D23C2'), text = legend_text),
      orientation = "h",
      thickness = 15,
      yanchor = "bottom",
      xanchor = "left",
      yref = "paper",
      y = 0,
      ypad = -4,
      tickcolor = "#8D23C2",
      tickfont = list(color = "#8D23C2"),
      outlinecolor = "#8D23C2"
    )
    
    return(fig)
  })
  
  output$australia_map <- plotly::renderPlotly({
    
    # Subset of data to plot.
    data = WHO_deaths %>%
      filter(Year == 2021, Cause == "Intentional self-harm", Age_Range == "All", Sex == "All")
    
    fig = plot_geo(data)
    
    fig = fig %>% add_trace(
      z = ~Deaths, color = ~Deaths,
      text = ~Deaths, locations = ~ISO_Code
    ) %>% hide_colorbar() # Hide the colour bar.
    
    # Customisation parameters available here: https://plotly.com/r/reference/layout/geo/
    g <- list(
      showframe = FALSE,
      projection = list(type = "orthographic", rotation = list(lat = -10, lon = 170))
    )
    
    fig = fig %>% layout(
      geo = g
    )
    
    return(fig)

  })
  
  output$australia_deaths_chapter_2_part_2 <- DT::renderDataTable({
    formattable::formattable(chapter_2_part_2) %>%
      DT::datatable(escape = FALSE,
                    options = list(
                      scrollX = TRUE,
                      dom = 't' # Turn off the buttons to sort the data set.
                    ),
                    rownames = FALSE)
  })
  
  output$australia_deaths_chapter_2_part_3 <- DT::renderDataTable({
    formattable::formattable(chapter_2_part_3) %>%
      DT::datatable(escape = FALSE,
                    options = list(
                      scrollX = TRUE,
                      dom = 't' # Turn off the buttons to sort the data set.
                    ),
                    rownames = FALSE)
  })
  
  output$australia_deaths_chapter_2_part_4 <- DT::renderDataTable({
    formattable::formattable(chapter_2_part_4) %>%
      DT::datatable(escape = FALSE,
                    options = list(
                      scrollX = TRUE,
                      dom = 't' # Turn off the buttons to sort the data set.
                    ),
                    rownames = FALSE)
  })
  
  australia_deaths_data_exploration <- reactive({
    data = australia_deaths %>%
      filter(`Age Group` == input$age_select_chapter_2, grepl(input$sex_select_chapter_2, Sex), `Cause of Death` != "All causes") %>%
      
      # Make a ranking column (rank based off number of deaths).
      mutate(Rank = dense_rank(desc(Deaths))) %>%
      
      # Re-order columns to make Rank, Deaths, Percentage of Deaths and Rate per 100k the first columns.
      select(Rank, Deaths,`Percentage of Deaths`, `Rate per 100k`, `Cause of Death`, `Age Group`, Sex)
    
    return(data)
  })
  output$australia_deaths_exploration_table <-  DT::renderDataTable({
    DT::datatable(
      australia_deaths_data_exploration(),
      rownames = FALSE,
      options = list(
        dom = 'tp'
      )
    ) %>% 
      formatStyle(
        'Cause of Death',
        target = 'row',
        backgroundColor = styleEqual(c("Suicide "), c('#fabdb9'))
      )
  })
  
  output$australia_deaths_exploration_graph <- plotly::renderPlotly({
    
    # Get the rank of suicide in the data.
    subset = australia_deaths %>%
      filter(`Age Group` == input$age_select_chapter_2, grepl(input$sex_select_chapter_2, Sex), `Cause of Death` != "All causes")
    suicide = subset %>% filter(`Cause of Death` == "Suicide ")
    rows = nrow(suicide)
    
    # In the case where there is not row with suicide, just get the rows with the 5 largest causes of death.
    if (rows == 0) {
      subset = subset %>%
        filter(Rank <= 5)
    }
    
    # If sucicide does exist in the dataset, get the 2 causes of death ahead of it and below it.
    else {
      suicide_rank = suicide %>% select(Rank) %>% pull()
      bottom_bound = suicide_rank - 2
      upper_bound = suicide_rank + 2
      subset = subset %>%
        filter(Rank >= bottom_bound & Rank <= upper_bound)
    }

    # Filter to the ranks of interest.
    subset_for_plotting = subset %>%
      mutate(`Cause of Death` = paste(str_trunc(`Cause of Death`, 25), "(Rank ", as.character(Rank), ")", sep = "")) %>%
      
      # Add colours for custom colouring (every bar will be grey, except for suicide which will be red).
      mutate(
        colour = case_when(
          grepl("Suicide", `Cause of Death`) ~ "rgba(250,189,185,1)",
          TRUE ~ "rgba(204,204,204,1)"
        )
      )
    
    # Reorder the bars to be in order of death total.
    subset_for_plotting$`Cause of Death` <- factor(subset_for_plotting$`Cause of Death`, levels = subset_for_plotting$`Cause of Death`[order(subset_for_plotting$Deaths)])
    
    
    # Create column graph
    fig <- plot_ly(
      x = subset_for_plotting$`Cause of Death`,
      y = subset_for_plotting$Deaths,
      type = "bar",
      marker = list(color = subset_for_plotting$colour)
    ) %>% 
      layout(yaxis = list(title = "Deaths"), xaxis = list(title = "Cause of Death"))
    
    return(fig)
  })
  
  output$chapter_3_table <-  DT::renderDataTable({
    DT::datatable(
      chapter_2_part_2,
      rownames = FALSE,
      options = list(
        dom = 't'
      )
    ) %>% 
      formatStyle(
        'Deaths',
        target = 'row',
        backgroundColor = styleEqual(c("9641"), c('#fabdb9'))
      )
  })
  
  output$australian_all_hospitalisations_line_graph = plotly::renderPlotly({

    fig = scatter_data_hospitalisations %>% filter(Location == "Australia") %>%
      ggplot() +
      aes(x = Year, y = Hospitalisations) + 
      geom_line() +
      geom_point() +
      expand_limits(y = 0) +
      labs(y = "Hospitalisations") +
      ggtitle("Mental Health-Related Hospitalisations in Australia") +
      
      # Remove scientific notation.
      scale_y_continuous(labels = scales::comma) 
    
    fig = ggplotly(fig)
    
    return(fig)
  })
  
  output$states_hospitalisations_line_graph = plotly::renderPlotly({
    
    fig = scatter_data_hospitalisations %>% filter(Location != "Australia") %>%
      ggplot() +
      aes(x = Year, y = Hospitalisations, colour = Location) + 
      geom_line() +
      geom_point() +
      expand_limits(y = 0) +
      labs(y = "Hospitalisations") +
      ggtitle("Mental Health-Related Hospitalisations in Australia") +
      
      # Remove scientific notation.
      scale_y_continuous(labels = scales::comma) 
    
    fig = ggplotly(fig)
    
    return(fig)
  })

  data_for_leaflet_plot <- reactive({
    req(input$year_select_chapter_3)  # Ensure the input is available before proceeding
    
    # Filter data to only focus on data of interest.
    subset_australia = scatter_data_hospitalisations %>%
      filter(Location != "Australia") %>%
      filter(Year == input$year_select_chapter_3)
    
    # Join data.
    merged = australia_sf %>% 
      dplyr::left_join(subset_australia, by = c("STATE_NAME" = "Location"))
    
    return(merged)
  })
  labels_for_leaflet_plot <- reactive({
    # Labels for each state.
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Mental Health-Related Presentations",
      data_for_leaflet_plot()$STATE_NAME, data_for_leaflet_plot()$Hospitalisations
    ) %>% lapply(htmltools::HTML)
    return(labels)
  })
  output$australia_hospitalisation_map = renderLeaflet({
    
    # Make the colour palette for the leaflet be based upon the Value column.
    pal <- colorQuantile("YlOrRd", domain = data_for_leaflet_plot()$Hospitalisations)
    
    leaflet(data_for_leaflet_plot()) %>%
      addPolygons(
        fillColor = ~pal(Hospitalisations),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#56575c",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels_for_leaflet_plot(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      )
 
  })
  
  output$lifeline_image <- renderImage({
    list(src = "www/images/lifeline-logo.png", alt = "Lifeline logo")
  }, deleteFile = FALSE)
  
  output$lifeline_calls_answered <- plotly::renderPlotly({
    # Make plot of the number of calls that lifeline has made.
    fig = lifeline_data %>%
      ggplot() +
      aes(x = year, y = calls_answered) +
      geom_bar(stat = "identity", fill = "steelblue") +
      scale_y_continuous(labels = scales::comma, breaks = seq(0, 1250000, by = 250000)) +
      labs(y = "Calls Answered", x = "Period (Financial Year)") +
      ggtitle("Lifeline Calls Answered") +
      coord_flip()
    fig = ggplotly(fig)
    return(fig)
  })
  
  output$lifeline_call_answer_rate<- plotly::renderPlotly({
    # Make plot of the number of calls that lifeline has made.
    fig = lifeline_data %>%
      ggplot() +
      aes(x = year, y = call_answer_rate) +
      geom_bar(stat = "identity", fill = "steelblue") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.2)) +
      labs(y = "Percentage of Calls Answered", x = "Period (Financial Year)") +
      ggtitle("Lifeline Call Answer Rate") +
      coord_flip()
    fig = ggplotly(fig)
    return(fig)
  })
  
  
  
  
  output$suicide_deaths_scrolly <- renderScrollytell({scrollytell()})
  


  setup_pushbar() # setup
  
  
  observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) dark else light
  ))
  
  observeEvent(input$extra_info_open, {
    pushbar_open(id = "myPushbar")
  })  
  
  observeEvent(input$extra_info_close, {
    pushbar_close()
  }) 
  
  output$label1 = renderText("Text")
  
}



shinyApp(ui, server)