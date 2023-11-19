# The Suicide Epidemic: A Data Story

- Entry for the ['Matilda Viz!'](https://www.sydney.edu.au/matilda-centre/news-and-events/matilda-viz-a-data-visualisation-competition-open-to-all-students.html) competition (2023).

- Winner of the Judge's Choice Award ([Linkedin Post](https://www.linkedin.com/posts/the-matilda-centre_congratulations-to-the-winners-of-the-2023-activity-7128562091380133888-L5jR?utm_source=share&utm_medium=member_desktop)).

- Made by Thomas Elton.

Submission Dashboard Link: https://data-viz-2023.shinyapps.io/mental_health_app/

## Supporting Statment

**As part of the submission, we were asked to provide a statment about our dashboard. Here is my statment:**

My dashboard tells a story of the sad reality of suicide in the world and Australia. To build upon the data-story notion, I separated my dashboard into four chapters that each progressively explore suicide. The first chapter explores the lives lost from suicide globally, before chapters two and three focus specifically on suicide and mental health in Australia. We end with chapter four which focuses on a case study of the great work that Lifeline does in supporting Australians in crisis.

Data was sourced primarily from the World Health Organisation Mortality Database, as well as the Australian Institute of Health and Welfare. From there, data cleaning was conducted in R. This proved quite challenging as data from the mortality database had to be merged with other data sets. Additionally, the Australian data was not in a form suitable for plotting and analysis.

The dashboard was constructed using Shiny for R, and most of the plots leveraged the Plotly package to create interactive visualisations. Additionally, the leaflet package was utilised for creating a choropleth map. Multiple packages were utilised to bring the dashboard to life, and these are documented within the dashboard.

## Data Cleaning

### Dashboard Chapter 1

***Citations***

Haedo, J. (2017). *Countries ISO Codes.* https://www.kaggle.com/datasets/juanumusic/countries-iso-codes/

Our World in Data (n.d.). *Population, 10,000 BCE to 2021*. https://ourworldindata.org/grapher/population

World Health Organisation (2023). *WHO Mortality Database.* https://www.who.int/data/data-collection-tools/who-mortality-database

***Data Cleaning***

The data cleaning process for chapter 1 of the dashboard was very extensive. The data cleaning process can be found in the "Data_Cleaning/WHO_Suicide_and_Substance_Abuse_Deaths_Per_Country" folder.

The rendered markdown file has been published on RPubs [here](https://rpubs.com/tjelton/WHO-Mortality-DB-Suicide-and-Substance-Abuse-Deaths).

### Dashboard Chapter 2

***Citation***

Australian Government: Australian Institue of Health and Welfare (2023). *Data tables: Deaths in Australia.* https://www.aihw.gov.au/reports/life-expectancy-deaths/deaths-in-australia/data 

***Data Cleaning***

From the spreadsheet download, only the sheet labelled "Table S3.2" was used. This table collated the leading underlying causes of death by sex and age group for Australians from 2019 to 2021.

The data cleaning R code can be found in the "Data_Cleaning/Australia_Deaths" folder.

The rendered markdown file has been published on RPubs [here](https://rpubs.com/tjelton/australian-deaths-2019-to-2021).

### Dashboard Chapter 3

***Citation***

Australian Government: Australian Institute of Health and Welfare (2023). Data tables: Mental health services provided in emergency departments 2021–22. https://www.aihw.gov.au/mental-health/topic-areas/emergency-departments

***Data Cleaning***

From the spreadsheet donwload, only the sheet labelled “Table ED.4” was used. The name of this table is “Mental health-related emergency department presentations in public hospitals, by states and territories, 2014–15 to 2021–22.”

The data cleaning R code can be found in the "Data_Cleaning/Emergencey_Department_Presentations" folder.

The rendered markdown file has been published on RPubs [here](https://rpubs.com/tjelton/mental-health-related-emergency-department-presentations-australia).

***Miscellaneous***

In order to create the chloropleth map of Australia, a geojson file of the boundaries of Australia's states and territories is required. Check out the following to get this data:

Hogan, R. (2014). australian-states/states.geojson. https://github.com/rowanhogan/australian-states/blob/master/states.geojson

### Dashboard Chapter 4

***Citation***

Lifeline (n.d). *Lifeline's Annual reports.* https://www.lifeline.org.au/about/governance/annual-reports/

***Data***

The data for Lifeline's calls answered and the call answer rate were manually taken taken from Lifeline's annual reports. The following lines of code can be used to construct the data frame/tibble used in chapter 4 of the dashboard.

```
year = c("21-22", "20-21", "19-20", "18-19", "17-18", "16-17", "15-16", "14-15")
calls_answered = c(1142234,1070860,835867,731646, 732365,793397, 831744, 821804)
call_answer_rate = c(87,90.34,84.5,80,82,85,85,85)/100
lifeline_data = tibble(year, calls_answered, call_answer_rate) 
```
