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

### Dashboard Chapter 3

***Citation***

Australian Government: Australian Institue of Health and Welfare (2023). *Data tables: Deaths in Australia.* https://www.aihw.gov.au/reports/life-expectancy-deaths/deaths-in-australia/data 

***Data Cleaning***

From the spreadsheet download, only the sheet labelled "Table S3.2" was used. This table collated the leading underlying causes of death by sex and age group for Australians from 2019 to 2021.

The data cleaning R code can be found in the "Data_Cleaning/Australia_Deaths" folder.

The rendered markdown file has been published on RPubs [here](https://rpubs.com/tjelton/australian-deaths-2019-to-2021).


