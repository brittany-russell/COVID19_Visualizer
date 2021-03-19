# COVID19_Visualizer

Repository for R Shiny App for visualizing the COVID-19 pandemic, using data from the 
COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University
(JHU CSSE COVID-19 Data for short, https://github.com/CSSEGISandData/COVID-19).

The app allows users to view cumulative and daily (rolling average) cases and deaths for a selected state and county, 
as well as compare to the national numbers. All cases and deaths are adjusted per capita for comparability.

Run the app in R/RStudio with the commands: 

library(Shiny) 

runGitHub("COVID19_Visualizer", "brittany-russell", ref = "main"). 

It requires the following packages: Shiny, Tidyverse, magrittr, and zoo.

