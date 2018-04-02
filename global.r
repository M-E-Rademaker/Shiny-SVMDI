#---------------------------------------------------------------------------------------------------
#
# Project: Shiny app for CSVMD Index proposed by Gründler and Krieger (2016)
#
#---------------------------------------------------------------------------------------------------
### Install packages if necessary

# if(!require(shiny)) install.packages("shiny")
# if(!require(broom)) install.packages("shinythemes")
# if(!require(tidyverse)) install.packages("tidyverse")
# if(!require(leaflet)) install.packages("leaflet")
# if(!require(maps)) install.packages("maps")
# if(!require(haven)) install.packages("haven")
# if(!require(broom)) install.packages("broom")


### Load required packages

library(shiny)
library(shinythemes)
library(tidyverse)
library(stringr)
library(leaflet)
library(maps)
library(haven)
library(broom)

### Load data (only once, when the app starts) -----------------------------------------------------

load("data/dat_csvmdi.RData")
