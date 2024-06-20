# Load used Libraries within application
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rvest)


# Data Scraping: Get data from alcohol-consumption of the world and GDP
gdp_url <- 'https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)_per_capita#table'
alcohol_url <- 'https://en.wikipedia.org/wiki/List_of_countries_by_alcohol_consumption_per_capita'

# Function to scrape GDP data
scrape_gdp_data <- function(url) {
  page <- read_html(url)
  table <- page %>% html_table(fill = TRUE)
  gdp_data <- table[[2]]
  colnames(gdp_data) <- c("Rank", "Country", "GDP_per_capita")
  gdp_data <- gdp_data %>% select(Country = `Country/Territory`, GDP_per_capita = `IMF Estimate`)
  return(gdp_data)
}

# Function to scrape Alcohol consumption data
scrape_alcohol_data <- function(url) {
  page <- read_html(url)
  table <- page %>% html_table(fill = TRUE)
  alcohol_data <- table[[2]]
  colnames(alcohol_data) <- c("Country", "Alcohol_consumption")
  alcohol_data <- alcohol_data %>% select(Country, Alcohol_consumption)
  return(alcohol_data)
}

gdp_data <- scrape_gdp_data(gdp_url)
alcohol_data <- scrape_alcohol_data(alcohol_url)
