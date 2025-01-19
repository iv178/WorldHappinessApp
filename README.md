# World Happiness Report Analysis App

## Description
This Shiny app provides an interactive interface to analyze the World Happiness Report dataset. Users can upload the dataset, explore summary statistics, perform bivariate analysis, visualize data using dynamic plots, and view insights through maps and tables.

## Features
- **Interactive Visualization**: Generate scatterplots, boxplots, histograms, and bar charts dynamically.
- **Summary Statistics**: Provides detailed summaries of selected variables.
- **Bivariate Analysis**: Correlation and ANOVA-based analysis depending on the variable types.
- **Top 5/Bottom 5 Analysis**: Display the happiest and least happy countries.
- **Regional Analysis**: Summarizes happiness scores by region.
- **Map Visualization**: Displays country happiness scores on an interactive map.

## Setup Instructions

### Prerequisites
- R (version 4.0 or higher)
- RStudio (optional, but recommended)
- Required R packages: `shiny`, `ggplot2`, `dplyr`, `plotly`, `leaflet`

### Installation
1. Install the required packages if not already installed:
   ```R
   install.packages(c("shiny", "ggplot2", "dplyr", "plotly", "leaflet"))
