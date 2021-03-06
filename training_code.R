# code to create a data set to use with the pc training
# Developed by: Apratim Mitra & Jenny Lee Hurst

# libraries needed to run this analysis
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(maps)

# set working directory to current folder
# NOTE: update this to reflect current location of the script
setwd('~/workspace/pc-training/covid-plots-training')

# name of the data file
data.file <- 'covid-data.xlsx'

# Here we find out the names of the sheets in the excel file
# with the excel_sheets function
sheet.names <- excel_sheets(data.file)

# empty data frame to hold the combined data from the sheets in the excel file
long.df <- data.frame()

# loop to repeat for sheets in excel file
for(i in 1:length(sheet.names)){
    # read sheet data into data frame
    # This function takes the name of the excel file and the name of the sheet as arguments
    covid.df <- read_excel(data.file, sheet=sheet.names[i])

    # - Excel converts dates into numbers, e.g. 43852 == 01-22-2020 or 22nd January
    # - We first convert column names to numeric format
    # - If the output is not NA, then that column name is a date. We check that with the is.na() function
    # - is.na() returns a vector of TRUE or FALSE values. TRUE corresponds to NA in numeric.cols,
    #   while FALSE is not NA. We want the FALSE locations.
    # - the which() function returns the TRUE locations. These are the date columns.
    numeric.cols <- as.numeric(colnames(covid.df))
    date.cols <- which(!is.na(numeric.cols))

    # Some countries have province/state info, some don't. We want to work with country-level data, so
    # here we sum all the state-level numbers into one value for each country.
    #
    # 1. For this, we first group the rows by region
    # 2. We select the date columns that we calculated above
    # 3. Then we sum all the state numbers for each country
    states.df <- covid.df %>%
        group_by(region) %>%
        select(all_of(date.cols)) %>%
        summarize_each(funs=sum)

    # 1. Long-form data vs short-form data
    # 
    # The above step generates data which is of the form:
    #
    # country      1-22-2020   2-22-2020 .  .  .
    # Afghanistan  0           0
    # Albania      0           0
    # .
    # .
    #
    # So, each row represents a country, with numbers from all dates in different columns.
    # This is called long-form data. For visualization and other subsequent analysis, it is
    # often useful to use short-form data, e.g.
    #
    # country      name       value
    # Afghanistan  1-22-2020  0
    # Afghanistan  2-22-2020  0
    # .
    # .
    # Albania      1-22-2020  0
    # Albania      2-22-2020  0
    #
    # Note:
    #
    # - Here we have multiple rows for each country, but we have only 3 columns. By default, the
    #   command creates a 'name' column with the column names, and 'value' column with the values
    #   in the data frame.
    # - We rename the 'name' column to date using the 'names_to' argument.
    # - We rename the 'value' column to the name of the sheet, using the 'values_to' argument.
    #
    #
    # 2. Convert date format with mutate
    #
    # mutate() can be used to create new columns in the data set. Here we convert the Excel
    # number format to dates.
    #
    # - first, the date values have to be numeric
    # - then, the as.Date() function with the argument origin='1899-12-30' has to be used.
    #
    # Note:
    #
    # - Since the new column is also called 'date', it just replaces the old 'date' column.
    # - We rename the 'value' column to the name of the current sheet.
    countries.df <- states.df %>%
        pivot_longer(-region,
                     names_to='date',
                     values_to=sheet.names[i]) %>%
        mutate(date=as.Date(as.numeric(date), origin='1899-12-30'))

    # Here we accumulate the data from the multiple sheets into the long.df data frame
    #
    # - The first time this is run, 'long.df' is empty. So, the current data set (countries.df) is
    #   assigned to 'long.df'
    # - On subsequent loops, 'countries.df' has one new column corresponding to the current sheet being read
    #   that does not already exist in 'long.df'
    # - So we do an inner_join to add this new column to 'long.df'. 'inner_join' compares the two data sets
    if(nrow(long.df) == 0){
      long.df <- countries.df
    } else {
      long.df <- inner_join(long.df, countries.df)
    }
}

# Here we select the dates to be plotted on maps. For this we select rows from the
# data set using the filter() function.
#
# - The 'plot.dates' variable holds the dates to be plotted. The as.Date() function is used here
#   to convert the strings to dates. This is necessary since we will be filtering dates from
#   the long.df data frame.
# - The filter() function is used to filter on the date column. '%in%' checks for the presence of
#   values from 'plot.dates' in the date column of 'long.df'
#
plot.dates <- as.Date(c('2020-03-01', '2020-05-01'))
long.df.subset <- long.df %>%
  filter(date %in% plot.dates)

# The latitude and longitude of country boundaries can be obtained from the map_data()
# function that is part of the maps package.
countries.data <- map_data('world')

# Here we match the covid data to the map data using inner_join.
#
# - The inner_join function looks for common columns in the data sets being joined.
# - if there are matching columns, it looks for values in the matching columns that are shared by
#   both data sets.
# - on finding shared values, it keeps the corresponding rows from both data sets
#
long.df.subset.map <- inner_join(long.df.subset, countries.data)

# Create map visualizing confirmed cases with ggplot

# First we create the base plot using the ggplot() function. It has two 
# necessary components:
#
# - data frame to be used for plotting
# - aes(), which is used to define aesthetics of the plot. Common elements
#   include 'x', 'y'. Others used here include:
#      - 'fill=confirmed' sets the map to be colored by confirmed cases
#      - 'label=region' will cause the mouse tooltip to show the region (country) name
#
p1 <- ggplot(long.df.subset.map, aes(x=long, y=lat, label=region,
                                     fill=confirmed, group=group))

# Now we add the geom_polygon function to draw the map boundaries
# - color specifies the boundary color
# - size defines the thickness of the line
p1 <- p1 + geom_polygon(color='gray', size=0.25)

# Next, we facet (or split) the plot by date. The number of columns (ncol)
# is set to 1, so this will create a 1-column stacked view of plots.
p1 <- p1 + facet_wrap(~ date, ncol=1)

# The default color gradient is often not very pretty. Here we adjust the plotting
# scale:
# 
# - We use 'Blues' as the color gradient
# - We perform a 'log10' transformation, which is often helpful to visualize
#   data that has a wide range of values, such as here, where numbers range from 0 to
#   1000000+
# - log10 of 0 returns an NA value. Here we set the color for NA's to 'white'
p1 <- p1 + scale_fill_gradientn(trans='log10', na.value='white',
                                colors = RColorBrewer::brewer.pal(9,'Blues'))

# Finally we make a few more tweaks
#
# - theme_bw() creates a black-and-white theme with a white background for the 
#   plot which typically makes it look cleaner
# - we also turn off major and minor grid lines and set legend position
#
# Note: the legend position is not changed if using ggplotly
p1 <- p1 + theme_bw() + theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank(),
                              legend.position='top')

# Render the plot object with ggplotly
ggplotly(p1)

# Here we create a plot for the death numbers. This is almost identical to the above,
# except for 'fill = deaths' and the color gradient which uses 'Reds'
p2 <- ggplot(long.df.subset.map, aes(x=long, y=lat, label=region, fill=deaths, group=group)) +
    geom_polygon(color='gray', size=0.25) +
    facet_wrap(~ date, ncol=1) +
    scale_fill_gradientn(trans='log10', na.value='white',
                         colors = RColorBrewer::brewer.pal(9,'Reds')) + 
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          legend.position='top')
ggplotly(p2)

# Create line plots of death rates

# Here we create a data frame which contains the max deaths for each country
# by using select() and 'max' as the summarizing function
max.deaths <- long.df %>%
    group_by(region) %>%
    select(deaths) %>%
    summarize_each(funs=max)

# Here we filter the countries that have at least 1000 deaths. For this
# we use filter() and select()
countries.to.plot <- max.deaths %>%
    filter(deaths > 1000) %>%
    select(region)

# Finally we calculate death rates 
# - We use mutate to create a new column 'death_rate' which is calculated
#   as a percentage: deaths*100/confirmed
# - We also plot from March 1 (post February 29) since that's when many countries
#   started reporting cases
death.rates <- long.df %>%
    filter(region %in% countries.to.plot$region) %>%
    mutate(death_rate = deaths*100/confirmed) %>%
    filter(date > as.Date('2020-02-29'))

# Here we visualize the death rates as a line plot
# 
# - The base plot contains ggplot() and geom_line() elements
# - we also include the tweaks to the theme as above
d1 <- ggplot(death.rates, aes(y=death_rate, x=date, group=region, color=region)) + 
  geom_line() +
  theme_bw() + theme(panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank())

# Render the interactive plot
ggplotly(d1)

