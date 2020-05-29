# code to create a data set to use with the pc training

# libraries needed to run this analysis
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)

# set working directory to current folder
# NOTE: update this to reflect current location of the script
setwd('~/workspace/pc-training/covid-plots-training')

# location of the data file
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
    #   FALSE == not NA. We want the TRUE locations.
    # - the which() function returns the TRUE locations. These are the date columns
    numeric.cols <- as.numeric(colnames(covid.df))
    date.cols <- which(!is.na(numeric.cols))

    # Some countries have province/state info, some don't. We want to work with country-level data, so
    # here we sum all the state-level numbers into one value for each country.
    #
    # 1. For this we first group the rows by 'Country/Region'
    # 2. We select the date columns that we calculated above
    # 3. Then we sum all the state numbers for each country
    states.df <- covid.df %>%
        group_by(`Country/Region`) %>%
        select(all_of(date.cols)) %>%
        summarize_each(funs=sum)

    # 1. Long-form data vs short-form data
    # The above step generates long-form data which is of the form:
    #
    # country      1-22-2020   2-22-2020 .  .  .
    # Afghanistan  0           0
    # Albania      0           0
    #
    # So, each row represents a country, with numbers from all dates in different columns.
    # This is called long-form data. For each of visualization and other analysis, it is
    # often useful to use short-form data, e.g.
    #
    # country      date       value
    # Afghanistan  1-22-2020  0
    # Afghanistan  2-22-2020  0
    # .
    # .
    # Albania      1-22-2020  0
    # Albania      2-22-2020  0
    #
    # Note that here we have multiple rows for each country, but we have only 3 columns.
    #
    # 2. Convert date format with mutate
    #
    # mutate() can be used to create new columns in the data set. Here we convert the Excel
    # number format to dates.
    #
    # - first, the date values have to be numeric
    # - then, the as.Date() function with the argument origin='1899-12-30' has to be used.
    #
    # Note that since the new column is also called 'date', it just replaces the old 'date' column.
    countries.df <- states.df %>%
        pivot_longer(-`Country/Region`,
                     names_to='date',
                     values_to=sheet.names[i]) %>%
        mutate(date=as.Date(as.numeric(date), origin='1899-12-30'))

    # Here we accumulate the data from the multiple sheets into the long.df data frame
    #
    # - The first time this is run, long.df is empty. So, the current data set (countries.df) is
    #   assigned to long.df
    # - On subsequent loops, we do an inner_join to add the value from the new sheet as a new column
    #   to long.df
    if(nrow(long.df) == 0){
      long.df <- countries.df
    } else {
      long.df <- inner_join(long.df, countries.df)
    }
}

# Here we use the rename() function to rename the 'Country/Region' column to
# 'region'. This will be used to match the covid data to the map coordinates.
long.df <- long.df %>%
  rename(region=`Country/Region`)

# Here we select the dates to be plotted on maps. For this we select rows from the
# data set using the filter() function.
#
# - The plot.dates variable holds the dates to be plotted
# - The filter() function is used to filter on the date column. '%in%' checks for the present of
#   values from plot.dates in the date column of long.df
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

# Create map

# confirmed
p1 <- ggplot(long.df.subset.map, aes(x=long, y=lat, label=region,
                                     fill=confirmed, group=group)) +
  geom_polygon(color='gray', size=0.25) + facet_wrap(~ date, ncol=1) +
  scale_fill_gradientn(trans='log10', na.value='white',
                       colors = RColorBrewer::brewer.pal(9,'Blues')) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position='top')
ggplotly(p1)

# deaths
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

# max deaths for each country
max.deaths <- long.df %>%
    group_by(region) %>%
    select(deaths) %>%
    summarize_each(funs=max)

# choose countries with min 500 deaths
countries.to.plot <- max.deaths %>%
    filter(deaths > 1000) %>%
    select(region)

# calculate death rate
death.rates <- long.df %>%
  filter(region %in% countries.to.plot$region) %>%
  mutate(death_rate = deaths*100/confirmed) %>%
  filter(date > as.Date('2020-02-29'))

d1 <- ggplot(death.rates, aes(y=death_rate, x=date, group=region, color=region)) +
  geom_line() + theme_bw() + theme(panel.grid.major=element_blank(),
                                   panel.grid.minor=element_blank())
ggplotly(d1)

