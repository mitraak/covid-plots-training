# code to create a data set to use with the pc training
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)

data.file <- '~/workspace/pc-training/covid-data.xlsx'

# this will be replaced by sheet names
sheet.names <- excel_sheets(data.file)

long.df <- data.frame()
for(i in 1:length(sheet.names)){
    covid.df <- read_excel(data.file, sheet=sheet.names[i])
    date.cols <- which(!is.na(as.numeric(colnames(covid.df))))
    
    # some countries have province/state info, those will be summarized
    states.df <- covid.df %>%
                    group_by(`Country/Region`) %>%
                    select(all_of(date.cols)) %>%
                    summarize_each(funs=sum)
    
    # pivot to longer form and convert date format
    countries.df <- states.df %>%
                      pivot_longer(-`Country/Region`,
                                   names_to='date',
                                   values_to=sheet.names[i]) %>%
                      mutate(date=as.Date(as.numeric(date), origin='1899-12-30'))
    
    if(nrow(long.df) == 0){
      long.df <- countries.df
    } else {
      long.df <- inner_join(long.df, countries.df)
    }
}

# change column name of long.df
long.df <- long.df %>%
  rename(region=`Country/Region`)

# date to plot on map
plot.dates <- as.Date(c('2020-03-01', '2020-05-01'))
long.df.subset <- long.df %>%
  filter(date %in% plot.dates)

# create maps
countries.data <- map_data('world')

# countries long df
long.df.subset.map <- inner_join(long.df.subset, countries.data)

# create map

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
  geom_polygon(color='gray', size=0.25) + facet_wrap(~ date, ncol=1) +
  scale_fill_gradientn(trans='log10', na.value='white',
                       colors = RColorBrewer::brewer.pal(9,'Reds')) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position='top')
ggplotly(p2)

# choose country to plot
# p3 <- long.df.subset.map %>% filter(region == 'China') %>%
#   ggplot(aes(x=long, y=lat, label=region, fill=confirmed, group=group)) +
#   geom_polygon(color='gray', size=0.25) + facet_wrap(~ date, ncol=1) +
#   scale_fill_gradientn(trans='log10', na.value='white',
#                        colors = RColorBrewer::brewer.pal(9,'Blues')) +
#   theme_bw() +
#   theme(panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         legend.position='top')
# ggplotly(p3)

# max deaths for each country
max.deaths <- long.df %>% group_by(region) %>% select(deaths) %>%
  summarize_each(funs=max)

# choose countries with min 500 deaths
countries.to.plot <- max.deaths %>% filter(deaths > 1000) %>% select(region)

# calculate death rate
death.rates <- long.df %>%
  filter(region %in% countries.to.plot$region) %>%
  mutate(death_rate = deaths*100/confirmed) %>%
  filter(date > as.Date('2020-02-29'))

d1 <- ggplot(death.rates, aes(y=death_rate, x=date, group=region, color=region)) +
  geom_line() + theme_bw() + theme(panel.grid.major=element_blank(),
                                   panel.grid.minor=element_blank())
ggplotly(d1)

