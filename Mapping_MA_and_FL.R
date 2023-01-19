
# Generating Maps for Health Insurance Coverage and other variables
# for Massachusetts and Florida  (2012-2019)
#
# For this code, we customized the code presented for Case Study #4 of
# Public Health Disparities Geocoding Project 2.0 Training Manual
# available at: https://phdgp.github.io/PHDGP2.0/index.html

# Dependencies ------------------------------------------------------------

# We need to load the following packages:
library(tidycensus)
library(tidyverse)
library(tidyr)
library(magrittr) # magrittr defines the %<>% operator, read more here: https://magrittr.tidyverse.org/
library(patchwork)


# Data Cleaning -----------------------------------------------------------

# build a data dictionary - based on ACS 2013
data_dictionary <- tibble::tribble(
  ~variable,     ~shortname,                       ~description,
  "B18135_002",  'under18_denom',                   "Estimate!!Total!!Under 18 years",
  "B18135_004",  'under18_insured_with_disability', "Estimate!!Total!!Under 18 years!!With a disability!!With health insurance coverage",
  "B18135_009",  'under18_insured_no_disability',   "Estimate!!Total!!Under 18 years!!No disability!!With health insurance coverage",
  "B18135_013",  'adult_denom',                     "Estimate!!Total!!18 to 64 years",
  "B18135_015",  'adult_insured_with_disability',   "Estimate!!Total!!18 to 64 years!!With a disability!!With health insurance coverage",
  "B18135_020",  'adult_insured_no_disability',     "Estimate!!Total!!18 to 64 years!!No disability!!With health insurance coverage",
  # ICEraceinc variables
  "B19001_001",  'hhinc_total',   "total population for household income estimates",
  "B19001A_002", 'hhinc_w_1',     "white n.h. pop with household income <$10k",
  "B19001A_003", 'hhinc_w_2',     "white n.h. pop with household income $10k-14 999k",
  "B19001A_004", 'hhinc_w_3',     "white n.h. pop with household income $15k-19 999k",
  "B19001A_005", 'hhinc_w_4',     "white n.h. pop with household income $20k-24 999k",
  "B19001A_014", 'hhinc_w_5',     "white n.h. pop with household income $100 000 to $124 999",
  "B19001A_015", 'hhinc_w_6',     "white n.h. pop with household income $125k-149 999k",
  "B19001A_016", 'hhinc_w_7',     "white n.h. pop with household income $150k-199 999k",
  "B19001A_017", 'hhinc_w_8',     "white n.h. pop with household income $196k+",
  "B19001_002",  'hhinc_total_1', "total pop with household income <$10k",
  "B19001_003",  'hhinc_total_2', "total pop with household income $10k-14 999k",
  "B19001_004",  'hhinc_total_3', "total pop with household income $15k-19 999k",
  "B19001_005",  'hhinc_total_4', "total pop with household income $20k-24 999k",
  "B05010_002",  'in_poverty',    "total pop in poverty",
  "B05010_001", 'total_pop_for_poverty_estimates', 
  "denominator population for poverty estimates"
)

# another way to create a data dictionary is to use tidycensus to get the variables tables and filter it:
acs2012variables <- tidycensus::load_variables(year = 2012, dataset = 'acs5')
acs2012variables %<>% filter(
  name %in% c("B18135_002", "B18135_004", "B18135_009", "B18135_013", "B18135_015", "B18135_020")) 
# then you could append `shortname` data if you'd like

# write a function to help query county insurance rates by state
get_county_health_insurance <- function(state, year) {
  get_acs(state = state, year = year, geography = 'county', variables = data_dictionary$variable)
}

# create a table shell for each year of data for each of massachusetts and florida
health_insurance <- expand.grid(state = c('MA', 'FL'), year = 2012:2019)

# query the data from ACS
health_insurance %<>% 
  rowwise() %>% 
  mutate(
    acs_data = list(get_county_health_insurance(state = state, year = year))) 

# unnest our data
health_insurance %<>% tidyr::unnest(cols = acs_data)

# recode variables to use friendlier shortnames
# the !!! operator, sometimes called the splat operator, expands a named vector into named argumes for a function;
# in this case, the output of setNames() is a named character vector with variable shortnames as the names and 
# variable codes as the values.  Passing these to !!! and then recode says "rename the variable codes to the 
# more human-readable, shortnames"
health_insurance$variable %<>% recode( !!! setNames(data_dictionary$shortname, data_dictionary$variable))

# pivot into wide format
health_insurance %<>% 
  tidyr::pivot_wider(
    id_cols = c('year', 'state', 'GEOID', 'NAME'),
    names_from = 'variable',
    values_from = c('estimate', 'moe'))

# calculate adult (18+ years), child, and 0-64 insurance rates by county and year 
# Below we are add the estimated number of insured adults with a disability and the number of insured adults without a disability 
# to get the total number of insured adults. We do the same for children and people aged 0-64 years.
health_insurance %<>% mutate(
  # calculate numerators
  estimate_adult_insurance_numerator = (estimate_adult_insured_with_disability + estimate_adult_insured_no_disability),
  estimate_under18_insurance_numerator = (
    estimate_under18_insured_with_disability + estimate_under18_insured_no_disability
  ),
  estimate_zero_to_64_insurance_numerator = (
    estimate_adult_insured_with_disability +
      estimate_adult_insured_no_disability +
      estimate_under18_insured_with_disability +
      estimate_under18_insured_no_disability
  ),
  # calculate denominator
  estimate_zero_to_64_insurance_denom = estimate_under18_denom + estimate_adult_denom,
  # calculate proportions
  estimate_adult_insurance_prop = estimate_adult_insurance_numerator / estimate_adult_denom,
  estimate_under18_insurance_prop = estimate_under18_insurance_numerator / estimate_under18_denom,
  estimate_zero_to_64_insurance_prop = estimate_zero_to_64_insurance_numerator / (estimate_zero_to_64_insurance_denom),
  
  # calculate social metrics
  poverty_prop = estimate_in_poverty / estimate_total_pop_for_poverty_estimates,
  
  # we calculate the people of color low income counts as the overall 
  # low income counts minus the White non-Hispanic low income counts
  people_of_color_low_income = 
    (estimate_hhinc_total_1 + estimate_hhinc_total_2 + estimate_hhinc_total_3 + estimate_hhinc_total_4) - 
    (estimate_hhinc_w_1 + estimate_hhinc_w_2 + estimate_hhinc_w_3 + estimate_hhinc_w_4),
  # sum up the White non-Hispanic high income counts
  white_non_hispanic_high_income = 
    (estimate_hhinc_w_5 + estimate_hhinc_w_6 + estimate_hhinc_w_7 + estimate_hhinc_w_8),
  # calculate the index of concentration at the extremes for racialized 
  # economic segregation (high income White non-Hispanic vs. low income 
  # people of color)
  ICEraceinc = 
    (white_non_hispanic_high_income - people_of_color_low_income) / 
    estimate_hhinc_total,
)

# use the moe_sum and moe_prop functions from tidycensus to create margin of error
# for aggregated estimates and proportion variables
health_insurance %<>% rowwise() %>% 
  mutate(
    # calculate moe for numerators
    moe_adult_insurance_numerator = tidycensus::moe_sum(
      moe = c(moe_adult_insured_with_disability, moe_adult_insured_no_disability),
      estimate = c(estimate_adult_insured_with_disability, estimate_adult_insured_no_disability)), 
    moe_under18_insurance_numerator = tidycensus::moe_sum(
      moe = c(moe_under18_insured_with_disability, moe_under18_insured_no_disability),
      estimate = c(estimate_under18_insured_with_disability, estimate_under18_insured_no_disability)), 
    moe_zero_to_64_insurance_numerator = tidycensus::moe_sum(
      moe = c(moe_adult_insured_with_disability,
              moe_adult_insured_no_disability,
              moe_under18_insured_with_disability,
              moe_under18_insured_no_disability),
      estimate = c(estimate_adult_insured_with_disability,
                   estimate_adult_insured_no_disability,
                   estimate_under18_insured_with_disability,
                   estimate_under18_insured_no_disability)),
    # calculate moe for proportions
    moe_adult_insurance_prop = tidycensus::moe_prop(
      num = estimate_adult_insurance_numerator,
      denom = estimate_adult_denom,
      moe_num = moe_adult_insurance_numerator,
      moe_denom = moe_adult_denom), 
    moe_under18_insurance_prop = tidycensus::moe_prop(
      num = estimate_under18_insurance_numerator,
      denom = estimate_under18_denom,
      moe_num = moe_under18_insurance_numerator,
      moe_denom = moe_under18_denom), 
    moe_zero_to_64_insurance_prop = tidycensus::moe_prop(
      num = estimate_zero_to_64_insurance_numerator,
      denom = estimate_zero_to_64_insurance_denom,
      moe_num = moe_zero_to_64_insurance_numerator,
      moe_denom = tidycensus::moe_sum(
        moe = c(moe_under18_denom, moe_adult_denom),
        estimate = c(estimate_under18_denom, estimate_adult_denom))
    ))

# use full state names
health_insurance$state %<>% recode(
  FL = 'Florida',
  MA = 'Massachusetts')

health_insurance$state %<>% factor(levels = sort(c('Florida', 'Massachusetts')))

# remove any unncessary variables going forward
health_insurance %<>% select(
  year, state, GEOID, NAME,
  estimate_adult_denom,
  estimate_under18_denom,
  estimate_zero_to_64_insurance_denom,
  estimate_adult_insurance_numerator,
  estimate_under18_insurance_numerator,
  estimate_zero_to_64_insurance_numerator,
  estimate_adult_insurance_prop,
  estimate_under18_insurance_prop,
  estimate_zero_to_64_insurance_prop,
  moe_adult_insurance_prop,
  moe_under18_insurance_prop,
  moe_zero_to_64_insurance_prop,
  poverty_prop,
  ICEraceinc
)

# table the top few lines of our dataframe so we can check that everything is
knitr::kable(head(health_insurance))



# Plotting line graphs --------------------------------------------------------------

#installing colorblindr package
# remotes::install_github("wilkelab/cowplot")
# install.packages("colorspace", repos = "http://R-Forge.R-project.org")
# remotes::install_github("clauswilke/colorblindr")
library(colorblindr)
# install.packages("ggokabeito")
library (ggokabeito)
library (ggplot2)
# install.packages("palette_OkabeIto")
# library(palette_OkabeIto)
library(stats)

# create plot of county level % with health insurance by year and smooth with geom_smooth method = 'loess' by year 
plot_ma_fl_trends <- ggplot(health_insurance, aes(x = year, y = estimate_zero_to_64_insurance_prop, color = state)) + 
  geom_jitter(alpha = 0.6, height = 0) + 
  geom_smooth(method = 'loess', color = 'dimgrey') + 
  facet_wrap(~state) + 
  xlab("Year") + 
  ylab("Percent with Health Insurance (age 0-64)") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  ggtitle("% with Health Insurance by County and State", 
          "Each dot represents a county observation from ACS\nSmooth fit shows average county observation")

# here's a version using ggdist to plot quantile ribbons on top of the jittered points 
library(ggdist)
ggplot(health_insurance,
       aes(
         x = year,
         y = estimate_zero_to_64_insurance_prop,
         fill = factor(state),
         color = factor(state)
       )) +
  geom_jitter(alpha = 0.3, height = 0) + 
  stat_lineribbon(aes(fill_ramp = stat(level)), color = 'black', alpha = 0.6) + 
  facet_wrap(~state) + 
  xlab("Year") + 
  ylab("Percent with Health Insurance (ages 0-64)") + 
  ggtitle("% with Health Insurance by County and State, Ages 0-64", 
          "Each dot represents a county observation from ACS\nSmoothed line represents median county estimate") +
  labs(
    fill_ramp = 'Quantile Range',
    color = 'State',
    fill = 'State',
  ) + 
  theme_bw() + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  guides(color = guide_legend(reverse = TRUE)) + 
  scale_y_continuous(labels = scales::percent_format())

# Plotting Animated Maps --------------------------------------------------------------
library (tidycensus)
library(tigris)
library(dplyr)
# install.packages("gganimate")
library(gganimate)
# install.packages("gifski")
library(gifski)
# install.packages("transformr")
library(transformr)
# install.packages("rayshader")
# library(rayshader) # this is needed only to create 3D maps
library(cowplot)

# charging sf data
counties_sf <- 
  tigris::counties(
    state = c('FL', 'MA'),
    cb = TRUE,
    resolution = '20m') # here we're downloading 1:20m resolution data 

# connecting counties do the dataset
health_insurance_sf <- inner_join(
  counties_sf %>% select(GEOID, STATE_NAME), # when joining spatial data in, put the spatial data argument first so
  health_insurance,
  by = c('GEOID' = 'GEOID'))

#creating a dataset only with data for MA
health_insurance_sf_ma <- health_insurance_sf[ which(health_insurance_sf$STATE_NAME=='Massachusetts'),]
health_insurance_sf_ma

#creating a dataset only with data for FL
health_insurance_sf_fl <- health_insurance_sf[ which(health_insurance_sf$STATE_NAME=='Florida'),]
health_insurance_sf_fl

# creating a base map without data for setting the background details
base_map_ma <- ggplot( data=health_insurance_sf_ma, aes()) + 
  geom_sf(color = 'white') 
base_map_ma

# Plotting animated maps for Massachusetts by year
animated_plot_ma <- ggplot( data=health_insurance_sf_ma, aes(fill = estimate_zero_to_64_insurance_prop, label = 'Percent with Health Insurance' )) + 
  geom_sf(color = 'white', size = .15) +
  scale_fill_distiller(palette = 'Blues', direction = 1, labels = scales::percent_format()) + 
  labs(fill = 'Health Insurance Proportion ages 0-64') +
  theme_minimal_vgrid(9) +
  transition_time(year) +
  ggtitle('Year: {frame_time}',
  subtitle = 'Frame {frame} of {nframes}')

#setting the number of frames = years
num_years <- max(health_insurance_sf_ma$year) - min(health_insurance_sf_ma$year) + 1
animate(animated_plot_ma, nframes = num_years, fps = 2) 
anim_save("ma01.gif")

# Plotting animated maps for Florida by year
animated_plot_fl <- ggplot( data=health_insurance_sf_fl, aes(fill = estimate_zero_to_64_insurance_prop, label = 'Percent with Health Insurance' )) + 
  geom_sf(color = 'white', size = .15) +
  scale_fill_distiller(palette = 'Blues', direction = 1, labels = scales::percent_format()) + 
  labs(fill = 'Health Insurance Proportion ages 0-64') +
  theme_minimal_vgrid(9) +
  transition_time(year) +
  ggtitle('Year: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}')

#setting the number of frames = years
num_years <- max(health_insurance_sf_ma$year) - min(health_insurance_sf_ma$year) + 1
animate(animated_plot_fl, nframes = num_years, fps = 2) 
anim_save("fl01.gif")


# Customizing projections -------------------------------------------------

#Plotting Poverty proportion in MA using different projections

#01 - coord_sf()
ggplot(health_insurance_sf_ma) + 
  geom_sf(
    aes(fill = poverty_prop),
    color = "black", size = 0.1
  ) +
  scale_fill_continuous_sequential(
    palette = "OrRd", rev = TRUE
  ) +
  coord_sf() + 
  labs(fill = 'Poverty proportion') +
  theme_minimal_grid(11)
ggsave("proj01_stand.png")

#2 - Texas Centric Albers Equal Area
ggplot(health_insurance_sf_ma) + 
  geom_sf(
    aes(fill = poverty_prop),
    color = "black", size = 0.1
  ) +
  scale_fill_continuous_sequential(
    palette = "OrRd", rev = TRUE
  ) +
  coord_sf(
    # Texas Centric Albers Equal Area
    crs = 3083
  ) + 
  labs(fill = 'Poverty proportion') +
  theme_minimal_grid(11)

#03 - Texas Centric Lambert Conformal Conic
ggplot(health_insurance_sf_ma) + 
  geom_sf(
    aes(fill = poverty_prop),
    color = "black", size = 0.1
  ) +
  scale_fill_continuous_sequential(
    palette = "OrRd", rev = TRUE
  ) +
  coord_sf(
    # Texas Centric Lambert Conformal Conic
    crs = 32139
  ) + 
  labs(fill = 'Poverty proportion') +
  theme_minimal_grid(11)

#04 - Web Mercator (used by Google Maps)
ggplot(health_insurance_sf_ma) + 
  geom_sf(
    aes(fill = poverty_prop),
    color = "black", size = 0.1
  ) +
  scale_fill_continuous_sequential(
    palette = "OrRd", rev = TRUE
  ) +
  coord_sf(
    # Web Mercator (used by Google Maps)
    crs = 3857
  ) + 
  labs(fill = 'Poverty proportion') +
  theme_minimal_grid(11)

#05 - Longitude-Latitude WGS84 (GPS)
ggplot(health_insurance_sf_ma) + 
  geom_sf(
    aes(fill = poverty_prop),
    color = "black", size = 0.1
  ) +
  scale_fill_continuous_sequential(
    palette = "OrRd", rev = TRUE
  ) +
  coord_sf(
    # Longitude-Latitude WGS84 (GPS)
    crs = 4326
  ) + 
  labs(fill = 'Poverty proportion') +
  theme_minimal_grid(11)

#06
ggplot(health_insurance_sf_ma) + 
  geom_sf(
    aes(fill = poverty_prop),
    color = "black", size = 0.1
  ) +
  scale_fill_continuous_sequential(
    palette = "OrRd", rev = TRUE
  ) +
  coord_sf(
    # Alaska Albers equal area
    crs = 3338
  ) + 
  labs(fill = 'Poverty proportion') +
  theme_minimal_grid(11)


# Animated plot for health insurance proportion x poverty proportion in MA and FL
# The dots represent counties in each state
num_years <- max(health_insurance_sf_ma$year) - min(health_insurance_sf$year) + 1
animate(animated_ma, nframes = num_years, fps = 2) 

animated_plot02 <- ggplot(health_insurance, aes(poverty_prop, estimate_zero_to_64_insurance_prop, colour = state)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~state) + 
  transition_time(year) +
  ggtitle('Year: {frame_time}',
             subtitle = 'Frame {frame} of {nframes}')

animated_plot02 

num_years <- max(health_insurance_sf_ma$year) - min(health_insurance_sf_ma$year) + 1
animate(animated_plot02, nframes = num_years, fps = 2) 


# One example of a 3D map - not useful here but just for showing how it works

# Massachusetts 3D map
test_3Dplot_ma <- ggplot( data=health_insurance_sf_fl, aes(fill = estimate_zero_to_64_insurance_prop, label = 'Percent with Health Insurance' )) + 
  geom_sf(color = 'white')
plot_gg(test_3Dplot_ma,  width = 5, height = 5, multicore = FALSE, scale = 250, 
        zoom = 0.7, theta = 10, phi = 50, windowsize = c(800, 800))

# Florida 3D map
test_3Dplot_fl <- ggplot( data=health_insurance_sf_fl, aes(fill = poverty_prop)) + 
  geom_sf(color = 'white')+
  theme(legend.position="bottom") +
  scale_fill_continuous_sequential(
    palette = "OrRd", rev = TRUE) +
  labs(fill = 'poverty proportion')
  
plot_gg(test_3Dplot_fl,  width = 5, height = 5, multicore = FALSE, scale = 250, 
        zoom = 0.7, theta = 10, phi = 50, windowsize = c(800, 800))







