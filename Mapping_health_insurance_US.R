
### Generating US Map for Health Insurance Coverage by state - 2012
# For this code, we customized the code presented for Case Study #4 of
# Public Health Disparities Geocoding Project 2.0 Training Manual
# available at: https://phdgp.github.io/PHDGP2.0/index.html

# Dependencies -----------------------------------------------------------

#Loading the following packages:
library(tidycensus)
library(tidyverse)
library(tidyr)
library(magrittr) # magrittr defines the %<>% operator, read more here: https://magrittr.tidyverse.org/
library(patchwork)

# Dataset -----------------------------------------------------------------

# Building a data dictionary - based on ACS 2013
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
get_county_health_insurance <- function(year) {
  get_acs( year = year, geography = 'state', variables = data_dictionary$variable)
}

# create a table shell for each year
health_insurance <- expand.grid(year = 2019)

# query the data from ACS
health_insurance %<>% 
  rowwise() %>% 
  mutate(
    acs_data = list(get_county_health_insurance(year = 2019))) 

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
    id_cols = c('year', 'GEOID', 'NAME'),
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


# remove any unncessary variables going forward
health_insurance %<>% select(
  year, GEOID, NAME,
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


# Plotting the US Map ---------------------------------------------------------

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
#library(rayshader)
# install.packages("remotes")
library(remotes)
# remotes::install_github("wilkelab/cowplot", force=TRUE)
library(ggplot2)
library(cowplot)

states_sf <- 
  tigris::states(cb = TRUE, resolution = "20m") %>%
  shift_geometry() # here we're downloading 1:20m resolution data 

# make sure to do an inner join since we have more than 1 row per county
# for more information on different types of joins: https://statisticsglobe.com/r-dplyr-join-inner-left-right-full-semi-anti
health_insurance_sf <- inner_join(
  states_sf %>% select(GEOID), # when joining spatial data in, put the spatial data argument first so
  # the output inherits the spatial data class 
  health_insurance,
  by = c('GEOID' = 'GEOID'))

#plotting the map as plot_us
plot_us<- ggplot(health_insurance_sf) + 
  geom_sf( aes(fill = estimate_zero_to_64_insurance_prop), color = "black", size=0.2) + 
  scale_fill_distiller(palette = 'Blues', direction = -1, labels = scales::percent_format()) +
  labs(fill = '') +
  theme_minimal_grid(11)

#looking the map created
plot_us

# saving the map as plot_us.png
ggsave("plot_us.png")
