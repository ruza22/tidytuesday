#load datasets and required functions
library(tidyverse)
library(usmap)
childcare_costs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/childcare_costs.csv')
counties <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/counties.csv')

n_weeks = 365/7 # number of weeks per one calendar year

chc <- childcare_costs %>% 
  select(county_fips_code, mhi_2018, mc_preschool) %>% 
  filter(!is.na(childcare_costs$mc_preschool)) %>% # removing NA values (cca 1/3 of original dataset)
  mutate(share = (mc_preschool*n_weeks) / mhi_2018) %>% # mhi is yearly income while mc is weekly expense
  inner_join(y = counties, by = "county_fips_code")

chc_plot <- select(chc, county_fips_code, share) %>% 
  group_by(county_fips_code) %>% 
  summarise(mean_share = mean(share)) %>% # getting the mean from all years (2008-2018)
  rename(fips = county_fips_code)

plot_usmap(data = chc_plot, values = "mean_share", color = "grey") +
  scale_fill_continuous(low = "white",
                        high = "orange",
                        name = "Share",
                        label = scales::label_percent()) +
  theme(legend.position = "right",
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8),
        plot.caption = element_text(size=6)) +
  geom_polygon(data = usmapdata::us_map(regions = "states"),
               aes(x, y, group = group), fill = NA, size = 0.3, color = "black") +
  labs(title = "Prices charged for Center-based Care for preschoolers as a share of median household income.",
       subtitle = "Childcare is an average of yearly full-time medians. Income expressed in 2018 dollars. Data collected between 2008-2018",
       caption = "Source: National Database of Childcare Prices (https://www.dol.gov/agencies/wb/topics/featured-childcare), Graphic: Lukas Ruzicka")

ggsave(filename = "childcare_income_share.png", bg = "white")
