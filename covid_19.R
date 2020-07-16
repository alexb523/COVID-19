# refs:
# https://datasharkie.com/covid-19-daily-statistics-updates/?fbclid=IwAR0EwXLIH_AiwEmU7KxOMyfgE83nmdLH34Dj5QCtOdFA78pTHRsdf2KRguQ
# https://github.com/CSSEGISandData/COVID-19


# library bank ----

library(tidyverse)
library(data.table)
library(scales)
library(directlabels)

# john hopskins data ----

# list all the files in the path
covid_files <- list.files(path = "csse_covid_19_data/csse_covid_19_daily_reports", pattern = ".csv", full.names = T)
gsub("_|.csv", "", basename(covid_files))

covid_data <- map(covid_files,
                  function(x) {
                    fread(x, na = "") %>%
                      mutate(Date = as.Date(gsub(".csv", "", basename(x)), format = "%m-%d-%Y")) %>% 
                      possibly(rename, otherwise = .)(Latitude = Lat, Longitude = Long_) %>% 
                      rename_all(.funs = ~gsub("/| ", "_", trimws(.)))
                  }) %>% bind_rows() %>%
  mutate(Country_Region = gsub("Mainland China", "China", Country_Region))

covid_data %>% glimpse()

# world analysis ----

# group by all countries date
covid_world <- covid_data %>%
  group_by(Date) %>%
  summarize(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered)) %>% 
  gather(key = "Outcome", value = "Total", -Date) %>% 
  filter(!is.na(Total))

# line graph
covid_world %>% 
  ggplot(aes(x = Date, y = Total, color = Outcome)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  scale_colour_discrete(guide = 'none') +
  scale_x_date(breaks = pretty_breaks(10), expand = c(0, 10)) +
  geom_dl(aes(label = Outcome), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  coord_cartesian(clip = 'off') +
  theme_bw() +
  ggtitle("Covid19 Cases in The World")

# stacked area graph
covid_world %>% 
  ggplot(aes(x = Date, y = Total, fill = Outcome)) +
  geom_area(position="fill", stat="identity", alpha=0.6) +
  scale_x_date(breaks = pretty_breaks(10)) +
  theme_classic() +
  ylab("Proportion") +
  ggtitle("Stacked Proportion of World Cases and Outcomes")

# death rate line
covid_world %>% 
  spread(key = "Outcome", value = "Total") %>% 
  filter(!is.na(Deaths)) %>% 
  mutate(Death_Rate = Deaths/Confirmed,
         Recovered_Rate = Recovered/Confirmed) %>% 
  select(Date, Death_Rate, Recovered_Rate) %>% 
  gather(key = "Outcome", value = "Total", -Date) %>% 
  ggplot(aes(x = Date, y = Total, color = Outcome)) +
  geom_line() +
  theme_bw() +
  ylab("Proportion") +
  ggtitle("Death & Recover Rate Overtime")

# death rate stacked
covid_world %>% 
  spread(key = "Outcome", value = "Total") %>% 
  filter(!is.na(Deaths)) %>% 
  mutate(Death_Rate = Deaths/Confirmed,
         Recovered_Rate = Recovered/Confirmed) %>% 
  select(Date, Death_Rate, Recovered_Rate) %>% 
  gather(key = "Outcome", value = "Total", -Date) %>%
  ggplot(aes(x = Date, y = Total, fill = Outcome)) +
  geom_area(position="fill", stat="identity", alpha=0.6) +
  scale_x_date(breaks = pretty_breaks(10)) +
  theme_classic() +
  ylab("Proportion") +
  ggtitle("Stacked Death & Recover Rate Overtime")

# country analysis ----

# group by country and dates
cntry_tots <- covid_data %>%
  group_by(Country_Region, Date) %>%
  summarize(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Recovered = sum(Recovered)) %>% 
  ungroup()

# top 10% of contries
top_cntry <- cntry_tots %>% 
  filter(Date == as.Date(max(gsub("_|.csv", "", basename(covid_files))), format = "%m-%d-%Y")) %>% 
  group_by(Country_Region) %>% 
  summarize(Confirmed = sum(Confirmed, na.rm = T),
            Deaths = sum(Deaths, na.rm = T),
            Recovered = sum(Recovered, na.rm = T)) %>% 
  # gather(key = "Outcome", value = "Total", -Country_Region) %>% 
  filter(!is.na(Confirmed)) %>% 
  top_frac(n = 0.05, wt = Confirmed) %>% 
  ungroup()

# top confirmed cases
top_cntry %>% 
  ggplot(aes(x = reorder(Country_Region, Confirmed), y = Confirmed)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  ggtitle("Countries with Top 10% of Confirmed Cases")

# top confirmed deaths
top_cntry %>% 
  ggplot(aes(x = reorder(Country_Region, Confirmed), y = Deaths)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  ggtitle("Deaths in Countries with Top 10% of Confirmed Cases")

# propotion outcome
top_cntry %>% 
  gather(key = "Outcome", value = "Total", -Country_Region) %>% 
  filter(!is.na(Total)) %>% 
  ggplot(aes(x = Country_Region, y = Total, fill = Outcome)) +
  geom_bar(position="fill", stat="identity", alpha=0.6) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  ggtitle("Covid Cases and outcomes of Top 10% of Countries w/ Confirmed Cases") +
  ylab("Proportion")

top_cntry %>%
  mutate(Prp_Death = round(Deaths/Confirmed, 2), Prp_Recovered = round(Recovered/Confirmed, 2)) %>%
  arrange(desc(Prp_Death))

# top 5 overtime
cntry_tots %>%
  filter(Country_Region %in% (cntry_tots %>% 
                                group_by(Country_Region) %>% 
                                summarize(Confirmed = sum(Confirmed, na.rm = T)) %>% 
                                top_n(n = 5, wt = Confirmed) %>% 
                                unlist())) %>% 
  gather(key = "Outcome", value = "Total", -c(Date, Country_Region)) %>% 
  filter(!is.na(Total) & Outcome == "Confirmed") %>% 
  ggplot(aes(x = Date, y = Total, color = Country_Region)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  scale_colour_discrete(guide = 'none') +
  scale_x_date(breaks = pretty_breaks(10), expand = c(0, 5)) +
  geom_dl(aes(label = Country_Region), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  coord_cartesian(clip = 'off') +
  theme_bw() +
  ggtitle("Top 5 Countries COVID-19 Cases Over Time")

# prop of pop
pop <- population %>% filter(year == 2013) %>% 
  mutate(country = trimws(gsub("\\(Islamic Republic of\\)|Virgin Islands|of Great Britain and Northern Ireland", "", country)),
         country = gsub("United States of America", "US", country)) %>% 
  group_by(country) %>% 
  summarize(population = sum(population)) %>% 
  rename(Country_Region = country, Population = population) %>% 
  ungroup()



####################################################
# country analysis ----

# group by country and dates
cntry_tots <- covid_data %>%
  group_by(Country_Region, Date) %>%
  summarize(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Recovered = sum(Recovered))

# top 10% of contries
top_cntry <- cntry_tots %>% 
  group_by(Country_Region) %>% 
  summarize(Confirmed = sum(Confirmed, na.rm = T),
            Deaths = sum(Deaths, na.rm = T),
            Recovered = sum(Recovered, na.rm = T)) %>% 
  # gather(key = "Outcome", value = "Total", -Country_Region) %>% 
  filter(!is.na(Confirmed)) %>% 
  top_frac(n = 0.1, wt = Confirmed)

# top confirmed cases
top_cntry %>% 
  ggplot(aes(x = reorder(Country_Region, Confirmed), y = Confirmed)) +
    geom_bar(position = "dodge", stat = "identity") +
    scale_y_continuous(labels = comma) +
    coord_flip() +
    theme_bw() +
    theme(axis.title.y = element_blank()) +
    ggtitle("Countries with Top 10% of Confirmed Cases")

# top confirmed cases deaths
top_cntry %>% 
  ggplot(aes(x = reorder(Country_Region, Confirmed), y = Deaths)) +
    geom_bar(position = "dodge", stat = "identity") +
    scale_y_continuous(labels = comma) +
    coord_flip() +
    theme_bw() +
    theme(axis.title.y = element_blank()) +
    ggtitle("Deaths in Countries with Top 10% of Confirmed Cases")

# top confirmed cases
top_cntry %>% 
  ggplot(aes(x = reorder(Country_Region, Confirmed), y = Recovered)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  ggtitle("Recoveries in Countries with Top 10% of Confirmed Cases")

top_cntry %>% 
  gather(key = "Outcome", value = "Total", -Country_Region) %>% 
  filter(!is.na(Total)) %>% 
  ggplot(aes(x = Country_Region, y = Total, fill = Outcome)) +
  geom_bar(position="fill", stat="identity", alpha=0.6) +
  coord_flip()

cntry_tots %>%
  filter(Country_Region %in% (cntry_tots %>% 
                                group_by(Country_Region) %>% 
                                summarize(Confirmed = sum(Confirmed, na.rm = T)) %>% 
                                top_n(n = 5, wt = Confirmed) %>% 
                                unlist())) %>% 
  gather(key = "Outcome", value = "Total", -c(Date, Country_Region)) %>% 
  filter(!is.na(Total) & Outcome == "Confirmed") %>% 
  ggplot(aes(x = Date, y = Total, color = Country_Region)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  scale_colour_discrete(guide = 'none') +
  scale_x_date(breaks = pretty_breaks(10), expand = c(0, 5)) +
  geom_dl(aes(label = Country_Region), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  coord_cartesian(clip = 'off') +
  theme_bw() +
  ggtitle("Top 10 Countries over time")

    