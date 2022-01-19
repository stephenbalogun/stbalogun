## load required libraries

library(tidyverse)
library(plotly)
library(lubridate)
library(ggExtra)
# library(wpp2019)



###################### Global Confirmed Cases ##


# file_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"

file_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

Africa <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Congo (Brazzaville)", "Congo (Kinshasa)", "Cote d'Ivoire", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Liberia", "Libya", "Madagascar", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Papua New Guinea", "Rwanda", "Saint Vincent and the Grenadines", "Senegal", "Seychelles", "Somalia", "South Africa", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")

Asia <- c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "Brunei", "Cambodia", "China", "India", "Indonesia", "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakstan", "Korea, South", "Kuwait", "Kyrgyzstan", "Lebanon", "Malaysia", "Maldives", "Mongolia", "Nepal", "Oman", "Pakistan", "Peru", "Qatar", "Saudi Arabia", "Singapore", "Sri Lanka", "Taiwan*", "Thailand", "United Arab Emirates", "Uzbekistan", "Vietnam", "Syria", "Timor-Leste", "Laos", "West Bank and Gaza", "Burma")

`South America` <- c("Argentina", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Suriname", "Trinidad and Tobago", "Venezuela")

Australia <- c("Australia", "Fiji", "New Zealand")

Europe <- c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bolivia", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Greece", "Holy See", "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Philippines", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom", "Uruguay", "Kosovo")

`North America` <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Canada", "Costa Rica", "Cuba", "Dominican Republic", "El Salvador", "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Saint Lucia", "US", "Dominica", "Grenada", "Belize", "Saint Kitts and Nevis")

Others <- c("Diamond Princess", "MS Zaandam")


#
# download.file(url = file_cases,
#               destfile = "./covid_cases.csv")

covid_cases <- read_csv("./covid_cases.csv")

names(covid_cases) <- stringr::str_to_lower(names(covid_cases))

covid_cases2 <- covid_cases %>%

  mutate(continent = case_when(`country/region` %in% Africa ~ "Africa",
                               `country/region` %in% Asia ~ "Asia",
                               `country/region` %in% Europe ~ "Europe",
                               `country/region` %in% `North America` ~ "North America",
                               `country/region` %in% `South America` ~ "South America",
                               `country/region` %in% Australia ~ "Australia",
                               `country/region` %in% Others ~ "Others")
  ) %>%

  rename(country = `country/region`)



covid_cases2 <- covid_cases2 %>%

  select(`province/state`,
         country,
         continent,
         # population,
         lat,
         long,
         everything()) %>%

  pivot_longer(- c(`province/state`,
                   `country`,
                   `continent`,
                   # `population`,
                   `lat`,
                   `long`),
               names_to = "date",
               values_to = "cases")


covid_cases2$date <- lubridate::mdy(covid_cases2$date)




####################### mortality data ####

file_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
#
# download.file(url = file_deaths,
#               destfile = "./covid_deaths.csv")

covid_deaths <- read_csv("./covid_deaths.csv")

names(covid_deaths) <- stringr::str_to_lower(names(covid_deaths))

covid_deaths2 <- covid_deaths %>%

  pivot_longer(- c(`province/state`,
                   `country/region`,
                   `lat`,
                   `long`),
               names_to = "date",
               values_to = "deaths") %>%

  rename(country = `country/region`)

covid_deaths2$date <- lubridate::mdy(covid_deaths2$date)

##################################################################




### merged data ###


covid_df <- left_join(covid_cases2,
                      covid_deaths2,
                      by = c("province/state",
                             "country",
                             "lat",
                             "long",
                             "date"))

covid_df$country <- as_factor(covid_df$country)

## number of countries affected at date of analysis

n_count <- covid_df %>%
  filter(date == ymd("2020-02-28")) %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases)) %>%
  filter(cases != 0)




## no of cases as at date of analysis

n_count2 <- covid_df %>%
  filter(date == max(covid_df$date)) %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases)) %>%
  filter(cases != 0)


current_cases <- covid_df %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  arrange(desc(cases))


global_trend <- covid_df %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(size = 1, colour = "darkred") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(from = 0, to = 400000000, by = 50000000), labels = scales::comma, minor_breaks = NULL) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL) +
  labs(x = "Date",
       y = "No of cases",
       title = "Global reported number of confirmed COVID-19 cases") +
  rotateTextX(angle = 45)



## no of countries affected

n_countries <- covid_df %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases)) %>%
  filter(cases != 0) %>%
  group_by(date) %>%
  count() %>%
  ggplot(aes(x = date, y = n)) +
  geom_line(colour = "powderblue", size = 1.5) +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL) +
  labs(y = "No of countries affected",
       title = "No of countries affected by COVID-19") +
  rotateTextX(angle = 45)




world <- covid_df %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(x = date, y = cases, color = country)) +
  geom_line(size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from = 0, to = 70000000, by = 10000000), labels = scales::comma, minor_breaks = NULL) +
  rotateTextX(angle = 45) +
  labs(x = "Date",
       y = "No of Cases",
       title = "COVID19 trend around the World") #+
# geom_text(data = covid19, aes(label = `country/region`), check_overlap = T)




## Top 10 countries in the world excluding China and Italy

world2 <- covid_df %>%  # select the data
  group_by(country, date) %>%  # one entry per country per day
  summarise(cases = sum(cases)) %>%  # add all the cases for the country
  pivot_wider(names_from = date, values_from = cases) %>%
  arrange(desc(`2020-03-30`))

world2 <- world2[c(1:7), ]  %>% ## filtering 10 worst hit countries apart                                   from China and Italy

  pivot_longer(-country, names_to = "date", values_to = "cases")  #converting back to long                                                format


world2$date <- lubridate::ymd(world2$date)

world2_plot <- world2 %>%
  ggplot(aes(x = date, y = cases, color = fct_reorder2(country, date, cases))) +
  geom_line(size = 1) +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from = 0, to = 70000000, by = 10000000), labels = scales::comma, minor_breaks = NULL) +
  labs(x = "Date",
       y = "No of Cases",
       color = "country",
       title = "COVID19 trend for top 7 hit countries") +
  rotateTextX(angle = 45)





## africa data analysis


africa <- covid_df %>%
  dplyr::filter(continent == "Africa")

africa_cases <- africa %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  arrange(desc(cases))


africa_trend <- africa %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(size = 1, colour = "darkred") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(from = 0, to = 10000000, by = 2000000), labels = scales::comma, minor_breaks = NULL) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL) +
  labs(x = "Date",
       y = "No of Cases",
       title = "COVID-19 trend in Africa") +
  rotateTextX(angle = 45)



africa2_plot <- africa %>%  # select the data
  group_by(country, date) %>%  # one entry per country per day
  summarise(cases = sum(cases)) %>%  # add all the cases for the country
  pivot_wider(names_from = date, values_from = cases) %>%
  arrange(desc(`2022-01-10`))

africa2_plot <- africa2_plot[c(1:7), ]  %>%
  pivot_longer(-country, names_to = "date", values_to = "cases")  #converting back to long                                                format


africa2_plot$date <- lubridate::ymd(africa2_plot$date)



africa2_plot <- africa2_plot %>%
  ggplot(aes(x = date, y = cases, colour = fct_reorder2(country, date, cases))) +
  geom_line(size = 1) +
  theme_light() +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %B", minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from = 0, to = 4000000, by = 1000000), labels = scales::comma, minor_breaks = NULL) +
  labs(x = "Date",
       y = "No of Cases",
       colour = "country",
       title = "COVID19 trend in top 7 hit countries in Africa") +
  rotateTextX(angle = 45)



ngr <- covid_df %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases)) %>%
  filter(country == "Nigeria") %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(size = 1.5, color = "lightblue") +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from = 0, to = 300000, by = 50000), labels = scales:: comma, minor_breaks = NULL) +
  labs(x = "Date",
       y = "No of Cases",
       title = "COVID-19 trend in Nigeria as at today") +
  rotateTextX(angle = 45)





## animated plot ##

# df <- covid_df %>%
#   mutate(month = month(covid_df$date),
#          day = day(covid_df$date)) %>%
#   filter(month == 3) %>%
#   dplyr::rename(March = day) %>%
#   group_by(March, country, continent) %>%
#   summarise(deaths = sum(deaths),
#             cases = sum(cases))

df <- covid_df %>%
  mutate(year = year(covid_df$date),
         month = month(covid_df$date, label = TRUE)) %>%
  filter(year == 2021,
         !is.na(continent)) %>%
  rename(`2021` = month) %>%
  group_by(`2021`, country, continent) %>%
  summarise(deaths = sum(deaths),
            cases = sum(cases))


animate <- df %>%
  ggplot(aes(x = cases,
             y = deaths,
             color = continent)) +

  geom_point(aes(frame = `2021`,
                 ids = country,
                 na.rm = FALSE),
             size = 5,
             alpha = .4,
             show.legend = FALSE) +

  labs(x = "No of confirmed cases",
       y = "No of confirmed deaths",
       title = "Trend Analysis of the COVID-19 Pandemic") +

  theme_minimal() +

  scale_x_continuous(breaks = seq(from = 0,
                                  to = 2000000000,
                                  by = 500000000),
                     labels = scales::comma,
                     minor_breaks = NULL) +

  scale_y_continuous(breaks = seq(from = 0,
                                  to = 30000000,
                                  by = 5000000),
                     labels = scales::comma,
                     minor_breaks = NULL)
