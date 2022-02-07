library(dplyr)
library(ggplot2)
library(jsonlite)
Covid_data <- fromJSON("https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")


Covid_data %>% 
  ggplot(aes(x=fully_vaccinated_pct_of_pop, 
             y=deaths_per_100k, label=name)) +
  geom_point() + 
  scale_x_continuous(labels = scales::percent, 
                     breaks = seq(0.45,0.8, 0.05)) +
  scale_y_continuous(breaks = seq(0,20,5))+
  geom_text(size=2, hjust=-0.3, vjust=0.6) +
  theme_bw() +
  labs(x="Share of total population fully vaccinated", 
       y="20 avg. monthly deaths per 100,000") +
  ggtitle("Covid-19 deaths since universal adult vaccine eligibility compared with 
                                     vaccination rates")
#Jeg gadd ikke her legge inn forkortelsene for alle statene som en egen kolonne.


lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = Covid_data)

#Her ser vi at skj??ringspunktet i y=aksen er 31,15
#Stigningstallet til regresjonslinja er -36,66
#Dette viser oss at vi har en negativ sammenheng.
#Når vaksinasjonsraten synker, så synker dødsfallene


Covid_data %>% 
  ggplot(aes(x=fully_vaccinated_pct_of_pop, 
             y=deaths_per_100k, label=name)) +
  geom_point() + 
  scale_x_continuous(labels = scales::percent, 
                     breaks = seq(0.45,0.8, 0.05)) +
  scale_y_continuous(breaks = seq(0,20,5))+
  geom_text(size=2, hjust=-0.3, vjust=0.6) +
  theme_bw() +
  geom_smooth(method = lm) +
  labs(x="Share of total population fully vaccinated", 
       y="20 avg. monthly deaths per 100,000") +
  ggtitle("Covid-19 deaths since universal adult vaccine eligibility compared with 
                                     vaccination rates")

#Her plotter jeg regresjonslinjen for punktene. Det er den bli linjen.

