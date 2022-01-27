library(readr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(zoo)
library(dplyr)


#Oppgave 1

Data <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")

#Her gj??r jeg alle kolonnene til tall. Alle steder det ikke er tall vil bli til NA. 
#Dette gj??r jeg for ?? isolere radene p?? bunn, som jeg ikke ??nsket, men ?? ha koden
#??pen for at flere m??neder og ??r kan legges til. 
Data <-Data %>% mutate_if(is.character,as.numeric)

#Bruker funksjonen fra tidyr for ?? fjerne alle radene med NA i seg.
#Dette gj??r at vi kun sitter igjen med de radene med dataen vi er interessert i.
Data <- Data %>% drop_na


#Lager en ny Dato kolonne for ?? ha til x-aksen
Data$Date <- zoo::as.yearmon(paste(Data$Year, Data$Mo), "%Y %m")


#Her lager jeg plottet som ligner s?? mye som mulig p?? plottet fra bildet.
#Eneste begrensningen jeg har er at jeg m?? definere start og sluttpunktet.
#Dette m??tte jeg for ?? kunne velge at jeg vil ha alle ??rene p?? x-aksen
#Her plotter jeg begge linjene og punktene og lager navn p?? aksene.
Data %>% ggplot(aes(x=Date, y=Globe)) + 
  geom_line(color="blue") + 
  geom_point(aes(x=Date,y=Globe), shape=1, color="sky blue", size=1, stroke=1, fill="white") + 
  geom_line(y=0) + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(1950,2040, 1)) +
  scale_y_continuous(breaks = seq(-0.8,1 , 0.1)) +
  labs(x="Latest Global Average Tropospheric Temperatures", y="T Departure from 2091-2020 Avg. (deg.C)") +
  ggtitle("Latest Global Temps") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_line(aes(y=rollmean(Globe, 12, na.pad=TRUE)), size=1, color="red")


#Oppgave 2

#For oppgave 2 bruker jeg bare samme koden jeg brukte i oppgave 1 og kaller de
#nye datasettene etter hva de er data for.

#Jeg lager Dato kolonnen og velger kun at jeg er interessert i dato og NoPol kolonnen.

#Kunne sikkert gjort det mer effektivt ved ?? lage en vektor med alle og gjort det samtidig med alle.
DataLT <- Data %>% select(Date, NoPol)

DataMT <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt") %>% 
  mutate_if(is.character,as.numeric) %>% 
  drop_na %>%  
  select(NoPol)

DataT <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt") %>% 
  mutate_if(is.character,as.numeric) %>% 
  drop_na %>%  
  select(NoPol)

DataLS <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt") %>% 
  mutate_if(is.character,as.numeric) %>% 
  select(NoPol, Mo, Year) %>% drop_na

#Her hadde jeg problemer, jeg fikk kun 516 verdier, og klarte ikke finne feilen. Mulig det er en verdi
#i de andre kolonnene som blir til NA. Det er ikke viktig for oppgaven n??r jeg fikk fikset det ved ?? legge til ??r
#og fjerne NA etter jeg hadde valgt ut kolonnene mine.



DataAll <- data.frame(Date=DataLT$Date, NoPolLT=DataLT$NoPol, 
                      NoPolMT=DataMT$NoPol, NoPolT=DataT$NoPol, NoPolLS=DataLS$NoPol)


#Lager her gjennomsnittet som en egen kolonne
DataAll <- DataAll %>% 
  rowwise() %>% 
    mutate(Mean=mean(c(NoPolMT,NoPolT,NoPolLS,NoPolLT)))

                     

DataAll %>% ggplot(aes(x=Date)) +
  geom_line(aes(y=NoPolLT, color="Lower Troposphere:")) + 
  geom_line(aes(y=NoPolMT, color="Mid-Troposphere:") )+ 
  geom_line(aes(y=NoPolT, color="Tropopause:") )+
  geom_line(aes(y=NoPolLS, color="Lower Stratosphere:")) + 
  geom_line(aes(y=Mean, color="Mean") )+
  theme_bw() + 
  labs(y="North Pole Temperatures", x="Time in years") +
  ggtitle("Latest Average Temperatures in the North Pole in deg. C") +
  scale_color_manual(name="Where in atmosphere",
                     values = c("Lower Troposphere:" = "darkblue", 
                                "Mid-Troposphere:" = "blue",
                                "Tropopause:" = "green", 
                                "Lower Stratosphere:"="sky blue",
                                "Mean"="red"))
                     
                     
                   
            

