#Mappeoppgave 4

#Jeg jobbet samen med Julia Ottesen

rm(list=ls())

library(rvest)
library(tidyverse)
library(rlist)
library(purrr)


#Jeg har andre fag en resten av klassen så valgte de istenden.
url1 <-"https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list"
url2 <-"https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=BED-2032-1&View=list"
url3 <-"https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-2030-1&View=list"

Url_liste <- list(url1,url2,url3)

#Bruker "." for å velge forrige r commando.

Fag_funksjon <- function(Url_liste_intern) {
  
  dframe <- 
  read_html(Url_liste_intern) %>% 
  html_nodes(., 'table') %>% 
  html_table(., fill=TRUE) %>% 
  list.stack(.) 
  colnames(dframe) <- dframe[1,]
  Timeplanen <- dframe %>% filter(!Dato=="Dato") %>% 
     select(Dato, Tid, Rom, Lærer, Emnekode)

  return(Timeplanen) }

  #Her kjører jeg koden med Url_liste som input til funksjonen "Fag_funksjon
  Timeplan <- map(Url_liste,Fag_funksjon) %>% bind_rows()
  Timeplan
  
  