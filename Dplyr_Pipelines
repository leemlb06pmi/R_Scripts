#Exploring the R tidyverse to create basic data pipelines

#The dataset; not loadable in this script
#scraped <- read_csv("ithaca_rentals.csv") 

#fundamental case filtering
cq1_answer<-filter(scraped, 
                   scraped_beds == '1BR' 
                   | scraped_beds == '2BR' 
                   | scraped_beds == '3BR')

#creating a categorical case-determined variable in the dataframe
cq2_answer$listed_in_march<-ifelse(str_detect(cq2_answer$listing_date,'2020-03'),1,0)

#combining verbs
grouped <- listings %>% group_by(source,bedrooms) 

Allsum <- grouped %>%
  summarise(n=n())%>%
  mutate(freq = n/sum(n))%>%
  select(-n)

#creating multiple groupings allows the summarize function to create nested valuations automatically
LAsum <- listings %>%
  filter(fips=="06037" & between(listings$beds,0,3))%>%
  group_by(source,beds)%>%
  summarise(med_rent=median(rent,na.rm=TRUE))

#creating a visualization with ggplot 
#plot will be organized by the groupings previously created
ggplot(LAsum, aes(x=beds,y=med_rent, fill=source)) +
  geom_bar(stat='identity',position = "dodge")
  
