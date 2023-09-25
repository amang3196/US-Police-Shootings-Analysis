#PDS Assignment.

#IMPORTING PACKAGES
library(magrittr)
library(ggplot2)
library(forcats)
library(treemap)
library(highcharter)
library(tidyverse)
library(lubridate)
library(plotly)
library(dplyr)
library(usmap)
library(viridis)
library(scales)
library(viridisLite)
library(ggthemes)
library(formatR)


#Reading the csv file
data1<-read.csv("shootings.csv")
data1
View(data1)

cols(
  id = col_double(),
  name = col_character(),
  date = col_date(format = ""),
  manner_of_death = col_character(),
  armed = col_character(),
  age = col_double(),
  gender = col_character(),
  race = col_character(),
  city = col_character(),
  state = col_character(),
  signs_of_mental_illness = col_logical(),
  threat_level = col_character(),
  flee = col_character(),
  body_camera = col_logical(),
  arms_category = col_character()
)
#MUTATING DATA
data1 <- data1 %>%
  mutate(year = year(date)) %>%
  mutate(manner_of_death=as.factor(manner_of_death), age=as.integer(age), 
         gender=as.factor(gender), race=as.factor(race),state=as.factor(state), 
         threat_level=as.factor(threat_level), flee=as.factor(flee), 
         arms_category=as.factor(arms_category))

data1[data1$state=="DC","state"] <- "WA"
summary(data1) 
class(data1$year)

#######################
# 1. YEARLY REPORTED DEATHS
#######################
data1 %>%
  group_by(year) %>%
  tally() %>%
  ggplot(aes(year, n)) + 
  geom_line(color="darkblue") + 
  geom_point(size=3) +
  ylab("Number of deaths reported") +
  expand_limits(y=0) +
  ggtitle("Yearly reported deaths")

#####################
# 2. Manner OF death graphs#
#####################
options(repr.plot.width=6, repr.plot.height=6)
data1 %>%
  group_by(manner_of_death) %>% 
  ggplot(aes(manner_of_death, fill=gender)) + 
  geom_bar() +
  ggtitle("Manner of Death by Gender")

#####################
# 3. RACE & GENDER GRAPH(BAR CHART)
#####################
options(repr.plot.width=6, repr.plot.height=6)
data1 %>%
  group_by(race) %>%
  ggplot(aes(fct_infreq(race), fill=gender)) +
  geom_bar() +
  xlab("race")

####################
# 4. US HEATMAP
####################
k <- data.frame(data1 %>% 
                  group_by(state) %>%
                  arrange(state) %>% 
                  tally())
states <- data.frame(state_abb = state.abb, state_names=state.name)
new_data <- cbind(k, states)
l <- new_data %>% select(state_names, n)
l$state_names <- tolower(l$state_names)
centroid <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y)
l <- cbind(l, centroid) %>% select(-region)
options(repr.plot.width=15, repr.plot.height=8)
map <- map_data("state")
l %>% ggplot(aes(fill=n)) +
  geom_map(aes(map_id=state_names), map=map) +
  expand_limits(x = map$long, y = map$lat) +
  scale_fill_gradient(low="yellow", high="red", name="Deaths Reported") +
  geom_text(x=l$long, y=l$lat, label=l$state_names)

#####################################
# 5. ARMS CATEGORY USED (BAR CHART)
###################################
options(repr.plot.width=8, repr.plot.height=8)
data1 %>%
  ggplot(aes(fct_infreq(arms_category))) +
  geom_bar(fill="purple") +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  xlab("arms category")

#################################
# 6. Threat_Level GGPLOT
################################
data1 %>%
  ggplot(aes(threat_level, flee)) +
  geom_jitter(color="maroon")

###############################
# 7. MANNER OF DEATH PIE CHART
###############################
count_MOD = data.frame(table(data1$manner_of_death))
pct<-round(count_MOD$Freq/sum(count_MOD$Freq)*100)
lbs<-paste(count_MOD$Var1,pct)
lbs<-paste(lbs,"%",sep = "")
pie(count_MOD$Freq,labels = lbs, col = c("cadetblue3","chocolate1"),  
    main = "Manner of death")

#############################
# 8. Sign OF MENTAL ILLNESS PIE CHART
#############################
count_MI = data.frame(table(data1$signs_of_mental_illness))
pct<-round(count_MI$Freq/sum(count_MOD$Freq)*100)
lbs<-paste(count_MI$Var1,pct)
lbs<-paste(lbs,"%",sep = "")
pie(count_MI$Freq,labels = lbs, col = c("cadetblue3","chocolate1"), 
    main = "Signs of mental illness")

###############################
# 9. Shooting BY MONTH AND YEAR  #########EDIT
##############################
library(lubridate)
data1$year <- year(ymd(data1$date))
data1$month <- month(ymd(data1$date))
data1$day <- day(ymd(data1$date))
counts_date <- table(data1$year,data1$month)
barplot(counts_date, main = "Shootings by year and month", 
        xlab = "Month", ylab = "Number of case",
        col = c("cadetblue3","chocolate1","plum1","palegreen3","maroon1","steelblue"),
        beside = TRUE,xlim = c(0,100),legend=TRUE)

##############################
# 10. OUTLIERS
############################
temp <- data1 %>% select(age, gender, race, flee)
temp$age <- as.integer(temp$age)

hcboxplot(x = temp$age,var = temp$flee, var2 = temp$race, outliers = FALSE) %>% 
  hc_chart(type = "column") %>% 
  hc_title(
    text = "Death distribution (Fleeing x Age) ",
    style = list(fontFamily = "Arial")
  )

##############################
# 11. US MAP OF DEATHS BY RACE
##############################
temp <- data1 %>% group_by(state) %>% 
  summarise(n = n(), unique = length(unique(state))) %>% 
  arrange(-n, -unique)
temp$unique <- NULL

aux <- data1 %>% select(race, state) %>% mutate(val = 1)

for(i in 1:nrow(temp))
  {
  temp$Asian[i] <- sum(aux$val[temp$state[i] == aux$state & aux$race %in% 'Asian'])
  temp$Black[i] <- sum(aux$val[temp$state[i] == aux$state & aux$race %in% 'Black'])
  temp$Hispanic[i] <- sum(aux$val[temp$state[i] == aux$state & aux$race %in% 'Hispanic'])
  temp$Native[i] <- sum(aux$val[temp$state[i] == aux$state & aux$race %in% 'Native'])
  temp$Other[i] <- sum(aux$val[temp$state[i] == aux$state & aux$race %in% 'Other'])
  temp$White[i] <- sum(aux$val[temp$state[i] == aux$state & aux$race %in% 'White'])
}

highchart() %>% 
  hc_title(text = "US Police Shootings Deaths by race") %>% 
  hc_add_series_map(usgeojson, temp, name = "Deaths",
                    value = "n", joinBy = c("postalcode", "state"),
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.postalcode}')) %>% 
  hc_colorAxis(stops = color_stops()) %>% 
  hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
  hc_mapNavigation(enabled = FALSE) %>%
  hc_tooltip(useHTML = TRUE,
             headerFormat = "<table>",
             pointFormat = paste("<tr><th colspan=\"1\"><td>Deaths</td></th></tr>",
                                 "<tr><th>Total<td>{point.n}</td></th></tr>",
                                 "<tr><th>Asian</th><td>{point.Asian}</td></tr>",
                                 "<tr><th>Black</th><td>{point.Black}</td></tr>",
                                 "<tr><th>Hispanic</th><td>{point.Hispanic}</td></tr>",
                                 "<tr><th>Native</th><td>{point.Native}</td></tr>",
                                 "<tr><th>Other</th><td>{point.Other}</td></tr>",
                                 "<tr><th>White</th><td>{point.White}</td></tr>"),
             footerFormat = "</table>")

#####################################
# 12. No.OF Guns used in states
#####################################
temp <- data1 %>% group_by(state) %>% 
  summarise(n = n(), unique = length(unique(state))) %>% 
  arrange(-n, -unique)
temp$unique <- NULL

aux <- data1 %>% select(arms_category, state) %>% mutate(val = 1)

for(i in 1:nrow(temp))
{
  temp$Guns[i] <- sum(aux$val[temp$state[i] == aux$state & aux$arms_category %in% 'Guns'])
  temp$Sharp_objects[i] <- sum(aux$val[temp$state[i] == aux$state & aux$arms_category %in% 'Sharp objects'])
  temp$Unknown[i] <- sum(aux$val[temp$state[i] == aux$state & aux$arms_category %in% 'Unknown'])
  temp$Unarmed[i] <- sum(aux$val[temp$state[i] == aux$state & aux$arms_category %in% 'Unarmed'])
  temp$Other_unusual_objects [i] <- sum(aux$val[temp$state[i] == aux$state & aux$arms_category %in% 'Other unusual objects '])
  temp$Blunt_instruments[i] <- sum(aux$val[temp$state[i] == aux$state & aux$arms_category %in% 'Blunt instruments'])
  temp$Vehicles[i] <- sum(aux$val[temp$state[i] == aux$state & aux$arms_category %in% 'Vehicles'])
  temp$Multiple[i] <- sum(aux$val[temp$state[i] == aux$state & aux$arms_category %in% 'Multiple '])
  temp$Piercing_objects[i] <- sum(aux$val[temp$state[i] == aux$state & aux$arms_category %in% 'Piercing objects'])
  temp$Electrical_devices[i] <- sum(aux$val[temp$state[i] == aux$state & aux$arms_category %in% 'Electrical devices'])
  temp$Explosives[i] <- sum(aux$val[temp$state[i] == aux$state & aux$arms_category %in% 'Explosives'])
  temp$Hand_tools [i] <- sum(aux$val[temp$state[i] == aux$state & aux$arms_category %in% 'Hand tools '])
}

highchart() %>% 
  hc_title(text = "US Police Shootings Deaths by Weapons Used") %>% 
  hc_add_series_map(usgeojson, temp, name = "Deaths",
                    value = "n", joinBy = c("postalcode", "state"),
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.postalcode}')) %>% 
  hc_colorAxis(stops = color_stops()) %>% 
  hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
  hc_mapNavigation(enabled = FALSE) %>%
  hc_tooltip(useHTML = TRUE,
             headerFormat = "<table>",
             pointFormat = paste("<tr><th colspan=\"1\"><td>Deaths</td></th></tr>",
                                 "<tr><th>Total<td>{point.n}</td></th></tr>",
                                 "<tr><th>Guns</th><td>{point.Guns}</td></tr>",
                                 "<tr><th>Sharp objects</th><td>{point.Sharp objects}</td></tr>",
                                 "<tr><th>Unknown</th><td>{point.Unknown}</td></tr>",
                                 "<tr><th>Unarmed</th><td>{point.Unarmed}</td></tr>",
                                 "<tr><th>Other unusual objects</th><td>{point.Other unusual objects}</td></tr>",
                                 "<tr><th>Blunt instruments</th><td>{point.Blunt instruments}</td></tr>",
                                 "<tr><th>Vehicles</th><td>{point.Vehicles}</td></tr>",
                                 "<tr><th>Multiple</th><td>{point.Multiple}</td></tr>",
                                 "<tr><th>Piercing objects</th><td>{point.Piercing objects}</td></tr>",
                                 "<tr><th>Electrical devices</th><td>{point.Electrical devices}</td></tr>",
                                 "<tr><th>Explosives</th><td>{point.Explosives}</td></tr>",
                                 "<tr><th>Hand tools</th><td>{point.Hand tools}</td></tr>"),
             footerFormat = "</table>")
####################################
# 13. No of victims per year
####################################
data1$year <- as.numeric(substr(data1$date, start = 1, stop = 4))
temp <- data1 %>%
  group_by(year) %>% 
  summarise(n = n(),unique = length(unique(year))) %>% 
  arrange(-n, -unique)

temp %>% hchart(type = "column", colorByPoint = TRUE, hcaes(x = year, y = n)) %>%
  hc_chart(style = list(fontFamily = "Arial")) %>%
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_yAxis(title = list(text = "Deaths")) %>%
  hc_title(text = "US Police Shootings Deaths by year (2015 to 2020)",
           style = list(fontFamily = "Arial"))
#######################################
# 14. AGE of Death
#######################################
data1$age <- as.integer(data1$age)
temp <- data1 %>% select(age) %>% table() %>% data.frame()
temp <- rename(temp, age = .)

temp %>% hchart(type = "column", hcaes(x = age, y = Freq), name = "Deaths") %>%
  hc_chart(style = list(fontFamily = "Arial")) %>%
  hc_xAxis(title = list(text = "Age")) %>% 
  hc_yAxis(title = list(text = "Deaths")) %>%
  hc_title(text = "US Police Shootings Deaths by age", 
           style = list(fontFamily = "Arial"))

########################################
# 15. Death distribution by state (Guns/Unarmed)
########################################
aux <- data1 %>% select(state, arms_category) %>% mutate(val = 1)

state_aux <- aux$state %>% as.data.frame() %>% unique()
state_aux <- rename(state_aux, state = .)

for(i in 1:nrow(state_aux)){
  state_aux$guns[i] <- sum(aux$val[state_aux$state[i] == aux$state & aux$arms_category %in% 'Guns'])
  state_aux$unm[i] <- sum(aux$val[state_aux$state[i] == aux$state & aux$arms_category %in% 'Unarmed'])
}
state_aux <- head(state_aux[order(state_aux$unm,decreasing = TRUE),], 10L)
highchart() %>% 
  hc_xAxis(categories = state_aux$state) %>% 
  hc_add_series(name = "Guns", data = state_aux$guns) %>%
  hc_add_series(name = "Unamerd", data = state_aux$unm) %>%
  hc_chart(type = "column") %>%
  hc_chart(style = list(fontFamily = "Arial")) %>%
  hc_title(
    text = "Death distribution by state (Guns/Unamerd)",
    style = list(fontFamily = "Arial")
  )

#########################################################
# 16. Threat Level
########################################################
temp <- data1 %>% select(threat_level) %>% table() %>% data.frame()
temp <- rename(temp,tl = .)
temp$Freq <- temp$Freq/sum(temp$Freq)

temp %>% hchart(type = "pie", hcaes(x = tl, y = Freq), name = "%Deaths") %>%
  hc_chart(style = list(fontFamily = "Arial")) %>%
  hc_title(
    text = "US Police Shootings Deaths by Threat Level",
    style = list(fontFamily = "Arial")
  )
###########################################################
# 17. Body Cam USAGE
###########################################################
temp <- data1 %>% select(body_camera) %>% table() %>% data.frame()
temp <- rename(temp, bc = .)
temp$Freq <- temp$Freq/sum(temp$Freq)

temp %>% hchart(type = "pie", hcaes(x = bc, y = Freq), name = "%Deaths") %>%
  hc_chart(style = list(fontFamily = "Arial")) %>%
  hc_title(
    text = "US Police Shootings Deaths by Body cam Use",
    style = list(fontFamily = "Arial")
  )

#############################################
# 18. Deaths By Year and race
#############################################
data1$year <- as.numeric(substr(data1$date, start = 1, stop = 4))

temp <- data1 %>%
  group_by(year) %>% 
  summarise(n = n(),
            unique = length(unique(year))) %>% 
  arrange(-n, -unique)
aux <- data1 %>% select(race, year) %>% mutate(val = 1)


for(i in 1:nrow(temp))
{
  temp$Asian[i] <- sum(aux$val[temp$year[i] == aux$year & aux$race %in% 'Asian'])
  temp$Black[i] <- sum(aux$val[temp$year[i] == aux$year & aux$race %in% 'Black'])
  temp$Hispanic[i] <- sum(aux$val[temp$year[i] == aux$year & aux$race %in% 'Hispanic'])
  temp$Native[i] <- sum(aux$val[temp$year[i] == aux$year & aux$race %in% 'Native'])
  temp$Other[i] <- sum(aux$val[temp$year[i] == aux$year & aux$race %in% 'Other'])
  temp$White[i] <- sum(aux$val[temp$year[i] == aux$year & aux$race %in% 'White'])
}

highchart() %>% 
  hc_xAxis(categories = temp$year) %>% 
  hc_add_series(name = "Total", data1 = temp$n) %>%
  hc_add_series(name = "Black", data1 = temp$Black) %>%
  hc_add_series(name = "White", data1 = temp$White) %>%
  hc_add_series(name = "Hispanic", data1 = temp$Hispanic) %>% 
  hc_add_series(name = "Native", data1 = temp$Native) %>% 
  hc_add_series(name = "Other", data1 = temp$Other) %>%  
  hc_add_series(name = "Asian", data1 = temp$Asian) %>% 
  hc_chart(type = "column") %>%
  hc_chart(style = list(fontFamily = "Arial")) %>%
  hc_title(
    text = "Death distribution by race (2015 to 2020)",
    style = list(fontFamily = "Arial")
  )

######################################
# 19. RACE WITH GENDER 
#####################################
# Lets get deep into racial deaths
temp <- data1 %>%
  select(race) %>% table() %>% as.data.frame()
temp <- rename(temp, race=.)
temp <- temp[order(temp$race),]

aux <- data1 %>%
  select(race, gender) %>% mutate(val = 1)

for(i in 1:nrow(temp))
  {
  temp$Man[i] <- sum(aux$val[aux$race == temp$race[i] & aux$gender == 'M'])
  temp$Fem[i] <- sum(aux$val[aux$race == temp$race[i] & aux$gender == 'F'])
  }


highchart() %>% 
  hc_xAxis(categories = temp$race) %>% 
  hc_add_series(name = "Total", data1 = temp$Freq) %>%
  hc_add_series(name = "Male", data1 = temp$Man) %>%
  hc_add_series(name = "Female", data1 = temp$Fem) %>% 
  hc_chart(type = "column") %>%
  hc_chart(style = list(fontFamily = "Arial")) %>%
  hc_title(
    text = "Death distribution between race and gender",
    style = list(fontFamily = "Arial")
  )
###############################################
# 20. Fleeing and Race
###############################################
temp <- data1 %>% select(flee) %>% table() %>% data.frame()
temp <- rename(temp, flee = .)
temp <- temp[order(temp$Freq, decreasing = TRUE),]
aux <- data1 %>% select(race, flee) %>% mutate(val = 1)

for(i in 1:nrow(temp))
{
  temp$Asian[i] <- sum(aux$val[temp$flee[i] == aux$flee & aux$race %in% 'Asian'])
  temp$Black[i] <- sum(aux$val[temp$flee[i] == aux$flee & aux$race %in% 'Black'])
  temp$Hispanic[i] <- sum(aux$val[temp$flee[i] == aux$flee & aux$race %in% 'Hispanic'])
  temp$Native[i] <- sum(aux$val[temp$flee[i] == aux$flee & aux$race %in% 'Native'])
  temp$Other[i] <- sum(aux$val[temp$flee[i] == aux$flee & aux$race %in% 'Other'])
  temp$White[i] <- sum(aux$val[temp$flee[i] == aux$flee & aux$race %in% 'White'])
}

highchart() %>% 
  hc_xAxis(categories = temp$flee) %>% 
  hc_add_series(name = "Total", data1 = temp$Freq) %>%
  hc_add_series(name = "Black", data1 = temp$Black) %>%
  hc_add_series(name = "White", data1 = temp$White) %>%
  hc_add_series(name = "Hispanic", data1 = temp$Hispanic) %>% 
  hc_add_series(name = "Native", data1 = temp$Native) %>% 
  hc_add_series(name = "Other", data1 = temp$Other) %>%
  hc_add_series(name = "Asian", data1 = temp$Asian) %>% 
  hc_chart(type = "column") %>%
  hc_chart(style = list(fontFamily = "Arial")) %>%
  hc_title(text = "Death distribution by race (Fleeing)",style = list(fontFamily = "Arial")
  )

###########################################
# 21. Race Bar chart
##########################################
temp <- data1$race %>% 
  table() %>% 
  as.data.frame()

temp <- rename(temp, Name=.,)

temp <- temp[order(temp$Freq , decreasing = TRUE),]

head(temp, n = 10L) %>% hchart(type = "column", colorByPoint = TRUE, hcaes(x = Name, y = Freq), name = "Deaths") %>%
  hc_chart(style = list(fontFamily = "Arial")) %>%
  hc_xAxis(title = list(text = "States")) %>% 
  hc_yAxis(title = list(text = "Deaths")) %>%
  hc_title(text = "US Police Shootings Deaths by race",
           style = list(fontFamily = "Arial"))

#######################
# 22. Race pie chart
#######
temp$Perc <- temp$Freq/sum(temp$Freq)

temp %>% hchart(type = "pie", hcaes(name = Name, y = Perc),name = "%") %>% 
  hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE)))%>%
  hc_title(text = "US Police Shootings Deaths by race (%)",
           style = list(fontFamily = "Arial"))
#########################
# 23. Tree Map of cities
##########################
temp <- data1 %>% group_by(city , state) %>% 
  summarise(n = n(), unique = length(unique(city))) %>% 
  arrange(-n, -unique)
temp$city_state <- paste(temp$city,temp$state, sep = "-")

head(temp, 20L) %>% hchart(type = "treemap", hcaes(x = city_state, value = n, color = n))%>% 
  hc_chart(style = list(fontFamily = "Arial")) %>% 
  hc_title(text = "US Police Shootings by Cities",style = list(fontFamily = "Arial")) %>% 
  hc_size(height = 700) %>%
  hc_colorAxis(stops = color_stops(colors = viridis::inferno(15)))

##############################
#24. Better TREE Map
#############################
#If you wanted a better treemap. The kaggle has a problem with the data_to_hierarchical function
lvl_opts <-  list(
  list(
    level = 1,
    borderWidth = 0,
    borderColor = "transparent",
   dataLabels = list(
      enabled = TRUE,
      align = "left",
      verticalAlign = "top",
      style = list(fontSize = "12px", textOutline = FALSE, color = "white")
    )
  ),
  list(
    level = 2,
    borderWidth = 0,
    borderColor = "transparent",
    colorVariation = list(key = "brightness", to = 0.250),
    dataLabels = list(enabled = FALSE),
    style = list(fontSize = "8px", textOutline = FALSE, color = "white")
  )
)

temp <- data1 %>%
select(state,city) %>%
mutate(val = 1)
hchart(data_to_hierarchical(temp, c(state, city), val),
type = "treemap",
levelIsConstant = FALSE,
allowDrillToNode = TRUE,
levels = lvl_opts,
tooltip = list(valueDecimals = FALSE)) %>% 
hc_chart(style = list(fontFamily = "Arial")) %>% 
hc_title(text = "US Police Shootings by State",style = list(fontFamily = "Arial")) %>% 
hc_size(height = 700)

####################
# 25. density plot for age and race
age_race <- ggplot(data1,aes(x=age,group=race))+geom_density(aes(fill=race),alpha=0.5)+ labs(title="Age - Race")
age_race

######################
#26. 
data1$race[data1$race == "W"] <- "White"
data1$race[data1$race == "B"] <- "Black"
data1$race[data1$race == "H"] <- "Hispanic/Latino"
data1$race[data1$race == "O"] <- "Other"
data1$race[data1$race == "A"] <- "Asian/Pacific Islander"
data1$race[data1$race == "N"] <- "Native American"

data1$gender[data1$gender == "M"] <- "Male"
data1$gender[data1$gender == "F"] <- "Female"

#create unarmed tag
data1$unarmed <- ifelse(data1$armed == "unarmed", TRUE, FALSE)

#check missing data
sapply(data1, function(y) sum(length(which(is.na(y)))))


#summary stats----------------------------

# state summary stats
state_summary <- data1 %>%
  group_by(state) %>%
  summarise(fatals = n(),
            unarmed_victim = sum(armed == "unarmed", na.rm=TRUE),
            unarmed_victim_rate = round(unarmed_victim / fatals * 100),
            gun_victim = sum(armed == "gun" | armed == "gun and knife" |
                             armed == "hatchet and gun" | armed == "guns and explosives", na.rm=TRUE),
            gun_victim_rate = round(gun_victim / fatals * 100),
            other_armed_victim = sum(fatals - unarmed_victim - gun_victim, na.rm=TRUE),
            mean_victim_age = round(mean(age, na.rm=TRUE)),
            no_body_camera_rate = round(sum(body_camera == "False") / fatals * 100),
            not_fleeing_rate = round(sum(flee == "Not fleeing", na.rm=TRUE) / fatals * 100),
            foot_fleeing_rate = round(sum(flee == "Foot", na.rm=TRUE) / fatals * 100),
            attacking_victim = sum(threat_level == "attack", na.rm=TRUE),
            attacking_victim_rate = round(attacking_victim / fatals * 100))

#race by state summary stats
race_state_summary <- data1 %>%
  group_by(state, race) %>%
  summarise(fatals = n(),
            unarmed_victim = sum(armed == "unarmed", na.rm=TRUE),
            unarmed_victim_rate = round(unarmed_victim / fatals * 100),
            gun_victim = sum(armed == "gun" | armed == "gun and knife" |armed == "hatchet and gun" | armed == "guns and explosives", na.rm=TRUE),
            gun_victim_rate = round(gun_victim / fatals * 100),
            other_armed_victim = sum(fatals - unarmed_victim - gun_victim, na.rm=TRUE),
            mean_victim_age = round(mean(age, na.rm=TRUE)),
            no_body_camera_rate = round(sum(body_camera == "False") / fatals * 100),
            not_fleeing_rate = round(sum(flee == "Not fleeing", na.rm=TRUE) / fatals * 100),
            foot_fleeing_rate = round(sum(flee == "Foot", na.rm=TRUE) / fatals * 100),
            attacking_victim = sum(threat_level == "attack", na.rm=TRUE),
            attacking_victim_rate = round(attacking_victim / fatals * 100))

#mapping items-----------------------
data(usgeojson)
n <- 4
colstops <- data.frame(q = 0:n/n,c = substring(viridis(n + 1, option = "A"), 0, 7)) %>%
list_parse2()

#census data-------------------------

data(states)
states$population <- as.numeric(states$population)

state_summary <- left_join(state_summary, states, by = c("state"="state")) %>%
  mutate(fatals_per_hunthou = round(fatals/population*100000, digits=2),
         unarmed_per_hunthou = round(unarmed_victim/population*100000, digits=2),
         gun_victim_per_hunthou = round(gun_victim/population*100000, digits=2),
         other_armed_victim_per_hunthou = round(other_armed_victim/population*100000, digits=2),
         attacking_victim_per_hunthou = round(attacking_victim/population*100000, digits=2))
race_state_summary <- left_join(race_state_summary, states, by = c("state"="state")) %>%
  mutate(fatals_per_hunthou = round(fatals/population*100000, digits=2),
         unarmed_per_hunthou = round(unarmed_victim/population*100000, digits=2),
         gun_victim_per_hunthou = round(gun_victim/population*100000, digits=2),
         other_armed_victim_per_hunthou = round(other_armed_victim/population*100000, digits=2),
         attacking_victim_per_hunthou = round(attacking_victim/population*100000, digits=2))


 # An analysis of fatal police shootings in the US (since 1st January 2015), mostly spatial lines of enquiry with some particular focus given to important public/political issues towards the end. Consideration to the sombre and emotive nature of this dataset has been made in the theme and language used in this piece.

  
  # Mapping the Victims
  
 # In this section, spatial properties of the data are explored in order to demonstrate how fatal police shootings look across US states. Importantly, viewing the data in this way helps keep these deaths grounded in reality.

#First, a look at the overall bodycount by state:
  
  
{r, echo=FALSE}
#fatals count
highchart() %>%
  hc_title(text = "Fatal US Police Shootings, 2015-Present <br> Civilian Bodycounts") %>%
  hc_subtitle(text = "Source: The Washington Post") %>%
  hc_add_series_map(usgeojson, df = state_summary,
                    value = "fatals", joinBy = c("postalcode", "state"),
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.postalcode}')) %>%
  hc_colorAxis(stops = colstops) %>%
  hc_legend(valueDecimals = 0, valueSuffix = "%") %>%  
  hc_mapNavigation(enabled = TRUE) %>%
  hc_add_theme(hc_theme_darkunica())
