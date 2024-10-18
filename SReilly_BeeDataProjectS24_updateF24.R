####Exploring Chicagoland bee data####
#Shannon Reilly
#10. Mai 2024

####Prepping Workspace####

#set working directory
setwd("~/Documents/School/NU/CHI_Bees/Bee_Code")

#load standard package(s)
library(tidyverse) #data manipulation (dplyr), plotting (ggplot)
library(patchwork) #multipanel plotting
library(car) #car::Anova()
library(emmeans) #emmeans::emmeans(), error bars and ANOVA stuff
library(readxl)
library(bbmle) #AICtab()
library(MASS)

#remember to turn on rainbow parentheses under "Code"

#R parameters
options(scipen = 99999) #this adjusts the printing of small decimals

#### ggplot theme ####
practical_theme <- function() {
  theme(panel.grid.major = element_blank(), #remove gridlines
        panel.grid.minor = element_blank(), #remove gridlines
        panel.background = element_blank(), #remove grey background
        panel.border = element_rect(color = "black", fill = NA, size = 1), #include black box around plot
        #aspect.ratio = 1, #make panel a square #don't use this if intending to "patchwork" plots later...
        axis.text.y = element_text(size = 10, color = "black"), #adjust text size and color
        axis.text.x = element_text(size = 10, color = "black"), #adjust text size and color
        strip.background = element_blank())
}
month_order2 <- c('June', 'July', 'August', 'September') 

#### REFERENCE ONLY Loading prelim bee data ####
#this was done previously, but code left for now in case of future confusion/information needs (at least for now)

#pull in downloaded .csv file (can look for package for google sheets direct access later)
# chibees.1 <- read.csv("Chicagoland_Avirescens_Box1to3ato6_SR.csv")
# 
# chibees.av <- chibees.1 %>% 
#   dplyr::filter(Species == "virescens")
# 
# chibees.av %>% 
#   dplyr::group_by(Location) %>% 
#   ggplot(aes(fill = Location, x = Date)) +
#   geom_bar(position = position_dodge(width = 0.5)) +
#   practical_theme()

#refine this mess by separating dates and also grouping/graphing by months!!!

# av.dates <- chibees.av %>% 
#   separate(Date, into = c("Day", "Month", "Year"), sep = " ", remove = F)
# 
# head(av.dates)
# 
# av.dates %>% 
#   dplyr::group_by(Location) %>% 
#   ggplot(aes(fill = Location, x = Month)) +
#   geom_bar(position = position_dodge(width = 0.5)) +
#   practical_theme()

#sort out locations by month, add values column, pivot wider, and reorder (so months chronological)

# av.mos <- av.dates %>% 
#   dplyr::group_by(Location) %>% 
#   dplyr::distinct(Month) %>% 
#   dplyr::arrange(Location)
# 
# view(av.mos)
# 
# av.mos2 <- av.mos %>% 
#   dplyr::mutate(value = TRUE) %>% 
#   pivot_wider(names_from = Month, values_from = value, values_fill = FALSE) %>% 
#   dplyr::select(Location, June, July, August, September)
# 
# head(av.mos2)

#explore which sites have samples in which month(s)

# av.mos2 %>% 
#   dplyr::filter(June == "TRUE") %>% 
#   dplyr::filter(July == "TRUE") %>% 
#   dplyr::filter(August == "TRUE") %>% 
#   dplyr::filter(September == "TRUE")
#16 June-July
#20 June & August
#13 June & September
#16 June-August
#11 June-July & September
#13 June & August-September
#21 July-August
#14 July & September
#13 July-September
#16 August-September
#11 all 4 months

#replicates of above code, but taking into account year as well:
#first using just 2010
# av.2010.mos <- av.dates %>% 
#   dplyr::filter(Year == "2010") %>% 
#   dplyr::group_by(Location) %>% 
#   dplyr::distinct(Month) %>% 
#   dplyr::arrange(Location)
# 
# av.2010.mos %>% 
#   distinct(Location)
#now 25 unique sites in 2010

# av.2010.mos2 <- av.2010.mos %>% 
#   dplyr::mutate(value = TRUE) %>% 
#   pivot_wider(names_from = Month, values_from = value, values_fill = FALSE) %>% 
#   dplyr::select(Location, June, July, August, September)
# 
# av.2010.mos2 %>% 
#   dplyr::filter(July == "TRUE") %>% 
#   dplyr::filter(August == "TRUE")
#Only 2 sites sampled in June; 10 in July, 14 in August, 19 in September
#Only 4 sites sampled July-August

#looking at 2011 data next
# av.2011.mos <- av.dates %>% 
#   dplyr::filter(Year == "2011") %>% 
#   dplyr::group_by(Location) %>% 
#   dplyr::distinct(Month) %>% 
#   dplyr::arrange(Location)
# 
# av.2011.mos %>% 
#   distinct(Location)
#32 unique sites--next to look at how many 2-3x samples among those

# av.2011.mos2 <- av.2011.mos %>% 
#   dplyr::mutate(value = TRUE) %>% 
#   pivot_wider(names_from = Month, values_from = value, values_fill = FALSE) %>% 
#   dplyr::select(Location, June, July, August)
#No September data from 2011? (Or at least none found so far...)

# head(av.2011.mos2)
# view(av.2011.mos2)
# 
# av.2011.mos2 %>% 
#   dplyr::filter(June == "TRUE") %>% 
#   dplyr::filter(August == "TRUE")
#Looks like 10 with all 3 months, 10 with June-July, 20 with June & August, 12 with July-August 

# av.2010.dates <- av.dates %>% 
#   dplyr::filter(Year == "2010")
# 
# av.2011.dates <- av.dates %>% 
#   dplyr::filter(Year == "2011")
# 
# av.2010.dates %>% 
#   dplyr::group_by(Location) %>% 
#   ggplot(aes(fill = Location, x = Month)) +
#   geom_bar(position = position_dodge(width = 0.5)) +
#   practical_theme()
# 
# av.2011.dates %>% 
#   dplyr::group_by(Location) %>% 
#   ggplot(aes(fill = Location, x = Month)) +
#   geom_bar(position = position_dodge(width = 0.5)) +
#   practical_theme()
# 
# nrow(av.2010.dates)
# #219 rows
# 
# nrow(av.2011.dates)
# #460 rows

# av.2011.dates %>% 
#   dplyr::filter(Month %in% c("June", "August")) %>% 
#   #dplyr::distinct(Location) %>% 
#   tally()
# #414 bees!!!
# 
# av.2011.dates %>% 
#   dplyr::filter(Month == "July") %>% 
#   tally()
# 
# av.2011.dates %>% 
#   dplyr::filter(Month %in% c("June", "August")) %>% 
#   dplyr::group_by(Sex) %>% 
#   tally()
# #286 females and 128 males


#For working with location info a bit more neatly:
# av.locales <- av.dates %>% 
#   tidyr::unite("Binomial", Genus:Species, sep = "_", remove = F) %>% 
#   tidyr::separate(Location, into = c("Location_Name", "Address"), sep = ", ", remove = F)
# 
# head(av.locales, n = 12)
# 
# av.clean <- av.locales %>% 
#   dplyr::select(Family, Binomial, Genus, Species, Sex,
#                 Country, State, City, 
#                 Location_Name, Address, Location_Type, 
#                 Lat, Long, 
#                 Date, Day, Month, Year,
#                 Collector)
# 
# head(av.clean)

#then save a copy of this file, to send to Ian!

#write.csv(av.clean, 'CHI.Avirescens.clean.csv')

# av.clean %>% 
#   distinct(Location_Name) %>% 
#   tally()
# 
# av.2011.mos2 %>% 
#   dplyr::filter(June == "TRUE") %>% 
#   dplyr::filter(August == "TRUE")
# 
# av.JunAu.2011.clean <- av.clean %>% 
#   dplyr::filter(Year == "2011") %>% 
#   dplyr::filter(Location_Name %in% c(
#     "65th & Woodlawn", "Andersonville",
#     "Brick Yard", "City Farm", "Ginkgo Garden",
#     "Green Youth", "Greenhouse", "Growing Power",
#     "Howard community", "Jackson Park", "Kendall",
#     "Lincoln Park", "Monticello", "Nicholas Park",
#     "North Park", "Peggy", "Procksa Park",
#     "S. Arts garden", "St. Ignathius", "Xochiquetzal"))
# 
# view(av.JunAu.2011.clean)

#write.csv(av.JunAu.2011.clean, 'CHI.Avirescens.JunAug2011.csv')


#av.2010.mos2 %>% 
#  dplyr::filter(June == "FALSE")


#### Adding Absence Sites to DataSet ####

#Find and bring in master list of sites (in this case, sticking to sites that had
  #any A. virescens for at least one sample period during the full season; it would
  #probably be rather logistically messy to figure out *all* possible sites at this
  #juncture, given how many boxes (and bees within them), of other, non-A. virescens
  #species, are still not entered yet!

#From google sheets, can download or otherwise copy master location list!
  #Or can maybe retrieve from Ian's recent GIS adventures?
#Then join to current arrangement, mutate to add values (counts by location by month), and
  #pivot_wider to create columns with 0 vs (# count) for each possible combination.

#Or maybe can just follow same pattern as creating binary pivot, but with numeric values instead
  #These could be calculated and added separately (counts per location per date), with
  #0s added where values are missing (as before/above)


CHIbees.IR <- read.csv("CHI.Avirescens.cleanIR2.csv")

av.mos.counts <- CHIbees.IR %>% 
  dplyr::filter(Year == "2011") %>% 
  dplyr::group_by(Location_Name, Month) %>% 
  dplyr::tally()

av.mos.counts.wide <- av.mos.counts %>% 
  pivot_wider(names_from = Month, values_from = n, values_fill = 0) %>% 
  dplyr::select(Location_Name, June, July, August)

view(av.mos.counts.wide)


#working out how to separately show M and F bees (for purposes of sex-ratio calculations)
  #this definitely still needs troubleshooting!!!
#Look up expand grid function/approach
    #Name data frame, then expand_grid() with [field] = [unique() or seq(min(), max())] (for
      #however many fields are being put into a grid of combinations)
    #Then create new combined dataframe with [grid dataframe] left_join([original df]) %>% 
      #select desired columns %>% 
      #mutate to add level (?)

#### Adding Absence Sites Part 2 ####

av.mos.counts2 <- CHIbees.IR %>% 
  dplyr::filter(Year == "2011") %>% 
  dplyr::group_by(Location_Name, Sex, Month) %>% 
  dplyr::tally()

av.mos.counts.wide2 <- av.mos.counts2 %>% 
  pivot_wider(names_from = Month, values_from = n, values_fill = 0) %>% 
  dplyr::select(Location_Name, Sex, June, July, August)

av.mos.mf.counts.wide <- av.mos.counts2 %>% 
  pivot_wider(names_from = Sex, values_from = n, values_fill = 0)

av.mos.mf.counts.long <- av.mos.mf.counts.wide %>% 
  pivot_longer(cols = c("M","F"),
               names_to = "Sex",
               values_to = "Bee_Count")

av.mos.loc.counts.wide <- av.mos.mf.counts.long %>% 
  pivot_wider(names_from = Month, values_from = Bee_Count, values_fill = 0) %>% 
  dplyr::select(Location_Name, Sex, June, July, August)

av.mos.loc.counts.long <- av.mos.loc.counts.wide %>% 
  pivot_longer(cols = c("June", "July", "August"),
               names_to = "Month",
               values_to = "Bee_Count")

view(av.mos.loc.counts.long) #This is the important dataframe!!!

#Pivoting the genralized bee count dataframe longer again, for purposes of more exploratory plotting
 av.mos.counts.long <- av.mos.counts.wide %>% 
   pivot_longer(cols = c("June", "July", "August"),
                names_to = "Month",
                values_to = "Bee_Count")
 
 av.mos.counts.long %>%
   dplyr::group_by(Location_Name) %>% 
   ggplot(aes(x = as.factor(Month), y = Bee_Count, group = Location_Name, colour = Location_Name)) +
   geom_point() +
   practical_theme() +
   theme(legend.position = "none")
 
 av.mos.counts.long %>%
   dplyr::group_by(Location_Name) %>% 
   ggplot(aes(x = as.factor(Month), y = Bee_Count, group = Location_Name, fill = Location_Name)) +
   geom_bar(stat = "identity", position = position_dodge(width = 0.5)) +
   practical_theme() +
   theme(legend.position = "none")
 
##Taking dataframe with 0 values added for months (with divisions by sex), can next add total bee counts
  ##(across the whole season) per location.
  ##If that works, then need to figure out most efficient way to calculate subtotals by sex and then
    ##calculate the sex ratio (males:females)
 
av.loc.totals <-  av.mos.loc.counts.long %>%
   group_by(Location_Name) %>% 
   dplyr::mutate(Bee_Total = sum(Bee_Count))

av.loc.totals2 <- av.loc.totals %>% 
  pivot_wider(names_from = Sex, values_from = Bee_Count) %>% 
  dplyr::rename(Male = M) %>% 
  dplyr::rename(Female = F)
 
av.loc.totals3 <- av.loc.totals %>% 
   group_by(Location_Name, Sex) %>% 
   dplyr::mutate(Bee_Subtotal = sum(Bee_Count))
 
av.loc.totals3 %>% 
   group_by(Location_Name, Sex) %>% 
   dplyr::mutate(Bee_Ratio = Bee_Subtotal/Bee_Subtotal)
 
av.loc.totals2 %>% 
  dplyr::group_by(Location_Name) %>% 
  dplyr::mutate(Bee_Ratio = Male/Female)

av.loc.totals2b <- av.loc.totals2 %>% 
  dplyr::group_by(Location_Name) %>% 
  dplyr::mutate(Bee_Subtotal_M = sum(Male)) %>% 
  dplyr::mutate(Bee_Subtotal_F = sum(Female))

av.loc.totals2c <- av.loc.totals2b %>% 
  pivot_longer(cols = c("Male", "Female"),
               names_to = "Sex",
               values_to = "Bee_Count") %>% 
  dplyr::select(Location_Name, Month, Sex, Bee_Count,
                Bee_Subtotal_M, Bee_Subtotal_F, Bee_Total)

#av.loc.ratios <- av.loc.totals2c %>% 
#  dplyr::group_by(Location_Name) %>% 
#  dplyr::mutate(Bee_Ratio = Bee_Subtotal_M/Bee_Subtotal_F)

#Trying ratios again but split and calculated against total bees (vs each other) in order to avoid "inf"
av.loc.ratios <- av.loc.totals2c %>% 
  dplyr::group_by(Location_Name) %>% 
  dplyr::mutate(Bee_Ratio_M = Bee_Subtotal_M/Bee_Total) %>% 
  dplyr::mutate(Bee_Ratio_F = Bee_Subtotal_F/Bee_Total)

view(av.loc.ratios)

head(av.loc.ratios, n = 18)

#av.loc.ratios %>% 
#  pivot_longer(cols = c("Bee_Ratio_M", "Bee_Ratio_F"),
#               names_to = "Ratio_Type",
#               values_to = "Proportion")

av.Mratios <- av.loc.ratios %>%
  ggplot(aes(fill = Location_Name)) +
  geom_bar(aes(x = as.factor(Location_Name), y = Bee_Ratio_M), stat = "identity", position = position_dodge(width = 0.5)) +
  practical_theme() +
  theme(legend.position = "none")

av.Fratios <- av.loc.ratios %>%
  ggplot(aes(fill = Location_Name)) +
  geom_bar(aes(x = as.factor(Location_Name), y = Bee_Ratio_F), stat = "identity", position = position_dodge(width = 0.5)) +
  practical_theme() +
  theme(legend.position = "none")

av.Totalbees <- av.loc.ratios %>%
  ggplot(aes(fill = Location_Name)) +
  geom_bar(aes(x = as.factor(Location_Name), y = Bee_Total), stat = "identity") +
  #scale_x_continuous(expand = expansion(mult = c(0, .1))) +
  practical_theme() +
  theme(legend.position = "none")

av.Mratios + av.Fratios

av.Mratios/av.Fratios #/av.Totalbees

av.Mratios2 <- av.loc.ratios %>%
  ggplot(aes()) +#fill = Location_Name)) +
  geom_bar(aes(y = as.factor(Location_Name), x = Bee_Ratio_M), stat = "identity") +
  scale_x_continuous(expand = expansion(mult = c(0, .1))) +
  practical_theme() +
  theme(legend.position = "none")+
  theme(axis.text.y = element_blank(), axis.text.x = element_text(size = 8),
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "pt"),
        #axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.length.y = unit(0, "pt"))

av.Fratios2 <- av.loc.ratios %>%
  ggplot(aes()) +#fill = Location_Name)) +
  geom_bar(aes(y = as.factor(Location_Name), x = Bee_Ratio_F), stat = "identity") +
  scale_x_continuous(expand = expansion(mult = c(0, .1))) +
  practical_theme() +
  theme(legend.position = "none") +
  theme(legend.position = "none")+
  theme(axis.text.y = element_blank(), axis.text.x = element_text(size = 8),
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "pt"),
        #axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.length.y = unit(0, "pt"))

av.Totalbees + av.Mratios2 + av.Fratios2

#### REFERENCE ONLLY OH Notes 23 Mai 2024 ####

#try using summarize to build simplified df
#look at interaction model of impervious surface effects on male vs female bees
#or for now just model/plot male and female bees separately
#compete models for poisson and negative binomial

#over the summer, spend ~30min minimum (per week) just "in contact" with data
#if this continues into a paper, consider things like: leadership/lead author(s)

#### Data analysis workflow 1 ####
#summarizing to simplify df
av.loc.ratios.clean <- av.loc.ratios %>% 
  dplyr::group_by(Location_Name) %>% 
  dplyr::summarize(
    Ratio_M = mean(Bee_Ratio_M),
    Ratio_F = mean(Bee_Ratio_F),
    Bees_M = mean(Bee_Subtotal_M),
    Bees_F = mean(Bee_Subtotal_F),
    Bees_Total = mean (Bee_Total),
  )

#Alternate query: How does the number of bees vary across months? Relatedly, how does the proportion of female bees vary across months?

#Create summary df that has location, month, sex, bee count (raw), and totals/subtotals/ratios grouped by month (vs location)
av.pheno.base <- av.loc.ratios %>% 
  dplyr::select(Month, Sex, Location_Name, Bee_Count) %>% 
  dplyr::arrange(Month)

av.pheno.totals <- av.pheno.base %>% 
  pivot_wider(names_from = Sex, values_from = Bee_Count) %>% 
  dplyr::mutate(Bee_Total = Male + Female) %>% 
  pivot_longer(cols = c("Male", "Female"),
               names_to = "Sex",
               values_to = "Bee_Count") %>% 
  dplyr::select(Month, Location_Name, Sex, Bee_Count, Bee_Total)

head(av.pheno.totals)

view(av.pheno.totals)

#Create a poisson and a negative binomial GLM to compete/check for overdispersal of count data

av.pheno.f <- av.pheno.totals %>% 
  dplyr::filter(Sex == "Female")
av.pheno.m <- av.pheno.totals %>% 
  dplyr::filter(Sex == "Male")

#looking at just females
av_mod_f_fish <- glm(Bee_Count ~ Month, family = "poisson", data = av.pheno.f)
Anova(av_mod_f_fish)
emmeans(av_mod_f_fish, ~ Month, type = "response")

av_mod_f_nbin <- glm.nb(Bee_Count ~ Month, data = av.pheno.f)
Anova(av_mod_f_nbin)
emmeans(av_mod_f_nbin, ~ Month, type = "response")

AICtab(av_mod_f_fish, av_mod_f_nbin)

plot_av_f_nbin <- as_tibble(emmeans(av_mod_f_nbin, ~ Month, type = "response"))

#looking at just males
av_mod_m_fish <- glm(Bee_Count ~ Month, family = "poisson", data = av.pheno.m)
Anova(av_mod_m_fish)
emmeans(av_mod_m_fish, ~ Month, type = "response")

av_mod_m_nbin <- glm.nb(Bee_Count ~ Month, data = av.pheno.m)
Anova(av_mod_m_nbin)
emmeans(av_mod_m_nbin, ~ Month, type = "response")

AICtab(av_mod_m_fish, av_mod_m_nbin)

plot_av_m_nbin <- as_tibble(emmeans(av_mod_m_nbin, ~ Month, type = "response"))

#looking at all bees together
av_mod_fish <- glm(Bee_Total ~ Month, family = "poisson", data = av.pheno.totals)
Anova(av_mod_fish)
emmeans(av_mod_fish, ~ Month, type = "response")

av_mod_nbin <- glm.nb(Bee_Total ~ Month, data = av.pheno.totals)
Anova(av_mod_nbin)
emmeans(av_mod_nbin, ~ Month, type = "response")

AICtab(av_mod_fish, av_mod_nbin)

plot_av_all_nbin <- as_tibble(emmeans(av_mod_nbin, ~ Month, type = "response"))

plot_av_f_nbin 

plot_av_m_nbin

plot_av_all_nbin

month_order <- c('June', 'July', 'August') 

#p <- ggplot(iris)
#p + geom_bar(aes(x = factor(Species, level = level_order)))

av_all_ptplot <-  
  ggplot(plot_av_all_nbin, aes(x = factor(Month, level = month_order), y = response, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0) +
  xlab("Month") +
  ylab("Average Number of Bees") +
  #scale_colour_manual(values = c("dodgerblue", "goldenrod")) +
  #ylim(0, 130) +
  #theme(axis.text = element_text(size = 12),
   #     axis.title = element_text(size = 12, face = "bold")) +
  practical_theme()+
  ggtitle("All Bees")

av_f_ptplot <- plot_av_f_nbin %>% 
  ggplot(aes(x = factor(Month, level = month_order), y = response, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0) +
  xlab("Month") +
  ylab("Average Number of Bees") +
  #scale_colour_manual(values = c("dodgerblue", "goldenrod")) +
  #ylim(0, 130) +
  #theme(axis.text = element_text(size = 12),
  #     axis.title = element_text(size = 12, face = "bold")) +
  practical_theme()+
  ggtitle("Female Bees")

av_m_ptplot <- plot_av_m_nbin %>% 
  ggplot(aes(x = factor(Month, level = month_order), y = response, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0) +
  xlab("Month") +
  ylab("Average Number of Bees") +
  #scale_colour_manual(values = c("dodgerblue", "goldenrod")) +
  #ylim(0, 130) +
  #theme(axis.text = element_text(size = 12),
  #     axis.title = element_text(size = 12, face = "bold")) +
  practical_theme()+
  ggtitle("Male Bees")

av_m_ptplot/av_f_ptplot/av_all_ptplot + plot_layout(axis_titles = "collect")

#plot_annotation(tag_levels = 'A')

####Setting up bee counts by location, with lat/long available to join####
CHIbees.IR <- read.csv("CHI.Avirescens.cleanIR2.csv")

av.month.counts <- CHIbees.IR %>% 
  dplyr::group_by(Location_Name, Sex, Month, Year) %>% 
  dplyr::tally()

#Looking at all months (within both years), with removal of September 2011
av.month.counts2 <- av.month.counts %>%
  pivot_wider(names_from = Sex, values_from = n, values_fill = 0) %>%
  dplyr::rename(Male = M) %>% 
  dplyr::rename(Female = F) %>% 
  pivot_longer(cols = c("Male","Female"),
               names_to = "Sex",
               values_to = "Bee_Count") %>% 
  pivot_wider(names_from = Month, values_from = Bee_Count, values_fill = 0) %>% 
  pivot_longer(cols = c("June", "July", "August", "September"),
               names_to = "Month",
               values_to = "Bee_Count") %>% 
  dplyr::filter(!(Year == "2011" & Month == "September"))

#### Proxies for urbanization? ####

view(CHIbees.IR)

av.latlong.counts <- CHIbees.IR %>% 
  dplyr::group_by(Location_Name, Lat, Long, Sex, Month, Year) %>% 
  dplyr::tally()

#Can filter or split out "year" data later, if necessary

view(av.latlong.counts)

av.latlong.counts2 <- av.latlong.counts %>%
  pivot_wider(names_from = Sex, values_from = n, values_fill = 0) %>%
  dplyr::rename(Male = M) %>% 
  dplyr::rename(Female = F) %>% 
  pivot_longer(cols = c("Male","Female"),
               names_to = "Sex",
               values_to = "Bee_Count") %>% 
  pivot_wider(names_from = Month, values_from = Bee_Count, values_fill = 0) %>% 
  pivot_longer(cols = c("June", "July", "August", "September"),
               names_to = "Month",
               values_to = "Bee_Count") %>% 
  dplyr::filter(!(Year == "2011" & Month == "September"))

head(av.latlong.counts2, n = 35)
view(av.latlong.counts2)

#### REFERENCE ONLY Go Figure Notes ####
#Use lines to connect points (comparing males vs females)
  #Show patterns over time, rather than automatically attending to month by month comparisons
#Look at interaction between bee sex and year
#Try looking at probability of presence/absence (of bees) on surveys, rather than numbers of bees,
  #at a given site for each month

#### Bringing in distance-from-city-center & urbanization info ####
av_loc_distcent1 <- read.csv("av_loc_DistToCent1.csv")

str(av_loc_distcent1)
view(av_loc_distcent1)

av_loc_distcent1$Location_Name = av_loc_distcent1$InputID
str(av_loc_distcent1)

CHIbees.IR2 <- left_join(CHIbees.IR, av_loc_distcent1, by = "Location_Name")

str(CHIbees.IR2)
view(CHIbees.IR2)

CHIbees.IR2a <- CHIbees.IR2 %>% 
  dplyr::select(!InputID) %>% 
  dplyr::select(!TargetID)

av.distcent.counts <- CHIbees.IR2a %>% 
  dplyr::group_by(Location_Name, Lat, Long, Sex, Month, Year, Distance, Urban_Pct, Urban_Cat) %>% 
  dplyr::tally()

av.distcent.counts2 <- av.distcent.counts %>%
  pivot_wider(names_from = Sex, values_from = n, values_fill = 0) %>%
  dplyr::rename(Male = M) %>% 
  dplyr::rename(Female = F) %>% 
  pivot_longer(cols = c("Male","Female"),
               names_to = "Sex",
               values_to = "Bee_Count") %>% 
  pivot_wider(names_from = Month, values_from = Bee_Count, values_fill = 0) %>% 
  pivot_longer(cols = c("June", "July", "August", "September"),
               names_to = "Month",
               values_to = "Bee_Count") %>% 
  dplyr::filter(!(Year == "2011" & Month == "September"))


view(av.distcent.counts2)

av.distcent.counts2b %>% 
  ggplot(aes(x = Urban_Pct)) +
  geom_histogram(stat = "count") +
  #scale_fill_manual(values = c("cornflowerblue", "violetred")) +
  practical_theme()

av.distcent.counts2b %>% 
  ggplot(aes(y = Location_Name, x = Urban_Pct)) +
  geom_point() +
  practical_theme()

av.distcent.counts2b %>% 
  ggplot(aes(x = Urban_Cat)) +
  geom_histogram(stat = "count") +
  #scale_fill_manual(values = c("cornflowerblue", "violetred")) +
  practical_theme()

#### Converting from counts to presence/absence ####
#Reference info: https://www.reddit.com/r/rstats/comments/s8qb1q/replacing_values_0_with_1_across_several_columns/

av.month.bin1 <- cbind(av.month.counts2[1:4], ifelse(av.month.counts2[5]>0,1,0))

#### Running some binomial models (phenology) ####
av.month10.bin <- av.month.bin1 %>% 
  dplyr::filter(Year == "2010")
av.month10.mod1 = glm(Bee_Count ~ 1, family = "binomial", data = av.month10.bin)
plogis(coef(av.month10.mod1))

av.month.mod0 <- glm(Bee_Count ~ 1, family = "binomial", data = av.month.bin1)
plogis(coef(av.month.mod0))
plogis(confint(av.month.mod0))

av.month.mod1 = glm(Bee_Count ~ as.factor(Month), family = "binomial", data = av.month.bin1)
Anova(av.month.mod1)
av.month.modyr <- glm(Bee_Count ~ as_factor(Year), family = "binomial", data = av.month.bin1)
Anova(av.month.modyr)

av.month.mod2 <- glm(Bee_Count ~Year*Sex, family = "binomial", data = av.month.bin1)
av.month.mod3 <- glm(Bee_Count ~ Month*Sex, family = "binomial", data = av.month.bin1)
av.month.mod4 <- glm(Bee_Count ~ Sex*Month*Year, family = "binomial", data = av.month.bin1)

AICtab(av.month.mod2, av.month.mod3, av.month.mod4)

Anova(av.month.mod4)

emmeans(av.month.mod4, ~ Sex*Month*Year, type = "response")

plot_av_month_mod4 <- as_tibble(emmeans(av.month.mod4, ~ Sex*Month*Year, type = "response"))

plot_av_month_mod4a <- plot_av_month_mod4 %>% 
  dplyr::filter(!(Year == "2011" & Month == "September"))

ggplot(plot_av_month_mod4a, aes(x = factor(Month, level = month_order2), y = prob,
                                  ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.3), aes(group = Sex), alpha = 0.75) +
  geom_line(aes(colour = Sex, group = Sex), position = position_dodge(width = 0.3),
                                                                      linewidth = 1) +
  geom_point(size = 3, position = position_dodge(width = 0.3), aes(colour = Sex)) +
  xlab("Month") +
  ylab("Probability of Bee Presence") +
  scale_colour_manual(values = c("goldenrod", "dodgerblue")) +
  theme(aspect.ratio = 1) +
  #theme(axis.text = element_text(size = 12),
  #     axis.title = element_text(size = 12, face = "bold")) +
  practical_theme()+
  facet_grid(~ Year)
  
#Change error bars to same colour as dots, then change alpha of line/path elements to deemphasize but still show

ggplot(plot_av_month_mod4a, aes(x = factor(Month, level = month_order2), y = prob,
                                ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.3), aes(colour = Sex, group = Sex)) +
  geom_line(aes(colour = Sex, group = Sex), alpha = 0.5, position = position_dodge(width = 0.3),
            linewidth = 1) +
  geom_point(size = 3, position = position_dodge(width = 0.3), aes(colour = Sex)) +
  xlab("Month of Sample") +
  ylab("Probability of Bee Presence") +
  scale_colour_manual(values = c("goldenrod", "dodgerblue")) +
  theme(axis.text = element_text(size = 12),
       axis.title = element_text(size = 12, face = "bold"),
       strip.text = element_text(face = "bold", size = 12),
       legend.title = element_text(size = 10.5, face = "bold"),
       legend.box.background = element_rect(color = "black"),
       aspect.ratio = 1,
       legend.position = "bottom") +
  practical_theme() +
  facet_grid(~ Year)

#### R Graphics Cookbook (reference) ####

#https://r-graphics.org/

#### Now back to urbanization proxies ####

view(av.distcent.counts2)
str(av.distcent.counts2)

#For this evening, straight-up forgot how to summarize things!
  #So when coming back to this later--find M and F subtotals for each location/distance,
    #then model to see how those might relate or not?

av.distcent.counts2a <- av.distcent.counts2 %>%
  dplyr::mutate(DistBuff = Distance - 500) %>% 
  dplyr::group_by(Location_Name, Lat, Long, Year, Distance, DistBuff, Urban_Pct, Urban_Cat, Sex) %>% 
  dplyr::mutate(Bee_Subtotal = sum(Bee_Count)) %>% 
  dplyr::select(Location_Name, Lat, Long, Distance, DistBuff, Urban_Pct, Urban_Cat, Year, Sex, Bee_Subtotal) %>% 
  dplyr::distinct()

av.distcent.counts2b <- av.distcent.counts2a %>%
  dplyr::group_by(Location_Name, Year) %>% 
  dplyr::mutate(Bee_Total = sum(Bee_Subtotal))

#If looking at total numbers of bees (lumped abundance or by sex), need to account for the
  #"extra" month of sampling done in September 2010
#av.distcent.counts3 <- av.distcent.counts2 %>%
 # dplyr::mutate(DistBuff = Distance - 500) %>% 
#  dplyr::filter(!Month == "September") %>% 
 # dplyr::group_by(Location_Name, Lat, Long, Year, Distance, DistBuff, Sex) %>% 
#  dplyr::mutate(Bee_Subtotal = sum(Bee_Count)) %>% 
 # dplyr::select(Location_Name, Lat, Long, Distance, DistBuff, Year, Sex, Bee_Subtotal) %>% 
#  dplyr::distinct()

##ASK PAUL/NICK ABOUT ACCOUNTING FOR DIFFERENT #s of MONTHS (OR JUST STICK TO ONE YEAR,
  ##OR STICK WITH WHAT MONTHS DO MATCH???)

#av.distcent.counts3a <- av.distcent.counts3 %>%
#  dplyr::group_by(Location_Name, Year) %>% 
#  dplyr::mutate(Bee_Total = sum(Bee_Subtotal))

#### Modelling w/ urbanization proxies v2 ####
#t.test(Bee_Total ~ Year, data = av.distcent.counts3a)
#Total # of bees found June-August (at a given site) differs significantly between 2010 & 2011

#av_dist_mod0 <- glm(Bee_Total ~ DistBuff, family = "poisson", data = av.distcent.counts3a)
#av_dist_mod1 <- glm.nb(Bee_Total ~ DistBuff, data = av.distcent.counts3a)
#av_dist_mod1a <- glm.nb(Bee_Total ~ DistBuff*Year, data = av.distcent.counts3a)

#AICtab(av_dist_mod0, av_dist_mod1, av_dist_mod1a)
#Data still definitely overidspersed!!
#Accounting for Year definitely important, when considering DistBuff!

#Looking at relationship between abundance and sex (in general, and between years)
#av_dist_mod2 <- glm.nb(Bee_Subtotal ~ Sex, data = av.distcent.counts3a)
#av_dist_mod2a <- glm.nb(Bee_Subtotal ~ Sex*Year, data = av.distcent.counts3a)

#Anova(av_dist_mod2)
#Anova(av_dist_mod2a)

#AICtab(av_dist_mod2, av_dist_mod2a)
#There does appear to be an interaction between sex and year, when looking at abundances
  #of bees of different sexes

#av_dist_mod3 <- glm.nb(Bee_Subtotal ~ DistBuff, data = av.distcent.counts3a)
#av_dist_mod3a <- glm.nb(Bee_Subtotal ~ DistBuff*Sex*Year, data = av.distcent.counts3a)

#AICtab(av_dist_mod3, av_dist_mod3a)

#Anova(av_dist_mod3a)

#### Breaking apart M and F bees, considering impacts of year? ####
av.dist10 <- av.distcent.counts2b %>% 
  dplyr::filter(Year == "2010") %>% 
  pivot_wider(names_from = Sex,
              values_from = Bee_Subtotal) %>% 
  pivot_longer(cols = c("Bee_Total", "Male", "Female"),
               names_to = "Sex",
               values_to = "Count")
av.dist10.f <- av.dist10 %>% 
  dplyr::filter(Sex == "Female")
av.dist10.m <- av.dist10 %>% 
  dplyr::filter(Sex == "Male")
av.dist10.total <- av.dist10 %>% 
  dplyr::filter(Sex == "Bee_Total")

av.dist11 <- av.distcent.counts2b %>% 
  dplyr::filter(Year == "2011") %>% 
  pivot_wider(names_from = Sex,
              values_from = Bee_Subtotal) %>% 
  pivot_longer(cols = c("Bee_Total", "Male", "Female"),
               names_to = "Sex",
               values_to = "Count")
av.dist11.f <- av.dist11 %>% 
  dplyr::filter(Sex == "Female")
av.dist11.m <- av.dist11 %>% 
  dplyr::filter(Sex == "Male")
av.dist11.total <- av.dist11 %>% 
  dplyr::filter(Sex == "Bee_Total")

#### Back to modelling stuff ####

#trying to model years (and maybe also sexes) separately, to account for differences in both
  #number of sample sites and number of bees found (different patterns in different years)

#### 2010 data without division ####
#Looking first at 2010 data:
av_dist10_mod1 <- glm.nb(Count ~ Distance, data = av.dist10)
av_dist10_mod2 <- glm.nb(Count ~ Sex, data = av.dist10)
av_dist10_mod3 <- glm.nb(Count ~ Distance*Sex, data = av.dist10)

AICtab(av_dist10_mod1, av_dist10_mod2, av_dist10_mod3)

Anova(av_dist10_mod1)
emmeans(av_dist10_mod1, ~ Distance, type = "response")

ggplot(av.dist10, aes(x = Distance, y = Count)) +
  geom_point(size = 4, shape = 21, fill = "slategrey", colour = "white", alpha = 0.5) +
  geom_smooth(method = "glm.nb", colour = "darkblue", fill = "darkblue") +
  labs(x = "Distance from CHI center", y = "Number of Bees") +
  practical_theme() +
  facet_wrap(~ Sex)

#### 2010 data split by sexes, and vs total ####
#Trying 2010 broken up by each sex and as total counts
av_dist10f_mod1 <- glm.nb(Count ~ Distance, data = av.dist10.f)
av_dist10m_mod1 <- glm.nb(Count ~ Distance, data = av.dist10.m)
av_dist10total_mod1 <- glm.nb(Count ~ Distance, data = av.dist10.total)

Anova(av_dist10f_mod1)
Anova(av_dist10m_mod1)
Anova(av_dist10total_mod1)

av_urban10f_mod1 <- glm.nb(Count ~ Urban_Pct, data = av.dist10.f)
Anova(av_urban10f_mod1)
av_urban10m_mod1 <- glm.nb(Count ~ Urban_Pct, data = av.dist10.m)
Anova(av_urban10m_mod1)
av_urban10total_mod1 <- glm.nb(Count ~ Urban_Pct, data = av.dist10.total)
Anova(av_urban10total_mod1)

summary(av_urban10f_mod1)$r.squared

ggplot(av.dist10, aes(x = Urban_Pct, y = Count)) +
  geom_point(size = 4, shape = 21, fill = "slategrey", colour = "white", alpha = 0.5) +
  geom_smooth(method = "glm.nb", colour = "darkblue", fill = "darkblue") +
  labs(x = "Urbanization (% cover)", y = "Number of Bees") +
  xlim(0, 100) +
  practical_theme() +
  facet_wrap(~ Sex)

av_10f_urbanplot <- ggplot(av.dist10.f, aes(x = Urban_Pct, y = Count)) +
  geom_point(size = 3, shape = 21, fill = "slategrey", colour = "white", alpha = 0.35) +
  geom_smooth(method = "glm.nb", colour = "goldenrod", fill = "goldenrod") +
  xlab("Urbanization (% cover)") +
  ylab("Abundance of Bees") +
  #labs(x = "Urbanization (% cover)", y = "Number of Bees") +
  xlim(0, 100) +
  ylim(0, 35) +
  ggtitle("Female") +
  practical_theme() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))
av_10m_urbanplot <- ggplot(av.dist10.m, aes(x = Urban_Pct, y = Count)) +
  geom_point(size = 3, shape = 21, fill = "slategrey", colour = "white", alpha = 0.35) +
  geom_smooth(method = "glm.nb", colour = "dodgerblue", fill = "dodgerblue") +
  xlab("Urbanization (% cover)") +
  ylab("Abundance of Bees") +
  #labs(x = "Urbanization (% cover)", y = "Number of Bees") +
  xlim(0, 100) +
  ylim(0, 35) +
  ggtitle("Male") +
  practical_theme() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))
av_10total_urbanplot <- ggplot(av.dist10.total, aes(x = Urban_Pct, y = Count)) +
  geom_point(size = 3, shape = 21, fill = "slategrey", colour = "white", alpha = 0.35) +
  geom_smooth(method = "glm.nb", colour = "purple4", fill = "purple4") +
  xlab("Urbanization (% cover)") +
  ylab("Abundance of Bees") +
  #labs(x = "Urbanization (% cover)", y = "Number of Bees") +
  xlim(0, 100) +
  ylim(0, 35) +
  ggtitle("All") +
  practical_theme() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))

#PATCHWORK IS IGNORING ALL COMMANDS RE AXIS TITLES...:(
av_10f_urbanplot + av_10m_urbanplot + av_10total_urbanplot + plot_layout(axis_titles = "collect") + plot_annotation(
  title = "Season-Total Bees: 2010", theme = theme(plot.title = element_text(size = 16, face = "bold")))


#### 2011 data without division ####
av_dist11_mod1 <- glm.nb(Count ~ Distance, data = av.dist11)
av_dist11_mod2 <- glm.nb(Count ~ Sex, data = av.dist11)
av_dist11_mod3 <- glm.nb(Count ~ Distance*Sex, data = av.dist11)
av_dist11_mod4 <- glm.nb(Count ~ Location_Name, data = av.dist11)
av_dist11_mod4a <- glm.nb(Count ~ Location_Name*Sex, data = av.dist11)

AICtab(av_dist11_mod1, av_dist11_mod2, av_dist11_mod3, av_dist11_mod4, av_dist11_mod4a)
Anova(av_dist11_mod3)

#The whole difference in sex-based abundances is making for heady confusion, so:
#### 2011 data divided by sex, and vs total ####
av_dist11f_mod1 <- glm.nb(Count ~ Distance, data = av.dist11.f)
av_dist11m_mod1 <- glm.nb(Count ~ Distance, data = av.dist11.m)
av_dist11total_mod1 <- glm.nb(Count ~ Distance, data = av.dist11.total)

Anova(av_dist11f_mod1)
Anova(av_dist11m_mod1)
Anova(av_dist11total_mod1)

ggplot(av.dist11, aes(x = Distance, y = Count)) +
  geom_point(size = 4, shape = 21, fill = "slategrey", colour = "white", alpha = 0.5) +
  geom_smooth(method = "glm.nb", colour = "darkblue", fill = "darkblue") +
  labs(x = "Distance from CHI center", y = "Number of Bees") +
  practical_theme() +
  facet_wrap(~ Sex)

av_urban11f_mod1 <- glm.nb(Count ~ Urban_Pct, data = av.dist11.f)
Anova(av_urban11f_mod1)
av_urban11m_mod1 <- glm.nb(Count ~ Urban_Pct, data = av.dist11.m)
Anova(av_urban11m_mod1)
av_urban11total_mod1 <- glm.nb(Count ~ Urban_Pct, data = av.dist11.total)
Anova(av_urban11total_mod1)

emmeans(av_urban11f_mod1, ~Urban_Pct, type = "response")

ggplot(av.dist11, aes(x = Urban_Pct, y = Count)) +
  geom_point(size = 4, shape = 21, fill = "slategrey", colour = "white", alpha = 0.3) +
  geom_smooth(method = "glm.nb", colour = "darkblue", fill = "darkblue") +
  labs(x = "Urbanization (% cover)", y = "Number of Bees") +
  xlim(0, 100) +
  practical_theme() +
  facet_wrap(~ Sex)

  #THIS PLOT DEFINITELY DOESN'T WORK
ggplot(av.dist11, aes(x = Urban_Pct, y = Count)) +
  geom_point(size = 4, shape = 21, fill = "slategrey", colour = "white", alpha = 0.3) +
  #geom_smooth(method = "glm.nb") +
  labs(x = "Urbanization (% cover)", y = "Number of Bees") +
  xlim(0, 100) +
  scale_color_manual(values=c('goldenrod','dodgerblue', 'forestgreen')) +
  facet_wrap(~ Sex) +
  practical_theme()

#THESE PLOTS SORT OF WORK BUT LOOK MESSY/CONFUSING
av_11f_urbanplot <- ggplot(av.dist11.f, aes(x = Urban_Pct, y = Count)) +
  geom_point(size = 3, shape = 21, fill = "slategrey", colour = "white", alpha = 0.35) +
  geom_smooth(method = "glm.nb", colour = "goldenrod", fill = "goldenrod") +
  xlab("Urbanization (% cover)") +
  ylab("Abundance of Bees") +
  #labs(x = "Urbanization (% cover)", y = "Number of Bees") +
  xlim(0, 100) +
  ylim(0, 80) +
  ggtitle("Female") +
  practical_theme() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))
av_11m_urbanplot <- ggplot(av.dist11.m, aes(x = Urban_Pct, y = Count)) +
  geom_point(size = 3, shape = 21, fill = "slategrey", colour = "white", alpha = 0.35) +
  geom_smooth(method = "glm.nb", colour = "dodgerblue", fill = "dodgerblue") +
  xlab("Urbanization (% cover)") +
  ylab("Abundance of Bees") +
  #labs(x = "Urbanization (% cover)", y = "Number of Bees") +
  xlim(0, 100) +
  ylim(0, 80) +
  ggtitle("Male") +
  practical_theme() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))
av_11total_urbanplot <- ggplot(av.dist11.total, aes(x = Urban_Pct, y = Count)) +
  geom_point(size = 3, shape = 21, fill = "slategrey", colour = "white", alpha = 0.35) +
  geom_smooth(method = "glm.nb", colour = "purple4", fill = "purple4") +
  xlab("Urbanization (% cover)") +
  ylab("Abundance of Bees") +
  #labs(x = "Urbanization (% cover)", y = "Number of Bees") +
  xlim(0, 100) +
  ylim(0, 80) +
  ggtitle("All") +
  practical_theme() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))

#PATCHWORK IS IGNORING ALL COMMANDS RE AXIS TITLES...:(
av_11f_urbanplot + av_11m_urbanplot + av_11total_urbanplot + plot_layout(axis_titles = "collect") + plot_annotation(
  title = "Season-Total Bees: 2011", theme = theme(plot.title = element_text(size = 16, face = "bold")))

# group_colours <- c(Female = "goldenrod", Male = "dodgerblue", Bee_Total = "forestgreen")
# group_fills <-  c(Female = "goldenrod", Male = "dodgerblue", Bee_Total = "forestgreen")
# 
# ggplot(av.dist11.trim, aes(x = Urban_Pct, y = Count)) +
#   geom_point(size = 4, shape = 21, alpha = 0.5) +
#   geom_smooth(method = "glm.nb") +
#   labs(x = "Urbanization Class (% cover)", y = "Number of Bees") +
#   practical_theme() +
#   facet_wrap(~ Sex) +
#   scale_colour_manual(values = group_colours) +
#   scale_fill_manual(values = group_fills)

# ggplot(av.dist11, aes(x = Urban_Pct, y = Count)) +
#   geom_point(size = 4, shape = 21, alpha = 0.3) +
#   geom_smooth(method = "glm.nb") +
#   labs(x = "Urbanization (% cover)", y = "Number of Bees") +
#   xlim(0, 100) +
#   practical_theme() +
#   facet_wrap(~ Sex) +
#   scale_color_manual(values=c('#669999','#a3c2c2', '#b30059'))
# 
# library(ggplot2)
# ggplot(mtcars, aes(mpg, disp)) +
#   geom_point(aes(shape = factor(gear), color = factor(cyl))) +
#   facet_grid(~ cyl) +
#   scale_color_manual(values = c("red", "green4", "blue"))

####Plotting urbanization (continuous) for both years, split by sexes####
(av_10f_urbanplot + av_10m_urbanplot + av_10total_urbanplot + plot_layout(axis_titles = "collect"))/(av_11f_urbanplot + av_11m_urbanplot + av_11total_urbanplot + plot_layout(axis_titles = "collect"))


#### Running urbanization as more of an anova (using factors) v1 ####
#https://help.xlstat.com/6741-how-interpret-contradictory-results-between-anova-and

av_urban10f_factor1 <- glm.nb(Count ~ as.factor(Urban_Pct), data = av.dist10.f)
Anova(av_urban10f_factor1)
av_urban10m_factor1 <- glm.nb(Count ~ as.factor(Urban_Pct), data = av.dist10.m)
Anova(av_urban10m_factor1)
av_urban10total_factor1 <- glm.nb(Count ~ as.factor(Urban_Pct), data = av.dist10.total)
Anova(av_urban10total_factor1)

av.urban10 <- av.dist10 %>% 
  dplyr::filter(!Sex == "Bee_Total")

av_urban10_factmod1 <- glm.nb(Count ~ as.factor(Urban_Pct)*Sex, data = av.urban10)
av_urban10_factmod2 <- glm.nb(Count ~ as.factor(Urban_Pct), data = av.urban10)
AICtab(av_urban10_factmod1, av_urban10_factmod2)
Anova(av_urban10_factmod2)
emmeans(av_urban10_factmod2, ~ as.factor(Urban_Pct), type = "response")
emmeans(av_urban10_factmod2, pairwise ~ as.factor(Urban_Pct), type = "response")

plot_urban10_factmod2 <- as_tibble(emmeans(av_urban10_factmod2, ~ as.factor(Urban_Pct), type = "response"))
urbanfacts_2010_plot <- ggplot(plot_urban10_factmod2, aes(x = as.factor(Urban_Pct), y = response, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_point(size = 4, colour = "purple4") +
  geom_errorbar(width = 0.1, colour = "purple4") +
  xlab("Urbanization (% cover)") +
  ylab("Average Abundance of Bees") +
  ylim(0, 30) +
  ggtitle("2010") +
  practical_theme() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))#,
        #aspect.ratio = 1)

av_urban11f_factor1 <- glm.nb(Count ~ as.factor(Urban_Pct), data = av.dist11.f)
Anova(av_urban11f_factor1)
av_urban11m_factor1 <- glm.nb(Count ~ as.factor(Urban_Pct), data = av.dist11.m)
Anova(av_urban11m_factor1)
av_urban11total_factor1 <- glm.nb(Count ~ as.factor(Urban_Pct), data = av.dist11.total)
Anova(av_urban11total_factor1)

av.urban11 <- av.dist11 %>% 
  dplyr::filter(!Sex == "Bee_Total")

av_urban11_factmod1 <- glm.nb(Count ~ as.factor(Urban_Pct)*Sex, data = av.urban11)
av_urban11_factmod2 <- glm.nb(Count ~ as.factor(Urban_Pct), data = av.urban11)
AICtab(av_urban11_factmod1, av_urban11_factmod2)
Anova(av_urban11_factmod2)
emmeans(av_urban11_factmod2, ~ as.factor(Urban_Pct), type = "response")
emmeans(av_urban11_factmod2, pairwise ~ as.factor(Urban_Pct), type = "response")

plot_urban11_factmod2 <- as_tibble(emmeans(av_urban11_factmod2, ~ as.factor(Urban_Pct), type = "response"))
urbanfacts_2011_plot <- ggplot(plot_urban11_factmod2, aes(x = as.factor(Urban_Pct), y = response, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_point(size = 4, colour = "purple4") +
  geom_errorbar(width = 0.1, colour = "purple4") +
  xlab("Urbanization (% cover)") +
  ylab("Average Abundance of Bees") +
  ylim(0, 30) +
  ggtitle("2011") +
  practical_theme() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))#,
        #aspect.ratio = 1)

urbanfacts_2010_plot + urbanfacts_2011_plot + plot_layout(axis_titles = "collect")

#### what about checking both years together, with urbanization as a factor? v1 ####
av.distcent.counts2b
av_urban_factmod1 <- glm.nb(Bee_Subtotal ~ as.factor(Urban_Pct), data = av.distcent.counts2b)
av_urban_factmod2 <- glm.nb(Bee_Subtotal ~ as.factor(Urban_Pct)*Sex, data = av.distcent.counts2b)
av_urban_factmod5 <- glm.nb(Bee_Subtotal ~ as.factor(Urban_Pct)*Year, data = av.distcent.counts2b)

AICtab(av_urban_factmod1, av_urban_factmod2, av_urban_factmod3)
Anova(av_urban_factmod1)
emmeans(av_urban_factmod1, ~ as.factor(Urban_Pct), type = "response")
emmeans(av_urban_factmod1, pairwise ~ as.factor(Urban_Pct), type = "response")

plot_urban_factmod1 <- as_tibble(emmeans(av_urban_factmod1, ~ as.factor(Urban_Pct), type = "response"))
ggplot(plot_urban_factmod1, aes(x = as.factor(Urban_Pct), y = response, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_point(size = 4, colour = "purple4") +
  geom_errorbar(width = 0.1, colour = "purple4") +
  xlab("Urbanization (% cover)") +
  ylab("Average Abundance of Bees Captured") +
  practical_theme() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"))


# #also checking 2011 data with smart farm removed
# av.urban11a <- av.dist11 %>% 
#   dplyr::filter(!Sex == "Bee_Total") %>% 
#   dplyr::filter(!Location_Name == "Smart Farm")
# 
# av_urban11_factmod1a <- glm.nb(Count ~ as.factor(Urban_Pct)*Sex, data = av.urban11a)
# Anova(av_urban11_factmod1a)
# emmeans(av_urban11_factmod1a, ~ as.factor(Urban_Pct)*Sex, type = "response")
# plot_urban11_factmod1a <- as_tibble(emmeans(av_urban11_factmod1a, ~ as.factor(Urban_Pct)*Sex, type = "response"))
# ggplot(plot_urban11_factmod1a, aes(x = as.factor(Urban_Pct), y = response, ymin = asymp.LCL, ymax = asymp.UCL,
#                                   colour = Sex)) +
#   geom_point(size = 4) +
#   geom_errorbar(width = 0.2) +
#   practical_theme() +
#   facet_wrap( ~ Sex)


#### maybe a funky outlier in 2011? ####

av.dist11.trim <- av.dist11 %>% 
  dplyr::filter(Distance < 40000)

ggplot(av.dist11.trim, aes(x = Distance, y = Count)) +
  geom_point(size = 4, shape = 21, fill = "slategrey", colour = "white", alpha = 0.5) +
  geom_smooth(method = "glm.nb", colour = "darkblue", fill = "darkblue") +
  labs(x = "Distance from CHI center", y = "Number of Bees") +
  practical_theme() +
  facet_wrap(~ Sex)

av.dist11.f.trim <- av.dist11.trim %>% 
  dplyr::filter(Sex == "Female")
av.dist11.m.trim <- av.dist11.trim %>% 
  dplyr::filter(Sex == "Male")
av.dist11.total.trim <- av.dist11.trim %>% 
  dplyr::filter(Sex == "Bee_Total")

av_dist11f_mod1trim <- glm.nb(Count ~ Distance, data = av.dist11.f.trim)
av_dist11m_mod1trim <- glm.nb(Count ~ Distance, data = av.dist11.m.trim)
av_dist11total_mod1trim <- glm.nb(Count ~ Distance, data = av.dist11.total.trim)

Anova(av_dist11f_mod1trim)
Anova(av_dist11m_mod1trim)
Anova(av_dist11total_mod1trim)

ggplot(av.dist11.trim, aes(x = Long, y = Count)) +
  geom_point(size = 4, shape = 21, fill = "slategrey", colour = "white", alpha = 0.5) +
  geom_smooth(method = "glm.nb", colour = "darkblue", fill = "darkblue") +
  labs(x = "Longitude", y = "Number of Bees") +
  practical_theme() +
  facet_wrap(~ Sex)
ggplot(av.dist11.trim, aes(x = Lat, y = Count)) +
  geom_point(size = 4, shape = 21, fill = "slategrey", colour = "white", alpha = 0.5) +
  geom_smooth(method = "glm.nb", colour = "darkblue", fill = "darkblue") +
  labs(x = "Latitude", y = "Number of Bees") +
  practical_theme() +
  facet_wrap(~ Sex)

av.dist11total_mod2 <- glm.nb(Count ~ Lat, data = av.dist11.total.trim)
av.dist11total_mod2a <- glm.nb(Count ~ Long, data = av.dist11.total.trim)
av.dist11total_mod2b <- glm.nb(Count ~ Lat*Long, data = av.dist11.total.trim)
av.dist11total_mod2c <- glm.nb(Count ~ Lat*Long*Distance, data = av.dist11.total.trim)

AICtab(av.dist11total_mod2, av.dist11total_mod2a, av.dist11total_mod2b, av.dist11total_mod2c)

Anova(av.dist11total_mod2c)

ggplot(av.dist11.trim, aes(x = Long, y = Distance)) +
  geom_point(size = 4, shape = 21, fill = "slategrey", colour = "white", alpha = 0.5) +
  geom_smooth(method = "glm", colour = "darkblue", fill = "darkblue") +
#  labs(x = "Latitude", y = "Longitude") +
  practical_theme() +
  facet_wrap(~ Sex)
ggplot(av.dist11.trim, aes(x = Lat, y = Distance)) +
  geom_point(size = 4, shape = 21, fill = "slategrey", colour = "white", alpha = 0.5) +
  geom_smooth(method = "glm", colour = "darkblue", fill = "darkblue") +
  #  labs(x = "Latitude", y = "Longitude") +
  practical_theme() +
  facet_wrap(~ Sex)

#### Clearly something is up with location, but probably not distance?!? ####

#Show iterative work on numbers for dist (or urban data once available) ie:
  #All data, nothing excluded
  #Data excluding September
  #All data with month used to standardize/find a rate

  #All data for 2011 with all locations
  #2011 data with "outlier" location excluded and note about direction-change in trend,
    #despite technical non-significance

#### Comparing urbanization against distance from city center ####

ggplot(av.distcent.counts2b, aes(x = Distance, y = Urban_Pct)) +
  geom_point(size = 4, shape = 21, fill = "slategrey", colour = "white", alpha = 0.5) +
  geom_smooth(method = "glm", colour = "darkblue", fill = "darkblue") +
  labs(x = "Distance from CHI Center (m)", y = "Urbanization Class (% cover)") +
  practical_theme()

av_distVurban_mod1 <- glm(Urban_Pct ~ Distance, data = av.distcent.counts2b)
Anova(av_distVurban_mod1)
#Check data set without smart farm point

av.distcent.counts2c <- av.distcent.counts2b %>% 
  dplyr::filter(!Location_Name == "Smart Farm")

view(av.distcent.counts2c)

av_distVurban_mod2 <- glm(Urban_Pct ~ Distance, data = av.distcent.counts2c)
Anova(av_distVurban_mod2)

ggplot(av.distcent.counts2c, aes(x = Distance, y = Urban_Pct)) +
  geom_point(size = 4, shape = 21, fill = "slategrey", colour = "white", alpha = 0.5) +
  geom_smooth(method = "glm", colour = "darkblue", fill = "darkblue") +
  labs(x = "Distance from CHI Center (m)", y = "Urbanization Class (% cover)") +
  practical_theme()

#### Running bee counts against urbanization with offsets (STUCK) ####

#Take number of months in which sampling occurred, and treat that as number of visits (per site per year)
view(av.distcent.counts2b)

av.numvisits1 <- av.distcent.counts2 %>% 
  dplyr::group_by(Location_Name, Year) %>% 
  dplyr::distinct(Month) %>% 
  tally() %>% 
  dplyr::rename(Num_Visits = n)

av.urbanoffset <- left_join(av.distcent.counts2b, av.numvisits1) %>% 
  dplyr::select(Location_Name, Distance, Urban_Pct, Year, Num_Visits, Sex, Bee_Subtotal, Bee_Total)

av_urban_mod1 <- glm.nb(Bee_Subtotal ~ Urban_Pct*Sex + offset(log(Num_Visits)), data = av.urbanoffset)
av_urban_mod1a <- glm.nb(Bee_Subtotal ~ Urban_Pct*Sex, data = av.urbanoffset, weights = Num_Visits)
av_urban_mod2 <- glm.nb(Bee_Subtotal ~ Urban_Pct*Sex, data = av.urbanoffset)
av_urban_mod3 <- glm.nb(Bee_Subtotal ~ Urban_Pct, data = av.urbanoffset)

AICtab(av_urban_mod1, av_urban_mod1a, av_urban_mod2, av_urban_mod3)

Anova(av_urban_mod1)
Anova(av_urban_mod2) 
Anova(av_urban_mod3)

emmeans(av_urban_mod2, ~ Urban_Pct*Sex, type = "response")
emmeans(av_urban_mod1, ~ Urban_Pct*Sex + offset(log(Num_Visits)), type = "response")
  
ggplot(av.urbanoffset, aes(x = Urban_Pct, y = Bee_Subtotal)) +
  geom_point(size = 4, shape = 21, fill = "slategrey", colour = "white", alpha = 0.5) +
  geom_smooth(method = "glm.nb", colour = "darkblue", fill = "darkblue") +
  labs(x = "Urbanization Class (% cover)", y = "Number of Bees") +
  scale_colour_manual(values = c("goldenrod", "dodgerblue")) +
  practical_theme() +
  facet_wrap( ~ Sex)


  
#### Running bee counts against urbanization, split by year ####

av.distcent.counts2c <- av.distcent.counts2b %>% 
  dplyr::filter(!Location_Name == "Smart Farm")

av.dist11.trim <- av.distcent.counts2c %>% 
  dplyr::filter(Year == "2011") %>% 
  pivot_wider(names_from = Sex,
              values_from = Bee_Subtotal) %>% 
  pivot_longer(cols = c("Bee_Total", "Male", "Female"),
               names_to = "Sex",
               values_to = "Count")
av.dist11.f.trim <- av.dist11.trim %>% 
  dplyr::filter(Sex == "Female")
av.dist11.m.trim <- av.dist11.trim %>% 
  dplyr::filter(Sex == "Male")
av.dist11.total.trim <- av.dist11.trim %>% 
  dplyr::filter(Sex == "Bee_Total")

av_urban11f_trim_mod1 <- glm.nb(Count ~ Urban_Pct, data = av.dist11.f.trim)
Anova(av_urban11f_mod1)
av_urban11m_trim_mod1 <- glm.nb(Count ~ Urban_Pct, data = av.dist11.m.trim)
Anova(av_urban11m_mod1)
av_urban11total_trim_mod1 <- glm.nb(Count ~ Urban_Pct, data = av.dist11.total.trim)
Anova(av_urban11total_mod1)

ggplot(av.dist11.trim, aes(x = Urban_Pct, y = Count)) +
  geom_point(size = 4, shape = 21, fill = "slategrey", colour = "white", alpha = 0.5) +
  geom_smooth(method = "glm.nb", colour = "darkblue", fill = "darkblue") +
  labs(x = "Urbanization Class (% cover)", y = "Number of Bees") +
  practical_theme() +
  facet_wrap(~ Sex)

# group_colours <- c(Female = "goldenrod", Male = "dodgerblue", Bee_Total = "forestgreen")
# group_fills <-  c(Female = "goldenrod", Male = "dodgerblue", Bee_Total = "forestgreen")
# 
# ggplot(av.dist11.trim, aes(x = Urban_Pct, y = Count)) +
#   geom_point(size = 4, shape = 21, alpha = 0.5) +
#   geom_smooth(method = "glm.nb") +
#   labs(x = "Urbanization Class (% cover)", y = "Number of Bees") +
#   practical_theme() +
#   facet_wrap(~ Sex) +
#   scale_colour_manual(values = group_colours) +
#   scale_fill_manual(values = group_fills)

#### Modeling urbanization with NAs instead of 0s added (STUCK--MODEL NON-CONVERGENCE) ####
#what happens if you use NA instead of 0? (looking just at Urbanization, not phenology)
av.distcent.counts3 <- av.distcent.counts %>%
  pivot_wider(names_from = Sex, values_from = n, values_fill = 0) %>%
  dplyr::rename(Male = M) %>% 
  dplyr::rename(Female = F) %>% 
  pivot_longer(cols = c("Male","Female"),
               names_to = "Sex",
               values_to = "Bee_Count") %>% 
  pivot_wider(names_from = Month, values_from = Bee_Count, values_fill = 0) %>% 
  pivot_longer(cols = c("June", "July", "August", "September"),
               names_to = "Month",
               values_to = "Bee_Count") %>% 
  dplyr::filter(!(Year == "2011" & Month == "September"))

av.distcent.counts3a <- av.distcent.counts3 %>%
  dplyr::mutate(DistBuff = Distance - 500) %>% 
  dplyr::group_by(Location_Name, Lat, Long, Year, Distance, DistBuff, Urban_Pct, Sex) %>% 
  dplyr::mutate(Bee_Subtotal = sum(Bee_Count)) %>% 
  dplyr::select(Location_Name, Lat, Long, Distance, DistBuff, Urban_Pct, Year, Sex, Bee_Subtotal) %>% 
  dplyr::distinct()

av.distcent.counts3b <- av.distcent.counts3a %>%
  dplyr::group_by(Location_Name, Year) %>% 
  dplyr::mutate(Bee_Total = sum(Bee_Subtotal))

av.distcent.counts3c <- av.distcent.counts3b %>%
  mutate_if(is.numeric, ~replace(., . == 0, NA))

av.urb10 <- av.distcent.counts3c %>% 
  dplyr::filter(Year == "2010") %>% 
  pivot_wider(names_from = Sex,
              values_from = Bee_Subtotal) %>% 
  pivot_longer(cols = c("Bee_Total", "Male", "Female"),
               names_to = "Sex",
               values_to = "Count")
av.urb10.f <- av.urb10 %>% 
  dplyr::filter(Sex == "Female")
av.urb10.m <- av.urb10 %>% 
  dplyr::filter(Sex == "Male")
av.urb10.total <- av.urb10 %>% 
  dplyr::filter(Sex == "Bee_Total")

av.urb11 <- av.distcent.counts3c %>% 
  dplyr::filter(Year == "2011") %>% 
  pivot_wider(names_from = Sex,
              values_from = Bee_Subtotal) %>% 
  pivot_longer(cols = c("Bee_Total", "Male", "Female"),
               names_to = "Sex",
               values_to = "Count")
av.urb11.f <- av.urb11 %>% 
  dplyr::filter(Sex == "Female")
av.urb11.m <- av.urb11 %>% 
  dplyr::filter(Sex == "Male")
av.urb11.total <- av.urb11 %>% 
  dplyr::filter(Sex == "Bee_Total")

av_urb10f_mod1 <- glm.nb(Count ~ Urban_Pct, data = av.urb10.f)
Anova(av_urb10f_mod1)
av_urb10m_mod1 <- glm.nb(Count ~ Urban_Pct, data = av.urb10.m)
Anova(av_urb10m_mod1)
av_urb10total_mod1 <- glm.nb(Count ~ Urban_Pct, data = av.urb10.total)
Anova(av_urb10total_mod1)

ggplot(av.urb10, aes(x = Urban_Pct, y = Count)) +
  geom_point(size = 4, shape = 21, fill = "slategrey", colour = "white", alpha = 0.5) +
  geom_smooth(method = "glm.nb", colour = "darkblue", fill = "darkblue") +
  labs(x = "Urbanization (% cover)", y = "Number of Bees") +
  xlim(0, 100) +
  practical_theme() +
  facet_wrap(~ Sex)
