### Italy mobility data:

## Load mobility data (in logic because quite a large file):
if(!exists("mobility")){
  library(readr)
  mobility <- read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
  mobility$country_region[mobility$country_region == "The Bahamas"] <- "Bahamas"
  mobility$country_region[mobility$country_region == "Côte d'Ivoire"] <- "Cote d'Ivoire"
  mobility$country_region[mobility$country_region == "Cape Verde"] <- "Cabo Verde"
  mobility$country_region[mobility$country_region == "Myanmar (Burma)"] <- "Myanmar"
  mobility$country_region[mobility$country_region == "Antigua and Barbuda"] <- "Antigua & Barbuda"
  mobility$country_region[mobility$country_region == "United Kingdom"] <- "Britain"
  mobility$country_region[mobility$country_region == "Trinidad and Tobago"] <- "Trinidad & Tobago"
  mobility <- data.frame(mobility)
  mobility$date <- as.Date(mobility$date)
}

# Function to prep mobility data:
prep_mobility <- function(dat, collapse.regions = T){
  
  # Get region identifier:
  dat$id <- paste0(dat$country_region, "_", dat$sub_region_1, "_", dat$sub_region_2, "_", dat$metro_area)
  
  if(collapse.regions){
  # Take means within regions-dates:
  for(i in grep("change_from_baseline", colnames(dat), value = T)){
    dat[, i] <- ave(dat[, i], paste0(dat$id, "_", as.numeric(dat$date)), FUN = function(x) mean(x, na.rm = T))
  }
  
  # Get unique region-dates:
  dat <- dat[!duplicated(paste0(dat$id, "_", as.numeric(dat$date))), ]
  }
  
  # Calculate mobility for a few combined categories:
  dat$mob_all_except_residential_change_from_baseline <- rowMeans(dat[, c(
    "retail_and_recreation_percent_change_from_baseline",
    "grocery_and_pharmacy_percent_change_from_baseline",
    "parks_percent_change_from_baseline",
    "transit_stations_percent_change_from_baseline",
    "workplaces_percent_change_from_baseline")], na.rm = T)
  dat$mob_transit_work_change_from_baseline <- rowMeans(dat[, c("transit_stations_percent_change_from_baseline",
                                                                "workplaces_percent_change_from_baseline")], na.rm = T)
  dat$mob_transit_work_grocert_pharmacy_retail_change_from_baseline <- rowMeans(dat[, c(
    "retail_and_recreation_percent_change_from_baseline",
    "grocery_and_pharmacy_percent_change_from_baseline",
    "transit_stations_percent_change_from_baseline",
    "workplaces_percent_change_from_baseline")], na.rm = T)
 
   # Sort by date
  dat <- dat[order(dat$date), ]
  
  # Calculate 7-day moving average
  fun_7dma <- function(x){
    unlist(lapply(1:length(x), FUN = function(i){
      mean(x[max(c(1,i-3)):min(c(length(x), i+3))], na.rm = T)
    }))
  }
  
  dat <- data.frame(dat)
  for(i in grep("baseline", colnames(dat), value = T)){
    dat[, paste0(i, "_7dma")] <- ave(dat[, i], dat$id, FUN = function(x) fun_7dma(x))
  }
  
  # Remove if missing:
  #  for(j in grep("baseline", colnames(mobility))){
  #    dat[is.na(dat[, j]), grep("baseline", setdiff(colnames(dat), colnames(mobility)), value = T)] <- NA
  #}
  
  
  # Plot for level below national:
  dat$level <- "National"
  dat$level[!is.na(dat$sub_region_1) & is.na(dat$sub_region_2)] <- "Sub-national"
  dat$level[!is.na(dat$sub_region_2)] <- "Sub-sub-national"
  
  return(dat)}

# Restrict to a few countries:
dat <- prep_mobility(mobility[mobility$country_region %in% c("Italy", "Britain", "Spain", "France", "Germany", "Sweden", "Denmark"), ])

# Note: We here collapsing areas within regions through a simple average - below we take the proper average by population, which depends on identifying the superregions by iso code.

# Plot these countries:
library(ggplot2)
ggplot(dat[dat$level == "Sub-national", ], aes(x=date, y=mob_transit_work_grocert_pharmacy_retail_change_from_baseline_7dma, col = sub_region_1))+geom_line()+facet_grid(.~country_region)+theme_minimal()+theme(legend.position = "none")+ylab("Mobility, change from baseline\n(excepting parks and residencies)")+xlab("")
ggsave("plots/mobility_in_italy_and_comparative_countries.png", width = 6, height = 4)
write.csv(dat, "output-data/mobility_in_italy_and_comparative_countries.csv")

# Restrict to a Lombardy:
lom <- prep_mobility(mobility[mobility$sub_region_1 == "Lombardy", ],
                     collapse.regions = FALSE)

# Calculate 7-day moving average by iso:
fun_7dma <- function(x){
  unlist(lapply(1:length(x), FUN = function(i){
    mean(x[max(c(1,i-3)):min(c(length(x), i+3))], na.rm = T)
  }))
}
lom[, "mob_transit_work_grocert_pharmacy_retail_change_from_baseline_7dma"] <- ave(lom[, "mob_transit_work_grocert_pharmacy_retail_change_from_baseline"], lom$iso_3166_2_code, FUN = function(x) fun_7dma(x))
lom <- lom[!is.na(lom$iso_3166_2_code), ]

# Dropping Sondrio province, which unfortunately had extensive missing data problems:
lom <- lom[lom$iso_3166_2_code != "IT-SO", ]

ggplot(lom, aes(x=date, y=mob_transit_work_grocert_pharmacy_retail_change_from_baseline_7dma, col = iso_3166_2_code))+geom_line()+theme_minimal()+theme(legend.position = "bottom", legend.title = element_blank())+xlab("Mobility")+ylab("Mobility compared to baseline")+geom_line(data = lom[lom$iso_3166_2_code == "IT-BG", ], size =2, alpha= 0.5)
ggsave("plots/mobility_in_lombardy.png", width = 6, height = 4)

write.csv(lom, "output-data/mobility_in_lombardy.csv")

# Calculate average for Italian regions except Lombardy (iso restriction selects region averages):
it <- prep_mobility(mobility[mobility$country_region == "Italy" & mobility$iso_3166_2_code %in%
                               c("IT-21", "IT-23", "IT-25", "IT-32", "IT-34", "IT-36", "IT-42", "IT-45",
                                 "IT-52", "IT-55", "IT-57", "IT-62", "IT-65", "IT-67", "IT-72", "IT-75",
                                 "IT-77", "IT-78", "IT-82", "IT-88"), ], collapse.regions = F)

# Check to make sure this is all 15 regions and 5 autonomous regions:
if(length(unique(it$sub_region_1)) != 20){
  stop("missing regions")
}

# Plot by provinces:
ggplot(it[, ], aes(x=date, y=mob_transit_work_grocert_pharmacy_retail_change_from_baseline_7dma, col = sub_region_1))+geom_line()+geom_line(data = it[it$iso_3166_2_code == "IT-25", ], size = 2, alpha = 0.2)+theme_minimal()+theme(legend.position = "bottom", legend.title = element_blank())+xlab("Mobility")+ylab("Mobility compared to baseline\n(Lombardy highlighted)")
ggsave("plots/mobility_in_italy.png", width = 10, height = 5)

# Add population in millions:
it$pop <- NA
it$pop[it$sub_region_1 == "Lombardy"] <- 10.06
it$pop[it$sub_region_1 == "Lazio"] <- 5.88
it$pop[it$sub_region_1 == "Campania"] <- 5.80 
it$pop[it$sub_region_1 == "Sicily"] <- 5.00
it$pop[it$sub_region_1 == "Veneto"] <- 4.90
it$pop[it$sub_region_1 == "Emilia-Romagna"] <- 4.46
it$pop[it$sub_region_1 == "Piedmont"] <- 4.36
it$pop[it$sub_region_1 == "Apulia"] <- 4.03
it$pop[it$sub_region_1 == "Tuscany"] <- 3.73
it$pop[it$sub_region_1 == "Calabria"] <- 1.95
it$pop[it$sub_region_1 == "Sardinia"] <- 1.64
it$pop[it$sub_region_1 == "Liguria"] <- 1.55
it$pop[it$sub_region_1 == "Marche"] <- 1.53
it$pop[it$sub_region_1 == "Abruzzo"] <- 1.31
it$pop[it$sub_region_1 == "Friuli-Venezia Giulia"] <- 1.21
it$pop[it$sub_region_1 == "Trentino-South Tyrol"] <- 1.07
it$pop[it$sub_region_1 == "Umbria"] <- 0.88
it$pop[it$sub_region_1 == "Basilicata"] <- 0.56
it$pop[it$sub_region_1 == "Molise"] <- 0.31
it$pop[it$sub_region_1 == "Aosta"] <- 0.13

it_except_lom <- it[it$sub_region_1 != "Lombardy" & !is.na(it$sub_region_1), ]
total_pop_except_lom <- sum(it_except_lom[it_except_lom$date == as.Date("2020-06-01"), "pop"])

# Calculate population-weighted average:
for(i in grep("change_from_baseline", colnames(it_except_lom), value = T)){
  #  it_except_lom[, paste0(i, "_raw_mean")] <- ave(it_except_lom[, i], it_except_lom$date, FUN = mean)
  it_except_lom[, i] <- ave(it_except_lom[, i]*it_except_lom$pop/total_pop_except_lom, it_except_lom$date, FUN = sum)
}

it_except_lom$sub_region_1 <- it_except_lom$iso_3166_2_code <- "Italy, excepting Lombardy"
it_except_lom <- it_except_lom[!duplicated(it_except_lom$date), ]

ggplot(it_except_lom, aes(x=date, col = sub_region_1))+geom_line(aes(y=mob_transit_work_grocert_pharmacy_retail_change_from_baseline_7dma, ))+theme_minimal()+theme(legend.position = "bottom", legend.title = element_blank())+xlab("Mobility")+ylab("Mobility compared to baseline")

# Merge this into lombardy dataset:
lom_plus <- rbind(lom, it_except_lom[, colnames(lom)])
lom_plus$name <- lom_plus$iso_3166_2_code
lom_plus$name[lom_plus$iso_3166_2_code == "IT-BG"] <- "Bergamo"

write.csv(lom_plus, "output-data/mobility_in_lombardy_plus_elsewhere_in_italy.csv")
library(readr)
lom_plus <- read_csv("output-data/mobility_in_lombardy_plus_elsewhere_in_italy.csv")
ggplot(lom_plus, aes(x=date, y=mob_transit_work_grocert_pharmacy_retail_change_from_baseline_7dma, 
                     group = name,
                     col = "Lombardian provinces"))+
  geom_line()+
  geom_line(data = lom_plus[lom_plus$name == "Bergamo", ], aes(col = "Bergamo"), size = 2, alpha = 0.5)+
  geom_line(data = lom_plus[lom_plus$name == "Italy, excepting Lombardy", ], aes(col = "Italy, population-weighted average outside Lombardy"), size = 2, alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_blank())+
  ylab("Mobility compared to baseline")+
  ggtitle("People stopped moving around in all parts of Lombardy, regardless of outbreak severity")
ggsave("plots/mobility_in_lombardy_plus_elsewhere_in_italy.png", width = 6, height = 4)

# Number quoted in text:
mean(it$mob_transit_work_grocert_pharmacy_retail_change_from_baseline_7dma[it$iso_3166_2_code == "IT-25" & it$date >= as.Date("2020-07-01") & it$date < as.Date("2020-08-01")], na.rm = T) # decline in Lombardy