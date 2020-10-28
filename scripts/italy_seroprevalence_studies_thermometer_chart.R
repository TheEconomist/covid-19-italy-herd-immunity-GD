### Italy Seroprevalence studies:

library(dplyr)
# Load latests seroprevalence studies:
# sero.sheet = "https://docs.google.com/spreadsheets/d/1kp-zWf-YcTl-HSF9YAF5sBikbI80H2qLRkR5oRsrMX4/edit#gid=0"
# sero.dat.raw <- googlesheets4::read_sheet(ss = sero.sheet, range = "InputData") %>% select(1:20)
# glimpse(sero.dat.raw)
# sero.dat <- sero.dat.raw %>%
#   filter(!is.na(country)) %>%
#   mutate(date.mid = round((as.numeric(as.Date(`date completed`)) + as.numeric(as.Date(`date started`)))/2, 0)) %>%
#   mutate(date.mid = as.Date(date.mid, origin="1970-01-01")-14) %>%
#   select(sero_id, iso3c, geo_id, country, region, geo_level, national.survey = 'national survey', date.started = 'date started', date.completed = 'date completed', date=date.mid, sample.size=N, result.pct=result, source = source) %>% filter(iso3c == "ITA") %>%
#   as.data.table
# sero.dat$region[sero.dat$region == "Castiglione D’Adda"] <- "Castiglione D'Adda"

library(readr)
sero.dat <- read_csv("source-data/sero.dat.csv")

# Classify by level of survey estimate:
sero.dat$level <- NA
sero.dat$level[sero.dat$region == "Italy"] <- "National"
sero.dat$level[!is.na(sero.dat$geo_id) & sero.dat$region != "Italy"] <- "Regions"
sero.dat$level[sero.dat$region %in% c("Varese", "Pavia", "Genova", "Savnona", "Lodi", "Bergamo", "Cremona")] <- "Northern provinces"
sero.dat$level[sero.dat$region %in% c("Milan")] <- "Metropolitan City of Milan"
sero.dat$level[sero.dat$region %in% c("Lodi Red Zone", "Castiglione D'Adda")] <- "Areas within Lodi\n(the epicenter of the epidemic)"

library(lubridate)
sero.dat$months <- NA
for(i in 1:nrow(sero.dat)){
  sero.dat$months[i] <- paste0(unique(c(as.character(lubridate::month(sero.dat$date.started[i], label = T)), 
                                        as.character(lubridate::month(sero.dat$date.completed[i], label = T)))), 
                               collapse = "-")
}

ggplot(sero.dat, aes(y= level, x=result.pct, col = level), alpha = 0.4)+
  geom_point(alpha = 0.4)+theme_minimal()+xlab("Estimated seroprevalence % with covid-19 antibodies")+
  ylab("")+theme(legend.title = element_blank(), legend.position = "none")#+ggtitle("Serosurveys in Italy\n")
ggsave("plots/seroprevalence_studies_in_italy.png", width = 6, height = 4)

ggplot(sero.dat[!sero.dat$national.survey & !sero.dat$region %in% c("Lombardia", "Bergamo"), ], aes(y= "", x=result.pct, col = level), alpha = 0.4)+
  geom_jitter(alpha = 0.4)+theme_minimal()+xlab("Seroprevalence\n(% with covid-19 antibodies)")+
  ylab("")+theme(legend.title = element_blank(), legend.position = "bottom")+
  ggtitle("Serosurveys in Italy\n")+
  geom_point(data = sero.dat[sero.dat$national.survey == TRUE, ], aes(col = "National level"), size = 2)+
  geom_point(data = sero.dat[sero.dat$region == "Lombardia", ], aes(col = "Lombardy"), size = 2)+
  geom_point(data = sero.dat[sero.dat$region == "Bergamo", ], aes(col = "Bergamo"), size = 2)+
  ggsave("plots/seroprevalence_studies_in_italy_shallow.png", width = 6, height = 2)
write.csv(sero.dat, "output-data/seroprevalence_studies_in_italy.csv")
