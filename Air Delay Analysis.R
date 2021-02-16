# Air delay analysis code ( Report_1 Code )
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(corrplot)
library(usmap)
library(ggpubr)
library(plotrix)
x <-
 c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19")
for (i in 1:length(x)) {
 current_year <- x[i]
 path <-
 paste(
 "C:\\StFX\\Bigdata\\Project1\\airline delay analysis\\20",
 current_year,
 ".csv",
 sep = ""
 )
 df <- read_csv(path)
 set.seed(1)
 sdf = df[sample(nrow(df), 200000),]
 assign(paste("sample", current_year, "data", sep = "") , sdf)
}
air_delay_df <-
 rbind(
 sample09data,
 sample10data,
 sample11data,
 sample12data,
 sample13data,
 sample14data,
 sample15data,
 sample16data,
 sample17data,
 sample18data
 )
write.csv(
 air_delay_df ,
 " C:\\StFX\\Bigdata\\Project1\\airline delay analysis\\final_master_df.csv"
)
air_delay_df <-
 read_csv(" C:\\StFX\\Bigdata\\Project1\\airline delay analysis\\final_master_df.csv")
airline_codes_df <-
 read_csv(" C:\\StFX\\Bigdata\\Project1\\airline delay analysis\\Airline_Codes_Csv.csv")
air_delay_df <-
 merge(air_delay_df,
 airline_codes_df,
 by.x = "ORIGIN",
 by.y = "CODE")
cancellation_codes_df <-
 data.frame(
 "CANCELLATION_CODE" = c("A", "B", "C", "D", NA),
 "CANCELLATION_REASON" = c("Weather", "Airlines/Carriers", "NAS", "Other", NA)
 )
air_delay_df <-
 merge(air_delay_df,
 cancellation_codes_df,
 by.x = "CANCELLATION_CODE",
 by.y = "CANCELLATION_CODE")
##################### SPLITTING DATE START ############
dates <- air_delay_df$FL_DATE
air_delay_df$FLIGHT_DAY <- day(dates)
air_delay_df$FLIGHT_MONTH <- month(dates)
air_delay_df$FLIGHT_YEAR <- year(dates)
air_delay_df$DAY_NAME <- weekdays(as.Date(air_delay_df$FL_DATE))
air_delay_df$MONTH_NAME <- month.abb[air_delay_df$FLIGHT_MONTH]
##################### SPLITTING DATE END ############
#Start of Figure 1 Causes of flight delay
###################### START OF BAR PLOT REASON FOR FILGHT DELAY
#####################
sum_carrier_delay <- sum(air_delay_df$CARRIER_DELAY, na.rm = TRUE)
sum_weather_delay <- sum(air_delay_df$WEATHER_DELAY, na.rm = TRUE)
sum_nas_delay <- sum(air_delay_df$NAS_DELAY, na.rm = TRUE)
sum_security_delay <- sum(air_delay_df$SECURITY_DELAY, na.rm = TRUE)
sum_late_aircraft_delay <-
 sum(air_delay_df$LATE_AIRCRAFT_DELAY, na.rm = TRUE)
############ STORING CATOGORIZED TOTAL DELAY IN A DATA FRAME TO PLOT
THESE REASONS ################
data <- data.frame(
 names = c("Carrier", "Weather", "NAS", "Security", "Late Aircraft") ,
 values = c(
 sum_carrier_delay,
 sum_weather_delay,
 sum_nas_delay,
 sum_security_delay,
 sum_late_aircraft_delay
 )
)
data %>%
 ggplot(aes(x = names, y = values, fill = names)) +
 geom_bar(stat = "identity",
 position = "dodge",
 width = 0.5) +
 labs(title = "Figure 1. What causes the flight delay", x = "Delay Reason", y = "Delay in
minutes") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
#End of Figure 1 Causes of flight delay
#start of Figure 2.Total delays per carrier
delay_summed_df <- air_delay_df %>%
 group_by(OP_CARRIER) %>%
 summarize(
 total_carrier_delay = sum(CARRIER_DELAY, na.rm = TRUE),
 total_weather_delay = sum(WEATHER_DELAY, na.rm = TRUE),
 total_nas_delay = sum(NAS_DELAY, na.rm = TRUE),
 total_security_delay = sum(SECURITY_DELAY, na.rm = TRUE),
 total_late_aircraft_delay = sum(LATE_AIRCRAFT_DELAY, na.rm = TRUE),
 total_delay = sum(
 total_carrier_delay,
 total_weather_delay,
 total_nas_delay,
 total_security_delay,
 total_late_aircraft_delay
 )
 )
delay_type_in_rows_df <-
 delay_summed_df %>% gather(key = delay_type,
 value = Value,
 total_carrier_delay:total_delay)
plot_six <- delay_type_in_rows_df %>%
 ggplot(aes(x = OP_CARRIER, y = Value)) +
 geom_bar(stat = "identity", position = "dodge") +
 facet_wrap(~ delay_type) +
 labs(title = "Figure 2.Total delays per carrier",
 x = "Carrier",
 y = "Delay type") +
 theme_bw()
plot_six
#End of Figure 2.Total delays per carrier
#Start of Figure 3 Code for Total Delay
##################### START FOR CREATING DATA FRAMES TO PLOT DELAY BY
DAY, MONTH AND YEAR ############
daily_delay_df <- air_delay_df %>%
 group_by(FLIGHT_DAY) %>%
 summarize(
 daily_delay = sum(
 CARRIER_DELAY,
 WEATHER_DELAY,
 NAS_DELAY,
 SECURITY_DELAY,
 LATE_AIRCRAFT_DELAY,
 na.rm = TRUE
 )
 )
daily_delay_df$FLIGHT_DAY <- factor(
 daily_delay_df$FLIGHT_DAY,
 levels = c( "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15","16",
"17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"
 )
)
daily_delay_df <- daily_delay_df[order(daily_delay_df$FLIGHT_DAY), ]
monthly_delay_df <- air_delay_df %>%
 group_by(FLIGHT_MONTH, MONTH_NAME) %>%
 summarize(
 monthly_delay = sum(
 CARRIER_DELAY,
 WEATHER_DELAY,
 NAS_DELAY,
 SECURITY_DELAY,
 LATE_AIRCRAFT_DELAY,
 na.rm = TRUE
 )
 )
yearly_delay_df <- air_delay_df %>%
 group_by(FLIGHT_YEAR) %>%
 summarize(
 yearly_delay = sum(
 CARRIER_DELAY,
 WEATHER_DELAY,
 NAS_DELAY,
 SECURITY_DELAY,
 LATE_AIRCRAFT_DELAY,
 na.rm = TRUE
 )
 )
##################### END FOR CREATING DATA FRAMES TO PLOT DELAY BY
DAY, MONTH AND YEAR ############
############ START, SORT BASED ON MONTH NAME AND TO PLOT ALL YEAR
VALUES ################
monthly_delay_df$MONTH_NAME <- factor(
 monthly_delay_df$MONTH_NAME,
 levels = c(
 "Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
 )
)
monthly_delay_df <-
 monthly_delay_df[order(monthly_delay_df$MONTH_NAME), ]
yearly_delay_df$FLIGHT_YEAR <- factor(
 yearly_delay_df$FLIGHT_YEAR,
 levels = c(
 "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"
 )
)
yearly_delay_df <-
 yearly_delay_df[order(yearly_delay_df$FLIGHT_YEAR), ]
############ END, SORT BASED ON MONTH NAME AND TO PLOT ALL YEAR
VALUES ################
plot_daily_delay <- daily_delay_df %>%
 ggplot(aes(
 x = FLIGHT_DAY,
 y = daily_delay / 1000000,
 group = 1,
 color = FLIGHT_DAY,
 size = 3
 )) +
 geom_line(size = 1,
 linetype = 1) +
 geom_smooth(size = 1) +
 geom_point(size = 3) +
 labs(title = "Figure 3. Total delay/Day",
 x = "Day",
 y = "Total delay/Million") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) + geom_line(size =
2)
plot_daily_delay
plot_monthly_delay <- monthly_delay_df %>%
 ggplot(aes(
 x = MONTH_NAME,
 y = monthly_delay / 1000000,
 group = 1,
 color = MONTH_NAME,
 size = 3
 )) +
 geom_line(size = 1,
 linetype = 1) +
 geom_smooth(size = 1) +
 geom_point(size = 3) +
 labs(title = "Total delay/Month",
 x = "Month",
 y = "Total delay/Million") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) + geom_line(size =
2)
plot_monthly_delay
plot_yearly_delay <- yearly_delay_df %>%
 ggplot(aes(
 x = FLIGHT_YEAR,
 y = yearly_delay / 1000000,
 group = 1,
 color = FLIGHT_YEAR,
 size = 3
 )) +
 geom_line(size = 1) +
 geom_smooth(size = 1) +
 geom_point(size = 3) +
 labs(title = "Total delay/Year",
 x = "Year",
 y = "Total delay/Million") + theme(panel.grid = element_blank()) + geom_line(size = 2) +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
plot_yearly_delay
plot_club_dmy <-
 ggarrange(
 plot_daily_delay,
 plot_monthly_delay,
 plot_yearly_delay,
 labels = c("A", "B", "C"),
 ncol = 1,
 nrow = 3
 ) +
 theme(panel.grid = element_blank())
plot_club_dmy
#End of Figure 3 Code for Total Delay
#Start of Figure 4. Categorized delay/Carrier
plot_categorzied_by_delay <- delay_type_in_rows_df %>%
 ggplot(aes(x = OP_CARRIER, y = Value / 60, fill = delay_type)) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Figure 4. Categorized delay/Carrier", x = "Carrier", y = "Delay/Hour") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
plot_categorzied_by_delay
#End of Figure 4. Categorized delay/Carrier
#Start of Figure 5. Total delay in each State
delay_by_state_df <- air_delay_df %>%
 group_by(STATE) %>%
 summarize(
 total_delay = sum(
 CARRIER_DELAY,
 WEATHER_DELAY,
 NAS_DELAY,
 SECURITY_DELAY,
 LATE_AIRCRAFT_DELAY,
 na.rm = TRUE
 )
 )
new_r <-
 merge(statepop, delay_by_state_df, by.x = "full", by.y = "STATE")
plot_usmap(
 data = new_r,
 values = "total_delay",
 color = "black",
 labels = TRUE
) +
 scale_fill_continuous(
 low = "#ED335F",
 high = "#761137",
 name = "Total delay",
 label = scales::comma
 ) +
 theme(legend.position = "right",
 plot.title = element_text(hjust = 0.5)) +
 labs(title = "Figure 5. Total delay in each State")
#End of Figure 5. Total delay in each State
#Start of Figure 6.Arrival vs Departure delay TAKES TIME TO RUN
air_delay_df %>%
 ggplot(aes(x = ARR_DELAY, y = DEP_DELAY)) +
 labs(title = "Figure 6.Arrival vs Departure delay",
 x = "Arraival Delay",
 y = "Departure Delay") + theme(panel.grid = element_blank()) +
 geom_point(alpha = 0.5)
#End of Figure 6.Arrival vs Departure delay
#Start of Figure 7. Total delay per carrier
##################### START OF TOTAL DELAY PER CARRIER VS NO OF FLIGHTS
OPERATED VS AVERAGE DELAY PER CARRIER #####################
flights_operated_df <- air_delay_df %>%
 count(OP_CARRIER)
plot_overall_delay_carrier <- delay_summed_df %>%
 ggplot(aes(x = OP_CARRIER, y = total_delay / 60, fill = OP_CARRIER)) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = " Figure 7. Total delay per carrier", x = "Carrier", y = "Total delay/Hour") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
plot_overall_delay_carrier
plot_flights_operated <- flights_operated_df %>%
 ggplot(aes(x = OP_CARRIER, y = n / 1000, fill = OP_CARRIER)) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = "No of flights operated/Carrier", x = "Carrier", y = "Total flights operated in
1000's") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
plot_flights_operated
plot_avg_delay <- delay_summed_df %>%
 ggplot(aes(
 x = OP_CARRIER,
 y = total_delay / flights_operated_df$n,
 fill = OP_CARRIER
 )) + geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Average delay per carrier", x = "Carrier", y = "Delay in minutes") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
plot_avg_delay
plot_club <-
 ggarrange(
 plot_overall_delay_carrier,
 plot_flights_operated,
 plot_avg_delay,
 labels = c("A", "B", "C"),
 ncol = 1,
 nrow = 3
 ) +
 labs(title = "Total delay in hours/No flights operatedr", y = "Total flights operated in 1000's") +
 theme(panel.grid = element_blank())
plot_club
#End of Figure 7. Total delay per carrier
# Start of Figure 8.Total distance travelled by carrier
delay_by_distance_df <- air_delay_df %>%
 group_by(OP_CARRIER) %>%
 summarize(
 total_disance = sum(DISTANCE),
 total_delay = sum(
 CARRIER_DELAY,
 WEATHER_DELAY,
 NAS_DELAY,
 SECURITY_DELAY,
 LATE_AIRCRAFT_DELAY,
 na.rm = TRUE
 )
 )
plot_distance_per_carrier <- delay_by_distance_df %>%
 ggplot(aes(
 x = OP_CARRIER,
 y = total_disance / 1000000,
 fill = OP_CARRIER
 )) + geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Figure 8.Total distance travelled by carrier", x = "Carrier", y =
 "Distance in millions") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
plot_distance_per_carrier
plot_club_2 <-
 ggarrange(
 plot_overall_delay_carrier,
 plot_distance_per_carrier,
 labels = c("A", "B"),
 ncol = 1,
 nrow = 2
 ) +
 theme(panel.grid = element_blank())
plot_club_2
#End of Figure 8.Total distance travelled by carrier
#Start of Figure 9.Reason of cancelled flights
Can_Reason <- air_delay_df %>%
 group_by(CANCELLATION_REASON) %>%
 summarise(COUNT_OF_CAN_REAS = n())
Can_Reason <- Can_Reason[complete.cases(Can_Reason), ]
pie3D(
 Can_Reason$COUNT_OF_CAN_REAS,
 labels = Can_Reason$CANCELLATION_REASON,
 explode = 0.1,
 theta = 1,
 start = 2,
 main = "Figure 9.Reason of cancelled flights"
)
#End of Figure 9.Reason of cancelled flights
#Start of Figure 10. No of Cancelled flights by month in 2017
options(scipen = 999)
cancellations_2017_df <- air_delay_df %>%
 filter(FLIGHT_YEAR == "2017") %>%
 group_by(MONTH_NAME) %>%
 summarize(NO_FLIGHTS_DELAYED = sum(CANCELLED))
cancellations_2017_df$MONTH_NAME <-
 factor(
 cancellations_2017_df$MONTH_NAME,
 levels = c(
 "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
 )
 )
cancellations_2017_df <-
 cancellations_2017_df[order(cancellations_2017_df$MONTH_NAME), ]
cancellations_2017_df %>%
 ggplot(aes(
 x = MONTH_NAME,
 y = NO_FLIGHTS_DELAYED,
 group = 1 ,
 color = MONTH_NAME,
 size = 2
 )) +
 labs(title = "Figure 10. No of Cancelled flights by month in 2017",
 x = "Months",
 y = "No of cancelled flights") +
 theme(panel.grid = element_blank()) +
 geom_point(alpha = 1) + theme(panel.grid = element_blank(),
 plot.title = element_text(hjust = 0.5)) + geom_line(size = 2) +
 geom_smooth(size = 1)
#End of Figure 10. No of Cancelled flights by month in 2017
#Start of Figure 11 Causes of flight delay Condition led to cancellations in airport
data <- data.frame(count(air_delay_df, vars = DEST))
data <- head(data[order(-data$n),], n = 10)
tom_cancelled_dest <- subset(air_delay_df, DEST == data$vars)
tom_cancelled_dest =
tom_cancelled_dest[!is.na(tom_cancelled_dest$CANCELLATION_REASON), ]
tom_cancelled_dest %>%
 ggplot(aes(x = DEST)) +
 labs(title = "Figure 11. Condition led to cancellations in airport",
 x = "Destinated Airport Name",
 y = "Cancelled Reason") + theme(panel.grid = element_blank()) +
 geom_bar(aes(fill = CANCELLATION_REASON)) #, position = "dodge")
#End of Figure 11. Causes of flight delay Condition led to cancellations in airport
#Start of Figure 12.Categorized delay/Day
delay_by_day_name_df <- air_delay_df %>%
 group_by(DAY_NAME) %>%
 summarize(
 carrier_delay_day = sum(CARRIER_DELAY, na.rm = TRUE),
 weather_delay_day = sum(WEATHER_DELAY, na.rm = TRUE),
 nas_delay_day = sum(NAS_DELAY, na.rm = TRUE),
 security_delay_day = sum(SECURITY_DELAY, na.rm = TRUE),
 late_aircraft_delay_day = sum(LATE_AIRCRAFT_DELAY, na.rm = TRUE),
 total_delay = sum(
 CARRIER_DELAY,
 WEATHER_DELAY,
 NAS_DELAY,
 SECURITY_DELAY,
 LATE_AIRCRAFT_DELAY,
 na.rm = TRUE
 )
 )
############ START, MERGING DELAYS IN A SINGLE COLUMN TO PLOT AND
SETTING THE LEVELS TO PLOT IN A DAY SERIES ############
delay_day_in_rows_df <-
 delay_by_day_name_df %>% gather(key = delay_type_day,
 value = Value,
 carrier_delay_day:total_delay)
delay_day_in_rows_df$DAY_NAME <-
 factor(
 delay_day_in_rows_df$DAY_NAME,
 levels = c(
 "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
 )
 )
delay_day_in_rows_df <-
 delay_day_in_rows_df[order(delay_day_in_rows_df$DAY_NAME), ]
plot_categorzied_day_delay <- delay_day_in_rows_df %>%
 ggplot(aes(
 x = DAY_NAME,
 y = Value / 1000000,
 fill = delay_type_day
 )) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Figure 12.Categorized delay/Day", x = "Day", y = "Delay in million minutes") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
plot_categorzied_day_delay
############ END, MERGING DELAYS IN A SINGLE COLUMN TO PLOT AND
SETTING THE LEVELS TO PLOT IN A DAY SERIES ############
#End of Figure 12.Categorized delay/Day
#Start of Figure 13. Day of the week delay for Texas
############ START OF DELAY PER EACH DAY FOR EACH STATE ############
delay_by_state_per_day_name <- air_delay_df %>%
 group_by(STATE, DAY_NAME) %>%
 summarize(
 carrier_delay_day_name = sum(CARRIER_DELAY, na.rm = TRUE),
 weather_delayday_name = sum(WEATHER_DELAY, na.rm = TRUE),
 nas_delayday_name = sum(NAS_DELAY, na.rm = TRUE),
 security_delayday_name = sum(SECURITY_DELAY, na.rm = TRUE),
 late_aircraft_delayday_name = sum(LATE_AIRCRAFT_DELAY, na.rm = TRUE),
 total_delay = sum(
 CARRIER_DELAY,
 WEATHER_DELAY,
 NAS_DELAY,
 SECURITY_DELAY,
 LATE_AIRCRAFT_DELAY,
 na.rm = TRUE
 )
 )
########## MERGING DELAY TYPES IN A SINGLE COLUMN FOR VISUALIZATION
#################
delay_by_state_per_day_name_row <-
 delay_by_state_per_day_name %>% gather(key = STATE_DELAY_TYPE,
 value = Value,
carrier_delay_day_name:total_delay)
############ SORTING DAYS BASED ON THE DAY OF THE WEEK
#####################
delay_by_state_per_day_name_row$DAY_NAME <-
 factor(
 delay_by_state_per_day_name_row$DAY_NAME,
 levels = c(
 "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
 )
 )
delay_by_state_per_day_name_row <-
 delay_by_state_per_day_name_row[order(delay_by_state_per_day_name_row$DAY_NAME),
]
delay_by_day_name_texas <-
 delay_by_state_per_day_name_row %>% filter(STATE == "Texas")
############# VISUALIZING WEEKDAY DELAYS FOR EACH STATE
##########################
delay_by_day_name_texas %>%
 ggplot(aes(
 x = DAY_NAME,
 y = Value / 1000000,
 fill = STATE_DELAY_TYPE
 )) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Figure 13. Day of the week delay for Texas", x = "Day", y = "Total delay/Million")
+
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
 coord_flip()
#End of Figure 13. Day of the week delay for Texas
#Start of Figure 14. Flights operated/State
state_flights_count_df <-
 air_delay_df %>% group_by(STATE) %>% count(STATE)
state_flights_count_df %>%
 ggplot(aes(x = STATE, y = n, fill = STATE)) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Figure 14. Flights operated/State", x = "State", y = "No of flights operated") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
 coord_flip()
#End of Figure 14. Flights operated/State
#Start of Figure 15. Monthly delay for Texas
############ START OF DELAY PER EACH MONTH FOR EACH STATE ############
delay_by_state_per_month <- air_delay_df %>%
 group_by(STATE, MONTH_NAME) %>%
 summarize(
 carrier_delay_month = sum(CARRIER_DELAY, na.rm = TRUE),
 weather_delay_month = sum(WEATHER_DELAY, na.rm = TRUE),
 nas_delay_month = sum(NAS_DELAY, na.rm = TRUE),
 security_delay_month = sum(SECURITY_DELAY, na.rm = TRUE),
 late_aircraft_delay_month = sum(LATE_AIRCRAFT_DELAY, na.rm = TRUE),
 total_delay = sum(
 CARRIER_DELAY,
 WEATHER_DELAY,
 NAS_DELAY,
 SECURITY_DELAY,
 LATE_AIRCRAFT_DELAY,
 na.rm = TRUE
 )
 )
########## MERGING DELAY TYPES IN A SINGLE COLUMN FOR VISUALIZATION
#################
delay_by_state_per_month_row <-
 delay_by_state_per_month %>% gather(key = delay_type_state_month,
 value = Value,
 carrier_delay_month:total_delay)
############ SETTING LEVELS TO PLOT MONTHS IN A MONTH SERIES
#####################
delay_by_state_per_month_row$MONTH_NAME <-
 factor(
 delay_by_state_per_month_row$MONTH_NAME,
 levels = c(
 "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
 )
 )
delay_by_state_per_month_row <-
 delay_by_state_per_month_row[order(delay_by_state_per_month_row$MONTH_NAME), ]
delay_by_month_texas <-
 delay_by_state_per_month_row %>% filter(STATE == "Texas")
############# VISUALIZING MONTHLY DELAYS FOR TEXAS STATE
##########################
delay_by_month_texas %>%
 ggplot(aes(x = MONTH_NAME, y = Value / 100000, fill = delay_type_state_month)) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Figure 15. Monthly delay for Texas", x = "Month", y = "Total delay/Million") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
 coord_flip()
############ END OF DELAY PER EACH MONTH FOR EACH STATE ############
#End of Figure 15. Monthly delay for Texas
d_2018 <- air_delay_df %>%
 filter(CANCELLED == "1")
hist(d_2018$FLIGHT_DAY)
hist(d_2018$FLIGHT_MONTH)
#Start of Figure 16. pie chart percentage of reason of cancelled flights
ggplot(Can_Reason,
 aes(x = "", y = COUNT_OF_CAN_REAS, fill = CANCELLATION_REASON)) +
 geom_bar(width = 1, stat = "identity") +
 coord_polar("y", start = 2)
#End of Figure 16. pie chart percentage of reason of cancelled flights