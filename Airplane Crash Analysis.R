#Aircrash Analysis R code ( Report 2 Code )
#importing all required libraries
library(tidyverse) # to manipulate data
library(dplyr) # to manipulate data
library(ggplot2) # to plot graph
library(lubridate) # to manipulate as date
library(gridExtra) # to arrange multiple grid based plots on a page
library(readr) # to read functions to import xls file / to read flat/tabular text files
library(stringr) # to use regx and other string functions
library(tidyverse) # to manipulate data
library(RColorBrewer)# to have nice color palettes
library(tm) # to perform text mining operations (for wordcloud here)
library(caret) # to spilt data and and select featured data
library(plotly) # makes interactive, publication-quality graphs
library(timetk) #to work with time frame graphs
library(tidytext) # sentiment analysis algorithms
library(corrplot) # graphical display of a correlation matrix
library(ggpubr) # provides some easy-to-use functions for creating and customizing
library(PerformanceAnalytics) # Econometric tools for performance and risk analysis
library(plotrix) # getting many sorts of specialized plots
#loading dataset
master_air_crash_df <-
 read_csv(

"C:\\StFX\\Bigdata\\Project1\\Airplane_Crashes_and_Fatalities_Since_1908_201908201056
39.csv"
 )
air_crash_df = master_air_crash_df
#data cleaning
#date conversion
air_crash_df$Crash_Date <- strptime(air_crash_df$Date, "%m/%d/%Y")
air_crash_df$Crash_Date <- as.Date(air_crash_df$Crash_Date)
dates <- air_crash_df$Crash_Date
air_crash_df$Day <- day(dates)
air_crash_df$Month <- month(dates)
air_crash_df$Year <- year(dates)
str(air_crash_df)
air_crash_df$Location <- sapply(air_crash_df$Location, as.character)
air_crash_df$LocationCountry <-
 gsub(".*,", "", air_crash_df$Location)
#remove white space at beginning
air_crash_df$LocationCountry <-
 str_trim(air_crash_df$LocationCountry, side = "both")
#Convert string back to factors
air_crash_df$LocationCountry <-
 sapply(air_crash_df$LocationCountry, as.factor)
#convert columns char to int
air_crash_df$Month <- sapply(air_crash_df$Month, as.numeric)
air_crash_df$Day <- sapply(air_crash_df$Day, as.numeric)
air_crash_df$Year <- sapply(air_crash_df$Year, as.character)
air_crash_df$Aboard <- sapply(air_crash_df$Aboard, as.numeric)
air_crash_df$`Aboard Crew` <-
 sapply(air_crash_df$`Aboard Crew`, as.numeric)
air_crash_df$`Aboard Passangers` <-
 sapply(air_crash_df$`Aboard Passangers`, as.numeric)
air_crash_df$Fatalities <-
 sapply(air_crash_df$Fatalities, as.numeric)
air_crash_df$`Fatalities Passangers` <-
 sapply(air_crash_df$`Fatalities Passangers`, as.numeric)
air_crash_df$`Fatalities Crew` <-
 sapply(air_crash_df$`Fatalities Crew`, as.numeric)
#start of Figure 1 correlation matrix code
air_crash_df$Year <- sapply(air_crash_df$Year, as.numeric)
selected_columns_df <- air_crash_df %>%
 select(
 Month,
 Day,
 Year,
 Aboard,
 `Aboard Passangers`,
 `Aboard Crew`,
 Fatalities,
 `Fatalities Passangers`,
 `Fatalities Crew`
 )
selected_columns_df = na.omit(selected_columns_df)
selected_columns_df.cor = cor(selected_columns_df, method = c("spearman"))
corrplot(selected_columns_df.cor) + title("Figure 1.Correlation Matrix ")
#end of figure 1 correlation matrix code
#start of Figure 2 code for the Use of chart.Correlation(): Draw scatter plots
selected_columns_df <- air_crash_df %>%
 select(Aboard, Fatalities)
selected_columns_df = na.omit(selected_columns_df)
selected_columns_df$Aboard <-
 sapply(selected_columns_df$Aboard, as.numeric)
selected_columns_df$Fatalities <-
 sapply(selected_columns_df$Fatalities, as.numeric)
chart.Correlation(selected_columns_df, histogram = TRUE, pch = 19)
+ title("Figure 2. Corr b/w Aboard and Fatalities ")
#end of Figure 2 Use chart.Correlation(): Draw scatter plots
#start of Figure 3 crash trend yearly and monthly
years <- as.data.frame(table(air_crash_df$Year))
years_graph <- years %>%
 ggplot(aes(
 y = Freq,
 x = Var1,
 color = Freq,
 group = 1
 )) +
 geom_line(size = 1,
 linetype = 1,
 color = "Navy") +
 geom_smooth() +
 geom_point(size = 3, shape = 20) +
 xlab("Years") + ylab("Crashes") +
 scale_x_discrete(breaks = seq(from = 1908, to = 2019, by = 10)) +
 ggtitle("Figure 3. Total number of crashes per year") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
months <- as.data.frame(table(air_crash_df$Month))
months_graph <- months %>%
 ggplot(aes(Var1, Freq, fill = Var1)) +
 geom_bar(stat = "identity", width = 0.3) +
 xlab("Months") + ylab("Crashes") +
 ggtitle("Total number of crashes per month") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
grid.arrange(years_graph,
 months_graph,
 nrow = 2,
 heights = 2:1)
#end of Figure 3 code crash trend yearly and monthly
#start of Figure 4 fatality percent
Fatalities <- air_crash_df %>%
 group_by(Year) %>%
 summarise(
 total_fatalities = sum(Fatalities, na.rm = TRUE),
 total_passengers = sum(Aboard, na.rm = TRUE)
 )
Fatalities %>%
 ggplot(aes(
 y = (total_fatalities / total_passengers) * 100,
 x = Year,
 color = (total_fatalities / total_passengers) * 100,
 group = 10
 )) +
 geom_line(size = 1, linetype = 1) +
 geom_point(size = 3, shape = 20) +
 geom_smooth() +
 xlab("Years") + ylab("% Fatalities") +
 scale_x_discrete(breaks = seq(from = 1908, to = 2019, by = 10)) +
 ggtitle("Figure 4.Percent of fatalities per year") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
#end of Figure 4 fatality percent
#start of Figure 5 crash locations
Location_Crash <- air_crash_df %>%
 group_by(LocationCountry) %>%
 summarise(total_fatalities = sum(Fatalities, na.rm = TRUE)) %>%
 arrange(desc(total_fatalities))
Location_Crash <- Location_Crash[1:10, ]
Location_Crash %>%
 ggplot(aes(
 x = reorder(LocationCountry,-total_fatalities),
 y = total_fatalities,
 fill = LocationCountry
 )) +
 geom_bar(stat = "identity", width = 0.5) +
 xlab("Countries") + ylab("Number of fatalities") +
 ggtitle("Figure 5.Top 10 Countries with Maximum Fatalities") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
pie3D(
 Location_Crash$total_fatalities,
 labels = Location_Crash$LocationCountry,
 main = "Figure 6. Reason of cancelled flights"
)
#end of Figure 5 and Figure 6 code
# start of Figure 7 aircraft operators code
operator <- air_crash_df %>%
 group_by(Operator) %>%
 summarise(Freq = n()) %>%
 arrange(desc(Freq))
ggplot(operator[1:10, ], aes(
 x = reorder(factor(Operator), Freq),
 y = Freq,
 fill = Freq
)) +
 geom_bar(stat = "identity", width = 0.09) +
 xlab("Aircraft Operators") + ylab("Crashes") +
 ggtitle("Figure 7. Top 10 Aircraft Operator causing Aircrash") +
 coord_flip() +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
# end of Figure 7 code for finding the top 10 aircraft operators causing aircrashes
# start of Figure 8. aircraft type
type <- air_crash_df %>%
 group_by(`AC Type`) %>%
 summarise(Freq = n()) %>%
 arrange(desc(Freq))
ggplot(type[1:10, ], aes(
 x = reorder(factor(`AC Type`), Freq),
 y = Freq,
 fill = Freq
)) +
 geom_bar(stat = "identity", width = 0.09) +
 xlab("Types") + ylab("Crashes") +
 ggtitle("Figure 8. Top 10 Aircraft Type causing Aircrash") +
 coord_flip() +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
# end of Figure 8. code for finding the top 10 aircraft type causing aircrashes
#start of Figure 9. Code for finding highest/lowest total passengers and total fatalities count
from the year 1970 to the 2019
total_fetalities_passengers <- air_crash_df %>%
 group_by(Year) %>%
 summarize(
 total_fetalities = sum(Fatalities, na.rm = TRUE),
 total_passengers = sum(Aboard, na.rm = TRUE)
 )
total_fetalities_passengers <- total_fetalities_passengers[60:109, ]
plot_ly(x = total_fetalities_passengers$Year) %>%
 add_lines(
 y = total_fetalities_passengers$total_passengers,
 color = I("red"),
 name = " Figure 9 .Total Passengers"
 ) %>%
 add_lines(
 y = total_fetalities_passengers$total_fetalities,
 color = I("green"),
 name = "Total Fatalities"
 )
#end of figure 9 code
#start of Figure 10 code for finding the count of accidents by year for Aeroflot
crash_of_aeroflot = air_crash_df %>%
 group_by(Year) %>%
 filter(Operator == "Aeroflot") %>%
 summarise(Freq = n())
ggplot(crash_of_aeroflot, aes(y = Freq, x = Year, group = 1)) +
 geom_line(size = 1,
 linetype = 1,
 color = "Navy") +
 geom_point(size = 3, shape = 20) +
 geom_smooth() +
 xlab("Years") + ylab("Count") +
 scale_x_discrete(breaks = seq(from = 1934, to = 2019, by = 10)) +
 ggtitle("Figure 10. Count of Accidents by year (Aeroflot)") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
#end of Figure 10. Code for finding count of accidents by year of Aeroflot
#start of Figure 11 code for finding the common air crash words
#all important words
datamining <- air_crash_df %>%
 unnest_tokens(word, Summary) %>%
 filter(!word %in% stop_words$word |
 word %in% c("crashed", "aircraft", "failure")) %>%
 dplyr::count(word, sort = TRUE)
ggplot(datamining[1:20, ], aes(
 x = reorder(word,-n),
 y = n,
 alpha = n
)) +
 geom_bar(stat = "identity", fill = "green", width = 0.5) +
 xlab("crash words") + ylab("Count") +
 ggtitle("Figure 11.Common air crash words") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
# end of figure 11 code for finding the common air crash words
# start of figure 12 code for finding the common air crash sentences
#only sentences
datamining_sentences <- air_crash_df %>%
 unnest_tokens(sentences, Summary, token = "sentences") %>%
 filter(!sentences %in% stop_words$sentences) %>%
 dplyr::count(sentences, sort = TRUE)
ggplot(datamining_sentences[1:20, ],
 aes(
 x = reorder(sentences,-n),
 y = n,
 alpha = n
 )) +
 geom_bar(stat = "identity", fill = "blue", width = 0.5) +
 xlab("Count") + ylab("Crash Phrases") +
 ggtitle("Figure 12.Common air crash words") +
 coord_flip() +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
#end of figure 12code for finding the common air crash words sentences
#start of Figure 13 code for finding the aircrashes per hour.
df_time <- air_crash_df %>%
 separate(Time, c("hours", "minutes"), sep = "(:)")
df_time = na.omit(df_time)
df_time_freq <- df_time %>%
 group_by(hours) %>%
 filter(hours < 25) %>%
 dplyr::summarise(Freq = n())
ggplot(df_time_freq,
 aes(x = hours, y = Freq, alpha = Freq)) +
 geom_bar(stat = "identity", fill = "maroon", width = 0.5) +
 xlab("Hour") + ylab("Number of crashes") +
 ggtitle("Figure 13. Air crashes per hour") +
 coord_flip() +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
#end of Figure 13 code for finding the air crashes per hour
#start of figure 14 code for finding the total number of crashes per day
day <- as.data.frame(table(air_crash_df$Day))
ggplot(day, aes(y = Freq, x = Var1, group = 1)) +
 geom_line(size = 1,
 linetype = 1,
 color = "Navy") +
 geom_point(size = 3, shape = 20) +
 geom_smooth() +
 xlab("Day") + ylab("Crashes") +
 scale_x_discrete(breaks = seq(from = 1, to = 31, by = 1)) +
 ggtitle("Figure 14 .Total number of crashes per day") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
#end of figure 14 code for finding the total number of crashes per day
#start of figure 15 code for finding the time series Crashes on each day of the week
air_crash_df$Day_Name <- weekdays(as.Date(air_crash_df$Crash_Date))
crashes_day_df <- air_crash_df %>%
 group_by(Day_Name) %>%
 dplyr::summarize(CRASHES_TOTAL_DAY = n())
crashes_day_df$Day_Name <-
 factor(
 crashes_day_df$Day_Name,
 levels = c(
 "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
 )
 )
ggplot(crashes_day_df,
 aes(x = Day_Name, y = CRASHES_TOTAL_DAY, alpha =
CRASHES_TOTAL_DAY)) +
 geom_bar(stat = "identity", fill = "maroon", width = 0.5) +
 xlab("Day") + ylab("Total crashes") +
 ggtitle("Figure 15. Crashes on each day of the week") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
#end of Figure 15 code for finding the time series Crashes on each day of the week
#Extra graphs
#start of Figure 16 code for correlation between aboard and fatalities
ggscatter(
 air_crash_df,
 x = "Aboard",
 y = "Fatalities",
 add = "reg.line",
 conf.int = TRUE,
 cor.coef = TRUE,
 cor.method = "pearson",
 xlab = "Aboard",
 ylab = "Fatalities"
)
ggqqplot(air_crash_df$Aboard, ylab = "Aboard")
ggqqplot(air_crash_df$Fatalities, ylab = "Fatalities")
#end of Figure 16 code for corelation between aboard and fatalities
#start of Figure 17 Code for finding Total number of crashes per year from 1970 to 2019
years <- as.data.frame(table(air_crash_df$Year))
years <- years[60:109, ]
ggplot(years, aes(
 y = Freq,
 x = Var1,
 color = Freq,
 group = 1
)) +
 geom_line(size = 1, linetype = 1) +
 geom_point(size = 3, shape = 20) +
 geom_smooth() +
 xlab("Years") + ylab("Crashes") +
 scale_x_discrete(breaks = seq(from = 1970, to = 2019, by = 10)) +
 ggtitle("Figure 9.Total number of crashes per year from 1970 to 2019") +
 theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
#end of Figure 17 code for finding the Total number of crashes per year from 1970 to 2019
#start of Figure 18 code for time series
ts_year_df <- air_crash_df %>% count(Year)
ts_month_df <- air_crash_df %>% count(Month)
ts_day_df <- air_crash_df %>% count(Day)
ts_year_df %>%
 plot_time_series(as.numeric(Year), n)
ts_month_df %>%
 plot_time_series(as.numeric(Month), n)
ts_day_df %>%
 plot_time_series(as.numeric(Day), n)
#end of Figure 18 code for time series
#start of Figure 19 code for normal distribution
selected_columns_df <- air_crash_df %>%
 select(Month, Day, Year, Aboard, Fatalities)
selected_columns_df = na.omit(selected_columns_df)
#month
y <- dnorm(air_crash_df$Month, mean = 2.5, sd = 0.5)
plot(air_crash_df$Month, y) +
 y <- pnorm(air_crash_df$Month, mean = 2.5, sd = 2)
plot(air_crash_df$Month, y)
#end of Figure 19 code for normal distribution