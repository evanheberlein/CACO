library(tidyverse)
library(gganimate)
library(gifski)

# Import data
# Site 1 (closest to dike): P6, Site 2: P7, Site 3: P5, Site 4: P1, Site 5 (farthest from dike): P4
P1 <- read_csv("/Users/evanheberlein/Library/CloudStorage/Box-Box/Cornell/CACO/2023/Thermistor/P1_HRE_23.asc"
               , skip = 50, col_names = c("Temperature (C)", "Pressure (PSI)", "Date (dd mmm yyyy)", "Time (HH:mm:ss)")) 
P1$datetime <- dmy_hms(paste(P1$`Date (dd mmm yyyy)`, "_", P1$`Time (HH:mm:ss)`), tz = "America/New_York")
P1 = tail(P1, -4) # Set start time to same as P4

P4 <- read_csv("/Users/evanheberlein/Library/CloudStorage/Box-Box/Cornell/CACO/2023/Thermistor/P4_HRE_23.asc"
               , skip = 50, col_names = c("Temperature (C)", "Pressure (PSI)", "Date (dd mmm yyyy)", "Time (HH:mm:ss)"))
P4$datetime <- dmy_hms(paste(P4$`Date (dd mmm yyyy)`, "_", P4$`Time (HH:mm:ss)`), tz = "America/New_York") - seconds(8) # started 2m 7-8s later than others?

P5 <- read_csv("/Users/evanheberlein/Library/CloudStorage/Box-Box/Cornell/CACO/2023/Thermistor/P5_HRE_23.asc"
               , skip = 50, col_names = c("Temperature (C)", "Pressure (PSI)", "Date (dd mmm yyyy)", "Time (HH:mm:ss)"))
P5$datetime <- dmy_hms(paste(P5$`Date (dd mmm yyyy)`, "_", P5$`Time (HH:mm:ss)`), tz = "America/New_York")
P5 = tail(P5, -4) 

P6 <- read_csv("/Users/evanheberlein/Library/CloudStorage/Box-Box/Cornell/CACO/2023/Thermistor/P6_HRE_23.asc"
               , skip = 47, col_names = c("Temperature (C)", "Pressure (PSI)", "Date (dd mmm yyyy)", "Time (HH:mm:ss)"))
P6$datetime <- dmy_hms(paste(P6$`Date (dd mmm yyyy)`, "_", P6$`Time (HH:mm:ss)`), tz = "America/New_York")
P6 = tail(P6, -4) 

P7 <- read_csv("/Users/evanheberlein/Library/CloudStorage/Box-Box/Cornell/CACO/2023/Thermistor/P7_HRE_23.asc"
               , skip = 47, col_names = c("Temperature (C)", "Pressure (PSI)", "Date (dd mmm yyyy)", "Time (HH:mm:ss)"))
P7$datetime <- dmy_hms(paste(P7$`Date (dd mmm yyyy)`, "_", P7$`Time (HH:mm:ss)`), tz = "America/New_York")
P7 = tail(P7, -4) 
# Thermistor GPS points:
T1 <- c(41.931185135204196, -70.06208609783494) # Transect 1
T2 <- c(41.93308483323645, -70.05988812565994) # Transect 2
T3 <- c(41.93732874590704, -70.05810612677863) # Transect 3
T4 <- c(41.938331240778865, -70.05742859429166) # Transect 4
T5 <- c(41.942262123604856, -70.05406343463181) # Transect 5
column.names <- c("Transect 1", "Transect 2", "Transect 3", "Transect 4", "Transect 5")
row.names <- c("Latitude", "Longitude")
Trans_GPS <- array(c(T1, T2, T3, T4, T5), dim = c(2, 5), dimnames = list(row.names, column.names))

plot(Trans_GPS["Longitude",], Trans_GPS["Latitude",])

in_water_vec = c(which(P7$`Pressure (PSI)`>0.15)[1],
                 which(P6$`Pressure (PSI)`>0.15)[1],
                 which(P5$`Pressure (PSI)`>0.15)[1],
                 which(P1$`Pressure (PSI)`>0.15)[1],
                 which(P4$`Pressure (PSI)`>0.15)[1])
i_in = max(in_water_vec)

out_water_vec = c(tail(which(P7$`Pressure (PSI)`>0.15), n=1),
                  tail(which(P6$`Pressure (PSI)`>0.15), n=1),
                  tail(which(P5$`Pressure (PSI)`>0.15), n=1),
                  tail(which(P1$`Pressure (PSI)`>0.15), n=1),
                  tail(which(P4$`Pressure (PSI)`>0.15), n=1))
i_out = min(out_water_vec)

# Divergent color scheme for temp

skipnum = 20
i_fig = seq(i_in, i_out, by = skipnum)

bardata = bind_rows(
  bind_cols(rep("T1", length(P6$`Pressure (PSI)`)), P6$`Temperature (C)`, 
            P6$`Pressure (PSI)`-mean(P6$`Pressure (PSI)`[i_fig]),  # Subtract mean from each P in range visualized
            P6$datetime, (1:1:length(P6$`Temperature (C)`)))[i_fig,],
  bind_cols(rep("T2", length(P7$`Pressure (PSI)`)), P7$`Temperature (C)`, 
            P7$`Pressure (PSI)`-mean(P7$`Pressure (PSI)`[i_fig]), 
            P7$datetime, (1:1:length(P7$`Temperature (C)`)))[i_fig,],
  bind_cols(rep("T3", length(P5$`Pressure (PSI)`)), P5$`Temperature (C)`, 
            P5$`Pressure (PSI)`-mean(P5$`Pressure (PSI)`[i_fig]),
            P5$datetime, (1:1:length(P5$`Temperature (C)`)))[i_fig,],
  bind_cols(rep("T4", length(P1$`Pressure (PSI)`)), P1$`Temperature (C)`, 
            P1$`Pressure (PSI)`-mean(P1$`Pressure (PSI)`[i_fig]), 
            P1$datetime, (1:1:length(P1$`Temperature (C)`)))[i_fig,],
  bind_cols(rep("T5", length(P4$`Pressure (PSI)`)), P4$`Temperature (C)`, 
            P4$`Pressure (PSI)`-mean(P4$`Pressure (PSI)`[i_fig]), 
            P4$datetime, (1:1:length(P4$`Temperature (C)`)))[i_fig,]
)
colnames(bardata) <-  c("Thermistor", "Temperature", "Pressure", "Date_Time", "Frame")
# bardata$Pressure <- bardata$Pressure - mean(bardata$Pressure)

ggplot(data = bardata, aes(x = Date_Time, y = Pressure, color = Thermistor)) +
  geom_line()

ggplot(data = bardata, aes(x = Date_Time, y = Temperature, color = Thermistor)) +
  geom_line()

# Animated bar plot
# https://r-graph-gallery.com/288-animated-barplot-transition.html
# https://towardsdatascience.com/create-animated-bar-charts-using-r-31d09e5841da

anim_bar = ggplot(data = bardata, aes(x = Thermistor, y = Pressure, fill = Temperature)) + 
  geom_bar(stat = "identity") +
  theme(text = element_text(size =30)) +
  transition_states(
    Frame,
    transition_length = 2,
    state_length = 2) + ease_aes('sine-in-out') + labs(x = as.character(P1$datetime[strtoi("{closest_state}")]))

animate(anim_bar, fps = 10, width = 1200, height = 1000, 
        renderer = gifski_renderer("/Users/evanheberlein/Library/CloudStorage/Box-Box/Cornell/CACO/2023/Thermistor/Figures/thermbars.gif"))
