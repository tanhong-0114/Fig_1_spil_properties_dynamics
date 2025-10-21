#figure 1. of the paper 
# soil temperature, soil moisture, soil conductivity

library(randomForest)

library(tidyverse)
library(viridis)

library(readxl)#read excel
library(rcompanion)#check variance
library(car)#anova
library(emmeans)#multiple comparison
library(multcomp)#multiple comparison
library(multcompView)#multiple comparison
library(dplyr)#getting the means and standard error
library(ggthemes) #畫圖
library(ggplot2) #畫圖
library(scales) #畫圖
install.packages("ggbreak")
install.packages("Hmisc")
Hmisc
library(ggbreak)
library(cowplot)#排圖
library(ggpubr) #存圖
#step 1. input the microclimate data

microclimate_data_2024 <- read_excel("C:/Users/hp/Desktop/aphid soil paper/all_microclimate_data_to_Nov.xlsx",
                                     sheet = "all_only_daily_mean")

spring<-c("2024 spring")
fall <- c("2024 fall")

microclimate_data_2024_spring <- microclimate_data_2024 %>%
  filter (`growing season` == spring)

microclimate_data_2024_fall <- microclimate_data_2024 %>%
  filter (`growing season` == fall)

microclimate_data_2024_spring_and_fall <- microclimate_data_2024_spring %>%
  bind_rows(microclimate_data_2024_fall)


plot_2024_spring_soil_temp <- 
  ggplot(microclimate_data_2024_spring, aes(x = as.Date(Date), y = daily_mean_soil_temp)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "line", color = "#006600", size = 1.2) + # Mean line with 95% CI
  stat_summary(fun.data = "mean_cl_normal", geom = "ribbon", fill = "#006600", alpha = 0.2) + # 95% CI ribbon
  labs(title = "", x = "Date", y = "Soil temperature (°C)") +
  scale_x_date(date_breaks = "28 days", date_labels = "%m/%d") +
  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_y_continuous(limits = c(23, 36)) +
  annotate("text", x = as.Date("2024-04-11"), y = 36,  # Outside the x and y scales
           label = paste("2024 Spring"), 
           hjust = 0, vjust = 0, size = 4, color = "#006600")

plot_2024_fall_soil_temp <- 
  ggplot(microclimate_data_2024_fall, aes(x = as.Date(Date), y = daily_mean_soil_temp)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "line", color =  "#867052", size = 1.0) + # Mean line with 95% CI
  stat_summary(fun.data = "mean_cl_normal", geom = "ribbon", fill =  "#867052", alpha = 0.2) + # 95% CI ribbon
  labs(title = "", x = "Date", y = "Soil temperature (°C)") +
  scale_x_date(date_breaks = "28 days", date_labels = "%m/%d") +
  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_y_continuous(limits = c(23, 36)) +
  annotate("text", x = as.Date("2024-08-15"), y = 36,  # Outside the x and y scales
           label = paste("2024 Fall"), 
           hjust = 0, vjust = 0, size = 4, color = "#867052")

plot_2024_spring_soil_moisture <- 
  ggplot(microclimate_data_2024_spring, aes(x = as.Date(Date), y = daily_mean_soil_moisture)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "line", color = "#006600", size = 1.0) + # Mean line with 95% CI
  stat_summary(fun.data = "mean_cl_normal", geom = "ribbon", fill = "#006600", alpha = 0.2) + # 95% CI ribbon
  labs(title = "", x = "Date", y = "Soil moisture (%)") +
  scale_x_date(date_breaks = "28 days", date_labels = "%m/%d") +
  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_y_continuous(limits = c(5, 45)) +
  annotate("text", x = as.Date("2024-04-11"), y = 42,  # Outside the x and y scales
           label = paste("2024 Spring"), 
           hjust = 0, vjust = 0, size = 4, color = "#006600")

plot_2024_fall_soil_moisture <- 
  ggplot(microclimate_data_2024_fall, aes(x = as.Date(Date), y = daily_mean_soil_moisture)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "line", color =  "#867052", size = 1.0) + # Mean line with 95% CI
  stat_summary(fun.data = "mean_cl_normal", geom = "ribbon", fill =  "#867052", alpha = 0.2) + # 95% CI ribbon
  labs(title = "", x = "Date", y = "Soil moisture (%)") +
  scale_x_date(date_breaks = "28 days", date_labels = "%m/%d") +
  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_y_continuous(limits = c(5, 45)) +
  annotate("text", x = as.Date("2024-08-15"), y = 42,  # Outside the x and y scales
           label = paste("2024 Fall"), 
           hjust = 0, vjust = 0, size = 4, color = "#867052")

plot_2024_spring_soil_conductivity <- 
  ggplot(microclimate_data_2024_spring, aes(x = as.Date(Date), y = daily_mean_soil_conductivity)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "line", color = "#006600", size = 1.0) + # Mean line with 95% CI
  stat_summary(fun.data = "mean_cl_normal", geom = "ribbon", fill = "#006600", alpha = 0.2) + # 95% CI ribbon
  labs(title = "", x = "Date", y = "Soil electrical conductivity (dS/m)") +
  scale_x_date(date_breaks = "28 days", date_labels = "%m/%d") +
  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_y_continuous(limits = c(0, 0.6)) +
  annotate("text", x = as.Date("2024-04-11"), y = 0.57,  # Outside the x and y scales
           label = paste("2024 Spring"), 
           hjust = 0, vjust = 0, size = 4, color = "#006600")

plot_2024_fall_soil_conductivity <- 
  ggplot(microclimate_data_2024_fall, aes(x = as.Date(Date), y = daily_mean_soil_conductivity)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "line", color =  "#867052", size = 1.0) + # Mean line with 95% CI
  stat_summary(fun.data = "mean_cl_normal", geom = "ribbon", fill =  "#867052", alpha = 0.2) + # 95% CI ribbon
  labs(title = "", x = "Date", y = "Soil electrical conductivity (dS/m)") +
  scale_x_date(date_breaks = "28 days", date_labels = "%m/%d") +
  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_y_continuous(limits = c(0, 0.6)) +
  annotate("text", x = as.Date("2024-08-15"), y = 0.57,  # Outside the x and y scales
           label = paste("2024 Fall"), 
           hjust = 0, vjust = 0, size = 4, color = "#867052")

plot_2024_soil_temp<-plot_grid(plot_2024_spring_soil_temp, plot_2024_fall_soil_temp, ncol = 2)
plot_2024_soil_moisture<-plot_grid(plot_2024_spring_soil_moisture, plot_2024_fall_soil_moisture, ncol = 2)
plot_2024_soil_conductivity<-plot_grid(plot_2024_spring_soil_conductivity, plot_2024_fall_soil_conductivity, ncol = 2)


microclimate_data_2024_spring_and_fall$`growing season` <- factor(microclimate_data_2024_spring_and_fall$`growing season`,
                                                                     levels = c("2024 spring","2024 fall"))

box_plot_soil_temp <-
  ggplot(microclimate_data_2024_spring_and_fall, aes(x = `growing season`, y = daily_mean_soil_temp)) +
  geom_boxplot(aes(color = `growing season`), size = 0.8) +
  geom_signif(comparisons = list(c("2024 spring", "2024 fall")),
              map_signif_level = TRUE) +
  labs(title = "",
       x = "",
       y = "Soil temperature (°C)") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none",
    legend.key.size = unit(10, "points"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    
    # Background and gridlines
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), # Remove minor gridlines
    axis.line = element_line(color = "black", size = 0.5)
  )+ scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))

box_plot_soil_moisture <-
  ggplot(microclimate_data_2024_spring_and_fall, aes(x = `growing season`, y = daily_mean_soil_moisture)) +
  geom_boxplot(aes(color = `growing season`), size = 0.8) +
  geom_signif(comparisons = list(c("2024 spring", "2024 fall")),
              map_signif_level = TRUE) +
  labs(title = "",
       x = "",
       y = "Soil moisture (%)") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none",
    legend.key.size = unit(10, "points"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    
    # Background and gridlines
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), # Remove minor gridlines
    axis.line = element_line(color = "black", size = 0.5)
  )+ scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))

box_plot_soil_conductivity <-
  ggplot(microclimate_data_2024_spring_and_fall, aes(x = `growing season`, y = daily_mean_soil_conductivity)) +
  geom_boxplot(aes(color = `growing season`), size = 0.8) +
  geom_signif(comparisons = list(c("2024 spring", "2024 fall")),
              map_signif_level = TRUE) +
  labs(title = "",
       x = "",
       y = "Soil electrical conductivity (dS/m)") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none",
    legend.key.size = unit(10, "points"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    
    # Background and gridlines
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), # Remove minor gridlines
    axis.line = element_line(color = "black", size = 0.5)
  )+ scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))


plot_soil_moisture <- 
  ggarrange(
    plot_2024_soil_moisture,
    box_plot_soil_moisture,
  ncol = 2,
  nrow = 1,
  widths = c(2.8, 1),
  heights = c(1, 1),# Adjust height proportions
  labels = c("A", "B")
)

plot_soil_conductivity <- 
  ggarrange(
    plot_2024_soil_conductivity,
    box_plot_soil_conductivity,
    ncol = 2,
    nrow = 1,
    widths = c(2.8, 1),
    heights = c(1, 1),# Adjust height proportions
    labels = c("C", "D")
  )

plot_soil_moisture_and_conductivity <- 
  ggarrange(
    plot_soil_moisture,
    plot_soil_conductivity,
    ncol = 1,
    nrow = 2,
    widths = c(1, 1),
    heights = c(1, 1)
  )

ggsave("plot_soil_moisture_and_conductivity_new.png", plot = plot_soil_moisture_and_conductivity, width = 24, height = 20, units = "cm", dpi = 300)+
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))


plot_soil_temp <- 
  ggarrange(
    plot_2024_soil_temp,
    box_plot_soil_temp,
    ncol = 2,
    nrow = 1,
    widths = c(2.8, 1),
    heights = c(1, 1),# Adjust height proportions
    labels = c("A", "B")
  )


ggsave("plot_soil_temp.png", plot = plot_soil_temp, width = 24, height = 10, units = "cm", dpi = 300)+
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))

#figure 2


plot_2024_spring_soil_moisture_and_conductivity <-
  ggplot(microclimate_data_2024_spring, aes(x = as.Date(Date))) +
  
  # Soil moisture: mean line + 95% CI ribbon
  stat_summary(aes(y = daily_mean_soil_moisture),
               fun.data = mean_cl_normal, geom = "ribbon",
               fill = "#006600", alpha = 0.2) +
  stat_summary(aes(y = daily_mean_soil_moisture),
               fun = mean, geom = "line", color = "#006600", size = 1.2) +
  
  # Soil conductivity (scaled): mean line + 95% CI ribbon
  stat_summary(aes(y = daily_mean_soil_conductivity * 50),
               fun.data = mean_cl_normal, geom = "ribbon",
               fill = "#006600", alpha = 0.1) +
  stat_summary(aes(y = daily_mean_soil_conductivity * 50),
               fun = mean, geom = "line", color = "#006600", size = 0.8, linetype = "dashed") +
  
  # X and Y axes
  scale_x_date(date_breaks = "28 days", date_labels = "%m/%d") +
  scale_y_continuous(
    name = "Soil Moisture (%)",  # Left Y-axis
    sec.axis = sec_axis(
      trans = ~ . / 50,
      name = "Soil Electrical Conductivity (dS/m)",
      breaks = seq(0, 1.0, by = 0.1)
    ),
    limits = c(0, 45)
  ) +
  
  # Labels and theme
  labs(title = "", x = "Date") +
  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none"
  )

plot_2024_fall_soil_moisture_and_conductivity<-
  ggplot(microclimate_data_2024_fall, aes(x = as.Date(Date))) +
  # Soil moisture: mean line + 95% CI ribbon
  stat_summary(aes(y = daily_mean_soil_moisture),
               fun.data = mean_cl_normal, geom = "ribbon",
               fill = "#867052", alpha = 0.2) +
  stat_summary(aes(y = daily_mean_soil_moisture),
               fun = mean, geom = "line", color = "#867052", size = 1.2) +
  
  # Soil conductivity (scaled): mean line + 95% CI ribbon
  stat_summary(aes(y = daily_mean_soil_conductivity * 50),
               fun.data = mean_cl_normal, geom = "ribbon",
               fill = "#867052", alpha = 0.1) +
  stat_summary(aes(y = daily_mean_soil_conductivity * 50),
               fun = mean, geom = "line", color = "#867052", size = 0.8, linetype = "dashed") +
  scale_x_date(date_breaks = "28 days", date_labels = "%m/%d") +
  
  scale_y_continuous(
    name = "Soil Moisture (%)",  # Left Y-axis for soil moisture
    sec.axis = sec_axis(
      trans = ~ . / 50,  # Inverse of the scaling factor used for conductivity
      name = "Soil Electrical Conductivity (dS/m)",
      breaks = seq(0, 1.0, by = 0.1)  # Adjust breaks
    ),
    limits = c(0, 45)  # Adjust Y-axis limits for soil moisture
  ) +
  
  labs(title = "", x = "Date") +
  
  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none"  # Hide legend since we're not using color mappings
  )



plot_2024_soil_moisture_and_conductivity<-plot_grid(plot_2024_spring_soil_moisture_and_conductivity, 
                                                    plot_2024_fall_soil_moisture_and_conductivity, ncol = 1,
                                                    labels = c("A", "B"))


# spring
#daily mean soil temp
soil_moisture_and_conductivity_correlation_test <- cor.test(microclimate_data_2024_spring$daily_mean_soil_moisture,
                                                            microclimate_data_2024_spring$daily_mean_soil_conductivity,
                                                            use = "complete.obs")
# Extract the correlation coefficient and p-value
soil_moisture_and_conductivity_correlation_coefficient <- soil_moisture_and_conductivity_correlation_test$estimate
soil_moisture_and_conductivity_p_value <- soil_moisture_and_conductivity_correlation_test$p.value

# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_soil_moisture_and_conductivity_spring_correlation <-
  ggplot(microclimate_data_2024_spring, aes(x = daily_mean_soil_conductivity, 
                                      y = daily_mean_soil_moisture,
                                      color = `growing season`)) +
  geom_point(size = 0.8, alpha = 0.5) +                        # Adds points
  geom_smooth(method = "lm", col = "blue", aes(group = 1)) +  # Adds a linear trend line
  labs(title = "",
       x = "Soil Electrical Conductivity (dS/m)",
       y = "Soil Moisture (%)") +
  theme_classic(base_size = 10, base_family = "sans")+
  theme(plot.title=element_text(size=10, face="bold",hjust = 0.5),
        axis.title=element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.x = element_text(size =10),
        axis.text.y = element_text(size = 10),
        legend.position= "none",
        legend.key.size = unit(12,"points"),
        legend.title = element_blank())+
  scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))+
  coord_cartesian(clip = "off") +  # Allows annotations to appear outside the plot limits
  annotate("text", x = 0.3, y = -8,  # Outside the x and y scales
           label = paste("p-value < 0.001"), 
           hjust = 0, vjust = 0, size = 4, color = "red") +
  annotate("text", x = 0.3, y = -4,  # Outside the x and y scales
           label = paste("r =", round(soil_moisture_and_conductivity_correlation_coefficient, 3)), 
           hjust = 0, vjust = 0, size = 4, color = "red")+
  scale_x_continuous(limits = c(0, 0.6)) +  # Expands x-axis range to accommodate annotations
  scale_y_continuous(limits = c(-10, 60))

# spring
#daily mean soil temp
soil_moisture_and_conductivity_fall_correlation_test <- cor.test(microclimate_data_2024_fall$daily_mean_soil_moisture,
                                                            microclimate_data_2024_fall$daily_mean_soil_conductivity,
                                                            use = "complete.obs")
# Extract the correlation coefficient and p-value
soil_moisture_and_conductivity_fall_correlation_coefficient <- soil_moisture_and_conductivity_fall_correlation_test$estimate
soil_moisture_and_conductivity_fall_p_value <- soil_moisture_and_conductivity_fall_correlation_test$p.value

# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_soil_moisture_and_conductivity_fall_correlation <-
  ggplot(microclimate_data_2024_fall, aes(x = daily_mean_soil_conductivity, 
                                                     y = daily_mean_soil_moisture,
                                                     color = `growing season`)) +
  geom_point(size = 0.8, alpha = 0.5) +                        # Adds points
  geom_smooth(method = "lm", col = "blue", aes(group = 1)) +  # Adds a linear trend line
  labs(title = "",
       x = "Soil Electrical Conductivity (dS/m)",
       y = "Soil Moisture (%)") +
  theme_classic(base_size = 10, base_family = "sans")+
  theme(plot.title=element_text(size=10, face="bold",hjust = 0.5),
        axis.title=element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.x = element_text(size =10),
        axis.text.y = element_text(size = 10),
        legend.position= "none",
        legend.key.size = unit(12,"points"),
        legend.title = element_blank())+
  scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))+
  coord_cartesian(clip = "off") +  # Allows annotations to appear outside the plot limits
  annotate("text", x = 0.3, y = -8,  # Outside the x and y scales
           label = paste("p-value < 0.001"), 
           hjust = 0, vjust = 0, size = 4, color = "red") +
  annotate("text", x = 0.3, y = -4,  # Outside the x and y scales
           label = paste("r =", round(soil_moisture_and_conductivity_fall_correlation_coefficient, 3)), 
           hjust = 0, vjust = 0, size = 4, color = "red")+
  scale_x_continuous(limits = c(0, 0.6)) +  # Expands x-axis range to accommodate annotations
  scale_y_continuous(limits = c(-10, 60))

plot_soil_moisture_and_conductivity_correlation<-plot_grid(plot_soil_moisture_and_conductivity_spring_correlation, 
                                                           plot_soil_moisture_and_conductivity_fall_correlation, 
                                                           ncol = 1, labels = c("B", "D"))

plot_soil_moisture_and_conductivity<- 
  ggarrange(
  plot_2024_soil_moisture_and_conductivity,
  plot_soil_moisture_and_conductivity_correlation,
  ncol = 2,
  nrow = 1,
  widths = c(2, 1),
  heights = c(1, 1)# Adjust height proportions
 )

ggsave("plot_soil_moisture_and_conductivity.png", 
       plot = plot_soil_moisture_and_conductivity, width = 24, height = 16, 
       units = "cm", dpi = 300)+
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))

plot_2024_spring_soil_conductivity<-
  ggplot(microclimate_data_2024_spring, aes(x = as.Date(Date), y = daily_mean_soil_conductivity, alpha =as.factor(block))) +
  geom_line(aes(color = as.factor(block)), alpha = 0.5, size = 0.6, show.legend = FALSE) + # Transparent lines for each block
  stat_summary(fun = mean, geom = "line", color = "#006600", size = 1.2, alpha = 1) + # Bold average line
  labs(title = "", x = "Date", y = "Soil conductivity") +
  scale_x_date(date_breaks = "28 days", date_labels = "%m/%d") +
  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none",
    legend.key.size = unit(10, "points"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )+
  scale_y_continuous(limits = c(0, 0.65))+
  annotate("text", x = as.Date("2024-04-11"), y = 0.6,  # Outside the x and y scales
           label = paste("2024 Spring"), 
           hjust = 0, vjust = 0, size = 4, color = "#006600")

plot_2024_fall_soil_conductivity<-
  ggplot(microclimate_data_2024_fall, aes(x = as.Date(Date), y = daily_mean_soil_conductivity, alpha =as.factor(block))) +
  geom_line(aes(color = as.factor(block)), alpha = 0.5, size = 0.6, show.legend = FALSE) + # Transparent lines for each block
  stat_summary(fun = mean, geom = "line", color = "#867052", size = 1.2, alpha = 1) + # Bold average line
  labs(title = "", x = "Date", y = "Soil conductivity") +
  scale_x_date(date_breaks = "28 days", date_labels = "%m/%d") +
  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none",
    legend.key.size = unit(10, "points"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )+
  scale_y_continuous(limits = c(0, 0.65))+
  annotate("text", x = as.Date("2024-08-15"), y = 0.6,  # Outside the x and y scales
           label = paste("2024 Fall"), 
           hjust = 0, vjust = 0, size = 4, color = "#867052")


plot_2024_soil_conductivity<-plot_grid(plot_2024_spring_soil_conductivity, plot_2024_fall_soil_conductivity, ncol = 2)


plot_soil_condition_through_days<-plot_grid(plot_2024_soil_temp,
                                            plot_2024_soil_moisture,
                                            plot_2024_soil_conductivity,
                                            labels = c("A","B","C"), ncol = 1)

ggsave("plot_soil_condition_through_days.png", plot = plot_soil_condition_through_days, width = 24, height = 30, units = "cm", dpi = 300)+
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))


#跑 ANOVA
# 確認Data符合常態分佈
qqnorm(microclimate_data_2024_spring_and_fall$daily_mean_soil_temp)#check normality
qqline(microclimate_data_2024_spring_and_fall$daily_mean_soil_temp)#check normality
plotNormalHistogram(microclimate_data_2024_spring_and_fall$daily_mean_soil_temp)#check variance.loss.1$water.loss)#check variance

microclimate_data_2024_spring_and_fall$`growing season`<-factor(microclimate_data_2024_spring_and_fall$`growing season`,levels = c("2024 spring","2024 fall"))
lm_soil_temp= lm(daily_mean_soil_temp ~ `growing season`,data= microclimate_data_2024_spring_and_fall)
summary(lm_soil_temp)
anova_soil_temp <- Anova(lm_soil_temp, type="II") #anova.自己命名= Anova(lm.自己命名, type="II")
anova_soil_temp#anova.自己命名
microclimate_data_2024_spring_and_fall$`growing season`<-factor(microclimate_data_2024_spring_and_fall$`growing season`,levels = c("2024 spring","2024 fall"))

ls_soil_temp = emmeans(lm_soil_temp,~`growing season`)#multiple comparison
mt_soil_temp<-cld(ls_soil_temp, by=NULL, alpha=.05, Letters=letters, reverse = TRUE)#multiple comparison
mt_soil_temp
mt_soil_temp$.group <- gsub(" ", "", mt_soil_temp$.group)#removing space in grouping info
mt_soil_temp <-dplyr::arrange(mt_soil_temp, `growing season`)#rearrange rows in dataframe
mt_soil_temp
gp_soil_temp<-mt_soil_temp$.group#isolating the grouping info
gp_soil_temp


text_labels <- group_info %>%
  mutate(y_position = 34)  # Set appropriate y-position for text

box_plot_soil_temp <-
  ggplot(microclimate_data_2024_spring_and_fall, aes(x = `growing season`, y = daily_mean_soil_temp)) +
  geom_boxplot(aes(color = `growing season`), size = 0.8) +
  geom_text(data = text_labels, aes(x = growing.season, y = y_position, label = gp_soil_temp),
            vjust = -0.3, color = "black", size = 2.8)  +
  labs(title = "",
       x = "",
       y = "Daily mean soil temperature") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none",
    legend.key.size = unit(10, "points"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    
    # Background and gridlines
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), # Remove minor gridlines
    axis.line = element_line(color = "black", size = 0.8)
  )+ scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))

#跑 ANOVA
# 確認Data符合常態分佈
qqnorm(microclimate_data_2024_spring_and_fall$daily_mean_soil_moisture)#check normality
qqline(microclimate_data_2024_spring_and_fall$daily_mean_soil_moisture)#check normality
plotNormalHistogram(microclimate_data_2024_spring_and_fall$daily_mean_soil_moisture)#check variance.loss.1$water.loss)#check variance

microclimate_data_2024_spring_and_fall$`growing season`<-factor(microclimate_data_2024_spring_and_fall$`growing season`,levels = c("2024 spring","2024 fall"))
lm_soil_moisture= lm(daily_mean_soil_moisture ~ `growing season`,data= microclimate_data_2024_spring_and_fall)
summary(lm_soil_moisture)
anova_soil_moisture <- Anova(lm_soil_moisture, type="II") #anova.自己命名= Anova(lm.自己命名, type="II")
anova_soil_moisture#anova.自己命名
microclimate_data_2024_spring_and_fall$`growing season`<-factor(microclimate_data_2024_spring_and_fall$`growing season`,levels = c("2024 spring","2024 fall"))

ls_soil_moisture = emmeans(lm_soil_moisture,~`growing season`)#multiple comparison
mt_soil_moisture<-cld(ls_soil_moisture, by=NULL, alpha=.05, Letters=letters, reverse = TRUE)#multiple comparison
mt_soil_moisture
mt_soil_moisture$.group <- gsub(" ", "", mt_soil_moisture$.group)#removing space in grouping info
mt_soil_moisture <-dplyr::arrange(mt_soil_moisture, `growing season`)#rearrange rows in dataframe
mt_soil_moisture
gp_soil_moisture<-mt_soil_moisture$.group#isolating the grouping info
gp_soil_moisture


moisture_text_labels <- group_info %>%
  mutate(y_position = 45)  # Set appropriate y-position for text

box_plot_soil_moisture <-
  ggplot(microclimate_data_2024_spring_and_fall, aes(x = `growing season`, y = daily_mean_soil_moisture)) +
  geom_boxplot(aes(color = `growing season`), size = 0.8) +
  geom_text(data = moisture_text_labels, aes(x = growing.season, y = y_position, label = gp_soil_moisture),
            vjust = -0.3, color = "black", size = 2.8)  +
  labs(title = "",
       x = "",
       y = "Daily mean soil moisture") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none",
    legend.key.size = unit(10, "points"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    
    # Background and gridlines
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), # Remove minor gridlines
    axis.line = element_line(color = "black", size = 0.8)
  )+ scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))


qqnorm(microclimate_data_2024_spring_and_fall$daily_mean_soil_conductivity)#check normality
qqline(microclimate_data_2024_spring_and_fall$daily_mean_soil_conductivity)#check normality
plotNormalHistogram(microclimate_data_2024_spring_and_fall$daily_mean_soil_conductivity)#check variance.loss.1$water.loss)#check variance

microclimate_data_2024_spring_and_fall$`growing season`<-factor(microclimate_data_2024_spring_and_fall$`growing season`,levels = c("2024 spring","2024 fall"))
lm_soil_conductivity= lm(daily_mean_soil_conductivity ~ `growing season`,data= microclimate_data_2024_spring_and_fall)
summary(lm_soil_conductivity)
anova_soil_conductivity <- Anova(lm_soil_conductivity, type="II") #anova.自己命名= Anova(lm.自己命名, type="II")
anova_soil_conductivity#anova.自己命名
microclimate_data_2024_spring_and_fall$`growing season`<-factor(microclimate_data_2024_spring_and_fall$`growing season`,levels = c("2024 spring","2024 fall"))

ls_soil_conductivity = emmeans(lm_soil_conductivity,~`growing season`)#multiple comparison
mt_soil_conductivity<-cld(ls_soil_conductivity, by=NULL, alpha=.05, Letters=letters, reverse = TRUE)#multiple comparison
mt_soil_conductivity
mt_soil_conductivity$.group <- gsub(" ", "", mt_soil_conductivity$.group)#removing space in grouping info
mt_soil_conductivity <-dplyr::arrange(mt_soil_conductivity, `growing season`)#rearrange rows in dataframe
mt_soil_conductivity
gp_soil_conductivity<-mt_soil_conductivity$.group#isolating the grouping info
gp_soil_conductivity

conductivity_text_labels <- group_info %>%
  mutate(y_position = 0.65)  # Set appropriate y-position for text

box_plot_soil_conductivity <-
  ggplot(microclimate_data_2024_spring_and_fall, aes(x = `growing season`, y = daily_mean_soil_conductivity)) +
  geom_boxplot(aes(color = `growing season`), size = 0.8) +
  geom_text(data =  conductivity_text_labels, aes(x = growing.season, y = y_position, label = gp_soil_conductivity),
            vjust = -0.3, color = "black", size = 2.8) +
  labs(title = "",
       x = "",
       y = "Daily mean soil conductivity") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none",
    legend.key.size = unit(10, "points"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    
    # Background and gridlines
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), # Remove minor gridlines
    axis.line = element_line(color = "black", size = 0.8)
  )+ scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))

plot_soil_condition_box<-plot_grid(box_plot_soil_temp,
                                   box_plot_soil_moisture,
                                   box_plot_soil_conductivity,
                                            labels = c("D","E","F"), ncol = 3)

plot_figure_1<-plot_grid(plot_soil_condition_through_days,
                         plot_soil_condition_box,ncol = 1)

library(ggpubr)

# Arrange the time-series plots (A, B, C) in one column
plot_soil_condition_through_days <- ggarrange(
  plot_2024_soil_temp,
  plot_2024_soil_moisture,
  plot_2024_soil_conductivity,
  labels = c("A", "B", "C"),
  ncol = 1,
  nrow = 3,
  align = "v" # Vertical alignment
)

# Arrange the boxplots (D, E, F) in one row
plot_soil_condition_box <- ggarrange(
  box_plot_soil_temp,
  box_plot_soil_moisture,
  box_plot_soil_conductivity,
  labels = c("D", "E", "F"),
  ncol = 3,
  nrow = 1,
  align = "h" # Horizontal alignment
)

# Combine the two main plots into one figure
plot_figure_1 <- ggarrange(
  plot_soil_condition_through_days,
  plot_soil_condition_box,
  ncol = 1,
  nrow = 2,
  heights = c(3, 1) # Adjust height proportions
)

# Print the final figure
print(plot_figure_1)

ggsave("plot_figure_1_new.png", plot = plot_figure_1, width = 24, height = 30, units = "cm", dpi = 300)+
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))

# Pearson correlation for spring
cor_spring <- cor.test(microclimate_data_2024_spring$daily_mean_soil_temp,
                       microclimate_data_2024_spring$daily_mean_temp,
                       method = "pearson")

# Pearson correlation for fall
cor_fall <- cor.test(microclimate_data_2024_fall$daily_mean_soil_temp,
                     microclimate_data_2024_fall$daily_mean_temp,
                     method = "pearson")


# Calculate temp range before plotting
min_temp <- min(microclimate_data_2024_spring$daily_mean_temp, na.rm = TRUE)
max_temp <- max(microclimate_data_2024_spring$daily_mean_temp, na.rm = TRUE)

# Check if values are valid
print(c(min_temp, max_temp))  # Should not be Inf or NA


# Step 1: Pre-scale air temperature to match soil temperature axis
microclimate_data_2024_spring$air_temp_scaled <- with(microclimate_data_2024_spring,
                                                      daily_mean_temp * (15 / (max_temp - min_temp)) + 20)
min_air <- 15
max_air <- 35
min_soil_plot <- 20
max_soil_plot <- 40

scale_factor <- (max_soil_plot - min_soil_plot) / (max_air - min_air)


# Create scaled air temp column
microclimate_data_2024_spring$air_temp_scaled <- 
  (microclimate_data_2024_spring$daily_mean_temp - min_air) * scale_factor + min_soil_plot

microclimate_data_2024_fall$air_temp_scaled <- 
  (microclimate_data_2024_fall$daily_mean_temp - min_air) * scale_factor + min_soil_plot


plot_2024_spring_soil_temp_and_temp <- 
  ggplot(microclimate_data_2024_spring, aes(x = as.Date(Date))) +
  
  # Soil temperature
  stat_summary(aes(y = daily_mean_soil_temp),
               fun.data = mean_cl_normal, geom = "ribbon",
               fill = "#006600", alpha = 0.2) +
  stat_summary(aes(y = daily_mean_soil_temp),
               fun = mean, geom = "line", color = "#006600", size = 1.2) +
  
  # Scaled air temperature
  stat_summary(aes(y = air_temp_scaled),
               fun.data = mean_cl_normal, geom = "ribbon",
               fill = "#006600", alpha = 0.1) +
  stat_summary(aes(y = air_temp_scaled),
               fun = mean, geom = "line", color = "#006600", size = 0.8, linetype = "dashed") +
  
  # Axes
  scale_x_date(date_breaks = "28 days", date_labels = "%m/%d") +
  scale_y_continuous(
    name = "Soil Temperature (°C)",
    limits = c(min_soil_plot, max_soil_plot),
    sec.axis = sec_axis(
      trans = ~ (. - min_soil_plot) / scale_factor + min_air,
      name = "Air Temperature (°C)"
    )
  ) +
  
  labs(title = "", x = "Date") +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = "none"
  )

"#867052"

plot_2024_fall_soil_temp_and_temp <- 
  ggplot(microclimate_data_2024_fall, aes(x = as.Date(Date))) +
  
  # Soil temperature
  stat_summary(aes(y = daily_mean_soil_temp),
               fun.data = mean_cl_normal, geom = "ribbon",
               fill = "#867052", alpha = 0.2) +
  stat_summary(aes(y = daily_mean_soil_temp),
               fun = mean, geom = "line", color = "#867052", size = 1.2) +
  
  # Scaled air temperature
  stat_summary(aes(y = air_temp_scaled),
               fun.data = mean_cl_normal, geom = "ribbon",
               fill = "#867052", alpha = 0.1) +
  stat_summary(aes(y = air_temp_scaled),
               fun = mean, geom = "line", color = "#867052", size = 0.8, linetype = "dashed") +
  
  # Axes
  scale_x_date(date_breaks = "28 days", date_labels = "%m/%d") +
  scale_y_continuous(
    name = "Soil Temperature (°C)",
    limits = c(min_soil_plot, max_soil_plot),
    sec.axis = sec_axis(
      trans = ~ (. - min_soil_plot) / scale_factor + min_air,
      name = "Air Temperature (°C)"
    )
  ) +
  
  labs(title = "", x = "Date") +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = "none"
  )



plot_2024_soil_temp_and_temp<-plot_grid(plot_2024_spring_soil_temp_and_temp, 
                                        plot_2024_fall_soil_temp_and_temp, ncol = 1,
                                                    labels = c("A", "C"))


# spring
#daily mean soil temp
soil_temp_and_temp_spring_correlation_test <- cor.test(microclimate_data_2024_spring$daily_mean_soil_temp,
                                                            microclimate_data_2024_spring$daily_mean_temp,
                                                            use = "complete.obs")
# Extract the correlation coefficient and p-value
soil_temp_and_temp_spring_correlation_coefficient <- soil_temp_and_temp_spring_correlation_test$estimate
soil_temp_and_temp_spring_p_value <- soil_temp_and_temp_spring_correlation_test$p.value

# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_soil_temp_and_temp_spring_correlation <-
  ggplot(microclimate_data_2024_spring, aes(x = daily_mean_temp, 
                                            y = daily_mean_soil_temp,
                                            color = `growing season`)) +
  geom_point() +                        # Adds points
  geom_smooth(method = "lm", col = "blue", aes(group = 1)) +  # Adds a linear trend line
  labs(title = "",
       x = "Air Temperature (°C)",
       y = "Soil Temperature (°C)") +
  theme_classic(base_size = 10, base_family = "sans")+
  theme(plot.title=element_text(size=10, face="bold",hjust = 0.5),
        axis.title=element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.x = element_text(size =10),
        axis.text.y = element_text(size = 10),
        legend.position= "none",
        legend.key.size = unit(12,"points"),
        legend.title = element_blank())+
  scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))+
  coord_cartesian(clip = "off") +  # Allows annotations to appear outside the plot limits
  annotate("text", x = 27, y = 22,  # Outside the x and y scales
           label = paste("p-value < 0.001"), 
           hjust = 0, vjust = 0, size = 4, color = "red") +
  annotate("text", x = 27, y = 23,  # Outside the x and y scales
           label = paste("r =", round(soil_temp_and_temp_spring_correlation_coefficient, 3)), 
           hjust = 0, vjust = 0, size = 4, color = "red")+
  scale_x_continuous(limits = c(23.5, 32)) +  # Expands x-axis range to accommodate annotations
  scale_y_continuous(limits = c(21, 34))

# spring
#daily mean soil temp
soil_temp_and_temp_fall_correlation_test <- cor.test(microclimate_data_2024_fall$daily_mean_soil_temp,
                                                       microclimate_data_2024_fall$daily_mean_temp,
                                                       use = "complete.obs")
# Extract the correlation coefficient and p-value
soil_temp_and_temp_fall_correlation_coefficient <- soil_temp_and_temp_fall_correlation_test$estimate
soil_temp_and_temp_fall_p_value <- soil_temp_and_temp_fall_correlation_test$p.value

# scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))
# Scatter plot with a linear trend line
plot_soil_temp_and_temp_fall_correlation <-
  ggplot(microclimate_data_2024_fall, aes(x = daily_mean_temp, 
                                            y = daily_mean_soil_temp,
                                            color = `growing season`)) +
  geom_point() +                        # Adds points
  geom_smooth(method = "lm", col = "blue", aes(group = 1)) +  # Adds a linear trend line
  labs(title = "",
       x = "Air Temperature (°C)",
       y = "Soil Temperature (°C)") +
  theme_classic(base_size = 10, base_family = "sans")+
  theme(plot.title=element_text(size=10, face="bold",hjust = 0.5),
        axis.title=element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.x = element_text(size =10),
        axis.text.y = element_text(size = 10),
        legend.position= "none",
        legend.key.size = unit(12,"points"),
        legend.title = element_blank())+
  scale_color_manual(values = c("2024 spring" = "#006600", "2024 fall" = "#867052"))+
  coord_cartesian(clip = "off") +  # Allows annotations to appear outside the plot limits
  annotate("text", x = 27, y = 22,  # Outside the x and y scales
           label = paste("p-value < 0.001"), 
           hjust = 0, vjust = 0, size = 4, color = "red") +
  annotate("text", x = 27, y = 23,  # Outside the x and y scales
           label = paste("r =", round(soil_temp_and_temp_fall_correlation_coefficient, 3)), 
           hjust = 0, vjust = 0, size = 4, color = "red")+
  scale_x_continuous(limits = c(23.5, 32)) +  # Expands x-axis range to accommodate annotations
  scale_y_continuous(limits = c(21, 34))

plot_soil_temp_and_temp_correlation<-plot_grid(plot_soil_temp_and_temp_spring_correlation, 
                                                           plot_soil_temp_and_temp_fall_correlation, 
                                                           ncol = 1, labels = c("B", "D"))

plot_soil_temp_and_air_temp<- 
  ggarrange(
    plot_2024_soil_temp_and_temp,
    plot_soil_temp_and_temp_correlation,
    ncol = 2,
    nrow = 1,
    widths = c(2, 1),
    heights = c(1, 1)# Adjust height proportions
  )

#
ggsave("plot_soil_temp_and_air_temp.png", plot = plot_soil_temp_and_air_temp, width = 24, height = 16, 
units = "cm", dpi = 300)


plot_2024_spring_soil_temp_and_temp <- 
  ggplot(microclimate_data_2024_spring, aes(x = as.Date(Date))) +
  
  # Soil temperature: mean line + 95% CI ribbon
  stat_summary(aes(y = daily_mean_soil_temp),
               fun.data = mean_cl_normal, geom = "ribbon",
               fill = "#006600", alpha = 0.2) +
  stat_summary(aes(y = daily_mean_soil_temp),
               fun = mean, geom = "line", color = "#006600", size = 1.2) +
  
  # Air temperature: mean line + 95% CI ribbon (dashed)
  stat_summary(aes(y = daily_mean_temp),
               fun.data = mean_cl_normal, geom = "ribbon",
               fill = "#006600", alpha = 0.1) +
  stat_summary(aes(y = daily_mean_temp),
               fun = mean, geom = "line", color = "#006600", size = 0.8, linetype = "dashed") +
  
  # X and Y axes
  scale_x_date(date_breaks = "28 days", date_labels = "%m/%d") +
  scale_y_continuous(name = "Temperature (°C)", limits = c(20, 35)) +
  
  # Labels and theme
  labs(title = "", x = "Date") +
  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none"
  )


"#867052"

plot_2024_fall_soil_temp_and_temp <- 
  ggplot(microclimate_data_2024_fall, aes(x = as.Date(Date))) +
  
  # Soil temperature: mean line + 95% CI ribbon
  stat_summary(aes(y = daily_mean_soil_temp),
               fun.data = mean_cl_normal, geom = "ribbon",
               fill = "#867052", alpha = 0.2) +
  stat_summary(aes(y = daily_mean_soil_temp),
               fun = mean, geom = "line", color = "#867052", size = 1.2) +
  
  # Air temperature: mean line + 95% CI ribbon (dashed)
  stat_summary(aes(y = daily_mean_temp),
               fun.data = mean_cl_normal, geom = "ribbon",
               fill = "#867052", alpha = 0.1) +
  stat_summary(aes(y = daily_mean_temp),
               fun = mean, geom = "line", color = "#867052", size = 0.8, linetype = "dashed") +
  
  # X and Y axes
  scale_x_date(date_breaks = "28 days", date_labels = "%m/%d") +
  scale_y_continuous(name = "Temperature (°C)", limits = c(20, 35)) +
  
  # Labels and theme
  labs(title = "", x = "Date") +
  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none"
  )



plot_2024_soil_temp_and_temp<-plot_grid(plot_2024_spring_soil_temp_and_temp, 
                                        plot_2024_fall_soil_temp_and_temp, ncol = 1,
                                        labels = c("A", "C"))


plot_soil_temp_and_air_temp<- 
  ggarrange(
    plot_2024_soil_temp_and_temp,
    plot_soil_temp_and_temp_correlation,
    ncol = 2,
    nrow = 1,
    widths = c(2, 1),
    heights = c(1, 1)# Adjust height proportions
  )

#
ggsave("plot_soil_temp_and_air_temp_new.png", plot = plot_soil_temp_and_air_temp, width = 24, height = 16, 
       units = "cm", dpi = 300)
