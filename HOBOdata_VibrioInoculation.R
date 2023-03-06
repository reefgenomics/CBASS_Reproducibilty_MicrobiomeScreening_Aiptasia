##### Loading libraries ####
library(drc)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(stringr)
library(ggh4x)

##### HOBO logger data for T1 Aiptasia F003 + H2, Reproducibility System A + B #####

# set some colors for the temperatures and save it as a new variable
Temperature_colors <- c("#8AC8F1", "#F7A857", "#F15758", "#AB1E22")
as.factor(Temperature_colors)

# read input file 
setwd("~/Desktop/PhD_VoolstraLab/CBASS_Reproducibility_Recycling/Submission/Input_Scripts/HOBO_Data") 
input <- readxl::read_excel("HOBOdata_VibrioInoculation_input.xlsx")


# change table formatting from wide to long using pivot_longer
input <- pivot_longer(input, starts_with("D", ignore.case = FALSE, vars = NULL))

# change name variable to a factor
input$name <- as.factor(input$name)

# subset dataset for plotting 
F003_Control <- filter(input, Strain == "F003 - Control")
H2_Control <- filter(input, Strain == "H2 - Control")
F003_H2_Vibrio <- filter(input, Strain == "F003 - H2 - Vibrio")

# plot HOBO data - F003 Control (T1, System B)
F003_C <- ggplot(data = F003_Control, aes(run_time, value, color = name, group = name)) + 
          geom_line() + 
          scale_y_continuous(breaks = c(30, 34, 36, 39), limits = c(28, 40.0), expand = c(0, 0)) +
          scale_x_datetime(date_breaks = "3 hours", date_minor_breaks = "1 hour", date_labels = "%H:%M") +#####if R reads spreadsheet time as time and date use this row and comment the one below (if not, invert comment)
          scale_color_manual("name", values = Temperature_colors) +
          labs(y = "Temperature Profile", x = "CBASS run time (h)")

F003_C_plot <- F003_C + theme (legend.position = "none", 
                        legend.title = element_text(colour="black", size=13,face="bold"),
                        legend.text=element_text(size = 13),
                        line = element_line(size = 0.8),
                        axis.line = element_line(colour = "black"),
                        axis.ticks = element_line(colour = "black"),
                        axis.ticks.length = unit(0.2 , "cm"),
                        axis.text = element_text(size = 13, colour = "black"),
                        text = element_text(size = 13, colour = "black"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        strip.background = element_blank(), 
                        strip.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"))
F003_C_plot

# plot HOBO data - H2 Control (T1, System B)
H2_C <- ggplot(data = H2_Control, aes(run_time, value, color = name, group = name)) + 
        geom_line() + 
        scale_y_continuous(breaks = c(30, 34, 36, 39), limits = c(28, 40.0), expand = c(0, 0)) +
        scale_x_datetime(date_breaks = "3 hours", date_minor_breaks = "1 hour", date_labels = "%H:%M") +#####if R reads spreadsheet time as time and date use this row and comment the one below (if not, invert comment)
        scale_color_manual("name", values = Temperature_colors) +
        labs(y = "Temperature Profile", x = "CBASS run time (h)") 

H2_C_plot <- H2_C + theme (legend.position = "none", 
                    legend.title = element_text(colour="black", size=13,face="bold"),
                    legend.text=element_text(size = 13),
                    line = element_line(size = 0.8),
                    axis.line = element_line(colour = "black"),
                    axis.ticks = element_line(colour = "black"),
                    axis.ticks.length = unit(0.2 , "cm"),
                    axis.text = element_text(size = 13, colour = "black"),
                    text = element_text(size = 13, colour = "black"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    strip.background = element_blank(), 
                    strip.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"))
H2_C_plot

# plot HOBO data - F003, H2 Vibrio (T1, System B)
F003_H2_V <- ggplot(data = F003_H2_Vibrio, aes(run_time, value, color = name, group = name)) + 
             geom_line() + 
             scale_y_continuous(breaks = c(30, 34, 36, 39), limits = c(28, 40.0), expand = c(0, 0)) +
             scale_x_datetime(date_breaks = "3 hours", date_minor_breaks = "1 hour", date_labels = "%H:%M") +#####if R reads spreadsheet time as time and date use this row and comment the one below (if not, invert comment)
             scale_color_manual("name", values = Temperature_colors) +
             labs(y = "Temperature Profile", x = "CBASS run time (h)")

F003_H2_V_plot <- F003_H2_V + theme (legend.position = "none", 
                              legend.title = element_text(colour="black", size=13,face="bold"),
                              legend.text=element_text(size = 13),
                              line = element_line(size = 0.8),
                              axis.line = element_line(colour = "black"),
                              axis.ticks = element_line(colour = "black"),
                              axis.ticks.length = unit(0.2 , "cm"),
                              axis.text = element_text(size = 13, colour = "black"),
                              text = element_text(size = 13, colour = "black"),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(),
                              strip.background = element_blank(), 
                              strip.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"))
F003_H2_V_plot

# arrange plots with ggarrange 
F003_H2_CV <- ggarrange(F003_C_plot, H2_C_plot, F003_H2_V_plot, nrow = 1)
F003_H2_CV


