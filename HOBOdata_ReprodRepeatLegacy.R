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
input <- readxl::read_excel("HOBOdata_ReprodRepeatLegacy_input.xlsx")


# change table formatting from wide to long using pivot_longer
input <- pivot_longer(input, starts_with("D", ignore.case = FALSE, vars = NULL))

# change name variable to a factor
input$name <- as.factor(input$name)

# subset dataset for plotting 
F003_A_T1 <- filter(input, Strain == "F003" & System == "A" & Timepoint == "T1")
F003_B_T1 <- filter(input, Strain == "F003" & System == "B" & Timepoint == "T1")
H2_A_T1 <- filter(input, Strain == "H2" & System == "A" & Timepoint == "T1")
H2_B_T1 <- filter(input, Strain == "H2" & System == "B" & Timepoint == "T1")

# plot HOBO data - F003 A
F003_A <- ggplot(data = F003_A_T1, aes(run_time, value, color = name, group = name)) + 
          geom_line() + 
          scale_y_continuous(breaks = c(30, 34, 36, 39), limits = c(28, 40.0), expand = c(0, 0)) +
          scale_x_datetime(date_breaks = "3 hours", date_minor_breaks = "1 hour", date_labels = "%H:%M") +#####if R reads spreadsheet time as time and date use this row and comment the one below (if not, invert comment)
          scale_color_manual("name", values = Temperature_colors) +
          labs(y = "Temperature Profile", x = "CBASS run time (h)")

F003_A_plot <- F003_A + theme (legend.position= "none", 
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
F003_A_plot

# plot HOBO data - F003 B
F003_B <- ggplot(data = F003_B_T1, aes(run_time, value, color = name, group = name)) + 
          geom_line() + 
          scale_y_continuous(breaks = c(30, 34, 36, 39), limits = c(28, 40.0), expand = c(0, 0)) +
          scale_x_datetime(date_breaks = "3 hours", date_minor_breaks = "1 hour", date_labels = "%H:%M") +#####if R reads spreadsheet time as time and date use this row and comment the one below (if not, invert comment)
          scale_color_manual("name", values = Temperature_colors) +
          labs(y = "Temperature Profile", x = "CBASS run time (h)")

F003_B_plot <- F003_B + theme (legend.position= "none", 
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
F003_B_plot

# plot HOBO data - H2 A
H2_A <- ggplot(data = H2_A_T1, aes(run_time, value, color = name, group = name)) + 
        geom_line() + 
        scale_y_continuous(breaks = c(30, 34, 36, 39), limits = c(28, 40.0), expand = c(0, 0)) +
        scale_x_datetime(date_breaks = "3 hours", date_minor_breaks = "1 hour", date_labels = "%H:%M") +#####if R reads spreadsheet time as time and date use this row and comment the one below (if not, invert comment)
        scale_color_manual("name", values = Temperature_colors) +
        labs(y = "Temperature Profile", x = "CBASS run time (h)") 

H2_A_plot <- H2_A + theme (legend.position= "none", 
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
H2_A_plot

# plot HOBO data - H2 B
H2_B <- ggplot(data = H2_B_T1, aes(run_time, value, color = name, group = name)) + 
        geom_line() + 
        scale_y_continuous(breaks = c(30, 34, 36, 39), limits = c(28, 40.0), expand = c(0, 0)) +
        scale_x_datetime(date_breaks = "3 hours", date_minor_breaks = "1 hour", date_labels = "%H:%M") +#####if R reads spreadsheet time as time and date use this row and comment the one below (if not, invert comment)
        scale_color_manual("name", values = Temperature_colors) +
        labs(y = "Temperature Profile", x = "CBASS run time (h)")

H2_B_plot <- H2_B + theme (legend.position= "none", 
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
H2_B_plot

# arrange plots with ggarrange 
HOBO_CBASS1 <- ggarrange(F003_A_plot, F003_B_plot, H2_A_plot, H2_B_plot, nrow = 2)
HOBO_CBASS1


##### HOBO logger data for CBASS 1 + 2 (T1-T4) Aiptasia F003 + H2, System B #####

# subset dataset for plotting 
F003_B <- filter(input, Strain == "F003" & System == "B")
H2_B <- filter(input, Strain == "H2" & System == "B")

# plot HOBO data - F003 B
F003_B <- ggplot(data = F003_B, aes(run_time, value, color = name, group = name)) + 
          geom_line() + 
          scale_y_continuous(breaks = c(30, 34, 36, 39), limits = c(28, 40.0), expand = c(0, 0)) +
          scale_x_datetime(date_breaks = "3 hours", date_minor_breaks = "1 hour", date_labels = "%H:%M") +#####if R reads spreadsheet time as time and date use this row and comment the one below (if not, invert comment)
          scale_color_manual("name", values = Temperature_colors) +
          labs(y = "Temperature Profile", x = "CBASS run time (h)")

F003_B_plot <- F003_B + theme (legend.position= "none", 
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
F003_B_plot

# plot HOBO data - H2 B
H2_B <- ggplot(data = H2_B, aes(run_time, value, color = name, group = name)) + 
        geom_line() + 
        scale_y_continuous(breaks = c(30, 34, 36, 39), limits = c(28, 40.0), expand = c(0, 0)) +
        scale_x_datetime(date_breaks = "3 hours", date_minor_breaks = "1 hour", date_labels = "%H:%M") +#####if R reads spreadsheet time as time and date use this row and comment the one below (if not, invert comment)
        scale_color_manual("name", values = Temperature_colors) +
        labs(y = "Temperature Profile", x = "CBASS run time (h)")

H2_B_plot <- H2_B + theme (legend.position= "none", 
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
H2_B_plot

# arrange plots with ggarrange 
HOBO_CBASS1_CBASS2 <- ggarrange(F003_B_plot, H2_B_plot, nrow = 2)
HOBO_CBASS1_CBASS2


##### HOBO logger data for CBASS 2 (T5) Aiptasia F003 + H2, Surviving animals, System B #####

# subset dataset for plotting 
F003_H2_Survivor_B <- filter(input, Strain == "F003 - H2" & System == "B" & Timepoint == "T3")
F003_H2_Survivor_B <- F003_H2_Survivor_B[complete.cases(F003_H2_Survivor_B), ]

# plot HOBO data - Surviving animals CBASS run
F003_H2_Survivor_B <- ggplot(data = F003_H2_Survivor_B, aes(run_time, value, color = name, group = name)) + 
                      geom_line() + 
                      scale_y_continuous(breaks = c(30, 34, 36), limits = c(28.0, 37.0), expand = c(0, 0)) +
                      scale_x_datetime(date_breaks = "3 hours", date_minor_breaks = "1 hour", date_labels = "%H:%M") +#####if R reads spreadsheet time as time and date use this row and comment the one below (if not, invert comment)
                      scale_color_manual("name", values = Temperature_colors) +
                      labs(y = "Temperature Profile", x = "CBASS run time (h)")

F003_H2_Survivor_B_plot <- F003_H2_Survivor_B + theme (legend.position= "none", 
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
F003_H2_Survivor_B_plot
