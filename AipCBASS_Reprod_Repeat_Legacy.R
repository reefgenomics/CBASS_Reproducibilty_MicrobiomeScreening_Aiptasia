##### Loading libraries ####
library(drc)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(stringr)
library(ggh4x)

##### T1 Aiptasia F003 + H2, Reproducibility System A + B #####

# read input file 
setwd("~/Desktop/PhD_VoolstraLab/CBASS_Reproducibility_Recycling/Submission/Input_Scripts/Reprod_Repeat_Legacy")
input <- read.table("AipCBASS_FvFm_Reprod_Repeat_Legacy_Input.txt", sep = "\t", header = T)
# change temperature and PAM values to numeric 
input$Temperature=as.numeric(input$Temperature) 
input$PAM=as.numeric(input$PAM) 
# remove rows with missing data
input<-input[complete.cases(input), ]
# filter for T1 only
input <- filter(input, Timepoint == "1")
# create a group to define population ID. One population = 1 set of 10 anemones. 
input$Population=as.factor(paste(input$System,input$Timepoint,input$Strain, sep = "_")) 
levels(input$Population)
# drm demo to one population without temperature limits (helps to fit regression curve better to data points, limits can be used but need to be adjusted to data)
drm(PAM ~ Temperature, data=input[input$Population=="A_1_F003",],
    fct = LL.3(names = c('Slope', 'Max', 'ED50')))
# loop for all populations
mod1<-lapply(unique(input$Population), 
             function(x) drm(PAM ~ Temperature, data=input[input$Population==x,],
                             fct = LL.3(names = c('Slope', 'Max', 'ED50'))))
# extract ED50 thermal tolerance thresholds
ed50_list<-lapply(c(1:length(mod1)), function(x) mod1[[x]][["coefficients"]][["ED50:(Intercept)"]])
ed50_df<-as.data.frame(do.call(rbind, ed50_list))
ed50_df$Sample=unique(input$Population)
ed50_df=tidyr::separate(ed50_df,Sample,into =c("System", "Timepoint", "Strain" ),sep = "_",remove = FALSE,extra = "merge") #, "Replicate"),sep = "_",remove = FALSE,extra = "merge")
ed50_df$Timepoint=as.integer(ed50_df$Timepoint)
ed50_df$Strain=factor(ed50_df$Strain, levels = c("F003", "H2"))
colnames(ed50_df)[1]="ED50"

#### Barplot T1 F003 + H2 ####
# extract standard error from ED50 model
error.list <- lapply(c(1:length(mod1)), function(x) ED(mod1[[x]], 50))

confidence_df <- as.data.frame(do.call(rbind, error.list))
confidence_df$Sample=unique(input$Population)
confidence_df$ED50=unique(input$Population)
confidence_df <- data.frame(ED50=confidence_df$Estimate, Error = confidence_df$`Std. Error`, Sample = confidence_df$Sample)
confidence_df <- tidyr::separate(confidence_df,Sample,into =c("System", "Timepoint", "Strain" ),sep = "_",remove = FALSE,extra = "merge")
confidence_df$Upper <- confidence_df$ED50 + confidence_df$Error
confidence_df$Lower <- confidence_df$ED50 - confidence_df$Error

confidence_df$Timepoint <- as.integer(confidence_df$Timepoint)
confidence_df$Strain <- as.factor(confidence_df$Strain)

confidence_df$StrainSystem <- paste(confidence_df$Strain, "-", confidence_df$System, sep = " ")
confidence_df$StrainSystem <- as.factor(confidence_df$StrainSystem)

# ED50 bar plot for F003/H2 T1 System A and B with standard error 
b1 <- ggplot(data = confidence_df, aes(x=System, y=ED50, color = StrainSystem)) +  ###use StrainSystem for x in the future - for the moement it's fine since I anyway use everything alphabetically.
      geom_bar(stat = "identity", color = "black", width = 0.75, aes(fill = StrainSystem)) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), size = 0.7, width = 0.15, color = "black") +
      labs(y= "ED50", x = "CBASS System") +
      coord_cartesian(ylim = c(33.75,38.25)) +
      scale_fill_manual(name = "StrainSystem",
                    labels = c("F003 - A", "F003 - B", "H2 - A", "H2 - B"),
                    values = c("#DFC4DE","#91618E", "#FFE25A", "#FFB94F")) +
      facet_wrap(~Strain)

b1_plot <- b1 + theme (legend.position= "none", 
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
b1_plot

### ED50 regression curves T1 F003 + H2, System A and B ####
temp_x<- seq(30, 41, length = 100)
# prediction of the fitted values corresponding to the range of temperatures above
pred1<-lapply(mod1, function(x) predict(x, data.frame(Temperature = temp_x)))
pred_df<-as.data.frame(do.call(rbind, pred1))
colnames(pred_df)= round(temp_x, digits = 2)
pred_df$Sample=unique(input$Population)
pred_df_long=reshape2::melt(pred_df, id.vars=c("Sample"))
colnames(pred_df_long)[2:3]<-c("Temperature", "Fv/Fm")
pred_df_long=tidyr::separate(pred_df_long,Sample,into =c("System", "Timepoint", "Strain"),sep = "_",remove = FALSE,extra = "merge")  # , "Replicate"),sep = "_",remove = FALSE,extra = "merge")
pred_df_long$Timepoint=factor(pred_df_long$Timepoint, levels=c("1", "2", "3", "4", "5"))
pred_df_long$Temperature=as.numeric(as.character(pred_df_long$Temperature))
pred_df_long$group=paste(pred_df_long$System, pred_df_long$Timepoint, pred_df_long$Strain)

# calculate mean ED50 
ED50_means<-ed50_df %>% 
  group_by(System, Timepoint, Strain) %>%
  summarise(mean=mean(ED50), sd=sd(ED50)) %>%
  unite(Group, c(Timepoint), sep = "-", remove = FALSE)
ED50_means$group=paste(ED50_means$System, ED50_means$Timepoint, ED50_means$Strain)
pred_df_long$meanED50=round(ED50_means$mean[match(pred_df_long$group,ED50_means$group)], 2)
curve_input<-pred_df_long
input$Timepoint <- as.factor(input$Timepoint)
curve_input_meanED50s <- curve_input[c(1:4),]

### plot ED50 regression curves for F003/H2 and System A/B ###
curve_input$System <- gsub("A", "System A", curve_input$System)
curve_input_meanED50s$System <- gsub("A", "System A", curve_input_meanED50s$System)
input$System <- gsub("A", "System A", input$System)

curve_input$System <- gsub("B", "System B", curve_input$System)
curve_input_meanED50s$System <- gsub("B", "System B", curve_input_meanED50s$System)
input$System <- gsub("B", "System B", input$System)

curve_input$StrainSystem <- paste(curve_input$Strain, "-", curve_input$System, sep = " ")
curve_input_meanED50s$StrainSystem <- paste(curve_input_meanED50s$Strain, "-", curve_input_meanED50s$System, sep = " ")
input$StrainSystem <- paste(input$Strain, "-", input$System, sep = " ")

c1 <- ggplot() +
      geom_line(data = curve_input, aes(x=Temperature, y=`Fv/Fm`, group=Sample, color = factor(StrainSystem)), size = 1, show.legend = F) +
      geom_segment(data = curve_input, aes(x = meanED50, y = 0, xend = meanED50, yend = 0.65, color = StrainSystem), linetype=3, size=1, show.legend = F) +
      ggrepel::geom_text_repel(aes(x=meanED50, y=0.68, label=meanED50, color=StrainSystem, fontface = 2), data=curve_input_meanED50s, size=6, angle=90, hjust=0, max.overlaps= 10, direction = "x", point.size = NA, segment.color = NA, show.legend = F) +
      theme(panel.grid = element_blank(), legend.position="bottom", line= element_line(size = 0.5), axis.ticks.length = unit(0.1 , "cm")) +
      theme_classic() +
      scale_x_continuous(breaks=c(30, 34, 36, 39), limits = c(29.5,41), expand = c(0, 0)) +
      scale_y_continuous(limits = c(-0.02,0.75), expand = c(0, 0)) + labs(color='') +
      geom_jitter(data = input, aes(x = Temperature, y = PAM, group=StrainSystem, color=StrainSystem, shape=StrainSystem), size = 3, width = 0.25) +
      scale_shape_manual(name = "StrainSystem",
                        labels = c("F003 - System A", "F003 - System B", "H2 - System A", "H2 - System B"),
                        values = c(15, 0, 15, 0)) +
      scale_color_manual(name = "StrainSystem",
                     labels = c("F003 - System A", "F003 - System B", "H2 - System A", "H2 - System B"),
                     values = c("#DFC4DE","#91618E", "#FFE25A", "#FFB94F")) +
      labs(y= "Photosynthetic Efficiency (Fv/Fm)", x = "Temperature (°C)") +
      facet_nested(~ Strain, scales ="free_x" )

c1_plot <- c1 + theme (legend.position= "bottom", 
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
c1_plot

##### Repeat CBASS after 1 day and 1 month: T1 - T5 F003 + H2, System B #####

# read input file 
setwd("~/Desktop/PhD_VoolstraLab/CBASS_Reproducibility_Recycling/Input")
input <- read.table("CBASS_Aiptasia_T1-T5 + 39_Input.txt", sep = "\t", header = T)

# change temperature and PAM values to numeric 
input$Temperature=as.numeric(input$Temperature) 
input$PAM=as.numeric(input$PAM) 
# remove rows with missing data
input<-input[complete.cases(input), ]
# filter for System B only
input <- filter(input, System == "B")
# create a group to define population ID
input$Population=as.factor(paste(input$System,input$Timepoint,input$Strain, sep = "_")) 
levels(input$Population)
# drm demo to one population without temperature limits (helps to fit regression curve better to data points, limits can be used but need to be adjusted to data)
drm(PAM ~ Temperature, data=input[input$Population=="B_1_F003",],
    fct = LL.3(names = c('Slope', 'Max', 'ED50')))
# loop for all populations 
mod1<-lapply(unique(input$Population), 
             function(x) drm(PAM ~ Temperature, data=input[input$Population==x,],
                             fct = LL.3(names = c('Slope', 'Max', 'ED50'))))

# extract ED50s
ed50_list<-lapply(c(1:length(mod1)), function(x) mod1[[x]][["coefficients"]][["ED50:(Intercept)"]])
ed50_df<-as.data.frame(do.call(rbind, ed50_list))
ed50_df$Sample=unique(input$Population)
ed50_df=tidyr::separate(ed50_df,Sample,into =c("System", "Timepoint", "Strain" ),sep = "_",remove = FALSE,extra = "merge") #, "Replicate"),sep = "_",remove = FALSE,extra = "merge")
ed50_df$Timepoint=as.integer(ed50_df$Timepoint)
ed50_df$Strain=factor(ed50_df$Strain, levels = c("F003", "H2"))
colnames(ed50_df)[1]="ED50"

#### Barplot T1 - T5, F003 + H2, System B ####
# extract standard error from ED50 model
error.list <- lapply(c(1:length(mod1)), function(x) ED(mod1[[x]], 50))

confidence_df <- as.data.frame(do.call(rbind, error.list))
confidence_df$Sample=unique(input$Population)
confidence_df$ED50=unique(input$Population)
confidence_df <- data.frame(ED50=confidence_df$Estimate, Error = confidence_df$`Std. Error`, Sample = confidence_df$Sample)
confidence_df <- tidyr::separate(confidence_df,Sample,into =c("System", "Timepoint", "Strain" ),sep = "_",remove = FALSE,extra = "merge")

confidence_df$Upper <- confidence_df$ED50 + confidence_df$Error
confidence_df$Lower <- confidence_df$ED50 - confidence_df$Error

confidence_df$Timepoint <- as.integer(confidence_df$Timepoint)
confidence_df$Strain <- as.factor(confidence_df$Strain)

confidence_df$StrainSystem <- paste(confidence_df$Strain, "-", confidence_df$System, sep = " ")
confidence_df$StrainSystem <- as.factor(confidence_df$StrainSystem)

confidence_df <- confidence_df %>% # run code for manuscript figure 2. Comment out for Supplement Figure S4.
  filter(Timepoint != "2") %>%
  filter(Timepoint != "4")

# ED50 bar plot with standard error 
b2 <- ggplot(data = confidence_df, aes(x=Timepoint, y=ED50, color = StrainSystem)) +
      geom_bar(stat = "identity", color = "black", width = 1.25, aes(fill = StrainSystem)) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), size = 0.7, width = 0.15, color = "black") +
      labs(y= "ED50", x = "PAM Measurement Timepoints") +
      coord_cartesian(ylim = c(33.75,38.25)) +
      scale_x_continuous(breaks = c(1, 3, 5)) +
      scale_fill_manual(name = "StrainSystem",
                    labels = c("F003 - B", "H2 - B"),
                    values = c("#91618E", "#FFB94F")) +
      facet_wrap(~Strain)

b2_plot <- b2 + theme (legend.position= "none", 
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
b2_plot

#### ED50 regression curves T1 - T5, F003 + H2, System B #####
temp_x<- seq(30, 41, length = 100)
# prediction of the fitted values corresponding to the range of temperatures above
pred1<-lapply(mod1, function(x) predict(x, data.frame(Temperature = temp_x)))
pred_df<-as.data.frame(do.call(rbind, pred1))
colnames(pred_df)= round(temp_x, digits = 2)
pred_df$Sample=unique(input$Population)
pred_df_long=reshape2::melt(pred_df, id.vars=c("Sample"))
colnames(pred_df_long)[2:3]<-c("Temperature", "Fv/Fm")
pred_df_long=tidyr::separate(pred_df_long,Sample,into =c("System", "Timepoint", "Strain"),sep = "_",remove = FALSE,extra = "merge")  # , "Replicate"),sep = "_",remove = FALSE,extra = "merge")
pred_df_long$Timepoint=factor(pred_df_long$Timepoint, levels=c("1", "2", "3", "4", "5"))
pred_df_long$Temperature=as.numeric(as.character(pred_df_long$Temperature))
pred_df_long$group=paste(pred_df_long$System, pred_df_long$Timepoint, pred_df_long$Strain)

# calculate mean ED50 per Timepoint
ED50_means<-ed50_df %>% 
  group_by(System, Timepoint, Strain) %>%
  summarise(mean=mean(ED50), sd=sd(ED50)) %>%
  unite(Group, c(Timepoint), sep = "-", remove = FALSE)
ED50_means$group=paste(ED50_means$System, ED50_means$Timepoint, ED50_means$Strain)

pred_df_long$meanED50=round(ED50_means$mean[match(pred_df_long$group,ED50_means$group)], 2)

curve_input<-pred_df_long
input$Timepoint <- as.factor(input$Timepoint)
curve_input_meanED50s <- curve_input[c(1:10),]

### plot ED50 regression curves T1, T3, T5 F003/H2 and System B ###
curve_input$System <- gsub("B", "System B", curve_input$System)
curve_input_meanED50s$System <- gsub("B", "System B", curve_input_meanED50s$System)
input$System <- gsub("B", "System B", input$System)

input <- input %>% 
          filter(Timepoint != "2") %>%
          filter(Timepoint != "4")

curve_input <- curve_input %>% 
                filter(Timepoint != "2") %>%
                filter(Timepoint != "4")

curve_input_meanED50s <- curve_input_meanED50s %>%
                          filter(Timepoint != "2") %>%
                          filter(Timepoint != "4")

curve_input$StrainTimepoint <- paste(curve_input$Strain, "-", curve_input$Timepoint, sep = " ")
curve_input_meanED50s$StrainTimepoint <- paste(curve_input_meanED50s$Strain, "-", curve_input_meanED50s$Timepoint, sep = " ")
input$StrainTimepoint <- paste(input$Strain, "-", input$Timepoint, sep = " ")

c2 <- ggplot() +
      geom_line(data = curve_input, aes(x=Temperature, y=`Fv/Fm`, group=Sample, color = factor(StrainTimepoint)), size = 1, show.legend = F) +
      geom_segment(data = curve_input, aes(x = meanED50, y = 0, xend = meanED50, yend = 0.65, color = StrainTimepoint), linetype=3, size=1, show.legend = F) +
      ggrepel::geom_text_repel(aes(x=meanED50, y=0.68, label=meanED50, color=StrainTimepoint, fontface = 2), data=curve_input_meanED50s, size=6, angle=90, hjust=0, max.overlaps= 10, direction = "x", point.size = NA, segment.color = NA, show.legend = F) +
      theme(panel.grid = element_blank(), legend.position="bottom", line= element_line(size = 0.5), axis.ticks.length = unit(0.1 , "cm")) +
      theme_classic() +
      scale_x_continuous(breaks=c(30, 34, 36, 39), limits = c(29.5,41), expand = c(0, 0)) +
      scale_y_continuous(limits = c(-0.02,0.75), expand = c(0, 0)) + labs(color='') +
      geom_jitter(data = input, aes(x = Temperature, y = PAM, group=StrainTimepoint, color=StrainTimepoint, shape=StrainTimepoint), size = 3, width = 0.25) +
      scale_color_manual(name = "StrainTimepoint",
                     labels = c("F003 - 1", "F003 - 3", "F003 - 5", "H2 - 1", "H2 - 3", "H2 - 5"),
                     values = c("#91618E", "#C68FC5", "#F89CC5", "#FFB94F", "#FC9E60", "#FFE25A")) +
      scale_shape_manual(name = "StrainTimepoint",
                     labels = c("F003 - 1", "F003 - 3", "F003 - 5", "H2 - 1", "H2 - 3", "H2 - 5"),
                     values = c(15, 16, 18, 15, 16, 18)) +
      labs(y= "Photosynthetic Efficiency (Fv/Fm)", x = "Temperature") +
      facet_wrap(Strain~System)

c2_plot <- c2 + theme (legend.position= "bottom", 
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
c2_plot
