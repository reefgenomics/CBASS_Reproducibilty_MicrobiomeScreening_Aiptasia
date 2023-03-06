##### Loading libraries ####
library(drc)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(stringr)
library(ggh4x)

#### T1 H2 + F003 anemones Control vs. Vibrio inoculation ####

# read input file
setwd("~/Desktop/PhD_VoolstraLab/CBASS_Reproducibility_Recycling/Submission/Input_Scripts/VibrioInoculation")
input <- read.table("AipCBASS_FvFm_VibrioInoculation_Input.txt", sep = "\t", header = T)
# change temperature and PAM values to numeric 
input$Temperature=as.numeric(input$Temperature) 
input$PAM=as.numeric(input$PAM) 
# remove rows with missing data
input<-input[complete.cases(input), ]
# create a group to define population ID
input$Population=as.factor(paste(input$System,input$Timepoint,input$Strain, input$Treatment, sep = "_")) #,input$Replicate, sep = "_")) # replicate per treatment per strain
levels(input$Population)
# drm demo to one population without limits (helps to fit regression curve better to data points, limits can be used but need to be adjusted to data)
drm(PAM ~ Temperature, data=input[input$Population=="B_1_F003_Vibrio",],
    fct = LL.3(names = c('Slope', 'Max', 'ED50')))
# loop for all populations
mod1<-lapply(unique(input$Population), 
             function(x) drm(PAM ~ Temperature, data=input[input$Population==x,],
                             fct = LL.3(names = c('Slope', 'Max', 'ED50'))))
# extract ED50s
ed50_list<-lapply(c(1:length(mod1)), function(x) mod1[[x]][["coefficients"]][["ED50:(Intercept)"]])
ed50_df<-as.data.frame(do.call(rbind, ed50_list))
ed50_df$Sample=unique(input$Population)
ed50_df=tidyr::separate(ed50_df,Sample,into =c("System", "Timepoint", "Strain", "Treatment" ),sep = "_",remove = FALSE,extra = "merge") #, "Replicate"),sep = "_",remove = FALSE,extra = "merge")
ed50_df$Timepoint=as.integer(ed50_df$Timepoint)
ed50_df$Strain=factor(ed50_df$Strain, levels = c("F003", "H2"))
colnames(ed50_df)[1]="ED50"

#### Barplot T1 F003 + H2 vs. Vibrio ####
# extract standard error from ED50 model
error.list <- lapply(c(1:length(mod1)), function(x) ED(mod1[[x]], 50))

confidence_df <- as.data.frame(do.call(rbind, error.list))
confidence_df$Sample=unique(input$Population)
confidence_df$ED50=unique(input$Population)
confidence_df <- data.frame(ED50=confidence_df$Estimate, Error = confidence_df$`Std. Error`, Sample = confidence_df$Sample)
confidence_df <- tidyr::separate(confidence_df,Sample,into =c("System", "Timepoint", "Strain", "Treatment" ),sep = "_",remove = FALSE,extra = "merge")
confidence_df$Upper <- confidence_df$ED50 + confidence_df$Error
confidence_df$Lower <- confidence_df$ED50 - confidence_df$Error

confidence_df$Timepoint <- as.integer(confidence_df$Timepoint)
confidence_df$Strain <- as.factor(confidence_df$Strain)
# filter for System B
confidence_df_B <- filter(confidence_df, System == "B")
# adjust dataframe for plotting 
confidence_df_B$StrainTreatment <- paste(confidence_df_B$Strain, "-", confidence_df_B$Treatment, sep = " ")
confidence_df_B$StrainTreatment <- as.factor(confidence_df_B$StrainTreatment)

# ED50 bar plot with standard error 
b3 <- ggplot(data = confidence_df_B, aes(x=Treatment, y=ED50, color = StrainTreatment)) +
      geom_bar(stat = "identity", color = "black", width = 0.75, aes(fill = StrainTreatment)) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), size = 0.7, width = 0.15, color = "black") +
      labs(y= "ED50", x = "Treatment") +
      coord_cartesian(ylim = c(33.75,38.25)) +
      scale_fill_manual(name = "StrainTreatment",
                     labels = c("F003 - Control", "F003 - Vibrio", "H2 - Control", "H2 - Vibrio"),
                     values = c("#916A8E", "#ADD8E6", "#FFB94F", "#ADD8E6")) +
      facet_wrap(~Strain)

b3_plot <- b3 + theme (legend.position= "none", 
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
b3_plot

### ED50 regression curves T1 F003 + H2 Control vs. Vibrio ####
temp_x<- seq(30, 41, length = 100)
# prediction of the fitted values corresponding to the range of temperatures above
pred1<-lapply(mod1, function(x) predict(x, data.frame(Temperature = temp_x)))
pred_df<-as.data.frame(do.call(rbind, pred1))
colnames(pred_df)= round(temp_x, digits = 2)
pred_df$Sample=unique(input$Population)
pred_df_long=reshape2::melt(pred_df, id.vars=c("Sample"))
colnames(pred_df_long)[2:3]<-c("Temperature", "Fv/Fm")
pred_df_long=tidyr::separate(pred_df_long,Sample,into =c("System", "Timepoint", "Strain", "Treatment"),sep = "_",remove = FALSE,extra = "merge")  # , "Replicate"),sep = "_",remove = FALSE,extra = "merge")
pred_df_long$Timepoint=factor(pred_df_long$Timepoint, levels=c("1", "2", "3", "4", "5"))
pred_df_long$Temperature=as.numeric(as.character(pred_df_long$Temperature))
pred_df_long$Treatment=factor(pred_df_long$Treatment)
pred_df_long$group=paste(pred_df_long$System, pred_df_long$Timepoint, pred_df_long$Strain, pred_df_long$Treatment)

# calculate mean ED50 per Treatment
ED50_means<-ed50_df %>% 
  group_by(System, Timepoint, Strain, Treatment) %>%
  summarise(mean=mean(ED50), sd=sd(ED50)) %>%
  unite(Group, c(Timepoint), sep = "-", remove = FALSE)
ED50_means$group=paste(ED50_means$System, ED50_means$Timepoint, ED50_means$Strain, ED50_means$Treatment)

pred_df_long$meanED50=round(ED50_means$mean[match(pred_df_long$group,ED50_means$group)], 2)

curve_input<-pred_df_long

# filter for System B only 
input_B <- filter(input, System == "B")
curve_input_B <- filter(curve_input, System == "B")
curve_input_meanED50s_B <- curve_input_B[c(1:4),]

input_B$StrainTreatment <- paste(input_B$Strain, "-", input_B$Treatment, sep = " ")
curve_input_B$StrainTreatment <- paste(curve_input_B$Strain, "-", curve_input_B$Treatment, sep = " ")
curve_input_meanED50s_B$StrainTreatment <- paste(curve_input_meanED50s_B$Strain, "-", curve_input_meanED50s_B$Treatment, sep = " ")

### plot ED50 regression curves F003/H2 Control vs. Vibrio inoculation ###
c3 <- ggplot() +
      geom_line(data = curve_input_B, aes(x=Temperature, y=`Fv/Fm`, group=Sample, color = factor(StrainTreatment)), size = 1, show.legend = F) +
      geom_segment(data = curve_input_B, aes(x = meanED50, y = 0, xend = meanED50, yend = 0.65, color = StrainTreatment), linetype=3, size=1, show.legend = F) +
      ggrepel::geom_text_repel(aes(x=meanED50, y=0.68, label=meanED50, color=StrainTreatment, fontface = 2), data=curve_input_meanED50s_B, size=6, angle=90, hjust=0, max.overlaps= 10, direction = "x", point.size = NA, segment.color = NA, show.legend = F) +
      theme(panel.grid = element_blank(), legend.position="bottom", line= element_line(size = 0.5), axis.ticks.length = unit(0.1 , "cm")) +
      theme_classic() +
      scale_x_continuous(breaks=c(30, 34, 36, 39), limits = c(29.5,41), expand = c(0, 0)) +
      scale_y_continuous(limits = c(-0.02,0.75), expand = c(0, 0)) + labs(color='') +
      geom_jitter(data = input_B, aes(x = Temperature, y = PAM, group=StrainTreatment, color=StrainTreatment, shape=StrainTreatment), size = 3, width = 0.25) +
      scale_color_manual(name = "StrainTreatment",
                     labels = c("F003 - Control", "F003 - Vibrio", "H2 - Control", "H2 - Vibrio"),
                     values = c("#A46A8C", "#ADD8E6", "#FFB94F", "#ADD8E6")) +
      scale_shape_manual(name = "StrainTreatment",
                     labels = c("F003 - Control", "F003 - Vibrio", "H2 - Control", "H2 - Vibrio"),
                     values = c(15, 16, 15, 16)) +
      labs(y= "Photosynthetic Efficiency (Fv/Fm)", x = "Treatment") +
      facet_grid(~Strain)

c3_plot <- c3 + theme (legend.position= "bottom", 
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
c3_plot 
