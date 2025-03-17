#Maxin et al., 2025 Nassau Grouper Manuscript Code#

#####Basics#####
  
  #Packages#
  library(readr)
  library(tidyverse)
  library(lubridate)
  library(ggplot2)
  library(hms)
  library(lunar)
  library(stringr)
  library(patchwork)
  library(MASS)
  library(emmeans)
  library(nlme)
  library(viridis)
  library(rcartocolor)
  library(ggforce)
  library(ggrepel)

  #Working Directory#
  setwd("E:/Thesis Project/NassauGrouper")
  
#### Temporal Linear Models & Associated Figures#### 

  #####Figure 2 Model#####
    
  #Context: Figure 2 depicts the log transformed linear model of the 
  #call rate across the study period (2014 to 2024)
  
    ##Clear workspace
    rm(list = ls())
  
    ##Import data
    all_filter_1<- read.csv("all_filter_1.csv")
  
    ##Make a new dataframe (af) with the mean number of calls in each hour
    ##for every year in the study period
    af<- all_filter_1 %>% 
    group_by(date_hour) %>% 
    summarize(avg_cpm = mean(calls_per_minute),
              year= first(year))

    ##Create the linear regression (lmcall_1)
    lmcall_1 <- lm(log(avg_cpm) ~ year, data = af) 
    summary(lmcall_1) 
    
    ##Making linear model + predicted values
      #Linear model (fitlm1)
      fitlm1 <- lm(formula = log(avg_cpm) ~ year, data = af)
      
      #Predicted values for the model
      af$predlm <- predict(fitlm1)
      predlm_ci <- predict(fitlm1, interval = "confidence")
        
      #Back transforming because model is log transformed
      predlm_ci <- exp(predlm_ci)
  
    ##Making a dataframe with predictions
    
      ##New dataframe (predictions_df)
      predictions_df<-data.frame(
      predlm=af$predlm,
      fit = predlm_ci[, "fit"],
      lwr = predlm_ci[, "lwr"],
      upr = predlm_ci[, "upr"])
    
      ##Merging the two dataframes
      af<-merge(af, predictions_df, by = "predlm")
    
    #####Figure 2 Plot#####
  
      figure2<-ggplot(af, aes(x = year, y = avg_cpm) ) +
      geom_jitter(alpha=0.2, width= 0.3, color= "#00B8E7") +
      geom_line(aes(y = exp(predlm), group=1), linewidth = 1) + 
      geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2)+
      labs(x = "Year", y = "Average call rate (calls per minute)") +
      scale_x_continuous(breaks = seq(2013, 2024, by = 1))+
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(),
            panel.background = element_blank()) 
    
      figure2<-figure2+theme(text=element_text(size=15))
    
    #####Figure 3 Model#####
  
    #Figure 3 shows the linear relationship between the maximum Nassau 
    #grouper count & the maximum call rate (calls/minute)
    
      ##Clear workspace
      rm(list = ls())

      ##Import call rate data
      all_filter_1<- read.csv("all_filter_1.csv")
  
      ##Make a new dataframe (gcd) with the maximum call rate 
      ##for each year in the study period
      gcd<- all_filter_1 %>% 
      group_by(year) %>% 
      summarize(max_calls = max(calls_per_minute))
        
        #Remove 2015 due to lack of corresponding UVC data
        gcd<-gcd[-c(3), ]
     
      ##Import grouper count data
      grouper_count_data<-read.csv("grouper_count_data.csv")
        
        #Remove weird 1st column 
        grouper_count_data<-grouper_count_data[, -c(1,4)]
        
        #Merge maximum call rate & maximum grouper count data
        grouper_count_data <- merge(grouper_count_data, gcd, by="year")
  
      ##Create linear model  
      max_model_1 <- lm(max_count ~ max_calls, data = grouper_count_data) 
      summary(max_model_1)

      #####Figure 3 plot#####
      figure3<- ggplot(data=grouper_count_data, aes(x= max_count,y = max_calls))+
      geom_point()+
      geom_smooth(method = lm)+
      labs(y = " Maximum call rate (calls per minute)",
           x = "Maximum Nassau Grouper Count")+theme_bw()

      figure3<-figure3+theme(text=element_text(size=15))
  
      figure3all<-figure3+theme(text=element_text(size=15))+ 
                  geom_text_repel(data=grouper_count_data, aes(label=year), 
                  size=5, hjust=1.15, vjust=0)

#### Spatial Figures ####
  
  ##### Figure 4 #####
  ##Figure 4 shows the maximum call rate per hour 
  
  ##Clear workspace
  rm(list = ls())
  
  ##Import data
  spaceman_hourly_all<-read.csv("spaceman_hourly_all.csv")
  
  ##Notch & saddle dataframes
    
    #2021 Dataframe
    A21<- spaceman_hourly_all %>% 
    filter(spaceman_hourly_all$year== "2021") %>% 
    add_row(location = "fringe")
      
    #Ordering the locations for plotting
    A21$location <- factor(A21$location , levels=c("notch", "saddle", "fringe"))
  
    #2024 Dataframe
    A24<-spaceman_hourly_all %>% 
    filter(spaceman_hourly_all$year== "2024") %>% 
    add_row(location = "fringe")
  
    #Ordering the locations for plotting
    A24$location <- factor(A24$location , levels=c("notch", "saddle", "fringe"))
  
  ##Notch & saddle plots
    
    #2021 plot
     plot_A21<- A21 %>%
        ggplot(aes(x=location, y=max_grouper, fill=location)) +
        ggtitle("2021")+
        geom_boxplot() +
        scale_fill_manual(values = c("#b6c8b9","#2987a1")) +
        annotate(
         'text',
         x = 'fringe',
         y = 1.1,
         label = 'n.d.',
         size = 4.5
        )+
        xlab("Location")+
        ylab("Average call rate (calls minute-1)")+ ylim(0,3)+labs(fill= "Location")+theme_bw()+
        theme(text=element_text(size=15))
  
    #2024 plot
     plot_A24<- A24 %>%
        ggplot(aes(x=location, y=max_grouper, fill=location)) +
        ggtitle("2024")+
        geom_boxplot() +
        scale_fill_manual(values = c("#b6c8b9","#2987a1")) +
        annotate(
         'text',
         x = 'fringe',
         y = 1.1,
         label = 'n.d.',
         size = 4.5
        )+
        xlab("Location")+
        ylab("Average call rate (calls minute-1)")+ ylim(0,3)+labs(fill= "Location")+theme_bw()+
        theme(text=element_text(size=15))
  
     #Putting the plots together
     plot3<-plot_A21 / plot_A24 
     
  ##Notch, saddle, & fringe dataframe 
    
    #2019 dataframe
    A19<- spaceman_hourly_all %>% 
    filter(spaceman_hourly_all$year== "2019") 
    
    #Ordering the locations for plotting 
    A19$location <- factor(A19$location , levels=c("notch", "saddle", "fringe"))
    
    ##2019 plot
    plot_A19<- A19 %>%
      ggplot(aes(x=location, y=max_grouper, fill=location)) +
      ggtitle("2019")+
      geom_boxplot() +
      scale_fill_manual(values = c("#b6c8b9","#2987a1","#aa7537")) +
      xlab("Location")+
      ylab("Average call rate (calls minute-1)")+ labs(fill= "Location")+
      theme_bw()+
      theme(text=element_text(size=15))
    
    #Re-namimg plot for order clarity
    plot2 <- plot_A19 
    
  #Saddle & fringe dataframes
    
    #2015 dataframe
    B15<- spaceman_hourly_all %>% 
    filter(spaceman_hourly_all$year== "2015") %>% 
    add_row(location = "notch")
  
    #Ordering the locations for plotting 
    B15$location <- factor(B15$location , levels=c("notch", "saddle", "fringe"))
  
    #2017 dataframe
    B17<- spaceman_hourly_all %>% 
    filter(spaceman_hourly_all$year== "2017") %>% 
    add_row(location = "notch")
  
    #Ordering the locations for plotting
    B17$location <- factor(B17$location , levels=c("notch", "saddle", "fringe"))
  
  
  ##Saddle + fringe plots
    
    #2015 plot
    plot_B15<- B15 %>%
    ggplot(aes(x=location, y=max_grouper, fill=location)) +
    ggtitle("2015")+
    geom_boxplot() +
    scale_fill_manual(values = c("#2987a1", "#aa7537" )) +
    annotate(
      'text',
      x = 'notch',
      y = 1.1,
      label = 'n.d.',
      size = 4.5
    )+
    ylim(0,3)+
    xlab("Location")+
    ylab("Average call rate (calls minute-1)")+ labs(fill= "Location")+theme_bw()+
    theme(text=element_text(size=15))
  
    #2017 plot
    plot_B17<- B17 %>%
    ggplot(aes(x=location, y=max_grouper, fill=location)) +
    ggtitle("2017")+
    geom_boxplot() +
    scale_fill_manual(values = c("#2987a1", "#aa7537" )) +
    annotate(
      'text',
      x = 'notch',
      y = 1.1,
      label = 'n.d.',
      size = 4.5
    )+
    ylim(0,3) +
    xlab("Location")+
    ylab("Average call rate (calls minute-1)")+ labs(fill= "Location")+theme_bw()+
    theme(text=element_text(size=15))
  
  ##Putting the saddle & fringe plots together
  plot1<-plot_B15 / plot_B17 
  
  ##Putting all of the plots together 
  figure4<- plot_B15 / plot_B17 / plot_A19 / plot_A21 / plot_A24
  
  figure4+plot_annotation(tag_levels = 'A')+ plot_layout(axis_titles = "collect")
  
  ##### Figure 5 #####  
  
  ##Clear workspace
  rm(list = ls())
  
  ##Import data
  spaceman_hourly_all<-read.csv("spaceman_hourly_5.csv")
  
  ##A) Overall trend at the saddle (all years minus 2024)
  
    #Filter out 2024
    spaceman_hourly_not24<- spaceman_hourly_all %>% 
    filter(spaceman_hourly_all$year!= "2024")
  
    #Filter out non-saddle sites
    spaceman_hourly_not24<- spaceman_hourly_not24 %>% 
    filter(spaceman_hourly_not24$location== "saddle") 
  
    #Making a column (total) with max calls/minute 
    spaceman_hourly_not24<- spaceman_hourly_not24 %>% 
    group_by(hour, location, year) %>% 
    summarise(total = max(calls_per_minute))
  
    #Making a column of the standard error of total
    spaceman_hourly_not24_sd <- spaceman_hourly_not24 %>% 
    group_by(location, hour) %>%      
    summarise(n= n(),
              mean= mean(total),
              se = sd(total)/sqrt(length(mean)))
    
    #Making the hour column a character for plotting 
    spaceman_hourly_not24$hour<-as.character(spaceman_hourly_not24$hour)
    spaceman_hourly_not24_sd$hour<-as.character(spaceman_hourly_not24_sd$hour)
  
    #Plot 5A
    n24test_line<-ggplot()+
    geom_line(aes(x = as.numeric(hour), y = mean, color= location, 
                  group=location), data = spaceman_hourly_not24_sd, 
              linewidth=1)+
    geom_ribbon(data = spaceman_hourly_not24_sd, 
                aes(x = as.numeric(hour), ymin = mean - se, ymax = mean + se, 
                    fill = location), 
                alpha = 0.2)+
    labs(x = "Hour",
         y = "Maximum hourly call rate 
      (calls per minute)
      averaged by year", 
         color= "Location",
         fill = "Location")+ 
    geom_point(data = spaceman_hourly_not24, 
               aes(x = as.numeric(hour), y = total, color= location),
               alpha = 0.7)+
    #ylim(-1, 7)+
    scale_color_manual(values = c("#2987a1"))+ 
    scale_fill_manual(values = c("#2987a1"))+ 
    theme_bw()+ ggtitle("2014-2023")
  
  ## Notch & Saddle plots:5B,5C,5D (2019,2021,2024)
  ## B) 2019
    #Filter for 2019
    spaceman_hourly_19<- spaceman_hourly_all %>% 
      filter(spaceman_hourly_all$year== "2019")
    
    #Filter out the fringe
    spaceman_hourly_19<- spaceman_hourly_19 %>% 
      filter(spaceman_hourly_19$location!= "fringe")
    
    #Making a column (total) with max calls/minute 
    spaceman_hourly_19<- spaceman_hourly_19 %>% 
      group_by(hour, location, year) %>% 
      summarise(max = max(calls_per_minute))
    
    #Plot 5B
    t19test_line<-ggplot(data = spaceman_hourly_19, 
                         aes(x = hour, y = max, color= location))+
      geom_line(aes(x = hour, y = max, color= location, 
                    group=location), data = spaceman_hourly_19, 
                linewidth=1)+
      labs(x = "Hour", 
           y = "Maximum hourly call rate 
      (calls per minute)",
           color= "Location")+ 
      ylim(-1, 7)+
      scale_color_manual(values = c("#b6c8b9","#2987a1"))+ 
      theme_bw()+ ggtitle("2019")  
    
  ## C) 2021    
    #Filter for 2021
    spaceman_hourly_21<- spaceman_hourly_all %>% 
      filter(spaceman_hourly_all$year== "2021")
    
    #Making a column (total) with max calls/minute 
    spaceman_hourly_21<- spaceman_hourly_21 %>% 
      group_by(hour, location, year) %>% 
      summarise(max = max(calls_per_minute))
    
    #Plot 5C
    t21test_line<-ggplot(data = spaceman_hourly_21, 
                         aes(x = hour, y = max, color= location))+
      geom_line(aes(x = hour, y = max, color= location, 
                    group=location), data = spaceman_hourly_21, linewidth=1)+
      labs(x = "Hour", 
           y = "Maximum hourly call rate 
    (calls per minute)",
           color= "Location")+ 
      ylim(-1, 7)+
      scale_color_manual(values = c("#b6c8b9","#2987a1"))+ 
      theme_bw()+ ggtitle("2021")     
    
    
  ## D) 2024 
    
    #Filter for 2024
    spaceman_hourly_24<- spaceman_hourly_all %>% 
      filter(spaceman_hourly_all$year== "2024")
    
    #Making a column (total) with max calls/minute
    spaceman_hourly_24<- spaceman_hourly_24 %>% 
      group_by(hour, location, year) %>% 
      summarise(max = max(calls_per_minute))
    
    #Plot 5D
    t24test_line<-ggplot(data = spaceman_hourly_24, 
                         aes(x = hour, y = max, color= location))+
      geom_line(aes(x = hour, y = max, color= location, 
                    group=location), data = spaceman_hourly_24, linewidth=1)+
      labs(x = "Hour", 
           y = "Maximum hourly call rate 
    (calls per minute)",
           color= "Location")+ 
      ylim(-1, 7)+
      scale_color_manual(values = c("#b6c8b9","#2987a1"))+ 
      theme_bw()+ ggtitle("2024") 
  
  ## Saddle & Fringe plots:5E,5F,5G (2015, 2017, 2019)
  ## E) 2015    
  
    #Filter for 2015
    spaceman_hourly_15<- spaceman_hourly_all %>% 
    filter(spaceman_hourly_all$year== "2015")
  
    #Making a column (total) with max calls/minute 
    spaceman_hourly_15<- spaceman_hourly_15 %>% 
    group_by(hour, location, year) %>% 
    summarise(max = max(calls_per_minute))
  
    #Plot 5E
    t15test_line<-ggplot(data = spaceman_hourly_15, 
                       aes(x = hour, y = max, color= location))+
    geom_line(aes(x = hour, y = max, color= location, 
                  group=location), data = spaceman_hourly_15, 
              linewidth=1)+
    labs(x = "Hour", 
         y = "Maximum hourly call rate 
    (calls per minute)",
         color= "Location")+ 
    ylim(-1, 7)+
    scale_color_manual(values = c("#aa7537", "#2987a1"))+ 
    theme_bw()+ ggtitle("2015")
  
  ## F) 2017    
  
    #Filter for 2017
    spaceman_hourly_17<- spaceman_hourly_all %>% 
    filter(spaceman_hourly_all$year== "2017")
  
    #Making a column (total) with max calls/minute 
    spaceman_hourly_17<- spaceman_hourly_17 %>% 
    group_by(hour, location, year) %>% 
    summarise(max = max(calls_per_minute))
  
    #Plot 5F
    t17test_line<-ggplot(data = spaceman_hourly_17, 
                       aes(x = hour, y = max, color= location))+
    geom_line(aes(x = hour, y = max, color= location, 
                  group=location), data = spaceman_hourly_17, 
              linewidth=1)+
    labs(x = "Hour", 
         y = "Maximum hourly call rate 
    (calls per minute)",
         color= "Location")+ 
    ylim(-1, 7)+
    scale_color_manual(values = c("#aa7537","#2987a1"))+ 
    theme_bw()+ ggtitle("2017")    
  
  ## G) 2019 (saddle & fringe)
  
    #Filter for 2019
    spaceman_hourly_19sf<- spaceman_hourly_all %>% 
      filter(spaceman_hourly_all$year== "2019")
    
    #Filter out the notch
    spaceman_hourly_19sf<- spaceman_hourly_19sf %>% 
      filter(spaceman_hourly_19sf$location!= "notch")
    
    #Making a column (total) with max calls/minute 
    spaceman_hourly_19sf<- spaceman_hourly_19sf %>% 
      group_by(hour, location, year) %>% 
      summarise(max = max(calls_per_minute))
    
    #Plot 5G
    t19sftest_line<-ggplot(data = spaceman_hourly_19sf, 
                           aes(x = hour, y = max, color= location))+
      geom_line(aes(x = hour, y = max, color= location, 
                    group=location), data = spaceman_hourly_19sf, 
                linewidth=1)+
      labs(x = "Hour", 
           y = "Maximum hourly call rate 
    (calls per minute)",
           color= "Location")+ 
      ylim(-1, 7)+
      scale_color_manual(values = c("#aa7537","#2987a1"))+ 
      theme_bw()+ ggtitle("2019") 
    
  ## Combine all plots (5A-5G)
    
    ## Patchwork plots together
    figure2024_line<- n24test_line/
    (t19test_line|t21test_line|t24test_line)/
    (t15test_line|t17test_line|t19sftest_line)
  
    ## Add labels
    figure2024_line+plot_annotation(tag_levels = 'A')+ 
    plot_layout(guides = "collect") &
    theme(legend.position = "right")
  
  