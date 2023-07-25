# Polar chart


# Creating Polar chart
df_grp_polar <- df %>% group_by(MonthName,violationsn) %>% 
  summarise(accidentsNum= n(),
            .groups = 'drop') %>%
  as.data.frame()
View(df_grp_polar)

# Look up unique values of violationsn
unique(df_grp_polar$violationsn)

# renaming too long values
df_grp_polar$violationsn[df_grp_polar$violationsn=="Driving a vehicle by a person who does not have the right to drive a vehicle"]<-"Driver isn't allowed to drive"
df_grp_polar$violationsn[df_grp_polar$violationsn=="Failure to provide an advantage in the movement of a vehicle that has special color schemes, inscriptions and designations applied to the outer surfaces, with a blue flashing beacon and a special sound signal turned on at the same time"]<-"Driver did not let the special transport pass"
df_grp_polar$violationsn[df_grp_polar$violationsn=="Driving a vehicle in the presence of malfunctions or conditions under which the operation of the vehicle is prohibited"]<-"Driving broken car"
unique(df_grp_polar$violationsn)

# grouping data
df_polar <- df_grp_polar[order(df_grp_polar, decreasing = TRUE), ]
df_polar <- Reduce(rbind,                                 # Top N highest values by group
                    by(df_polar,
                       df_polar["MonthName"],
                       head,
                       n = 1))
df_grp_polar <- df_grp_polar[order(df_grp_polar$MonthName,df_grp_polar$accidentsNum),]
View(df_grp_polar)

# creating stacked bar chart
stacked <- ggplot(df_grp_polar, aes(x = factor(MonthName), y = accidentsNum, fill = violationsn)) + 
  geom_bar(stat = "identity", colour="darkorange", position = position_stack()) + 
  geom_text(size = 2, aes(label = accidentsNum), position = position_stack(vjust = 0.5),colour = "Black") +
  geom_col(colour = "white", linewidth = 1.5) + 
  scale_fill_brewer(palette = "Spectral") + 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_markdown(color = "Cyan"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "black"), 
        panel.background = element_rect(fill = "black"),
        #legend.text = element_markdown(color = "white"),
        legend.background = element_rect(fill = "black"),
  )
stacked
# creating stacked polar chart

stacked + coord_curvedpolar()
  theme(
    plot.background = element_rect(fill = "Cyan"), 
        panel.background = element_rect(fill = "Cyan"),
        legend.background = element_rect(fill = "Cyan")
    ) +
  coord_curvedpolar()

  # step 1: creating stacked bar chart
  stacked <- ggplot(df_grp_polar, aes(x = MonthName, y = accidentsNum, fill = violationsn)) + 
    geom_bar(stat = "identity", position = position_stack()) + 
    geom_text(size = 2, aes(label = accidentsNum), position = position_stack(vjust = 0.5),colour = "Black") +
    geom_col(colour = "white", linewidth = 1.5) +   
    scale_fill_brewer(palette = "Spectral") + 
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_markdown(color = "black"),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.background = element_rect(fill = "deepskyblue1"), 
          panel.background = element_rect(fill = "deepskyblue1"),
          legend.background = element_rect(fill = "deepskyblue1")
          #legend.text = element_markdown(color = "black") # this line makes the chart dizzy :(
          )
  stacked
  
  # step 2: creating stacked polar chart
  margins = c(2, 5, 2, 5)
  stacked + theme_bw() + theme(
    
    plot.background = element_rect(fill = "Cyan"), 
      panel.background = element_rect(fill = "Cyan"),
      legend.background = element_rect(fill = "Cyan"),
    plot.margin = unit(margins, "pt")
    
    ) + coord_curvedpolar()
  

