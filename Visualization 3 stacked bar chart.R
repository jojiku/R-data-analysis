# Stacked bar chart

# First, let's find unique values of weather column
unique(df$`weather conditions`)

# Getting rid of other columns
df_stackedbar = subset(df, select = c(`weather conditions`, NInjured, NFatal))
View(df_stackedbar)

# Getting overall number of accidents (rows) & fatal cases & injuries by light conditions
df_stackedbar = df_stackedbar %>% 
  group_by(`weather conditions`) %>% summarize(injuries = sum(NInjured), fatals = sum(NFatal), accidentsNum= n())
View(df_stackedbar)

# Reshaping the dataframe
df_stackedbar <- melt(df_stackedbar, id.vars= "weather conditions")
names(df_stackedbar) <- c('weather conditions', 'cases', 'occurrences')
View(df_stackedbar)

# Grouping 
df_stackedbar = df_stackedbar %>% group_by(`weather conditions`, cases) %>%
  summarise(cases = cases,
            occurrences = occurrences,
            .groups = 'drop')
View(df_stackedbar)

# Visualizing the results
ggplot(df_stackedbar, aes(x = `weather conditions`, y = occurrences, fill = cases, palette="Royal2")) + 
  geom_bar(stat = "identity", colour="white",position = position_stack()) +
  geom_col(colour = "white", linewidth = 1.5) + 
  theme(
    plot.background = element_rect(fill = "black"), 
    panel.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    axis.text.x = element_markdown(color = "Cyan"),
    axis.text.y = element_markdown(color = "Cyan"),
    legend.text = element_markdown(color = "Cyan")
  )

