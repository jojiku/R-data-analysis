# Dodge bar chart

# First, let's find unique values of lightConditions column
unique(df$lightConditions)

# Then let's change values in our lightConditions column to only day & night 

df$lightConditions[df$lightConditions=="At night, lights on" | 
                     df$lightConditions== "At night, lighting is not turned on" |
                     df$lightConditions== "At night, there is no lighting"]<-"Night"

df$lightConditions[df$lightConditions=="daylight hours" | 
                     df$lightConditions== "Twilight"]<-"Day"
unique(df$lightConditions)

# Now let's combine the number of accidents, injuries and deaths (fatal) into one table

# Getting rid of other columns
df_dodge = subset(df, select = c(lightConditions, NInjured, NFatal))
View(df_dodge)

# Getting overall number of accidents (rows) & fatal cases & injuries by light conditions
df_fat_inj = df_dodge %>% 
  group_by(lightConditions) %>% summarize(injuries = sum(NInjured), fatals = sum(NFatal), accidentsNum= n())
View(df_fat_inj)

# Reshaping the dataframe
df_fat_inj <- melt(df_fat_inj, id.vars= "lightConditions")
names(df_fat_inj) <- c('lightConditions', 'cases', 'occurrences')
View(df_fat_inj)

# Grouping 
df_fat_inj = df_fat_inj %>% group_by(lightConditions, cases) %>%
  summarise(cases = cases,
            occurrences = occurrences,
            .groups = 'drop')
View(df_fat_inj)

# Visualizing the results
ggplot(df_fat_inj, aes(x = lightConditions, y = occurrences, fill = cases, palette="Royal1")) + 
  geom_bar(stat = "identity", colour="white", position = "dodge",linewidth = 1.5) +
  theme(
    plot.background = element_rect(fill = "black"), 
        panel.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
        axis.text.x = element_markdown(color = "Cyan"),
    axis.text.y = element_markdown(color = "Cyan"),
        legend.text = element_markdown(color = "Cyan")
    )
