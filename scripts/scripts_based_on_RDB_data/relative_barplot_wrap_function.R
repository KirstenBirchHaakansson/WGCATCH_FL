

#relative_barplot - a function for making the RCG barplots
#Kirsten Birch håkansson, DTU Aqua, Denmark 

#You need to filter you dataset before running this plot 

#df: a dataframe with you data
#x: whatever should be on the x-axis (character)
#y: whatever should be on the y-axis (numeric)
#theshold: The accumulated pct of y for the x you want to display. If you only want e.g. harbour accounting for 95% of the landings 
#then set this one to 95. There are often a lot of unimpotaint x's in the big picture, so to avoid that the x axis is spammed use this one 
# - or filter data beforehand
#title: This one could be empty, but I like to know what I'm looking at

relative_barplot_wrap <- function(df, wrap = year, x = year, y = landWt, z = vslFlgCtry, title = title) {
  
  require(dplyr)
  require(ggplot2)
  
  x <- enquo(x)
  y <- enquo(y)
  z <- enquo(z)
  wrap <- enquo(wrap)
  
  name_y <- quo_name(y)
  name_z <- quo_name(z)
  
  df <- mutate(df, x = !!x, y = !!y, z = !!z, wrap = !!wrap)
  
  df_sum <- summarise(group_by(df, x, z, wrap), y_sum = sum(y, na.rm = T)) 
  
  df_n <- summarise(group_by(df, z), y_order = sum(y, na.rm = T), n = 1) 
  
  df_cumn <- mutate(arrange(df_n,-y_order), cumn = cumsum(n))
  
  #For the sake of an overview -  only selecting x's, which account for the treshhold selected
  df_cumn_1 <- left_join(df_sum, df_cumn)
  
  df_cumsum_1 <- mutate(data.frame(df_cumn_1), z = ifelse(cumn < 12, z, "other"))
  
  df_cumsum_1$z <- as.factor(df_cumsum_1$z)
  
  df_total <- summarise(group_by(df_cumsum_1, x, wrap), y_total = sum(y_sum, na.rm = T))
  df_per_x_z <- summarise(group_by(df_cumsum_1, x, z, wrap), y_per = sum(y_sum, na.rm = T))
  df_rela <- mutate(left_join(df_per_x_z, df_total), pct = (y_per/y_total)*100)
  df_rela$x <- as.character(df_rela$x)

  ggplot(df_rela, aes(x = x, y = pct, fill = z)) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values = brewer.pal(length(unique(df_rela$z)), "Paired"), name = paste(name_z)) +
    facet_wrap(~wrap) +
    labs(title = title, x = "", y = paste("Pct. of ", name_y, sep = ""),  
    caption = paste("The 11 ", name_z, "'s with most ", name_y, " over the period shown - \nrest grouped in 'other'", sep = ""))
  
}

