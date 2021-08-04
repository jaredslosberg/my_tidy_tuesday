if(!require(tidyTuesdayR)){
  remotes::install_github("thebioengineer/tidytuesdayR")
}

library(tidyTuesdayR)
library(tidyverse)
library(purrr)

tt_data <- tt_load("2021-08-03")

str(tt_data)

athletes <- tt_data[[1]] %>% 
  as.data.frame()

head(athletes)

athletes <- athletes %>% mutate(abb = recode(abb,"-" = "N/A"))

year_range <- paste(min(athletes$year), max(athletes$year), sep = "-")

#aggregate number of gold/silver/bronze medals for each country.
#country also holds country info but is messy.
medals_type <- c("Gold", "Silver","Bronze")
medal_counts_df <- map_df(medals_type, function(medal_type){
  medal_count <- athletes %>%
    group_by(abb) %>%
    summarize(count = sum(medal == medal_type),
              type = medal_type)

})

#number of top medalling countries to include in plot
n_countries <- 20
top_scoring_countries <- medal_counts_df %>%
  group_by(abb) %>%
  summarize(total_medals = sum(count)) %>%
  arrange(desc(total_medals)) %>%
  slice_head(n = n_countries) %>%
  pull(abb)

#filter to top countries
medal_counts_filt <- medal_counts_df %>% filter(abb %in% top_scoring_countries) %>% mutate(abb = factor(abb, levels = top_scoring_countries))

ggplot(medal_counts_filt) +
  geom_col(aes(x = abb, y = count, fill = type), color = "black", position = "dodge", width = .75) +
  scale_fill_manual(values = c("#FFCD00","#E0E0E0","#B36427")) +
  theme(
    # get rid of panel grids
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    # Change plot and panel background
    plot.background=element_rect(fill = "#272523"),
    panel.background = element_rect(fill = '#272523'),
    legend.background = element_rect(fill = '#272523'),
    legend.title = element_blank(),
    legend.text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white", size = 12),
    plot.title = element_text(color = "white", hjust = 0.5),
    legend.position = c(0.8,0.75),
    legend.direction = "horizontal") +
  xlab("Country") + 
  ylab("# of Medals") + 
  ggtitle(paste0("Top medalling countries in the Paralympics (", year_range, ")"))

,
# Change legend 
legend.position = c(0.6, 0.07),
legend.direction = "horizontal",
legend.background = element_rect(fill = "black", color = NA),
legend.key = element_rect(color = "gray", fill = "black")
) +
                                