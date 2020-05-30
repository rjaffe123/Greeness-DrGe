pdf("/Users/rachaeljaffe/Greeness-DrGe/all_travel_time.pdf", width = 10, height = 8)
plot_all <- merge_all2 %>% ggplot(aes(x=travel_time)) + 
  geom_histogram(binwidth = 50) +
  ggtitle('All travel times')
plot_all
dev.off()



pdf("/Users/rachaeljaffe/Greeness-DrGe/travel_time_per_mode_year.pdf", width = 10, height = 8)
plots <- merge_all2 %>% ggplot( aes(x=travel_time, color = green_type))+
  geom_histogram(binwidth = 30) +
  facet_grid(mode ~ year)+
  ggtitle("Travel time for each mode and year, color = greenness type")
plots
dev.off()

walking_data <- merge_all2 %>% filter(mode == "walking" && travel_time < 60)
walking_plots <- walking_data %>% ggplot(aes(x=travel_time)) +
  geom_histogram(binwidth=25) +
  facet_grid(green_type ~ year) +
  ggtitle("Walking for each green type and per year") +
  scale_fill_gradient(low = "blue", high = "green") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
walking_plots

biking_data <- merge_all2 %>% filter(mode == "biking")
biking_plots <- biking_data %>% ggplot(aes(x=travel_time)) +
  geom_histogram(binwidth=25) +
  facet_grid(green_type ~ year) +
  ggtitle("Biking for each green type and per year") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
biking_plots


driving_data <- merge_all2 %>% filter(mode == "driving")
driving_plots <- driving_data %>% ggplot(aes(x=travel_time)) +
  geom_histogram(binwidth=25) +
  facet_grid(green_type ~ year) +
  ggtitle("Driving for each green type and per year") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
driving_plots









