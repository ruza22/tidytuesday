# loadi libraries and data
library(tidyverse)
library(effsize)
library(rsample)
library(gridExtra)

richmondway <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-26/richmondway.csv')


# is there an evidence that dating or coaching flags have an effect on Imdb rating or F count?
# data set is small -> using bootstrapping to get more robust estimates of LM coefficients

estims_imdb <- reg_intervals(formula = Imdb_rating ~ Dating_flag + Coaching_flag,
                             data = richmondway,
                             model_fn = "glm",
                             family = "gaussian",
                             alpha = 0.05,
                             keep_reps = TRUE) %>% unnest(.replicates)

estims_fck <- reg_intervals(formula = F_count_RK ~ Dating_flag + Coaching_flag,
                             data = richmondway,
                             model_fn = "glm",
                             family = "gaussian",
                             alpha = 0.05,
                             keep_reps = TRUE) %>% unnest(.replicates)

# means of the bootstrap estimates
imdb_mean <- estims_imdb %>%
  group_by(term) %>% 
  summarise(mean_estim = mean(estimate))
fck_mean <- estims_fck %>%
  group_by(term) %>% 
  summarise(mean_estim = mean(estimate))

# Cohen's d to quantify effect size
cd_imdb <- cohen.d(estims_imdb$estimate, estims_imdb$term) 
cd_fck <- cohen.d(estims_fck$estimate, estims_fck$term) 

#plotting histograms of estimates
imdb_hist <- ggplot(data = estims_imdb, mapping = aes(x = estimate)) +
  geom_histogram(aes(fill = term), position = "identity", alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  scale_fill_discrete(name = "Occurence in episode",
                      labels = c("Coaching", "Dating")) +
  geom_vline(data = imdb_mean,
             aes(xintercept = mean_estim),
             linetype = "longdash") +
  annotate("text", x = imdb_mean$mean_estim[1] - 0.03, y = 50, angle = 90,
           label = "Coaching estimate mean") +
  annotate("text", x = imdb_mean$mean_estim[2] - 0.03, y = 50, angle = 90,
           label = "Dating estimate mean") +
  labs(title = "Bootstrapping estimates of linear coefficients",
       subtitle = sprintf("Formula: Imbd_rating ~ Coach + Date, 1000 iterations, Cohen's d = %.2f", cd_imdb$estimate),
       caption = "Source: https://github.com/deepshamenghani/richmondway, Graphics: Lukas Ruzicka") +
  xlab("Coefficient estimate") +
  ylab("Count")

fck_hist <- ggplot(data = estims_fck, mapping = aes(x = estimate)) +
  geom_histogram(aes(fill = term), position = "identity", alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  scale_fill_discrete(name = "Occurence in episode",
                      labels = c("Coaching", "Dating")) +
  geom_vline(data = fck_mean,
             aes(xintercept = mean_estim),
             linetype = "longdash") +
  annotate("text", x = fck_mean$mean_estim[1] - 0.2, y = 50, angle = 90,
           label = "Coaching estimate mean") +
  annotate("text", x = fck_mean$mean_estim[2] - 0.2, y = 50, angle = 90,
           label = "Dating estimate mean") +
  labs(title = "Bootstrapping estimates of linear coefficients",
       subtitle = sprintf("Formula: Fck_count ~ Coach + Date, 1000 iterations, Cohen's d = %.2f", cd_fck$estimate),
       caption = "Source: https://github.com/deepshamenghani/richmondway, Graphics: Lukas Ruzicka") +
  xlab("Coefficient estimate") +
  ylab("Count")
  
#saving files into wd
ggsave("imdb_hist.png", plot = imdb_hist)
ggsave("fck_hist.png", plot = fck_hist)
