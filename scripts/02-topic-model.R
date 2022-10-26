#####################################
# Latent Dirichlet Allocation (LDA)
#####################################

# Primer on Latent Dirichlet Allocation (LDA):
# https://sicss.io/2020/materials/day3-text-analysis/topic-modeling/rmarkdown/Topic_Modeling.html

# Setup ----------------------------------------------------------------------------------------------------

library(topicmodels)

library(tidytext)
library(dplyr)
library(ggplot2)
data('AssociatedPress')


# LDA on AP data with 10 topics specified
AP_topic_model <- LDA(AssociatedPress, k=10, control = list(seed = 321)) #control for reproducability



# Here, betas correspond to probs of each word being associated with each topic
AP_topics <- tidy(AP_topic_model, matrix = "beta") #turn object into tidy tibble (extract beta attribute)

ap_top_terms <-
  AP_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


topic_labels <- c(`1` = 'Topic 1', `2` = 'Topic 2', `3` = 'Topic 3', `4` = 'Topic 4', `5` = 'Topic 5',
                  `6` = 'Topic 6', `7` = 'Topic 7', `8` = 'Topic 8', `9` = 'Topic 9', `10` = 'Topic 10')
ap_top_terms_plot <- ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol=2,
             labeller = as_labeller(topic_labels)) + # Not fixed for each separate graph
  coord_flip() +
  xlab('') +
  ylab('Probability that term belongs to topic')


ggsave('images/ap-top-terms.png', ap_top_terms_plot)
