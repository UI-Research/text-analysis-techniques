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


ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") + # Not fixed for each separate graph
  coord_flip()
