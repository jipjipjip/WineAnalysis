library("ggplot2")
library ("dplyr")
library("gridExtra")
library(GGally)
library("patchwork")
library('corrplot')
library('tidyr')

#multivariate plots

#Alcohol only contributes to 22% of total quality even though has the highest correlation
#now look at how other variables contribute w/alcohol constant

ggplot(dataset, aes(x = alcohol, y = density, color = quality))+
  geom_point(alpha = 0.5, size = 1)+
  geom_smooth(method = 'lm', se = FALSE, size=1)+
  scale_color_brewer(type = 'seq', guide = guide_legend(title ='Quality'))+ 
  ggtitle('Correlation between Density and Quality', 'with alcohol constant')+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


#negative correlation between density and quality i.e doesn't play a dominant role in affecting the quality and due to alcohol percentage

ggplot(dataset, aes(y = sulphates, x = alcohol, color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_y_continuous(limits=c(0.3,1.5)) +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))

#higher alcohol content = higher quality w/higher levels of sulphate

ggplot(dataset,
       aes(y = volatile.acidity, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))

#less volatile acidity = higher quality and higher alcohol

ggplot(dataset,
       aes(y = pH, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))

#low pH and higher alcohol = higher quality

ggplot(dataset,
       aes(y = residual.sugar, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))
#NO CORRELATION

ggplot(dataset,
       aes(y = total.sulfur.dioxide, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))
#lower sulphur dioxid = higher quality. high outliers exist

#effects of acids on quality of wine

ggplot(dataset,
       aes(y = citric.acid, x = volatile.acidity,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))
#higher citric and low volatile  = better wines

ggplot(dataset,
       aes(y = citric.acid, x = fixed.acidity,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))

ggplot(dataset,
       aes(y = fixed.acidity, x = volatile.acidity,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))
#no correlation on both

