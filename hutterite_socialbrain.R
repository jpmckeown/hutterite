if(!require('readxl')){
  install.packages('readxl')
  library(readxl)
}
if(!require('varhandle')){
  install.packages('varhandle')
  library(varhandle)
}
library('tidyverse')

# Notes on data
# must import members as text due to problems in data
olsen <- read_csv('hutterite_colonies.csv', col_names = c('size', 'leut', 'stage', 'sister', 'note'), col_types = 'icccc', skip=1)

## draw plots (not distinguishing Leut)

# Figure A (not in chapter)
# density plot, size before fission
olsen %>%
  select(size, stage) %>%
  filter(!is.na(size)) %>%
  filter(stage=='mother') %>%
  ggplot(aes(x=size)) +
    geom_density() +
    ggtitle('Hutterite colony size before fission event') +
    labs(x = 'Number (all ages) in colony') +
    theme_bw() +
    theme(axis.text = element_text(size=13), axis.title = element_text(size=13), plot.title = element_text(size=15))

# Figure 1 of chapter
olsen %>%
  select(size, stage) %>%
  filter(!is.na(size)) %>%
  filter(stage=='mother') %>%
  ggplot(aes(x=size)) +
    geom_histogram(binwidth = 5) +
    ggtitle('Hutterite colony size before fission event') +
    labs(x = 'Number in colony, all ages') +
  theme_bw() +
  theme(axis.text = element_text(size=13), axis.title = element_text(size=13), plot.title = element_text(size=15))

# Figure B (not in chapter)
# Lehrerleut big and little daughters, density plot
olsen %>%
  select(size, leut, stage, sister) %>%
  filter(!is.na(size)) %>%
  filter(stage %in% c('stay','leave')) %>%
  filter(leut == 'L') %>%
  ggplot(aes(x = size, colour = sister)) +
  geom_density() +
  ggtitle('New colony initial size') +
  labs(x = 'Number (all ages) in new colony') +
  theme_bw() +
  theme(axis.text = element_text(size=13), axis.title = element_text(size=13), plot.title = element_text(size=15))

# Figure C (not in chapter)
# Little daughters only, by Leut, density plot 
olsen %>%
  select(size, leut, stage, sister) %>%
  filter(!is.na(size)) %>%
  filter(stage %in% c('stay','leave')) %>%
  filter(sister == 'little') %>%
  ggplot(aes(x = size, colour = leut)) +
  geom_density() +
  ggtitle('Hutterite colony initial size') +
  labs(x = 'Number (all ages) in new colony') +
  theme_bw() +
  theme(axis.text = element_text(size=13), axis.title = element_text(size=13), plot.title = element_text(size=15))

# Figure D (not in chapter)
# Little daughters only
fill <- "#444444"
line <- "#000000"
olsen %>%
  select(size, leut, stage, sister) %>%
  filter(!is.na(size)) %>%
  filter(stage %in% c('stay','leave')) %>%
  filter(sister == 'little') %>%
  ggplot(aes(x = size)) +
  geom_area(aes(y = ..density..), stat = 'bin', fill='grey') +
  ggtitle('Daughter colony initial size') +
  scale_x_continuous(breaks = seq(30, 110, 10),
                     limits=c(30, 110)) +
  labs(x = 'Number (all ages) in new colony') +
  theme_bw() +
  theme(axis.text = element_text(size=13), axis.title = element_text(size=13), plot.title = element_text(size=15))

# Figure 2 in chapter
# histogram version 
olsen %>%
  select(size, leut, stage, sister) %>%
  filter(!is.na(size)) %>%
  filter(stage %in% c('stay','leave')) %>%
  filter(sister == 'little') %>%
  ggplot(aes(x = size)) +
  geom_histogram(binwidth = 5) +
  ggtitle('Smaller daughter colony initial size') +
  scale_x_continuous(breaks = seq(30, 110, 10), limits=c(30, 110)) +
  labs(x = 'Number (all ages) in new colony') +
  theme_bw() +
  theme(axis.text = element_text(size=13), axis.title = element_text(size=13), plot.title = element_text(size=15))
