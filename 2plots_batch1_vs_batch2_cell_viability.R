# Xandy Duhe

library(readr)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(reshape)
library(extrafont)
#library(heatmap)


table <- read.csv('https://raw.githubusercontent.com/xandyduhe/test/main/1Plots_batch1_vs_batch2.csv')

#remove the last row of the table
table <- table[1:24,]
colnames(table) <- c('Gene', 'CW20107', 'KOLF2.2J', 'CD14')

# reformat to long format
table_long <- melt(table, id.vars = 'Gene')
colnames(table_long) <- c('Gene', 'Batch', 'value')
level_order <- table$Gene

# create barplot

ggplot(table_long, aes(fill = Batch, x = Gene, y = value))+
  geom_bar(position = 'dodge', stat = 'identity',
           aes(x = factor(Gene, level = level_order))) + # order the vars in the x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12, family = 'Sans'))+
  labs(title = 'Post Sorting Viability',
       x = 'Gene',
       y = '(%)',
       color = 'Batch') +
  scale_fill_manual(values = c('#0023FF',
                               '#FF7600',
                               '#8F8F8F'))


ggplot(table_long, aes(group = Batch, x = Gene, y = value))+
  geom_line(aes(x = factor(Gene, level = level_order),
                color = Batch), size = 1) +
  theme_minimal() +
  scale_color_manual(values = c('#0023FF', '#FF7600', '#8F8F8F')) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12, family = 'Sans'))+
  labs(title = 'Post Sorting Viability',
       x = 'Gene',
       y = '(%)',
       color = 'Batch')


ggplot(table_long, aes(fill = Batch, x = Gene, y = value))+
  geom_bar(position = 'dodge', stat = 'identity',
           aes(x = factor(Gene, level = level_order))) + # order the vars in the x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12, family = 'Sans'),
        legend.position = 'bottom')+
  labs(title = 'Post Sorting Viability',
       x = 'Gene',
       y = '(%)',
       color = 'Batch') +
  scale_fill_manual(values = c('#AEE0FF',
                               '#009EFF',
                               '#002FBD'),
                    name = '')


ggplot(table_long, aes(group = Batch, x = Gene, y = value))+
  geom_line(aes(x = factor(Gene, level = level_order),
                color = Batch), size = 1.5) +
  theme_minimal() +
  scale_color_manual(values = c('#AEE0FF',
                                '#009EFF',
                                '#002FBD')) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12, family = 'Sans'),
        legend.position = 'bottom')+
  labs(title = 'Post Sorting Viability',,
       y = '(%)',
       color = 'Batch')

ggplot(table_long, aes(group = Batch, x = Gene, y = value))+
  geom_line(aes(x = factor(Gene, level = level_order),
                color = Batch), size = 1.5) +
  theme_minimal() +
  scale_color_manual(values = c('#AEE0FF',
                                '#009EFF',
                                '#002FBD'),
                     name = '') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12, family = 'Sans'),
        legend.position = 'bottom')+
  labs(y = 'Post Sorting Viability',
       color = 'Batch')
