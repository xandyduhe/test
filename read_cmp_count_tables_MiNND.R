# Xandy 11/2/2023

#https://github.com/xandyduhe/QC_Training-/blob/main/cpm_count_tables_MiNND_batch1_30Oct2023.csv

library(readr)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(reshape)
#library(heatmap)

# read in cvs table containing data (from Xandy's github)
table <- read.csv('https://raw.githubusercontent.com/xandyduhe/QC_Training-/main/cpm_count_tables_MiNND_batch1_30Oct2023.csv')

# create duplicate table to test manipulations
table_test <- table

# remove extraneous columns
table_test <- subset(table_test, select = -Gene_symbol)
table_test <- subset(table_test, select = -knockdown_samples_corresponding_to_genes)

# makes sure values are numeric (will remove row names)
table_test <- sapply(table_test, as.numeric)

# add row names again
rownames(table_test) <- table$knockdown_samples_corresponding_to_genes

heatmap(table_test)

# compare with gene symbol
rownames(table_test) <- table$Gene_symbol

# reordering for clustering suppressed
heatmap(table_test,
        Colv = NA, Rowv = NA,
        main = 'Heatmap without dendogram')

# scaled by column
heatmap(table_test,
        Colv = NA, Rowv = NA)

heatmap(table_test,
        main = 'Heatmap with dendogram')




data <- table

data <- cor(table[sapply(table, is.numeric)])
data1 <- melt(data)
library(reshape)
data1 <- melt(data)
data1
ggplot(data1, aes(x = X1, y = X2, fill = value)) +
  geom_title() +
  labs( title = "Correlation Heatmap",
        x = table$Gene_symbol,
        y = table$knockdown_samples_corresponding_to_genes)
ggplot(data1, aes(x = X1, y = X2, fill = value)) +
  geom_tile() +
  labs( title = "Correlation Heatmap",
        x = table$Gene_symbol,
        y = table$knockdown_samples_corresponding_to_genes)
ggplot(data1, aes(x = X1, y = X2, fill = value)) +
  geom_tile() +
  labs( title = "Correlation Heatmap")
View(data1)



# 11/3/2023
# various graphs

data <- subset(table, select = -knockdown_samples_corresponding_to_genes)
rownames(data) <- data$Gene_symbol
data <- subset(data, select = -Gene_symbol)
data <- t(data)

data_store <- data

data1 <- cor(data)
data1 <- melt(data1)

head(data1)
ggplot(data1, aes(x = X1, y = X2, fill = value)) +
  geom_tile() +
  labs( title = "Correlation Heatmap",
        x = 'Gene_symbol',
        y = 'Gene_symbol')

heatmap(data1)


z_test_table <- subset(table, select = -V3865)
z_test_table <- subset(z_test_table, select = -knockdown_samples_corresponding_to_genes)
z_test_table <- subset(z_test_table, select = -Gene_symbol)
rownames(z_test_table) <- table$Gene_symbol
colnames(z_test_table) <- table$knockdown_samples_corresponding_to_genes

# create empty df to storez scores
z_df_test <- data.frame(matrix(ncol = ncol(z_test_table),
                               nrow = nrow(z_test_table)))
colnames(z_df_test) <- colnames(z_test_table)

# calculate z score of each cell
for (i in 1:nrow(z_test_table)){
  sd <- sd(z_test_table[i,])
  mean <- unname(rowMeans(z_test_table[i,], na.rm = T))
  print(paste0('Row: ', i, ' the sd is ', sd, ' the mean is ', mean))

  for (j in 1:ncol(z_test_table)){
    z_score <- (z_test_table[i, j]-mean)/sd
    print(paste0(i, ' ',  j, ' ', z_score))
    z_df_test[i, j] <- z_score
  }
}
z_df_test <- sapply(z_df_test, as.numeric)
rownames(z_df_test) <- rownames(z_test_table)


heatmap(z_df_test, Colv = NA, Rowv = NA)

z_df_melt <- melt(z_df_test)

ggplot(z_df_melt, aes(X2, X1)) +
  geom_tile(aes(fill = value))+
  labs(title = 'z-score heatmap',
       x = 'Sample',
       y = 'Gene')+
  scale_fill_gradient(low = '#F3FF00',
                      high = '#0024D9')

ggplot(z_df_melt, aes(X2, X1)) +
  geom_tile(aes(fill = value))+
  labs(title = 'z-score heatmap',
       x = 'Sample',
       y = 'Gene')+
  scale_fill_viridis_c()



ggplot(z_df_melt, aes(X2, X1)) +
  geom_tile(aes(fill = value))+
  labs(title = 'z-score heatmap',
       x = 'Sample',
       y = 'Gene')+
  scale_fill_gradient(low = '#FFFFFF',
                      high = '#410090')

save(z_df_test, file = 'z_score_cpm_MiNND_batch1_3Nov2023.csv')





