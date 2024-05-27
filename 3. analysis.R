# FACTOR ANALYSIS ---------------

library(psych)
library(REdaS)

df <- df_student # change to df_teacher if needed
df_full<-df[complete.cases(df[,11:88]),] # this is for student
df_naud<-df_full[,v_columns]
bart_spher(df_naud)
KMO(df_naud)

ev <- eigen(cor(df_naud)) # get eigenvalues
scree(df_naud, pc=FALSE)
fa.parallel(df_naud, fa="fa")

faa<-fa(df_naud, nfactors=3, rotate='oblimin')
fa.diagram(fa(df_naud, nfactors=3, rotate='oblimin'))


# CLUSTER ANALYSIS --------------
df_full<-df[complete.cases(df[,11:88]),]
pca_result <- prcomp(df_full[, 11:88])


# Vars
variances <- pca_result$sdev^2
proportion_variances <- variances / sum(variances) * 100
variance_df <- data.frame(PC = 1:20, Variance = proportion_variances[1:20])

variance_df$PC <- factor(variance_df$PC)
ggplot(variance_df, aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f", Variance)), vjust = -0.5) +
  scale_x_discrete(breaks = variance_df$PC) + # Ensure all x-axis ticks are shown
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", colour = "white"))+ 
  xlab("Principinė komponentė") +
  ylab("% dispersijos paaiškinta")

# 90% variance
which(cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2) >= 0.90)[1]

pca_df <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2])
ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point() +
  theme_minimal() +
  labs(color = "Skaičius") +
  xlab("Pirmoji pagrindinė komponentė") +
  ylab("Antroji pagrindinė komponentė")