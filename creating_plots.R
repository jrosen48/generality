# for parsing Stina' codes

library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)

setwd("~/Dropbox/research/cluster-compare-text")

# list.files("~/Dropbox/research/cluster-compare-text")

clust_codes <- read.csv("csv/cluster_codes.csv", skip = 1)

str(clust_codes)

clust_codes <- select(clust_codes, id, cluster)
split_id <- str_split(clust_codes$id, "-")
split_id
clust_codes$id <- sapply(split_id, function(x) paste0(x[1], "_", x[2]))
unique_ids <- unique(clust_codes$id)
tmp_bool <- unique_ids %in% clust_codes$id
new_clust_codes <- tbl_df(clust_codes)
new_clust_codes <- distinct(new_clust_codes)

# write.csv(new_clust_codes, "new_clust_codes.csv")

new_clust_codes_1 <- read.csv("csv/new_clust_codes_1.csv")

tmp_df <- select(new_clust_codes_1, Student_ID, Lesson, code = new_code)
tmp_df$other_id <- paste0(tmp_df$Student_ID, "_", tmp_df$Lesson)
tmp_df <- arrange(tmp_df, other_id)
new_clust_codes <- arrange(new_clust_codes, id)

manual_codes <- tbl_df(select(tmp_df, other_id, code))
manual_codes <- manual_codes[-10, ]
cluster_codes <- select(new_clust_codes, id, cluster)
cluster_codes <- cluster_codes[-174, ]

all_codes <- bind_cols(manual_codes, cluster_codes)
all_codes <- filter(all_codes)

View(all_codes)

all_codes_ss <- select(all_codes, ID = other_id, code, cluster)

write.csv(all_codes_ss, "csv/all_codes.csv")

all_codes_tbl <- table(all_codes$code, all_codes$cluster)

all_codes_vec <- as.vector(all_codes_tbl)
all_codes_index <- t(matrix(all_codes_vec, nrow = 7, ncol = 9))

# row.names(all_codes_index) <- paste0("Cluster ", 0:8)

row.names(all_codes_index) <- c("Helping the Reader Understand Why General or Specific Would Be Better", "No Rationale", "Clarity and Utility of the Representation it is Whether General or Specific",
                                "Showing the Mechanism", "Weighing Two Options Against One Another", "Similarities and Comparisons Across Processes and Classes",
                                "Communicating the `Main Point`", "What We Are Doing", "Unclear")

colnames(all_codes_index) <-  c("code_0", "code_1a", "code_1b", "code_3", "code_4a", "code_4b", "code_5")

colSums(all_codes_index)

all_codes_index <- select(as.data.frame(all_codes_index), code_0, code_1a, code_1b, code_4a, code_5)

all_codes_index <- select(as.data.frame(t(all_codes_index)), -`No Rationale`, -`What We Are Doing`, -`Unclear`)


# for plot

# chisq_p <- chisq.test(all_codes_index)
# asterisk <- as.vector(chisq_p$stdres > 1.96 | chisq_p$stdres < -1.96)
# asterisk[asterisk == TRUE] <- "*"
# asterisk[asterisk == FALSE] <- ""

doc_plot <- gather(as.data.frame(all_codes_index), `Cluster Topic`, N)

doc_plot

#   doc_plot[, 4] <- rep(paste0(1:n_clusters), ncol(all_codes_index))
# doc_plot$Cluster <- rep(paste0(c("Helping the Reader Understand General or Specific", "Clarity and Utility of the Representation it is Whether General or Specific",
#                               "Showing the Mechanism", "Weighing Two Options Against Each Other", "Similarities and Comparisons Across Content Matter",
#                               "Communicating the \"Main Point\"")), ncol(all_codes_index))

doc_plot$Group <- rep(paste0(c("0", "1", "2", "4a", "5")), ncol(all_codes_index))

dodge = position_dodge(.9)

doc_plot

require(RColorBrewer)

plot <- ggplot(doc_plot, aes(x = Group, y = N, fill = `Cluster Topic`, ymax = max(N))) + 
  geom_bar(width = .825, position = dodge, stat = "identity") + 
#   geom_text(aes(label = ChiSq), position = dodge, vjust = .25) +
  xlab("Manual Code") + 
  ylab("Number of Responses") +
  ggtitle("Distribution of Manual Codes by Cluster Topic") +
  theme_tufte() +
  theme(text=element_text(family="Garamond", size = 14)) +
  scale_fill_brewer(type = "qual", palette = 1) +
  theme(legend.position = "right")

plot

ggsave(filename = 'stina_plot.png')

