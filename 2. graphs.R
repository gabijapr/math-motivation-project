### NEEDED VARIABLES

## Given in data folder:
# df_teacher
# df_student
# items

## Calculated in variable_calc.R:
# 1. avgs_student
# 2. avgs_teacher
# 3. avgs_student_merged
# 4. avg_merged_teach_stud_melted
# 5. avgs_student_gpa_category

# 6. df_long_student
# 7. df_long_teacher

# 8. correlation_df_merged

# 9. merged_data_avg # TO-DO: FIX


Sys.setlocale("LC_CTYPE", "lithuanian")

#### AVERAGE BAR CHARTS ---------------------------------------------------

##### 0. Unfiltered means -----

### Vertical PLOT
# Merge student and teacher averages with items
avgs_merged_plot <- avgs_student_merged %>% 
  pivot_longer(cols = c(Naudojimas, Veiksmingumas), names_to = "Type_plot", 
               values_to = "Average")

# change sign at -avgs_merged_plot$Average to sort by other factor
avgs_merged_plot <- avgs_merged_plot[order(avgs_merged_plot$Type_plot, -avgs_merged_plot$Average, decreasing = T), ]
avgs_merged_plot$Abbr <- factor(avgs_merged_plot$Abbr, levels = unique(avgs_merged_plot$Abbr))

# Not color coded
ggplot(avgs_merged_plot, aes(x = Average, y = Abbr, fill = Type_plot)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Vidurkiai (pagal Veiksmingumą)", x = "", y = "Teiginio nr.") + 
  scale_fill_manual(values = c("Naudojimas" = "#0ca57f", "Veiksmingumas" = "#087257")) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim = c(-2, 2))  

# Color coded

ggplot(avgs_merged_plot, aes(x = Average, y = Abbr, fill = Type_plot)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Vidurkiai (pagal Veiksmingumą)", x = "",y = "Teiginio nr.") + 
  scale_fill_manual(values = c("Naudojimas" = "#0ca57f", "Veiksmingumas" = "#087257")) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(color = ifelse(merged_data_avg$Group == "Skatinimo priemonės", "red",
                                                  ifelse(merged_data_avg$Group == "Grupinis darbas", "blue",
                                                         ifelse(merged_data_avg$Group == "Autonomijos sąlygų sudarymas", "green",
                                                                ifelse(merged_data_avg$Group == "Individualizavimas (pamokoj)", "orange",
                                                                       ifelse(merged_data_avg$Group == "Asmeninis bendravimas", "purple",
                                                                              ifelse(merged_data_avg$Group == "Kita", "yellow",
                                                                                     ifelse(merged_data_avg$Group == "Bendravimas matematikos tema", "pink", "brown"))))))))) +
  coord_cartesian(xlim = c(-2, 2)) 





### Vertical stacked PLOT

ggplot(avgs_merged_plot, aes(x = Abbr, fill = Type_plot)) +
  geom_bar(data = subset(avgs_merged_plot, Type_plot == "Veiksmingumas"), aes(y = Average-1), stat = "identity", alpha = 1, width = 0.8) +
  geom_bar(data = subset(avgs_merged_plot, Type_plot == "Naudojimas"), aes(y = Average-1), stat = "identity", alpha = 0.7, width = 0.8) +
  labs(title = "Vidurkiai (pagal Veiksmingumą)",x = "",y = "") + 
  scale_fill_manual(values = c("Naudojimas" = "#0ca57f", "Veiksmingumas" = "#087257")) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = function(y) y + 1) + coord_flip()


##### 1. Teacher (Location Analysis) -----
# !! Change the variables as needed
columns_to_plot <- c("V6", "V20", "V37")

# Calculate mean values for specified columns, grouped by Location
averages <- df_teacher %>%
  filter(!is.na(Location) & Location != "Kaim") %>%
  group_by(Location) %>%
  summarise(across(all_of(columns_to_plot), ~ mean(., na.rm = TRUE)))

averages_long <- tidyr::gather(averages, key = "Variable", value = "Average", -Location)
averages_long <- merge(averages_long, items, by.x = "Variable", by.y = "Vcol", all.x = TRUE)

# Plotting
ggplot(averages_long, aes(x = Abbr2, y = Average, fill = Location)) +
  geom_bar(stat = "identity", width = 0.8, position = "dodge") +
  scale_fill_manual(values = c("#0094ff", "#52d0fe"), name = "Mokyklos vietovės tipas", labels = c("Didmiestis", "Miestas/rajono centras")) +
  labs(x = "", y = "Veiksmingumas") +
  theme_minimal() +
  geom_text(aes(label = round(Average, 2)), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  coord_cartesian(ylim = c(-1, 2))


##### 2. Student (Class Analysis) -----
# !! Change the variables as needed
columns_to_plot <- c("V6", "V20", "V37")
columns_to_plot <- c("V33", "V36", "V38")

# Calculate mean values for specified columns, grouped by Class
averages <- df_student %>%
  filter(!is.na(Class)) %>%
  group_by(Class) %>%
  summarise(across(all_of(columns_to_plot), ~ mean(., na.rm = TRUE)))

averages_long <- tidyr::gather(averages, key = "Variable", value = "Average", -Class)
averages_long <- merge(averages_long, items, by.x = "Variable", by.y = "Vcol", all.x = TRUE)

# Plotting
ggplot(averages_long, aes(x = forcats::fct_rev(Abbr2), y = Average, fill = Class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#2c4d8a", "#8497b0", "#0094ff", "#52d0fe"), name = "Klasė", labels = c("9", "10", "11", "12")) +
  labs(x = "", y = "Veiksmingumas") +
  theme_minimal() +
  geom_text(aes(label = round(Average, 2)), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  coord_cartesian(ylim = c(-1, 1))


##### 3. LIKERT percentage plot -----
# Summarize and calculate percentages for Likert scale data
# Student data
summarized <- df_long_student %>%
  filter(!is.na(LikertValue) & !startsWith(LikertType, "V")) %>%
  group_by(LikertType, LikertValue) %>%
  summarise(count = n()) %>%
  mutate(percentage = prop.table(count) * 100)

summarized$LikertValue <- as.factor(summarized$LikertValue)
strongly_disagree <- summarized %>%
  filter(LikertValue == 2)

# Reorder factor levels based on percentage of "strongly disagree"
summarized$LikertType <- factor(summarized$LikertType, levels = rev(strongly_disagree$LikertType[order(-strongly_disagree$percentage)]))
summarized$LikertValue <- factor(summarized$LikertValue, levels = rev(levels(factor(summarized$LikertValue))))

# Plotting
ggplot(data = summarized, aes(x = LikertType, y = percentage, fill = factor(LikertValue))) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5), color = "black", size = 2, fontface = "bold") +
  coord_flip() +
  scale_fill_brewer(palette = "PRGn") +
  theme_minimal() +
  theme(legend.position = "bottom")

##### 4. LIKERT individual plot -----

df_long_student_ordered <- df_long_student
df_long_student_ordered$Number <- factor(df_long_student$Number, levels = unique(df_long_student$Number)[order(as.numeric(gsub("V", "", unique(df_long_student$Number))))])

ggplot(df_long_student_ordered, aes(x = LikertValue, fill = LikertType)) +
  geom_bar(stat = "count", position = "dodge") +
  facet_wrap(~Number, ncol = 4, nrow = 10, scales = "free") +
  labs(x = "Value", y = "Count") +
  scale_x_continuous() +
  theme_minimal() +
  theme(legend.position = "none")


# #### HEATMAPS ------------------------------------------------------------------
##### 1. Student + Teacher, all items -----

ggplot(avg_merged_teach_stud_melted, aes(x = variable, y = Abbr, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#0070c0", high = "#FFD700", name = "Įvertinimas") +
  labs(x = "", y = "", fill = "Average Rating") +
  geom_text(aes(label = round(value, 2)), color = "black") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 17))

##### 2. Student GPA category, significant items -----

# Merge and filter Likert data
likert_long <- merge(avgs_student_gpa_category, items, by.x = "LikertType", by.y = "Vcol", all.x = TRUE)
likert_long <- likert_long[, c("LikertType", "Abbr2", "Patenkinamas", "Pagrindinis", "Aukstesnysis")]

likert_melted <- melt(likert_long, id.vars = c("LikertType", "Abbr2"))
likert_filtered <- likert_melted %>%
  filter(LikertType %in% c("V3", "V6", "V7", "V9", "V16", "V17", "V21", "V26", "V29", "V31", "V32", "V33", "V34", "V37"))

# Plot Likert data
ggplot(likert_filtered, aes(x = variable, y = Abbr2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#0070c0", high = "#FFD700", name = "Veiksmingumo \n Įvertinimas") +
  labs(x = "", y = "", fill = "Average Rating") +
  geom_text(aes(label = round(value, 2)), color = "black") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 17))


# #### CORRELATION BAR CHART -------------------------------------------------------

# !! filter columns for plot
numbers <- c(26, 32, 31, 37, 13, 3, 16, 21, 35, 12, 6, 19, 29, 34, 7, 20, 15, 17, 4)
correlation_df_merged <- correlation_df_merged %>% filter(Number %in% numbers)

# Plot correlation data
ggplot(correlation_df_merged, aes(y = reorder(Abbr2, correlation_n), x = correlation_n)) +
  geom_bar(stat = "identity", fill = "#52d0fe", width = 0.5) +
  geom_text(aes(label = round(correlation_n, 2)), hjust = -0.2, size = 3, color = "black") +
  labs(x = "", y = "Spirmeno koreliacija r") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1))

