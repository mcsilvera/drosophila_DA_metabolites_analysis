# Install necessary packages
install.packages("readxl")
install.packages("openxlsx")
install.packages ("carData")
install.packages ("ggsignif")

# Load necessary packages
library(readxl)
library(openxlsx)
library(car)
library(tidyr)
library(ggplot2)
library(ggsignif)

# Set working directory and import data from Excel file
HPLC <- read_excel("hplc.xlsx", sheet ="HVA")   

# Generate vectors with data 
WT <- HPLC$WT
w1118 <- HPLC$w1118
prtpΔ1 <- HPLC$prtpΔ1
parkina <- HPLC$parkina
prtpΔ1parkina <- HPLC$prtpΔ1parkina


#Normality 
shapiro.test(WT)
shapiro.test(w1118)
shapiro.test(prtpΔ1)
shapiro.test(parkina)
shapiro.test(prtpΔ1parkina)

# Create a data frames with data
df <- data.frame(
  group = c(rep("WT", length(WT)),
            rep("w1118", length(w1118)),
            rep("prtpΔ1", length(prtpΔ1)),
            rep("parkina", length(parkina)),
            rep("prtpΔ1parkina", length(prtpΔ1parkina))),
  value = c(WT, w1118, prtpΔ1, parkina, prtpΔ1parkina))

# Create a data frames with data
df1 <- data.frame(
  group = c(rep("w1118", length(w1118)),
            rep("prtpΔ1", length(prtpΔ1)),
            rep("parkina", length(parkina)),
            rep("prtpΔ1parkina", length(prtpΔ1parkina))),
  value = c(w1118, prtpΔ1, parkina, prtpΔ1parkina))

# Perform the Levene test
leveneTest(value ~ group, data = df)

# Define new order of groups
new_order <- c("WT", "w1118", "prtpΔ1", "parkina", "prtpΔ1parkina")
new_order1 <- c("w1118", "prtpΔ1", "parkina", "prtpΔ1parkina")

# Change order of levels of "group" factor
df$group <- factor(df$group, levels = new_order)
df1$group <- factor(df1$group, levels = new_order1)

# New names for legend
nuevos_nombres <- c("WT", "w", "prtp", "park","prtp-park")
nuevos_nombres1 <- c("w", "prtp", "park","prtp-park")

# Plot WT vs all genotypes
ggplot(df, aes(x = group, y = value, fill = group)) + 
  geom_boxplot(size = 0.5, width = 0.8) +
  geom_jitter(width = 0.2, height = 0, size = 3, alpha = 1, color = "black", shape = 20) +
  scale_fill_manual(values = c("#2D4AEC", "#5C5C5C", "#D5B73C", "#D85353", "#97B15A")) + 
  scale_x_discrete(labels = nuevos_nombres) +
  ggtitle("Cuantificación HVA") +
  labs (x = "", y = "HVA (ng/mL)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, face = "bold.italic"), 
        axis.text.y = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(vjust = 4, size = 14, face = "bold"), 
        plot.margin = unit(c(1, 1, 0, 1), "lines"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  scale_y_continuous(limits = c(0, 1.6), breaks = seq(0, 1.2, by = 0.2)) + 
  geom_signif(comparisons = list(c("WT", "prtpΔ1parkina")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(1.5, 1.55)) +
  geom_signif(comparisons = list(c("WT", "parkina")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(1.4, 1.45)) +
  geom_signif(comparisons = list(c("WT", "prtpΔ1")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(1.3, 1.35)) +
  geom_signif(comparisons = list(c("WT", "w1118")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(1.2, 1.25))

# Plot prtp;parkina vs parkina and other significant comparisons without WT
# All comparisons are NS
ggplot(df1, aes(x = group, y = value, fill = group)) + 
  geom_boxplot(size = 0.5, width = 0.8) +
  geom_jitter(width = 0.2, height = 0, size = 3, alpha = 1, color = "black", shape = 20) +
  scale_fill_manual(values = c("#5C5C5C", "#D5B73C", "#D85353", "#97B15A")) + 
  scale_x_discrete(labels = nuevos_nombres1) +
  ggtitle("Cuantificación HVA") +
  labs (x = "", y = "HVA (ng/mL)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, face = "bold.italic"), 
        axis.text.y = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(vjust = 4, size = 14, face = "bold"), 
        plot.margin = unit(c(1, 1, 0, 1), "lines"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  scale_y_continuous(limits = c(0, 1.6), breaks = seq(0, 1.2, by = 0.2)) + 
  geom_signif(comparisons = list(c("WT", "prtpΔ1parkina")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(1.5, 1.55)) +
  geom_signif(comparisons = list(c("WT", "parkina")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(1.4, 1.45)) +
  geom_signif(comparisons = list(c("WT", "prtpΔ1")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(1.3, 1.35)) +
  geom_signif(comparisons = list(c("WT", "w1118")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(1.2, 1.25))
