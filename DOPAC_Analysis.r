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
HPLC <- read_excel("hplc.xlsx", sheet ="DOPAC")   

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

# Create a data frames with data
df2 <- data.frame(
  group = c(rep("parkina", length(parkina)),
            rep("prtpΔ1parkina", length(prtpΔ1parkina))),
  value = c(parkina, prtpΔ1parkina))

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
  ggtitle("Cuantificación DOPAC") +
  labs (x = "", y = "DOPAC (ng/mL)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, face = "bold.italic"), 
        axis.text.y = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(vjust = 4, size = 14, face = "bold"), 
        plot.margin = unit(c(10, 1, 1, 1), "lines"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  scale_y_continuous(limits = c(0, 180), breaks = seq(0, 150, by = 25)) + 
  geom_signif(comparisons = list(c("WT", "prtpΔ1parkina")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(165, 165.5)) +
  geom_signif(comparisons = list(c("WT", "parkina")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(155, 155.5)) +
  geom_signif(comparisons = list(c("WT", "prtpΔ1")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(145, 145.5)) +
  geom_signif(comparisons = list(c("WT", "w1118")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(135, 135.5))

# Plot WT and all relevant comparisons
ggplot(df, aes(x = group, y = value, fill = group)) + 
  geom_boxplot(size = 0.5, width = 0.8) +
  geom_jitter(width = 0.2, height = 0, size = 3, alpha = 1, color = "black", shape = 20) +
  scale_fill_manual(values = c("#2D4AEC", "#5C5C5C", "#D5B73C", "#D85353", "#97B15A")) + 
  scale_x_discrete(labels = nuevos_nombres) +
  ggtitle("Cuantificación DOPAC") +
  labs (x = "", y = "DOPAC (ng/mL)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, face = "bold.italic"), 
        axis.text.y = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(vjust = 4, size = 14, face = "bold"), 
        plot.margin = unit(c(10, 1, 1, 1), "lines"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  scale_y_continuous(limits = c(0, 180), breaks = seq(0, 150, by = 25)) + 
  geom_signif(comparisons = list(c("WT", "prtpΔ1parkina")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(165, 165.5)) +
  geom_signif(comparisons = list(c("WT", "parkina")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(155, 155.5)) +
  geom_signif(comparisons = list(c("WT", "prtpΔ1")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(145, 145.5)) +
  geom_signif(comparisons = list(c("WT", "w1118")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(135, 135.5)) +
  geom_signif(comparisons = list(c("prtpΔ1parkina", "parkina")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(50, 50.5)) +
  geom_signif(comparisons = list(c("parkina", "prtpΔ1")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(60, 60.5)) +
  geom_signif(comparisons = list(c("w1118", "prtpΔ1")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 4,
              y_position = c(75, 75.5))


# Plot prtp;parkina vs parkina and other significant comparisons without WT
# All the remaining comparisons are NS
ggplot(df1, aes(x = group, y = value, fill = group)) + 
  geom_boxplot(size = 0.5, width = 0.8) +
  geom_jitter(width = 0.2, height = 0, size = 3, alpha = 1, color = "black", shape = 20) +
  scale_fill_manual(values = c("#5C5C5C", "#D5B73C", "#D85353", "#97B15A")) + 
  scale_x_discrete(labels = nuevos_nombres1) +
  ggtitle("Cuantificación DOPAC") +
  labs(x = "", y = "DOPAC (ng/mL)") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, face = "bold.italic"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(vjust = 4, size = 14, face = "bold"),
    plot.margin = unit(c(10, 1, 1, 1), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 25, by = 5)) +
  geom_signif(comparisons = list(c("prtpΔ1parkina", "parkina")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 6,
              y_position = c(20, 20.5)) +
  geom_signif(comparisons = list(c("parkina", "prtpΔ1")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 6,
              y_position = c(24, 24.5)) +
  geom_signif(comparisons = list(c("w1118", "prtpΔ1")),
              test = "t.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 6,
              y_position = c(22, 22.5))
# Run t.test (Relevant comparison)
t.test (prtpΔ1parkina, parkina)



