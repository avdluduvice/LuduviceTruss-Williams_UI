# -------------------------------------------------------------------------------
# Title: How Insured Are Workers Against Unemployment? Unemployment 
# Insurance and the Distribution of Liquid Wealth 
# Purpose: Replication codes for FRBC's Economic Commentary
# File: Script to generate Figures 2 to 3 and Figure 4 in the appendix 
# Authors: Andre  Victor D. Luduvice and Anaya Truss-Williams
# Cleveland, September 2024
# -------------------------------------------------------------------------------

#### Housekeeping ---------------------------------------------------------------

# Set colors
color1 = "#2875a8"   #blue
color2 = "#7fb6ca"   #blue 2

# setting penalty for scientific notation
options(scipen=1000)

# auxiliary function for graphing 
big_mark <-  function(x) format(x, big.mark = ",", scientific = FALSE)

# formatting x-axis labels in $ notation

# defining quartiles ranges for x-axis legend
x6 <- paste0("\u2264 $", big_mark(round(tpinc_quarts[2],-2)))
x7 <- paste(paste0("$",big_mark(round(tpinc_quarts[2], -2)+1)), paste0("$",big_mark(round(tpinc_quarts[3],-2))), sep = " - ")
x8 <- paste(paste0("$",big_mark(round(tpinc_quarts[3], -2)+1)), paste0("$",big_mark(round(tpinc_quarts[4],-2))), sep = " - ")
x9 <- paste0(paste0("$",big_mark(round(tpinc_quarts[4], -2)+1)), "+")

tpinc_quartiles <- c(
  paste0(x6, " \n (0%-25%)"),
  paste0(x7, " \n (26%-50%)"),
  paste0(x8, "\n (51%-75%)"), 
  paste0(x9, "\n (76%-100%)"))

### Fig. 2 - Income Distribution of Unemployed by UI Receipt -------------------

fig_two <- ggplot(pu_Graphs, aes(x = factor(tpinc_quartile, labels=tpinc_quartiles), 
                                                         y=TPTOTINC_quarts, fill = as.factor(EUCANY.f))) +
  geom_bar(position="dodge", stat="summary", fun="median") +
  xlab("Personal Income Quartiles") +
  ylab("Median Annual Personal Income ($)") + theme_classic() + 
  theme(axis.title = element_text(size=15, color="black"),
        axis.text.y=element_text(size=15, color="black"),
        axis.text.x=element_text(size=15, color="black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", linewidth = 1),
        plot.title=element_text(size=20, color="black"),
        legend.key.size = unit(.5, 'cm'),
        legend.text = element_text(size=15),
        legend.text.align = 1,
        legend.title = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.background = element_rect(linetype = 1,colour = 1),
        legend.justification = c(0,1),
        legend.position = c(0,1)) + 
  scale_y_continuous(labels = big_mark, breaks = seq(0,140000, by=20000)) +
  scale_fill_manual(values = c(color1,"darkgreen"), name = "Received UI?", 
                    guide = guide_legend(reverse = F))

ggsave(paste0(Chart_Path, "Figure_2.png"), width = 12, height=6, unit="in")

# # writes the data represented at each quartile bin in the graph
# ggplot_build(fig_two)$data[[1]]

#### Fig. 3 - Median Net Liquid Wealth of Unemployed by UI Receipt  -------------

fig_three <- ggplot(pu_Graphs, aes(x = factor(tpinc_quartile, labels=tpinc_quartiles),
                                                       y=net_liq_wealth_quarts, fill = as.factor(EUCANY.f))) +
  geom_bar(position="dodge", stat="summary", fun="median") +
  xlab("Personal Income Quartiles") +
  ylab("Median Annual Net Liquid Wealth ($)") + theme_classic() +
  theme(axis.title = element_text(size=15, color="black"),
        axis.text.y=element_text(size=15, color="black"),
        axis.text.x=element_text(size=15, color="black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black"),
        plot.title=element_text(size=20, color="black"),
        legend.key.size = unit(.5, 'cm'),
        legend.text=element_text( size=15),
        legend.text.align = 1,
        legend.title=element_blank(),
        legend.key = element_rect(colour = "transparent", fill="transparent"),
        legend.background = element_rect(linetype = 1, colour = 1),
        legend.justification = c(0,1),
        legend.position=c(0,1)) +
  scale_y_continuous(labels = big_mark, breaks = seq(0,40000, by=5000)) +
  scale_fill_manual(values = c(color1,"darkgreen"), name = "Received UI?",
                    guide = guide_legend(reverse = F))

ggsave(paste0(Chart_Path, "Figure_3.png"), width = 12, height=6, unit="in")

# # writes the data represented at each quartile bin in the graph
# ggplot_build(fig_three)$data[[1]]

#### Fig. 4 - Median Net Liquid Wealth of Unemployed by UI Receipt  -------------

fig_four <- ggplot(pu_Graphs, aes(x = factor(tpinc_quartile, labels=tpinc_quartiles),
                                                       y=TNETWORTH_quarts, fill = as.factor(EUCANY.f))) +
  geom_bar(position="dodge", stat="summary", fun="median") +
  xlab("Personal Income Quartiles") +
  ylab("Median Annual Net Worth ($)") + theme_classic() +
  theme(axis.title = element_text(size=15, color="black"),
        axis.text.y=element_text(size=15, color="black"),
        axis.text.x=element_text(size=15, color="black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black"),
        plot.title=element_text(size=20, color="black"),
        legend.key.size = unit(.5, 'cm'),
        legend.text=element_text( size=15),
        legend.text.align = 1,
        legend.title=element_blank(),
        legend.key = element_rect(colour = "transparent", fill="transparent"),
        legend.background = element_rect(linetype = 1, colour = 1),
        legend.justification = c(0,1),
        legend.position=c(0,1)) +
  scale_y_continuous(labels = big_mark, breaks = seq(0,300000, by=50000)) +
  scale_fill_manual(values = c(color1,"darkgreen"), name = "Received UI?",
                    guide = guide_legend(reverse = F))

ggsave(paste0(Chart_Path, "Figure_4.png"), width = 12, height=6, unit="in")

# # writes the data represented at each quartile bin in the graph
# ggplot_build(fig_four)$data[[1]]
