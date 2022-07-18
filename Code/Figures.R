# associated GitHub repo: 
# https://github.com/CourtneyStuart/FL_Patch_Reefs


# install packages (only need to do this once)
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("conflicted")
#install.packages("PNWColors")
#install.packages("easypackages")


# load packages
library(easypackages)
libraries("tidyverse", "ggplot2", "dplyr", "PNWColors", "conflicted")
conflicted::conflict_prefer("select", "dplyr")


# read in the csv of results (covariate name, beta coefficient, incident
# rate ratio (IRR), 95% confidence interval (CI) around IRR, Z value, p value)
# from the gray snapper (lg) and bluestriped grunt (hs) negative binomial models
lg = read.csv("https://raw.githubusercontent.com/CourtneyStuart/FL_Patch_Reefs/main/Data/Subadult_Gray_Snapper_Negative_Binomial_Results.csv")
hs = read.csv("https://raw.githubusercontent.com/CourtneyStuart/FL_Patch_Reefs/main/Data/Subadult_Bluestriped_Grunt_Negative_Binomial_Results.csv")

# add species name as a new column to both data frames then combine
lg$Species = "Lutjanus griseus"
hs$Species = "Haemulon sciurus"

nb_results = rbind(lg, hs)

# color palette
my_pal = pnw_palette("Bay",9)

nb_results = nb_results %>% 
  dplyr::filter(Covariates != "Intercept")
  

# plot coefficients
irr_plot = ggplot(data = nb_results,
                aes(x = exp.Est..,
                    y = Covariates,
                    fill = Species)) +
  geom_vline(xintercept = 1, color = "gray") +
  geom_point(aes(color = Species)) +
  scale_color_manual(values = c(my_pal[1], my_pal[5])) +
  scale_fill_manual(values = c(my_pal[1], my_pal[5])) +
  labs(x = "Incident Rate Ratio",
       y = "Covariate") +
  theme_bw() + 
  theme(legend.position = "top", legend.title = element_blank(),
        panel.border = element_rect(color = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.text = element_text(face = "italic", size = 8),
        legend.margin = margin(0,0,0,0), 
        plot.margin = margin(c(0,0,0,0)),
        axis.text = element_text(size = 8), 
        axis.title.x = element_text(size = 10),
        legend.box.margin = margin(t = 0, r = 0, b = -10, l = 0))

irr_plot

lg_plot = ggplot(data = lg,
                  aes(x = exp.Est..,
                      y = Covariates,
                      fill = Species)) +
  geom_vline(xintercept = 1, color = "gray") +
  geom_point(aes(color = Species)) +
  scale_color_manual(values = c(my_pal[5])) +
  scale_fill_manual(values = c(my_pal[5])) +
  labs(x = "Incident Rate Ratio",
       y = "Covariate") +
  theme_bw() + 
  theme(legend.position = "top", legend.title = element_blank(),
        panel.border = element_rect(color = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.text = element_text(face = "italic", size = 8),
        legend.margin = margin(0,0,0,0), 
        plot.margin = margin(c(0,0,0,0)),
        axis.text = element_text(size = 8), 
        axis.title.x = element_text(size = 10),
        legend.box.margin = margin(t = 0, r = 0, b = -10, l = 0))

lg_plot

hs_plot = ggplot(data = hs,
                 aes(x = exp.Est..,
                     y = Covariates,
                     fill = Species)) +
  geom_vline(xintercept = 1, color = "gray") +
  geom_point(aes(color = Species)) +
  scale_color_manual(values = c(my_pal[1])) +
  scale_fill_manual(values = c(my_pal[1])) +
  labs(x = "Incident Rate Ratio",
       y = "Covariate") +
  theme_bw() + 
  theme(legend.position = "top", legend.title = element_blank(),
        panel.border = element_rect(color = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.text = element_text(face = "italic", size = 8),
        legend.margin = margin(0,0,0,0), 
        plot.margin = margin(c(0,0,0,0)),
        axis.text = element_text(size = 8), 
        axis.title.x = element_text(size = 10),
        legend.box.margin = margin(t = 0, r = 0, b = -10, l = 0))

hs_plot
