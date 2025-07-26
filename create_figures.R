library(fixest)
library(tidyr)
library(ggplot2)
library(extrafont)
library(scales)
loadfonts(device = "postscript")



theme_set(theme_minimal(base_size = 11, base_family = "CMU Serif"))


dir <- '../data'

figs <- '/../figures'

setwd(dir)

kg <- read.csv('/Users/Simon/Downloads/output_new.csv')

data <- readRDS('ITPDE_total.rds')

df <- data[data$year == 2015, ]

df$BRDR <- ifelse(df$exporter != df$importer, 1, 0)


model <- feglm(trade ~ log(distance) + rta + BRDR | exporter + importer, data=df, family = poisson(link = 'log'))

predictions <- predict(model, newdata = df, type = "response")

df$predictions <- predictions

df <- df[!is.na(df$prediction), ]

comp <- merge(df[, c('exporter', 'importer', 'trade', 'predictions')], kg[, !names(kg) %in% "trade"], by = c('exporter', 'importer'))

comp <- comp[complete.cases(comp), ]

# Specificity
sum((kg$trade == 0) & (kg$without.context == 0), na.rm = TRUE) / sum(kg$trade == 0, na.rm = TRUE)

sum((kg$trade == 0) & (kg$with.context == 0), na.rm = TRUE) / sum(kg$trade == 0, na.rm = TRUE)


sum((kg$trade != 0) & (kg$with.context == 0), na.rm = TRUE) / sum(kg$trade != 0, na.rm = TRUE)

sum((kg$trade != 0) & (kg$without.context == 0), na.rm = TRUE) / sum(kg$trade != 0, na.rm = TRUE)

mse_lm <- mean((comp$predictions - comp$trade)^2, na.rm=TRUE)
mse_nn <- mean((comp$predicted_trade - comp$trade)^2, na.rm=TRUE)

lng <- comp[, c('trade', 'predictions',  'without.context', 'with.context')]
names(lng) <- c('trade', 'PPML',  'KG', 'KG with context')

lng <- pivot_longer(lng, 'PPML':'KG with context')

# Filter out non-positive values (zero or negative)
lng <- lng[lng$trade > 0 & lng$value > 0, ]

# Determine data ranges for positive values
trade_range <- range(lng$trade, na.rm = TRUE)
value_range <- range(lng$value, na.rm = TRUE)

# Calculate min and max exponents, ensuring they cover both variables
min_val <- floor(min(log10(trade_range[1]), log10(value_range[1])))
max_val <- ceiling(max(log10(trade_range[2]), log10(value_range[2])))

# Set limits to encompass both ranges
axis_limits <- c(10^min_val, 10^max_val)

# Define breaks as a sequence of powers of 10, with controlled steps
axis_breaks <- 10^seq(min_val, max_val, by = 4) 

font_size <- 16

ggplot(data = lng) + 
  geom_point(aes(x = trade, y = value, color = name), alpha = 0.2) + 
  scale_y_continuous(trans = "log10", labels = function(x) number(x, accuracy = 0.1)) + 
  scale_x_continuous(trans = "log10", labels = function(x) number(x, accuracy = 0.1)) + 
  geom_abline(aes(intercept = 0, slope = 1, linetype = "Perfect Prediction"), color = "black", size = 0.6) + 
  scale_linetype_manual(values = c("Perfect Prediction" = "dashed"), name = "Reference Line") + 
  labs(x = 'Trade', y = 'Prediction', color = 'Model') + 
  theme(text = element_text(size = 12))
  
ggsave('performance_in_one.eps', path = figs, device = cairo_ps)


ggplot(data = lng) + 
  geom_point(aes(x = trade, y = value, color = name), alpha = 0.2) + 
  geom_abline(aes(intercept = 0, slope = 1, linetype = "Perfect Prediction"), color = "black", size = 0.6) + 
  scale_y_continuous(trans = "log10", 
  					 labels = function(x) parse(text = sprintf("10^%d", log10(x))),
  					 limits = axis_limits, 
       				 breaks = axis_breaks) + 
  scale_x_continuous(trans = "log10",
  					 labels = function(x) parse(text = sprintf("10^%d", log10(x))),
					 limits = axis_limits, 
                     breaks = axis_breaks
  ) + 
  scale_linetype_manual(values = c("Perfect Prediction" = "dashed"), name = "") + 
  scale_color_manual(values = c("KG" = "#edae49", 
                                "KG with context" = "#d1495b", 
                                "PPML" = "#00798c"), 
                     name = "") +
  facet_wrap(~ name, ncol = 3) + 
  labs(x = "Actual Trade Flows", y = "Prediction", color = "Model") + 
  theme(text = element_text(size = 15),
  		strip.text = element_text(size = 15),
  		legend.text = element_text(size = 15),
  		axis.text = element_text(size = 15),
  		legend.title = element_text(size = 15), 
        legend.position = "bottom",
        panel.spacing.x = unit(1, "cm"))


ggsave('performance_facet.eps', path = figs, device = cairo_ps)

