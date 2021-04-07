library(ggplot2)

# Load in our data, I have it stored in a "data" folder relative to my
# project/working directory
load("data/df_long.RData")

# Create a ggplot with information about data distributions
# combination of scatterplot, density plot and boxplot
ggplot(df_long,
       aes(x = age.category, 
           y = ffa,
           fill = age.category)) + 
  geom_dotplot(binaxis = "y",     
               stackdir='center', 
               dotsize = 0.5,     
               binwidth = .2,     
               alpha = .6) +      
  geom_violin(alpha = .5) + 
  geom_boxplot(alpha = .5, width = .2) + 
  scale_fill_brewer(palette = "Dark2") +
  theme_light() +
  labs(
    title = "FFA activation across age categories",
    subtitle = "distributions of repeated participant measurements",
    x = "Age category",
    y = "FFA activation", 
    fill = "Age category") +
  facet_wrap(~face.emotion) +
  NULL


# BONUS: Combine ggseg and barplot ----
# package that easily combines plots
library(patchwork)

# If you need to install it, it needs a special installation to work.
# Uncomment the next two lines to install ggseg.
# Once installed, comment the lines again. You dont want to install every
# time you run your script!
# install.packages("remotes")
# remotes::install_github("LCBC-UiO/ggseg")
library(ggseg)

# Make up some data with dk atlas information
test_data <- dplyr::tribble(
  ~region, ~activation,
  "bankssts", .5,
  "precentral", 1,
  "precuneus", 1.5,
  "lingual", .2
)

# save ggseg plot to an object
ggseg_p <- ggplot(test_data) + 
  geom_brain(
    aes(fill = activation),
    atlas = dk,
    position = position_brain(hemi ~ side),
    show.legend = FALSE
  ) +
  theme_brain2()

# save barchart to an object
bar_p <- ggplot(test_data, 
                aes(x = region, 
                    y = activation, 
                    fill = activation)) +
  geom_bar(stat = "identity") +
  theme_light()

# Combine the plot objects rowwise (/)
# apply the same color scale to all
# Add plot annotation
ggseg_p / bar_p  & 
  scale_fill_viridis_c(na.value = "grey20") & 
  plot_annotation( 
    title = "Brain activation strength",  
    subtitle = "Desikan-Killiany atlas", 
  ) 