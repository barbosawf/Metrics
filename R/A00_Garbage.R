data_list |>
  map_dfr(\(x) {
    x |>
      select_at(vars(
        Variable,
        Prop_Marker,
        Neurons,
        Maxit,
        Learn_Rate,
        R2_t,
        R2_v,
        RMSE_t,
        RMSE_v
      )) |>
      slice_max(R2_v)
  }) |>
  pivot_longer(
    cols = R2_t:RMSE_v,
    values_to = "Value",
    names_sep = "_",
    names_to = c("Metrics", "População")
  ) |>
  mutate_at(vars(`População`),
            ~ case_match(.,
                         "t" ~ "Treinamento",
                         "v" ~ "Validação")) |>
  mutate_if(is.character, as_factor) |>
  pivot_wider(names_from =  Metrics,
              values_from = Value) ->
  data_table


x |>
  pivot_longer(cols = !Variable,
               values_to = "Value",
               names_sep = "_",
               names_to = c("Metrics", "Stats")) |>
  pivot_wider(names_from =  Metrics,
              values_from = Value)


set.seed(2022)
specie <- c(rep("sorgho", 6), rep("poacee", 6),
            rep("banana", 6), rep("triticum", 6))
condition <- rep(c("normal" , "stress" , "N2") , 8)
continent <- rep(c("Europe", "Africa", "Asia", "South America",
                   "North America", "Australia"), 4)
value <- abs(rnorm(24 , 0 , 15))
data <- data.frame(specie, condition, continent, value)
head(data)

ggplot(data) +
  geom_bar(aes(x = condition, y = value, fill = continent),
           position = "stack",
           stat = "identity") +
  facet_grid(~ specie, switch = "x") +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = "white"),
        panel.spacing = unit(-.01,"cm"))



# Interaction discret x-axis ----------------------------------------------

library(ggplot2)
library(ggh4x)

data <- read.table(text = "Group Category Value
    S1 A   73
    S2 A   57
    S1 B   7
    S2 B   23
    S1 C   51
    S2 C   87",
                   header = TRUE)

# Only one more line of code with the function 'guide_axis_nested'
# and changing the data from x axis to interaction(Group,Category, sep = "!")

ggplot(data = data, aes(
  x = interaction(Group, Category, sep = "|"),
  y = Value,
  fill = Group
)) +
  geom_col(position = 'dodge', show.legend = FALSE) +
  geom_text(aes(label = paste(Value, "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  scale_x_discrete(guide = guide_axis_nested(delim = "|"), name = "Category")

