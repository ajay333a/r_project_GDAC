library("tidyverse")
library("ggpubr")
library("GGally")
library(broom)
library("AICcmodavg")
library(modelr)
library(gt)


data <- read_csv("F:/r_language/treatment_effect/species_treatment_data.csv")

# Naming, factoring of the data ----
sps_data <- data
sps_data$Treatment <- as.factor(sps_data$Treatment)
sps_data$Treatment <- 
  factor(sps_data$Treatment,levels = c("Control",
                                       "Coconut water", 
                                       "IBA 1000ppm",
                          "IBA 100ppm + Coconut water"))
glimpse(sps_data)


# Means and SD for Species and Treatments ----

sps_data_1 <- sps_data %>%  
  summarise(avg_n_roots = mean(num_roots_n),
            SD_n_roots = sd(num_roots_n),
            avg_lng_root = mean(lng_long_root_cm),
            SD_lng_root = sd(lng_long_root_cm),
            avg_dia_root = mean(dia_long_root_mm),
            SD_dia_root = sd(dia_long_root_mm),
            .by = c(Species, Treatment)) %>%  
  mutate(across(where(is.double), ~round(.x, digits = 3))) 

glimpse(sps_data_1)



ths_nest <- sps_data |> group_by(Species) |> nest()

ths_nest
View(ths_nest)
ths_model <- function(df){
  lm(num_roots_n ~ Treatment, data = df)
}

ths_nest <- ths_nest %>% 
  mutate(model = map(data, ths_model))
ths_nest
ths_nest <- ths_nest %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
ths_nest

resids <- unnest(ths_nest, resids)
glimpse(resids)

resids %>% ggplot(aes(num_roots_n, resid)) +
  geom_line(aes(group = Treatment), alpha = 1/3) +
  facet_wrap(~Species)

sps_data_2 <- split(sps_data_1, sps_data_1$Species)
class(sps_data_2)

sps_data_3 <- sps_data_1 |>
  select(-SD_n_roots, -SD_lng_root, -SD_dia_root) |>
  tidyr::pivot_longer(c(avg_n_roots, avg_lng_root, avg_dia_root),
                      names_to = "params")

sps_data_3$params <- as.factor(sps_data_3$params)
sps_data_3$params <- factor(sps_data_3$params, 
                            levels = c("avg_n_roots",
                                       "avg_lng_root",
                                       "avg_dia_root"))
str(sps_data_3)



# Graphs ----

sps_data %>% filter(Species == "Conidium verigatum") %>% 
  ggplot(aes(Treatment, num_roots_n, fill = Treatment))+
  geom_col() + stat_compare_means(method = "anova",
                                  label.x = "Control",
                                  label.y = 60)+
  labs(y = "Number of Roots") +
  theme_bw() + theme(legend.position = "bottom")

plot_3 <- sps_data %>% filter(Treatment != "Control") %>% 
  ggplot(aes(lng_long_root_cm, dia_long_root_mm,
                        color = Treatment))+
  geom_point()+ geom_smooth(method = "loess", se = FALSE)+
   theme_bw()+
  labs(x= "Length of longest root",
       y= "Diameter of Longest Root") +
  theme(legend.position = "bottom")
plot_3




sps_data |> filter(Treatment!= "Control") |> 
   ggpairs(columns = 3:5,
           columnLabels = c("Number of roots",
                            "Length of longest roots(cm)",
                            "Diameter of longest roots(mm)"),
           aes(color = Treatment, alpha = 0.5),
           upper = list(continuous = wrap("cor", size = 5)),
           lower = list(continuous = wrap("smooth", size = 3, alpha = 1)),
           diag = list(continuous = wrap("blankDiag"))
          ) + theme_bw()


sps_data_1 |> 
  tidyr::pivot_longer(c(avg_n_roots, avg_lng_root,
                        avg_dia_root)) |> 
  ggplot(aes(x = Species, y = value, fill = name)) + 
  geom_col(alpha = 0.7, position = "dodge") + 
  facet_wrap(~Treatment, ncol = 2) + 
  theme_bw() +
 theme(legend.position = "bottom",
       axis.text.x = element_text(angle = 45, hjust = 1))


sps_data_3 |>  
  ggplot(aes(x = Species, y = value, fill = Treatment)) + 
  geom_col(alpha = 0.7, position = "dodge") + 
  facet_wrap(~params, scales = "free", ncol = 1,
             labeller = as_labeller(c(
               `avg_n_roots` = "Average number of roots per leaf",
               `avg_lng_root` = "Average leangth of longest root",
               `avg_dia_root` = "Average diameter of longest root"
             ))) + 
  theme_bw() + labs(y = "Parameters") +
  theme(legend.position = "bottom")



