# Useful Functions --------------------------------------------------------


package_check <- readRDS("A01_Useful_Functions/package_check.rds")



# Packages ----------------------------------------------------------------


packs <- c("tidyverse",
           "readxl",
           "writexl",
           "ggbreak",
           "ggpubr",
           "ggtext",
           "ggh4x")


package_check(packs)



# Data --------------------------------------------------------------------


(data_files <- list.files("Data", pattern = "metrics")[-c(1:2)])


(data_names <-
    str_remove_all(data_files, "metrics_|_iter_500.xlsx"))


(data_path <- str_c("Data/", data_files))


(names(data_path) <- data_names)


(data_list <- data_path |>
    map(read_xlsx))



data_file_all_Sel_1_0000 <-
  str_c("Data/",
        list.files("Data", pattern = "metrics_all_iter_500_Sel_1_0000")) |>
  read_xlsx()


(res_files <- list.files("Data", pattern = "res_var")[-1])


(res_names <- str_remove_all(res_files, "res_var_|.xlsx"))


(res_path <- str_c("Data/", res_files))


names(res_path) <- res_names


res_path |>
  map(excel_sheets)


(res_list <- res_path |>
    map(\(x) read_xlsx(x, sheet = "Metrics") |>
          left_join(read_xlsx(x, sheet = "Time"))))


res_all_Sel_1_0000_Metrics <-
  str_c("Data/",
        list.files("Data", pattern = "res_var_all_Sel_1_0000")) |>
  read_xlsx(sheet = "Metrics")


res_all_Sel_1_0000_Time <-
  str_c("Data/",
        list.files("Data", pattern = "res_var_all_Sel_1_0000")) |>
  read_xlsx(sheet = "Time")


res_all_Sel_1_0000 <-
  res_all_Sel_1_0000_Metrics |>
  left_join(res_all_Sel_1_0000_Time) |>
  select_at(vars(!Pop)) |>
  rename_at(vars(Time), ~ str_c(., "_t"))


(final_files <- list.files("Data", pattern = "final"))


(final_names <- str_remove_all(final_files, "final_res_|.xlsx"))


(final_path <- str_c("Data/", final_files))


names(final_path) <- final_names


final_path |>
  map(excel_sheets)


(final_df <- final_path |>
    map_dfr(
      \(x) read_xlsx(x, sheet = "Metrics_Original") |>
        left_join(read_xlsx(x, sheet = "Time")),
      .id = "Ano"
    ))


(blue_2020_no_design <- list.files("Data", pattern = "no_design"))


(blue_2020_no_design_path <- str_c("Data/", blue_2020_no_design))


names(blue_2020_no_design_path) <- "2020_no_design"


blue_2020_no_design_path |>
  map(excel_sheets)


(
  blue_2020_no_design_df <- blue_2020_no_design_path |>
    read_xlsx(sheet = "Metrics") |>
    left_join(read_xlsx(blue_2020_no_design_path, sheet = "Time"))
)


(data_bglr <- list.files("Data", pattern = "BGLR"))


(data_bglr_names <- str_remove_all(data_bglr, ".xlsx"))


(data_bglr_path <- str_c("Data/", data_bglr))


(names(data_bglr_path) <- data_bglr_names)


(bglr_rkhs_list <- data_bglr_path |>
    map(read_xlsx))


bglr_rkhs_list$BGLR_RKHS_metrics |>
  left_join(bglr_rkhs_list$BGLR_RKHS_time |>
              select_at(vars(!Pop)) |>
              rename_at(vars(Time), ~ str_c(., "_t"))) ->
  bglr_rkhs_data



# Data tidying up ---------------------------------------------------------


res_list |>
  map_dfr(\(x) {
    x |>
      select_at(
        vars(
          Variable,
          Prop_Marker,
          Neurons,
          Maxit,
          Learn_Rate,
          Rep,
          Fold,
          Cor_t,
          Cor_v,
          R2_t,
          R2_v,
          RMSE_t,
          RMSE_v,
          Time
        )
      ) |>
      group_by(Prop_Marker, Neurons, Maxit, Learn_Rate, Rep) |>
      summarise_at(vars(Cor_t, Cor_v, R2_t, R2_v, RMSE_t, RMSE_v, Time), mean) |>
      ungroup() |>
      group_by(Prop_Marker, Neurons, Maxit, Learn_Rate) |>
      summarise_at(vars(Cor_t, Cor_v, R2_t, R2_v, RMSE_t, RMSE_v, Time),
                   list(mean = mean, sd = sd)) |>
      ungroup() |>
      slice_max(R2_v_mean)
  }, .id = "Variable") |>
  rename_at(vars(matches("Time")), ~ str_replace_all(., "_", "_t_")) |>
  pivot_longer(
    cols = Cor_t_mean:last_col(),
    values_to = "Value",
    names_sep = "_",
    names_to = c("Metrics", "Population", "Estimate")
  ) |>
  mutate_at(vars(`Population`),
            ~ case_match(., "t" ~ "Training", "v" ~ "Validation")) |>
  unite("Metrics", Metrics, Estimate) |>
  pivot_wider(names_from = "Metrics", values_from = Value) |>
  mutate_if(is.character, as_factor) ->
  res_table_str_by_mean_rep_fold


res_table_str_by_mean_rep_fold


res_table_str_by_mean_rep_fold |>
  write_xlsx(str_c("A02/XLSX/", "df_str_by_mean_rep_fold.xlsx"))


res_all_Sel_1_0000 |>
  select_at(
    vars(
      Variable,
      Prop_Marker,
      Neurons,
      Maxit,
      Learn_Rate,
      Rep,
      Fold,
      Cor_t,
      Cor_v,
      R2_t,
      R2_v,
      RMSE_t,
      RMSE_v,
      Time_t
    )
  ) |>
  group_by(Variable, Prop_Marker, Neurons, Maxit, Learn_Rate, Rep) |>
  summarise_at(vars(Cor_t, Cor_v, R2_t, R2_v, RMSE_t, RMSE_v, Time_t), mean) |>
  ungroup() |>
  group_by(Variable, Prop_Marker, Neurons, Maxit, Learn_Rate) |>
  summarise_at(vars(Cor_t, Cor_v, R2_t, R2_v, RMSE_t, RMSE_v, Time_t),
               list(mean = mean, sd = sd)) |>
  ungroup() |>
  group_by(Variable) |>
  slice_max(R2_v_mean) |>
  ungroup() |>
  pivot_longer(
    cols = Cor_t_mean:last_col(),
    values_to = "Value",
    names_sep = "_",
    names_to = c("Metrics", "Population", "Estimate")
  ) |>
  mutate_at(vars(`Population`),
            ~ case_match(., "t" ~ "Training", "v" ~ "Validation")) |>
  unite("Metrics", Metrics, Estimate) |>
  pivot_wider(names_from = "Metrics", values_from = Value) |>
  mutate_if(is.character, as_factor) ->
  res_all_Sel_1_0000_str_by_mean_rep_fold


res_all_Sel_1_0000_str_by_mean_rep_fold


res_all_Sel_1_0000_str_by_mean_rep_fold |>
  write_xlsx(str_c("A02/XLSX/", "df_all_Sel_1_0000_str_by_mean_rep_fold.xlsx"))


res_list |>
  map_dfr(\(x) {
    x |>
      select_at(
        vars(
          Variable,
          Prop_Marker,
          Neurons,
          Maxit,
          Learn_Rate,
          Rep,
          Fold,
          Cor_t,
          Cor_v,
          R2_t,
          R2_v,
          RMSE_t,
          RMSE_v,
          Time
        )
      ) |>
      group_by(Rep, Fold) |>
      slice_max(R2_v) |>
      ungroup()
  }) |>
  rename(Time_t = Time) |>
  pivot_longer(
    cols = Cor_t:last_col(),
    values_to = "Value",
    names_sep = "_",
    names_to = c("Metrics", "Population")
  ) |>
  mutate_at(vars(`Population`),
            ~ case_match(., "t" ~ "Training", "v" ~ "Validation")) |>
  mutate_if(is.character, as_factor) |>
  pivot_wider(names_from =  Metrics, values_from = Value) ->
  df_best_mlp_models


df_best_mlp_models


df_best_mlp_models |>
  write_xlsx(str_c("A02/XLSX/", "df_best_mlp_models.xlsx"))


res_all_Sel_1_0000 |>
  select_at(
    vars(
      Variable,
      Prop_Marker,
      Neurons,
      Maxit,
      Learn_Rate,
      Rep,
      Fold,
      Cor_t,
      Cor_v,
      R2_t,
      R2_v,
      RMSE_t,
      RMSE_v,
      Time_t
    )
  ) |>
  group_by(Variable, Rep, Fold) |>
  slice_max(R2_v) |>
  ungroup() |>
  pivot_longer(
    cols = Cor_t:last_col(),
    values_to = "Value",
    names_sep = "_",
    names_to = c("Metrics", "Population")
  ) |>
  mutate_at(vars(`Population`),
            ~ case_match(., "t" ~ "Training", "v" ~ "Validation")) |>
  mutate_if(is.character, as_factor) |>
  pivot_wider(names_from =  Metrics, values_from = Value) ->
  df_all_Sel_1_0000_best_mlp_models


df_all_Sel_1_0000_best_mlp_models


df_all_Sel_1_0000_best_mlp_models |>
  write_xlsx(str_c("A02/XLSX/", "df_all_Sel_1_0000_best_mlp_models.xlsx"))


df_best_mlp_models |>
  group_by(Variable, Rep, `Population`) |>
  summarise_at(vars(Cor, R2, RMSE, Time), mean) |>
  ungroup() |>
  group_by(Variable, `Population`) |>
  summarize_at(vars(Cor, R2, RMSE, Time), list(mean = mean, sd = sd)) |>
  ungroup() ->
  df_best_mlp_models_summarized


df_best_mlp_models_summarized


df_best_mlp_models_summarized |>
  write_xlsx(str_c("A02/XLSX/", "df_best_mlp_models_summarized.xlsx"))



df_all_Sel_1_0000_best_mlp_models |>
  group_by(Variable, Rep, Population) |>
  summarise_at(vars(Cor, R2, RMSE, Time), mean) |>
  ungroup() |>
  group_by(Variable, `Population`) |>
  summarize_at(vars(Cor, R2, RMSE, Time), list(mean = mean, sd = sd)) |>
  ungroup() ->
  df_all_Sel_1_0000_best_mlp_models_summarized


df_all_Sel_1_0000_best_mlp_models_summarized


df_all_Sel_1_0000_best_mlp_models_summarized |>
  write_xlsx(str_c(
    "A02/XLSX/",
    "df_all_Sel_1_0000_best_mlp_models_summarized.xlsx"
  ))



final_df |>
  select_at(
    vars(
      Variable,
      Ano,
      Prop_Marker,
      Neurons,
      Maxit,
      Learn_Rate,
      Fold,
      Cor_t,
      Cor_v,
      R2_t,
      R2_v,
      RMSE_t,
      RMSE_v,
      Time
    )
  ) |>
  group_by(Variable, Ano, Prop_Marker, Neurons, Maxit, Learn_Rate) |>
  summarise_at(vars(Cor_t, Cor_v, R2_t, R2_v, RMSE_t, RMSE_v, Time),
               list(mean = mean, sd = sd)) |>
  ungroup() |>
  rename_at(vars(matches("Time")), ~ str_replace_all(., "_", "_t_")) |>
  group_by(Ano) |>
  slice_max(R2_v_mean) |>
  ungroup() |>
  pivot_longer(
    cols = Cor_t_mean:last_col(),
    values_to = "Value",
    names_sep = "_",
    names_to = c("Metrics", "Population", "Estimate")
  ) |>
  mutate_at(vars(`Population`),
            ~ case_match(., "t" ~ "Training", "v" ~ "Validation")) |>
  unite("Metrics", Metrics, Estimate) |>
  pivot_wider(names_from = "Metrics", values_from = Value) |>
  mutate_if(is.character, as_factor) ->
  final_df_str_by_mean_fold


final_df_str_by_mean_fold


final_df_str_by_mean_fold |>
  write_xlsx(str_c("A02/XLSX/", "final_df_str_by_mean_fold.xlsx"))


final_df |>
  select_at(
    vars(
      Variable,
      Ano,
      Prop_Marker,
      Neurons,
      Maxit,
      Learn_Rate,
      Fold,
      Cor_t,
      Cor_v,
      R2_t,
      R2_v,
      RMSE_t,
      RMSE_v,
      Time
    )
  ) |>
  group_by(Ano, Fold) |>
  slice_max(R2_v) |>
  ungroup() |>
  rename(Time_t = Time) |>
  pivot_longer(
    cols = Cor_t:last_col(),
    values_to = "Value",
    names_sep = "_",
    names_to = c("Metrics", "Population")
  ) |>
  mutate_at(vars(`Population`),
            ~ case_match(., "t" ~ "Training", "v" ~ "Validation")) |>
  mutate_if(is.character, as_factor) |>
  pivot_wider(names_from =  Metrics, values_from = Value) ->
  final_df_best_mlp_models


final_df_best_mlp_models


final_df_best_mlp_models |>
  write_xlsx(str_c("A02/XLSX/", "final_df_best_mlp_models.xlsx"))


final_df_best_mlp_models |>
  group_by(Variable, Ano, `Population`) |>
  summarise_at(vars(Cor, R2, RMSE, Time), list(mean = mean, sd = sd)) |>
  ungroup() ->
  final_df_best_mlp_models_summarized


final_df_best_mlp_models_summarized


final_df_best_mlp_models_summarized |>
  write_xlsx(str_c("A02/XLSX/", "final_df_best_mlp_models_summarized.xlsx"))


blue_2020_no_design_df |>
  select_at(
    vars(
      Variable,
      Prop_Marker,
      Neurons,
      Maxit,
      Learn_Rate,
      Fold,
      Cor_t,
      Cor_v,
      R2_t,
      R2_v,
      RMSE_t,
      RMSE_v,
      Time
    )
  ) |>
  group_by(Variable, Prop_Marker, Neurons, Maxit, Learn_Rate) |>
  summarise_at(vars(Cor_t, Cor_v, R2_t, R2_v, RMSE_t, RMSE_v, Time),
               list(mean = mean, sd = sd)) |>
  ungroup() |>
  rename_at(vars(matches("Time")), ~ str_replace_all(., "_", "_t_")) |>
  slice_max(R2_v_mean) |>
  ungroup() |>
  pivot_longer(
    cols = Cor_t_mean:last_col(),
    values_to = "Value",
    names_sep = "_",
    names_to = c("Metrics", "Population", "Estimate")
  ) |>
  mutate_at(vars(`Population`),
            ~ case_match(., "t" ~ "Training", "v" ~ "Validation")) |>
  unite("Metrics", Metrics, Estimate) |>
  pivot_wider(names_from = "Metrics", values_from = Value) |>
  mutate_if(is.character, as_factor) ->
  blue_2020_no_design_str_by_mean_fold


blue_2020_no_design_str_by_mean_fold


blue_2020_no_design_str_by_mean_fold |>
  write_xlsx(str_c("A02/XLSX/", "blue_2020_no_design_str_by_mean_fold.xlsx"))


blue_2020_no_design_df |>
  select_at(
    vars(
      Variable,
      Prop_Marker,
      Neurons,
      Maxit,
      Learn_Rate,
      Fold,
      Cor_t,
      Cor_v,
      R2_t,
      R2_v,
      RMSE_t,
      RMSE_v,
      Time
    )
  ) |>
  group_by(Fold) |>
  slice_max(R2_v) |>
  ungroup() |>
  rename(Time_t = Time) |>
  pivot_longer(
    cols = Cor_t:last_col(),
    values_to = "Value",
    names_sep = "_",
    names_to = c("Metrics", "Population")
  ) |>
  mutate_at(vars(`Population`),
            ~ case_match(., "t" ~ "Training", "v" ~ "Validation")) |>
  mutate_if(is.character, as_factor) |>
  pivot_wider(names_from =  Metrics, values_from = Value) ->
  blue_2020_no_design_df_best_mlp_models


blue_2020_no_design_df_best_mlp_models


blue_2020_no_design_df_best_mlp_models |>
  write_xlsx(str_c("A02/XLSX/", "blue_2020_no_design_df_best_mlp_models.xlsx"))


blue_2020_no_design_df_best_mlp_models |>
  group_by(Variable, `Population`) |>
  summarise_at(vars(Cor, R2, RMSE, Time), list(mean = mean, sd = sd)) |>
  ungroup() ->
  blue_2020_no_design_df_best_mlp_models_summarized


blue_2020_no_design_df_best_mlp_models_summarized


blue_2020_no_design_df_best_mlp_models_summarized |>
  write_xlsx(str_c(
    "A02/XLSX/",
    "blue_2020_no_design_df_best_mlp_models_summarized.xlsx"
  ))


bglr_rkhs_data |>
  select_at(vars(
    Variable,
    Prop_Marker,
    Fold,
    Rep,
    Cor_t,
    Cor_v,
    R2_t,
    R2_v,
    RMSE_t,
    RMSE_v,
    Time_t
  )) |>
  filter_at(vars(Prop_Marker), ~ . != "1_0000") |>
  group_by(Variable, Prop_Marker, Rep) |>
  summarise_at(vars(Cor_t, Cor_v, R2_t, R2_v, RMSE_t, RMSE_v, Time_t), mean) |>
  ungroup() |>
  group_by(Variable, Prop_Marker) |>
  summarise_at(vars(Cor_t, Cor_v, R2_t, R2_v, RMSE_t, RMSE_v, Time_t),
               list(mean = mean, sd = sd)) |>
  ungroup() |>
  group_by(Variable) |>
  slice_max(R2_v_mean) |>
  ungroup() |>
  pivot_longer(
    cols = Cor_t_mean:last_col(),
    values_to = "Value",
    names_sep = "_",
    names_to = c("Metrics", "Population", "Estimate")
  ) |>
  mutate_at(vars(`Population`),
            ~ case_match(., "t" ~ "Training", "v" ~ "Validation")) |>
  unite("Metrics", Metrics, Estimate) |>
  pivot_wider(names_from = "Metrics", values_from = Value) |>
  mutate_if(is.character, as_factor) ->
  bglr_rkhs_selected_prop_marker_by_mean_fold


bglr_rkhs_selected_prop_marker_by_mean_fold


bglr_rkhs_selected_prop_marker_by_mean_fold |>
  write_xlsx(str_c(
    "A02/XLSX/",
    "bglr_rkhs_selected_prop_marker_by_mean_fold.xlsx"
  ))


bglr_rkhs_data |>
  select_at(vars(
    Variable,
    Prop_Marker,
    Fold,
    Rep,
    Cor_t,
    Cor_v,
    R2_t,
    R2_v,
    RMSE_t,
    RMSE_v,
    Time_t
  )) |>
  filter_at(vars(Prop_Marker), ~ . != "1_0000") |>
  group_by(Variable, Rep, Fold) |>
  slice_max(R2_v) |>
  ungroup() |>
  pivot_longer(
    cols = Cor_t:last_col(),
    values_to = "Value",
    names_sep = "_",
    names_to = c("Metrics", "Population")
  ) |>
  mutate_at(vars(`Population`),
            ~ case_match(., "t" ~ "Training", "v" ~ "Validation")) |>
  mutate_if(is.character, as_factor) |>
  pivot_wider(names_from =  Metrics, values_from = Value) ->
  df_best_bglr_rkhs_models


df_best_bglr_rkhs_models


df_best_bglr_rkhs_models |>
  write_xlsx(str_c("A02/XLSX/", "df_best_bglr_rkhs_models.xlsx"))


df_best_bglr_rkhs_models |>
  group_by(Variable, Rep, `Population`) |>
  summarise_at(vars(Cor, R2, RMSE, Time), mean) |>
  ungroup() |>
  group_by(Variable, `Population`) |>
  summarize_at(vars(Cor, R2, RMSE, Time), list(mean = mean, sd = sd)) |>
  ungroup() ->
  df_best_bglr_rkhs_models_summarized


df_best_bglr_rkhs_models_summarized


df_best_bglr_rkhs_models_summarized |>
  write_xlsx(str_c("A02/XLSX/", "df_best_bglr_rkhs_models_summarized.xlsx"))


bglr_rkhs_data |>
  select_at(vars(
    Variable,
    Prop_Marker,
    Fold,
    Rep,
    Cor_t,
    Cor_v,
    R2_t,
    R2_v,
    RMSE_t,
    RMSE_v,
    Time_t
  )) |>
  filter_at(vars(Prop_Marker), ~ . == "1_0000") |>
  pivot_longer(
    cols = Cor_t:last_col(),
    values_to = "Value",
    names_sep = "_",
    names_to = c("Metrics", "Population")
  ) |>
  mutate_at(vars(`Population`),
            ~ case_match(., "t" ~ "Training", "v" ~ "Validation")) |>
  mutate_if(is.character, as_factor) |>
  pivot_wider(names_from =  Metrics, values_from = Value) ->
  df_bglr_rkhs_prop_marker_1_0000_model


df_bglr_rkhs_prop_marker_1_0000_model


df_bglr_rkhs_prop_marker_1_0000_model |>
  write_xlsx(str_c("A02/XLSX/", "df_bglr_rkhs_prop_marker_1_0000_model.xlsx"))


df_bglr_rkhs_prop_marker_1_0000_model |>
  group_by(Variable, Rep, `Population`) |>
  summarise_at(vars(Cor, R2, RMSE, Time), mean) |>
  ungroup() |>
  group_by(Variable, `Population`) |>
  summarize_at(vars(Cor, R2, RMSE, Time), list(mean = mean, sd = sd)) |>
  ungroup() ->
  df_bglr_rkhs_prop_marker_1_0000_model_summarized


df_bglr_rkhs_prop_marker_1_0000_model_summarized


df_bglr_rkhs_prop_marker_1_0000_model_summarized |>
  write_xlsx(str_c(
    "A02/XLSX/",
    "df_bglr_rkhs_prop_marker_1_0000_model_summarized.xlsx"
  ))



df_bglr_rkhs_prop_marker_1_0000_model_summarized |>
  add_column(Model = factor("RKHS models with all markers"),
             .before = TRUE) |>
  bind_rows(
    df_all_Sel_1_0000_best_mlp_models_summarized |>
      add_column(
        Model = factor("Best MLP models with all markers"),
        .before = TRUE
      )
  ) |>
  bind_rows(df_best_bglr_rkhs_models_summarized |>
              add_column(
                Model = factor("Best RKHS models within selected markers"),
                .before = TRUE
              )) |>
  bind_rows(df_best_mlp_models_summarized |>
              add_column(
                Model = factor("Best MLP models within selected markers"),
                .before = TRUE
              )) ->
  df_bglr_rkhs_mlp_best_models_summarized



df_bglr_rkhs_mlp_best_models_summarized |>
  write_xlsx(str_c("A02/XLSX/", "df_bglr_rkhs_mlp_best_models_summarized.xlsx"))



df_bglr_rkhs_prop_marker_1_0000_model_summarized |>
  add_column(Model = factor("RKHS models with all markers"),
             .before = TRUE) |>
  bind_rows(
    res_all_Sel_1_0000_str_by_mean_rep_fold |>
      select_at(vars(!Prop_Marker:Learn_Rate)) |>
      add_column(
        Model = factor("Best MLP models with all markers"),
        .before = TRUE
      )
  ) |>
  bind_rows(
    bglr_rkhs_selected_prop_marker_by_mean_fold |>
      select_at(vars(!Prop_Marker)) |>
      add_column(
        Model = factor("Best RKHS models within selected markers"),
        .before = TRUE
      )
  ) |>
  bind_rows(
    res_table_str_by_mean_rep_fold |>
      select_at(vars(!Prop_Marker:Learn_Rate)) |>
      add_column(
        Model = factor("Best MLP models within selected markers"),
        .before = TRUE
      )
  ) ->
  df_bglr_rkhs_mlp_best_models_after_summarizing


df_bglr_rkhs_mlp_best_models_after_summarizing |>
  write_xlsx(str_c(
    "A02/XLSX/",
    "df_bglr_rkhs_mlp_best_models_after_summarizing.xlsx"
  ))

##


df_bglr_rkhs_prop_marker_1_0000_model_summarized |>
  add_column(
    Model = factor("RKHS"),
    Markers = factor("All markers"),
    Selection = factor("Before summarizing"),
    .before = TRUE
  ) |>
  bind_rows(
    df_all_Sel_1_0000_best_mlp_models_summarized |>
      add_column(
        Model = factor("MLP"),
        Markers = factor("All markers"),
        Selection = factor("Before summarizing"),
        .before = TRUE
      )
  ) |>
  bind_rows(
    df_best_bglr_rkhs_models_summarized |>
      add_column(
        Model = factor("RKHS"),
        Markers = factor("Selected markers"),
        Selection = factor("Before summarizing"),
        .before = TRUE
      )
  ) |>
  bind_rows(
    df_best_mlp_models_summarized |>
      add_column(
        Model = factor("MLP"),
        Markers = factor("Selected markers"),
        Selection = factor("Before summarizing"),
        .before = TRUE
      )
  ) ->
  df_bglr_rkhs_mlp_best_models_summarized_2



df_bglr_rkhs_mlp_best_models_summarized_2 |>
  write_xlsx(str_c(
    "A02/XLSX/",
    "df_bglr_rkhs_mlp_best_models_summarized_2.xlsx"
  ))


df_bglr_rkhs_prop_marker_1_0000_model_summarized |>
  add_column(
    Model = factor("RKHS"),
    Markers = factor("All markers"),
    Selection = factor("After summarizing"),
    .before = TRUE
  ) |>
  bind_rows(
    res_all_Sel_1_0000_str_by_mean_rep_fold |>
      select_at(vars(!Prop_Marker:Learn_Rate)) |>
      add_column(
        Model = factor("MLP"),
        Markers = factor("All markers"),
        Selection = factor("After summarizing"),
        .before = TRUE
      )
  ) |>
  bind_rows(
    bglr_rkhs_selected_prop_marker_by_mean_fold |>
      select_at(vars(!Prop_Marker)) |>
      add_column(
        Model = factor("RKHS"),
        Markers = factor("Selected markers"),
        Selection = factor("After summarizing"),
        .before = TRUE
      )
  ) |>
  bind_rows(
    res_table_str_by_mean_rep_fold |>
      select_at(vars(!Prop_Marker:Learn_Rate)) |>
      add_column(
        Model = factor("MLP"),
        Markers = factor("Selected markers"),
        Selection = factor("After summarizing"),
        .before = TRUE
      )
  ) ->
  df_bglr_rkhs_mlp_best_models_after_summarizing_2


df_bglr_rkhs_mlp_best_models_after_summarizing_2 |>
  write_xlsx(str_c(
    "A02/XLSX/",
    "df_bglr_rkhs_mlp_best_models_after_summarizing_2.xlsx"
  ))


df_bglr_rkhs_mlp_best_models_summarized_2 |>
  bind_rows(df_bglr_rkhs_mlp_best_models_after_summarizing_2) |>
  mutate_at(vars(Variable), ~ as_factor(
    case_match(
      .,
      "brix" ~ "Brix",
      "comp" ~ "Length",
      "diam" ~ "Diameter",
      "n.entr" ~ "Internodes",
      "peso" ~ "Weight"
    )
  )) ->
  df_bglr_rkhs_mlp_best_models


df_bglr_rkhs_mlp_best_models |>
  write_xlsx(str_c("A02/XLSX/", "df_bglr_rkhs_mlp_best_models.xlsx"))


df_bglr_rkhs_mlp_best_models |>
  saveRDS(str_c("A02/RDS/", "df_bglr_rkhs_mlp_best_models.rds"))



df_bglr_rkhs_mlp_best_models <-
  readRDS(str_c("A02/RDS/", "df_bglr_rkhs_mlp_best_models.rds"))



# Graphics ----------------------------------------------------------------


df_best_mlp_models_summarized |>
  ggplot(aes(x = Variable, y = R2_mean, fill = Population)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = R2_mean - R2_sd, ymax = R2_mean + R2_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(name = "R<sup>2</sup>",
                     limits = c(0, 1),
                     n.breaks = 5) +
  theme_bw() +
  ggsci::scale_fill_jama() +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) ->
  p01_r2_by_str_within_rep_fold


p01_r2_by_str_within_rep_fold


ggsave(
  filename = "p01_r2_by_str_within_rep_fold.svg",
  plot = p01_r2_by_str_within_rep_fold,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


res_table_str_by_mean_rep_fold  |>
  ggplot(aes(x = Variable, y = R2_mean, fill = Population)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = R2_mean - R2_sd, ymax = R2_mean + R2_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(limits = c(0, 1), n.breaks = 5) +
  theme_bw() +
  ggsci::scale_fill_jama() +
  labs(y = "R<sup>2</sup>") +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) ->
  p02_r2_by_structure


p02_r2_by_structure


ggsave(
  filename = "p02_r2_by_structure.svg",
  plot = p02_r2_by_structure,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


final_df_best_mlp_models_summarized |>
  ggplot(aes(x = Variable, y = R2_mean, fill = Population)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = R2_mean - R2_sd, ymax = R2_mean + R2_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(name = "R<sup>2</sup>",
                     limits = c(0, 1),
                     n.breaks = 5) +
  theme_bw() +
  ggsci::scale_fill_jama() +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12, color = "black")
  ) +
  facet_wrap(. ~ Ano) ->
  p03_final_r2_by_str_within_fold


p03_final_r2_by_str_within_fold


ggsave(
  filename = "p03_final_r2_by_str_within_fold.svg",
  plot = p03_final_r2_by_str_within_fold,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


final_df_str_by_mean_fold |>
  ggplot(aes(x = Variable, y = R2_mean, fill = Population)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = R2_mean - R2_sd, ymax = R2_mean + R2_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(limits = c(0, 1), n.breaks = 5) +
  theme_bw() +
  ggsci::scale_fill_jama() +
  labs(y = "R<sup>2</sup>") +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12, color = "black")
  ) +
  facet_wrap(. ~ Ano) ->
  p04_final_r2_by_structure


p04_final_r2_by_structure


ggsave(
  filename = "p04_final_r2_by_structure.svg",
  plot = p04_final_r2_by_structure,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)



blue_2020_no_design_df_best_mlp_models_summarized |>
  ggplot(aes(x = Variable, y = R2_mean, fill = Population)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = R2_mean - R2_sd, ymax = R2_mean + R2_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(limits = c(0, 1), n.breaks = 5) +
  scale_x_discrete(labels = "Peso (Ano = 2020)") +
  theme_bw() +
  ggsci::scale_fill_jama() +
  labs(y = "R<sup>2</sup>") +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12, color = "black")
  ) +
  facet_wrap(. ~ Population) ->
  p05_r2_blue_2020_no_design_by_str_within_fold


p05_r2_blue_2020_no_design_by_str_within_fold


ggsave(
  filename = "p05_r2_blue_2020_no_design_by_str_within_fold.svg",
  plot = p05_r2_blue_2020_no_design_by_str_within_fold,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


blue_2020_no_design_str_by_mean_fold |>
  ggplot(aes(x = Variable, y = R2_mean, fill = Population)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = R2_mean - R2_sd, ymax = R2_mean + R2_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(limits = c(0, 1), n.breaks = 5) +
  scale_x_discrete(labels = "Peso (Ano = 2020)") +
  theme_bw() +
  ggsci::scale_fill_jama() +
  labs(y = "R<sup>2</sup>") +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12, color = "black")
  ) +
  facet_wrap(. ~ Population) ->
  p06_r2_blue_2020_no_design_by_structure


p06_r2_blue_2020_no_design_by_structure


ggsave(
  filename = "p06_r2_blue_2020_no_design_by_structure.svg",
  plot = p06_r2_blue_2020_no_design_by_structure,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


df_best_mlp_models_summarized |>
  ggplot(aes(x = Variable, y = RMSE_mean, fill = Population)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = RMSE_mean - RMSE_sd, ymax = RMSE_mean + RMSE_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(name = "RMSE",
                     limits = c(0, 20),
                     n.breaks = 5) +
  theme_bw() +
  ggsci::scale_fill_jama() +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) ->
  p07_rmse_by_str_within_rep_fold


p07_rmse_by_str_within_rep_fold


ggsave(
  filename = "p07_rmse_by_str_within_rep_fold.svg",
  plot = p07_rmse_by_str_within_rep_fold,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


res_table_str_by_mean_rep_fold  |>
  ggplot(aes(x = Variable, y = RMSE_mean, fill = Population)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = RMSE_mean - RMSE_sd, ymax = RMSE_mean + RMSE_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(limits = c(0, 25), n.breaks = 5) +
  theme_bw() +
  ggsci::scale_fill_jama() +
  labs(y = "RMSE") +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) ->
  p08_rmse_by_structure


p08_rmse_by_structure


ggsave(
  filename = "p08_rmse_by_structure.svg",
  plot = p08_rmse_by_structure,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


final_df_best_mlp_models_summarized |>
  ggplot(aes(x = Variable, y = RMSE_mean, fill = Population)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = RMSE_mean - RMSE_sd, ymax = RMSE_mean + RMSE_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(name = "RMSE",
                     limits = c(0, 40),
                     n.breaks = 5) +
  theme_bw() +
  ggsci::scale_fill_jama() +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12, color = "black")
  ) +
  facet_wrap(. ~ Ano) ->
  p09_final_rmse_by_str_within_fold


p09_final_rmse_by_str_within_fold


ggsave(
  filename = "p09_final_rmse_by_str_within_fold.svg",
  plot = p09_final_rmse_by_str_within_fold,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


final_df_str_by_mean_fold |>
  ggplot(aes(x = Variable, y = RMSE_mean, fill = Population)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = RMSE_mean - RMSE_sd, ymax = RMSE_mean + RMSE_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(limits = c(0, 40), n.breaks = 5) +
  theme_bw() +
  ggsci::scale_fill_jama() +
  labs(y = "RMSE") +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12, color = "black")
  ) +
  facet_wrap(. ~ Ano) ->
  p10_final_rmse_by_structure


p10_final_rmse_by_structure


ggsave(
  filename = "p10_final_rmse_by_structure.svg",
  plot = p10_final_rmse_by_structure,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


blue_2020_no_design_df_best_mlp_models_summarized  |>
  ggplot(aes(x = Variable, y = RMSE_mean, fill = Population)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = RMSE_mean - RMSE_sd, ymax = RMSE_mean + RMSE_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_x_discrete(labels = "Peso (Ano = 2020)") +
  scale_y_continuous(limits = c(0, 25), n.breaks = 5) +
  theme_bw() +
  ggsci::scale_fill_jama() +
  labs(y = "RMSE") +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12, color = "black")
  ) +
  facet_wrap(. ~ Population) ->
  p11_rmse_blue_2020_no_design_by_str_within_fold


p11_rmse_blue_2020_no_design_by_str_within_fold


ggsave(
  filename = "p11_rmse_blue_2020_no_design_by_str_within_fold.svg",
  plot = p11_rmse_blue_2020_no_design_by_str_within_fold,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


blue_2020_no_design_str_by_mean_fold  |>
  ggplot(aes(x = Variable, y = RMSE_mean, fill = Population)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = RMSE_mean - RMSE_sd, ymax = RMSE_mean + RMSE_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_x_discrete(labels = "Peso (Ano = 2020)") +
  scale_y_continuous(limits = c(0, 25), n.breaks = 5) +
  theme_bw() +
  ggsci::scale_fill_jama() +
  labs(y = "RMSE") +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12, color = "black")
  ) +
  facet_wrap(. ~ Population) ->
  p12_rmse_blue_2020_no_design_by_structure


p12_rmse_blue_2020_no_design_by_structure


ggsave(
  filename = "p12_rmse_blue_2020_no_design_by_structure.svg",
  plot = p12_rmse_blue_2020_no_design_by_structure,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


df_best_mlp_models_summarized  |>
  filter_at(vars(Population), ~ . == "Training") |>
  droplevels() |>
  ggplot(aes(x = Variable, y = Time_mean, fill = Variable)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = Time_mean - Time_sd, ymax = Time_mean + Time_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(limits = c(0, 250), n.breaks = 5) +
  theme_bw() +
  ggsci::scale_fill_jama() +
  labs(y = "Time (s)") +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) ->
  p13_time_by_str_within_rep_fold


p13_time_by_str_within_rep_fold


ggsave(
  filename = "p13_time_by_str_within_rep_fold.svg",
  plot = p13_time_by_str_within_rep_fold,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


res_table_str_by_mean_rep_fold  |>
  filter_at(vars(Population), ~ . == "Training") |>
  droplevels() |>
  ggplot(aes(x = Variable, y = Time_mean, fill = Variable)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = Time_mean - Time_sd, ymax = Time_mean + Time_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(limits = c(0, 20), n.breaks = 5) +
  theme_bw() +
  ggsci::scale_fill_jama() +
  labs(y = "Time (s)") +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) ->
  p14_time_by_structure


p14_time_by_structure


ggsave(
  filename = "p14_time_by_structure.svg",
  plot = p14_time_by_structure,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


final_df_best_mlp_models_summarized  |>
  filter_at(vars(Population), ~ . == "Training") |>
  droplevels() |>
  ggplot(aes(x = Variable, y = Time_mean, fill = Ano)) +
  geom_errorbar(
    aes(ymin = Time_mean, ymax = Time_mean + Time_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  geom_col(position = "dodge", aes(color = Ano)) +
  scale_y_continuous(limits = c(0, 1200), n.breaks = 4) +
  theme_bw() +
  ggsci::scale_fill_jama() +
  ggsci::scale_color_jama() +
  labs(y = "Time (s)") +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12, color = "black")
  ) +
  facet_wrap(. ~ Ano) ->
  p15_final_time_by_str_within_fold


p15_final_time_by_str_within_fold


ggsave(
  filename = "p15_final_time_by_str_within_fold.svg",
  plot = p15_final_time_by_str_within_fold,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


final_df_str_by_mean_fold  |>
  filter_at(vars(Population), ~ . == "Training") |>
  droplevels() |>
  ggplot(aes(x = Variable, y = Time_mean, fill = Ano)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = Time_mean - Time_sd, ymax = Time_mean + Time_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(limits = c(0, 150), n.breaks = 5) +
  theme_bw() +
  ggsci::scale_fill_jama() +
  labs(y = "Time (s)") +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12, color = "black")
  ) +
  facet_wrap(. ~ Ano) ->
  p16_final_time_str_by_mean_fold


p16_final_time_str_by_mean_fold


ggsave(
  filename = "p16_final_time_str_by_mean_fold.svg",
  plot = p16_final_time_str_by_mean_fold,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


blue_2020_no_design_df_best_mlp_models_summarized |>
  filter_at(vars(Population), ~ . == "Training") |>
  droplevels() |>
  ggplot(aes(x = Variable, y = Time_mean, fill = Variable)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = Time_mean - Time_sd, ymax = Time_mean + Time_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(limits = c(0, 10), n.breaks = 5) +
  scale_x_discrete(labels = "Peso (Ano = 2020)") +
  theme_bw() +
  ggsci::scale_fill_jama() +
  labs(y = "Time (s)") +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12, color = "black")
  ) ->
  p17_time_blue_2020_no_design_by_str_within_fold



p17_time_blue_2020_no_design_by_str_within_fold


ggsave(
  filename = "p17_time_blue_2020_no_design_by_str_within_fold.svg",
  plot = p17_time_blue_2020_no_design_by_str_within_fold,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)


blue_2020_no_design_str_by_mean_fold  |>
  filter_at(vars(Population), ~ . == "Training") |>
  droplevels() |>
  ggplot(aes(x = Variable, y = Time_mean, fill = Variable)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = Time_mean - Time_sd, ymax = Time_mean + Time_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_x_discrete(labels = "Peso (Ano = 2020)") +
  scale_y_continuous(limits = c(0, 10), n.breaks = 5) +
  theme_bw() +
  ggsci::scale_fill_jama() +
  labs(y = "Time (s)") +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12, color = "black")
  ) ->
  p18_time_blue_2020_no_design_by_structure


p18_time_blue_2020_no_design_by_structure


ggsave(
  filename = "p18_time_blue_2020_no_design_by_structure.svg",
  plot = p18_time_blue_2020_no_design_by_structure,
  path = "A02/SVG/",
  scale = 1,
  width = 7,
  height = 4,
  dpi = 600
)



df_bglr_rkhs_mlp_best_models_summarized |>
  ggplot(aes(x = Variable, y = R2_mean, fill = Model)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = R2_mean - R2_sd, ymax = R2_mean + R2_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(
    name = "R<sup>2</sup>",
    limits = c(0, 1),
    n.breaks = 5,
    expand =  expansion(mult = c(0, 0.05)),
  ) +
  theme_bw() +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_x_discrete(
    labels = c(
      "brix" = "Brix",
      "comp" = "Length",
      "diam" = "Diameter",
      "n.entr" = "Internodes",
      "peso" = "Weight"
    )
  ) +
  theme(
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(
      size = 12,
      color = "black",
      angle = 90,
      hjust = 1,
      vjust = 0.4
    ),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    strip.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(. ~ Population, nrow = 2) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) ->
  p19_r2_within_rep_fold


p19_r2_within_rep_fold


ggsave(
  filename = "p19_r2_within_rep_fold.svg",
  plot = p19_r2_within_rep_fold,
  path = "A02/SVG/",
  scale = 1,
  width = 4.5,
  height = 9,
  dpi = 600
)



df_bglr_rkhs_mlp_best_models_summarized |>
  ggplot(aes(x = Variable, y = Cor_mean, fill = Model)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = Cor_mean - Cor_sd, ymax = Cor_mean + Cor_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(
    name = "Predictive ability",
    limits = c(0, 1),
    n.breaks = 5,
    expand =  expansion(mult = c(0, 0.05)),
  ) +
  theme_bw() +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_x_discrete(
    labels = c(
      "brix" = "Brix",
      "comp" = "Length",
      "diam" = "Diameter",
      "n.entr" = "Internodes",
      "peso" = "Weight"
    )
  ) +
  theme(
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(
      size = 12,
      color = "black",
      angle = 90,
      hjust = 1,
      vjust = 0.4
    ),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    strip.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(. ~ Population, nrow = 2) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) ->
  p20_Cor_within_rep_fold


p20_Cor_within_rep_fold


ggsave(
  filename = "p20_Cor_within_rep_fold.svg",
  plot = p20_Cor_within_rep_fold,
  path = "A02/SVG/",
  scale = 1,
  width = 4.5,
  height = 9,
  dpi = 600
)



df_bglr_rkhs_mlp_best_models_summarized |>
  ggplot(aes(x = Variable, y = RMSE_mean, fill = Model)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = RMSE_mean - RMSE_sd, ymax = RMSE_mean + RMSE_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(
    name = "RMSE",
    limits = c(0, 21),
    breaks = seq(0, 20, 5),
    expand =  expansion(mult = c(0, 0.05))
  ) +
  theme_bw() +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_x_discrete(
    labels = c(
      "brix" = "Brix",
      "comp" = "Length",
      "diam" = "Diameter",
      "n.entr" = "Internodes",
      "peso" = "Weight"
    )
  ) +
  theme(
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(
      size = 12,
      color = "black",
      angle = 90,
      hjust = 1,
      vjust = 0.4
    ),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    strip.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(. ~ Population, nrow = 2) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))  ->
  p21_RMSE_within_rep_fold


p21_RMSE_within_rep_fold


ggsave(
  filename = "p21_RMSE_within_rep_fold.svg",
  plot = p21_RMSE_within_rep_fold,
  path = "A02/SVG/",
  scale = 1,
  width = 4.5,
  height = 9,
  dpi = 600
)



df_bglr_rkhs_mlp_best_models_summarized |>
  filter_at(vars(Time_mean), ~ !is.na(.)) |>
  ggplot(aes(x = Variable, y = Time_mean, fill = Model)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = Time_mean - Time_sd, ymax = Time_mean + Time_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(
    name = "Time (min)",
    limits = c(-3.5, 250),
    breaks = seq(0, 250, 50)
  ) +
  theme_bw() +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_x_discrete(
    labels = c(
      "brix" = "Brix",
      "comp" = "Length",
      "diam" = "Diameter",
      "n.entr" = "Internodes",
      "peso" = "Weight"
    )
  ) +
  theme(
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(
      size = 12,
      color = "black",
      angle = 90,
      hjust = 1,
      vjust = 0.4
    ),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    strip.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(. ~ Population, nrow = 2) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))  ->
  p22_time_within_rep_fold


p22_time_within_rep_fold


ggsave(
  filename = "p22_time_within_rep_fold.svg",
  plot = p22_time_within_rep_fold,
  path = "A02/SVG/",
  scale = 1,
  width = 4.5,
  height = 5,
  dpi = 600
)



df_bglr_rkhs_mlp_best_models_after_summarizing |>
  ggplot(aes(x = Variable, y = R2_mean, fill = Model)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = R2_mean - R2_sd, ymax = R2_mean + R2_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(
    name = "R<sup>2</sup>",
    limits = c(0, 1),
    n.breaks = 5,
    expand =  expansion(mult = c(0, 0.05)),
  ) +
  theme_bw() +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_x_discrete(
    labels = c(
      "brix" = "Brix",
      "comp" = "Length",
      "diam" = "Diameter",
      "n.entr" = "Internodes",
      "peso" = "Weight"
    )
  ) +
  theme(
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(
      size = 12,
      color = "black",
      angle = 90,
      hjust = 1,
      vjust = 0.4
    ),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    strip.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(. ~ Population, nrow = 2) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) ->
  p23_r2_best_models_after_summarizing


p23_r2_best_models_after_summarizing


ggsave(
  filename = "p23_r2_best_models_after_summarizing.svg",
  plot = p23_r2_best_models_after_summarizing,
  path = "A02/SVG/",
  scale = 1,
  width = 4.5,
  height = 9,
  dpi = 600
)



df_bglr_rkhs_mlp_best_models_after_summarizing |>
  ggplot(aes(x = Variable, y = Cor_mean, fill = Model)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = Cor_mean - Cor_sd, ymax = Cor_mean + Cor_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(
    name = "Predictive ability",
    limits = c(0, 1),
    n.breaks = 5,
    expand =  expansion(mult = c(0, 0.05)),
  ) +
  theme_bw() +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_x_discrete(
    labels = c(
      "brix" = "Brix",
      "comp" = "Length",
      "diam" = "Diameter",
      "n.entr" = "Internodes",
      "peso" = "Weight"
    )
  ) +
  theme(
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(
      size = 12,
      color = "black",
      angle = 90,
      hjust = 1,
      vjust = 0.4
    ),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    strip.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(. ~ Population, nrow = 2) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) ->
  p24_Cor_best_models_after_summarizing


p24_Cor_best_models_after_summarizing


ggsave(
  filename = "p24_Cor_best_models_after_summarizing.svg",
  plot = p24_Cor_best_models_after_summarizing,
  path = "A02/SVG/",
  scale = 1,
  width = 4.5,
  height = 9,
  dpi = 600
)



df_bglr_rkhs_mlp_best_models_after_summarizing |>
  ggplot(aes(x = Variable, y = RMSE_mean, fill = Model)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = RMSE_mean - RMSE_sd, ymax = RMSE_mean + RMSE_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(
    name = "RMSE",
    limits = c(0, 21),
    breaks = seq(0, 20, 5),
    expand =  expansion(mult = c(0, 0.05))
  ) +
  theme_bw() +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_x_discrete(
    labels = c(
      "brix" = "Brix",
      "comp" = "Length",
      "diam" = "Diameter",
      "n.entr" = "Internodes",
      "peso" = "Weight"
    )
  ) +
  theme(
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(
      size = 12,
      color = "black",
      angle = 90,
      hjust = 1,
      vjust = 0.4
    ),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    strip.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(. ~ Population, nrow = 2) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))  ->
  p25_RMSE_best_models_after_summarizing


p25_RMSE_best_models_after_summarizing


ggsave(
  filename = "p25_RMSE_best_models_after_summarizing.svg",
  plot = p25_RMSE_best_models_after_summarizing,
  path = "A02/SVG/",
  scale = 1,
  width = 4.5,
  height = 9,
  dpi = 600
)



df_bglr_rkhs_mlp_best_models_after_summarizing |>
  filter_at(vars(Time_mean), ~ !is.na(.)) |>
  ggplot(aes(x = Variable, y = Time_mean, fill = Model)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = Time_mean - Time_sd, ymax = Time_mean + Time_sd),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_y_continuous(
    name = "Time (min)",
    limits = c(-3.5, 250),
    breaks = seq(0, 250, 50),
    #    expand =  expansion(mult = c(0, 0.05)),
    position = "left"
  ) +
  theme_bw() +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_x_discrete(
    labels = c(
      "brix" = "Brix",
      "comp" = "Length",
      "diam" = "Diameter",
      "n.entr" = "Internodes",
      "peso" = "Weight"
    )
  ) +
  # scale_y_break(c(21, 120), scales = 1) +
  theme(
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(
      size = 12,
      color = "black",
      angle = 90,
      hjust = 1,
      vjust = 0.4
    ),
    axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    strip.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_blank()
  ) +
  facet_wrap(. ~ Population, nrow = 2) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) ->
  p26_time_best_models_after_summarizing


p26_time_best_models_after_summarizing


ggsave(
  filename = "p26_time_best_models_after_summarizing.svg",
  plot = p26_time_best_models_after_summarizing,
  path = "A02/SVG/",
  scale = 1,
  width = 4.5,
  height = 5,
  dpi = 600
)


ggarrange(
  p19_r2_within_rep_fold,
  p23_r2_best_models_after_summarizing,
  common.legend = TRUE,
  labels = str_c(LETTERS[1:2], ")")
) ->
  p27_r2


p27_r2


path_A02 <- c(SVG = "A02/SVG/", JPG = "A02/JPG/", PDF = "A02/PDF/")


path_A02 |>
  imap(\(x, idx) {
    ggsave(
      filename = str_c("p27_r2.", tolower(idx)),
      plot = p27_r2,
      path = x,
      scale = 1,
      width = 8.27,
      height = 8,
      dpi = 600
    )

  })




ggarrange(
  p20_Cor_within_rep_fold,
  p24_Cor_best_models_after_summarizing,
  common.legend = TRUE,
  labels = str_c(LETTERS[1:2], ")")
) ->
  p28_cor


p28_cor


path_A02 |>
  imap(\(x, idx) {
    ggsave(
      filename = str_c("p28_cor.", tolower(idx)),
      plot = p28_cor,
      path = x,
      scale = 1,
      width = 8.27,
      height = 8,
      dpi = 600
    )

  })



ggarrange(
  p21_RMSE_within_rep_fold,
  p25_RMSE_best_models_after_summarizing,
  common.legend = TRUE,
  labels = str_c(LETTERS[1:2], ")")
) ->
  p29_RMSE


p29_RMSE


path_A02 |>
  imap(\(x, idx) {
    ggsave(
      filename = str_c("p29_RMSE.", tolower(idx)),
      plot = p29_RMSE,
      path = x,
      scale = 1,
      width = 8.27,
      height = 8,
      dpi = 600
    )

  })



ggarrange(
  p22_time_within_rep_fold,
  p26_time_best_models_after_summarizing,
  common.legend = TRUE,
  labels = str_c(LETTERS[1:2], ")")
) ->
  p30_time


p30_time


path_A02 |>
  imap(\(x, idx) {
    ggsave(
      filename = str_c("p30_time.", tolower(idx)),
      plot = p30_time,
      path = x,
      scale = 1,
      width = 8.27,
      height = 5,
      dpi = 600
    )

  })



# Perspective graphics ----------------------------------------------------



list(
  P01 = list(FILL = "Selection", FACET = "Markers ~ Model"),
  P02 = list(FILL = "Model", FACET = "Markers  ~ Selection"),
  P03 = list(FILL = "Markers", FACET = "Model ~ Selection")
) ->
  perspective



perspective |>
  map(\(x) {
    df_bglr_rkhs_mlp_best_models |>
      filter_at(vars(Population), ~ . == "Validation") |>
      droplevels() |>
      ggplot(aes(
        x = Variable,
        y = R2_mean,
        fill = !!sym(x$FILL)
      )) +
      geom_col(position = "dodge") +
      geom_errorbar(
        aes(ymin = R2_mean - R2_sd, ymax = R2_mean + R2_sd),
        position = position_dodge(0.9),
        width = 0.25
      ) +
      scale_y_continuous(
        name = "R<sup>2</sup>",
        limits = c(0, 1),
        n.breaks = 5,
        expand =  expansion(mult = c(0, 0.05)),
      ) +
      theme_bw() +
      scale_fill_grey(start = 0.2, end = 0.6) +
      theme(
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(
          size = 12,
          color = "black",
          angle = 90,
          hjust = 1,
          vjust = 0.4
        ),
        axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 10.5),
        strip.text = element_text(size = 12),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "lines"),

      ) +
      facet_grid(x$FACET) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE))


  }) ->
  r2_perspectives


r2_perspectives



path_A02 <- c(SVG = "A02/SVG/", JPG = "A02/JPG/", PDF = "A02/PDF/")


r2_perspectives |>
  imap(\(y, idy) {
    path_A02 |>
      imap(\(x, idx) {
        ggsave(
          filename = str_c("R2_", idy, ".", tolower(idx)),
          plot = y,
          path = x,
          scale = 1,
          width = 8.27,
          height = 8,
          dpi = 600
        )

      })

  })



perspective |>
  map(\(x) {
    df_bglr_rkhs_mlp_best_models |>
      filter_at(vars(Population), ~ . == "Validation") |>
      droplevels() |>
      ggplot(aes(
        x = Variable,
        y = Cor_mean,
        fill = !!sym(x$FILL)
      )) +
      geom_col(position = "dodge") +
      geom_errorbar(
        aes(ymin = Cor_mean - Cor_sd, ymax = Cor_mean + Cor_sd),
        position = position_dodge(0.9),
        width = 0.25
      ) +
      scale_y_continuous(
        name = "Predictive ability",
        limits = c(0, 1),
        n.breaks = 5,
        expand =  expansion(mult = c(0, 0.05)),
      ) +
      theme_bw() +
      scale_fill_grey(start = 0.2, end = 0.6) +
      theme(
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(
          size = 12,
          color = "black",
          angle = 90,
          hjust = 1,
          vjust = 0.4
        ),
        axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 10.5),
        strip.text = element_text(size = 12),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "lines"),

      ) +
      facet_grid(x$FACET) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE))


  }) ->
  cor_perspectives


cor_perspectives |>
  imap(\(y, idy) {
    path_A02 |>
      imap(\(x, idx) {
        ggsave(
          filename = str_c("Cor_", idy, ".", tolower(idx)),
          plot = y,
          path = x,
          scale = 1,
          width = 8.27,
          height = 8,
          dpi = 600
        )

      })

  })



perspective |>
  map(\(x) {
    df_bglr_rkhs_mlp_best_models |>
      filter_at(vars(Population), ~ . == "Validation") |>
      droplevels() |>
      ggplot(aes(
        x = Variable,
        y = RMSE_mean,
        fill = !!sym(x$FILL)
      )) +
      geom_col(position = "dodge") +
      geom_errorbar(
        aes(ymin = RMSE_mean - RMSE_sd, ymax = RMSE_mean + RMSE_sd),
        position = position_dodge(0.9),
        width = 0.25
      ) +
      scale_y_continuous(
        name = "RMSE",
        limits = c(0, 21),
        breaks = seq(0, 20, 5),
        expand =  expansion(mult = c(0, 0.05))
      ) +
      theme_bw() +
      scale_fill_grey(start = 0.2, end = 0.6) +
      theme(
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(
          size = 12,
          color = "black",
          angle = 90,
          hjust = 1,
          vjust = 0.4
        ),
        axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 10.5),
        strip.text = element_text(size = 12),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "lines"),

      ) +
      facet_grid(x$FACET) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE))


  }) ->
  rmse_perspectives


rmse_perspectives


rmse_perspectives |>
  imap(\(y, idy) {
    path_A02 |>
      imap(\(x, idx) {
        ggsave(
          filename = str_c("RMSE_", idy, ".", tolower(idx)),
          plot = y,
          path = x,
          scale = 1,
          width = 8.27,
          height = 8,
          dpi = 600
        )

      })

  })


perspective |>
  map(\(x) {
    df_bglr_rkhs_mlp_best_models |>
      filter_at(vars(Population), ~ . == "Training") |>
      droplevels() |>
      ggplot(aes(
        x = Variable,
        y = Time_mean,
        fill = !!sym(x$FILL)
      )) +
      geom_col(position = "dodge") +
      geom_errorbar(
        aes(ymin = Time_mean - Time_sd, ymax = Time_mean + Time_sd),
        position = position_dodge(0.9),
        width = 0.25
      ) +
      scale_y_continuous(
        name = "Time (min)",
        limits = c(-3.5, 250),
        breaks = seq(0, 250, 50)
      ) +
      theme_bw() +
      scale_fill_grey(start = 0.2, end = 0.6) +
      theme(
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(
          size = 12,
          color = "black",
          angle = 90,
          hjust = 1,
          vjust = 0.4
        ),
        axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 10.5),
        strip.text = element_text(size = 12),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "lines"),

      ) +
      facet_grid(x$FACET) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE))


  }) ->
  time_perspectives


time_perspectives


time_perspectives |>
  imap(\(y, idy) {
    path_A02 |>
      imap(\(x, idx) {
        ggsave(
          filename = str_c("Time_", idy, ".", tolower(idx)),
          plot = y,
          path = x,
          scale = 1,
          width = 8.27,
          height = 8,
          dpi = 600
        )

      })

  })




# Perspective graphics 2 --------------------------------------------------


list(
  P04 = list(FILL = "Population", FACET = "Markers ~ Model"),
  P05 = list(FILL = "Model", FACET = "Markers  ~ Population"),
  P06 = list(FILL = "Markers", FACET = "Model ~ Population")
) ->
  perspective_2



perspective_2 |>
  map(\(x) {
    df_bglr_rkhs_mlp_best_models |>
      filter_at(vars(Selection), ~ . == "After summarizing") |>
      droplevels() |>
      ggplot(aes(
        x = Variable,
        y = R2_mean,
        fill = !!sym(x$FILL)
      )) +
      geom_col(position = "dodge") +
      geom_errorbar(
        aes(ymin = R2_mean - R2_sd, ymax = R2_mean + R2_sd),
        position = position_dodge(0.9),
        width = 0.25
      ) +
      scale_y_continuous(
        name = "R<sup>2</sup>",
        limits = c(0, 1),
        n.breaks = 5,
        expand =  expansion(mult = c(0, 0.05)),
      ) +
      theme_bw() +
      scale_fill_grey(start = 0.2, end = 0.6) +
      theme(
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(
          size = 12,
          color = "black",
          angle = 90,
          hjust = 1,
          vjust = 0.4
        ),
        axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 10.5),
        strip.text = element_text(size = 12),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "lines"),

      ) +
      facet_grid(x$FACET) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE))


  }) ->
  r2_perspectives_2


r2_perspectives_2


r2_perspectives_2 |>
  imap(\(y, idy) {
    path_A02 |>
      imap(\(x, idx) {
        ggsave(
          filename = str_c("R2_", idy, ".", tolower(idx)),
          plot = y,
          path = x,
          scale = 1,
          width = 8.27,
          height = 8,
          dpi = 600
        )

      })

  })



perspective_2 |>
  map(\(x) {
    df_bglr_rkhs_mlp_best_models |>
      filter_at(vars(Selection), ~ . == "After summarizing") |>
      droplevels() |>
      ggplot(aes(
        x = Variable,
        y = Cor_mean,
        fill = !!sym(x$FILL)
      )) +
      geom_col(position = "dodge") +
      geom_errorbar(
        aes(ymin = Cor_mean - Cor_sd, ymax = Cor_mean + Cor_sd),
        position = position_dodge(0.9),
        width = 0.25
      ) +
      scale_y_continuous(
        name = "Predictive ability",
        limits = c(0, 1),
        n.breaks = 5,
        expand =  expansion(mult = c(0, 0.05)),
      ) +
      theme_bw() +
      scale_fill_grey(start = 0.2, end = 0.6) +
      theme(
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(
          size = 12,
          color = "black",
          angle = 90,
          hjust = 1,
          vjust = 0.4
        ),
        axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 10.5),
        strip.text = element_text(size = 12),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "lines"),

      ) +
      facet_grid(x$FACET) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE))


  }) ->
  cor_perspectives_2


cor_perspectives_2 |>
  imap(\(y, idy) {
    path_A02 |>
      imap(\(x, idx) {
        ggsave(
          filename = str_c("Cor_", idy, ".", tolower(idx)),
          plot = y,
          path = x,
          scale = 1,
          width = 8.27,
          height = 8,
          dpi = 600
        )

      })

  })


perspective_2 |>
  map(\(x) {
    df_bglr_rkhs_mlp_best_models |>
      filter_at(vars(Selection), ~ . == "After summarizing") |>
      droplevels() |>
      ggplot(aes(
        x = Variable,
        y = RMSE_mean,
        fill = !!sym(x$FILL)
      )) +
      geom_col(position = "dodge") +
      geom_errorbar(
        aes(ymin = RMSE_mean - RMSE_sd, ymax = RMSE_mean + RMSE_sd),
        position = position_dodge(0.9),
        width = 0.25
      ) +
      scale_y_continuous(
        name = "RMSE",
        limits = c(0, 21),
        breaks = seq(0, 20, 5),
        expand =  expansion(mult = c(0, 0.05))
      ) +
      theme_bw() +
      scale_fill_grey(start = 0.2, end = 0.6) +
      theme(
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(
          size = 12,
          color = "black",
          angle = 90,
          hjust = 1,
          vjust = 0.4
        ),
        axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 10.5),
        strip.text = element_text(size = 12),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "lines"),

      ) +
      facet_grid(x$FACET) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE))


  }) ->
  rmse_perspectives_2


rmse_perspectives_2


rmse_perspectives_2 |>
  imap(\(y, idy) {
    path_A02 |>
      imap(\(x, idx) {
        ggsave(
          filename = str_c("RMSE_", idy, ".", tolower(idx)),
          plot = y,
          path = x,
          scale = 1,
          width = 8.27,
          height = 8,
          dpi = 600
        )

      })

  })



list(
  P04 = list(FILL = "Model", FACET = ". ~ Markers"),
  P05 = list(FILL = "Markers", FACET = ". ~ Model")
) ->
  perspective_time



perspective_time |>
  map(\(x) {
    df_bglr_rkhs_mlp_best_models |>
      filter_at(vars(Selection), ~ . == "After summarizing") |>
      filter_at(vars(Population), ~ . == "Training") |>
      droplevels() |>
      ggplot(aes(
        x = Variable,
        y = Time_mean,
        fill = !!sym(x$FILL)
      )) +
      geom_col(position = "dodge") +
      geom_errorbar(
        aes(ymin = Time_mean - Time_sd, ymax = Time_mean + Time_sd),
        position = position_dodge(0.9),
        width = 0.25
      ) +
      scale_y_continuous(
        name = "Time (min)",
        limits = c(-3.5, 250),
        breaks = seq(0, 250, 50)
      ) +
      theme_bw() +
      scale_fill_grey(start = 0.2, end = 0.6) +
      theme(
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(
          size = 12,
          color = "black",
          angle = 90,
          hjust = 1,
          vjust = 0.4
        ),
        axis.title.y = ggtext::element_markdown(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 10.5),
        strip.text = element_text(size = 12),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "lines"),

      ) +
      facet_grid(x$FACET) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE))


  }) ->
  time_perspectives_2


time_perspectives_2


time_perspectives_2 |>
  imap(\(y, idy) {
    path_A02 |>
      imap(\(x, idx) {
        ggsave(
          filename = str_c("Time_", idy, ".", tolower(idx)),
          plot = y,
          path = x,
          scale = 1,
          width = 8.27,
          height = 5,
          dpi = 600
        )

      })

  })



# New plots ---------------------------------------------------------------


df_bglr_rkhs_mlp_best_models <-
  readRDS(str_c("A02/RDS/", "df_bglr_rkhs_mlp_best_models.rds"))


df_bglr_rkhs_mlp_best_models |>
  pivot_longer(
    cols = Cor_mean:last_col(),
    names_sep = "_",
    names_to = c("Metrics", "Type"),
    values_to = "Value"
  ) |>
  mutate_at(vars(Metrics),
            ~ case_match(., "Cor" ~ "PA", "R2" ~ "R<sup>2</sup>", .default = .)) |>
  filter_at(vars(Metrics), ~ . != "Time") |>
  filter_at(vars(Selection), ~ . == "Before summarizing") |>
  filter_at(vars(Population), ~ . == "Validation") |>
  mutate_at(vars(Metrics), as_factor) |>
  mutate_at(vars(Type), str_to_title) |>
  pivot_wider(values_from  = Value, names_from = Type) |>
  droplevels() |>
  ggplot(aes(x = Variable, y = Mean, fill = Model)) +
  geom_col(width = 0.75, position = "dodge") +
  geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd),
                position = position_dodge(0.725),
                width = 0.25) +
  theme_bw() +
  scale_fill_grey(start = 0.2, end = 0.6) +
  theme(
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.4,
      size = 12,
      color = "black"
    ),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    strip.text = element_markdown(size = 12),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  ggh4x::facet_nested(Metrics ~ Markers, scales = "free_y") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  ggh4x::facetted_pos_scales(y = list(
    Metrics %in% c("PA", "R<sup>2</sup>") ~ scale_y_continuous(
      limits = c(0, 1),
      n.breaks = 6,
      expand = expansion(mult = c(0, 0.05))
    ),
    Metrics == "RMSE" ~ scale_y_continuous(
      limits = c(0, 21),
      breaks = seq(0, 20, 5),
      expand = expansion(mult = c(0, 0.05))
    )

  ))



df_bglr_rkhs_mlp_best_models |>
  pivot_longer(
    cols = Cor_mean:last_col(),
    names_sep = "_",
    names_to = c("Metrics", "Type"),
    values_to = "Value"
  ) |>
  mutate_at(vars(Metrics),
            ~ case_match(., "Cor" ~ "PA", "R2" ~ "R<sup>2</sup>", .default = .)) |>
  filter_at(vars(Metrics), ~ . != "Time") |>
  filter_at(vars(Selection), ~ . == "Before summarizing") |>
  filter_at(vars(Population), ~ . == "Training") |>
  mutate_at(vars(Metrics), as_factor) |>
  mutate_at(vars(Type), str_to_title) |>
  pivot_wider(values_from  = Value, names_from = Type) |>
  droplevels() |>
  ggplot(aes(x = Variable, y = Mean, fill = Model)) +
  geom_col(width = 0.75, position = "dodge") +
  geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd),
                position = position_dodge(0.725),
                width = 0.25) +
  theme_bw() +
  scale_fill_grey(start = 0.2, end = 0.6) +
  theme(
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.4,
      size = 12,
      color = "black"
    ),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    strip.text = element_markdown(size = 12),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  ggh4x::facet_nested(Metrics ~ Markers, scales = "free") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  ggh4x::facetted_pos_scales(y = list(
    Metrics %in% c("PA", "R<sup>2</sup>") ~ scale_y_continuous(
      limits = c(0, 1),
      n.breaks = 6,
      expand = expansion(mult = c(0, 0.05))
    ),
    Metrics == "RMSE" ~ scale_y_continuous(
      limits = c(0, 21),
      breaks = seq(0, 20, 5),
      expand = expansion(mult = c(0, 0.05))
    )

  ))


# Final Plots -------------------------------------------------------------


path_A02 <- c(SVG = "A02/SVG/", JPG = "A02/JPG/", PDF = "A02/PDF/")


df_bglr_rkhs_mlp_best_models <-
  readRDS(str_c("A02/RDS/", "df_bglr_rkhs_mlp_best_models.rds"))


df_bglr_rkhs_mlp_best_models |>
  pivot_longer(
    cols = Cor_mean:last_col(),
    names_sep = "_",
    names_to = c("Metrics", "Type"),
    values_to = "Value"
  ) |>
  mutate_at(vars(Metrics),
            ~ case_match(., "Cor" ~ "PA",
                         "R2" ~ "R<sup>2</sup>",
                         .default = .)) |>
  mutate_at(vars(Markers),
            ~ case_match(., "All markers" ~ "AM",
                         "Selected markers" ~ "SM")) |>
  filter_at(vars(Metrics), ~ . != "Time") |>
  filter_at(vars(Selection), ~ . == "Before summarizing") |>
  mutate_if(is_character, as_factor) |>
  mutate_at(vars(Type), str_to_title) |>
  pivot_wider(values_from  = Value, names_from = Type) |>
  droplevels() |>
  ggplot(aes(
    x = interaction(Markers, Population, sep = "|"),
    y = Mean,
    fill = Model
  )) +
  geom_col(width = 0.75, position = "dodge") +
  geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd),
                position = position_dodge(0.725),
                width = 0.25) +
  theme_bw() +
  scale_fill_grey(start = 0.2, end = 0.6) +
  scale_x_discrete(guide = guide_axis_nested(delim = "|"),
                   name = "Category") +
  theme(
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    strip.text = element_textbox(size = 10, face = "bold",
                                        halign = 0.5,
                                        margin = margin(0.75, 0, 0.75, 0, "mm")),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  facet_nested_wrap(vars(Variable, Metrics), ncol = 3, scales = "free_y") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  facetted_pos_scales(y = list(
    Metrics %in% c("PA", "R<sup>2</sup>") ~ scale_y_continuous(
      limits = c(0, 1),
      n.breaks = 6,
      expand = expansion(mult = c(0, 0.05))
    ),
    Metrics == "RMSE" ~ scale_y_continuous(expand = expansion(mult = c(0, 0.05)), n.breaks = 6)

  )) ->
  final_plot_bs


path_A02 |>
  imap(\(x, idx) {
    ggsave(
      filename = str_c("Final_Plot_BS", ".", tolower(idx)),
      plot = final_plot_bs,
      path = x,,
      units = "mm",
      scale = 1,
      width = 210,
      height = 250,
      dpi = 600
    )

  })



df_bglr_rkhs_mlp_best_models |>
  pivot_longer(
    cols = Cor_mean:last_col(),
    names_sep = "_",
    names_to = c("Metrics", "Type"),
    values_to = "Value"
  ) |>
  mutate_at(vars(Metrics),
            ~ case_match(., "Cor" ~ "PA",
                         "R2" ~ "R<sup>2</sup>",
                         .default = .)) |>
  mutate_at(vars(Markers),
            ~ case_match(., "All markers" ~ "AM",
                         "Selected markers" ~ "SM")) |>
  filter_at(vars(Metrics), ~ . != "Time") |>
  filter_at(vars(Selection), ~ . == "After summarizing") |>
  mutate_if(is_character, as_factor) |>
  mutate_at(vars(Type), str_to_title) |>
  pivot_wider(values_from  = Value, names_from = Type) |>
  droplevels() |>
  ggplot(aes(
    x = interaction(Markers, Population, sep = "|"),
    y = Mean,
    fill = Model
  )) +
  geom_col(width = 0.75, position = "dodge") +
  geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd),
                position = position_dodge(0.725),
                width = 0.25) +
  theme_bw() +
  scale_fill_grey(start = 0.2, end = 0.6) +
  scale_x_discrete(guide = guide_axis_nested(delim = "|"),
                   name = "Category") +
  theme(
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    strip.text = element_textbox(size = 10, face = "bold",
                                 halign = 0.5,
                                 margin = margin(0.75, 0, 0.75, 0, "mm")),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  facet_nested_wrap(vars(Variable, Metrics), ncol = 3, scales = "free_y") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  facetted_pos_scales(y = list(
    Metrics %in% c("PA", "R<sup>2</sup>") ~ scale_y_continuous(
      limits = c(0, 1),
      n.breaks = 6,
      expand = expansion(mult = c(0, 0.05))
    ),
    Metrics == "RMSE" ~ scale_y_continuous(expand = expansion(mult = c(0, 0.05)), n.breaks = 6)

  )) ->
  final_plot_as



path_A02 |>
  imap(\(x, idx) {
    ggsave(
      filename = str_c("Final_Plot_AS", ".", tolower(idx)),
      plot = final_plot_as,
      path = x,,
      units = "mm",
      scale = 1,
      width = 210,
      height = 250,
      dpi = 600
    )

  })


# Useful links ------------------------------------------------------------

# Changing legend rows
# https://stackoverflow.com/questions/27130610/legend-on-bottom-two-rows-wrapped-in-ggplot2-in-r
# https://www.statology.org/ggplot2-legend-multiple-rows/
# https://cran.r-project.org/web/packages/lemon/vignettes/legends.html


# Graphic scales
# http://www.sthda.com/english/wiki/ggplot2-axis-scales-and-transformations


# Development version ggh4x
# https://stackoverflow.com/questions/78411863/error-when-trying-to-plot-relative-abundance-using-ggplot-in-r
# https://github.com/teunbrand/ggh4x/issues/156

# X-axis interaction
# https://stackoverflow.com/questions/18165863/multirow-axis-labels-with-nested-grouping-variables/18167422#18167422


# ggbreak
# https://stackoverflow.com/questions/78180105/change-the-y-axis-labels-after-using-scale-y-break-in-r
# https://cran.r-project.org/web/packages/ggbreak/vignettes/ggbreak.html

# strip size
# https://stackoverflow.com/questions/41428344/edit-strip-size-ggplot2
