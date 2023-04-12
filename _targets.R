# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tidyverse", "democracyData", "httr"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
future::plan(future.callr::callr)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(
    name = fh_data,
    command = democracyData::download_fh() |>
      filter(year < 2022)
#   format = "feather" # efficient storage of large data frames # nolint
  ),
  tar_target(
    name = sample_fh,
    command = fh_data |>
      filter(year %% 5 == 0)
  ),
  tar_target(
    name = prompts,
    command = create_prompt(sample_fh),
    iteration = "list"
  ),
  tar_target(
    name = pacl_prompts,
    command = create_prompt_pacl(sample_fh),
    iteration = "list"),
  tar_target(
    name  = openai_completions,
    command = submit_openai(prompts),
    pattern = map(prompts)
  ),
  tar_target(
    name  = openai_completions_pacl,
    command = submit_openai(pacl_prompts),
    pattern = map(pacl_prompts)
  ),
  tar_target(
    name  = completions_tibble_pacl,
    command = format_results_pacl(openai_completions_pacl)
  ),
  tar_target(
    name = completions_tibble,
    command = format_results(openai_completions)
  ),
  tar_target(
    name = claude_prompts,
    command = create_prompt_claude(sample_fh),
    iteration = "list"
  ),
  tar_target(
    name = claude_prompts_pacl,
    command = create_prompt_claude_pacl(fh_data),
    iteration = "list"
  ),
  tar_target(
    name = claude_completions_pacl,
    command = submit_claude(claude_prompts_pacl),
    pattern = map(claude_prompts_pacl)
  ),
  tar_target(
    name = claude_completions,
    command = submit_claude(claude_prompts),
    pattern = map(claude_prompts)
  ),
  tar_target(
    name = claude_prompts2,
    command = create_prompt_claude(fh_data),
    iteration = "list"
  ),
  tar_target(
    name = claude_completions2,
    command = submit_claude(claude_prompts2),
    pattern = map(claude_prompts2)
  ),
  tar_target(
    name = claude_tibble,
    command = format_results_claude(claude_completions)
  ),
  tar_target(
    name = claude_tibble2,
    command = format_results_claude(claude_completions2)
  ),
  tar_target(
    name = correlations_all,
    command = all_dem |> 
      pivot_wider(id_cols = c(extended_country_name:year), 
                  values_from = value_rescaled, 
                  names_from = measure,
                  names_sort = TRUE) |> 
      select(-extended_country_name:-year) |> 
      corrr::correlate()
  ),
  tar_target(
    name = correlations_ordinal,
    command = all_dem |> 
      filter(index_type == "ordinal") |>
      pivot_wider(id_cols = extended_country_name:year, 
                  values_from = value_rescaled, 
                  names_from = measure,
                  names_sort = TRUE) |> 
      select(-extended_country_name:-year) |> 
      corrr::correlate()
  ),
  tar_target(
    name = all_dem,
    command = generate_democracy_scores_dataset(verbose = FALSE) |>
      filter(extended_country_name %in% combined_ai_scores$extended_country_name, 
             year %in% combined_ai_scores$year) |>
      bind_rows(combined_ai_scores) |>
      group_by(measure) |>
      mutate(value_rescaled = scales::rescale(value, to = c(0,1))) %>%
      group_by(measure, extended_country_name) |>
      mutate(coverage = n()) |>
      ungroup() 
  ),
  tar_target(
    name = combined_ai_scores,
    command = completions_tibble |>
      mutate(dataset = "openai",
             index_type = "ordinal",
             measure = "openai_score") |>
      rename(value = score) |>
      bind_rows(claude_tibble |>
                  mutate(dataset = "anthropic small sample",
                         index_type = "ordinal",
                         measure = "claude_score") |>
                  rename(value = score)) |>
      bind_rows(claude_tibble2 |>
                  mutate(dataset = "anthropic larger sample",
                         index_type = "ordinal",
                         measure = "claude_score_2") |>
                  rename(value = score)) |>
      country_year_coder(fh_country, year, 
                         include_in_output = c(
                           "extended_country_name", "GWn", "cown", "in_GW_system"
                           ),
                         verbose = FALSE) |>
      select(extended_country_name, GWn, cown, in_GW_system, 
             year, measure, value, confidence, index_type, dataset, justification)
  )
)
