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
      filter(year < 2020)
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
    name  = openai_completions,
    command = submit_openai(prompts),
    pattern = map(prompts)
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
    name = correlations,
    command = completions_tibble |> 
      left_join(claude_tibble) |>
      left_join(fh_data) |> 
      left_join(democracyData::vdem_simple) |> 
      select(score, claude_score, fh_total_reversed, v2x_polyarchy) |> 
      corrr::correlate()
  )

)
