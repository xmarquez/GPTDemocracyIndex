create_prompt <- function(fh_data) {
  prompts <- purrr::map2(fh_data$fh_country, fh_data$year,
                         ~list(
                           list(
                             "role" = "system",
                             "content" = stringr::str_c("You are a political scientist with deep area knowledge of ", .x, ". ")
                           ),
                           list(
                             "role" = "user",
                             "content" = stringr::str_c("You are investigating the extent to which the ideal of electoral democracy in ",
                                                        .x, " was achieved in ", .y,
                                                        ". \n\nThe electoral principle of democracy seeks to embody the core value of making ",
                                                        "rulers responsive to citizens, achieved through electoral competition for the ",
                                                        "electorate’s approval under circumstances when suffrage is extensive; political ",
                                                        "and civil society organizations can operate freely; elections are clean and not ",
                                                        "marred by fraud or systematic irregularities; and elections affect the composition ",
                                                        "of the chief executive of the country. In between elections, there is freedom of ",
                                                        "expression and an independent media capable of presenting alternative views on ",
                                                        "matters of political relevance. \n\nTo what extent was the ideal of electoral democracy ",
                                                        "in its fullest sense achieved in ", .x, " in ", .y, "? \n\nUse only knowledge relevant to ",
                                                        .x, " in ", .y, " and answer in a scale of 0 to 10, with 0 representing \"not at all\" ",
                                                        " and 10 representing \"completely\", and add a measure of your confidence from 0 to 1, ",
                                                        "with 0 representing  \"not confident at all\", and 1 representing \"very confident\". ",
                                                        "Use this template to answer: \n", .x," in ", .y,
                                                        ": number from 0-10, confidence: number from 0-1")
                             )
                           )
                         )
}

submit_openai <- function(prompt, temperature = 0.2, n = 1) {
  res <- openai::create_chat_completion(model = "gpt-3.5-turbo",
                                        messages = prompt,
                                        temperature = temperature,
                                        n = n)
  Sys.sleep(7)
  res
}

format_results <- function(openai_completions) {
  completions <- openai_completions |> unlist() 
  completions <- completions[which(str_detect(names(completions), "message.content"))]
  
  scores <- str_extract(completions, "(?<=: )[0-9]+(\\.[0-9])?") |>
    as.numeric()
  
  confidence <- str_extract(completions, "(?<=fidence: )[0-9]+(\\.[0-9])?") |>
    as.numeric()
  
  completions |> 
    str_split(pattern = "\n\n", n = 2) |> 
    map_dfr(as_tibble_row, .name_repair = ~c("rating", "justification")) |>
    separate(rating, into = c("fh_country", "year"), sep = " in ") |>
    mutate(year = str_extract(year, "[0-9]{4}") |> as.numeric()) |>
    mutate(score = scores, confidence = confidence) |>
    relocate(justification, .after = everything())
  
}

create_prompt_claude <- function(fh_data) {
  prompts <- purrr::map2(fh_data$fh_country, fh_data$year,
                         ~stringr::str_c("Human: You are a political scientist with deep area knowledge of ", .x, ". ",
                                        "You are investigating the extent to which the ideal of electoral democracy in ",
                                        .x, " was achieved in ", .y,
                                        ". The electoral principle of democracy seeks to embody the core value of making ",
                                        "rulers responsive to citizens, achieved through electoral competition for the ",
                                        "electorate’s approval under circumstances when suffrage is extensive; political ",
                                        "and civil society organizations can operate freely; elections are clean and not ",
                                        "marred by fraud or systematic irregularities; and elections affect the composition ",
                                        "of the chief executive of the country. In between elections, there is freedom of ",
                                        "expression and an independent media capable of presenting alternative views on ",
                                        "matters of political relevance. To what extent was the ideal of electoral democracy ",
                                        "in its fullest sense achieved in ", .x, " in ", .y, "? Use only knowledge relevant to ",
                                        .x, " in ", .y, " and answer in a scale of 0 to 10, with 0 representing \"not at all\" ",
                                        " and 10 representing \"completely\", and add a measure of your confidence from 0 to 1, ",
                                        "with 0 representing  \"not confident at all\", and 1 representing \"very confident\". ",
                                        "Use this template to answer: \n", .x," in ", .y, ": {number from 0-10}, confidence: {number from 0-1},",
                                        " justification: {brief justification for your response}",
                                        "\n\nAssistant: ")

                         )
}

submit_claude <- function(prompt, temperature = 0.2) {
  body <- jsonlite::toJSON(
    list(
      prompt = prompt,
      model = "claude-v1",
      max_tokens_to_sample = 300,
      temperature = temperature
      ),
    auto_unbox = TRUE,
    pretty = TRUE
    )
  
  response <- POST(
    url = "https://api.anthropic.com/v1/complete",
    add_headers("x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"), "content-type" = "application/json"),
    body = body,
    encode = "json"
  )
  httr::stop_for_status(response)
  res <- httr::content(response)
  res
}

format_results_claude <- function(claude_completions) {
  completions <- claude_completions[which(str_detect(names(claude_completions), "_completion$"))]
  
  completions <- completions[!(str_detect(unlist(completions), "I apologize"))]
  completions[!str_detect(completions, " Here")] <- str_c(" Here is my attempt at an answer:\n", completions[!str_detect(completions, " Here")])
  scores <- str_extract(completions, "(?<=: )[0-9]+(\\.[0-9])?") |>
    as.numeric()
  
  confidence <- str_extract(completions, "(?<=fidence: )[0-9]+(\\.[0-9])?") |>
    as.numeric()
  
  completions |> 
    str_split(pattern = "\n", n = 2) |> 
    map_dfr(as_tibble_row, .name_repair = ~c("preamble", "justification")) |>
    select(-preamble) |>
    separate(justification, into = c("country_year", "claude_justification"), sep = ":", extra = "merge") |>
    separate(country_year, into = c("fh_country", "year"), sep = " in ") |>
    mutate(claude_score = scores, claude_confidence = confidence, year = as.numeric(year)) |>
    relocate(claude_justification, .after = everything())
  
}
