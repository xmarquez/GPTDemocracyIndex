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

create_prompt_pacl <- function(fh_data) {
  prompts <- purrr::map2(fh_data$fh_country, fh_data$year,
                         ~list(
                           list(
                             "role" = "system",
                             "content" = stringr::str_c("You are a political scientist with deep area knowledge of ", .x, ". ")
                           ),
                           list(
                             "role" = "user",
                             "content" = stringr::str_c("A democracy is a system where 1) the top executive is elected (whether directly ",
                                                        "by popular vote or indirectly by popularly elected legislators); 2) ",
                                                        " there is a legislature with members directly or indirectly elected; 3) ", 
                                                        "There are no major restrictions on popular suffrage; 4) Multiple parties are legally ",
                                                        "allowed and actually compete as independent entities ",
                                                        "(not just as members of a Popular Front controlled by a single party); ",
                                                        "5) Multiple parties are actually represented in the legislature; 6) the current ",
                                                        "incumbent has not unconstitutionally closed the legislature and rewritten the rules ",
                                                        "in their favor; and 7) there has been or it is obvious that there could be an ",
                                                        "alternation of power through elections, where the current executive would lose power ",
                                                        "and an opposition party would take over. \n\nBased on this definition and your knowlege ",
                                                        "of the politics of ", .x, " in ", .y, ", was ", .x, " a democracy in ", .y, 
                                                        "?  \n\nUse only knowledge relevant to ", .x, " in ", .y, 
                                                        " and answer simply yes or no. Add a measure of your ",
                                                        "confidence from 0 to 1, with 0 representing  \"not confident at all\", and 1 representing \"very confident\". ",
                                                        "Use this template to answer: \n", .x," in ", .y,
                                                        ": yes or no, confidence: number from 0-1, justification: brief justification ",
                                                        "referring back to the 7 conditions above.")
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

format_results_pacl <- function(openai_completions_pacl) {
  completions <- openai_completions_pacl |> unlist() 
  completions <- completions[which(str_detect(names(completions), "message.content"))]
  
  scores <- str_extract(completions, "(?<=: )(Yes|No)") 
  scores <- ifelse(scores == "No", 0, 1)
  
  confidence <- str_extract(completions, "(?<=fidence: )[0-9]+(\\.[0-9])?") |>
    as.numeric()
  
  completions |> 
    map_dfr(as_tibble_row, .name_repair = ~c("justification")) |>
    separate(justification, into = c("fh_country", "year", "justification"), 
             sep = " in |:", extra = "merge") |>
    mutate(year = str_extract(year, "[0-9]{4}") |> as.numeric()) |>
    mutate(score = scores, confidence = confidence, 
           justification = str_trim(justification)) |>
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

create_prompt_claude_pacl <- function(fh_data) {
  prompts <- purrr::map2(fh_data$fh_country, fh_data$year,
                         ~stringr::str_c("Human: A democracy is a system where 1) the top executive is elected (whether directly ",
                                         "by popular vote or indirectly by popularly elected legislators); 2) ",
                                         " there is a legislature with members directly or indirectly elected; 3) ",
                                         "There are no major restrictions on popular suffrage; 4) Multiple parties are legally ",
                                         "allowed and actually compete as independent entities ",
                                         "(not just as members of a Popular Front controlled by a single party); ",
                                         "5) Multiple parties are actually represented in the legislature; 6) the current ",
                                         "incumbent has not unconstitutionally closed the legislature and rewritten the rules ",
                                         "in their favor; and 7) there has been or it is obvious that there could be an ",
                                         "alternation of power through elections, where the current executive would lose power ",
                                         "and an opposition party would take over. \n\nBased on this definition and your knowlege ",
                                         "of the politics of ", .x, " in ", .y, ", was ", .x, " a democracy in ", .y,
                                         "?  \n\nUse only knowledge relevant to ", .x, " in ", .y,
                                         " and answer simply yes or no. Add a measure of your ",
                                         "confidence from 0 to 1, with 0 representing  \"not confident at all\", and ",
                                         "1 representing \"very confident\". ",
                                         "Use this template to answer: \n", .x," in ", .y, ": {yes or no}, confidence: ", 
                                         "number from {0-1}, justification: {brief justification referring back to the 7 conditions above}.",
                                         "\n\nAssistant: ")
                         
  )
  prompts
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
    separate(justification, into = c("country_year", "justification"), sep = ":", extra = "merge") |>
    separate(country_year, into = c("fh_country", "year"), sep = " in ") |>
    mutate(score = scores, confidence = confidence, year = as.numeric(year)) |>
    relocate(justification, .after = everything())
  
}

