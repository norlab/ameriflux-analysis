sites |> 
  separate_longer_delim(`Years of AmeriFlux FLUXNET Data`, delim=", ") |>
  group_by(`Site ID`) %>%
  mutate(Diff_FLUXNET = as.numeric(`Years of AmeriFlux FLUXNET Data`) - lag(as.numeric(`Years of AmeriFlux FLUXNET Data`))) |> 
  View()