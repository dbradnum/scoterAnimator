library(googlesheets)

key = extract_key_from_url("https://docs.google.com/spreadsheets/d/13HxwrtlBLOKFKs_1R8TGZPzBDUWQ8Ew2B9NmXgtqwM4/edit#gid=0")

# This fails...
gs_key(key)


# See https://github.com/jennybc/googlesheets/issues/272 for more on making this work