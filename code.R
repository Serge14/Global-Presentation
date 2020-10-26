## Slides for global team

library(data.table)
library(stringi)
library(officer)
library(flextable)
library(magrittr)


extract.category.data = function(df,
                                 selection,
                                 measure,
                                 selection.list,
                                 selection.level,
                                 exclude.malysh) {
  # measure = paste0(measure, "C")
  df = df[eval(parse(text = selection))]
  
  if (exclude.malysh == TRUE) {
    df[Brand == "Malysh Istr", Company := "No Name"]
  }
  
  # df = df[PS3 != "Base Plus"]
  
  df[PS2 %in% c("IF", "FO", "Gum") & PS3 != "Specials", PS3 := PS2]
  df[PS3 == "Specials", PS3 := "TN"]
  df[PS3 == "Fruits" | PS3 == "Savoury Meal", PS3 := "Puree"]
  df[PS3 == "Cereal Biscuits", PS3 := "Biscuits"]
  df[PS3 == "Instant Cereals", PS3 := "Cereals"]
  
  
  if (selection.level == "Category"){
  
    df = df[, .(Volume = sum(VolumeC), Value = sum(ValueC)),
            by = .(PS3, Ynb, Mnb)]
    names(df)[1] = c("Category")
    
    df = dcast.data.table(
      df,
      Category ~ Ynb + Mnb,
      value.var = measure,
      fun.aggregate = sum
    )
      
  } else {
  
    df = df[, .(Volume = sum(VolumeC), Value = sum(ValueC)),
            by = .(PS3, get(selection.level), Ynb, Mnb)]
    names(df)[1:2] = c("Category", selection.level)
    
  df = dcast.data.table(
    df,
    Category + get(selection.level) ~ Ynb + Mnb,
    value.var = measure,
    fun.aggregate = sum
  )
  
  names(df)[2] = selection.level
  
  }
  
  n = dim(df)[2]
  
  df[, L12M := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 11):n]
  df[, L3M := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 2):n]
  df[, LM := rowSums(.SD, na.rm = TRUE), .SDcols = n]
  
  df[, L12MPY := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 23):(n - 12)]
  df[, L3MPY := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 14):(n - 12)]
  df[, LMPY := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 12)]
  
  
  if (selection.level == "Category"){
    
    df[, `:=`(
      L12M.delta.bps = 100*(L12M/L12MPY - 1),
      L3M.delta.bps = 100*(L3M/L3MPY - 1),
      LM.delta.bps = 100*(LM/LMPY - 1)
    )]
    
    df = df[, .(
      Category,
      L12M.delta.bps = sprintf("%+.1f%%", L12M.delta.bps),
      L3M.delta.bps = sprintf("%+.1f%%", L3M.delta.bps),
      LM.delta.bps = sprintf("%+.1f%%", LM.delta.bps)
    )]
    
  } else {
    
    
    # MS
    df[, names(df)[3:length(df)] := lapply(.SD, function(x)
      100 * x / sum(x)),
      .SDcols = 3:length(df), by = Category]
    
    df = df[get(selection.level) %in% selection.list]
    
    df[, `:=`(
      L12M.delta.bps = (L12M - L12MPY) * 100,
      L3M.delta.bps = (L3M - L3MPY) * 100,
      LM.delta.bps = (LM - LMPY) * 100
    )]
    
    df = df[, .(
      Category,
      # L12M.delta.bps = sprintf("%+.0f \nbps", L12M.delta.bps),
      # L3M.delta.bps = sprintf("%+.0f\nbps", L3M.delta.bps),
      # LM.delta.bps = sprintf("%+.0f \nbps", LM.delta.bps)
      L12M.delta.bps = sprintf("%+.0f bps", L12M.delta.bps),
      L3M.delta.bps = sprintf("%+.0f bps", L3M.delta.bps),
      LM.delta.bps = sprintf("%+.0f bps", LM.delta.bps)
    )]
    
  }
  
  row.order = c("IF",
                "FO",
                "Gum",
                "TN",
                "Cereals",
                "Biscuits",
                "Puree")
  
  df[, Category := factor(Category, levels = row.order)]
  df = df[order(Category)]

  return(df)
  
}

extract.topline.data = function(df,
                                 selection,
                                 measure,
                                 selection.list,
                                 selection.level,
                                 exclude.malysh) {
  # measure = paste0(measure, "C")
  df = df[eval(parse(text = selection))]
  
  if (exclude.malysh == TRUE) {
    df[Brand == "Malysh Istr", Company := "No Name"]
  }
  df[PS2 %in% c("IF", "FO", "Gum") & PS3 != "Specials", PS3 := PS2]
  
  if (selection.level == "Category"){
    
    df = df[, .(Volume = sum(VolumeC), Value = sum(ValueC)),
            by = .(PS0, Ynb, Mnb)]
    names(df)[1] = c("Category")
    
    df = dcast.data.table(
      df,
      Category ~ Ynb + Mnb,
      value.var = measure,
      fun.aggregate = sum
    )
    
  } else {
    
    df = df[, .(Volume = sum(VolumeC), Value = sum(ValueC)),
            by = .(PS0, get(selection.level), Ynb, Mnb)]
    names(df)[1:2] = c("Category", selection.level)
    
    df = dcast.data.table(
      df,
      Category + get(selection.level) ~ Ynb + Mnb,
      value.var = measure,
      fun.aggregate = sum
    )
    
    names(df)[2] = selection.level
    
  }
  
  n = dim(df)[2]
  
  df[, L12M := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 11):n]
  df[, L3M := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 2):n]
  df[, LM := rowSums(.SD, na.rm = TRUE), .SDcols = n]
  
  df[, L12MPY := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 23):(n - 12)]
  df[, L3MPY := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 14):(n - 12)]
  df[, LMPY := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 12)]
  
  
  if (selection.level == "Category"){
    
    df[, `:=`(
      L12M.delta.bps = 100*(L12M/L12MPY - 1),
      L3M.delta.bps = 100*(L3M/L3MPY - 1),
      LM.delta.bps = 100*(LM/LMPY - 1)
    )]
    
    
    if (measure == "Value"){
    df = df[, .(
      Category,
      L3M = sprintf("%.1f€ M", L3M/1000000),
      L3M.delta.bps = sprintf("%+.1f%% vs LY", L3M.delta.bps)
    )]
    }
    
    if (measure == "Volume"){
    
    df = df[, .(
      Category,
      L3M = sprintf("%.1fKg M", L3M/1000000),
      L3M.delta.bps = sprintf("%+.1f%% vs LY", L3M.delta.bps)
    )]
    
    }
    
  } else {
    
    
    # MS
    df[, names(df)[3:length(df)] := lapply(.SD, function(x)
      100 * x / sum(x)),
      .SDcols = 3:length(df), by = Category]
    
    df = df[get(selection.level) %in% selection.list]
    
    df[, `:=`(
      L12M.delta.bps = (L12M - L12MPY) * 100,
      L3M.delta.bps = (L3M - L3MPY) * 100,
      LM.delta.bps = (LM - LMPY) * 100
    )]
    
    df = df[, .(
      Category,
      L3M = sprintf("%.1f%%", L3M),
      L3M.delta.bps = sprintf("%+.0f bps vs LY", L3M.delta.bps)
    )]
    
  }
  
  row.order = c("IF",
                "FO",
                "Gum",
                "Specials",
                "Instant Cereals",
                "Cereal Bisquits",
                "Puree")
  
  return(df)
  
}

make.flextable = function(df) {
  
  for (j in seq_len(ncol(df))){set(df, which(is.na(df[[j]])), j, "")}
  
  # Rename
  n = (length(names(df)) - 1)/3
  
  column.names = unique(names(df))[-1]
  column.keys = c(column.names, "col")
  
  column.names = as.vector(t(unlist(sapply(column.names, function(x)
    paste0(x, seq_len(n))))))
  column.keys = as.vector(t(unlist(sapply(column.keys, function(x)
    paste0(x, seq_len(n))))))
  
  column.names = c("Category", column.names)
  column.keys = c("Category", "col", column.keys)
  
  names(df) = column.names
  
  ft = flextable(df, col_keys = column.keys)

  label.names = as.list(c("", "col", rep(c("MAT", "L3M", "LM", "col"), n)))
  names(label.names) = column.keys
  
  # Assign labels to the header
  ft <- set_header_labels(ft, values = label.names)
  
    # Theme
  ft = theme_box(ft)
  ft = border(ft,
    i = 1,
    j = 1,
    border.top = fp_border(color = "white"),
    border.left = fp_border(color = "white"),
    border.right = fp_border(color = "white"),
    part = "header"
  )
  
  ft = border(ft,
              i = 1,
              j = 2,
              border.left = fp_border(color = "white"),
              part = "header"
  )
  
  ft = border(ft,
              # i = 1,
              j = length(column.keys),
              border.right = fp_border(color = "white"),
              part = "all"
  )
  
  ft = empty_blanks(ft)
  
  # Color and font size of the header
  # ft <-  bg(ft, bg = "#D3D3D3", part = "header")
  # ft = fontsize(ft, size = 11, part = "header")
  
  # Font
  ft = font(ft, fontname = "Calibri")
  
  ft = color(ft, j = column.names[-1] , part = "body", color = "white")
  ft = color(ft, j = column.names[-1] , part = "header", color = "#808080")
  ft = color(ft, j = column.names[1] , part = "body", color = "#808080")
  
  ft = bold(ft, j = column.names , part = "body")
  
  # Alignment
  ft =  align(ft,
              j = column.names[-1],
              align = "center",
              part = "all"
  )
  


  # 00b050
  ft = bg(ft, j = column.names[-1], bg = "grey")
  
  for (k in column.names[-1]) {
    ft = bg(ft, i = df[, stri_extract_all_regex(get(k), "-?[0-9]+.?[0-9]*") < 0], 
            j = k, 
            bg = "red")}
  
  for (k in column.names[-1]) {
    ft = bg(ft, i = df[, stri_extract_all_regex(get(k), "-?[0-9]+.?[0-9]*") > 0], 
            j = k, 
            bg = "#00b050")}
  
  for (k in column.names[-1]) {
    ft = bg(ft, i = df[, stri_extract_all_regex(get(k), "-?[0-9]+.?[0-9]*") == 0], 
            j = k, 
            bg = "#ffc000")}
  
  # Font size
  ft = fontsize(ft, size = 11, part = "all")
  
  # Width
  ft <- width(ft, j = 1, width = 0.82)
  ft <- width(ft, j = seq(2, 5), width = 0.8)
  ft <- width(ft, j = seq(7, length(column.keys)), width = 0.63)
  ft <- width(ft, j = seq(2, length(column.keys), by = 4), width = 0.14)
  
# Height
  ft <- height_all(ft, height = 0.55)
  
  return(ft)
  
}


get.table = function(){
  
  for (i in 1:length(selection.list)){
    
    df.temp  = extract.category.data(df, 
                                     selection, 
                                     measure, 
                                     selection.list[i], 
                                     selection.level[i], 
                                     FALSE)
    
    df.temp = df.pattern[df.temp, 
                         on = "Category",
                         `:=`(L12M.delta.bps = i.L12M.delta.bps,
                              L3M.delta.bps = i.L3M.delta.bps,
                              LM.delta.bps = i.LM.delta.bps)]
    
    df.table = cbind(df.table, df.temp[, .(L12M.delta.bps,
                                           L3M.delta.bps,
                                           LM.delta.bps)])
    
    
    df.pattern = df.pattern[, .(Category)]
    
  }
  
  return(df.table)
  
}



get.topline = function(){

df.list = vector(mode = "list", length = length(selection.list))

for (i in 1:length(selection.list)){
  
  df.temp  = extract.topline.data(df, 
                                   selection, 
                                   measure, 
                                   selection.list[i], 
                                   selection.level[i], 
                                   FALSE)
  
  df.list[[i]] = df.temp
  
}

return(df.list)

}

# Dictionaries
dict.months = data.table(Mnb = as.character(1:12),
                         month.name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Dataset
df = fread("/home/serhii/Documents/Work/Nutricia/Data/202008/df.csv")
Month = 8
Year = 2020
exchage.rate = 27.43

df[, ValueC := ValueC/exchage.rate]


reporting.period = paste("L3M", 
                         dict.months[Mnb == Month, toupper(month.name)],
                         Year)

selection = 'Form != "Liquid" & PS0 == "IMF"'
measure = "Value"

selection.list = c("IMF", "Nutricia", "Nutrilon", "Milupa", "Nestle")
selection.level = c("Category", "Company", "Brand", "Brand", "Company")


df.pattern = data.table(Category = c("IF", "FO", "Gum", "TN"))
df.table = data.table(Category = c("IF", "FO", "Gum", "TN"))


ppt <- read_pptx("/home/serhii/Documents/Work/Nutricia/Scripts/Global-Presentation/Global pattern.pptx")


for (i in seq_len(4)) {
  
  print(i)
  
  if (i == 1) {
    selection = 'Form != "Liquid" & PS0 == "IMF"'
    measure = "Value"
    exchage.rate = 1
    selection.list = c("IMF", "Nutricia", "Nutrilon", "Milupa", "Nestle")
    selection.level = c("Category", "Company", "Brand", "Brand", "Company")
    df.pattern = data.table(Category = c("IF", "FO", "Gum", "TN"))
    df.table = data.table(Category = c("IF", "FO", "Gum", "TN"))
    slide.title = "Value Market Share - Milks"
  }
  
  if (i == 2) {
    selection = 'Form != "Liquid" & PS0 == "IMF"'
    measure = "Volume"
    exchage.rate = 1
    selection.list = c("IMF", "Nutricia", "Nutrilon", "Milupa", "Nestle")
    selection.level = c("Category", "Company", "Brand", "Brand", "Company")
    df.pattern = data.table(Category = c("IF", "FO", "Gum", "TN"))
    df.table = data.table(Category = c("IF", "FO", "Gum", "TN"))
    slide.title = "Volume Market Share - Milks"
  }
  
  if (i == 3) {
    selection = 'PS3 == "Fruits" | PS3 == "Savoury Meal" |
    PS3 == "Instant Cereals" | PS3 == "Cereal Biscuits"'
    measure = "Value"
    exchage.rate = 1
    selection.list = c("IMF", "Nutricia", "Nutrilon", "Milupa", "Nestle")
    selection.level = c("Category", "Company", "Brand", "Brand", "Company")
    df.pattern = data.table(Category = c("Cereals", "Biscuits", "Puree"))
    df.table = data.table(Category = c("Cereals", "Biscuits", "Puree"))
    slide.title = "Value Market Share - Foods"
  }
  
  if (i == 4) {
    sselection = 'PS3 == "Fruits" | PS3 == "Savoury Meal" |
    PS3 == "Instant Cereals" | PS3 == "Cereal Biscuits"'
    measure = "Volume"
    exchage.rate = 1
    df.pattern = data.table(Category = c("Cereals", "Biscuits", "Puree"))
    df.table = data.table(Category = c("Cereals", "Biscuits", "Puree"))
    slide.title = "Volume Market Share - Foods"
  }
  
  ppt %>%
    on_slide(index = i) %>%
    ph_with(value = slide.title, location = ph_location_type(type = "title")) %>% 
    ph_with(value = make.flextable(get.table()),
            location = ph_location_type("body", id = 6)) %>%
    ph_add_text(
      reporting.period,
      type = "body",
      id = 8,
      style = fp_text(color = "#808080", font.size = 17, font.family = "Calibri")
    ) %>%
    ph_add_text(
      get.topline()[[1]][, L3M],
      type = "body",
      id = 9,
      style = fp_text(color = "#808080", font.size = 30, font.family = "Calibri")
    ) %>%
    ph_add_text(
      get.topline()[[1]][, L3M.delta.bps],
      type = "body",
      id = 10,
      style = fp_text(
        color = ifelse(
          stri_extract_all_regex(get.topline()[[1]][, L3M.delta.bps], "-?[0-9]+.?[0-9]*") < 0,
          "red",
          "#00b050"
        ),
        font.size = 17, font.family = "Calibri"
      )
    ) %>%
    
    ph_add_text(
      reporting.period,
      type = "body",
      id = 2,
      style = fp_text(color = "#808080", font.size = 17, font.family = "Calibri")
    ) %>%
    ph_add_text(
      get.topline()[[2]][, L3M],
      type = "body",
      id = 3,
      style = fp_text(color = "#808080", font.size = 30, font.family = "Calibri")
    ) %>%
    ph_add_text(
      get.topline()[[2]][, L3M.delta.bps],
      type = "body",
      id = 4,
      style = fp_text(
        color = ifelse(
          stri_extract_all_regex(get.topline()[[2]][, L3M.delta.bps], "-?[0-9]+.?[0-9]*") < 0,
          "red",
          "#00b050"
        ),
        font.size = 17, font.family = "Calibri"
      )
    ) %>%
    
    ph_add_text(
      reporting.period,
      type = "body",
      id = 5,
      style = fp_text(color = "#808080", font.size = 17, font.family = "Calibri")
    ) %>%
    ph_add_text(
      get.topline()[[3]][, L3M],
      type = "body",
      id = 6,
      style = fp_text(color = "#808080", font.size = 30, font.family = "Calibri")
    ) %>%
    ph_add_text(
      get.topline()[[3]][, L3M.delta.bps],
      type = "body",
      id = 7,
      style = fp_text(
        color = ifelse(
          stri_extract_all_regex(get.topline()[[3]][, L3M.delta.bps], "-?[0-9]+.?[0-9]*") < 0,
          "red",
          "#00b050"
        ),
        font.size = 17, font.family = "Calibri"
      )
    ) %>%
    
    ph_add_text(
      reporting.period,
      type = "body",
      id = 11,
      style = fp_text(color = "#808080", font.size = 17, font.family = "Calibri")
    ) %>%
    ph_add_text(
      get.topline()[[4]][, L3M],
      type = "body",
      id = 12,
      style = fp_text(color = "#808080", font.size = 30, font.family = "Calibri")
    ) %>%
    ph_add_text(
      get.topline()[[4]][, L3M.delta.bps],
      type = "body",
      id = 13,
      style = fp_text(
        color = ifelse(
          stri_extract_all_regex(get.topline()[[4]][, L3M.delta.bps], "-?[0-9]+.?[0-9]*") < 0,
          "red",
          "#00b050"
        ),
        font.size = 17, font.family = "Calibri"
      )
    ) %>%
    
    ph_add_text(
      reporting.period,
      type = "body",
      id = 14,
      style = fp_text(color = "#808080", font.size = 17, font.family = "Calibri")
    ) %>%
    ph_add_text(
      get.topline()[[5]][, L3M],
      type = "body",
      id = 15,
      style = fp_text(color = "#808080", font.size = 30, font.family = "Calibri")
    ) %>%
    ph_add_text(
      get.topline()[[5]][, L3M.delta.bps],
      type = "body",
      id = 16,
      style = fp_text(
        color = ifelse(
          stri_extract_all_regex(get.topline()[[5]][, L3M.delta.bps], "-?[0-9]+.?[0-9]*") < 0,
          "red",
          "#00b050"
        ),
        font.size = 17, font.family = "Calibri"
      )
    )
  
}

print(ppt, target="/home/serhii/Documents/Work/Nutricia/Scripts/Global-Presentation/Global dashboard.pptx")