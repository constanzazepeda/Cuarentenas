

may_cap_letter<- function(data, col){
  
  # vector<- data %>%  pull({{col}}) 
  # 
  # output<- data %>%  
  #   mutate(col = stri_trans_general(str = vector , id = "Latin-ASCII"), 
  #          col = toupper({{col}}))
  
  
  output<- data %>%
    mutate(across(.cols = {{col}},
                  .fns = ~ stringi::stri_trans_general(., id = "Latin-ASCII") %>% 
                    toupper()))
  
  return(output)

  }

rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}


cod_com_character<- function(data, col){
  output<- data %>%
    mutate(cod_com = ifelse(str_length({{col}})==4, 
                          paste0(0,{{col}}), 
                          {{col}}))
  
  return(output)

}


make_panel_zc<- function(input , fecha_inicio, fecha_fin, freq){
  dates <- seq(ymd(fecha_inicio), ymd(fecha_fin), "day")
  date_seq<- floor_date(dates, unit = freq, week_start = 1) %>%
    unique()
  
  new_cols<- colnames(input %>% select(-gcod))
  
  output<- input %>% 
    expand_grid(date_seq) %>%  
    group_by(gcod, date_seq) %>% 
    summarise(across(all_of(new_cols), ~sum(.x)), .groups = "keep") %>% 
    ungroup()
  
  return(output)
}



