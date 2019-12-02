Webscraping data on Soviet secret police leaders
================
Martin Kosík
02 prosinec, 2019

``` r
knitr::opts_chunk$set(echo = TRUE)
library(rvest)
```

    ## Loading required package: xml2

``` r
library(magrittr)
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## <U+221A> ggplot2 3.2.1     <U+221A> purrr   0.3.3
    ## <U+221A> tibble  2.1.3     <U+221A> dplyr   0.8.3
    ## <U+221A> tidyr   1.0.0     <U+221A> stringr 1.4.0
    ## <U+221A> readr   1.3.1     <U+221A> forcats 0.4.0

    ## -- Conflicts ---------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x tidyr::extract()        masks magrittr::extract()
    ## x dplyr::filter()         masks stats::filter()
    ## x readr::guess_encoding() masks rvest::guess_encoding()
    ## x dplyr::lag()            masks stats::lag()
    ## x purrr::pluck()          masks rvest::pluck()
    ## x purrr::set_names()      masks magrittr::set_names()

``` r
library(stringr)
library(stringi)
Sys.setlocale("LC_CTYPE", "russian") 
```

    ## [1] "Russian_Russia.1251"

Download the data and convert them to text

``` r
memo_site <- "http://old.memo.ru/history/nkvd/kto2/"


iframe <- html_session("http://old.memo.ru/history/nkvd/kto2/")%>% 
  html_nodes(xpath = "//html//frameset//frame") %>% 
  magrittr::extract2(3) %>% 
  html_attr("src") 

number_of_leaders = 1308

all_iframes <- str_c("kto_2-", str_pad(1:number_of_leaders, 4, pad = "0"), ".html")

get_htmls <- function(iframe, memo_site = "http://old.memo.ru/history/nkvd/kto2/") {
  html_session(str_c(memo_site, iframe)) %>%
    html_nodes("body > div > p")
}

all_htmls <- all_iframes %>% 
  map(get_htmls)

all_texts <- all_htmls %>% 
  map(html_text)
```

Extract information from the text

``` r
nkvd_leaders <- tibble(.rows = number_of_leaders)
  
nkvd_leaders$full_name <- all_texts %>% 
  map_chr(1)


nkvd_leaders$birth_year <- all_texts %>% 
  map_chr(2) %>% 
  map(str_extract_all, pattern = "[:digit:]{4}") %>% 
  map(1) %>% 
  map(1) %>% 
  map(as.integer)

nkvd_leaders$death_year <- all_texts %>% 
  map_chr(2) %>% 
  map(str_extract_all, pattern = "[:digit:]{4}") %>% 
  map(1) %>% 
  map(2) %>% 
  map(as.integer)


#all_texts %>% 
#  map_chr(2) %>% 
#  gsub("^\\(.+, (.+) - .* \\)\\.$", "\\1", .)
  


communist_party_entrance_date <- all_texts %>% 
  map_chr(3) %>% 
  gsub("^.*(В|Член) КП ?с?( |\\.)(\\d{2}\\.\\d{2})\\.?.*$", "\\3", .) %>% 
  str_split(pattern = "\\.", n = 2)

nkvd_leaders$party_entrance_year <- communist_party_entrance_date %>% 
  map(2) %>% 
  map(~ str_c("19", .)) %>% 
  map(as.numeric)
```

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

``` r
nkvd_leaders$party_entrance_month <- communist_party_entrance_date %>% 
  map(1) %>% 
  map(as.numeric)
```

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

    ## Warning in .Primitive("as.double")(x, ...): NAs introduced by coercion

``` r
nkvd_leaders$ethnicity <-all_texts %>% 
  map_chr(3) %>% 
  gsub("^Родился в семье (.+?)\\. (.+?)\\. .*$", "\\2", ., perl = TRUE) # %>%  map(as.factor)

nkvd_leaders %>%
  count(ethnicity, sort = T) 
```

    ## # A tibble: 124 x 2
    ##    ethnicity         n
    ##    <chr>         <int>
    ##  1 Русский         798
    ##  2 Украинец        142
    ##  3 Еврей            47
    ##  4 Белорус          40
    ##  5 Грузин           34
    ##  6 Армянин          34
    ##  7 Узбек            19
    ##  8 Казах            17
    ##  9 Татарин          15
    ## 10 Азербайджанец    11
    ## # ... with 114 more rows

``` r
#all_texts %>% 
#  map_chr(3) %>% 
#  sub("^Родился в семье (.+?)\\. ", "\\2", ., perl = TRUE) 


#all_texts %>% 
#  map_chr(3) %>% 
#  sub("^Родился в семье ((.+?)(?=\\. ))", "\\1", ., perl = TRUE)
```

PDF file reading

``` r
#library(pdftools)
#library(tabulizer)
#library(here)
library(tidyverse)
```

``` r
download.file("http://old.memo.ru/uploads/files/845.pdf", "soviet_secret_police_1941_to_1954.pdf", mode = "wb")

txt <- pdf_text("soviet_secret_police_1941_to_1954.pdf")

fonts <- pdf_fonts("soviet_secret_police_1941_to_1954.pdf")


out1 <- extract_tables("soviet_secret_police_1941_to_1954.pdf", pages = 80, guess = FALSE, output = "data.frame",
                       encoding = "UTF-8")
str(out1)


table_data <- extract_areas("soviet_secret_police_1941_to_1954.pdf", pages = 80, output = "data.frame",
              encoding = "UTF-8")


text = txt[[80]] %>% 
  str_sub(start = 88+1)

src <- ""
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


result <- ''
QTD_COLUMNS = 2
lstops <- gregexpr(pattern =" ", txt[[80]])
stops <- as.integer(names(sort(table(unlist(lstops)),decreasing=TRUE)[1:2]))


for(i in seq(1, QTD_COLUMNS, by=1))
  {
    temp_result <- sapply(text, function(x){
      start <- 1
      stop <-stops[i] 
      if(i > 1)            
        start <- stops[i-1] + 1
      if(i == QTD_COLUMNS)#last column, read until end.
        stop <- nchar(x)+1
      substr(x, start=start, stop=stop)
    }, USE.NAMES=FALSE)
    temp_result <- trim(temp_result)
    result <- append(result, temp_result)
  }
  result



txt[[80]] %>% 
  str_remove("\\A([^\\r\\n]*\\n)") %>% 
 # str_remove(pattern = "                             Руководители органов НКГБ–НКВД–МГБ–МВД СССР 1941–1954\r\n80\r\n") %>% 
  cat()

  
txt[[80]] %>% 
  str_sub(
    start = nchar("                             Руководители органов НКГБ–НКВД–МГБ–МВД СССР 1941–1954\r\n80\r\n") + 1) %>% 
  #str_trim() %>% 
  cat()
  
out1 <- extract_tables(txt[[80]])


nchar("                             Руководители органов НКГБ–НКВД–МГБ–МВД СССР 1941–1954\r\n80\r\n")
```
