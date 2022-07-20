#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

chars = c(c(0:9), letters, LETTERS, c(',', ';', ':', '-', '?', '!', '%', '*', '&', '(', ')', '=', '+', '@'))

create.keys = function(chars) {
  key = c(c(0:9), letters, LETTERS, c(',', ';', ':', '-', '?', '!', '%', '*', '&', '(', ')', '=', '+', '@'))
  r = runif(length(key))
  df = data.frame(key, r)
  df = df[order(df$r), ]
  df$yek = key
  df = within(df, rm(r))
  key = df$key
  yek = df$yek
  return(list(key, yek))
}

# lock with the key, unlock with the yek
lock0 = function(text, key, yek) {
  words = unlist(strsplit(text, ' '))
  i = 0
  while (i < length(words)) {
    i = i + 1
    word = words[i]
    word = unlist(strsplit(word, ''))
    j = 0
    while (j < length(word)) {
      j = j + 1
      if (sum(word[j] == yek)) {
        word[j] = key[word[j] == yek]  
      } else {
        word[j] = '•'  
      }
    }
    p = length(words) %% i
    if (p) {
      if (p < length(word)) {
        word = c(word[1:p], sample(key, 1), word[(p+1):length(word)])
      } else {
        word = c(word[1:length(word)], sample(key, 1))
      }
    }
    words[i] = paste0(word, collapse = '')
  }
  text = paste0(words, collapse = ' ')
  return(text)
}

unlock0 = function(text, key, yek) {
  words = unlist(strsplit(text, ' '))
  i = 0
  while (i < length(words)) {
    i = i + 1
    word = unlist(strsplit(words[i], ''))
    p = length(words) %% i
    if (p) {
      if (p < length(word)-1) {
        word = c(word[1:p], word[(p+2):length(word)])
      } else {
        word = word[1:length(word)-1]
      }
    }
    j = 0
    while (j < length(word)) {
      j = j + 1
      if (sum(word[j] == yek)) {
        word[j] = yek[word[j] == key]
      } else {
        word[j] = '•'  
      }
    }
    words[i] = paste0(word, collapse = '')
  }
  text = paste0(words, collapse = ' ')
  return(text)
}

parse = function(text) trimws(unlist(strsplit(text, '[.]')))

lock = function(text) {
  text = parse(text)
  text = unlist(lapply(text, function(x) lock0(x, key, yek)))
  text = paste0(text, '.', sep='')
  text = paste(text, collapse = ' ')
  return(text)
}

unlock = function(text, key, yek) {
  text = parse(text)
  text = unlist(lapply(text, function(x) paste(unlock0(x, key, yek), '.', sep = '')))
  text = paste(text, collapse = ' ')
  return(text)
}

pack.keys = function() {
  keys = c()
  i = 0
  while (i < length(key)) {
    i = i + 1
    keys = c(keys, key[i], yek[i])
    if (runif(1) < .3) {
      keys = c(keys, ' ')
    } else if (runif(1) < .16) {
      keys = c(keys, '. ')
    }
  }
  paste0(keys, collapse='')
}

# ******************************************************************************
# Two arguments
# 1. --lock or -l         or         --unlock or -ul
# 2. input file path
# ******************************************************************************
vargs = c('--help', '-h', '--lock', '-l', '--unlock', '-ul')
if (length(args) == 0) {
  cat('\nAt least one argument must be provided.\n',
      '--help or -h for help\n\n')
} else {
  if (args[1] %in% vargs) {
    if (args[1] == '--help' | args[1] == '-h') {
      cat('\nHelp is on the way!...\n\n')
    } else {
      if (length(args) < 2) {
        cat('\nPlease provide input file path/name...\n\n')
      } else {
        text = list()
        con = file(args[2], 'r')
        while (T) {
          line = readLines(con, n = 1, warn = F)
          if (length(line) == 0) {
            break
          }
          text = append(text, line)
        }
        close(con)
      }
      if (length(text)) {
        if (args[1] == '--lock' | args[1] == '-l') {
          keys = create.keys(chars)
          key = keys[[1]]
          yek = keys[[2]]
          text = lock(unlist(text))
          text = paste0(pack.keys(), text, sep = '')
        } else {
          text = unlist(strsplit(unlist(text), ''))
          keys = c()
          i = 0
          while (i < (length(chars)*2)) {
            while (text[1] %in% c(' ', '.')) text = text[-1]
            i = i + 1
            keys = c(keys, text[1])
            text = text[-1]
          }
          keys = t(matrix(keys, nrow = 2))
          key = keys[,1]
          yek = keys[,2]
          text = unlock(unlist(paste0(text, collapse = '')), key, yek)
        }
        con = file(args[2], 'w')
        writeLines(text, con)
        close(con)
      }
    }
  } else {
    cat('\nInvalid argument!\n--help or -h for help\n\n')
  }
}
