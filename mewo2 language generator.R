#### Setup ####
# Original JS source:
# https://github.com/mewo2/naming-language/blob/master/language.js
# Notes at:
# http://mewo2.com/notes/naming-language/

# Started 25 Jul 2021, completed 9 Aug 2021
## ** in comment = question to revisit/think about

library(stringr)



#### Default values ####
default_ortho <- list(
  "ʃ" = "sh",
  "ʒ" = "zh",
  "ʧ" = "ch",
  "ʤ" = "j",
  "ŋ" = "ng",
  "j" = "y",
  "x" = "kh",
  "ɣ" = "gh",
  "ʔ" = "‘",
  "A" = "á",
  "E" = "é",
  "I" = "í",
  "O" = "ó",
  "U" = "ú"
)

c_orth_sets <- list(
  "Default" = list(), # **Should this be blank? Vs `default_ortho`?
  "Slavic" = list(
    "ʃ" = "š",
    "ʒ" = "ž",
    "ʧ" = "č",
    "ʤ" = "ǧ",
    "j" = "j"
  ),
  "German" = list(
    "ʃ" = "sch",
    "ʒ" = "zh",
    "ʧ" = "tsch",
    "ʤ" = "dz",
    "j" = "j",
    "x" = "ch"
  ),
  "French" = list(
    "ʃ" = "ch",
    "ʒ" = "j",
    "ʧ" = "tch",
    "ʤ" = "dj",
    "x" = "kh"
  ),
  "Chinese (pinyin)" = list(
    "ʃ" = "x",
    "ʧ" = "q",
    "ʤ" = "j"
  )
)

v_orth_sets <- list(
  "Ácutes" = list(), # **Should this be blank? vs something specified ("aeiou") or one of `vow_sets`?
  "Ümlauts" = list(
    "A" = "ä",
    "E" = "ë",
    "I" = "ï",
    "O" = "ö",
    "U" = "ü"
  ),
  "Welsh" = list(
    "A" = "â",
    "E" = "ê",
    "I" = "y",
    "O" = "ô",
    "U" = "w"
  ),
  "Diphthongs" = list(
    "A" = "au",
    "E" = "ei",
    "I" = "ie",
    "O" = "ou",
    "U" = "oo"
  ),
  "Doubles" = list(
    "A" = "aa",
    "E" = "ee",
    "I" = "ii",
    "O" = "oo",
    "U" = "uu"
  )
)

con_sets <- list(
  "Minimal" = "ptkmnls",
  "English-ish" = "ptkbdgmnlrsʃzʒʧ",
  "English-lite" = "ptkbdgmnszʒʧhjw",
  "Pirahã (very simple)" = "ptkmnh",
  "Hawaiian-ish" = "hklmnpwʔ",
  "Greenlandic-ish" = "ptkqvsgrmnŋlj",
  "Arabic-ish" = "tksʃdbqɣxmnlrwj",
  "Arabic-lite" = "tkdgmnsʃ"
)

vow_sets <- list(
  "Standard 5-vowel" = "aeiou",
  "3-vowel a i u" = "aiu",
  "Extra A E I" = "aeiouAEI",
  "Extra U" = "aeiouU",
  "5-vowel a i u A I" = "aiuAI",
  "3-vowel e o u" = "eou",
  "Extra A O U" = "aeiouAOU"
)

s_sets <- list( # sibilants
  "Just s" = "s",
  "s ʃ" = "sʃ",
  "s ʃ f" = "sʃf"
)

l_sets <- list( # liquids
  "r l" = "rl",
  "Just r" = "r",
  "Just l" = "l",
  "w j" = "wj",
  "r l w j" = "rlwj"
)

f_sets <- list( # finals
  "m n" = "mn",
  "s k" = "sk",
  "m n ŋ" = "mnŋ",
  "s ʃ z ʒ" = "sʃzʒ"
)

syll_structs <- c( # see key below for letter meanings
  "CVC",
  "CVV?C",
  "CVVC?", "CVC?", "CV", "VC", "CVF", "C?VC", "CVF?",
  "CL?VC", "CL?VF", "S?CVC", "S?CVF", "S?CVC?",
  "C?VF", "C?VC?", "C?VF?", "C?L?VC", "VC",
  "CVL?C?", "C?VL?C", "C?VLC?"
)
#' C = consonant
#' V = vowel
#' F = final
#' S = sibilant
#' ? = indicates the prior letter is optional
#'     (e.g., "C?VC" is optional-consonant then vowel then consonant)

res_sets <- list(
  "None" = "",
  "Double sounds" = "(.)\\1", # any double letters
  "Doubles and hard clusters" = c("[sʃf][sʃ]", "(.)\\1", "[rl][rl]")
  # ^ any double letters, plus sʃ/ʃs/fs/fʃ and rl/lr
)



#### Helper functions ####
unlist_splitstr <- function(orig_list, sep = "") {
  ## Shorthand for the unlist + str_split combo I use a lot
  unlist(str_split(orig_list, sep))
}

shuffle_list <- function(orig_list) {
  ## Scrambles the order of the letters in the provided list, and converts from a
  ## no-space single string to a (shuffled) character vector of all options
  ## - orig_list = character; string of multiple characters (e.g., "abcd")
  
  split_list <- unlist_splitstr(orig_list)
  sample(split_list)
}

choose_option <- function(choice_set, exponent = 1) {
  ## Select a random option from the provided set, preferentially choosing
  ## earlier or later options based on provided exponent
  ## - choice_set = character; list or vector of characters ("a", "ab", "abc"),
  ##                or single string of multiple characters ("abcd")
  ## - exponent = numeric; serves as exponent for a 0-1 random (uniform) number,
  ##              to skew the result away from a uniform distribution
  
  # If `choice_set` is a multi-character string, convert into character vector
  # (no adverse effects on lists or other character vectors)
  if(is.vector(choice_set) && length(choice_set) == 1) {
    choice_set <- unlist_splitstr(choice_set)
  }
  
  # Generate index
  idx <- ceiling((runif(1) ^ exponent) * length(choice_set))
  
  unlist(choice_set[[idx]])
}

rand_range <- function(lo, hi = NULL) {
  ## Randomly select an integer from the provided range
  ## NOTE: If only one bound is provided, it is assumed to be an upper bound, and
  ##       the lower bound is assumed to be 1 (the lowest R index)
  ## - lo = numeric; bound #1
  ## - hi = numeric; bound #2 (optional)
  
  if(is.null(hi)) {
    hi <- lo
    lo <- 1
  }
  
  sample(lo:hi, 1)
}

join_strings <- function(orig_list, sep = "") {
  ## Concatenate one or more string-pieces into a single string
  ## - orig_list = character, list or vector; all items to combine
  ## - sep = character; character that separates the items being joined
  
  if(length(orig_list) == 0 || is.na(orig_list) || is.null(orig_list)) {
    ""
  } else {
    paste(orig_list, collapse = sep)
  }
}



#### Base language functions ####
make_basic_language <- function() {
  ## **description goes here
  list(
    "phonemes" = list(
      "C" = "ptkmnls",
      "V" = "aeiou",
      "S" = "s",
      "F" = "mn",
      "L" = "rl"
    ),
    "structure" = "CVC",
    "exponent" = 2,
    "restricts" = NULL, # **If specified, does this need to be the name of the set, or the set itself (e.g., "None" vs empty quotes)?
    "c_ortho" = list(), # **Does this need to be specified?
    "v_ortho" = list(), # **Does this need to be specified?
    "no_ortho" = TRUE,
    "no_morph" = TRUE,
    # "no_word_pool" = TRUE, # **Never used - remove?
    "min_syll" = 1,
    "max_syll" = 1,
    "morphemes" = list(), # Will be filled in later
    "words" = list(), # Will be filled in later
    "names" = list(), # Will be filled in later
    "joiner" = " ",
    "max_char" = 12,
    "min_char" = 5,
    "genitive" = NULL, # Original script doesn't include this here but not sure that makes sense
    "definite" = NULL # Original script doesn't include this here but not sure that makes sense
  )
}

make_ortho_language <- function() {
  ## **description goes here
  lang <- make_basic_language()
  lang[["no_ortho"]] <- FALSE
  return(lang)
}

make_random_language <- function() {
  ## **description goes here
  lang <- make_basic_language()
  
  lang[["no_ortho"]] <- FALSE
  lang[["no_morph"]] <- FALSE
  # lang[["no_word_pool"]] <- FALSE
  
  lang[["phonemes"]][["C"]] <- shuffle_list(choose_option(con_sets))
  lang[["phonemes"]][["V"]] <- shuffle_list(choose_option(vow_sets))
  lang[["phonemes"]][["S"]] <- shuffle_list(choose_option(s_sets))
  lang[["phonemes"]][["F"]] <- shuffle_list(choose_option(f_sets))
  lang[["phonemes"]][["L"]] <- shuffle_list(choose_option(l_sets))
  
  lang[["c_ortho"]] <- choose_option(c_orth_sets)
  lang[["v_ortho"]] <- choose_option(v_orth_sets)
  
  lang[["structure"]] <- choose_option(syll_structs)
  lang[["restricts"]] <- res_sets[[3]] # **Make this random?
  lang[["joiner"]] <- sample(c(" ", "-"), 1, prob = c(0.75, 0.25))
  
  
  lang[["min_syll"]] <- rand_range(1, 3)
  if(str_length(lang[["structure"]]) < 3) {
    # Presumably this is to prevent words from being too short?
    # Structure is, by default, "CVC" (3 characters)
    # E.g., if it's just "VC" then this would increase the minimum syllables by 1,
    # to have (slightly) longer resulting words
    lang[["min_syll"]] <- lang[["min_syll"]] + 1
  }
  lang[["max_syll"]] <- rand_range(lang[["min_syll"]] + 1, 7)
  
  return(lang)
}

spell_syllable <- function(lang, syll) {
  ## Convert IPA/shorthand symbols to Latin alphabet
  ## - lang = list; output of make_*_language()
  ## - syll = character string
  ## The function output is a string
  
  if(lang[["no_ortho"]]) {
    # Keep IPA/shorthand symbols instead of converting
    return(syll)
  } else {
    gen_spell <- character()
    syll_split <- unlist_splitstr(syll)
    
    for(i in seq_along(syll_split)) {
      index_name <- syll_split[i]
      
      if(index_name %in% names(lang[["c_ortho"]])) {
        matched_category <- lang[["c_ortho"]][index_name]
      } else if(index_name %in% names(lang[["v_ortho"]])) {
        matched_category <- lang[["v_ortho"]][index_name]
      } else if(index_name %in% names(default_ortho)) {
        matched_category <- default_ortho[index_name]
      } else {
        matched_category <- index_name
      }
      
      gen_spell <- paste0(gen_spell, matched_category)
    }
    return(gen_spell)
  }
}



#### Language-part generation functions ####
make_syllable <- function(lang) {
  ## Generate a syllable based on language structure and syllable restrictions
  ## - lang = list; output of make_*_language()
  ## The function output is a string
  
  keep_going <- TRUE
  
  while(keep_going) {
    syll <- ""
    struc_split <- unlist_splitstr(lang[["structure"]])
    
    for(i in seq_along(struc_split)) {
      phoneme_type <- struc_split[i]
      
      # If the next character is a "?" (i.e., current syllable is optional)
      # then 50/50 chance of skipping the current syllable
      if(!is.na(struc_split[i + 1]) && struc_split[i + 1] == "?") { 
        # !is.na() prevents error if `i` is already at max (i.e., is final structure element)
        if(runif(1) < 0.5) {next}
      }
      if(struc_split[i] == "?") {next} # Skip the actual "?" too
      
      select_from <- unlist_splitstr(lang[["phonemes"]][[phoneme_type]])
      syll <- paste0(syll, choose_option(select_from))
    }
    
    # Only add to the language if `syll` doesn't hit restrictions; break from while() loop
    if(!any(str_detect(syll, lang[["restricts"]]))) {keep_going <- FALSE}
  }
  
  return(spell_syllable(lang, syll))
}

make_morpheme <- function(lang, key, only_new = TRUE) {
  ## Generate a morpheme based on language specifications and an optional key
  ## - lang = list; output of make_*_language()
  ## - key = (character) string; keyword to reference the association of the
  ##         generated morpheme (e.g., "city"); if no key is provided, a
  ##         generic morpheme (no specific association) is generated
  ##         > NOTE: Multiple morphemes can be associated with a specific `key`
  ## - only_new = logical; if FALSE, allow function to return existing morphemes
  ##         (vs only generating new ones if TRUE)
  ## Function output is a string
  
  if(missing(key) || is.na(key) || is.null(key)) {
    key <- "generic"
  }
  
  # If the language doesn't have morphemes, return a regular syllable instead of
  # jumping through hoops to make it specified/special
  if(lang[["no_morph"]]) {
    return(make_syllable(lang))
  }
  
  # Create morpheme list if it doesn't already exist
  if(is.null(lang[["morphemes"]][[key]])) {
    morpheme_list <- character()
  } else {
    morpheme_list <- lang[["morphemes"]][[key]]
  }
  
  # Preferentially add variation in morphemes used:
  # if `key` == "generic", higher chance of new morpheme being generated
  # if `key` is specific, lower chance of of new morpheme being generated
  extras <- ifelse(key == "generic", 10, 1)
  samp_result <- rand_range(length(morpheme_list) + extras)
  
  if(samp_result > length(morpheme_list) || only_new) {
    # "extras" territory (or generate new only); generate new morpheme
    keep_going <- TRUE
    while(keep_going) {
      gen_morpheme <- make_syllable(lang)
      
      # Check for duplication in morpheme
      if(any(str_detect(unlist(lang[["morphemes"]]), gen_morpheme))) {
        next
      } else {
        keep_going <- FALSE
      }
    }
  } else {
    # existing morpheme; pick from `morpheme_list`
    gen_morpheme <- morpheme_list[samp_result]
  }
  
  return(gen_morpheme)
}

make_word <- function(lang, key, only_new = TRUE) {
  ## Generate a word based on language specifications and an optional key
  ## - lang = output of make_*_language() (list)
  ## - key = (character) string; keyword to reference the association of the
  ##         generated word (e.g., "city"); if no key is provided, the
  ##         generic (no specific association) is used
  ##         > NOTE: Multiple words can be associated with a specific `key`
  ## - only_new = logical; if FALSE, allow function to return existing words
  ##         (vs only generating new ones if TRUE)
  ## The function output is a string
  
  if(missing(key) || is.na(key) || is.null(key)) {
    key <- "generic"
  }
  
  # Create word list if it doesn't already exist
  if(is.null(lang[["words"]][[key]])) {
    word_list <- character()
  } else {
    word_list <- lang[["words"]][[key]]
  }
  
  n_sylls <- rand_range(lang[["min_syll"]], lang[["max_syll"]])
  gen_word <- ""
  
  # Generate key for each syllable/morpheme in the generated word;
  # all are "generic" except for one that is randomly selected to be `key`
  key_sequence <- rep_len("generic", n_sylls)
  key_sequence[rand_range(n_sylls)] <- key
  
  extras <- ifelse(key == "generic", 3, 2)
  samp_result <- rand_range(length(word_list) + extras)
  
  if(samp_result > length(word_list) || only_new) {
    # "extras" territory (or generate new); generate new word
    keep_going <- TRUE
    while(keep_going) {
      for(i in 1:n_sylls) {
        gen_word <- paste0(gen_word, make_morpheme(lang, key_sequence[i]))
        # ** Should there be a check somewhere for min/max character count?
        #    Otherwise lang$min_char & lang$max_char are never used.
        #    It's mentioned on the notes page, but isn't in the script?
        # Idea: check letter count after generating (but prior to adding) each syllable;
        # if length < min, go again (another while() loop?)
        # if length > max, don't add the syllable and stop trying (i.e., `break`)
      }
      
      # Check for duplication in words
      if(any(str_detect(unlist(lang[["words"]]), gen_word))) {
        next
      } else {
        keep_going <- FALSE
      }
    }
  } else {
    # existing word; pick from word_list
    gen_word <- word_list[samp_result]
  }
  
  return(gen_word)
}

make_name <- function(lang, key) {
  ## Generate a proper name based on language specifications and an optional key
  ## - lang = output of make_*_language() (list)
  ## - key = (character) string; keyword to reference the association of the
  ##         generated name (e.g., "city"); if no key is provided, the
  ##         generic (no specific association) is used
  ##         > NOTE: Multiple names can be associated with a specific `key`
  ## The function output is a string
  
  # **Add only_new argument to this too
  
  if(missing(key) || is.na(key) || is.null(key)) {
    key <- "generic"
  }
  
  # Create name list if it doesn't already exist
  if(is.null(lang[["names"]][[key]])) {
    name_list <- character()
  } else {
    name_list <- lang[["names"]][["key"]]
  }
  
  # Add definite & genitive if they don't already exist
  lang <- add_genitive(lang)
  lang <- add_definite(lang)
  
  keep_going <- TRUE
  while(keep_going) {
    gen_name <- ""
    
    if(runif(1) < 0.5) { # 50/50 chance of single-word name
      gen_name <- str_to_sentence(make_word(lang, key))
    } else { # multi-word name
      # 60% chance to use provided key in the name each time
      word1 <- str_to_sentence(make_word(lang, ifelse(runif(1) < 0.6, key, "generic")))
      word2 <- str_to_sentence(make_word(lang, ifelse(runif(1) < 0.6, key, "generic")))
      
      if(word1 == word2) {
        next
      }
      
      if (runif(1) > 0.5) { # chance of using genitive (e.g., "of") between name-words
        gen_name <- join_strings(c(word1, lang[["genitive"]], word2), lang[["joiner"]])
      } else {
        gen_name <- join_strings(c(word1, word2), lang[["joiner"]])
      }
    }
    
    # Small chance of using definite article (e.g., "the") as a phrase-prefix
    if(runif(1) < 0.1) {
      gen_name <- join_strings(c(lang[["definite"]], gen_name), lang[["joiner"]])
      # ** Capitalization of definite-word yes/no? My choice; source script doesn't cap
    }
    
    # Check name length against min/max
    if(str_length(gen_name) < lang[["min_char"]] || str_length(gen_name) > lang[["max_char"]]) {next}
    
    # Check for duplicated name
    if(any(str_detect(unlist(lang[["names"]]), gen_name))) {
      next
    } else {
      keep_going <- FALSE
    }
  }
  
  return(gen_name)
}



#### Language-part addition functions ####
add_morpheme <- function(lang, key = "generic") {
  ## Generate a morpheme based on language specifications and an optional key
  ## - lang = list; output of make_*_language()
  ## - key = (character) string; keyword to reference the association of the
  ##         generated morpheme (e.g., "city"); if no key is provided, a
  ##         generic morpheme (no specific association) is generated
  ##         > NOTE: Multiple morphemes can be associated with a specific `key`
  ## Function output is a modified version of `lang` that has the morpheme added
  
  if(is.null(lang[["morphemes"]][[key]])) {
    morpheme_list <- character()
  } else {
    morpheme_list <- lang[["morphemes"]][[key]]
  }
  
  morpheme_result <- make_morpheme(lang, key)
  
  # Save morpheme_list to `lang`, then return the modified version of `lang`
  lang[["morphemes"]][[key]] <- c(morpheme_list, morpheme_result)
  return(lang)
}

add_word <- function(lang, key = "generic") {
  ## Generate a word based on language specifications and an optional key
  ## - lang = output of make_*_language() (list)
  ## - key = (character) string; keyword to reference the association of the
  ##         generated morpheme (e.g., "city"); if no key is provided, a
  ##         generic morpheme (no specific association) is used
  ##         > NOTE: Multiple morphemes can be associated with a specific `key`
  ## Function output is a modified version of `lang` that has the word added
  
  if(is.null(lang[["words"]][[key]])) {
    word_list <- character()
  } else {
    word_list <- lang[["words"]][[key]]
  }
  
  word_result <- make_word(lang, key)
  
  # Save word_list to `lang`, then return the modified version of `lang`
  lang[["words"]][[key]] <- c(word_list, word_result)
  return(lang)
}

add_genitive <- function(lang) {
  ## Check for existing genitive in `lang`, create one if it doesn't exist
  ## Outputs modified version of `lang` with new genitive morpheme
  
  if(is.null(lang[["genitive"]])) {
    lang <- c(lang, "genitive" = make_morpheme(lang, "of"))
    # **Does it make more sense to just `add_morpheme(lang, "of")`?
    # What's the benefit from having it separate?
  }
  
  return(lang)
}

add_definite <- function(lang) {
  ## Check for existing definite in `lang`, create one if it doesn't exist
  ## Outputs modified version of `lang` with new definite morpheme
  
  if(is.null(lang[["definite"]])) {
    lang <- c(lang, "definite" = make_morpheme(lang, "the"))
    # **Does it make more sense to just `add_morpheme(lang, "the")`?
    # What's the benefit from having it separate?
  }
  
  return(lang)
}

add_name <- function(lang, key = "generic") {
  ## Generate a name based on language specifications and an optional key
  ## - lang = output of make_*_language() (list)
  ## - key = (character) string; keyword to reference the association of the
  ##         generated morpheme (e.g., "city"); if no key is provided, a
  ##         generic morpheme (no specific association) is used
  ##         > NOTE: Multiple morphemes can be associated with a specific `key`
  ## Function output is a modified version of `lang` that has the name added
  
  if(is.null(lang[["names"]][[key]])) {
    name_list <- character()
  } else {
    name_list <- lang[["names"]][[key]]
  }
  
  name_result <- make_name(lang, key)
  
  # Save word_list to `lang`, then return the modified version of `lang`
  lang[["names"]][[key]] <- c(name_list, name_result)
  return(lang)
}



#### Using the functions ####
# Base language
gen_lang <- make_random_language()

# Add morphemes to (copy of) language
gen_lang2 <- add_morpheme(gen_lang)
for(i in 1:4) {
  gen_lang2 <- add_morpheme(gen_lang2)
}
gen_lang2$morphemes


# Add words to (copy of) language
gen_lang3 <- add_word(gen_lang2)
for(i in 1:4) {
  gen_lang3 <- add_word(gen_lang3)
}
gen_lang3$words


# Make a name
make_name(gen_lang3)


# Add names to (copy of) language
gen_lang4 <- add_name(gen_lang3)
gen_lang4 <- add_name(gen_lang4)
gen_lang4$names

rm(i)

# Can save a language using
# dump("gen_lang4", file = "~/language_dump.R")
