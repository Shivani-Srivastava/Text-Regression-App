# +++ defining a purely clean_text op
clean_text <- function(text, lower=FALSE, alphanum=FALSE, drop_num=FALSE){
  text  =  str_replace_all(text, "<.*?>", " ")   # drop html junk
  
  if (lower=="TRUE") {text = text %>% str_to_lower()}
  if (alphanum=="TRUE") {text = text %>% str_replace_all("[^[:alnum:]]", " ")}
  if (drop_num=="TRUE") {text = text %>% str_replace_all("[:digit:]", "")}
  
  # collapse multiple spaces
  text = text %>%   
    str_replace_all("\\\\s+", " ")  
  
  return(text) } # clean_text() ends

### +++ new func to cast DTMs outta processed corpora +++ ###
casting_dtm <- function(text_as_df,      # text_as_df is single df colm 
                        tfidf=FALSE,     
                        use.stopwords=TRUE,    # whether to use stopwords at all 
                        additional.stopwords=NULL){ # any additional stopwords?
  
  ## tokenizing the corpus
  textdf1 = text_as_df %>% 
    mutate(docID = row_number()) %>%    # row_number() is v useful.    
    group_by(docID) %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) %>% ungroup()
  
  ## make stop.words list
  stop.words = data.frame(word = as.character(unique(c(additional.stopwords, stop_words$word))),
                          stringsAsFactors=FALSE)   
  
  if (use.stopwords == "TRUE"){ textdf1 = textdf1 %>% anti_join(stop.words) }
  
  ## cast into a Matrix object
  if (tfidf == "TRUE") {
    textdf2 = textdf1 %>% group_by(docID) %>% 
      count(word, sort=TRUE) %>% ungroup() %>%
      bind_tf_idf(word, docID, n) %>% 
      rename(value = tf_idf)
  } else { 
    textdf2 = textdf1 %>% rename(value = n)  }
  
  m <- textdf2 %>% cast_sparse(docID, word, value); dim(m)
  
  # reorder dtm to have sorted rows by doc_num and cols by colsums  
  m = m[order(as.numeric(rownames(m))),]    # reorder rows  
  b0 = apply(m, 2, sum) %>% order(decreasing = TRUE)
  m = m[, b0]
  
  # anomaly handling
  m = as.matrix(m)
  a5 = seq(1:nrow(text_as_df))
  a6 = !(a5 %in% rownames(m)); length(a6); a6[1:5]
  a7 = matrix(0, nrow=sum(a6), ncol=ncol(m))
  rownames(a7) = which(a6)
  colnames(a7) = colnames(m)
  a8 = rbind(m, a7); dim(a8); 
  a9 = sort(as.numeric(rownames(a8)), decreasing=FALSE, index.return=TRUE)
  a8 = a8[a9$ix,]  # this is output. matrix
  
  return(a8) }    # func ends


### +++ new func to preprocess n prune DTMs +++ ###
preprocess_dtm <- function(dtm, min_occur = 0.02, max_occur = 0.95){
  
  a0 = apply(dtm, 2, sum) %>% as.numeric()
  min_thresh = quantile(a0, min_occur) %>% as.numeric(); min_thresh
  max_thresh = quantile(a0, max_occur) %>% as.numeric(); max_thresh
  a1 = (a0 > max_thresh | a0 < min_thresh); length(a1); sum(a1)
  dtm1 = dtm[,!a1]; dim(dtm1)
  return(dtm1)
  
} # func ends


# ' --- piping a workflow based on above 3 sourced funcs --- '

text_regn_main <- function(text_colm_ui, min_occurrence_ui, max_occurrence_ui, 
                           tfidf_ui)
{
  
  my_dtm = text_colm_ui %>% tibble(text = .) %>% 
    
    map_dfr(., function(x) clean_text(x, lower=TRUE)) %>% 
    
    casting_dtm(tfidf=tfidf_ui) %>% 
    
    preprocess_dtm(min_occur = min_occurrence_ui, 
                   max_occur = max_occurrence_ui) %>% 
    
    as.data.frame()  # 58s # make a n x 2000 DF available for download?
  
} # func ends


