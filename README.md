# Politial Blog Posts

- Scrape blog posts: a scrape_blog_posts.ipynb file.

  The main script loops through blog urls and apply the appropriate scraping function to each. The scraping function takes the blog .html and provides a list of links to urls at which the posts can be found. Then another function extracts the desired data from the .html of each blog post.

  - Inputs: .csv of blog urls. 
  - Outputs: .json file of blog posts. In python this is a list of dictionaries. Each list element is a blog post, with dictionary key attributes of:  
    - domain: string with the general web site of blog where post was found.
    - links: string vector with other web sites which occurred as hyperlinks throughout the post.
    - token_stem_text: string vector with stemmed, tokenized text of the blog post.
    - url: string with web url where the blog post can be viewed.
    - date_flag: string with how the date was obtained.
    - text_body: string with the entire raw text of the blog post.
    - date: string giving the date in a mm/dd/yyyy format.
    
- Pre-process blog posts: PreProcessBlogPosts.R and PreProcessBlogPosts_Auxiliary.R files.  

  The main script uses functions in the auxiliary script to clean and format the posts. For the domains, www is removed. For links, duplicate links, self links, and external links are removed and the links are sorted alphabetically. For words, the punctuation, symbols, and spaces are removed. Letters, days, months, numbers, stop words are removed, as well as infrequent, uncommon, and unpopular words. TFIDF variance thresholding is used to remove non-informative words. Short posts and duplicate posts are deleted.

  - Inputs: for_tim.json.gz file of blog posts.  
  - Outputs: textNetwork.csv file of cleaned blog posts. This is rows of blog posts, columns of:
    - date: a string with the post's date in a mm/dd/yyyy format.
    - domain: a string with the post's minimal web domain.
    - links: a string with the post's links, separated by spaces.
    - words: a string with the post's words, separated by spaces.
    
- Analyze blog posts: BNBP_PFA.R and BNBP_PFA_Auxiliary.R, rtm_on_blogs.R, sta663_topic_link_block_lda_Derek.ipynb files.

  The first script sources the second and implements Gibbs sampling from the Marked Beta Negative Binomial Process Topic Model of http://people.ee.duke.edu/~lcarin/Mingyuan_PAMI_6.pdf.

  - Inputs: textNetwork.csv file with cleaned blog posts.
  - Outputs: posterior samples of:
    - theta_kn: document topic scores.
    - phi_kv: topic word probabilities. 
    - r_k: stopping parameter for NB distribution on document topic counts.
    - p_k: success probability parameter for NB distribution on document topic counts.
    
  The third script uses Jonathan Chang's R package lda https://cran.r-project.org/web/packages/lda/lda.pdf which implements Gibbs sampling from the relational topic model of http://proceedings.mlr.press/v5/chang09a/chang09a.pdf.

  - Inputs: 
  - Outputs:
  
  The fourth script implements Gibbs sampling from Derek Owens-Oas's topic link block model, a novel Bayesian probabilistic model for learning topics and finding communities in documents with links.

  - Inputs: textNetwork.csv file with cleaned blog posts.
  - Outputs: posterior samples of:  
    - b_n: block assignment for observation n.
    - z_wn: topic assignment for word w of observation n.
    - phi_vk: probability for token v in topic k.
    - pi_bl: link l proportion for block b.
    - theta_bk: topic k proportion for block b.

