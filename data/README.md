# Hadley Wickham and Guido van Rossum personalized pagerank sample

This is a sample of the Twitter following graph from mid July 2021, focused on
Hadley Wickham and Guido van Rossum. This sample includes 122,986 twitter
users and 249,108 edges. It does *is not* the full induced subgraph on
these nodes -- in particular, we only have full following information for a 
certain subset of particular important users.

If you find anything interesting in this data please let me know :). I am
about present work on the tool I used to collect this data at the JSM and fun
data tidbits would be very fun to include! 

Please direct any data questions / fun data analysis to <alex.hayes@wisc.edu>.
I am also very happy to help you collect similar data on different users.

## Data Dictionary

### `hadley_guido_users.csv`

 - `user_id <chr>`: Twitter user id of user doing the following. A 64 bit
  integer that you should almost certainly just treat as a character
  vector/string.
 - `screen_name <chr>`: Twitter user handle
 - `protected <lgl>`: Where or not an account is protected. You need addition
  permissions from protected users to see their tweets or friend and following
  lists. There should be no protected users in our sample, we purposefully
  exclude them.
 - `followers_count <int>`: How many follows a user has.
 - `friends_count <int>`: How many people the user follows.
 - `listed_count <int>`: How many lists a person is on, I think. Might be how 
  how many lists they have.
 - `statuses_count <int>`: How many tweets (including retweets) a user has done.
  Deleted tweets still count towards this IIUC.
 - `favourites_count <int>`: How many tweets a user has liked.
 - `account_created_at <dttm>`: When the account was created.
 - `verified <lgl>`: If the account is verified (i.e. a blue check)
 - `profile_url <chr>`: Shortened user specified link that appears near their
  bio
 - `profile_expanded_url <chr>`: User specified link, but without url
  shortening
 - `account_lang <chr>`: ISO2 language code of account
 - `profile_banner_url <chr>`: I never use this so I don't recall precisely
  what it is.
 - `profile_background_url <chr>`: I also never use this.
 - `profile_image_url <chr>`: URL of profile pic. Nice if you want to
  embed bio-related info in stuff.
 - `name <chr>`: What the user lists as their name. People change these all the
  time and they can contain Unicode bombs, so have fun with that. In contrast,
  screen names are much harder to change (but still can change!). In further
  contrast, `user_id` is an unchanging user id that you should use as a
  primary key.
 - `location <chr>`: User entered text for the location box in their bio
 - `description <chr>`: User entered bio
 - `url <chr>`: Uh... a copy of `profile_url`? The v1 Twitter API is a
  shitshow, I don't know what to tell you.
 - `sampled_at <dttm>`: The timestamp when I ran `rtweet::lookup_users()`
  on the user and collected their bio info. I did this for all users in this
  sample.
 - `sampled_friends_at <dttm>`: The timestamp when I ran `rtweet::get_friends()`
  on the user and figured out who they followed. `NA` otherwise. Will be
  `NA` for almost all users.
 - `sampled_followers_at <dttm>`: The timestamp when I ran
  `rtweet::get_follows()` on the user and figured out who followed them.
  Should be `NA` for all users. The only way will have incoming edges is if
  we sampled them as outgoing edges for a different user.
 - `personal_pagerank_residual <dbl>`: For advanced users only, i.e. Karl. `r`
  for the `aPPR` calculation.
 - `approximate_personal_pagerank <dbl>`: An estimate of the accounts 
  personalized pagerank, where the seed nodes are `hadleywickham` and 
  `gvanrossum`, the dampening factor is `0.15` and the approximation is
  guaranteed to have maximum elementwise error of `1e-6`.
 - `degree_adjusted_personal_pagerank <dbl>`: The approximate personalized
  pagerank divided by node degree.
 - `regularized_personal_pagerank <dbl>`: The approximate personalized
  pagerank divided by node degree plus a regularization factor, in this
  case the mean degree of all nodes in this csv. If you rank nodes according to
  `regularized_personal_pagerank` the top nodes should constitute a block
  in a stockmodel model. i.e. this is a consistent estimator of community
  structure under a basic blockmodel. See <https://arxiv.org/abs/1910.12937>
  for details.

### `hadley_guido_edges.csv`

Each row corresponds to one edge in the Twitter following graph. These edges
are directed.

- `from <chr>`: Twitter user id of user doing the following. A 64 bit integer that
  you should almost certainly just treat as a character vector/string.
- `to <chr>`: Twitter used id of user who is followed. A 64 bit integer that
  you should almost certainly just treat as a character vector/string.
