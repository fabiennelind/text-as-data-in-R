# Wordfish
# June 2025



require(quanteda)
require(quanteda.textmodels)
require(quanteda.textplots)

# A quanteda tutorial: https://tutorials.quanteda.io/machine-learning/wordfish/
# Slapin, Jonathan and Sven-Oliver Proksch. 2008. “A Scaling Model for Estimating Time-Series Party Positions from Texts.” American Journal of Political Science 52(3): 705-772.

# This time, we use a corpus provided with the quanteda package
data_corpus_irishbudget2010


toks_irish <- tokens(data_corpus_irishbudget2010, remove_punct = TRUE)
toks_irish
dfmat_irish <- dfm(toks_irish)

# Run wordfish
tmod_wf <- textmodel_wordfish(dfmat_irish, dir = c(6, 5))
summary(tmod_wf)

# We can plot the results of a fitted scaling model 
textplot_scale1d(tmod_wf)

# plot scores by a grouping variable, in this case the party affiliation of the speakers.
textplot_scale1d(tmod_wf, groups = dfmat_irish$party)

# plot the estimated word positions and highlight certain features.
textplot_scale1d(tmod_wf, margin = "features", 
                 highlighted = c("government", "global", "children", 
                                 "bank", "economy", "the", "citizenship",
                                 "productivity", "deficit"))
