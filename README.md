very basic pos tagger in haskell
==========

This is my first to write something using Haskell, so it may not be very "functional". I hope to update the code once I'm more into it.

* function dp is the implementation of Virtebi Algorithm without back pointers
* function dpbp is the implementation of Virtebi Algorithm with back pointers
* function tag is the naive implementation (very slow for sentences with more than 3 words)

The training data is taken from http://www.cnts.ua.ac.be/conll2000/chunking/train.txt.gz

Example
=======

The training data should be in this format:

He        PRP  B-NP
reckons   VBZ  B-VP
the       DT   B-NP
current   JJ   I-NP
account   NN   I-NP
deficit   NN   I-NP
will      MD   B-VP
narrow    VB   I-VP
to        TO   B-PP
only      RB   B-NP
\#         #    I-NP
1.8       CD   I-NP
billion   CD   I-NP
in        IN   B-PP
September NNP  B-NP
.         .    O

Example:

Run from terminal using:

runhaskell postagger.hs

The performance is not that good:

$ time runhaskell postagger.hs
Begin
["nnp","vbz","dt","nn"]
("chancellor walks the dog",["nnp","vbz","dt","nn"])
("the dog walks",["dt","nn","vbz"])
("i make dinner",["prp","vbp","nn"])
("the dog makes the dinner for us",["dt","nn","vbz","dt","nn","in","prp"])
End

real    0m27.020s
user    0m0.000s
sys     0m0.000s


Sentences to be analyzed are hardcoded :)
