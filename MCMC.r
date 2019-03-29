reference=readLines("/Users/rajat/Downloads/brown.txt")
reference=toupper(reference)
 
trans.mat=matrix(0,27,27)
rownames(trans.mat)=colnames(trans.mat)=c(toupper(letters),"")
lastletter=""
for (ln in 1:length(reference)) {
  if (ln %% 1000 ==0) {cat("Line",ln,"\n")}
  for (pos in 1:nchar(reference[ln])) {
    curletter=substring(reference[ln],pos,pos)
    if (curletter %in% toupper(letters)) {
      trans.mat[rownames(trans.mat)==lastletter,
                colnames(trans.mat)==curletter]=
        trans.mat[rownames(trans.mat)==lastletter,
                  colnames(trans.mat)==curletter]+1
      lastletter=curletter
    } else {
      if (lastletter!="") {
        trans.mat[rownames(trans.mat)==lastletter,27]=
          trans.mat[rownames(trans.mat)==lastletter,27]+1
        lastletter=""
      }
    }
  }
  curletter=""
  if (lastletter!="") {
    trans.mat[rownames(trans.mat)==lastletter,27]=
      trans.mat[rownames(trans.mat)==lastletter,27]+1
  }
  lastletter=""
}
 
trans.prob.mat=sweep(trans.mat+1,1,rowSums(trans.mat+1),FUN="/")

decode <- function(mapping,coded) {
  coded=toupper(coded)
  decoded=coded
  for (i in 1:nchar(coded)) {
    if (substring(coded,i,i) %in% toupper(letters)) {
      substring(decoded,i,i)=toupper(letters[mapping==substring(coded,i,i)])
    }
  }
  decoded
}
 
 
log.prob <- function(mapping,decoded) {
  logprob=0
 
  lastletter=""
  for (i in 1:nchar(decoded)) {
    curletter=substring(decoded,i,i)
    if (curletter %in% toupper(letters)) {
      logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,
                                         colnames(trans.mat)==curletter])
      lastletter=curletter
    } else {
      if (lastletter!="") {
        logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,27])
        lastletter=""
      }
    }
  }
 
  if (lastletter!="") {
    logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,27])
    lastletter=""
  }
  logprob
}

#coded=decode(sample(toupper(letters)),correctTxt) # randomly scramble the text
coded = "A BCDE DFGBHIJ KLMKGN 
OPQRSTU AV CW XY ZSQa bSc 
Fd He Zfgh iSj klmn DABGCIJ 
oFBp JqZr Hc saE btuvLTV ONQRSWP 
ZRj AX dlU ZMwk xmnJhgfST qIAZRG 
fi mBG st DCBG WMZrVaFcJ JHyEX 
ZL dlN ZYwk sauAzGCjJ PbKUvhnQr 
FV HT EyLI ONdkPg slqc JUVdAjJ 
afSu vtQDW Mbi oCka R JFgB slh 
OrXV KRud Yb Hk Aw saqV olEn C 
xFL H oAzG ON vPOfgI Cc KmuqpFQU 
mjx RBB kah H lmyr DABBEp oCzG 
OLQMZN Za TzRyPW A oAGB Ifk 
JFyP atS Za cmZN OEQRSXr afS 
oABz sga VY TGMo xMoj tv esYK Za 
QtzzUQkCIJ Mi TBRyEW bfg Za mbdPu GCiN"

mapping=sample(toupper(letters)) # initialize a random mapping
i=1
iters=10000
cur.decode=decode(mapping,coded)
cur.loglike=log.prob(mapping,cur.decode)
max.loglike=cur.loglike
max.decode=cur.decode
while (i<=iters) {
  proposal=sample(1:26,2) # select 2 letters to switch
  prop.mapping=mapping
  prop.mapping[proposal[1]]=mapping[proposal[2]]
  prop.mapping[proposal[2]]=mapping[proposal[1]]
 
  prop.decode=decode(prop.mapping,coded)
  prop.loglike=log.prob(prop.mapping,prop.decode)
 
  if (runif(1)<exp(prop.loglike-cur.loglike)) {
    mapping=prop.mapping
    cur.decode=prop.decode
    cur.loglike=prop.loglike
 
    if (cur.loglike>max.loglike) {
      max.loglike=cur.loglike
      max.decode=cur.decode
    }
 
    cat(i,cur.decode,"\n")
    i=i+1
  }
}