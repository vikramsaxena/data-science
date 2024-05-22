library(readtext)
wlove<-readLines("roget-love")
wfear<-readLines("roget-fear")
stopwords<-readLines("stopwords-big")

wlove<-paste(wlove, collapse=" ")
wlove<-strsplit(tolower(wlove), "[^a-z]")[[1]]
wlove<-wlove[nchar(wlove)>5]
wlove<-unique(wlove)

wfear<-paste(wfear, collapse=" ")
wfear<-strsplit(tolower(wfear), "[^a-z]")[[1]]
wfear<-wfear[nchar(wfear)>5]
wfear<-unique(wfear)

swd<-data.frame(character(), integer(), integer(), stringsAsFactors=FALSE)

wil<-readLines("C-evilgenius.txt")
wil<-paste(wil, collapse=" ")
wil<-strsplit(tolower(wil), "[^a-z]")[[1]]
wil<-wil[nchar(wil)>5]
wil<-wil[!is.element(wil, stopwords)]
leng<-length(wil)
wilwds<-as.data.frame(table(wil))

nn<-0
for(ww in wlove) {
	cn<-wilwds[wilwds$wil==ww, 2];
	if(length(cn)>0) {nn<-nn+cn;}
}

fn<-0
for(ww in wfear) {
	cn<-wilwds[wilwds$wil==ww, 2];
	if(length(cn)>0) {fn<-fn+cn;}
}

sen<-c(nn,fn)

barplot(sen, names.arg=c("Love","Fear"), main="Sentiment Analysis of 'Evil Genius'", ylab="Number of Words", ylim=c(0, 500))