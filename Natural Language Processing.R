# Importing the datset

stayzilla <- read.csv(file="stayzilla_com-travel_sample1.csv", header=TRUE, sep=",")

#https://rpubs.com/lmullen/nlp-chapter
options(java.parameters = "-Xmx2048m")
library(rJava)
library(NLP)
library(openNLP)
library(magrittr)
library(openNLPmodels.en)

require(rJava)
require(NLP)
require(openNLP)
require(magrittr)


text <- stayzilla$description
text <- paste(text, collapse = "")
text = as.String(text)

#sentences and word Annotations
word_ann = Maxent_Word_Token_Annotator()
sent_ann = Maxent_Sent_Token_Annotator()
pos_ann = Maxent_POS_Tag_Annotator()

pos_annotation = annotate(text, list(sent_ann,word_ann,pos_ann))
text_annotation = annotate(text,list(sent_ann,word_ann))
head(text_annotation)

text_doc = AnnotatedPlainTextDocument(text,text_annotation)
words(text_doc) %>% head(3)

#annotating People, places
person_ann = Maxent_Entity_Annotator(kind = "person")
location_ann = Maxent_Entity_Annotator(kind = "location")

#putting everything in a text pipeline
pipeline = list(sent_ann,word_ann,person_ann,location_ann,date_ann,organization_ann)

text_annotation = annotate(text,pipeline)
text_doc = AnnotatedPlainTextDocument(text,text_annotation)
entities = function(doc,kind) #function extract name entities from the document.
{
  s = doc$content
  a = annotations(doc)[[1]]
  if (hasArg(kind))
  {
    k = sapply(a$features,'[[',"kind")
    s[a[k == kind]]
  }
  else
  {
    s[a[a$type == "entity"]]
  }
}

#allocating words to their respective entity.
person <- entities(text_doc, kind = "person")
loc <- entities(text_doc, kind = "location")

#visualization 
#bar plots of the word counts.
library(googleVis)

df_person <- data.frame(table(person))
barp <- gvisColumnChart(df_person)
plot(barp)

df_location <- data.frame(table(loc))
barl <- gvisColumnChart(df_location)
plot(barl)







