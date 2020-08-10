\name{ctgdist}
\alias{ctgdist}
\title{Likert Category Scaling}
\usage{
ctgdist(dataset)
}
\description{
It is assumed that psychological distances between the categories are equal for the measurement instruments consisted of polytomously scored items. According to Muraki (1993), this assumption must be tested. In the examination process of this assumption, the fit indexes are obtained and evaluated. This package provides that this assumption is removed. By with this package, the converted scale values of all items in a measurement instrument can be calculated by estimating a category parameter set for each item. Thus, the calculations can be made without any need to usage of the common category parameter set. Through this package, the psychological distances of the items are scaled. The scaling of a category parameter set for each item cause differentiation of score of the categories will be got from items. Also, the total measurement instrument score of an individual can be calculated according to the scaling of item score categories by with this package.
This package provides that the place of individuals related to the structure to be measured with a measurement instrument consisted of polytomously scored items can be reveal more accurately. In this way, it is thought that the results obtained about individuals can be made more sensitive, and the differences between individuals can be revealed more accurately. On the other hand, it can be argued that more accurate evidences can be obtained regarding the psychometric properties of the measurement instruments.
}
\arguments{
  \item{dataset}{A data frame or matrix of polytomous item responses.}

}
\value{

A data frame of category score values (converted scale values) for all items.

Total test scores for all individuals which calculated with converted scale values.

}


\examples{


x1<-sample(1:5, 500, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2))
x2<-sample(1:5, 500, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.2, 0.1))
x3<-sample(1:5, 500, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2))
x4<-sample(1:5, 500, replace = TRUE, prob = c(0.15, 0.2, 0.3, 0.2, 0.15))
x5<-sample(1:5, 500, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2))
mydat<-cbind(x1,x2,x3,x4,x5)

ctgdist(mydat)


}
