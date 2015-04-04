from pprint import pprint
from Parser import Parser
import util
import operator
import math

class VectorSpace:
    """ A algebraic model for representing text documents as vectors of identifiers. 
    A document is represented as a vector. Each dimension of the vector corresponds to a 
    separate term. If a term occurs in the document, then the value in the vector is non-zero.
    """

    #Collection of document term vectors
    documentVectors = []
    documentVectorsIDF = []
    documentVectorsTFIDF = []
    globalDocuments = []

    #Mapping of vector index to keyword
    vectorKeywordIndex=[]

    #Tidies terms
    parser=None


    def __init__(self, documents=[]):
        self.documentVectors=[]
        self.parser = Parser()
        self.globalDocuments = documents
        if(len(documents)>0):
            self.build(documents)

    def build(self,documents):
        """ Create the vector space for the passed document strings """
        self.vectorKeywordIndex = self.getVectorKeywordIndex(documents)
        self.documentVectors = [self.makeVector(document) for document in documents]
        for i in range(len(self.documentVectors)):
            tmp = []
            for j in range(len(self.documentVectors[i])):
                tmp.append(self.documentVectorsIDF[j] * float(self.documentVectors[i][j]))
            self.documentVectorsTFIDF.append(tmp)


    def getVectorKeywordIndex(self, documentList):
        """ create the keyword associated to the position of the elements within the document vectors """

        #Mapped documents into a single word string	
        vocabularyString = " ".join(documentList)

        vocabularyList = self.parser.tokenise(vocabularyString)
        #Remove common words which have no search value
        vocabularyList = self.parser.removeStopWords(vocabularyList)
        uniqueVocabularyList = util.removeDuplicates(vocabularyList)

        vectorIndex={}
        offset=0
        #Associate a position with the keywords which maps to the dimension on the vector used to represent this word
        self.documentVectorsIDF = [0.0] * len(uniqueVocabularyList)
        for word in uniqueVocabularyList:
            vectorIndex[word]=offset
            self.documentVectorsIDF[offset] = math.log10(float(len(self.globalDocuments)) / float(self.includingDocs(word))) + 1.0
            offset+=1
        return vectorIndex  #(keyword:position)

    def includingDocs(self, word):
        counter = 1
        for doc in self.globalDocuments:
            if(doc.find(word) != -1):
                counter += 1
        return counter


    def makeVector(self, wordString, mode = "tf"):
        """ @pre: unique(vectorIndex) """
        vector = [0.0] * len(self.vectorKeywordIndex)
        wordList = self.parser.tokenise(wordString)
        wordList = self.parser.removeStopWords(wordList)

        for word in wordList:
            vector[self.vectorKeywordIndex[word]] += 1.0

        if(mode == "tf-idf"):
            for i in range(len(vector)):
                if(vector[i] > 0.0):
                    vector[i] *= self.documentVectorsIDF[i]
        return vector


    def buildQueryVector(self, termList, mode="tf"):
        """ convert query string into a term vector """
        if(mode == "tf"):
            query = self.makeVector(" ".join(termList))
        elif(mode == "tf-idf"):
            query = self.makeVector(" ".join(termList), mode="tf-idf")
        return query


    # def related(self,documentId):
    #     """ find documents that are related to the document indexed by passed Id within the document Vectors"""
    #     ratings = [util.cosine(self.documentVectors[documentId], documentVector) for documentVector in self.documentVectors]
    #     #ratings.sort(reverse=True)
    #     return ratings


    def search(self,searchList,mode="tf_cos"):
        """ search for documents that match based on a list of terms """
        if(mode == "tf_cos"):
            queryVector = self.buildQueryVector(searchList)
            ratings = [util.cosine(queryVector, documentVector) for documentVector in self.documentVectors]
        elif(mode == "tfidf_cos"):
            queryVector = self.buildQueryVector(searchList, mode="tf-idf")
            ratings = [util.cosine(queryVector, documentVector) for documentVector in self.documentVectorsTFIDF]
        elif(mode == "tf_jac"):
        	queryVector = self.buildQueryVector(searchList)
        	ratings = [util.jaccard(queryVector, documentVector) for documentVector in self.documentVectors]
        elif(mode == "tfidf_jac"):
        	queryVector = self.buildQueryVector(searchList)
        	ratings = [util.jaccard(queryVector, documentVector) for documentVector in self.documentVectorsTFIDF]
        return ratings


if __name__ == '__main__':
    #test data
    # documents = ["The cat in the hat disabled", "A cat is a fine pet ponies.", "Dogs and cats make good pets.","I haven't got a hat."]

    docLines = open('wsm_essays_small.txt', 'r').readlines()
    documents = []
    for line in docLines:
        tmp = line.split(":::")[1].replace("!", "").replace("@", "").replace("#", "").replace("$", "").replace("%", "").replace("^", "").replace("&", "").replace("*", "").replace("(", "").replace(")", "").replace("-", "").replace("_", "").replace("+", "").replace("=", "").replace("<", "").replace(">", "").replace("\\", "").replace("/", "").replace("?", "").replace("\r", "").replace("\\n", "").replace("\"", "").replace("\'", "").replace(",", "").replace("~", "").replace("     ", " ").replace("    ", " ").replace("   ", " ").replace("  ", " ")
        tmp = tmp[:-3]
        documents.append(tmp)

    vectorSpace= VectorSpace(documents)

    # Problem 1

    problem_1 = vectorSpace.search(["sport"])
    problem_1_match = {}

    for i in range(len(problem_1)):
        if(problem_1[i] > 0.0):
            problem_1_match[i] = problem_1[i]

    print "Term Frequency (TF) Weighting + Cosine Similarity"
    print ""
    print "DocID   Score"
    problem_1_top5 = sorted(problem_1_match.items(), key=operator.itemgetter(1), reverse=True)[:5]
    for rank in problem_1_top5:
        print '{:<7}'.format(rank[0]), round(problem_1_match[rank[0]], 6)
    print ""

    # Problem 2

    problem_2 = vectorSpace.search(["sport"], mode="tf_jac")
    problem_2_match = {}

    for i in range(len(problem_2)):
        if(problem_2[i] > 0.0):
            problem_2_match[i] = problem_2[i]

    print "Term Frequency (TF) Weighting + Jaccard Similarity"
    print ""
    print "DocID   Score"
    problem_2_top5 = sorted(problem_2_match.items(), key=operator.itemgetter(1), reverse=True)[:5]
    for rank in problem_2_top5:
        print '{:<7}'.format(rank[0]), round(problem_2_match[rank[0]], 6)
    print ""

    # Problem 3

    problem_3 = vectorSpace.search(["sport"], mode="tfidf_cos")
    problem_3_match = {}

    for i in range(len(problem_3)):
        if(problem_3[i] > 0.0):
            problem_3_match[i] = problem_3[i]

    print "TF-IDF Weighting + Cosine Similarity"
    print ""
    print "DocID   Score"
    problem_3_top5 = sorted(problem_3_match.items(), key=operator.itemgetter(1), reverse=True)[:5]
    for rank in problem_3_top5:
        print '{:<7}'.format(rank[0]), round(problem_3_match[rank[0]], 6)
    print ""

    # Problem 4

    problem_4 = vectorSpace.search(["sport"], mode="tfidf_jac")
    problem_4_match = {}

    for i in range(len(problem_4)):
        if(problem_4[i] > 0.0):
            problem_4_match[i] = problem_4[i]

    print "TF-IDF Weighting + Jaccard Similarity"
    print ""
    print "DocID   Score"
    problem_4_top5 = sorted(problem_4_match.items(), key=operator.itemgetter(1), reverse=True)[:5]
    for rank in problem_4_top5:
        print '{:<7}'.format(rank[0]), round(problem_4_match[rank[0]], 6)
    print ""

    ###################################################
