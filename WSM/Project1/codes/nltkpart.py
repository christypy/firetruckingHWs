import nltk

def nltkProc(doc):
    tokens = nltk.word_tokenize(doc)
    tagged = nltk.pos_tag(tokens)
    # print tagged
    return list(set([pair[0] for pair in tagged if pair[1].find("NN") != -1 or pair[1].find("VB") != -1]))
    # return list(set([pair[0] for pair in tagged if pair[1] == "NN" or pair[1] == "VB"]))

# print nltkProc("Hello, my name name name is Noel, motherfuckers. And I feel this fucking homework is fucked up.")
