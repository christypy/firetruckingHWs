import sys

#http://www.scipy.org/
try:
	from numpy import dot
	from numpy.linalg import norm
except:
	print "Error: Requires numpy from http://www.scipy.org/. Have you installed scipy?"
	sys.exit() 

def removeDuplicates(list):
	""" remove duplicates from a list """
	return set((item for item in list))


def cosine(vector1, vector2):
	""" related documents j and q are in the concept space by comparing the vectors :
		cosine  = ( V1 * V2 ) / ||V1|| x ||V2|| """
	return float(dot(vector1,vector2) / (norm(vector1) * norm(vector2)))

def jaccard(vector1, vector2):
	orr = 0
	andd = 0
	zipped = zip(vector1,vector2)
	for (i, j) in zipped:
		if(i > 0 and j > 0):
			andd += 1
		if(i > 0 or j > 0):
			orr += 1
	return float(andd) / float(orr)
