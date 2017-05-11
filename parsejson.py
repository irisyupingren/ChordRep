def readJsonFile(file):
    with open(file) as data_file:
    	data = json.load(data_file)
    return data