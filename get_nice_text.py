def get_nice_text(filename = "Complete_text2.txt"):
    
    import codecs
    import os
    import string
    import numpy as np 
    
    with codecs.open("Complete_data2.txt", 'r', encoding='utf-8', errors='ignore') as fdata:
        data = fdata.readlines()
        
    data = np.array(data)
    data = data[1::2]
    
    for i in range(len(data)):
        
        chapter = data[i]
        
        # remove special characters
        chapter = chapter.translate ({ord(c): " " for c in "!@#$%^&*()[]{};:,./<>?\|`~-=_+\""})
        
        # remove digits
        chapter = ''.join([i for i in chapter if not i.isdigit()])  
        
        # remove first and last characters
        chapter = chapter[2:-2]
        
        data[i] = chapter
    
    
    return data