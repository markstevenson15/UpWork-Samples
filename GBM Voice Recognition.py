

import pandas as pd
from sklearn import cross_validation
from sklearn.ensemble import GradientBoostingClassifier

url = "https://raw.githubusercontent.com/primaryobjects/voice-gender/master/voice.csv"

dataframe = pandas.read_csv(url)

array = dataframe.values

array2 = array[2001:3168,:]
array1 = array[0:2000,:]

X = array1[:,0:19]
Y = array1[:,20]

num_folds = 10
num_instances = len(X)
seed = 1
num_trees = 100
kfold = cross_validation.KFold(n=num_instances, n_folds=num_folds, random_state=seed)
model = GradientBoostingClassifier(n_estimators=num_trees, random_state=seed)
results = cross_validation.cross_val_score(model, X, Y, cv=kfold)
print(results.mean())


# In[166]:

model.fit(X, Y)
pred = model.predict(X)



# In[170]:

preds = model.predict(array2[:,0:19])
results2=pd.concat([pd.DataFrame(preds),pd.DataFrame(array[2001:3168,20:])],axis=1)

