from sklearn import svm
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import cross_val_score
from sklearn.ensemble import RandomForestClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.naive_bayes import ComplementNB
from sklearn.naive_bayes import BernoulliNB
from sklearn.neural_network import MLPClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import KNeighborsClassifier
from sklearn.neighbors.nearest_centroid import NearestCentroid
from sklearn.preprocessing import StandardScaler


def mean(value):
    return sum(value) / len(value)


random_state = 42

df = pd.read_csv("cleaned_trials_for_python.csv")
# remove R's row name column
df.drop(df.columns[0], axis=1, inplace=True)

y = df["latinSquare"]
x = df.drop(["latinSquare"], axis=1, inplace=False)
# use stratify so every face type is shaved off evenly
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.2, random_state=42, stratify=y)

"""
classifier = svm.SVC(gamma="scale", decision_function_shape="ovo")


classifier.fit(x_train, y_train)
score_train = classifier.score(x_train, y_train)

score_test = classifier.score(x_test, y_test)

y_pred = classifier.predict(x_test)
labels = np.unique(y)
print(labels)
print(confusion_matrix(y_test, y_pred, labels=labels))
print("SVM: Train: {}, Test: {}".format(score_train, score_test))



scores = cross_val_score(classifier, x, y, cv=15)
print(mean(scores))
"""

classifiers = {
    "SVC": svm.SVC(gamma="scale", decision_function_shape="ovo"),
    "Random Forest": RandomForestClassifier(criterion="entropy", max_depth=6, n_estimators=100, max_features="auto",
                                            random_state=random_state),
    "Gaussian Naive Bayes": GaussianNB(),
    "Complement Naive Bayes": ComplementNB(),
    "Bernoulli Naive Bayes": BernoulliNB(),
    "MLPClassifier (Feed Forward Neural Network)": MLPClassifier(solver="lbfgs", alpha=1e-5,
                                                                 hidden_layer_sizes=(90, 12),
                                                                 random_state=random_state),
    "Logistic Regression": LogisticRegression(random_state=random_state, solver="lbfgs", multi_class="multinomial",
                                              max_iter=300),
    "K(2) Nearest Neighbour": KNeighborsClassifier(n_neighbors=2, algorithm="ball_tree"),
    "K(2) Nearest Neighbour Weighted": KNeighborsClassifier(n_neighbors=2, algorithm="ball_tree", weights="distance"),
    "K(3) Nearest Neighbour": KNeighborsClassifier(n_neighbors=3, algorithm="ball_tree"),
    "K(3) Nearest Neighbour Weighted": KNeighborsClassifier(n_neighbors=3, algorithm="ball_tree", weights="distance"),
    "K(10) Nearest Neighbour": KNeighborsClassifier(n_neighbors=10, algorithm="ball_tree"),
    "K(10) Nearest Neighbour Weighted": KNeighborsClassifier(n_neighbors=10, algorithm="ball_tree", weights="distance"),
    # using 15 cause that's my corss validation k-fold
    "K(15) Nearest Neighbour": KNeighborsClassifier(n_neighbors=15, algorithm="ball_tree"),
    "K(15) Nearest Neighbour Weighted": KNeighborsClassifier(n_neighbors=15, algorithm="ball_tree", weights="distance"),
    "Nearest Centroid": NearestCentroid(),
    # "CRAZY MLPClassifier (Feed Forward Neural Network)": MLPClassifier(solver="lbfgs", alpha=1e-5, hidden_layer_sizes=(9000, 1200), random_state=random_state)

}

for key in classifiers:
    classifier = classifiers[key]
    scores = cross_val_score(classifier, x, y, cv=15)
    mean_result = "{0:.3g}".format(mean(scores))
    print("{}: Mean accuracy: {}".format(key, mean_result))

""""
#results for these don't seem significantly different, so saving processing power here
print("now with transform:")
scaler = StandardScaler()
x = scaler.fit_transform(x)

for key in classifiers:
    classifier = classifiers[key]
    scores = cross_val_score(classifier, x, y, cv=15, error_score=np.nan)
    print("{}: Mean accuracy: {}".format(key, mean(scores)))
"""
