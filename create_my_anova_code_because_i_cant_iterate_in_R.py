features = ["faceGender", "faceStyle", "skinDetails", "skinColor", "hairColor", "eyeColor", "eyeShape",
                "eyeOpening", "eyeSize", "eyeHeight", "eyeDistance", "eyeDepth", "eyeRotation", "eyebrowsColor",
                "eyebrowsShape", "eyebrowsLine", "noseShape", "noseLength", "noseWidth", "noseBridge", "noseCartilage",
                "foreheadHeight", "cheeksBone", "jawShape", "jawChin", "jawLength", "throatSize", "earSize",
                "mouthVolume", "lipRatio", "mouthOverlap", "mouthWidth", "mouthHeight", "mouthDepth", "eyeShadow",
                "lipStick", "rouge"]

print(len(features))
for feature in features:
    print("cat(\"-----------------------------------------------------------\")")
    print("cat(\"Test for: {}\")".format(feature))
    print("summary(aov({} ~ latinSquare + Error(userUID), df))".format(feature))
    print("pairwise.t.test(df${}, df$latinSquare, p.adj= \"bonf\")".format(feature))
print("cat(\"-----------------------------------------------------------\")")