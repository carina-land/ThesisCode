##
# Import packages
import os
import pandas as pd
import numpy as np
import json
import cv2
from PIL import Image, ImageStat
import glob
from sklearn.model_selection import train_test_split

##
# Sentiment
# additional idea for Sentiment: visualize entities with word cloud
sentiment_analysis = sorted(glob.glob('/Users/carinaland/Documents/Thesis/05 Data/train_sentiment/*.json'))
print('num of train sentiment files: {}'.format(len(sentiment_analysis)))  # 14,442

# Define Empty lists
score = []
magnitude = []
petid = []
language = []

for filename in sentiment_analysis:
    with open(filename, 'r') as f:
        sentiment_file = json.load(f)
    file_sentiment = sentiment_file['documentSentiment']
    file_score = np.asarray(sentiment_file['documentSentiment']['score'])
    file_magnitude = np.asarray(sentiment_file['documentSentiment']['magnitude'])
    file_language = np.asarray(sentiment_file['language'])

    score.append(file_score)
    magnitude.append(file_magnitude)
    language.append(file_language)

    petid.append(
        filename.replace('.json', '').replace('/Users/carinaland/Documents/Thesis/05 Data/train_sentiment/', ''))

# Output with sentiment data for each pet
sentiment_analysis = pd.concat(
    [pd.DataFrame(petid, columns=['PetID']), pd.DataFrame(score, columns=['sentiment_document_score']),
     pd.DataFrame(magnitude, columns=['sentiment_document_magnitude']),
     pd.DataFrame(language, columns=['sentiment_document_language'])], axis=1)


##
# Image Metadata
# image_metadata = sorted(glob.glob('/Users/carinaland/Documents/Thesis/05 Data/train_metadata/*.json'))
# print('num of train metadata: {}'.format(len(image_metadata)))

# extract Description & Topicality: not useful for prediction - omit,
# but could check compliance with entered breed and size of rescuers
#
# description = []
# topicality = []
# imageid = []
#
# # Read Zip File and Export a Data set with the Score and the ID
# for filename in image_metadata:
#     with open(filename, 'r') as f:
#         d = json.load(f)
#         file_keys = list(
#             d.keys())  # dict_keys(['labelAnnotations', 'imagePropertiesAnnotation', 'cropHintsAnnotation'])
#
#     if 'labelAnnotations' in file_keys:
#         file_annots = d['labelAnnotations']
#         file_topicality = np.asarray([x['topicality'] for x in file_annots])
#         file_description = [x['description'] for x in file_annots]
#         # Create a list of all descriptions and topicality
#         description.append(file_description)
#         topicality.append(file_topicality)
#         # Create a list with all image id name
#         imageid.append(
#             filename.replace('.json', '').replace('/Users/carinaland/Documents/Thesis/05 Data/train_metadata/', ''))
#
# # Prepare the output by renaming all variables
# description = pd.DataFrame(description)
# topicality = pd.DataFrame(topicality)
#
# new_names = [(i, 'metadata_description_' + str(i)) for i in description.iloc[:, 0:].columns.values]
# description.rename(columns=dict(new_names), inplace=True)
#
# new_names = [(i, 'metadata_topicality_' + str(i)) for i in topicality.iloc[:, 0:].columns.values]
# topicality.rename(columns=dict(new_names), inplace=True)
#
# # Output with sentiment data for each pet
# image_labelannot = pd.concat([pd.DataFrame(imageid, columns=['ImageId']), topicality, description], axis=1)
#
# # create the PetId variable
# image_labelannot['PetID'] = image_labelannot['ImageId'].str.split('-').str[0]


# Crop Hints

# im = Image.open('/Users/carinaland/Documents/Thesis/05 Data/train_images_test/0a0e8c15b-1.jpg')
# im = im.crop((43, 1, 299, 303))
# im.show()


##
# Image Quality
def brightness(img):
    """average pixels, then transform to 'perceived brightness'
    im: PIL image file (Image.open(im_file))"""
    stat_mean = ImageStat.Stat(img).mean
    if len(stat_mean) < 3:
        return stat_mean[0]
    else:
        r, g, b = stat_mean
        return np.sqrt(0.299 * (r ** 2) + 0.587 * (g ** 2) + 0.114 * (b ** 2))


# Image quality assessment aims to quantitatively represent the human perception of quality.
# We will add : pixels and  blur score using the variance of Laplacian.
# The following variables are created:
# -  Pixel of profile image for a pet
# -  Pixel average, sum, mind and max for all pictures for a Pet
# -  Blur of profile image for a pet
# -  Blur average, sum, mind and max for all pictures for a Pet

image_quality = sorted(glob.glob('/Users/carinaland/Documents/Thesis/05 Data/train_images/*.jpg'))
print('num of train images: {}'.format(len(image_quality)))  # 58,311

blur = []
image_pixel = []
image_brightness = []
imageid = []

for filename in image_quality:
    # Blur
    image = cv2.imread(filename)
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    gauss = cv2.GaussianBlur(gray,(3,3),0)
    result = cv2.Laplacian(gauss, cv2.CV_64F).var()

    # Pixels
    with Image.open(filename) as im:
        width, height = im.size
        bright = brightness(im)
    pixel = width * height

    # image pixel size for each image
    image_pixel.append(pixel)
    # blur for each image
    blur.append(result)
    # brightness for each image
    image_brightness.append(bright)

    # image id
    imageid.append(filename.replace('.jpg', '').replace(
        '/Users/carinaland/Documents/Thesis/05 Data/train_images/', ''))

# Join Pixel, Blur and Image ID
image_quality = pd.concat([pd.DataFrame(imageid, columns=['ImageId']), pd.DataFrame(blur, columns=['blur']),
                           pd.DataFrame(image_pixel, columns=['pixel']),
                           pd.DataFrame(image_brightness, columns=['brightness'])], axis=1)

# create the PetId variable
image_quality['PetID'] = image_quality['ImageId'].str.split('-').str[0]
image_quality = image_quality[['PetID', 'ImageId', 'blur', 'pixel', 'brightness']]

image_quality['profile'] = image_quality['ImageId'].str.split('-').str[1] == '1'

# Mean, Min, Max, Sum
image_quality['pixel_mean'] = image_quality.groupby(['PetID'])['pixel'].transform('mean')
image_quality['blur_mean'] = image_quality.groupby(['PetID'])['blur'].transform('mean')
image_quality['brightness_mean'] = image_quality.groupby(['PetID'])['brightness'].transform('mean')

image_quality['pixel_min'] = image_quality.groupby(['PetID'])['pixel'].transform('min')
image_quality['blur_min'] = image_quality.groupby(['PetID'])['blur'].transform('min')
image_quality['brightness_min'] = image_quality.groupby(['PetID'])['brightness'].transform('min')

image_quality['pixel_max'] = image_quality.groupby(['PetID'])['pixel'].transform('max')
image_quality['blur_max'] = image_quality.groupby(['PetID'])['blur'].transform('max')
image_quality['brightness_max'] = image_quality.groupby(['PetID'])['brightness'].transform('max')

image_quality = image_quality[image_quality.profile == 1]
image_quality = image_quality.drop(['ImageId', 'profile'], 1)
image_quality.rename({'blur': 'profile_blur', 'pixel': 'profile_pixel', 'brightness': 'profile_brightness'},
                     axis=1, inplace=True)

##
# Load Data
data = pd.read_csv('/Users/carinaland/Documents/Thesis/05 Data/train.csv')

breed = pd.read_csv('/Users/carinaland/Documents/Thesis/05 Data/breed_labels.csv', usecols=["BreedID", "BreedName"])
color = pd.read_csv('/Users/carinaland/Documents/Thesis/05 Data/color_labels.csv')
state = pd.read_csv('/Users/carinaland/Documents/Thesis/05 Data/state_labels.csv')

# Add information about color, breed, state and sentiment data
data = (pd.merge(data, breed.rename(columns={"BreedName": "BreedName1"}), how='left', left_on=['Breed1'],
                 right_on=['BreedID']).drop('BreedID', axis=1))
data = (pd.merge(data, breed.rename(columns={"BreedName": "BreedName2"}), how='left', left_on=['Breed2'],
                 right_on=['BreedID']).drop('BreedID', axis=1))
data = (pd.merge(data, color.rename(columns={"ColorName": "ColorName1"}), how='left', left_on=['Color1'],
                 right_on=['ColorID']).drop('ColorID', axis=1))
data = (pd.merge(data, color.rename(columns={"ColorName": "ColorName2"}), how='left', left_on=['Color2'],
                 right_on=['ColorID']).drop('ColorID', axis=1))
data = (pd.merge(data, color.rename(columns={"ColorName": "ColorName3"}), how='left', left_on=['Color3'],
                 right_on=['ColorID']).drop('ColorID', axis=1))
data = (pd.merge(data, state, how='inner', left_on=['State'], right_on=['StateID']).drop('StateID', axis=1))

# Add information about sentimental analysis
data = (pd.merge(data, sentiment_analysis, how='left', left_on=['PetID'], right_on=['PetID']))

# # Add information about Metadata Images - omitted
# train = (pd.merge(train, image_properties,  how='left', left_on=['PetID'], right_on=['PetID']))
# train = (pd.merge(train, image_labelannot,  how='left', left_on=['PetID'], right_on=['PetID']))
# train = (pd.merge(train, image_moments,  how='left', left_on=['PetID'], right_on=['PetID']))

# Add information about image quality
data = (pd.merge(data, image_quality, how='left', left_on=['PetID'], right_on=['PetID']))
##
data.sentiment_document_language.value_counts(dropna=False)
# explore missing values
language_indices = list(np.where(data['sentiment_document_language'].isna())[0])
list(data.Description.iloc[language_indices[:20]])
# observation: Google Language API didn't recognize and analyze Malay (checked with google translate),
# also mix of languages

# set language to unknown, set score and magnitude to median values of training_set --> see next cell
# data.sentiment_document_language.fillna('unknown', inplace=True)
# set it to zero if no description there --> see next cell

data.profile_blur.isna().sum()  # 341
# fill na values in training set with zero, as no pictures are there

##
# Create New Features
data['Adoption'] = [x in [0, 1, 2, 3] for x in data.AdoptionSpeed]
data['no_name'] = data.Name.isna()
data['no_fee'] = data.Fee == 0
data['no_description'] = data.Description.isna()
data['no_photo'] = data.PhotoAmt == 0
data['pure_breed'] = (data.Breed2 == 0 | (data.Breed1 == data.Breed2)) & (data.Breed1 != 307)
data['vet_count'] = np.where(data.Dewormed == 1, 1, 0) + np.where(data.Sterilized == 1, 1, 0) \
                    + np.where(data.Vaccinated == 1, 1, 0)
data['col_count'] = np.where(data.Color1 == 0, 0, 1) + np.where(data.Color2 == 0, 0, 1) \
                    + np.where(data.Color3 == 0, 0, 1)

##
# fill values of newly created features for profiles without any pictures or descriptions with zeros
data.profile_blur.fillna(0, inplace=True)
data.profile_brightness.fillna(0, inplace=True)
data.profile_pixel.fillna(0, inplace=True)

data.blur_min.fillna(0, inplace=True)
data.blur_max.fillna(0, inplace=True)
data.blur_mean.fillna(0, inplace=True)

data.brightness_min.fillna(0, inplace=True)
data.brightness_max.fillna(0, inplace=True)
data.brightness_mean.fillna(0, inplace=True)

data.pixel_min.fillna(0, inplace=True)
data.pixel_max.fillna(0, inplace=True)
data.pixel_mean.fillna(0, inplace=True)

data.loc[data.Description.isna(), 'sentiment_document_score'] = 0
data.loc[data.Description.isna(), 'sentiment_document_magnitude'] = 0
data.sentiment_document_language.fillna('unknown', inplace=True)

##
# don't use mixed profiles
data = data[data.Quantity == 1]
data = data.drop('Quantity', axis=1)

# Split into train and test
y = data.Adoption
X = data.drop('Adoption', axis=1)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

train_df = pd.concat([X_train, y_train], axis=1)
test_df = pd.concat([X_test, y_test], axis=1)

# replace missing score and magnitude of documents (by median) that couln't be analyzed with sentiment data
train_df.sentiment_document_score = train_df.sentiment_document_score.fillna(train_df.sentiment_document_score.median())
test_df.sentiment_document_score = test_df.sentiment_document_score.fillna(train_df.sentiment_document_score.median())

train_df.sentiment_document_magnitude = \
    train_df.sentiment_document_magnitude.fillna(train_df.sentiment_document_magnitude.median())
test_df.sentiment_document_magnitude = \
    test_df.sentiment_document_magnitude.fillna(train_df.sentiment_document_magnitude.median())

# Feature Rescuer Experience - omitted but defined in another way in R (would have different scales when defining it
# on train and test set separately
# train_rescuer_df = train_df.RescuerID.value_counts().rename_axis('Rescuer').reset_index(name='ProfileCounts')
# train_df = (pd.merge(train_df, train_rescuer_df, how='inner',
#                     left_on=['RescuerID'], right_on=['Rescuer']).drop('Rescuer', axis=1))
train_df.to_excel('/Users/carinaland/Documents/Thesis/05 Data/train_df.xlsx')

# test_rescuer_df = test_df.RescuerID.value_counts().rename_axis('Rescuer').reset_index(name='ProfileCounts')
# test_df = (pd.merge(test_df, test_rescuer_df, how='inner',
#                    left_on=['RescuerID'], right_on=['Rescuer']).drop('Rescuer', axis=1))

# export to excel to be able to use it in R
test_df.to_excel('/Users/carinaland/Documents/Thesis/05 Data/test_df.xlsx')
