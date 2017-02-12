{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.Product where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Rekognition.Types.Sum

-- | Structure containing the estimated age range, in years, for a face.
--
--
-- Rekognition estimates an age-range for faces detected in the input image. Estimated age ranges can overlap; a face of a 5 year old may have an estimated range of 4-6 whilst the face of a 6 year old may have an estimated range of 4-8.
--
--
-- /See:/ 'ageRange' smart constructor.
data AgeRange = AgeRange'
    { _arLow  :: !(Maybe Nat)
    , _arHigh :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AgeRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arLow' - The lowest estimated age.
--
-- * 'arHigh' - The highest estimated age.
ageRange
    :: AgeRange
ageRange =
    AgeRange'
    { _arLow = Nothing
    , _arHigh = Nothing
    }

-- | The lowest estimated age.
arLow :: Lens' AgeRange (Maybe Natural)
arLow = lens _arLow (\ s a -> s{_arLow = a}) . mapping _Nat;

-- | The highest estimated age.
arHigh :: Lens' AgeRange (Maybe Natural)
arHigh = lens _arHigh (\ s a -> s{_arHigh = a}) . mapping _Nat;

instance FromJSON AgeRange where
        parseJSON
          = withObject "AgeRange"
              (\ x ->
                 AgeRange' <$> (x .:? "Low") <*> (x .:? "High"))

instance Hashable AgeRange

instance NFData AgeRange

-- | Indicates whether or not the face has a beard, and the confidence level in the determination.
--
--
--
-- /See:/ 'beard' smart constructor.
data Beard = Beard'
    { _bValue      :: !(Maybe Bool)
    , _bConfidence :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Beard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bValue' - Boolean value that indicates whether the face has beard or not.
--
-- * 'bConfidence' - Level of confidence in the determination.
beard
    :: Beard
beard =
    Beard'
    { _bValue = Nothing
    , _bConfidence = Nothing
    }

-- | Boolean value that indicates whether the face has beard or not.
bValue :: Lens' Beard (Maybe Bool)
bValue = lens _bValue (\ s a -> s{_bValue = a});

-- | Level of confidence in the determination.
bConfidence :: Lens' Beard (Maybe Double)
bConfidence = lens _bConfidence (\ s a -> s{_bConfidence = a});

instance FromJSON Beard where
        parseJSON
          = withObject "Beard"
              (\ x ->
                 Beard' <$> (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable Beard

instance NFData Beard

-- | Identifies the bounding box around the object or face. The @left@ (x-coordinate) and @top@ (y-coordinate) are coordinates representing the top and left sides of the bounding box. Note that the upper-left corner of the image is the origin (0,0).
--
--
-- The @top@ and @left@ values returned are ratios of the overall image size. For example, if the input image is 700x200 pixels, and the top-left coordinate of the bounding box is 350x50 pixels, the API returns a @left@ value of 0.5 (350/700) and a @top@ value of 0.25 (50/200).
--
-- The @width@ and @height@ values represent the dimensions of the bounding box as a ratio of the overall image dimension. For example, if the input image is 700x200 pixels, and the bounding box width is 70 pixels, the width returned is 0.1.
--
--
-- /See:/ 'boundingBox' smart constructor.
data BoundingBox = BoundingBox'
    { _bbHeight :: !(Maybe Double)
    , _bbLeft   :: !(Maybe Double)
    , _bbWidth  :: !(Maybe Double)
    , _bbTop    :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BoundingBox' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bbHeight' - Height of the bounding box as a ratio of the overall image height.
--
-- * 'bbLeft' - Left coordinate of the bounding box as a ratio of overall image width.
--
-- * 'bbWidth' - Width of the bounding box as a ratio of the overall image width.
--
-- * 'bbTop' - Top coordinate of the bounding box as a ratio of overall image height.
boundingBox
    :: BoundingBox
boundingBox =
    BoundingBox'
    { _bbHeight = Nothing
    , _bbLeft = Nothing
    , _bbWidth = Nothing
    , _bbTop = Nothing
    }

-- | Height of the bounding box as a ratio of the overall image height.
bbHeight :: Lens' BoundingBox (Maybe Double)
bbHeight = lens _bbHeight (\ s a -> s{_bbHeight = a});

-- | Left coordinate of the bounding box as a ratio of overall image width.
bbLeft :: Lens' BoundingBox (Maybe Double)
bbLeft = lens _bbLeft (\ s a -> s{_bbLeft = a});

-- | Width of the bounding box as a ratio of the overall image width.
bbWidth :: Lens' BoundingBox (Maybe Double)
bbWidth = lens _bbWidth (\ s a -> s{_bbWidth = a});

-- | Top coordinate of the bounding box as a ratio of overall image height.
bbTop :: Lens' BoundingBox (Maybe Double)
bbTop = lens _bbTop (\ s a -> s{_bbTop = a});

instance FromJSON BoundingBox where
        parseJSON
          = withObject "BoundingBox"
              (\ x ->
                 BoundingBox' <$>
                   (x .:? "Height") <*> (x .:? "Left") <*>
                     (x .:? "Width")
                     <*> (x .:? "Top"))

instance Hashable BoundingBox

instance NFData BoundingBox

-- | For the provided the bounding box, confidence level that the bounding box actually contains a face, and the similarity between the face in the bounding box and the face in the source image.
--
--
--
-- /See:/ 'compareFacesMatch' smart constructor.
data CompareFacesMatch = CompareFacesMatch'
    { _cfmSimilarity :: !(Maybe Double)
    , _cfmFace       :: !(Maybe ComparedFace)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CompareFacesMatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfmSimilarity' - Level of confidence that the faces match.
--
-- * 'cfmFace' - Provides face metadata (bounding box and confidence that the bounding box actually contains a face).
compareFacesMatch
    :: CompareFacesMatch
compareFacesMatch =
    CompareFacesMatch'
    { _cfmSimilarity = Nothing
    , _cfmFace = Nothing
    }

-- | Level of confidence that the faces match.
cfmSimilarity :: Lens' CompareFacesMatch (Maybe Double)
cfmSimilarity = lens _cfmSimilarity (\ s a -> s{_cfmSimilarity = a});

-- | Provides face metadata (bounding box and confidence that the bounding box actually contains a face).
cfmFace :: Lens' CompareFacesMatch (Maybe ComparedFace)
cfmFace = lens _cfmFace (\ s a -> s{_cfmFace = a});

instance FromJSON CompareFacesMatch where
        parseJSON
          = withObject "CompareFacesMatch"
              (\ x ->
                 CompareFacesMatch' <$>
                   (x .:? "Similarity") <*> (x .:? "Face"))

instance Hashable CompareFacesMatch

instance NFData CompareFacesMatch

-- | Provides face metadata (bounding box and confidence that the bounding box actually contains a face).
--
--
--
-- /See:/ 'comparedFace' smart constructor.
data ComparedFace = ComparedFace'
    { _cfBoundingBox :: !(Maybe BoundingBox)
    , _cfConfidence  :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ComparedFace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfBoundingBox' - Undocumented member.
--
-- * 'cfConfidence' - Level of confidence that what the bounding box contains is a face.
comparedFace
    :: ComparedFace
comparedFace =
    ComparedFace'
    { _cfBoundingBox = Nothing
    , _cfConfidence = Nothing
    }

-- | Undocumented member.
cfBoundingBox :: Lens' ComparedFace (Maybe BoundingBox)
cfBoundingBox = lens _cfBoundingBox (\ s a -> s{_cfBoundingBox = a});

-- | Level of confidence that what the bounding box contains is a face.
cfConfidence :: Lens' ComparedFace (Maybe Double)
cfConfidence = lens _cfConfidence (\ s a -> s{_cfConfidence = a});

instance FromJSON ComparedFace where
        parseJSON
          = withObject "ComparedFace"
              (\ x ->
                 ComparedFace' <$>
                   (x .:? "BoundingBox") <*> (x .:? "Confidence"))

instance Hashable ComparedFace

instance NFData ComparedFace

-- | Type that describes the face Amazon Rekognition chose to compare with the faces in the target. This contains a bounding box for the selected face and confidence level that the bounding box contains a face. Note that Amazon Rekognition selects the largest face in the source image for this comparison.
--
--
--
-- /See:/ 'comparedSourceImageFace' smart constructor.
data ComparedSourceImageFace = ComparedSourceImageFace'
    { _csifBoundingBox :: !(Maybe BoundingBox)
    , _csifConfidence  :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ComparedSourceImageFace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csifBoundingBox' - Undocumented member.
--
-- * 'csifConfidence' - Confidence level that the selected bounding box contains a face.
comparedSourceImageFace
    :: ComparedSourceImageFace
comparedSourceImageFace =
    ComparedSourceImageFace'
    { _csifBoundingBox = Nothing
    , _csifConfidence = Nothing
    }

-- | Undocumented member.
csifBoundingBox :: Lens' ComparedSourceImageFace (Maybe BoundingBox)
csifBoundingBox = lens _csifBoundingBox (\ s a -> s{_csifBoundingBox = a});

-- | Confidence level that the selected bounding box contains a face.
csifConfidence :: Lens' ComparedSourceImageFace (Maybe Double)
csifConfidence = lens _csifConfidence (\ s a -> s{_csifConfidence = a});

instance FromJSON ComparedSourceImageFace where
        parseJSON
          = withObject "ComparedSourceImageFace"
              (\ x ->
                 ComparedSourceImageFace' <$>
                   (x .:? "BoundingBox") <*> (x .:? "Confidence"))

instance Hashable ComparedSourceImageFace

instance NFData ComparedSourceImageFace

-- | The emotions detected on the face, and the confidence level in the determination. For example, HAPPY, SAD, and ANGRY.
--
--
--
-- /See:/ 'emotion' smart constructor.
data Emotion = Emotion'
    { _eConfidence :: !(Maybe Double)
    , _eType       :: !(Maybe EmotionName)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Emotion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eConfidence' - Level of confidence in the determination.
--
-- * 'eType' - Type of emotion detected.
emotion
    :: Emotion
emotion =
    Emotion'
    { _eConfidence = Nothing
    , _eType = Nothing
    }

-- | Level of confidence in the determination.
eConfidence :: Lens' Emotion (Maybe Double)
eConfidence = lens _eConfidence (\ s a -> s{_eConfidence = a});

-- | Type of emotion detected.
eType :: Lens' Emotion (Maybe EmotionName)
eType = lens _eType (\ s a -> s{_eType = a});

instance FromJSON Emotion where
        parseJSON
          = withObject "Emotion"
              (\ x ->
                 Emotion' <$> (x .:? "Confidence") <*> (x .:? "Type"))

instance Hashable Emotion

instance NFData Emotion

-- | Indicates whether or not the eyes on the face are open, and the confidence level in the determination.
--
--
--
-- /See:/ 'eyeOpen' smart constructor.
data EyeOpen = EyeOpen'
    { _eoValue      :: !(Maybe Bool)
    , _eoConfidence :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EyeOpen' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eoValue' - Boolean value that indicates whether the eyes on the face are open.
--
-- * 'eoConfidence' - Level of confidence in the determination.
eyeOpen
    :: EyeOpen
eyeOpen =
    EyeOpen'
    { _eoValue = Nothing
    , _eoConfidence = Nothing
    }

-- | Boolean value that indicates whether the eyes on the face are open.
eoValue :: Lens' EyeOpen (Maybe Bool)
eoValue = lens _eoValue (\ s a -> s{_eoValue = a});

-- | Level of confidence in the determination.
eoConfidence :: Lens' EyeOpen (Maybe Double)
eoConfidence = lens _eoConfidence (\ s a -> s{_eoConfidence = a});

instance FromJSON EyeOpen where
        parseJSON
          = withObject "EyeOpen"
              (\ x ->
                 EyeOpen' <$>
                   (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable EyeOpen

instance NFData EyeOpen

-- | Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.
--
--
--
-- /See:/ 'eyeglasses' smart constructor.
data Eyeglasses = Eyeglasses'
    { _eyeValue      :: !(Maybe Bool)
    , _eyeConfidence :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Eyeglasses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eyeValue' - Boolean value that indicates whether the face is wearing eye glasses or not.
--
-- * 'eyeConfidence' - Level of confidence in the determination.
eyeglasses
    :: Eyeglasses
eyeglasses =
    Eyeglasses'
    { _eyeValue = Nothing
    , _eyeConfidence = Nothing
    }

-- | Boolean value that indicates whether the face is wearing eye glasses or not.
eyeValue :: Lens' Eyeglasses (Maybe Bool)
eyeValue = lens _eyeValue (\ s a -> s{_eyeValue = a});

-- | Level of confidence in the determination.
eyeConfidence :: Lens' Eyeglasses (Maybe Double)
eyeConfidence = lens _eyeConfidence (\ s a -> s{_eyeConfidence = a});

instance FromJSON Eyeglasses where
        parseJSON
          = withObject "Eyeglasses"
              (\ x ->
                 Eyeglasses' <$>
                   (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable Eyeglasses

instance NFData Eyeglasses

-- | Describes the face properties such as the bounding box, face ID, image ID of the source image, and external image ID that you assigned.
--
--
--
-- /See:/ 'face' smart constructor.
data Face = Face'
    { _fFaceId          :: !(Maybe Text)
    , _fBoundingBox     :: !(Maybe BoundingBox)
    , _fExternalImageId :: !(Maybe Text)
    , _fConfidence      :: !(Maybe Double)
    , _fImageId         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Face' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fFaceId' - Unique identifier that Amazon Rekognition assigns to the face.
--
-- * 'fBoundingBox' - Undocumented member.
--
-- * 'fExternalImageId' - Identifier that you assign to all the faces in the input image.
--
-- * 'fConfidence' - Confidence level that the bounding box contains a face (and not a different object such as a tree).
--
-- * 'fImageId' - Unique identifier that Amazon Rekognition assigns to the source image.
face
    :: Face
face =
    Face'
    { _fFaceId = Nothing
    , _fBoundingBox = Nothing
    , _fExternalImageId = Nothing
    , _fConfidence = Nothing
    , _fImageId = Nothing
    }

-- | Unique identifier that Amazon Rekognition assigns to the face.
fFaceId :: Lens' Face (Maybe Text)
fFaceId = lens _fFaceId (\ s a -> s{_fFaceId = a});

-- | Undocumented member.
fBoundingBox :: Lens' Face (Maybe BoundingBox)
fBoundingBox = lens _fBoundingBox (\ s a -> s{_fBoundingBox = a});

-- | Identifier that you assign to all the faces in the input image.
fExternalImageId :: Lens' Face (Maybe Text)
fExternalImageId = lens _fExternalImageId (\ s a -> s{_fExternalImageId = a});

-- | Confidence level that the bounding box contains a face (and not a different object such as a tree).
fConfidence :: Lens' Face (Maybe Double)
fConfidence = lens _fConfidence (\ s a -> s{_fConfidence = a});

-- | Unique identifier that Amazon Rekognition assigns to the source image.
fImageId :: Lens' Face (Maybe Text)
fImageId = lens _fImageId (\ s a -> s{_fImageId = a});

instance FromJSON Face where
        parseJSON
          = withObject "Face"
              (\ x ->
                 Face' <$>
                   (x .:? "FaceId") <*> (x .:? "BoundingBox") <*>
                     (x .:? "ExternalImageId")
                     <*> (x .:? "Confidence")
                     <*> (x .:? "ImageId"))

instance Hashable Face

instance NFData Face

-- | Structure containing attributes of the face that the algorithm detected.
--
--
--
-- /See:/ 'faceDetail' smart constructor.
data FaceDetail = FaceDetail'
    { _fdAgeRange    :: !(Maybe AgeRange)
    , _fdSunglasses  :: !(Maybe Sunglasses)
    , _fdMouthOpen   :: !(Maybe MouthOpen)
    , _fdBoundingBox :: !(Maybe BoundingBox)
    , _fdEmotions    :: !(Maybe [Emotion])
    , _fdEyesOpen    :: !(Maybe EyeOpen)
    , _fdPose        :: !(Maybe Pose)
    , _fdConfidence  :: !(Maybe Double)
    , _fdGender      :: !(Maybe Gender)
    , _fdQuality     :: !(Maybe ImageQuality)
    , _fdEyeglasses  :: !(Maybe Eyeglasses)
    , _fdBeard       :: !(Maybe Beard)
    , _fdMustache    :: !(Maybe Mustache)
    , _fdSmile       :: !(Maybe Smile)
    , _fdLandmarks   :: !(Maybe [Landmark])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FaceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdAgeRange' - The estimated age range, in years, for the face. Low represents the lowest estimated age and High represents the highest estimated age.
--
-- * 'fdSunglasses' - Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.
--
-- * 'fdMouthOpen' - Indicates whether or not the mouth on the face is open, and the confidence level in the determination.
--
-- * 'fdBoundingBox' - Bounding box of the face.
--
-- * 'fdEmotions' - The emotions detected on the face, and the confidence level in the determination. For example, HAPPY, SAD, and ANGRY.
--
-- * 'fdEyesOpen' - Indicates whether or not the eyes on the face are open, and the confidence level in the determination.
--
-- * 'fdPose' - Indicates the pose of the face as determined by pitch, roll, and the yaw.
--
-- * 'fdConfidence' - Confidence level that the bounding box contains a face (and not a different object such as a tree).
--
-- * 'fdGender' - Gender of the face and the confidence level in the determination.
--
-- * 'fdQuality' - Identifies image brightness and sharpness.
--
-- * 'fdEyeglasses' - Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.
--
-- * 'fdBeard' - Indicates whether or not the face has a beard, and the confidence level in the determination.
--
-- * 'fdMustache' - Indicates whether or not the face has a mustache, and the confidence level in the determination.
--
-- * 'fdSmile' - Indicates whether or not the face is smiling, and the confidence level in the determination.
--
-- * 'fdLandmarks' - Indicates the location of the landmark on the face.
faceDetail
    :: FaceDetail
faceDetail =
    FaceDetail'
    { _fdAgeRange = Nothing
    , _fdSunglasses = Nothing
    , _fdMouthOpen = Nothing
    , _fdBoundingBox = Nothing
    , _fdEmotions = Nothing
    , _fdEyesOpen = Nothing
    , _fdPose = Nothing
    , _fdConfidence = Nothing
    , _fdGender = Nothing
    , _fdQuality = Nothing
    , _fdEyeglasses = Nothing
    , _fdBeard = Nothing
    , _fdMustache = Nothing
    , _fdSmile = Nothing
    , _fdLandmarks = Nothing
    }

-- | The estimated age range, in years, for the face. Low represents the lowest estimated age and High represents the highest estimated age.
fdAgeRange :: Lens' FaceDetail (Maybe AgeRange)
fdAgeRange = lens _fdAgeRange (\ s a -> s{_fdAgeRange = a});

-- | Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.
fdSunglasses :: Lens' FaceDetail (Maybe Sunglasses)
fdSunglasses = lens _fdSunglasses (\ s a -> s{_fdSunglasses = a});

-- | Indicates whether or not the mouth on the face is open, and the confidence level in the determination.
fdMouthOpen :: Lens' FaceDetail (Maybe MouthOpen)
fdMouthOpen = lens _fdMouthOpen (\ s a -> s{_fdMouthOpen = a});

-- | Bounding box of the face.
fdBoundingBox :: Lens' FaceDetail (Maybe BoundingBox)
fdBoundingBox = lens _fdBoundingBox (\ s a -> s{_fdBoundingBox = a});

-- | The emotions detected on the face, and the confidence level in the determination. For example, HAPPY, SAD, and ANGRY.
fdEmotions :: Lens' FaceDetail [Emotion]
fdEmotions = lens _fdEmotions (\ s a -> s{_fdEmotions = a}) . _Default . _Coerce;

-- | Indicates whether or not the eyes on the face are open, and the confidence level in the determination.
fdEyesOpen :: Lens' FaceDetail (Maybe EyeOpen)
fdEyesOpen = lens _fdEyesOpen (\ s a -> s{_fdEyesOpen = a});

-- | Indicates the pose of the face as determined by pitch, roll, and the yaw.
fdPose :: Lens' FaceDetail (Maybe Pose)
fdPose = lens _fdPose (\ s a -> s{_fdPose = a});

-- | Confidence level that the bounding box contains a face (and not a different object such as a tree).
fdConfidence :: Lens' FaceDetail (Maybe Double)
fdConfidence = lens _fdConfidence (\ s a -> s{_fdConfidence = a});

-- | Gender of the face and the confidence level in the determination.
fdGender :: Lens' FaceDetail (Maybe Gender)
fdGender = lens _fdGender (\ s a -> s{_fdGender = a});

-- | Identifies image brightness and sharpness.
fdQuality :: Lens' FaceDetail (Maybe ImageQuality)
fdQuality = lens _fdQuality (\ s a -> s{_fdQuality = a});

-- | Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.
fdEyeglasses :: Lens' FaceDetail (Maybe Eyeglasses)
fdEyeglasses = lens _fdEyeglasses (\ s a -> s{_fdEyeglasses = a});

-- | Indicates whether or not the face has a beard, and the confidence level in the determination.
fdBeard :: Lens' FaceDetail (Maybe Beard)
fdBeard = lens _fdBeard (\ s a -> s{_fdBeard = a});

-- | Indicates whether or not the face has a mustache, and the confidence level in the determination.
fdMustache :: Lens' FaceDetail (Maybe Mustache)
fdMustache = lens _fdMustache (\ s a -> s{_fdMustache = a});

-- | Indicates whether or not the face is smiling, and the confidence level in the determination.
fdSmile :: Lens' FaceDetail (Maybe Smile)
fdSmile = lens _fdSmile (\ s a -> s{_fdSmile = a});

-- | Indicates the location of the landmark on the face.
fdLandmarks :: Lens' FaceDetail [Landmark]
fdLandmarks = lens _fdLandmarks (\ s a -> s{_fdLandmarks = a}) . _Default . _Coerce;

instance FromJSON FaceDetail where
        parseJSON
          = withObject "FaceDetail"
              (\ x ->
                 FaceDetail' <$>
                   (x .:? "AgeRange") <*> (x .:? "Sunglasses") <*>
                     (x .:? "MouthOpen")
                     <*> (x .:? "BoundingBox")
                     <*> (x .:? "Emotions" .!= mempty)
                     <*> (x .:? "EyesOpen")
                     <*> (x .:? "Pose")
                     <*> (x .:? "Confidence")
                     <*> (x .:? "Gender")
                     <*> (x .:? "Quality")
                     <*> (x .:? "Eyeglasses")
                     <*> (x .:? "Beard")
                     <*> (x .:? "Mustache")
                     <*> (x .:? "Smile")
                     <*> (x .:? "Landmarks" .!= mempty))

instance Hashable FaceDetail

instance NFData FaceDetail

-- | Provides face metadata. In addition, it also provides the confidence in the match of this face with the input face.
--
--
--
-- /See:/ 'faceMatch' smart constructor.
data FaceMatch = FaceMatch'
    { _fmSimilarity :: !(Maybe Double)
    , _fmFace       :: !(Maybe Face)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FaceMatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fmSimilarity' - Confidence in the match of this face with the input face.
--
-- * 'fmFace' - Undocumented member.
faceMatch
    :: FaceMatch
faceMatch =
    FaceMatch'
    { _fmSimilarity = Nothing
    , _fmFace = Nothing
    }

-- | Confidence in the match of this face with the input face.
fmSimilarity :: Lens' FaceMatch (Maybe Double)
fmSimilarity = lens _fmSimilarity (\ s a -> s{_fmSimilarity = a});

-- | Undocumented member.
fmFace :: Lens' FaceMatch (Maybe Face)
fmFace = lens _fmFace (\ s a -> s{_fmFace = a});

instance FromJSON FaceMatch where
        parseJSON
          = withObject "FaceMatch"
              (\ x ->
                 FaceMatch' <$>
                   (x .:? "Similarity") <*> (x .:? "Face"))

instance Hashable FaceMatch

instance NFData FaceMatch

-- | Object containing both the face metadata (stored in the back-end database) and facial attributes that are detected but aren't stored in the database.
--
--
--
-- /See:/ 'faceRecord' smart constructor.
data FaceRecord = FaceRecord'
    { _frFaceDetail :: !(Maybe FaceDetail)
    , _frFace       :: !(Maybe Face)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FaceRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frFaceDetail' - Undocumented member.
--
-- * 'frFace' - Undocumented member.
faceRecord
    :: FaceRecord
faceRecord =
    FaceRecord'
    { _frFaceDetail = Nothing
    , _frFace = Nothing
    }

-- | Undocumented member.
frFaceDetail :: Lens' FaceRecord (Maybe FaceDetail)
frFaceDetail = lens _frFaceDetail (\ s a -> s{_frFaceDetail = a});

-- | Undocumented member.
frFace :: Lens' FaceRecord (Maybe Face)
frFace = lens _frFace (\ s a -> s{_frFace = a});

instance FromJSON FaceRecord where
        parseJSON
          = withObject "FaceRecord"
              (\ x ->
                 FaceRecord' <$>
                   (x .:? "FaceDetail") <*> (x .:? "Face"))

instance Hashable FaceRecord

instance NFData FaceRecord

-- | Gender of the face and the confidence level in the determination.
--
--
--
-- /See:/ 'gender' smart constructor.
data Gender = Gender'
    { _gValue      :: !(Maybe GenderType)
    , _gConfidence :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Gender' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gValue' - Gender of the face.
--
-- * 'gConfidence' - Level of confidence in the determination.
gender
    :: Gender
gender =
    Gender'
    { _gValue = Nothing
    , _gConfidence = Nothing
    }

-- | Gender of the face.
gValue :: Lens' Gender (Maybe GenderType)
gValue = lens _gValue (\ s a -> s{_gValue = a});

-- | Level of confidence in the determination.
gConfidence :: Lens' Gender (Maybe Double)
gConfidence = lens _gConfidence (\ s a -> s{_gConfidence = a});

instance FromJSON Gender where
        parseJSON
          = withObject "Gender"
              (\ x ->
                 Gender' <$> (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable Gender

instance NFData Gender

-- | Provides the source image either as bytes or an S3 object.
--
--
-- The region for the S3 bucket containing the S3 object must match the region you use for Amazon Rekognition operations.
--
-- You may need to Base64-encode the image bytes depending on the language you are using and whether or not you are using the AWS SDK. For more information, see 'example4' .
--
-- If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes using the Bytes property is not supported. You must first upload the image to an Amazon S3 bucket and then call the operation using the S3Object property.
--
-- For Amazon Rekognition to process an S3 object, the user must have permission to access the S3 object. For more information, see 'manage-access-resource-policies' .
--
--
-- /See:/ 'image' smart constructor.
data Image = Image'
    { _iS3Object :: !(Maybe S3Object)
    , _iBytes    :: !(Maybe Base64)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iS3Object' - Identifies an S3 object as the image source.
--
-- * 'iBytes' - Blob of image bytes up to 5 MBs.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
image
    :: Image
image =
    Image'
    { _iS3Object = Nothing
    , _iBytes = Nothing
    }

-- | Identifies an S3 object as the image source.
iS3Object :: Lens' Image (Maybe S3Object)
iS3Object = lens _iS3Object (\ s a -> s{_iS3Object = a});

-- | Blob of image bytes up to 5 MBs.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
iBytes :: Lens' Image (Maybe ByteString)
iBytes = lens _iBytes (\ s a -> s{_iBytes = a}) . mapping _Base64;

instance Hashable Image

instance NFData Image

instance ToJSON Image where
        toJSON Image'{..}
          = object
              (catMaybes
                 [("S3Object" .=) <$> _iS3Object,
                  ("Bytes" .=) <$> _iBytes])

-- | Identifies image brightness and sharpness.
--
--
--
-- /See:/ 'imageQuality' smart constructor.
data ImageQuality = ImageQuality'
    { _iqSharpness  :: !(Maybe Double)
    , _iqBrightness :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImageQuality' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iqSharpness' - Value representing sharpness of the face.
--
-- * 'iqBrightness' - Value representing brightness of the face. The service returns a value between 0 and 1 (inclusive).
imageQuality
    :: ImageQuality
imageQuality =
    ImageQuality'
    { _iqSharpness = Nothing
    , _iqBrightness = Nothing
    }

-- | Value representing sharpness of the face.
iqSharpness :: Lens' ImageQuality (Maybe Double)
iqSharpness = lens _iqSharpness (\ s a -> s{_iqSharpness = a});

-- | Value representing brightness of the face. The service returns a value between 0 and 1 (inclusive).
iqBrightness :: Lens' ImageQuality (Maybe Double)
iqBrightness = lens _iqBrightness (\ s a -> s{_iqBrightness = a});

instance FromJSON ImageQuality where
        parseJSON
          = withObject "ImageQuality"
              (\ x ->
                 ImageQuality' <$>
                   (x .:? "Sharpness") <*> (x .:? "Brightness"))

instance Hashable ImageQuality

instance NFData ImageQuality

-- | Structure containing details about the detected label, including name, and level of confidence.
--
--
--
-- /See:/ 'label' smart constructor.
data Label = Label'
    { _lConfidence :: !(Maybe Double)
    , _lName       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Label' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lConfidence' - Level of confidence.
--
-- * 'lName' - The name (label) of the object.
label
    :: Label
label =
    Label'
    { _lConfidence = Nothing
    , _lName = Nothing
    }

-- | Level of confidence.
lConfidence :: Lens' Label (Maybe Double)
lConfidence = lens _lConfidence (\ s a -> s{_lConfidence = a});

-- | The name (label) of the object.
lName :: Lens' Label (Maybe Text)
lName = lens _lName (\ s a -> s{_lName = a});

instance FromJSON Label where
        parseJSON
          = withObject "Label"
              (\ x ->
                 Label' <$> (x .:? "Confidence") <*> (x .:? "Name"))

instance Hashable Label

instance NFData Label

-- | Indicates the location of the landmark on the face.
--
--
--
-- /See:/ 'landmark' smart constructor.
data Landmark = Landmark'
    { _lType :: !(Maybe LandmarkType)
    , _lX    :: !(Maybe Double)
    , _lY    :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Landmark' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lType' - Type of the landmark.
--
-- * 'lX' - x-coordinate from the top left of the landmark expressed as the ration of the width of the image. For example, if the images is 700x200 and the x-coordinate of the landmark is at 350 pixels, this value is 0.5.
--
-- * 'lY' - y-coordinate from the top left of the landmark expressed as the ration of the height of the image. For example, if the images is 700x200 and the y-coordinate of the landmark is at 100 pixels, this value is 0.5.
landmark
    :: Landmark
landmark =
    Landmark'
    { _lType = Nothing
    , _lX = Nothing
    , _lY = Nothing
    }

-- | Type of the landmark.
lType :: Lens' Landmark (Maybe LandmarkType)
lType = lens _lType (\ s a -> s{_lType = a});

-- | x-coordinate from the top left of the landmark expressed as the ration of the width of the image. For example, if the images is 700x200 and the x-coordinate of the landmark is at 350 pixels, this value is 0.5.
lX :: Lens' Landmark (Maybe Double)
lX = lens _lX (\ s a -> s{_lX = a});

-- | y-coordinate from the top left of the landmark expressed as the ration of the height of the image. For example, if the images is 700x200 and the y-coordinate of the landmark is at 100 pixels, this value is 0.5.
lY :: Lens' Landmark (Maybe Double)
lY = lens _lY (\ s a -> s{_lY = a});

instance FromJSON Landmark where
        parseJSON
          = withObject "Landmark"
              (\ x ->
                 Landmark' <$>
                   (x .:? "Type") <*> (x .:? "X") <*> (x .:? "Y"))

instance Hashable Landmark

instance NFData Landmark

-- | Indicates whether or not the mouth on the face is open, and the confidence level in the determination.
--
--
--
-- /See:/ 'mouthOpen' smart constructor.
data MouthOpen = MouthOpen'
    { _moValue      :: !(Maybe Bool)
    , _moConfidence :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MouthOpen' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'moValue' - Boolean value that indicates whether the mouth on the face is open or not.
--
-- * 'moConfidence' - Level of confidence in the determination.
mouthOpen
    :: MouthOpen
mouthOpen =
    MouthOpen'
    { _moValue = Nothing
    , _moConfidence = Nothing
    }

-- | Boolean value that indicates whether the mouth on the face is open or not.
moValue :: Lens' MouthOpen (Maybe Bool)
moValue = lens _moValue (\ s a -> s{_moValue = a});

-- | Level of confidence in the determination.
moConfidence :: Lens' MouthOpen (Maybe Double)
moConfidence = lens _moConfidence (\ s a -> s{_moConfidence = a});

instance FromJSON MouthOpen where
        parseJSON
          = withObject "MouthOpen"
              (\ x ->
                 MouthOpen' <$>
                   (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable MouthOpen

instance NFData MouthOpen

-- | Indicates whether or not the face has a mustache, and the confidence level in the determination.
--
--
--
-- /See:/ 'mustache' smart constructor.
data Mustache = Mustache'
    { _mValue      :: !(Maybe Bool)
    , _mConfidence :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Mustache' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mValue' - Boolean value that indicates whether the face has mustache or not.
--
-- * 'mConfidence' - Level of confidence in the determination.
mustache
    :: Mustache
mustache =
    Mustache'
    { _mValue = Nothing
    , _mConfidence = Nothing
    }

-- | Boolean value that indicates whether the face has mustache or not.
mValue :: Lens' Mustache (Maybe Bool)
mValue = lens _mValue (\ s a -> s{_mValue = a});

-- | Level of confidence in the determination.
mConfidence :: Lens' Mustache (Maybe Double)
mConfidence = lens _mConfidence (\ s a -> s{_mConfidence = a});

instance FromJSON Mustache where
        parseJSON
          = withObject "Mustache"
              (\ x ->
                 Mustache' <$>
                   (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable Mustache

instance NFData Mustache

-- | Indicates the pose of the face as determined by pitch, roll, and the yaw.
--
--
--
-- /See:/ 'pose' smart constructor.
data Pose = Pose'
    { _pYaw   :: !(Maybe Double)
    , _pRoll  :: !(Maybe Double)
    , _pPitch :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Pose' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pYaw' - Value representing the face rotation on the yaw axis.
--
-- * 'pRoll' - Value representing the face rotation on the roll axis.
--
-- * 'pPitch' - Value representing the face rotation on the pitch axis.
pose
    :: Pose
pose =
    Pose'
    { _pYaw = Nothing
    , _pRoll = Nothing
    , _pPitch = Nothing
    }

-- | Value representing the face rotation on the yaw axis.
pYaw :: Lens' Pose (Maybe Double)
pYaw = lens _pYaw (\ s a -> s{_pYaw = a});

-- | Value representing the face rotation on the roll axis.
pRoll :: Lens' Pose (Maybe Double)
pRoll = lens _pRoll (\ s a -> s{_pRoll = a});

-- | Value representing the face rotation on the pitch axis.
pPitch :: Lens' Pose (Maybe Double)
pPitch = lens _pPitch (\ s a -> s{_pPitch = a});

instance FromJSON Pose where
        parseJSON
          = withObject "Pose"
              (\ x ->
                 Pose' <$>
                   (x .:? "Yaw") <*> (x .:? "Roll") <*> (x .:? "Pitch"))

instance Hashable Pose

instance NFData Pose

-- | Provides the S3 bucket name and object name.
--
--
-- The region for the S3 bucket containing the S3 object must match the region you use for Amazon Rekognition operations.
--
-- For Amazon Rekognition to process an S3 object, the user must have permission to access the S3 object. For more information, see 'manage-access-resource-policies' .
--
--
-- /See:/ 's3Object' smart constructor.
data S3Object = S3Object'
    { _soBucket  :: !(Maybe Text)
    , _soName    :: !(Maybe Text)
    , _soVersion :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3Object' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'soBucket' - Name of the S3 bucket.
--
-- * 'soName' - S3 object key name.
--
-- * 'soVersion' - If the bucket is versioning enabled, you can specify the object version.
s3Object
    :: S3Object
s3Object =
    S3Object'
    { _soBucket = Nothing
    , _soName = Nothing
    , _soVersion = Nothing
    }

-- | Name of the S3 bucket.
soBucket :: Lens' S3Object (Maybe Text)
soBucket = lens _soBucket (\ s a -> s{_soBucket = a});

-- | S3 object key name.
soName :: Lens' S3Object (Maybe Text)
soName = lens _soName (\ s a -> s{_soName = a});

-- | If the bucket is versioning enabled, you can specify the object version.
soVersion :: Lens' S3Object (Maybe Text)
soVersion = lens _soVersion (\ s a -> s{_soVersion = a});

instance Hashable S3Object

instance NFData S3Object

instance ToJSON S3Object where
        toJSON S3Object'{..}
          = object
              (catMaybes
                 [("Bucket" .=) <$> _soBucket,
                  ("Name" .=) <$> _soName,
                  ("Version" .=) <$> _soVersion])

-- | Indicates whether or not the face is smiling, and the confidence level in the determination.
--
--
--
-- /See:/ 'smile' smart constructor.
data Smile = Smile'
    { _smiValue      :: !(Maybe Bool)
    , _smiConfidence :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Smile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smiValue' - Boolean value that indicates whether the face is smiling or not.
--
-- * 'smiConfidence' - Level of confidence in the determination.
smile
    :: Smile
smile =
    Smile'
    { _smiValue = Nothing
    , _smiConfidence = Nothing
    }

-- | Boolean value that indicates whether the face is smiling or not.
smiValue :: Lens' Smile (Maybe Bool)
smiValue = lens _smiValue (\ s a -> s{_smiValue = a});

-- | Level of confidence in the determination.
smiConfidence :: Lens' Smile (Maybe Double)
smiConfidence = lens _smiConfidence (\ s a -> s{_smiConfidence = a});

instance FromJSON Smile where
        parseJSON
          = withObject "Smile"
              (\ x ->
                 Smile' <$> (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable Smile

instance NFData Smile

-- | Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.
--
--
--
-- /See:/ 'sunglasses' smart constructor.
data Sunglasses = Sunglasses'
    { _sValue      :: !(Maybe Bool)
    , _sConfidence :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Sunglasses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sValue' - Boolean value that indicates whether the face is wearing sunglasses or not.
--
-- * 'sConfidence' - Level of confidence in the determination.
sunglasses
    :: Sunglasses
sunglasses =
    Sunglasses'
    { _sValue = Nothing
    , _sConfidence = Nothing
    }

-- | Boolean value that indicates whether the face is wearing sunglasses or not.
sValue :: Lens' Sunglasses (Maybe Bool)
sValue = lens _sValue (\ s a -> s{_sValue = a});

-- | Level of confidence in the determination.
sConfidence :: Lens' Sunglasses (Maybe Double)
sConfidence = lens _sConfidence (\ s a -> s{_sConfidence = a});

instance FromJSON Sunglasses where
        parseJSON
          = withObject "Sunglasses"
              (\ x ->
                 Sunglasses' <$>
                   (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable Sunglasses

instance NFData Sunglasses
