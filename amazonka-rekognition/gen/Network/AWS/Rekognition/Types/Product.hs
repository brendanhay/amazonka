{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.Sum

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AgeRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arLow' - The lowest estimated age.
--
-- * 'arHigh' - The highest estimated age.
ageRange
    :: AgeRange
ageRange = AgeRange' {_arLow = Nothing, _arHigh = Nothing}


-- | The lowest estimated age.
arLow :: Lens' AgeRange (Maybe Natural)
arLow = lens _arLow (\ s a -> s{_arLow = a}) . mapping _Nat

-- | The highest estimated age.
arHigh :: Lens' AgeRange (Maybe Natural)
arHigh = lens _arHigh (\ s a -> s{_arHigh = a}) . mapping _Nat

instance FromJSON AgeRange where
        parseJSON
          = withObject "AgeRange"
              (\ x ->
                 AgeRange' <$> (x .:? "Low") <*> (x .:? "High"))

instance Hashable AgeRange where

instance NFData AgeRange where

-- | Indicates whether or not the face has a beard, and the confidence level in the determination.
--
--
--
-- /See:/ 'beard' smart constructor.
data Beard = Beard'
  { _bValue      :: !(Maybe Bool)
  , _bConfidence :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Beard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bValue' - Boolean value that indicates whether the face has beard or not.
--
-- * 'bConfidence' - Level of confidence in the determination.
beard
    :: Beard
beard = Beard' {_bValue = Nothing, _bConfidence = Nothing}


-- | Boolean value that indicates whether the face has beard or not.
bValue :: Lens' Beard (Maybe Bool)
bValue = lens _bValue (\ s a -> s{_bValue = a})

-- | Level of confidence in the determination.
bConfidence :: Lens' Beard (Maybe Double)
bConfidence = lens _bConfidence (\ s a -> s{_bConfidence = a})

instance FromJSON Beard where
        parseJSON
          = withObject "Beard"
              (\ x ->
                 Beard' <$> (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable Beard where

instance NFData Beard where

-- | Identifies the bounding box around the object, face or text. The @left@ (x-coordinate) and @top@ (y-coordinate) are coordinates representing the top and left sides of the bounding box. Note that the upper-left corner of the image is the origin (0,0).
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
bbHeight = lens _bbHeight (\ s a -> s{_bbHeight = a})

-- | Left coordinate of the bounding box as a ratio of overall image width.
bbLeft :: Lens' BoundingBox (Maybe Double)
bbLeft = lens _bbLeft (\ s a -> s{_bbLeft = a})

-- | Width of the bounding box as a ratio of the overall image width.
bbWidth :: Lens' BoundingBox (Maybe Double)
bbWidth = lens _bbWidth (\ s a -> s{_bbWidth = a})

-- | Top coordinate of the bounding box as a ratio of overall image height.
bbTop :: Lens' BoundingBox (Maybe Double)
bbTop = lens _bbTop (\ s a -> s{_bbTop = a})

instance FromJSON BoundingBox where
        parseJSON
          = withObject "BoundingBox"
              (\ x ->
                 BoundingBox' <$>
                   (x .:? "Height") <*> (x .:? "Left") <*>
                     (x .:? "Width")
                     <*> (x .:? "Top"))

instance Hashable BoundingBox where

instance NFData BoundingBox where

-- | Provides information about a celebrity recognized by the operation.
--
--
--
-- /See:/ 'celebrity' smart constructor.
data Celebrity = Celebrity'
  { _cMatchConfidence :: !(Maybe Double)
  , _cURLs            :: !(Maybe [Text])
  , _cName            :: !(Maybe Text)
  , _cId              :: !(Maybe Text)
  , _cFace            :: !(Maybe ComparedFace)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Celebrity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cMatchConfidence' - The confidence, in percentage, that Rekognition has that the recognized face is the celebrity.
--
-- * 'cURLs' - An array of URLs pointing to additional information about the celebrity. If there is no additional information about the celebrity, this list is empty.
--
-- * 'cName' - The name of the celebrity.
--
-- * 'cId' - A unique identifier for the celebrity.
--
-- * 'cFace' - Provides information about the celebrity's face, such as its location on the image.
celebrity
    :: Celebrity
celebrity =
  Celebrity'
    { _cMatchConfidence = Nothing
    , _cURLs = Nothing
    , _cName = Nothing
    , _cId = Nothing
    , _cFace = Nothing
    }


-- | The confidence, in percentage, that Rekognition has that the recognized face is the celebrity.
cMatchConfidence :: Lens' Celebrity (Maybe Double)
cMatchConfidence = lens _cMatchConfidence (\ s a -> s{_cMatchConfidence = a})

-- | An array of URLs pointing to additional information about the celebrity. If there is no additional information about the celebrity, this list is empty.
cURLs :: Lens' Celebrity [Text]
cURLs = lens _cURLs (\ s a -> s{_cURLs = a}) . _Default . _Coerce

-- | The name of the celebrity.
cName :: Lens' Celebrity (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

-- | A unique identifier for the celebrity.
cId :: Lens' Celebrity (Maybe Text)
cId = lens _cId (\ s a -> s{_cId = a})

-- | Provides information about the celebrity's face, such as its location on the image.
cFace :: Lens' Celebrity (Maybe ComparedFace)
cFace = lens _cFace (\ s a -> s{_cFace = a})

instance FromJSON Celebrity where
        parseJSON
          = withObject "Celebrity"
              (\ x ->
                 Celebrity' <$>
                   (x .:? "MatchConfidence") <*>
                     (x .:? "Urls" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "Face"))

instance Hashable Celebrity where

instance NFData Celebrity where

-- | Information about a recognized celebrity.
--
--
--
-- /See:/ 'celebrityDetail' smart constructor.
data CelebrityDetail = CelebrityDetail'
  { _cdBoundingBox :: !(Maybe BoundingBox)
  , _cdURLs        :: !(Maybe [Text])
  , _cdConfidence  :: !(Maybe Double)
  , _cdName        :: !(Maybe Text)
  , _cdId          :: !(Maybe Text)
  , _cdFace        :: !(Maybe FaceDetail)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CelebrityDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdBoundingBox' - Bounding box around the body of a celebrity.
--
-- * 'cdURLs' - An array of URLs pointing to additional celebrity information.
--
-- * 'cdConfidence' - The confidence, in percentage, that Amazon Rekognition has that the recognized face is the celebrity.
--
-- * 'cdName' - The name of the celebrity.
--
-- * 'cdId' - The unique identifier for the celebrity.
--
-- * 'cdFace' - Face details for the recognized celebrity.
celebrityDetail
    :: CelebrityDetail
celebrityDetail =
  CelebrityDetail'
    { _cdBoundingBox = Nothing
    , _cdURLs = Nothing
    , _cdConfidence = Nothing
    , _cdName = Nothing
    , _cdId = Nothing
    , _cdFace = Nothing
    }


-- | Bounding box around the body of a celebrity.
cdBoundingBox :: Lens' CelebrityDetail (Maybe BoundingBox)
cdBoundingBox = lens _cdBoundingBox (\ s a -> s{_cdBoundingBox = a})

-- | An array of URLs pointing to additional celebrity information.
cdURLs :: Lens' CelebrityDetail [Text]
cdURLs = lens _cdURLs (\ s a -> s{_cdURLs = a}) . _Default . _Coerce

-- | The confidence, in percentage, that Amazon Rekognition has that the recognized face is the celebrity.
cdConfidence :: Lens' CelebrityDetail (Maybe Double)
cdConfidence = lens _cdConfidence (\ s a -> s{_cdConfidence = a})

-- | The name of the celebrity.
cdName :: Lens' CelebrityDetail (Maybe Text)
cdName = lens _cdName (\ s a -> s{_cdName = a})

-- | The unique identifier for the celebrity.
cdId :: Lens' CelebrityDetail (Maybe Text)
cdId = lens _cdId (\ s a -> s{_cdId = a})

-- | Face details for the recognized celebrity.
cdFace :: Lens' CelebrityDetail (Maybe FaceDetail)
cdFace = lens _cdFace (\ s a -> s{_cdFace = a})

instance FromJSON CelebrityDetail where
        parseJSON
          = withObject "CelebrityDetail"
              (\ x ->
                 CelebrityDetail' <$>
                   (x .:? "BoundingBox") <*> (x .:? "Urls" .!= mempty)
                     <*> (x .:? "Confidence")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "Face"))

instance Hashable CelebrityDetail where

instance NFData CelebrityDetail where

-- | Information about a detected celebrity and the time the celebrity was detected in a stored video. For more information, see .
--
--
--
-- /See:/ 'celebrityRecognition' smart constructor.
data CelebrityRecognition = CelebrityRecognition'
  { _crCelebrity :: !(Maybe CelebrityDetail)
  , _crTimestamp :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CelebrityRecognition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crCelebrity' - Information about a recognized celebrity.
--
-- * 'crTimestamp' - The time, in milliseconds from the start of the video, that the celebrity was recognized.
celebrityRecognition
    :: CelebrityRecognition
celebrityRecognition =
  CelebrityRecognition' {_crCelebrity = Nothing, _crTimestamp = Nothing}


-- | Information about a recognized celebrity.
crCelebrity :: Lens' CelebrityRecognition (Maybe CelebrityDetail)
crCelebrity = lens _crCelebrity (\ s a -> s{_crCelebrity = a})

-- | The time, in milliseconds from the start of the video, that the celebrity was recognized.
crTimestamp :: Lens' CelebrityRecognition (Maybe Integer)
crTimestamp = lens _crTimestamp (\ s a -> s{_crTimestamp = a})

instance FromJSON CelebrityRecognition where
        parseJSON
          = withObject "CelebrityRecognition"
              (\ x ->
                 CelebrityRecognition' <$>
                   (x .:? "Celebrity") <*> (x .:? "Timestamp"))

instance Hashable CelebrityRecognition where

instance NFData CelebrityRecognition where

-- | Provides information about a face in a target image that matches the source image face analysed by @CompareFaces@ . The @Face@ property contains the bounding box of the face in the target image. The @Similarity@ property is the confidence that the source image face matches the face in the bounding box.
--
--
--
-- /See:/ 'compareFacesMatch' smart constructor.
data CompareFacesMatch = CompareFacesMatch'
  { _cfmSimilarity :: !(Maybe Double)
  , _cfmFace       :: !(Maybe ComparedFace)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
  CompareFacesMatch' {_cfmSimilarity = Nothing, _cfmFace = Nothing}


-- | Level of confidence that the faces match.
cfmSimilarity :: Lens' CompareFacesMatch (Maybe Double)
cfmSimilarity = lens _cfmSimilarity (\ s a -> s{_cfmSimilarity = a})

-- | Provides face metadata (bounding box and confidence that the bounding box actually contains a face).
cfmFace :: Lens' CompareFacesMatch (Maybe ComparedFace)
cfmFace = lens _cfmFace (\ s a -> s{_cfmFace = a})

instance FromJSON CompareFacesMatch where
        parseJSON
          = withObject "CompareFacesMatch"
              (\ x ->
                 CompareFacesMatch' <$>
                   (x .:? "Similarity") <*> (x .:? "Face"))

instance Hashable CompareFacesMatch where

instance NFData CompareFacesMatch where

-- | Provides face metadata for target image faces that are analysed by @CompareFaces@ and @RecognizeCelebrities@ .
--
--
--
-- /See:/ 'comparedFace' smart constructor.
data ComparedFace = ComparedFace'
  { _cfBoundingBox :: !(Maybe BoundingBox)
  , _cfPose        :: !(Maybe Pose)
  , _cfConfidence  :: !(Maybe Double)
  , _cfQuality     :: !(Maybe ImageQuality)
  , _cfLandmarks   :: !(Maybe [Landmark])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ComparedFace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfBoundingBox' - Bounding box of the face.
--
-- * 'cfPose' - Indicates the pose of the face as determined by its pitch, roll, and yaw.
--
-- * 'cfConfidence' - Level of confidence that what the bounding box contains is a face.
--
-- * 'cfQuality' - Identifies face image brightness and sharpness.
--
-- * 'cfLandmarks' - An array of facial landmarks.
comparedFace
    :: ComparedFace
comparedFace =
  ComparedFace'
    { _cfBoundingBox = Nothing
    , _cfPose = Nothing
    , _cfConfidence = Nothing
    , _cfQuality = Nothing
    , _cfLandmarks = Nothing
    }


-- | Bounding box of the face.
cfBoundingBox :: Lens' ComparedFace (Maybe BoundingBox)
cfBoundingBox = lens _cfBoundingBox (\ s a -> s{_cfBoundingBox = a})

-- | Indicates the pose of the face as determined by its pitch, roll, and yaw.
cfPose :: Lens' ComparedFace (Maybe Pose)
cfPose = lens _cfPose (\ s a -> s{_cfPose = a})

-- | Level of confidence that what the bounding box contains is a face.
cfConfidence :: Lens' ComparedFace (Maybe Double)
cfConfidence = lens _cfConfidence (\ s a -> s{_cfConfidence = a})

-- | Identifies face image brightness and sharpness.
cfQuality :: Lens' ComparedFace (Maybe ImageQuality)
cfQuality = lens _cfQuality (\ s a -> s{_cfQuality = a})

-- | An array of facial landmarks.
cfLandmarks :: Lens' ComparedFace [Landmark]
cfLandmarks = lens _cfLandmarks (\ s a -> s{_cfLandmarks = a}) . _Default . _Coerce

instance FromJSON ComparedFace where
        parseJSON
          = withObject "ComparedFace"
              (\ x ->
                 ComparedFace' <$>
                   (x .:? "BoundingBox") <*> (x .:? "Pose") <*>
                     (x .:? "Confidence")
                     <*> (x .:? "Quality")
                     <*> (x .:? "Landmarks" .!= mempty))

instance Hashable ComparedFace where

instance NFData ComparedFace where

-- | Type that describes the face Amazon Rekognition chose to compare with the faces in the target. This contains a bounding box for the selected face and confidence level that the bounding box contains a face. Note that Amazon Rekognition selects the largest face in the source image for this comparison.
--
--
--
-- /See:/ 'comparedSourceImageFace' smart constructor.
data ComparedSourceImageFace = ComparedSourceImageFace'
  { _csifBoundingBox :: !(Maybe BoundingBox)
  , _csifConfidence  :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ComparedSourceImageFace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csifBoundingBox' - Bounding box of the face.
--
-- * 'csifConfidence' - Confidence level that the selected bounding box contains a face.
comparedSourceImageFace
    :: ComparedSourceImageFace
comparedSourceImageFace =
  ComparedSourceImageFace'
    {_csifBoundingBox = Nothing, _csifConfidence = Nothing}


-- | Bounding box of the face.
csifBoundingBox :: Lens' ComparedSourceImageFace (Maybe BoundingBox)
csifBoundingBox = lens _csifBoundingBox (\ s a -> s{_csifBoundingBox = a})

-- | Confidence level that the selected bounding box contains a face.
csifConfidence :: Lens' ComparedSourceImageFace (Maybe Double)
csifConfidence = lens _csifConfidence (\ s a -> s{_csifConfidence = a})

instance FromJSON ComparedSourceImageFace where
        parseJSON
          = withObject "ComparedSourceImageFace"
              (\ x ->
                 ComparedSourceImageFace' <$>
                   (x .:? "BoundingBox") <*> (x .:? "Confidence"))

instance Hashable ComparedSourceImageFace where

instance NFData ComparedSourceImageFace where

-- | Information about a moderation label detection in a stored video.
--
--
--
-- /See:/ 'contentModerationDetection' smart constructor.
data ContentModerationDetection = ContentModerationDetection'
  { _cmdModerationLabel :: !(Maybe ModerationLabel)
  , _cmdTimestamp       :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContentModerationDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmdModerationLabel' - The moderation label detected by in the stored video.
--
-- * 'cmdTimestamp' - Time, in milliseconds from the beginning of the video, that the moderation label was detected.
contentModerationDetection
    :: ContentModerationDetection
contentModerationDetection =
  ContentModerationDetection'
    {_cmdModerationLabel = Nothing, _cmdTimestamp = Nothing}


-- | The moderation label detected by in the stored video.
cmdModerationLabel :: Lens' ContentModerationDetection (Maybe ModerationLabel)
cmdModerationLabel = lens _cmdModerationLabel (\ s a -> s{_cmdModerationLabel = a})

-- | Time, in milliseconds from the beginning of the video, that the moderation label was detected.
cmdTimestamp :: Lens' ContentModerationDetection (Maybe Integer)
cmdTimestamp = lens _cmdTimestamp (\ s a -> s{_cmdTimestamp = a})

instance FromJSON ContentModerationDetection where
        parseJSON
          = withObject "ContentModerationDetection"
              (\ x ->
                 ContentModerationDetection' <$>
                   (x .:? "ModerationLabel") <*> (x .:? "Timestamp"))

instance Hashable ContentModerationDetection where

instance NFData ContentModerationDetection where

-- | The emotions detected on the face, and the confidence level in the determination. For example, HAPPY, SAD, and ANGRY.
--
--
--
-- /See:/ 'emotion' smart constructor.
data Emotion = Emotion'
  { _eConfidence :: !(Maybe Double)
  , _eType       :: !(Maybe EmotionName)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Emotion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eConfidence' - Level of confidence in the determination.
--
-- * 'eType' - Type of emotion detected.
emotion
    :: Emotion
emotion = Emotion' {_eConfidence = Nothing, _eType = Nothing}


-- | Level of confidence in the determination.
eConfidence :: Lens' Emotion (Maybe Double)
eConfidence = lens _eConfidence (\ s a -> s{_eConfidence = a})

-- | Type of emotion detected.
eType :: Lens' Emotion (Maybe EmotionName)
eType = lens _eType (\ s a -> s{_eType = a})

instance FromJSON Emotion where
        parseJSON
          = withObject "Emotion"
              (\ x ->
                 Emotion' <$> (x .:? "Confidence") <*> (x .:? "Type"))

instance Hashable Emotion where

instance NFData Emotion where

-- | Indicates whether or not the eyes on the face are open, and the confidence level in the determination.
--
--
--
-- /See:/ 'eyeOpen' smart constructor.
data EyeOpen = EyeOpen'
  { _eoValue      :: !(Maybe Bool)
  , _eoConfidence :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EyeOpen' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eoValue' - Boolean value that indicates whether the eyes on the face are open.
--
-- * 'eoConfidence' - Level of confidence in the determination.
eyeOpen
    :: EyeOpen
eyeOpen = EyeOpen' {_eoValue = Nothing, _eoConfidence = Nothing}


-- | Boolean value that indicates whether the eyes on the face are open.
eoValue :: Lens' EyeOpen (Maybe Bool)
eoValue = lens _eoValue (\ s a -> s{_eoValue = a})

-- | Level of confidence in the determination.
eoConfidence :: Lens' EyeOpen (Maybe Double)
eoConfidence = lens _eoConfidence (\ s a -> s{_eoConfidence = a})

instance FromJSON EyeOpen where
        parseJSON
          = withObject "EyeOpen"
              (\ x ->
                 EyeOpen' <$>
                   (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable EyeOpen where

instance NFData EyeOpen where

-- | Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.
--
--
--
-- /See:/ 'eyeglasses' smart constructor.
data Eyeglasses = Eyeglasses'
  { _eyeValue      :: !(Maybe Bool)
  , _eyeConfidence :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Eyeglasses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eyeValue' - Boolean value that indicates whether the face is wearing eye glasses or not.
--
-- * 'eyeConfidence' - Level of confidence in the determination.
eyeglasses
    :: Eyeglasses
eyeglasses = Eyeglasses' {_eyeValue = Nothing, _eyeConfidence = Nothing}


-- | Boolean value that indicates whether the face is wearing eye glasses or not.
eyeValue :: Lens' Eyeglasses (Maybe Bool)
eyeValue = lens _eyeValue (\ s a -> s{_eyeValue = a})

-- | Level of confidence in the determination.
eyeConfidence :: Lens' Eyeglasses (Maybe Double)
eyeConfidence = lens _eyeConfidence (\ s a -> s{_eyeConfidence = a})

instance FromJSON Eyeglasses where
        parseJSON
          = withObject "Eyeglasses"
              (\ x ->
                 Eyeglasses' <$>
                   (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable Eyeglasses where

instance NFData Eyeglasses where

-- | Describes the face properties such as the bounding box, face ID, image ID of the input image, and external image ID that you assigned.
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Face' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fFaceId' - Unique identifier that Amazon Rekognition assigns to the face.
--
-- * 'fBoundingBox' - Bounding box of the face.
--
-- * 'fExternalImageId' - Identifier that you assign to all the faces in the input image.
--
-- * 'fConfidence' - Confidence level that the bounding box contains a face (and not a different object such as a tree).
--
-- * 'fImageId' - Unique identifier that Amazon Rekognition assigns to the input image.
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
fFaceId = lens _fFaceId (\ s a -> s{_fFaceId = a})

-- | Bounding box of the face.
fBoundingBox :: Lens' Face (Maybe BoundingBox)
fBoundingBox = lens _fBoundingBox (\ s a -> s{_fBoundingBox = a})

-- | Identifier that you assign to all the faces in the input image.
fExternalImageId :: Lens' Face (Maybe Text)
fExternalImageId = lens _fExternalImageId (\ s a -> s{_fExternalImageId = a})

-- | Confidence level that the bounding box contains a face (and not a different object such as a tree).
fConfidence :: Lens' Face (Maybe Double)
fConfidence = lens _fConfidence (\ s a -> s{_fConfidence = a})

-- | Unique identifier that Amazon Rekognition assigns to the input image.
fImageId :: Lens' Face (Maybe Text)
fImageId = lens _fImageId (\ s a -> s{_fImageId = a})

instance FromJSON Face where
        parseJSON
          = withObject "Face"
              (\ x ->
                 Face' <$>
                   (x .:? "FaceId") <*> (x .:? "BoundingBox") <*>
                     (x .:? "ExternalImageId")
                     <*> (x .:? "Confidence")
                     <*> (x .:? "ImageId"))

instance Hashable Face where

instance NFData Face where

-- | Structure containing attributes of the face that the algorithm detected.
--
--
-- A @FaceDetail@ object contains either the default facial attributes or all facial attributes. The default attributes are @BoundingBox@ , @Confidence@ , @Landmarks@ , @Pose@ , and @Quality@ .
--
-- is the only Rekognition Video stored video operation that can return a @FaceDetail@ object with all attributes. To specify which attributes to return, use the @FaceAttributes@ input parameter for . The following Rekognition Video operations return only the default attributes. The corresponding Start operations don't have a @FaceAttributes@ input parameter.
--
--     * GetCelebrityRecognition
--
--     * GetPersonTracking
--
--     * GetFaceSearch
--
--
--
-- The Rekognition Image and operations can return all facial attributes. To specify which attributes to return, use the @Attributes@ input parameter for @DetectFaces@ . For @IndexFaces@ , use the @DetectAttributes@ input parameter.
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
-- * 'fdBoundingBox' - Bounding box of the face. Default attribute.
--
-- * 'fdEmotions' - The emotions detected on the face, and the confidence level in the determination. For example, HAPPY, SAD, and ANGRY.
--
-- * 'fdEyesOpen' - Indicates whether or not the eyes on the face are open, and the confidence level in the determination.
--
-- * 'fdPose' - Indicates the pose of the face as determined by its pitch, roll, and yaw. Default attribute.
--
-- * 'fdConfidence' - Confidence level that the bounding box contains a face (and not a different object such as a tree). Default attribute.
--
-- * 'fdGender' - Gender of the face and the confidence level in the determination.
--
-- * 'fdQuality' - Identifies image brightness and sharpness. Default attribute.
--
-- * 'fdEyeglasses' - Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.
--
-- * 'fdBeard' - Indicates whether or not the face has a beard, and the confidence level in the determination.
--
-- * 'fdMustache' - Indicates whether or not the face has a mustache, and the confidence level in the determination.
--
-- * 'fdSmile' - Indicates whether or not the face is smiling, and the confidence level in the determination.
--
-- * 'fdLandmarks' - Indicates the location of landmarks on the face. Default attribute.
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
fdAgeRange = lens _fdAgeRange (\ s a -> s{_fdAgeRange = a})

-- | Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.
fdSunglasses :: Lens' FaceDetail (Maybe Sunglasses)
fdSunglasses = lens _fdSunglasses (\ s a -> s{_fdSunglasses = a})

-- | Indicates whether or not the mouth on the face is open, and the confidence level in the determination.
fdMouthOpen :: Lens' FaceDetail (Maybe MouthOpen)
fdMouthOpen = lens _fdMouthOpen (\ s a -> s{_fdMouthOpen = a})

-- | Bounding box of the face. Default attribute.
fdBoundingBox :: Lens' FaceDetail (Maybe BoundingBox)
fdBoundingBox = lens _fdBoundingBox (\ s a -> s{_fdBoundingBox = a})

-- | The emotions detected on the face, and the confidence level in the determination. For example, HAPPY, SAD, and ANGRY.
fdEmotions :: Lens' FaceDetail [Emotion]
fdEmotions = lens _fdEmotions (\ s a -> s{_fdEmotions = a}) . _Default . _Coerce

-- | Indicates whether or not the eyes on the face are open, and the confidence level in the determination.
fdEyesOpen :: Lens' FaceDetail (Maybe EyeOpen)
fdEyesOpen = lens _fdEyesOpen (\ s a -> s{_fdEyesOpen = a})

-- | Indicates the pose of the face as determined by its pitch, roll, and yaw. Default attribute.
fdPose :: Lens' FaceDetail (Maybe Pose)
fdPose = lens _fdPose (\ s a -> s{_fdPose = a})

-- | Confidence level that the bounding box contains a face (and not a different object such as a tree). Default attribute.
fdConfidence :: Lens' FaceDetail (Maybe Double)
fdConfidence = lens _fdConfidence (\ s a -> s{_fdConfidence = a})

-- | Gender of the face and the confidence level in the determination.
fdGender :: Lens' FaceDetail (Maybe Gender)
fdGender = lens _fdGender (\ s a -> s{_fdGender = a})

-- | Identifies image brightness and sharpness. Default attribute.
fdQuality :: Lens' FaceDetail (Maybe ImageQuality)
fdQuality = lens _fdQuality (\ s a -> s{_fdQuality = a})

-- | Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.
fdEyeglasses :: Lens' FaceDetail (Maybe Eyeglasses)
fdEyeglasses = lens _fdEyeglasses (\ s a -> s{_fdEyeglasses = a})

-- | Indicates whether or not the face has a beard, and the confidence level in the determination.
fdBeard :: Lens' FaceDetail (Maybe Beard)
fdBeard = lens _fdBeard (\ s a -> s{_fdBeard = a})

-- | Indicates whether or not the face has a mustache, and the confidence level in the determination.
fdMustache :: Lens' FaceDetail (Maybe Mustache)
fdMustache = lens _fdMustache (\ s a -> s{_fdMustache = a})

-- | Indicates whether or not the face is smiling, and the confidence level in the determination.
fdSmile :: Lens' FaceDetail (Maybe Smile)
fdSmile = lens _fdSmile (\ s a -> s{_fdSmile = a})

-- | Indicates the location of landmarks on the face. Default attribute.
fdLandmarks :: Lens' FaceDetail [Landmark]
fdLandmarks = lens _fdLandmarks (\ s a -> s{_fdLandmarks = a}) . _Default . _Coerce

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

instance Hashable FaceDetail where

instance NFData FaceDetail where

-- | Information about a face detected in a video analysis request and the time the face was detected in the video.
--
--
--
-- /See:/ 'faceDetection' smart constructor.
data FaceDetection = FaceDetection'
  { _fdTimestamp :: !(Maybe Integer)
  , _fdFace      :: !(Maybe FaceDetail)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FaceDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdTimestamp' - Time, in milliseconds from the start of the video, that the face was detected.
--
-- * 'fdFace' - The face properties for the detected face.
faceDetection
    :: FaceDetection
faceDetection = FaceDetection' {_fdTimestamp = Nothing, _fdFace = Nothing}


-- | Time, in milliseconds from the start of the video, that the face was detected.
fdTimestamp :: Lens' FaceDetection (Maybe Integer)
fdTimestamp = lens _fdTimestamp (\ s a -> s{_fdTimestamp = a})

-- | The face properties for the detected face.
fdFace :: Lens' FaceDetection (Maybe FaceDetail)
fdFace = lens _fdFace (\ s a -> s{_fdFace = a})

instance FromJSON FaceDetection where
        parseJSON
          = withObject "FaceDetection"
              (\ x ->
                 FaceDetection' <$>
                   (x .:? "Timestamp") <*> (x .:? "Face"))

instance Hashable FaceDetection where

instance NFData FaceDetection where

-- | Provides face metadata. In addition, it also provides the confidence in the match of this face with the input face.
--
--
--
-- /See:/ 'faceMatch' smart constructor.
data FaceMatch = FaceMatch'
  { _fmSimilarity :: !(Maybe Double)
  , _fmFace       :: !(Maybe Face)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FaceMatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fmSimilarity' - Confidence in the match of this face with the input face.
--
-- * 'fmFace' - Describes the face properties such as the bounding box, face ID, image ID of the source image, and external image ID that you assigned.
faceMatch
    :: FaceMatch
faceMatch = FaceMatch' {_fmSimilarity = Nothing, _fmFace = Nothing}


-- | Confidence in the match of this face with the input face.
fmSimilarity :: Lens' FaceMatch (Maybe Double)
fmSimilarity = lens _fmSimilarity (\ s a -> s{_fmSimilarity = a})

-- | Describes the face properties such as the bounding box, face ID, image ID of the source image, and external image ID that you assigned.
fmFace :: Lens' FaceMatch (Maybe Face)
fmFace = lens _fmFace (\ s a -> s{_fmFace = a})

instance FromJSON FaceMatch where
        parseJSON
          = withObject "FaceMatch"
              (\ x ->
                 FaceMatch' <$>
                   (x .:? "Similarity") <*> (x .:? "Face"))

instance Hashable FaceMatch where

instance NFData FaceMatch where

-- | Object containing both the face metadata (stored in the back-end database) and facial attributes that are detected but aren't stored in the database.
--
--
--
-- /See:/ 'faceRecord' smart constructor.
data FaceRecord = FaceRecord'
  { _frFaceDetail :: !(Maybe FaceDetail)
  , _frFace       :: !(Maybe Face)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FaceRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frFaceDetail' - Structure containing attributes of the face that the algorithm detected.
--
-- * 'frFace' - Describes the face properties such as the bounding box, face ID, image ID of the input image, and external image ID that you assigned.
faceRecord
    :: FaceRecord
faceRecord = FaceRecord' {_frFaceDetail = Nothing, _frFace = Nothing}


-- | Structure containing attributes of the face that the algorithm detected.
frFaceDetail :: Lens' FaceRecord (Maybe FaceDetail)
frFaceDetail = lens _frFaceDetail (\ s a -> s{_frFaceDetail = a})

-- | Describes the face properties such as the bounding box, face ID, image ID of the input image, and external image ID that you assigned.
frFace :: Lens' FaceRecord (Maybe Face)
frFace = lens _frFace (\ s a -> s{_frFace = a})

instance FromJSON FaceRecord where
        parseJSON
          = withObject "FaceRecord"
              (\ x ->
                 FaceRecord' <$>
                   (x .:? "FaceDetail") <*> (x .:? "Face"))

instance Hashable FaceRecord where

instance NFData FaceRecord where

-- | Input face recognition parameters for an Amazon Rekognition stream processor. @FaceRecognitionSettings@ is a request parameter for .
--
--
--
-- /See:/ 'faceSearchSettings' smart constructor.
data FaceSearchSettings = FaceSearchSettings'
  { _fssFaceMatchThreshold :: !(Maybe Double)
  , _fssCollectionId       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FaceSearchSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fssFaceMatchThreshold' - Minimum face match confidence score that must be met to return a result for a recognized face. Default is 70. 0 is the lowest confidence. 100 is the highest confidence.
--
-- * 'fssCollectionId' - The ID of a collection that contains faces that you want to search for.
faceSearchSettings
    :: FaceSearchSettings
faceSearchSettings =
  FaceSearchSettings'
    {_fssFaceMatchThreshold = Nothing, _fssCollectionId = Nothing}


-- | Minimum face match confidence score that must be met to return a result for a recognized face. Default is 70. 0 is the lowest confidence. 100 is the highest confidence.
fssFaceMatchThreshold :: Lens' FaceSearchSettings (Maybe Double)
fssFaceMatchThreshold = lens _fssFaceMatchThreshold (\ s a -> s{_fssFaceMatchThreshold = a})

-- | The ID of a collection that contains faces that you want to search for.
fssCollectionId :: Lens' FaceSearchSettings (Maybe Text)
fssCollectionId = lens _fssCollectionId (\ s a -> s{_fssCollectionId = a})

instance FromJSON FaceSearchSettings where
        parseJSON
          = withObject "FaceSearchSettings"
              (\ x ->
                 FaceSearchSettings' <$>
                   (x .:? "FaceMatchThreshold") <*>
                     (x .:? "CollectionId"))

instance Hashable FaceSearchSettings where

instance NFData FaceSearchSettings where

instance ToJSON FaceSearchSettings where
        toJSON FaceSearchSettings'{..}
          = object
              (catMaybes
                 [("FaceMatchThreshold" .=) <$>
                    _fssFaceMatchThreshold,
                  ("CollectionId" .=) <$> _fssCollectionId])

-- | Gender of the face and the confidence level in the determination.
--
--
--
-- /See:/ 'gender' smart constructor.
data Gender = Gender'
  { _gValue      :: !(Maybe GenderType)
  , _gConfidence :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Gender' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gValue' - Gender of the face.
--
-- * 'gConfidence' - Level of confidence in the determination.
gender
    :: Gender
gender = Gender' {_gValue = Nothing, _gConfidence = Nothing}


-- | Gender of the face.
gValue :: Lens' Gender (Maybe GenderType)
gValue = lens _gValue (\ s a -> s{_gValue = a})

-- | Level of confidence in the determination.
gConfidence :: Lens' Gender (Maybe Double)
gConfidence = lens _gConfidence (\ s a -> s{_gConfidence = a})

instance FromJSON Gender where
        parseJSON
          = withObject "Gender"
              (\ x ->
                 Gender' <$> (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable Gender where

instance NFData Gender where

-- | Information about where text detected by is located on an image.
--
--
--
-- /See:/ 'geometry' smart constructor.
data Geometry = Geometry'
  { _gBoundingBox :: !(Maybe BoundingBox)
  , _gPolygon     :: !(Maybe [Point])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Geometry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gBoundingBox' - An axis-aligned coarse representation of the detected text's location on the image.
--
-- * 'gPolygon' - Within the bounding box, a fine-grained polygon around the detected text.
geometry
    :: Geometry
geometry = Geometry' {_gBoundingBox = Nothing, _gPolygon = Nothing}


-- | An axis-aligned coarse representation of the detected text's location on the image.
gBoundingBox :: Lens' Geometry (Maybe BoundingBox)
gBoundingBox = lens _gBoundingBox (\ s a -> s{_gBoundingBox = a})

-- | Within the bounding box, a fine-grained polygon around the detected text.
gPolygon :: Lens' Geometry [Point]
gPolygon = lens _gPolygon (\ s a -> s{_gPolygon = a}) . _Default . _Coerce

instance FromJSON Geometry where
        parseJSON
          = withObject "Geometry"
              (\ x ->
                 Geometry' <$>
                   (x .:? "BoundingBox") <*>
                     (x .:? "Polygon" .!= mempty))

instance Hashable Geometry where

instance NFData Geometry where

-- | Provides the input image either as bytes or an S3 object.
--
--
-- You pass image bytes to a Rekognition API operation by using the @Bytes@ property. For example, you would use the @Bytes@ property to pass an image loaded from a local file system. Image bytes passed by using the @Bytes@ property must be base64-encoded. Your code may not need to encode image bytes if you are using an AWS SDK to call Rekognition API operations. For more information, see 'images-bytes' .
--
-- You pass images stored in an S3 bucket to a Rekognition API operation by using the @S3Object@ property. Images stored in an S3 bucket do not need to be base64-encoded.
--
-- The region for the S3 bucket containing the S3 object must match the region you use for Amazon Rekognition operations.
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iS3Object' - Identifies an S3 object as the image source.
--
-- * 'iBytes' - Blob of image bytes up to 5 MBs.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
image
    :: Image
image = Image' {_iS3Object = Nothing, _iBytes = Nothing}


-- | Identifies an S3 object as the image source.
iS3Object :: Lens' Image (Maybe S3Object)
iS3Object = lens _iS3Object (\ s a -> s{_iS3Object = a})

-- | Blob of image bytes up to 5 MBs.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
iBytes :: Lens' Image (Maybe ByteString)
iBytes = lens _iBytes (\ s a -> s{_iBytes = a}) . mapping _Base64

instance Hashable Image where

instance NFData Image where

instance ToJSON Image where
        toJSON Image'{..}
          = object
              (catMaybes
                 [("S3Object" .=) <$> _iS3Object,
                  ("Bytes" .=) <$> _iBytes])

-- | Identifies face image brightness and sharpness.
--
--
--
-- /See:/ 'imageQuality' smart constructor.
data ImageQuality = ImageQuality'
  { _iqSharpness  :: !(Maybe Double)
  , _iqBrightness :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImageQuality' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iqSharpness' - Value representing sharpness of the face. The service returns a value between 0 and 100 (inclusive). A higher value indicates a sharper face image.
--
-- * 'iqBrightness' - Value representing brightness of the face. The service returns a value between 0 and 100 (inclusive). A higher value indicates a brighter face image.
imageQuality
    :: ImageQuality
imageQuality = ImageQuality' {_iqSharpness = Nothing, _iqBrightness = Nothing}


-- | Value representing sharpness of the face. The service returns a value between 0 and 100 (inclusive). A higher value indicates a sharper face image.
iqSharpness :: Lens' ImageQuality (Maybe Double)
iqSharpness = lens _iqSharpness (\ s a -> s{_iqSharpness = a})

-- | Value representing brightness of the face. The service returns a value between 0 and 100 (inclusive). A higher value indicates a brighter face image.
iqBrightness :: Lens' ImageQuality (Maybe Double)
iqBrightness = lens _iqBrightness (\ s a -> s{_iqBrightness = a})

instance FromJSON ImageQuality where
        parseJSON
          = withObject "ImageQuality"
              (\ x ->
                 ImageQuality' <$>
                   (x .:? "Sharpness") <*> (x .:? "Brightness"))

instance Hashable ImageQuality where

instance NFData ImageQuality where

-- | The Kinesis data stream Amazon Rekognition to which the analysis results of a Amazon Rekognition stream processor are streamed. For more information, see .
--
--
--
-- /See:/ 'kinesisDataStream' smart constructor.
newtype KinesisDataStream = KinesisDataStream'
  { _kdsARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisDataStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kdsARN' - ARN of the output Amazon Kinesis Data Streams stream.
kinesisDataStream
    :: KinesisDataStream
kinesisDataStream = KinesisDataStream' {_kdsARN = Nothing}


-- | ARN of the output Amazon Kinesis Data Streams stream.
kdsARN :: Lens' KinesisDataStream (Maybe Text)
kdsARN = lens _kdsARN (\ s a -> s{_kdsARN = a})

instance FromJSON KinesisDataStream where
        parseJSON
          = withObject "KinesisDataStream"
              (\ x -> KinesisDataStream' <$> (x .:? "Arn"))

instance Hashable KinesisDataStream where

instance NFData KinesisDataStream where

instance ToJSON KinesisDataStream where
        toJSON KinesisDataStream'{..}
          = object (catMaybes [("Arn" .=) <$> _kdsARN])

-- | Kinesis video stream stream that provides the source streaming video for a Rekognition Video stream processor. For more information, see .
--
--
--
-- /See:/ 'kinesisVideoStream' smart constructor.
newtype KinesisVideoStream = KinesisVideoStream'
  { _kvsARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisVideoStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kvsARN' - ARN of the Kinesis video stream stream that streams the source video.
kinesisVideoStream
    :: KinesisVideoStream
kinesisVideoStream = KinesisVideoStream' {_kvsARN = Nothing}


-- | ARN of the Kinesis video stream stream that streams the source video.
kvsARN :: Lens' KinesisVideoStream (Maybe Text)
kvsARN = lens _kvsARN (\ s a -> s{_kvsARN = a})

instance FromJSON KinesisVideoStream where
        parseJSON
          = withObject "KinesisVideoStream"
              (\ x -> KinesisVideoStream' <$> (x .:? "Arn"))

instance Hashable KinesisVideoStream where

instance NFData KinesisVideoStream where

instance ToJSON KinesisVideoStream where
        toJSON KinesisVideoStream'{..}
          = object (catMaybes [("Arn" .=) <$> _kvsARN])

-- | Structure containing details about the detected label, including name, and level of confidence.
--
--
--
-- /See:/ 'label' smart constructor.
data Label = Label'
  { _lConfidence :: !(Maybe Double)
  , _lName       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Label' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lConfidence' - Level of confidence.
--
-- * 'lName' - The name (label) of the object.
label
    :: Label
label = Label' {_lConfidence = Nothing, _lName = Nothing}


-- | Level of confidence.
lConfidence :: Lens' Label (Maybe Double)
lConfidence = lens _lConfidence (\ s a -> s{_lConfidence = a})

-- | The name (label) of the object.
lName :: Lens' Label (Maybe Text)
lName = lens _lName (\ s a -> s{_lName = a})

instance FromJSON Label where
        parseJSON
          = withObject "Label"
              (\ x ->
                 Label' <$> (x .:? "Confidence") <*> (x .:? "Name"))

instance Hashable Label where

instance NFData Label where

-- | Information about a label detected in a video analysis request and the time the label was detected in the video.
--
--
--
-- /See:/ 'labelDetection' smart constructor.
data LabelDetection = LabelDetection'
  { _ldLabel     :: !(Maybe Label)
  , _ldTimestamp :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldLabel' - Details about the detected label.
--
-- * 'ldTimestamp' - Time, in milliseconds from the start of the video, that the label was detected.
labelDetection
    :: LabelDetection
labelDetection = LabelDetection' {_ldLabel = Nothing, _ldTimestamp = Nothing}


-- | Details about the detected label.
ldLabel :: Lens' LabelDetection (Maybe Label)
ldLabel = lens _ldLabel (\ s a -> s{_ldLabel = a})

-- | Time, in milliseconds from the start of the video, that the label was detected.
ldTimestamp :: Lens' LabelDetection (Maybe Integer)
ldTimestamp = lens _ldTimestamp (\ s a -> s{_ldTimestamp = a})

instance FromJSON LabelDetection where
        parseJSON
          = withObject "LabelDetection"
              (\ x ->
                 LabelDetection' <$>
                   (x .:? "Label") <*> (x .:? "Timestamp"))

instance Hashable LabelDetection where

instance NFData LabelDetection where

-- | Indicates the location of the landmark on the face.
--
--
--
-- /See:/ 'landmark' smart constructor.
data Landmark = Landmark'
  { _lType :: !(Maybe LandmarkType)
  , _lX    :: !(Maybe Double)
  , _lY    :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Landmark' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lType' - Type of the landmark.
--
-- * 'lX' - x-coordinate from the top left of the landmark expressed as the ratio of the width of the image. For example, if the images is 700x200 and the x-coordinate of the landmark is at 350 pixels, this value is 0.5.
--
-- * 'lY' - y-coordinate from the top left of the landmark expressed as the ratio of the height of the image. For example, if the images is 700x200 and the y-coordinate of the landmark is at 100 pixels, this value is 0.5.
landmark
    :: Landmark
landmark = Landmark' {_lType = Nothing, _lX = Nothing, _lY = Nothing}


-- | Type of the landmark.
lType :: Lens' Landmark (Maybe LandmarkType)
lType = lens _lType (\ s a -> s{_lType = a})

-- | x-coordinate from the top left of the landmark expressed as the ratio of the width of the image. For example, if the images is 700x200 and the x-coordinate of the landmark is at 350 pixels, this value is 0.5.
lX :: Lens' Landmark (Maybe Double)
lX = lens _lX (\ s a -> s{_lX = a})

-- | y-coordinate from the top left of the landmark expressed as the ratio of the height of the image. For example, if the images is 700x200 and the y-coordinate of the landmark is at 100 pixels, this value is 0.5.
lY :: Lens' Landmark (Maybe Double)
lY = lens _lY (\ s a -> s{_lY = a})

instance FromJSON Landmark where
        parseJSON
          = withObject "Landmark"
              (\ x ->
                 Landmark' <$>
                   (x .:? "Type") <*> (x .:? "X") <*> (x .:? "Y"))

instance Hashable Landmark where

instance NFData Landmark where

-- | Provides information about a single type of moderated content found in an image or video. Each type of moderated content has a label within a hierarchical taxonomy. For more information, see 'moderation' .
--
--
--
-- /See:/ 'moderationLabel' smart constructor.
data ModerationLabel = ModerationLabel'
  { _mlConfidence :: !(Maybe Double)
  , _mlName       :: !(Maybe Text)
  , _mlParentName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModerationLabel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mlConfidence' - Specifies the confidence that Amazon Rekognition has that the label has been correctly identified. If you don't specify the @MinConfidence@ parameter in the call to @DetectModerationLabels@ , the operation returns labels with a confidence value greater than or equal to 50 percent.
--
-- * 'mlName' - The label name for the type of content detected in the image.
--
-- * 'mlParentName' - The name for the parent label. Labels at the top-level of the hierarchy have the parent label @""@ .
moderationLabel
    :: ModerationLabel
moderationLabel =
  ModerationLabel'
    {_mlConfidence = Nothing, _mlName = Nothing, _mlParentName = Nothing}


-- | Specifies the confidence that Amazon Rekognition has that the label has been correctly identified. If you don't specify the @MinConfidence@ parameter in the call to @DetectModerationLabels@ , the operation returns labels with a confidence value greater than or equal to 50 percent.
mlConfidence :: Lens' ModerationLabel (Maybe Double)
mlConfidence = lens _mlConfidence (\ s a -> s{_mlConfidence = a})

-- | The label name for the type of content detected in the image.
mlName :: Lens' ModerationLabel (Maybe Text)
mlName = lens _mlName (\ s a -> s{_mlName = a})

-- | The name for the parent label. Labels at the top-level of the hierarchy have the parent label @""@ .
mlParentName :: Lens' ModerationLabel (Maybe Text)
mlParentName = lens _mlParentName (\ s a -> s{_mlParentName = a})

instance FromJSON ModerationLabel where
        parseJSON
          = withObject "ModerationLabel"
              (\ x ->
                 ModerationLabel' <$>
                   (x .:? "Confidence") <*> (x .:? "Name") <*>
                     (x .:? "ParentName"))

instance Hashable ModerationLabel where

instance NFData ModerationLabel where

-- | Indicates whether or not the mouth on the face is open, and the confidence level in the determination.
--
--
--
-- /See:/ 'mouthOpen' smart constructor.
data MouthOpen = MouthOpen'
  { _moValue      :: !(Maybe Bool)
  , _moConfidence :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MouthOpen' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'moValue' - Boolean value that indicates whether the mouth on the face is open or not.
--
-- * 'moConfidence' - Level of confidence in the determination.
mouthOpen
    :: MouthOpen
mouthOpen = MouthOpen' {_moValue = Nothing, _moConfidence = Nothing}


-- | Boolean value that indicates whether the mouth on the face is open or not.
moValue :: Lens' MouthOpen (Maybe Bool)
moValue = lens _moValue (\ s a -> s{_moValue = a})

-- | Level of confidence in the determination.
moConfidence :: Lens' MouthOpen (Maybe Double)
moConfidence = lens _moConfidence (\ s a -> s{_moConfidence = a})

instance FromJSON MouthOpen where
        parseJSON
          = withObject "MouthOpen"
              (\ x ->
                 MouthOpen' <$>
                   (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable MouthOpen where

instance NFData MouthOpen where

-- | Indicates whether or not the face has a mustache, and the confidence level in the determination.
--
--
--
-- /See:/ 'mustache' smart constructor.
data Mustache = Mustache'
  { _mValue      :: !(Maybe Bool)
  , _mConfidence :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Mustache' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mValue' - Boolean value that indicates whether the face has mustache or not.
--
-- * 'mConfidence' - Level of confidence in the determination.
mustache
    :: Mustache
mustache = Mustache' {_mValue = Nothing, _mConfidence = Nothing}


-- | Boolean value that indicates whether the face has mustache or not.
mValue :: Lens' Mustache (Maybe Bool)
mValue = lens _mValue (\ s a -> s{_mValue = a})

-- | Level of confidence in the determination.
mConfidence :: Lens' Mustache (Maybe Double)
mConfidence = lens _mConfidence (\ s a -> s{_mConfidence = a})

instance FromJSON Mustache where
        parseJSON
          = withObject "Mustache"
              (\ x ->
                 Mustache' <$>
                   (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable Mustache where

instance NFData Mustache where

-- | The Amazon Simple Notification Service topic to which Amazon Rekognition publishes the completion status of a video analysis operation. For more information, see 'api-video' .
--
--
--
-- /See:/ 'notificationChannel' smart constructor.
data NotificationChannel = NotificationChannel'
  { _ncSNSTopicARN :: !Text
  , _ncRoleARN     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotificationChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncSNSTopicARN' - The Amazon SNS topic to which Amazon Rekognition to posts the completion status.
--
-- * 'ncRoleARN' - The ARN of an IAM role that gives Amazon Rekognition publishing permissions to the Amazon SNS topic.
notificationChannel
    :: Text -- ^ 'ncSNSTopicARN'
    -> Text -- ^ 'ncRoleARN'
    -> NotificationChannel
notificationChannel pSNSTopicARN_ pRoleARN_ =
  NotificationChannel' {_ncSNSTopicARN = pSNSTopicARN_, _ncRoleARN = pRoleARN_}


-- | The Amazon SNS topic to which Amazon Rekognition to posts the completion status.
ncSNSTopicARN :: Lens' NotificationChannel Text
ncSNSTopicARN = lens _ncSNSTopicARN (\ s a -> s{_ncSNSTopicARN = a})

-- | The ARN of an IAM role that gives Amazon Rekognition publishing permissions to the Amazon SNS topic.
ncRoleARN :: Lens' NotificationChannel Text
ncRoleARN = lens _ncRoleARN (\ s a -> s{_ncRoleARN = a})

instance Hashable NotificationChannel where

instance NFData NotificationChannel where

instance ToJSON NotificationChannel where
        toJSON NotificationChannel'{..}
          = object
              (catMaybes
                 [Just ("SNSTopicArn" .= _ncSNSTopicARN),
                  Just ("RoleArn" .= _ncRoleARN)])

-- | Details about a person detected in a video analysis request.
--
--
--
-- /See:/ 'personDetail' smart constructor.
data PersonDetail = PersonDetail'
  { _pdBoundingBox :: !(Maybe BoundingBox)
  , _pdIndex       :: !(Maybe Integer)
  , _pdFace        :: !(Maybe FaceDetail)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PersonDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdBoundingBox' - Bounding box around the detected person.
--
-- * 'pdIndex' - Identifier for the person detected person within a video. Use to keep track of the person throughout the video. The identifier is not stored by Amazon Rekognition.
--
-- * 'pdFace' - Face details for the detected person.
personDetail
    :: PersonDetail
personDetail =
  PersonDetail'
    {_pdBoundingBox = Nothing, _pdIndex = Nothing, _pdFace = Nothing}


-- | Bounding box around the detected person.
pdBoundingBox :: Lens' PersonDetail (Maybe BoundingBox)
pdBoundingBox = lens _pdBoundingBox (\ s a -> s{_pdBoundingBox = a})

-- | Identifier for the person detected person within a video. Use to keep track of the person throughout the video. The identifier is not stored by Amazon Rekognition.
pdIndex :: Lens' PersonDetail (Maybe Integer)
pdIndex = lens _pdIndex (\ s a -> s{_pdIndex = a})

-- | Face details for the detected person.
pdFace :: Lens' PersonDetail (Maybe FaceDetail)
pdFace = lens _pdFace (\ s a -> s{_pdFace = a})

instance FromJSON PersonDetail where
        parseJSON
          = withObject "PersonDetail"
              (\ x ->
                 PersonDetail' <$>
                   (x .:? "BoundingBox") <*> (x .:? "Index") <*>
                     (x .:? "Face"))

instance Hashable PersonDetail where

instance NFData PersonDetail where

-- | Details and tracking information for a single time a person is tracked in a video. Amazon Rekognition operations that track persons return an array of @PersonDetection@ objects with elements for each time a person is tracked in a video. For more information, see .
--
--
--
-- /See:/ 'personDetection' smart constructor.
data PersonDetection = PersonDetection'
  { _pdPerson    :: !(Maybe PersonDetail)
  , _pdTimestamp :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PersonDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdPerson' - Details about a person tracked in a video.
--
-- * 'pdTimestamp' - The time, in milliseconds from the start of the video, that the person was tracked.
personDetection
    :: PersonDetection
personDetection = PersonDetection' {_pdPerson = Nothing, _pdTimestamp = Nothing}


-- | Details about a person tracked in a video.
pdPerson :: Lens' PersonDetection (Maybe PersonDetail)
pdPerson = lens _pdPerson (\ s a -> s{_pdPerson = a})

-- | The time, in milliseconds from the start of the video, that the person was tracked.
pdTimestamp :: Lens' PersonDetection (Maybe Integer)
pdTimestamp = lens _pdTimestamp (\ s a -> s{_pdTimestamp = a})

instance FromJSON PersonDetection where
        parseJSON
          = withObject "PersonDetection"
              (\ x ->
                 PersonDetection' <$>
                   (x .:? "Person") <*> (x .:? "Timestamp"))

instance Hashable PersonDetection where

instance NFData PersonDetection where

-- | Information about a person whose face matches a face(s) in a Amazon Rekognition collection. Includes information about the faces in the Amazon Rekognition collection (, information about the person ('PersonDetail' ) and the timestamp for when the person was detected in a video. An array of @PersonMatch@ objects is returned by .
--
--
--
-- /See:/ 'personMatch' smart constructor.
data PersonMatch = PersonMatch'
  { _pmFaceMatches :: !(Maybe [FaceMatch])
  , _pmPerson      :: !(Maybe PersonDetail)
  , _pmTimestamp   :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PersonMatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmFaceMatches' - Information about the faces in the input collection that match the face of a person in the video.
--
-- * 'pmPerson' - Information about the matched person.
--
-- * 'pmTimestamp' - The time, in milliseconds from the beginning of the video, that the person was matched in the video.
personMatch
    :: PersonMatch
personMatch =
  PersonMatch'
    {_pmFaceMatches = Nothing, _pmPerson = Nothing, _pmTimestamp = Nothing}


-- | Information about the faces in the input collection that match the face of a person in the video.
pmFaceMatches :: Lens' PersonMatch [FaceMatch]
pmFaceMatches = lens _pmFaceMatches (\ s a -> s{_pmFaceMatches = a}) . _Default . _Coerce

-- | Information about the matched person.
pmPerson :: Lens' PersonMatch (Maybe PersonDetail)
pmPerson = lens _pmPerson (\ s a -> s{_pmPerson = a})

-- | The time, in milliseconds from the beginning of the video, that the person was matched in the video.
pmTimestamp :: Lens' PersonMatch (Maybe Integer)
pmTimestamp = lens _pmTimestamp (\ s a -> s{_pmTimestamp = a})

instance FromJSON PersonMatch where
        parseJSON
          = withObject "PersonMatch"
              (\ x ->
                 PersonMatch' <$>
                   (x .:? "FaceMatches" .!= mempty) <*> (x .:? "Person")
                     <*> (x .:? "Timestamp"))

instance Hashable PersonMatch where

instance NFData PersonMatch where

-- | The X and Y coordinates of a point on an image. The X and Y values returned are ratios of the overall image size. For example, if the input image is 700x200 and the operation returns X=0.5 and Y=0.25, then the point is at the (350,50) pixel coordinate on the image.
--
--
-- An array of @Point@ objects, @Polygon@ , is returned by . @Polygon@ represents a fine-grained polygon around detected text. For more information, see .
--
--
-- /See:/ 'point' smart constructor.
data Point = Point'
  { _pX :: !(Maybe Double)
  , _pY :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Point' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pX' - The value of the X coordinate for a point on a @Polygon@ .
--
-- * 'pY' - The value of the Y coordinate for a point on a @Polygon@ .
point
    :: Point
point = Point' {_pX = Nothing, _pY = Nothing}


-- | The value of the X coordinate for a point on a @Polygon@ .
pX :: Lens' Point (Maybe Double)
pX = lens _pX (\ s a -> s{_pX = a})

-- | The value of the Y coordinate for a point on a @Polygon@ .
pY :: Lens' Point (Maybe Double)
pY = lens _pY (\ s a -> s{_pY = a})

instance FromJSON Point where
        parseJSON
          = withObject "Point"
              (\ x -> Point' <$> (x .:? "X") <*> (x .:? "Y"))

instance Hashable Point where

instance NFData Point where

-- | Indicates the pose of the face as determined by its pitch, roll, and yaw.
--
--
--
-- /See:/ 'pose' smart constructor.
data Pose = Pose'
  { _pYaw   :: !(Maybe Double)
  , _pRoll  :: !(Maybe Double)
  , _pPitch :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
pose = Pose' {_pYaw = Nothing, _pRoll = Nothing, _pPitch = Nothing}


-- | Value representing the face rotation on the yaw axis.
pYaw :: Lens' Pose (Maybe Double)
pYaw = lens _pYaw (\ s a -> s{_pYaw = a})

-- | Value representing the face rotation on the roll axis.
pRoll :: Lens' Pose (Maybe Double)
pRoll = lens _pRoll (\ s a -> s{_pRoll = a})

-- | Value representing the face rotation on the pitch axis.
pPitch :: Lens' Pose (Maybe Double)
pPitch = lens _pPitch (\ s a -> s{_pPitch = a})

instance FromJSON Pose where
        parseJSON
          = withObject "Pose"
              (\ x ->
                 Pose' <$>
                   (x .:? "Yaw") <*> (x .:? "Roll") <*> (x .:? "Pitch"))

instance Hashable Pose where

instance NFData Pose where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
  S3Object' {_soBucket = Nothing, _soName = Nothing, _soVersion = Nothing}


-- | Name of the S3 bucket.
soBucket :: Lens' S3Object (Maybe Text)
soBucket = lens _soBucket (\ s a -> s{_soBucket = a})

-- | S3 object key name.
soName :: Lens' S3Object (Maybe Text)
soName = lens _soName (\ s a -> s{_soName = a})

-- | If the bucket is versioning enabled, you can specify the object version.
soVersion :: Lens' S3Object (Maybe Text)
soVersion = lens _soVersion (\ s a -> s{_soVersion = a})

instance Hashable S3Object where

instance NFData S3Object where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Smile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smiValue' - Boolean value that indicates whether the face is smiling or not.
--
-- * 'smiConfidence' - Level of confidence in the determination.
smile
    :: Smile
smile = Smile' {_smiValue = Nothing, _smiConfidence = Nothing}


-- | Boolean value that indicates whether the face is smiling or not.
smiValue :: Lens' Smile (Maybe Bool)
smiValue = lens _smiValue (\ s a -> s{_smiValue = a})

-- | Level of confidence in the determination.
smiConfidence :: Lens' Smile (Maybe Double)
smiConfidence = lens _smiConfidence (\ s a -> s{_smiConfidence = a})

instance FromJSON Smile where
        parseJSON
          = withObject "Smile"
              (\ x ->
                 Smile' <$> (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable Smile where

instance NFData Smile where

-- | An object that recognizes faces in a streaming video. An Amazon Rekognition stream processor is created by a call to . The request parameters for @CreateStreamProcessor@ describe the Kinesis video stream source for the streaming video, face recognition parameters, and where to stream the analysis resullts.
--
--
--
-- /See:/ 'streamProcessor' smart constructor.
data StreamProcessor = StreamProcessor'
  { _spStatus :: !(Maybe StreamProcessorStatus)
  , _spName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamProcessor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spStatus' - Current status of the Amazon Rekognition stream processor.
--
-- * 'spName' - Name of the Amazon Rekognition stream processor.
streamProcessor
    :: StreamProcessor
streamProcessor = StreamProcessor' {_spStatus = Nothing, _spName = Nothing}


-- | Current status of the Amazon Rekognition stream processor.
spStatus :: Lens' StreamProcessor (Maybe StreamProcessorStatus)
spStatus = lens _spStatus (\ s a -> s{_spStatus = a})

-- | Name of the Amazon Rekognition stream processor.
spName :: Lens' StreamProcessor (Maybe Text)
spName = lens _spName (\ s a -> s{_spName = a})

instance FromJSON StreamProcessor where
        parseJSON
          = withObject "StreamProcessor"
              (\ x ->
                 StreamProcessor' <$>
                   (x .:? "Status") <*> (x .:? "Name"))

instance Hashable StreamProcessor where

instance NFData StreamProcessor where

-- | Information about the source streaming video.
--
--
--
-- /See:/ 'streamProcessorInput' smart constructor.
newtype StreamProcessorInput = StreamProcessorInput'
  { _spiKinesisVideoStream :: Maybe KinesisVideoStream
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamProcessorInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spiKinesisVideoStream' - The Kinesis video stream input stream for the source streaming video.
streamProcessorInput
    :: StreamProcessorInput
streamProcessorInput = StreamProcessorInput' {_spiKinesisVideoStream = Nothing}


-- | The Kinesis video stream input stream for the source streaming video.
spiKinesisVideoStream :: Lens' StreamProcessorInput (Maybe KinesisVideoStream)
spiKinesisVideoStream = lens _spiKinesisVideoStream (\ s a -> s{_spiKinesisVideoStream = a})

instance FromJSON StreamProcessorInput where
        parseJSON
          = withObject "StreamProcessorInput"
              (\ x ->
                 StreamProcessorInput' <$>
                   (x .:? "KinesisVideoStream"))

instance Hashable StreamProcessorInput where

instance NFData StreamProcessorInput where

instance ToJSON StreamProcessorInput where
        toJSON StreamProcessorInput'{..}
          = object
              (catMaybes
                 [("KinesisVideoStream" .=) <$>
                    _spiKinesisVideoStream])

-- | Information about the Amazon Kinesis Data Streams stream to which a Rekognition Video stream processor streams the results of a video analysis. For more information, see .
--
--
--
-- /See:/ 'streamProcessorOutput' smart constructor.
newtype StreamProcessorOutput = StreamProcessorOutput'
  { _spoKinesisDataStream :: Maybe KinesisDataStream
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamProcessorOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spoKinesisDataStream' - The Amazon Kinesis Data Streams stream to which the Amazon Rekognition stream processor streams the analysis results.
streamProcessorOutput
    :: StreamProcessorOutput
streamProcessorOutput = StreamProcessorOutput' {_spoKinesisDataStream = Nothing}


-- | The Amazon Kinesis Data Streams stream to which the Amazon Rekognition stream processor streams the analysis results.
spoKinesisDataStream :: Lens' StreamProcessorOutput (Maybe KinesisDataStream)
spoKinesisDataStream = lens _spoKinesisDataStream (\ s a -> s{_spoKinesisDataStream = a})

instance FromJSON StreamProcessorOutput where
        parseJSON
          = withObject "StreamProcessorOutput"
              (\ x ->
                 StreamProcessorOutput' <$>
                   (x .:? "KinesisDataStream"))

instance Hashable StreamProcessorOutput where

instance NFData StreamProcessorOutput where

instance ToJSON StreamProcessorOutput where
        toJSON StreamProcessorOutput'{..}
          = object
              (catMaybes
                 [("KinesisDataStream" .=) <$> _spoKinesisDataStream])

-- | Input parameters used to recognize faces in a streaming video analyzed by a Amazon Rekognition stream processor.
--
--
--
-- /See:/ 'streamProcessorSettings' smart constructor.
newtype StreamProcessorSettings = StreamProcessorSettings'
  { _spsFaceSearch :: Maybe FaceSearchSettings
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamProcessorSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spsFaceSearch' - Face search settings to use on a streaming video.
streamProcessorSettings
    :: StreamProcessorSettings
streamProcessorSettings = StreamProcessorSettings' {_spsFaceSearch = Nothing}


-- | Face search settings to use on a streaming video.
spsFaceSearch :: Lens' StreamProcessorSettings (Maybe FaceSearchSettings)
spsFaceSearch = lens _spsFaceSearch (\ s a -> s{_spsFaceSearch = a})

instance FromJSON StreamProcessorSettings where
        parseJSON
          = withObject "StreamProcessorSettings"
              (\ x ->
                 StreamProcessorSettings' <$> (x .:? "FaceSearch"))

instance Hashable StreamProcessorSettings where

instance NFData StreamProcessorSettings where

instance ToJSON StreamProcessorSettings where
        toJSON StreamProcessorSettings'{..}
          = object
              (catMaybes [("FaceSearch" .=) <$> _spsFaceSearch])

-- | Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.
--
--
--
-- /See:/ 'sunglasses' smart constructor.
data Sunglasses = Sunglasses'
  { _sValue      :: !(Maybe Bool)
  , _sConfidence :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Sunglasses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sValue' - Boolean value that indicates whether the face is wearing sunglasses or not.
--
-- * 'sConfidence' - Level of confidence in the determination.
sunglasses
    :: Sunglasses
sunglasses = Sunglasses' {_sValue = Nothing, _sConfidence = Nothing}


-- | Boolean value that indicates whether the face is wearing sunglasses or not.
sValue :: Lens' Sunglasses (Maybe Bool)
sValue = lens _sValue (\ s a -> s{_sValue = a})

-- | Level of confidence in the determination.
sConfidence :: Lens' Sunglasses (Maybe Double)
sConfidence = lens _sConfidence (\ s a -> s{_sConfidence = a})

instance FromJSON Sunglasses where
        parseJSON
          = withObject "Sunglasses"
              (\ x ->
                 Sunglasses' <$>
                   (x .:? "Value") <*> (x .:? "Confidence"))

instance Hashable Sunglasses where

instance NFData Sunglasses where

-- | Information about a word or line of text detected by .
--
--
-- The @DetectedText@ field contains the text that Amazon Rekognition detected in the image.
--
-- Every word and line has an identifier (@Id@ ). Each word belongs to a line and has a parent identifier (@ParentId@ ) that identifies the line of text in which the word appears. The word @Id@ is also an index for the word within a line of words.
--
-- For more information, see 'text-detection' .
--
--
-- /See:/ 'textDetection' smart constructor.
data TextDetection = TextDetection'
  { _tdDetectedText :: !(Maybe Text)
  , _tdConfidence   :: !(Maybe Double)
  , _tdGeometry     :: !(Maybe Geometry)
  , _tdId           :: !(Maybe Nat)
  , _tdType         :: !(Maybe TextTypes)
  , _tdParentId     :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TextDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdDetectedText' - The word or line of text recognized by Amazon Rekognition.
--
-- * 'tdConfidence' - The confidence that Amazon Rekognition has in the accuracy of the detected text and the accuracy of the geometry points around the detected text.
--
-- * 'tdGeometry' - The location of the detected text on the image. Includes an axis aligned coarse bounding box surrounding the text and a finer grain polygon for more accurate spatial information.
--
-- * 'tdId' - The identifier for the detected text. The identifier is only unique for a single call to @DetectText@ .
--
-- * 'tdType' - The type of text that was detected.
--
-- * 'tdParentId' - The Parent identifier for the detected text identified by the value of @ID@ . If the type of detected text is @LINE@ , the value of @ParentId@ is @Null@ .
textDetection
    :: TextDetection
textDetection =
  TextDetection'
    { _tdDetectedText = Nothing
    , _tdConfidence = Nothing
    , _tdGeometry = Nothing
    , _tdId = Nothing
    , _tdType = Nothing
    , _tdParentId = Nothing
    }


-- | The word or line of text recognized by Amazon Rekognition.
tdDetectedText :: Lens' TextDetection (Maybe Text)
tdDetectedText = lens _tdDetectedText (\ s a -> s{_tdDetectedText = a})

-- | The confidence that Amazon Rekognition has in the accuracy of the detected text and the accuracy of the geometry points around the detected text.
tdConfidence :: Lens' TextDetection (Maybe Double)
tdConfidence = lens _tdConfidence (\ s a -> s{_tdConfidence = a})

-- | The location of the detected text on the image. Includes an axis aligned coarse bounding box surrounding the text and a finer grain polygon for more accurate spatial information.
tdGeometry :: Lens' TextDetection (Maybe Geometry)
tdGeometry = lens _tdGeometry (\ s a -> s{_tdGeometry = a})

-- | The identifier for the detected text. The identifier is only unique for a single call to @DetectText@ .
tdId :: Lens' TextDetection (Maybe Natural)
tdId = lens _tdId (\ s a -> s{_tdId = a}) . mapping _Nat

-- | The type of text that was detected.
tdType :: Lens' TextDetection (Maybe TextTypes)
tdType = lens _tdType (\ s a -> s{_tdType = a})

-- | The Parent identifier for the detected text identified by the value of @ID@ . If the type of detected text is @LINE@ , the value of @ParentId@ is @Null@ .
tdParentId :: Lens' TextDetection (Maybe Natural)
tdParentId = lens _tdParentId (\ s a -> s{_tdParentId = a}) . mapping _Nat

instance FromJSON TextDetection where
        parseJSON
          = withObject "TextDetection"
              (\ x ->
                 TextDetection' <$>
                   (x .:? "DetectedText") <*> (x .:? "Confidence") <*>
                     (x .:? "Geometry")
                     <*> (x .:? "Id")
                     <*> (x .:? "Type")
                     <*> (x .:? "ParentId"))

instance Hashable TextDetection where

instance NFData TextDetection where

-- | Video file stored in an Amazon S3 bucket. Amazon Rekognition video start operations such as use @Video@ to specify a video for analysis. The supported file formats are .mp4, .mov and .avi.
--
--
--
-- /See:/ 'video' smart constructor.
newtype Video = Video'
  { _vS3Object :: Maybe S3Object
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Video' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vS3Object' - The Amazon S3 bucket name and file name for the video.
video
    :: Video
video = Video' {_vS3Object = Nothing}


-- | The Amazon S3 bucket name and file name for the video.
vS3Object :: Lens' Video (Maybe S3Object)
vS3Object = lens _vS3Object (\ s a -> s{_vS3Object = a})

instance Hashable Video where

instance NFData Video where

instance ToJSON Video where
        toJSON Video'{..}
          = object (catMaybes [("S3Object" .=) <$> _vS3Object])

-- | Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition video operation.
--
--
--
-- /See:/ 'videoMetadata' smart constructor.
data VideoMetadata = VideoMetadata'
  { _vmFrameRate      :: !(Maybe Double)
  , _vmFormat         :: !(Maybe Text)
  , _vmCodec          :: !(Maybe Text)
  , _vmFrameHeight    :: !(Maybe Nat)
  , _vmDurationMillis :: !(Maybe Nat)
  , _vmFrameWidth     :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VideoMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmFrameRate' - Number of frames per second in the video.
--
-- * 'vmFormat' - Format of the analyzed video. Possible values are MP4, MOV and AVI.
--
-- * 'vmCodec' - Type of compression used in the analyzed video.
--
-- * 'vmFrameHeight' - Vertical pixel dimension of the video.
--
-- * 'vmDurationMillis' - Length of the video in milliseconds.
--
-- * 'vmFrameWidth' - Horizontal pixel dimension of the video.
videoMetadata
    :: VideoMetadata
videoMetadata =
  VideoMetadata'
    { _vmFrameRate = Nothing
    , _vmFormat = Nothing
    , _vmCodec = Nothing
    , _vmFrameHeight = Nothing
    , _vmDurationMillis = Nothing
    , _vmFrameWidth = Nothing
    }


-- | Number of frames per second in the video.
vmFrameRate :: Lens' VideoMetadata (Maybe Double)
vmFrameRate = lens _vmFrameRate (\ s a -> s{_vmFrameRate = a})

-- | Format of the analyzed video. Possible values are MP4, MOV and AVI.
vmFormat :: Lens' VideoMetadata (Maybe Text)
vmFormat = lens _vmFormat (\ s a -> s{_vmFormat = a})

-- | Type of compression used in the analyzed video.
vmCodec :: Lens' VideoMetadata (Maybe Text)
vmCodec = lens _vmCodec (\ s a -> s{_vmCodec = a})

-- | Vertical pixel dimension of the video.
vmFrameHeight :: Lens' VideoMetadata (Maybe Natural)
vmFrameHeight = lens _vmFrameHeight (\ s a -> s{_vmFrameHeight = a}) . mapping _Nat

-- | Length of the video in milliseconds.
vmDurationMillis :: Lens' VideoMetadata (Maybe Natural)
vmDurationMillis = lens _vmDurationMillis (\ s a -> s{_vmDurationMillis = a}) . mapping _Nat

-- | Horizontal pixel dimension of the video.
vmFrameWidth :: Lens' VideoMetadata (Maybe Natural)
vmFrameWidth = lens _vmFrameWidth (\ s a -> s{_vmFrameWidth = a}) . mapping _Nat

instance FromJSON VideoMetadata where
        parseJSON
          = withObject "VideoMetadata"
              (\ x ->
                 VideoMetadata' <$>
                   (x .:? "FrameRate") <*> (x .:? "Format") <*>
                     (x .:? "Codec")
                     <*> (x .:? "FrameHeight")
                     <*> (x .:? "DurationMillis")
                     <*> (x .:? "FrameWidth"))

instance Hashable VideoMetadata where

instance NFData VideoMetadata where
