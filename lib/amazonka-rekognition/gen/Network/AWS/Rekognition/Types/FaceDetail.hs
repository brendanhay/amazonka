{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceDetail where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.AgeRange
import Network.AWS.Rekognition.Types.Beard
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.Emotion
import Network.AWS.Rekognition.Types.EyeOpen
import Network.AWS.Rekognition.Types.Eyeglasses
import Network.AWS.Rekognition.Types.Gender
import Network.AWS.Rekognition.Types.ImageQuality
import Network.AWS.Rekognition.Types.Landmark
import Network.AWS.Rekognition.Types.MouthOpen
import Network.AWS.Rekognition.Types.Mustache
import Network.AWS.Rekognition.Types.Pose
import Network.AWS.Rekognition.Types.Smile
import Network.AWS.Rekognition.Types.Sunglasses

-- | Structure containing attributes of the face that the algorithm detected.
--
--
-- A @FaceDetail@ object contains either the default facial attributes or all facial attributes. The default attributes are @BoundingBox@ , @Confidence@ , @Landmarks@ , @Pose@ , and @Quality@ .
--
-- 'GetFaceDetection' is the only Amazon Rekognition Video stored video operation that can return a @FaceDetail@ object with all attributes. To specify which attributes to return, use the @FaceAttributes@ input parameter for 'StartFaceDetection' . The following Amazon Rekognition Video operations return only the default attributes. The corresponding Start operations don't have a @FaceAttributes@ input parameter.
--
--     * GetCelebrityRecognition
--
--     * GetPersonTracking
--
--     * GetFaceSearch
--
--
--
-- The Amazon Rekognition Image 'DetectFaces' and 'IndexFaces' operations can return all facial attributes. To specify which attributes to return, use the @Attributes@ input parameter for @DetectFaces@ . For @IndexFaces@ , use the @DetectAttributes@ input parameter.
--
--
-- /See:/ 'faceDetail' smart constructor.
data FaceDetail = FaceDetail'
  { _fdAgeRange :: !(Maybe AgeRange),
    _fdSunglasses :: !(Maybe Sunglasses),
    _fdMouthOpen :: !(Maybe MouthOpen),
    _fdBoundingBox :: !(Maybe BoundingBox),
    _fdEmotions :: !(Maybe [Emotion]),
    _fdEyesOpen :: !(Maybe EyeOpen),
    _fdPose :: !(Maybe Pose),
    _fdConfidence :: !(Maybe Double),
    _fdGender :: !(Maybe Gender),
    _fdQuality :: !(Maybe ImageQuality),
    _fdEyeglasses :: !(Maybe Eyeglasses),
    _fdBeard :: !(Maybe Beard),
    _fdMustache :: !(Maybe Mustache),
    _fdSmile :: !(Maybe Smile),
    _fdLandmarks :: !(Maybe [Landmark])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
-- * 'fdEmotions' - The emotions that appear to be expressed on the face, and the confidence level in the determination. The API is only making a determination of the physical appearance of a person's face. It is not a determination of the person’s internal emotional state and should not be used in such a way. For example, a person pretending to have a sad face might not be sad emotionally.
--
-- * 'fdEyesOpen' - Indicates whether or not the eyes on the face are open, and the confidence level in the determination.
--
-- * 'fdPose' - Indicates the pose of the face as determined by its pitch, roll, and yaw. Default attribute.
--
-- * 'fdConfidence' - Confidence level that the bounding box contains a face (and not a different object such as a tree). Default attribute.
--
-- * 'fdGender' - The predicted gender of a detected face.
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
faceDetail ::
  FaceDetail
faceDetail =
  FaceDetail'
    { _fdAgeRange = Nothing,
      _fdSunglasses = Nothing,
      _fdMouthOpen = Nothing,
      _fdBoundingBox = Nothing,
      _fdEmotions = Nothing,
      _fdEyesOpen = Nothing,
      _fdPose = Nothing,
      _fdConfidence = Nothing,
      _fdGender = Nothing,
      _fdQuality = Nothing,
      _fdEyeglasses = Nothing,
      _fdBeard = Nothing,
      _fdMustache = Nothing,
      _fdSmile = Nothing,
      _fdLandmarks = Nothing
    }

-- | The estimated age range, in years, for the face. Low represents the lowest estimated age and High represents the highest estimated age.
fdAgeRange :: Lens' FaceDetail (Maybe AgeRange)
fdAgeRange = lens _fdAgeRange (\s a -> s {_fdAgeRange = a})

-- | Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.
fdSunglasses :: Lens' FaceDetail (Maybe Sunglasses)
fdSunglasses = lens _fdSunglasses (\s a -> s {_fdSunglasses = a})

-- | Indicates whether or not the mouth on the face is open, and the confidence level in the determination.
fdMouthOpen :: Lens' FaceDetail (Maybe MouthOpen)
fdMouthOpen = lens _fdMouthOpen (\s a -> s {_fdMouthOpen = a})

-- | Bounding box of the face. Default attribute.
fdBoundingBox :: Lens' FaceDetail (Maybe BoundingBox)
fdBoundingBox = lens _fdBoundingBox (\s a -> s {_fdBoundingBox = a})

-- | The emotions that appear to be expressed on the face, and the confidence level in the determination. The API is only making a determination of the physical appearance of a person's face. It is not a determination of the person’s internal emotional state and should not be used in such a way. For example, a person pretending to have a sad face might not be sad emotionally.
fdEmotions :: Lens' FaceDetail [Emotion]
fdEmotions = lens _fdEmotions (\s a -> s {_fdEmotions = a}) . _Default . _Coerce

-- | Indicates whether or not the eyes on the face are open, and the confidence level in the determination.
fdEyesOpen :: Lens' FaceDetail (Maybe EyeOpen)
fdEyesOpen = lens _fdEyesOpen (\s a -> s {_fdEyesOpen = a})

-- | Indicates the pose of the face as determined by its pitch, roll, and yaw. Default attribute.
fdPose :: Lens' FaceDetail (Maybe Pose)
fdPose = lens _fdPose (\s a -> s {_fdPose = a})

-- | Confidence level that the bounding box contains a face (and not a different object such as a tree). Default attribute.
fdConfidence :: Lens' FaceDetail (Maybe Double)
fdConfidence = lens _fdConfidence (\s a -> s {_fdConfidence = a})

-- | The predicted gender of a detected face.
fdGender :: Lens' FaceDetail (Maybe Gender)
fdGender = lens _fdGender (\s a -> s {_fdGender = a})

-- | Identifies image brightness and sharpness. Default attribute.
fdQuality :: Lens' FaceDetail (Maybe ImageQuality)
fdQuality = lens _fdQuality (\s a -> s {_fdQuality = a})

-- | Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.
fdEyeglasses :: Lens' FaceDetail (Maybe Eyeglasses)
fdEyeglasses = lens _fdEyeglasses (\s a -> s {_fdEyeglasses = a})

-- | Indicates whether or not the face has a beard, and the confidence level in the determination.
fdBeard :: Lens' FaceDetail (Maybe Beard)
fdBeard = lens _fdBeard (\s a -> s {_fdBeard = a})

-- | Indicates whether or not the face has a mustache, and the confidence level in the determination.
fdMustache :: Lens' FaceDetail (Maybe Mustache)
fdMustache = lens _fdMustache (\s a -> s {_fdMustache = a})

-- | Indicates whether or not the face is smiling, and the confidence level in the determination.
fdSmile :: Lens' FaceDetail (Maybe Smile)
fdSmile = lens _fdSmile (\s a -> s {_fdSmile = a})

-- | Indicates the location of landmarks on the face. Default attribute.
fdLandmarks :: Lens' FaceDetail [Landmark]
fdLandmarks = lens _fdLandmarks (\s a -> s {_fdLandmarks = a}) . _Default . _Coerce

instance FromJSON FaceDetail where
  parseJSON =
    withObject
      "FaceDetail"
      ( \x ->
          FaceDetail'
            <$> (x .:? "AgeRange")
            <*> (x .:? "Sunglasses")
            <*> (x .:? "MouthOpen")
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
            <*> (x .:? "Landmarks" .!= mempty)
      )

instance Hashable FaceDetail

instance NFData FaceDetail
