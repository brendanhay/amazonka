{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ComparedFace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ComparedFace where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.ImageQuality
import Network.AWS.Rekognition.Types.Landmark
import Network.AWS.Rekognition.Types.Pose

-- | Provides face metadata for target image faces that are analyzed by @CompareFaces@ and @RecognizeCelebrities@ .
--
--
--
-- /See:/ 'comparedFace' smart constructor.
data ComparedFace = ComparedFace'
  { _cfBoundingBox ::
      !(Maybe BoundingBox),
    _cfPose :: !(Maybe Pose),
    _cfConfidence :: !(Maybe Double),
    _cfQuality :: !(Maybe ImageQuality),
    _cfLandmarks :: !(Maybe [Landmark])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
comparedFace ::
  ComparedFace
comparedFace =
  ComparedFace'
    { _cfBoundingBox = Nothing,
      _cfPose = Nothing,
      _cfConfidence = Nothing,
      _cfQuality = Nothing,
      _cfLandmarks = Nothing
    }

-- | Bounding box of the face.
cfBoundingBox :: Lens' ComparedFace (Maybe BoundingBox)
cfBoundingBox = lens _cfBoundingBox (\s a -> s {_cfBoundingBox = a})

-- | Indicates the pose of the face as determined by its pitch, roll, and yaw.
cfPose :: Lens' ComparedFace (Maybe Pose)
cfPose = lens _cfPose (\s a -> s {_cfPose = a})

-- | Level of confidence that what the bounding box contains is a face.
cfConfidence :: Lens' ComparedFace (Maybe Double)
cfConfidence = lens _cfConfidence (\s a -> s {_cfConfidence = a})

-- | Identifies face image brightness and sharpness.
cfQuality :: Lens' ComparedFace (Maybe ImageQuality)
cfQuality = lens _cfQuality (\s a -> s {_cfQuality = a})

-- | An array of facial landmarks.
cfLandmarks :: Lens' ComparedFace [Landmark]
cfLandmarks = lens _cfLandmarks (\s a -> s {_cfLandmarks = a}) . _Default . _Coerce

instance FromJSON ComparedFace where
  parseJSON =
    withObject
      "ComparedFace"
      ( \x ->
          ComparedFace'
            <$> (x .:? "BoundingBox")
            <*> (x .:? "Pose")
            <*> (x .:? "Confidence")
            <*> (x .:? "Quality")
            <*> (x .:? "Landmarks" .!= mempty)
      )

instance Hashable ComparedFace

instance NFData ComparedFace
