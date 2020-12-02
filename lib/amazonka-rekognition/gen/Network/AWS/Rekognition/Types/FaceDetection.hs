{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceDetection where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.FaceDetail

-- | Information about a face detected in a video analysis request and the time the face was detected in the video.
--
--
--
-- /See:/ 'faceDetection' smart constructor.
data FaceDetection = FaceDetection'
  { _fdTimestamp ::
      !(Maybe Integer),
    _fdFace :: !(Maybe FaceDetail)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FaceDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdTimestamp' - Time, in milliseconds from the start of the video, that the face was detected.
--
-- * 'fdFace' - The face properties for the detected face.
faceDetection ::
  FaceDetection
faceDetection =
  FaceDetection' {_fdTimestamp = Nothing, _fdFace = Nothing}

-- | Time, in milliseconds from the start of the video, that the face was detected.
fdTimestamp :: Lens' FaceDetection (Maybe Integer)
fdTimestamp = lens _fdTimestamp (\s a -> s {_fdTimestamp = a})

-- | The face properties for the detected face.
fdFace :: Lens' FaceDetection (Maybe FaceDetail)
fdFace = lens _fdFace (\s a -> s {_fdFace = a})

instance FromJSON FaceDetection where
  parseJSON =
    withObject
      "FaceDetection"
      (\x -> FaceDetection' <$> (x .:? "Timestamp") <*> (x .:? "Face"))

instance Hashable FaceDetection

instance NFData FaceDetection
