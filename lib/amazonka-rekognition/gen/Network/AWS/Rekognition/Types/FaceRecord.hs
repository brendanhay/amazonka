{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceRecord where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.Face
import Network.AWS.Rekognition.Types.FaceDetail

-- | Object containing both the face metadata (stored in the backend database), and facial attributes that are detected but aren't stored in the database.
--
--
--
-- /See:/ 'faceRecord' smart constructor.
data FaceRecord = FaceRecord'
  { _frFaceDetail :: !(Maybe FaceDetail),
    _frFace :: !(Maybe Face)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FaceRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frFaceDetail' - Structure containing attributes of the face that the algorithm detected.
--
-- * 'frFace' - Describes the face properties such as the bounding box, face ID, image ID of the input image, and external image ID that you assigned.
faceRecord ::
  FaceRecord
faceRecord =
  FaceRecord' {_frFaceDetail = Nothing, _frFace = Nothing}

-- | Structure containing attributes of the face that the algorithm detected.
frFaceDetail :: Lens' FaceRecord (Maybe FaceDetail)
frFaceDetail = lens _frFaceDetail (\s a -> s {_frFaceDetail = a})

-- | Describes the face properties such as the bounding box, face ID, image ID of the input image, and external image ID that you assigned.
frFace :: Lens' FaceRecord (Maybe Face)
frFace = lens _frFace (\s a -> s {_frFace = a})

instance FromJSON FaceRecord where
  parseJSON =
    withObject
      "FaceRecord"
      (\x -> FaceRecord' <$> (x .:? "FaceDetail") <*> (x .:? "Face"))

instance Hashable FaceRecord

instance NFData FaceRecord
