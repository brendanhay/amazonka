{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceMatch where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.Face

-- | Provides face metadata. In addition, it also provides the confidence in the match of this face with the input face.
--
--
--
-- /See:/ 'faceMatch' smart constructor.
data FaceMatch = FaceMatch'
  { _fmSimilarity :: !(Maybe Double),
    _fmFace :: !(Maybe Face)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FaceMatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fmSimilarity' - Confidence in the match of this face with the input face.
--
-- * 'fmFace' - Describes the face properties such as the bounding box, face ID, image ID of the source image, and external image ID that you assigned.
faceMatch ::
  FaceMatch
faceMatch = FaceMatch' {_fmSimilarity = Nothing, _fmFace = Nothing}

-- | Confidence in the match of this face with the input face.
fmSimilarity :: Lens' FaceMatch (Maybe Double)
fmSimilarity = lens _fmSimilarity (\s a -> s {_fmSimilarity = a})

-- | Describes the face properties such as the bounding box, face ID, image ID of the source image, and external image ID that you assigned.
fmFace :: Lens' FaceMatch (Maybe Face)
fmFace = lens _fmFace (\s a -> s {_fmFace = a})

instance FromJSON FaceMatch where
  parseJSON =
    withObject
      "FaceMatch"
      (\x -> FaceMatch' <$> (x .:? "Similarity") <*> (x .:? "Face"))

instance Hashable FaceMatch

instance NFData FaceMatch
