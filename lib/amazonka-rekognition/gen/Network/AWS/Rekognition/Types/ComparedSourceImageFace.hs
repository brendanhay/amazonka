{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ComparedSourceImageFace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ComparedSourceImageFace where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.BoundingBox

-- | Type that describes the face Amazon Rekognition chose to compare with the faces in the target. This contains a bounding box for the selected face and confidence level that the bounding box contains a face. Note that Amazon Rekognition selects the largest face in the source image for this comparison.
--
--
--
-- /See:/ 'comparedSourceImageFace' smart constructor.
data ComparedSourceImageFace = ComparedSourceImageFace'
  { _csifBoundingBox ::
      !(Maybe BoundingBox),
    _csifConfidence :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComparedSourceImageFace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csifBoundingBox' - Bounding box of the face.
--
-- * 'csifConfidence' - Confidence level that the selected bounding box contains a face.
comparedSourceImageFace ::
  ComparedSourceImageFace
comparedSourceImageFace =
  ComparedSourceImageFace'
    { _csifBoundingBox = Nothing,
      _csifConfidence = Nothing
    }

-- | Bounding box of the face.
csifBoundingBox :: Lens' ComparedSourceImageFace (Maybe BoundingBox)
csifBoundingBox = lens _csifBoundingBox (\s a -> s {_csifBoundingBox = a})

-- | Confidence level that the selected bounding box contains a face.
csifConfidence :: Lens' ComparedSourceImageFace (Maybe Double)
csifConfidence = lens _csifConfidence (\s a -> s {_csifConfidence = a})

instance FromJSON ComparedSourceImageFace where
  parseJSON =
    withObject
      "ComparedSourceImageFace"
      ( \x ->
          ComparedSourceImageFace'
            <$> (x .:? "BoundingBox") <*> (x .:? "Confidence")
      )

instance Hashable ComparedSourceImageFace

instance NFData ComparedSourceImageFace
