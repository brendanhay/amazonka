{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Face
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Face where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.BoundingBox

-- | Describes the face properties such as the bounding box, face ID, image ID of the input image, and external image ID that you assigned.
--
--
--
-- /See:/ 'face' smart constructor.
data Face = Face'
  { _fFaceId :: !(Maybe Text),
    _fBoundingBox :: !(Maybe BoundingBox),
    _fExternalImageId :: !(Maybe Text),
    _fConfidence :: !(Maybe Double),
    _fImageId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
face ::
  Face
face =
  Face'
    { _fFaceId = Nothing,
      _fBoundingBox = Nothing,
      _fExternalImageId = Nothing,
      _fConfidence = Nothing,
      _fImageId = Nothing
    }

-- | Unique identifier that Amazon Rekognition assigns to the face.
fFaceId :: Lens' Face (Maybe Text)
fFaceId = lens _fFaceId (\s a -> s {_fFaceId = a})

-- | Bounding box of the face.
fBoundingBox :: Lens' Face (Maybe BoundingBox)
fBoundingBox = lens _fBoundingBox (\s a -> s {_fBoundingBox = a})

-- | Identifier that you assign to all the faces in the input image.
fExternalImageId :: Lens' Face (Maybe Text)
fExternalImageId = lens _fExternalImageId (\s a -> s {_fExternalImageId = a})

-- | Confidence level that the bounding box contains a face (and not a different object such as a tree).
fConfidence :: Lens' Face (Maybe Double)
fConfidence = lens _fConfidence (\s a -> s {_fConfidence = a})

-- | Unique identifier that Amazon Rekognition assigns to the input image.
fImageId :: Lens' Face (Maybe Text)
fImageId = lens _fImageId (\s a -> s {_fImageId = a})

instance FromJSON Face where
  parseJSON =
    withObject
      "Face"
      ( \x ->
          Face'
            <$> (x .:? "FaceId")
            <*> (x .:? "BoundingBox")
            <*> (x .:? "ExternalImageId")
            <*> (x .:? "Confidence")
            <*> (x .:? "ImageId")
      )

instance Hashable Face

instance NFData Face
