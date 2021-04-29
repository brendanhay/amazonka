{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Face
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Face where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.BoundingBox

-- | Describes the face properties such as the bounding box, face ID, image
-- ID of the input image, and external image ID that you assigned.
--
-- /See:/ 'newFace' smart constructor.
data Face = Face'
  { -- | Unique identifier that Amazon Rekognition assigns to the face.
    faceId :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier that Amazon Rekognition assigns to the input image.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | Identifier that you assign to all the faces in the input image.
    externalImageId :: Prelude.Maybe Prelude.Text,
    -- | Bounding box of the face.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | Confidence level that the bounding box contains a face (and not a
    -- different object such as a tree).
    confidence :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Face' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceId', 'face_faceId' - Unique identifier that Amazon Rekognition assigns to the face.
--
-- 'imageId', 'face_imageId' - Unique identifier that Amazon Rekognition assigns to the input image.
--
-- 'externalImageId', 'face_externalImageId' - Identifier that you assign to all the faces in the input image.
--
-- 'boundingBox', 'face_boundingBox' - Bounding box of the face.
--
-- 'confidence', 'face_confidence' - Confidence level that the bounding box contains a face (and not a
-- different object such as a tree).
newFace ::
  Face
newFace =
  Face'
    { faceId = Prelude.Nothing,
      imageId = Prelude.Nothing,
      externalImageId = Prelude.Nothing,
      boundingBox = Prelude.Nothing,
      confidence = Prelude.Nothing
    }

-- | Unique identifier that Amazon Rekognition assigns to the face.
face_faceId :: Lens.Lens' Face (Prelude.Maybe Prelude.Text)
face_faceId = Lens.lens (\Face' {faceId} -> faceId) (\s@Face' {} a -> s {faceId = a} :: Face)

-- | Unique identifier that Amazon Rekognition assigns to the input image.
face_imageId :: Lens.Lens' Face (Prelude.Maybe Prelude.Text)
face_imageId = Lens.lens (\Face' {imageId} -> imageId) (\s@Face' {} a -> s {imageId = a} :: Face)

-- | Identifier that you assign to all the faces in the input image.
face_externalImageId :: Lens.Lens' Face (Prelude.Maybe Prelude.Text)
face_externalImageId = Lens.lens (\Face' {externalImageId} -> externalImageId) (\s@Face' {} a -> s {externalImageId = a} :: Face)

-- | Bounding box of the face.
face_boundingBox :: Lens.Lens' Face (Prelude.Maybe BoundingBox)
face_boundingBox = Lens.lens (\Face' {boundingBox} -> boundingBox) (\s@Face' {} a -> s {boundingBox = a} :: Face)

-- | Confidence level that the bounding box contains a face (and not a
-- different object such as a tree).
face_confidence :: Lens.Lens' Face (Prelude.Maybe Prelude.Double)
face_confidence = Lens.lens (\Face' {confidence} -> confidence) (\s@Face' {} a -> s {confidence = a} :: Face)

instance Prelude.FromJSON Face where
  parseJSON =
    Prelude.withObject
      "Face"
      ( \x ->
          Face'
            Prelude.<$> (x Prelude..:? "FaceId")
            Prelude.<*> (x Prelude..:? "ImageId")
            Prelude.<*> (x Prelude..:? "ExternalImageId")
            Prelude.<*> (x Prelude..:? "BoundingBox")
            Prelude.<*> (x Prelude..:? "Confidence")
      )

instance Prelude.Hashable Face

instance Prelude.NFData Face
