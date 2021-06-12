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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.BoundingBox

-- | Describes the face properties such as the bounding box, face ID, image
-- ID of the input image, and external image ID that you assigned.
--
-- /See:/ 'newFace' smart constructor.
data Face = Face'
  { -- | Unique identifier that Amazon Rekognition assigns to the face.
    faceId :: Core.Maybe Core.Text,
    -- | Unique identifier that Amazon Rekognition assigns to the input image.
    imageId :: Core.Maybe Core.Text,
    -- | Identifier that you assign to all the faces in the input image.
    externalImageId :: Core.Maybe Core.Text,
    -- | Bounding box of the face.
    boundingBox :: Core.Maybe BoundingBox,
    -- | Confidence level that the bounding box contains a face (and not a
    -- different object such as a tree).
    confidence :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { faceId = Core.Nothing,
      imageId = Core.Nothing,
      externalImageId = Core.Nothing,
      boundingBox = Core.Nothing,
      confidence = Core.Nothing
    }

-- | Unique identifier that Amazon Rekognition assigns to the face.
face_faceId :: Lens.Lens' Face (Core.Maybe Core.Text)
face_faceId = Lens.lens (\Face' {faceId} -> faceId) (\s@Face' {} a -> s {faceId = a} :: Face)

-- | Unique identifier that Amazon Rekognition assigns to the input image.
face_imageId :: Lens.Lens' Face (Core.Maybe Core.Text)
face_imageId = Lens.lens (\Face' {imageId} -> imageId) (\s@Face' {} a -> s {imageId = a} :: Face)

-- | Identifier that you assign to all the faces in the input image.
face_externalImageId :: Lens.Lens' Face (Core.Maybe Core.Text)
face_externalImageId = Lens.lens (\Face' {externalImageId} -> externalImageId) (\s@Face' {} a -> s {externalImageId = a} :: Face)

-- | Bounding box of the face.
face_boundingBox :: Lens.Lens' Face (Core.Maybe BoundingBox)
face_boundingBox = Lens.lens (\Face' {boundingBox} -> boundingBox) (\s@Face' {} a -> s {boundingBox = a} :: Face)

-- | Confidence level that the bounding box contains a face (and not a
-- different object such as a tree).
face_confidence :: Lens.Lens' Face (Core.Maybe Core.Double)
face_confidence = Lens.lens (\Face' {confidence} -> confidence) (\s@Face' {} a -> s {confidence = a} :: Face)

instance Core.FromJSON Face where
  parseJSON =
    Core.withObject
      "Face"
      ( \x ->
          Face'
            Core.<$> (x Core..:? "FaceId")
            Core.<*> (x Core..:? "ImageId")
            Core.<*> (x Core..:? "ExternalImageId")
            Core.<*> (x Core..:? "BoundingBox")
            Core.<*> (x Core..:? "Confidence")
      )

instance Core.Hashable Face

instance Core.NFData Face
