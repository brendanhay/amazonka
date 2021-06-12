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
-- Module      : Network.AWS.Rekognition.Types.ComparedFace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ComparedFace where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.ImageQuality
import Network.AWS.Rekognition.Types.Landmark
import Network.AWS.Rekognition.Types.Pose

-- | Provides face metadata for target image faces that are analyzed by
-- @CompareFaces@ and @RecognizeCelebrities@.
--
-- /See:/ 'newComparedFace' smart constructor.
data ComparedFace = ComparedFace'
  { -- | Indicates the pose of the face as determined by its pitch, roll, and
    -- yaw.
    pose :: Core.Maybe Pose,
    -- | An array of facial landmarks.
    landmarks :: Core.Maybe [Landmark],
    -- | Bounding box of the face.
    boundingBox :: Core.Maybe BoundingBox,
    -- | Level of confidence that what the bounding box contains is a face.
    confidence :: Core.Maybe Core.Double,
    -- | Identifies face image brightness and sharpness.
    quality :: Core.Maybe ImageQuality
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ComparedFace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pose', 'comparedFace_pose' - Indicates the pose of the face as determined by its pitch, roll, and
-- yaw.
--
-- 'landmarks', 'comparedFace_landmarks' - An array of facial landmarks.
--
-- 'boundingBox', 'comparedFace_boundingBox' - Bounding box of the face.
--
-- 'confidence', 'comparedFace_confidence' - Level of confidence that what the bounding box contains is a face.
--
-- 'quality', 'comparedFace_quality' - Identifies face image brightness and sharpness.
newComparedFace ::
  ComparedFace
newComparedFace =
  ComparedFace'
    { pose = Core.Nothing,
      landmarks = Core.Nothing,
      boundingBox = Core.Nothing,
      confidence = Core.Nothing,
      quality = Core.Nothing
    }

-- | Indicates the pose of the face as determined by its pitch, roll, and
-- yaw.
comparedFace_pose :: Lens.Lens' ComparedFace (Core.Maybe Pose)
comparedFace_pose = Lens.lens (\ComparedFace' {pose} -> pose) (\s@ComparedFace' {} a -> s {pose = a} :: ComparedFace)

-- | An array of facial landmarks.
comparedFace_landmarks :: Lens.Lens' ComparedFace (Core.Maybe [Landmark])
comparedFace_landmarks = Lens.lens (\ComparedFace' {landmarks} -> landmarks) (\s@ComparedFace' {} a -> s {landmarks = a} :: ComparedFace) Core.. Lens.mapping Lens._Coerce

-- | Bounding box of the face.
comparedFace_boundingBox :: Lens.Lens' ComparedFace (Core.Maybe BoundingBox)
comparedFace_boundingBox = Lens.lens (\ComparedFace' {boundingBox} -> boundingBox) (\s@ComparedFace' {} a -> s {boundingBox = a} :: ComparedFace)

-- | Level of confidence that what the bounding box contains is a face.
comparedFace_confidence :: Lens.Lens' ComparedFace (Core.Maybe Core.Double)
comparedFace_confidence = Lens.lens (\ComparedFace' {confidence} -> confidence) (\s@ComparedFace' {} a -> s {confidence = a} :: ComparedFace)

-- | Identifies face image brightness and sharpness.
comparedFace_quality :: Lens.Lens' ComparedFace (Core.Maybe ImageQuality)
comparedFace_quality = Lens.lens (\ComparedFace' {quality} -> quality) (\s@ComparedFace' {} a -> s {quality = a} :: ComparedFace)

instance Core.FromJSON ComparedFace where
  parseJSON =
    Core.withObject
      "ComparedFace"
      ( \x ->
          ComparedFace'
            Core.<$> (x Core..:? "Pose")
            Core.<*> (x Core..:? "Landmarks" Core..!= Core.mempty)
            Core.<*> (x Core..:? "BoundingBox")
            Core.<*> (x Core..:? "Confidence")
            Core.<*> (x Core..:? "Quality")
      )

instance Core.Hashable ComparedFace

instance Core.NFData ComparedFace
