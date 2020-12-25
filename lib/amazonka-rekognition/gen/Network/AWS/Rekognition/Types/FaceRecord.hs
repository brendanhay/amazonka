{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceRecord
  ( FaceRecord (..),

    -- * Smart constructor
    mkFaceRecord,

    -- * Lenses
    frFace,
    frFaceDetail,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.Face as Types
import qualified Network.AWS.Rekognition.Types.FaceDetail as Types

-- | Object containing both the face metadata (stored in the backend database), and facial attributes that are detected but aren't stored in the database.
--
-- /See:/ 'mkFaceRecord' smart constructor.
data FaceRecord = FaceRecord'
  { -- | Describes the face properties such as the bounding box, face ID, image ID of the input image, and external image ID that you assigned.
    face :: Core.Maybe Types.Face,
    -- | Structure containing attributes of the face that the algorithm detected.
    faceDetail :: Core.Maybe Types.FaceDetail
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FaceRecord' value with any optional fields omitted.
mkFaceRecord ::
  FaceRecord
mkFaceRecord =
  FaceRecord' {face = Core.Nothing, faceDetail = Core.Nothing}

-- | Describes the face properties such as the bounding box, face ID, image ID of the input image, and external image ID that you assigned.
--
-- /Note:/ Consider using 'face' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frFace :: Lens.Lens' FaceRecord (Core.Maybe Types.Face)
frFace = Lens.field @"face"
{-# DEPRECATED frFace "Use generic-lens or generic-optics with 'face' instead." #-}

-- | Structure containing attributes of the face that the algorithm detected.
--
-- /Note:/ Consider using 'faceDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frFaceDetail :: Lens.Lens' FaceRecord (Core.Maybe Types.FaceDetail)
frFaceDetail = Lens.field @"faceDetail"
{-# DEPRECATED frFaceDetail "Use generic-lens or generic-optics with 'faceDetail' instead." #-}

instance Core.FromJSON FaceRecord where
  parseJSON =
    Core.withObject "FaceRecord" Core.$
      \x ->
        FaceRecord'
          Core.<$> (x Core..:? "Face") Core.<*> (x Core..:? "FaceDetail")
