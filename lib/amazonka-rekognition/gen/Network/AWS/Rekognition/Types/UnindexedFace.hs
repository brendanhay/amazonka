{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.UnindexedFace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.UnindexedFace
  ( UnindexedFace (..),

    -- * Smart constructor
    mkUnindexedFace,

    -- * Lenses
    ufFaceDetail,
    ufReasons,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.FaceDetail as Types
import qualified Network.AWS.Rekognition.Types.Reason as Types

-- | A face that 'IndexFaces' detected, but didn't index. Use the @Reasons@ response attribute to determine why a face wasn't indexed.
--
-- /See:/ 'mkUnindexedFace' smart constructor.
data UnindexedFace = UnindexedFace'
  { -- | The structure that contains attributes of a face that @IndexFaces@ detected, but didn't index.
    faceDetail :: Core.Maybe Types.FaceDetail,
    -- | An array of reasons that specify why a face wasn't indexed.
    --
    --
    --     * EXTREME_POSE - The face is at a pose that can't be detected. For example, the head is turned too far away from the camera.
    --
    --
    --     * EXCEEDS_MAX_FACES - The number of faces detected is already higher than that specified by the @MaxFaces@ input parameter for @IndexFaces@ .
    --
    --
    --     * LOW_BRIGHTNESS - The image is too dark.
    --
    --
    --     * LOW_SHARPNESS - The image is too blurry.
    --
    --
    --     * LOW_CONFIDENCE - The face was detected with a low confidence.
    --
    --
    --     * SMALL_BOUNDING_BOX - The bounding box around the face is too small.
    reasons :: Core.Maybe [Types.Reason]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnindexedFace' value with any optional fields omitted.
mkUnindexedFace ::
  UnindexedFace
mkUnindexedFace =
  UnindexedFace' {faceDetail = Core.Nothing, reasons = Core.Nothing}

-- | The structure that contains attributes of a face that @IndexFaces@ detected, but didn't index.
--
-- /Note:/ Consider using 'faceDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufFaceDetail :: Lens.Lens' UnindexedFace (Core.Maybe Types.FaceDetail)
ufFaceDetail = Lens.field @"faceDetail"
{-# DEPRECATED ufFaceDetail "Use generic-lens or generic-optics with 'faceDetail' instead." #-}

-- | An array of reasons that specify why a face wasn't indexed.
--
--
--     * EXTREME_POSE - The face is at a pose that can't be detected. For example, the head is turned too far away from the camera.
--
--
--     * EXCEEDS_MAX_FACES - The number of faces detected is already higher than that specified by the @MaxFaces@ input parameter for @IndexFaces@ .
--
--
--     * LOW_BRIGHTNESS - The image is too dark.
--
--
--     * LOW_SHARPNESS - The image is too blurry.
--
--
--     * LOW_CONFIDENCE - The face was detected with a low confidence.
--
--
--     * SMALL_BOUNDING_BOX - The bounding box around the face is too small.
--
--
--
-- /Note:/ Consider using 'reasons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufReasons :: Lens.Lens' UnindexedFace (Core.Maybe [Types.Reason])
ufReasons = Lens.field @"reasons"
{-# DEPRECATED ufReasons "Use generic-lens or generic-optics with 'reasons' instead." #-}

instance Core.FromJSON UnindexedFace where
  parseJSON =
    Core.withObject "UnindexedFace" Core.$
      \x ->
        UnindexedFace'
          Core.<$> (x Core..:? "FaceDetail") Core.<*> (x Core..:? "Reasons")
