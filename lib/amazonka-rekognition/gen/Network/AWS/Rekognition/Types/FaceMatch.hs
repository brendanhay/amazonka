{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceMatch
  ( FaceMatch (..),

    -- * Smart constructor
    mkFaceMatch,

    -- * Lenses
    fmFace,
    fmSimilarity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.Face as Types

-- | Provides face metadata. In addition, it also provides the confidence in the match of this face with the input face.
--
-- /See:/ 'mkFaceMatch' smart constructor.
data FaceMatch = FaceMatch'
  { -- | Describes the face properties such as the bounding box, face ID, image ID of the source image, and external image ID that you assigned.
    face :: Core.Maybe Types.Face,
    -- | Confidence in the match of this face with the input face.
    similarity :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FaceMatch' value with any optional fields omitted.
mkFaceMatch ::
  FaceMatch
mkFaceMatch =
  FaceMatch' {face = Core.Nothing, similarity = Core.Nothing}

-- | Describes the face properties such as the bounding box, face ID, image ID of the source image, and external image ID that you assigned.
--
-- /Note:/ Consider using 'face' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmFace :: Lens.Lens' FaceMatch (Core.Maybe Types.Face)
fmFace = Lens.field @"face"
{-# DEPRECATED fmFace "Use generic-lens or generic-optics with 'face' instead." #-}

-- | Confidence in the match of this face with the input face.
--
-- /Note:/ Consider using 'similarity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmSimilarity :: Lens.Lens' FaceMatch (Core.Maybe Core.Double)
fmSimilarity = Lens.field @"similarity"
{-# DEPRECATED fmSimilarity "Use generic-lens or generic-optics with 'similarity' instead." #-}

instance Core.FromJSON FaceMatch where
  parseJSON =
    Core.withObject "FaceMatch" Core.$
      \x ->
        FaceMatch'
          Core.<$> (x Core..:? "Face") Core.<*> (x Core..:? "Similarity")
