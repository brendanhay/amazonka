{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ComparedSourceImageFace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.ComparedSourceImageFace
  ( ComparedSourceImageFace (..)
  -- * Smart constructor
  , mkComparedSourceImageFace
  -- * Lenses
  , csifBoundingBox
  , csifConfidence
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.BoundingBox as Types

-- | Type that describes the face Amazon Rekognition chose to compare with the faces in the target. This contains a bounding box for the selected face and confidence level that the bounding box contains a face. Note that Amazon Rekognition selects the largest face in the source image for this comparison. 
--
-- /See:/ 'mkComparedSourceImageFace' smart constructor.
data ComparedSourceImageFace = ComparedSourceImageFace'
  { boundingBox :: Core.Maybe Types.BoundingBox
    -- ^ Bounding box of the face.
  , confidence :: Core.Maybe Core.Double
    -- ^ Confidence level that the selected bounding box contains a face.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComparedSourceImageFace' value with any optional fields omitted.
mkComparedSourceImageFace
    :: ComparedSourceImageFace
mkComparedSourceImageFace
  = ComparedSourceImageFace'{boundingBox = Core.Nothing,
                             confidence = Core.Nothing}

-- | Bounding box of the face.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csifBoundingBox :: Lens.Lens' ComparedSourceImageFace (Core.Maybe Types.BoundingBox)
csifBoundingBox = Lens.field @"boundingBox"
{-# INLINEABLE csifBoundingBox #-}
{-# DEPRECATED boundingBox "Use generic-lens or generic-optics with 'boundingBox' instead"  #-}

-- | Confidence level that the selected bounding box contains a face.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csifConfidence :: Lens.Lens' ComparedSourceImageFace (Core.Maybe Core.Double)
csifConfidence = Lens.field @"confidence"
{-# INLINEABLE csifConfidence #-}
{-# DEPRECATED confidence "Use generic-lens or generic-optics with 'confidence' instead"  #-}

instance Core.FromJSON ComparedSourceImageFace where
        parseJSON
          = Core.withObject "ComparedSourceImageFace" Core.$
              \ x ->
                ComparedSourceImageFace' Core.<$>
                  (x Core..:? "BoundingBox") Core.<*> x Core..:? "Confidence"
