{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.CompareFacesMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.CompareFacesMatch
  ( CompareFacesMatch (..)
  -- * Smart constructor
  , mkCompareFacesMatch
  -- * Lenses
  , cfmFace
  , cfmSimilarity
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.ComparedFace as Types

-- | Provides information about a face in a target image that matches the source image face analyzed by @CompareFaces@ . The @Face@ property contains the bounding box of the face in the target image. The @Similarity@ property is the confidence that the source image face matches the face in the bounding box.
--
-- /See:/ 'mkCompareFacesMatch' smart constructor.
data CompareFacesMatch = CompareFacesMatch'
  { face :: Core.Maybe Types.ComparedFace
    -- ^ Provides face metadata (bounding box and confidence that the bounding box actually contains a face).
  , similarity :: Core.Maybe Core.Double
    -- ^ Level of confidence that the faces match.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompareFacesMatch' value with any optional fields omitted.
mkCompareFacesMatch
    :: CompareFacesMatch
mkCompareFacesMatch
  = CompareFacesMatch'{face = Core.Nothing,
                       similarity = Core.Nothing}

-- | Provides face metadata (bounding box and confidence that the bounding box actually contains a face).
--
-- /Note:/ Consider using 'face' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfmFace :: Lens.Lens' CompareFacesMatch (Core.Maybe Types.ComparedFace)
cfmFace = Lens.field @"face"
{-# INLINEABLE cfmFace #-}
{-# DEPRECATED face "Use generic-lens or generic-optics with 'face' instead"  #-}

-- | Level of confidence that the faces match.
--
-- /Note:/ Consider using 'similarity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfmSimilarity :: Lens.Lens' CompareFacesMatch (Core.Maybe Core.Double)
cfmSimilarity = Lens.field @"similarity"
{-# INLINEABLE cfmSimilarity #-}
{-# DEPRECATED similarity "Use generic-lens or generic-optics with 'similarity' instead"  #-}

instance Core.FromJSON CompareFacesMatch where
        parseJSON
          = Core.withObject "CompareFacesMatch" Core.$
              \ x ->
                CompareFacesMatch' Core.<$>
                  (x Core..:? "Face") Core.<*> x Core..:? "Similarity"
