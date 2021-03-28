{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.AgeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.AgeRange
  ( AgeRange (..)
  -- * Smart constructor
  , mkAgeRange
  -- * Lenses
  , arHigh
  , arLow
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Structure containing the estimated age range, in years, for a face.
--
-- Amazon Rekognition estimates an age range for faces detected in the input image. Estimated age ranges can overlap. A face of a 5-year-old might have an estimated range of 4-6, while the face of a 6-year-old might have an estimated range of 4-8.
--
-- /See:/ 'mkAgeRange' smart constructor.
data AgeRange = AgeRange'
  { high :: Core.Maybe Core.Natural
    -- ^ The highest estimated age.
  , low :: Core.Maybe Core.Natural
    -- ^ The lowest estimated age.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AgeRange' value with any optional fields omitted.
mkAgeRange
    :: AgeRange
mkAgeRange = AgeRange'{high = Core.Nothing, low = Core.Nothing}

-- | The highest estimated age.
--
-- /Note:/ Consider using 'high' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arHigh :: Lens.Lens' AgeRange (Core.Maybe Core.Natural)
arHigh = Lens.field @"high"
{-# INLINEABLE arHigh #-}
{-# DEPRECATED high "Use generic-lens or generic-optics with 'high' instead"  #-}

-- | The lowest estimated age.
--
-- /Note:/ Consider using 'low' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arLow :: Lens.Lens' AgeRange (Core.Maybe Core.Natural)
arLow = Lens.field @"low"
{-# INLINEABLE arLow #-}
{-# DEPRECATED low "Use generic-lens or generic-optics with 'low' instead"  #-}

instance Core.FromJSON AgeRange where
        parseJSON
          = Core.withObject "AgeRange" Core.$
              \ x ->
                AgeRange' Core.<$> (x Core..:? "High") Core.<*> x Core..:? "Low"
