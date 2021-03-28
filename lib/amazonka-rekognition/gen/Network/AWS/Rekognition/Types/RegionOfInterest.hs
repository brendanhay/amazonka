{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.RegionOfInterest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.RegionOfInterest
  ( RegionOfInterest (..)
  -- * Smart constructor
  , mkRegionOfInterest
  -- * Lenses
  , roiBoundingBox
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.BoundingBox as Types

-- | Specifies a location within the frame that Rekognition checks for text. Uses a @BoundingBox@ object to set a region of the screen.
--
-- A word is included in the region if the word is more than half in that region. If there is more than one region, the word will be compared with all regions of the screen. Any word more than half in a region is kept in the results.
--
-- /See:/ 'mkRegionOfInterest' smart constructor.
newtype RegionOfInterest = RegionOfInterest'
  { boundingBox :: Core.Maybe Types.BoundingBox
    -- ^ The box representing a region of interest on screen.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RegionOfInterest' value with any optional fields omitted.
mkRegionOfInterest
    :: RegionOfInterest
mkRegionOfInterest = RegionOfInterest'{boundingBox = Core.Nothing}

-- | The box representing a region of interest on screen.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roiBoundingBox :: Lens.Lens' RegionOfInterest (Core.Maybe Types.BoundingBox)
roiBoundingBox = Lens.field @"boundingBox"
{-# INLINEABLE roiBoundingBox #-}
{-# DEPRECATED boundingBox "Use generic-lens or generic-optics with 'boundingBox' instead"  #-}

instance Core.FromJSON RegionOfInterest where
        toJSON RegionOfInterest{..}
          = Core.object
              (Core.catMaybes [("BoundingBox" Core..=) Core.<$> boundingBox])
