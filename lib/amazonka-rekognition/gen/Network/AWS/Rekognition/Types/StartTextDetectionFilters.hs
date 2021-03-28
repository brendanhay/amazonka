{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StartTextDetectionFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.StartTextDetectionFilters
  ( StartTextDetectionFilters (..)
  -- * Smart constructor
  , mkStartTextDetectionFilters
  -- * Lenses
  , stdfRegionsOfInterest
  , stdfWordFilter
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.DetectionFilter as Types
import qualified Network.AWS.Rekognition.Types.RegionOfInterest as Types

-- | Set of optional parameters that let you set the criteria text must meet to be included in your response. @WordFilter@ looks at a word's height, width and minimum confidence. @RegionOfInterest@ lets you set a specific region of the screen to look for text in.
--
-- /See:/ 'mkStartTextDetectionFilters' smart constructor.
data StartTextDetectionFilters = StartTextDetectionFilters'
  { regionsOfInterest :: Core.Maybe [Types.RegionOfInterest]
    -- ^ Filter focusing on a certain area of the frame. Uses a @BoundingBox@ object to set the region of the screen.
  , wordFilter :: Core.Maybe Types.DetectionFilter
    -- ^ Filters focusing on qualities of the text, such as confidence or size.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartTextDetectionFilters' value with any optional fields omitted.
mkStartTextDetectionFilters
    :: StartTextDetectionFilters
mkStartTextDetectionFilters
  = StartTextDetectionFilters'{regionsOfInterest = Core.Nothing,
                               wordFilter = Core.Nothing}

-- | Filter focusing on a certain area of the frame. Uses a @BoundingBox@ object to set the region of the screen.
--
-- /Note:/ Consider using 'regionsOfInterest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdfRegionsOfInterest :: Lens.Lens' StartTextDetectionFilters (Core.Maybe [Types.RegionOfInterest])
stdfRegionsOfInterest = Lens.field @"regionsOfInterest"
{-# INLINEABLE stdfRegionsOfInterest #-}
{-# DEPRECATED regionsOfInterest "Use generic-lens or generic-optics with 'regionsOfInterest' instead"  #-}

-- | Filters focusing on qualities of the text, such as confidence or size.
--
-- /Note:/ Consider using 'wordFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdfWordFilter :: Lens.Lens' StartTextDetectionFilters (Core.Maybe Types.DetectionFilter)
stdfWordFilter = Lens.field @"wordFilter"
{-# INLINEABLE stdfWordFilter #-}
{-# DEPRECATED wordFilter "Use generic-lens or generic-optics with 'wordFilter' instead"  #-}

instance Core.FromJSON StartTextDetectionFilters where
        toJSON StartTextDetectionFilters{..}
          = Core.object
              (Core.catMaybes
                 [("RegionsOfInterest" Core..=) Core.<$> regionsOfInterest,
                  ("WordFilter" Core..=) Core.<$> wordFilter])
