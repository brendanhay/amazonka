{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.DetectTextFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.DetectTextFilters
  ( DetectTextFilters (..)
  -- * Smart constructor
  , mkDetectTextFilters
  -- * Lenses
  , dtfRegionsOfInterest
  , dtfWordFilter
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.DetectionFilter as Types
import qualified Network.AWS.Rekognition.Types.RegionOfInterest as Types

-- | A set of optional parameters that you can use to set the criteria that the text must meet to be included in your response. @WordFilter@ looks at a word’s height, width, and minimum confidence. @RegionOfInterest@ lets you set a specific region of the image to look for text in. 
--
-- /See:/ 'mkDetectTextFilters' smart constructor.
data DetectTextFilters = DetectTextFilters'
  { regionsOfInterest :: Core.Maybe [Types.RegionOfInterest]
    -- ^ A Filter focusing on a certain area of the image. Uses a @BoundingBox@ object to set the region of the image.
  , wordFilter :: Core.Maybe Types.DetectionFilter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectTextFilters' value with any optional fields omitted.
mkDetectTextFilters
    :: DetectTextFilters
mkDetectTextFilters
  = DetectTextFilters'{regionsOfInterest = Core.Nothing,
                       wordFilter = Core.Nothing}

-- | A Filter focusing on a certain area of the image. Uses a @BoundingBox@ object to set the region of the image.
--
-- /Note:/ Consider using 'regionsOfInterest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfRegionsOfInterest :: Lens.Lens' DetectTextFilters (Core.Maybe [Types.RegionOfInterest])
dtfRegionsOfInterest = Lens.field @"regionsOfInterest"
{-# INLINEABLE dtfRegionsOfInterest #-}
{-# DEPRECATED regionsOfInterest "Use generic-lens or generic-optics with 'regionsOfInterest' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'wordFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfWordFilter :: Lens.Lens' DetectTextFilters (Core.Maybe Types.DetectionFilter)
dtfWordFilter = Lens.field @"wordFilter"
{-# INLINEABLE dtfWordFilter #-}
{-# DEPRECATED wordFilter "Use generic-lens or generic-optics with 'wordFilter' instead"  #-}

instance Core.FromJSON DetectTextFilters where
        toJSON DetectTextFilters{..}
          = Core.object
              (Core.catMaybes
                 [("RegionsOfInterest" Core..=) Core.<$> regionsOfInterest,
                  ("WordFilter" Core..=) Core.<$> wordFilter])
