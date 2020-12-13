{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StartTextDetectionFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StartTextDetectionFilters
  ( StartTextDetectionFilters (..),

    -- * Smart constructor
    mkStartTextDetectionFilters,

    -- * Lenses
    stdfRegionsOfInterest,
    stdfWordFilter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.DetectionFilter
import Network.AWS.Rekognition.Types.RegionOfInterest

-- | Set of optional parameters that let you set the criteria text must meet to be included in your response. @WordFilter@ looks at a word's height, width and minimum confidence. @RegionOfInterest@ lets you set a specific region of the screen to look for text in.
--
-- /See:/ 'mkStartTextDetectionFilters' smart constructor.
data StartTextDetectionFilters = StartTextDetectionFilters'
  { -- | Filter focusing on a certain area of the frame. Uses a @BoundingBox@ object to set the region of the screen.
    regionsOfInterest :: Lude.Maybe [RegionOfInterest],
    -- | Filters focusing on qualities of the text, such as confidence or size.
    wordFilter :: Lude.Maybe DetectionFilter
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartTextDetectionFilters' with the minimum fields required to make a request.
--
-- * 'regionsOfInterest' - Filter focusing on a certain area of the frame. Uses a @BoundingBox@ object to set the region of the screen.
-- * 'wordFilter' - Filters focusing on qualities of the text, such as confidence or size.
mkStartTextDetectionFilters ::
  StartTextDetectionFilters
mkStartTextDetectionFilters =
  StartTextDetectionFilters'
    { regionsOfInterest = Lude.Nothing,
      wordFilter = Lude.Nothing
    }

-- | Filter focusing on a certain area of the frame. Uses a @BoundingBox@ object to set the region of the screen.
--
-- /Note:/ Consider using 'regionsOfInterest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdfRegionsOfInterest :: Lens.Lens' StartTextDetectionFilters (Lude.Maybe [RegionOfInterest])
stdfRegionsOfInterest = Lens.lens (regionsOfInterest :: StartTextDetectionFilters -> Lude.Maybe [RegionOfInterest]) (\s a -> s {regionsOfInterest = a} :: StartTextDetectionFilters)
{-# DEPRECATED stdfRegionsOfInterest "Use generic-lens or generic-optics with 'regionsOfInterest' instead." #-}

-- | Filters focusing on qualities of the text, such as confidence or size.
--
-- /Note:/ Consider using 'wordFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdfWordFilter :: Lens.Lens' StartTextDetectionFilters (Lude.Maybe DetectionFilter)
stdfWordFilter = Lens.lens (wordFilter :: StartTextDetectionFilters -> Lude.Maybe DetectionFilter) (\s a -> s {wordFilter = a} :: StartTextDetectionFilters)
{-# DEPRECATED stdfWordFilter "Use generic-lens or generic-optics with 'wordFilter' instead." #-}

instance Lude.ToJSON StartTextDetectionFilters where
  toJSON StartTextDetectionFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RegionsOfInterest" Lude..=) Lude.<$> regionsOfInterest,
            ("WordFilter" Lude..=) Lude.<$> wordFilter
          ]
      )
