{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.DetectTextFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.DetectTextFilters
  ( DetectTextFilters (..),

    -- * Smart constructor
    mkDetectTextFilters,

    -- * Lenses
    dtfRegionsOfInterest,
    dtfWordFilter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.DetectionFilter
import Network.AWS.Rekognition.Types.RegionOfInterest

-- | A set of optional parameters that you can use to set the criteria that the text must meet to be included in your response. @WordFilter@ looks at a word’s height, width, and minimum confidence. @RegionOfInterest@ lets you set a specific region of the image to look for text in.
--
-- /See:/ 'mkDetectTextFilters' smart constructor.
data DetectTextFilters = DetectTextFilters'
  { -- | A Filter focusing on a certain area of the image. Uses a @BoundingBox@ object to set the region of the image.
    regionsOfInterest :: Lude.Maybe [RegionOfInterest],
    wordFilter :: Lude.Maybe DetectionFilter
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectTextFilters' with the minimum fields required to make a request.
--
-- * 'regionsOfInterest' - A Filter focusing on a certain area of the image. Uses a @BoundingBox@ object to set the region of the image.
-- * 'wordFilter' -
mkDetectTextFilters ::
  DetectTextFilters
mkDetectTextFilters =
  DetectTextFilters'
    { regionsOfInterest = Lude.Nothing,
      wordFilter = Lude.Nothing
    }

-- | A Filter focusing on a certain area of the image. Uses a @BoundingBox@ object to set the region of the image.
--
-- /Note:/ Consider using 'regionsOfInterest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfRegionsOfInterest :: Lens.Lens' DetectTextFilters (Lude.Maybe [RegionOfInterest])
dtfRegionsOfInterest = Lens.lens (regionsOfInterest :: DetectTextFilters -> Lude.Maybe [RegionOfInterest]) (\s a -> s {regionsOfInterest = a} :: DetectTextFilters)
{-# DEPRECATED dtfRegionsOfInterest "Use generic-lens or generic-optics with 'regionsOfInterest' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'wordFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfWordFilter :: Lens.Lens' DetectTextFilters (Lude.Maybe DetectionFilter)
dtfWordFilter = Lens.lens (wordFilter :: DetectTextFilters -> Lude.Maybe DetectionFilter) (\s a -> s {wordFilter = a} :: DetectTextFilters)
{-# DEPRECATED dtfWordFilter "Use generic-lens or generic-optics with 'wordFilter' instead." #-}

instance Lude.ToJSON DetectTextFilters where
  toJSON DetectTextFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RegionsOfInterest" Lude..=) Lude.<$> regionsOfInterest,
            ("WordFilter" Lude..=) Lude.<$> wordFilter
          ]
      )
