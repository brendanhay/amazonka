-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.RegionOfInterest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.RegionOfInterest
  ( RegionOfInterest (..),

    -- * Smart constructor
    mkRegionOfInterest,

    -- * Lenses
    roiBoundingBox,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.BoundingBox

-- | Specifies a location within the frame that Rekognition checks for text. Uses a @BoundingBox@ object to set a region of the screen.
--
-- A word is included in the region if the word is more than half in that region. If there is more than one region, the word will be compared with all regions of the screen. Any word more than half in a region is kept in the results.
--
-- /See:/ 'mkRegionOfInterest' smart constructor.
newtype RegionOfInterest = RegionOfInterest'
  { boundingBox ::
      Lude.Maybe BoundingBox
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegionOfInterest' with the minimum fields required to make a request.
--
-- * 'boundingBox' - The box representing a region of interest on screen.
mkRegionOfInterest ::
  RegionOfInterest
mkRegionOfInterest = RegionOfInterest' {boundingBox = Lude.Nothing}

-- | The box representing a region of interest on screen.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roiBoundingBox :: Lens.Lens' RegionOfInterest (Lude.Maybe BoundingBox)
roiBoundingBox = Lens.lens (boundingBox :: RegionOfInterest -> Lude.Maybe BoundingBox) (\s a -> s {boundingBox = a} :: RegionOfInterest)
{-# DEPRECATED roiBoundingBox "Use generic-lens or generic-optics with 'boundingBox' instead." #-}

instance Lude.ToJSON RegionOfInterest where
  toJSON RegionOfInterest' {..} =
    Lude.object
      (Lude.catMaybes [("BoundingBox" Lude..=) Lude.<$> boundingBox])
