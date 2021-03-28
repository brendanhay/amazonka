{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ImageQuality
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.ImageQuality
  ( ImageQuality (..)
  -- * Smart constructor
  , mkImageQuality
  -- * Lenses
  , iqBrightness
  , iqSharpness
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifies face image brightness and sharpness. 
--
-- /See:/ 'mkImageQuality' smart constructor.
data ImageQuality = ImageQuality'
  { brightness :: Core.Maybe Core.Double
    -- ^ Value representing brightness of the face. The service returns a value between 0 and 100 (inclusive). A higher value indicates a brighter face image.
  , sharpness :: Core.Maybe Core.Double
    -- ^ Value representing sharpness of the face. The service returns a value between 0 and 100 (inclusive). A higher value indicates a sharper face image.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImageQuality' value with any optional fields omitted.
mkImageQuality
    :: ImageQuality
mkImageQuality
  = ImageQuality'{brightness = Core.Nothing,
                  sharpness = Core.Nothing}

-- | Value representing brightness of the face. The service returns a value between 0 and 100 (inclusive). A higher value indicates a brighter face image.
--
-- /Note:/ Consider using 'brightness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iqBrightness :: Lens.Lens' ImageQuality (Core.Maybe Core.Double)
iqBrightness = Lens.field @"brightness"
{-# INLINEABLE iqBrightness #-}
{-# DEPRECATED brightness "Use generic-lens or generic-optics with 'brightness' instead"  #-}

-- | Value representing sharpness of the face. The service returns a value between 0 and 100 (inclusive). A higher value indicates a sharper face image.
--
-- /Note:/ Consider using 'sharpness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iqSharpness :: Lens.Lens' ImageQuality (Core.Maybe Core.Double)
iqSharpness = Lens.field @"sharpness"
{-# INLINEABLE iqSharpness #-}
{-# DEPRECATED sharpness "Use generic-lens or generic-optics with 'sharpness' instead"  #-}

instance Core.FromJSON ImageQuality where
        parseJSON
          = Core.withObject "ImageQuality" Core.$
              \ x ->
                ImageQuality' Core.<$>
                  (x Core..:? "Brightness") Core.<*> x Core..:? "Sharpness"
