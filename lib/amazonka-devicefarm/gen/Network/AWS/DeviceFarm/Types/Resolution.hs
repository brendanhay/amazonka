{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Resolution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Resolution
  ( Resolution (..)
  -- * Smart constructor
  , mkResolution
  -- * Lenses
  , rHeight
  , rWidth
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the screen resolution of a device in height and width, expressed in pixels.
--
-- /See:/ 'mkResolution' smart constructor.
data Resolution = Resolution'
  { height :: Core.Maybe Core.Int
    -- ^ The screen resolution's height, expressed in pixels.
  , width :: Core.Maybe Core.Int
    -- ^ The screen resolution's width, expressed in pixels.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Resolution' value with any optional fields omitted.
mkResolution
    :: Resolution
mkResolution
  = Resolution'{height = Core.Nothing, width = Core.Nothing}

-- | The screen resolution's height, expressed in pixels.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rHeight :: Lens.Lens' Resolution (Core.Maybe Core.Int)
rHeight = Lens.field @"height"
{-# INLINEABLE rHeight #-}
{-# DEPRECATED height "Use generic-lens or generic-optics with 'height' instead"  #-}

-- | The screen resolution's width, expressed in pixels.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rWidth :: Lens.Lens' Resolution (Core.Maybe Core.Int)
rWidth = Lens.field @"width"
{-# INLINEABLE rWidth #-}
{-# DEPRECATED width "Use generic-lens or generic-optics with 'width' instead"  #-}

instance Core.FromJSON Resolution where
        parseJSON
          = Core.withObject "Resolution" Core.$
              \ x ->
                Resolution' Core.<$>
                  (x Core..:? "height") Core.<*> x Core..:? "width"
