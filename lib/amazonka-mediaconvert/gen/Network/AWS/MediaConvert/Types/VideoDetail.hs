{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoDetail
  ( VideoDetail (..),

    -- * Smart constructor
    mkVideoDetail,

    -- * Lenses
    vdHeightInPx,
    vdWidthInPx,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains details about the output's video stream
--
-- /See:/ 'mkVideoDetail' smart constructor.
data VideoDetail = VideoDetail'
  { -- | Height in pixels for the output
    heightInPx :: Core.Maybe Core.Int,
    -- | Width in pixels for the output
    widthInPx :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VideoDetail' value with any optional fields omitted.
mkVideoDetail ::
  VideoDetail
mkVideoDetail =
  VideoDetail' {heightInPx = Core.Nothing, widthInPx = Core.Nothing}

-- | Height in pixels for the output
--
-- /Note:/ Consider using 'heightInPx' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdHeightInPx :: Lens.Lens' VideoDetail (Core.Maybe Core.Int)
vdHeightInPx = Lens.field @"heightInPx"
{-# DEPRECATED vdHeightInPx "Use generic-lens or generic-optics with 'heightInPx' instead." #-}

-- | Width in pixels for the output
--
-- /Note:/ Consider using 'widthInPx' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdWidthInPx :: Lens.Lens' VideoDetail (Core.Maybe Core.Int)
vdWidthInPx = Lens.field @"widthInPx"
{-# DEPRECATED vdWidthInPx "Use generic-lens or generic-optics with 'widthInPx' instead." #-}

instance Core.FromJSON VideoDetail where
  parseJSON =
    Core.withObject "VideoDetail" Core.$
      \x ->
        VideoDetail'
          Core.<$> (x Core..:? "heightInPx") Core.<*> (x Core..:? "widthInPx")
