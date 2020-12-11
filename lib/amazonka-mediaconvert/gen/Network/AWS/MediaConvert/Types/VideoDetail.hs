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
import qualified Network.AWS.Prelude as Lude

-- | Contains details about the output's video stream
--
-- /See:/ 'mkVideoDetail' smart constructor.
data VideoDetail = VideoDetail'
  { heightInPx :: Lude.Maybe Lude.Int,
    widthInPx :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VideoDetail' with the minimum fields required to make a request.
--
-- * 'heightInPx' - Height in pixels for the output
-- * 'widthInPx' - Width in pixels for the output
mkVideoDetail ::
  VideoDetail
mkVideoDetail =
  VideoDetail' {heightInPx = Lude.Nothing, widthInPx = Lude.Nothing}

-- | Height in pixels for the output
--
-- /Note:/ Consider using 'heightInPx' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdHeightInPx :: Lens.Lens' VideoDetail (Lude.Maybe Lude.Int)
vdHeightInPx = Lens.lens (heightInPx :: VideoDetail -> Lude.Maybe Lude.Int) (\s a -> s {heightInPx = a} :: VideoDetail)
{-# DEPRECATED vdHeightInPx "Use generic-lens or generic-optics with 'heightInPx' instead." #-}

-- | Width in pixels for the output
--
-- /Note:/ Consider using 'widthInPx' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdWidthInPx :: Lens.Lens' VideoDetail (Lude.Maybe Lude.Int)
vdWidthInPx = Lens.lens (widthInPx :: VideoDetail -> Lude.Maybe Lude.Int) (\s a -> s {widthInPx = a} :: VideoDetail)
{-# DEPRECATED vdWidthInPx "Use generic-lens or generic-optics with 'widthInPx' instead." #-}

instance Lude.FromJSON VideoDetail where
  parseJSON =
    Lude.withObject
      "VideoDetail"
      ( \x ->
          VideoDetail'
            Lude.<$> (x Lude..:? "heightInPx") Lude.<*> (x Lude..:? "widthInPx")
      )
