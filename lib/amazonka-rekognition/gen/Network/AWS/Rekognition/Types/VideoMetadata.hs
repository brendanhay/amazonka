{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.VideoMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.VideoMetadata
  ( VideoMetadata (..)
  -- * Smart constructor
  , mkVideoMetadata
  -- * Lenses
  , vmCodec
  , vmDurationMillis
  , vmFormat
  , vmFrameHeight
  , vmFrameRate
  , vmFrameWidth
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition video operation.
--
-- /See:/ 'mkVideoMetadata' smart constructor.
data VideoMetadata = VideoMetadata'
  { codec :: Core.Maybe Core.Text
    -- ^ Type of compression used in the analyzed video. 
  , durationMillis :: Core.Maybe Core.Natural
    -- ^ Length of the video in milliseconds.
  , format :: Core.Maybe Core.Text
    -- ^ Format of the analyzed video. Possible values are MP4, MOV and AVI. 
  , frameHeight :: Core.Maybe Core.Natural
    -- ^ Vertical pixel dimension of the video.
  , frameRate :: Core.Maybe Core.Double
    -- ^ Number of frames per second in the video.
  , frameWidth :: Core.Maybe Core.Natural
    -- ^ Horizontal pixel dimension of the video.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VideoMetadata' value with any optional fields omitted.
mkVideoMetadata
    :: VideoMetadata
mkVideoMetadata
  = VideoMetadata'{codec = Core.Nothing,
                   durationMillis = Core.Nothing, format = Core.Nothing,
                   frameHeight = Core.Nothing, frameRate = Core.Nothing,
                   frameWidth = Core.Nothing}

-- | Type of compression used in the analyzed video. 
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmCodec :: Lens.Lens' VideoMetadata (Core.Maybe Core.Text)
vmCodec = Lens.field @"codec"
{-# INLINEABLE vmCodec #-}
{-# DEPRECATED codec "Use generic-lens or generic-optics with 'codec' instead"  #-}

-- | Length of the video in milliseconds.
--
-- /Note:/ Consider using 'durationMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmDurationMillis :: Lens.Lens' VideoMetadata (Core.Maybe Core.Natural)
vmDurationMillis = Lens.field @"durationMillis"
{-# INLINEABLE vmDurationMillis #-}
{-# DEPRECATED durationMillis "Use generic-lens or generic-optics with 'durationMillis' instead"  #-}

-- | Format of the analyzed video. Possible values are MP4, MOV and AVI. 
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmFormat :: Lens.Lens' VideoMetadata (Core.Maybe Core.Text)
vmFormat = Lens.field @"format"
{-# INLINEABLE vmFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | Vertical pixel dimension of the video.
--
-- /Note:/ Consider using 'frameHeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmFrameHeight :: Lens.Lens' VideoMetadata (Core.Maybe Core.Natural)
vmFrameHeight = Lens.field @"frameHeight"
{-# INLINEABLE vmFrameHeight #-}
{-# DEPRECATED frameHeight "Use generic-lens or generic-optics with 'frameHeight' instead"  #-}

-- | Number of frames per second in the video.
--
-- /Note:/ Consider using 'frameRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmFrameRate :: Lens.Lens' VideoMetadata (Core.Maybe Core.Double)
vmFrameRate = Lens.field @"frameRate"
{-# INLINEABLE vmFrameRate #-}
{-# DEPRECATED frameRate "Use generic-lens or generic-optics with 'frameRate' instead"  #-}

-- | Horizontal pixel dimension of the video.
--
-- /Note:/ Consider using 'frameWidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmFrameWidth :: Lens.Lens' VideoMetadata (Core.Maybe Core.Natural)
vmFrameWidth = Lens.field @"frameWidth"
{-# INLINEABLE vmFrameWidth #-}
{-# DEPRECATED frameWidth "Use generic-lens or generic-optics with 'frameWidth' instead"  #-}

instance Core.FromJSON VideoMetadata where
        parseJSON
          = Core.withObject "VideoMetadata" Core.$
              \ x ->
                VideoMetadata' Core.<$>
                  (x Core..:? "Codec") Core.<*> x Core..:? "DurationMillis" Core.<*>
                    x Core..:? "Format"
                    Core.<*> x Core..:? "FrameHeight"
                    Core.<*> x Core..:? "FrameRate"
                    Core.<*> x Core..:? "FrameWidth"
