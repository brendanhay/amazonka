{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.VideoMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.VideoMetadata
  ( VideoMetadata (..),

    -- * Smart constructor
    mkVideoMetadata,

    -- * Lenses
    vmCodec,
    vmDurationMillis,
    vmFormat,
    vmFrameHeight,
    vmFrameRate,
    vmFrameWidth,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.String as Types

-- | Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition video operation.
--
-- /See:/ 'mkVideoMetadata' smart constructor.
data VideoMetadata = VideoMetadata'
  { -- | Type of compression used in the analyzed video.
    codec :: Core.Maybe Types.String,
    -- | Length of the video in milliseconds.
    durationMillis :: Core.Maybe Core.Natural,
    -- | Format of the analyzed video. Possible values are MP4, MOV and AVI.
    format :: Core.Maybe Types.String,
    -- | Vertical pixel dimension of the video.
    frameHeight :: Core.Maybe Core.Natural,
    -- | Number of frames per second in the video.
    frameRate :: Core.Maybe Core.Double,
    -- | Horizontal pixel dimension of the video.
    frameWidth :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VideoMetadata' value with any optional fields omitted.
mkVideoMetadata ::
  VideoMetadata
mkVideoMetadata =
  VideoMetadata'
    { codec = Core.Nothing,
      durationMillis = Core.Nothing,
      format = Core.Nothing,
      frameHeight = Core.Nothing,
      frameRate = Core.Nothing,
      frameWidth = Core.Nothing
    }

-- | Type of compression used in the analyzed video.
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmCodec :: Lens.Lens' VideoMetadata (Core.Maybe Types.String)
vmCodec = Lens.field @"codec"
{-# DEPRECATED vmCodec "Use generic-lens or generic-optics with 'codec' instead." #-}

-- | Length of the video in milliseconds.
--
-- /Note:/ Consider using 'durationMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmDurationMillis :: Lens.Lens' VideoMetadata (Core.Maybe Core.Natural)
vmDurationMillis = Lens.field @"durationMillis"
{-# DEPRECATED vmDurationMillis "Use generic-lens or generic-optics with 'durationMillis' instead." #-}

-- | Format of the analyzed video. Possible values are MP4, MOV and AVI.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmFormat :: Lens.Lens' VideoMetadata (Core.Maybe Types.String)
vmFormat = Lens.field @"format"
{-# DEPRECATED vmFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | Vertical pixel dimension of the video.
--
-- /Note:/ Consider using 'frameHeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmFrameHeight :: Lens.Lens' VideoMetadata (Core.Maybe Core.Natural)
vmFrameHeight = Lens.field @"frameHeight"
{-# DEPRECATED vmFrameHeight "Use generic-lens or generic-optics with 'frameHeight' instead." #-}

-- | Number of frames per second in the video.
--
-- /Note:/ Consider using 'frameRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmFrameRate :: Lens.Lens' VideoMetadata (Core.Maybe Core.Double)
vmFrameRate = Lens.field @"frameRate"
{-# DEPRECATED vmFrameRate "Use generic-lens or generic-optics with 'frameRate' instead." #-}

-- | Horizontal pixel dimension of the video.
--
-- /Note:/ Consider using 'frameWidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmFrameWidth :: Lens.Lens' VideoMetadata (Core.Maybe Core.Natural)
vmFrameWidth = Lens.field @"frameWidth"
{-# DEPRECATED vmFrameWidth "Use generic-lens or generic-optics with 'frameWidth' instead." #-}

instance Core.FromJSON VideoMetadata where
  parseJSON =
    Core.withObject "VideoMetadata" Core.$
      \x ->
        VideoMetadata'
          Core.<$> (x Core..:? "Codec")
          Core.<*> (x Core..:? "DurationMillis")
          Core.<*> (x Core..:? "Format")
          Core.<*> (x Core..:? "FrameHeight")
          Core.<*> (x Core..:? "FrameRate")
          Core.<*> (x Core..:? "FrameWidth")
