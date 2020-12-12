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
    vmFrameRate,
    vmFormat,
    vmCodec,
    vmFrameHeight,
    vmDurationMillis,
    vmFrameWidth,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition video operation.
--
-- /See:/ 'mkVideoMetadata' smart constructor.
data VideoMetadata = VideoMetadata'
  { frameRate ::
      Lude.Maybe Lude.Double,
    format :: Lude.Maybe Lude.Text,
    codec :: Lude.Maybe Lude.Text,
    frameHeight :: Lude.Maybe Lude.Natural,
    durationMillis :: Lude.Maybe Lude.Natural,
    frameWidth :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VideoMetadata' with the minimum fields required to make a request.
--
-- * 'codec' - Type of compression used in the analyzed video.
-- * 'durationMillis' - Length of the video in milliseconds.
-- * 'format' - Format of the analyzed video. Possible values are MP4, MOV and AVI.
-- * 'frameHeight' - Vertical pixel dimension of the video.
-- * 'frameRate' - Number of frames per second in the video.
-- * 'frameWidth' - Horizontal pixel dimension of the video.
mkVideoMetadata ::
  VideoMetadata
mkVideoMetadata =
  VideoMetadata'
    { frameRate = Lude.Nothing,
      format = Lude.Nothing,
      codec = Lude.Nothing,
      frameHeight = Lude.Nothing,
      durationMillis = Lude.Nothing,
      frameWidth = Lude.Nothing
    }

-- | Number of frames per second in the video.
--
-- /Note:/ Consider using 'frameRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmFrameRate :: Lens.Lens' VideoMetadata (Lude.Maybe Lude.Double)
vmFrameRate = Lens.lens (frameRate :: VideoMetadata -> Lude.Maybe Lude.Double) (\s a -> s {frameRate = a} :: VideoMetadata)
{-# DEPRECATED vmFrameRate "Use generic-lens or generic-optics with 'frameRate' instead." #-}

-- | Format of the analyzed video. Possible values are MP4, MOV and AVI.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmFormat :: Lens.Lens' VideoMetadata (Lude.Maybe Lude.Text)
vmFormat = Lens.lens (format :: VideoMetadata -> Lude.Maybe Lude.Text) (\s a -> s {format = a} :: VideoMetadata)
{-# DEPRECATED vmFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | Type of compression used in the analyzed video.
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmCodec :: Lens.Lens' VideoMetadata (Lude.Maybe Lude.Text)
vmCodec = Lens.lens (codec :: VideoMetadata -> Lude.Maybe Lude.Text) (\s a -> s {codec = a} :: VideoMetadata)
{-# DEPRECATED vmCodec "Use generic-lens or generic-optics with 'codec' instead." #-}

-- | Vertical pixel dimension of the video.
--
-- /Note:/ Consider using 'frameHeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmFrameHeight :: Lens.Lens' VideoMetadata (Lude.Maybe Lude.Natural)
vmFrameHeight = Lens.lens (frameHeight :: VideoMetadata -> Lude.Maybe Lude.Natural) (\s a -> s {frameHeight = a} :: VideoMetadata)
{-# DEPRECATED vmFrameHeight "Use generic-lens or generic-optics with 'frameHeight' instead." #-}

-- | Length of the video in milliseconds.
--
-- /Note:/ Consider using 'durationMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmDurationMillis :: Lens.Lens' VideoMetadata (Lude.Maybe Lude.Natural)
vmDurationMillis = Lens.lens (durationMillis :: VideoMetadata -> Lude.Maybe Lude.Natural) (\s a -> s {durationMillis = a} :: VideoMetadata)
{-# DEPRECATED vmDurationMillis "Use generic-lens or generic-optics with 'durationMillis' instead." #-}

-- | Horizontal pixel dimension of the video.
--
-- /Note:/ Consider using 'frameWidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmFrameWidth :: Lens.Lens' VideoMetadata (Lude.Maybe Lude.Natural)
vmFrameWidth = Lens.lens (frameWidth :: VideoMetadata -> Lude.Maybe Lude.Natural) (\s a -> s {frameWidth = a} :: VideoMetadata)
{-# DEPRECATED vmFrameWidth "Use generic-lens or generic-optics with 'frameWidth' instead." #-}

instance Lude.FromJSON VideoMetadata where
  parseJSON =
    Lude.withObject
      "VideoMetadata"
      ( \x ->
          VideoMetadata'
            Lude.<$> (x Lude..:? "FrameRate")
            Lude.<*> (x Lude..:? "Format")
            Lude.<*> (x Lude..:? "Codec")
            Lude.<*> (x Lude..:? "FrameHeight")
            Lude.<*> (x Lude..:? "DurationMillis")
            Lude.<*> (x Lude..:? "FrameWidth")
      )
