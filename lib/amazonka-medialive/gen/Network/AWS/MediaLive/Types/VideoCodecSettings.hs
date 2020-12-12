{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoCodecSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoCodecSettings
  ( VideoCodecSettings (..),

    -- * Smart constructor
    mkVideoCodecSettings,

    -- * Lenses
    vcsFrameCaptureSettings,
    vcsH265Settings,
    vcsH264Settings,
    vcsMpeg2Settings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FrameCaptureSettings
import Network.AWS.MediaLive.Types.H264Settings
import Network.AWS.MediaLive.Types.H265Settings
import Network.AWS.MediaLive.Types.Mpeg2Settings
import qualified Network.AWS.Prelude as Lude

-- | Video Codec Settings
--
-- /See:/ 'mkVideoCodecSettings' smart constructor.
data VideoCodecSettings = VideoCodecSettings'
  { frameCaptureSettings ::
      Lude.Maybe FrameCaptureSettings,
    h265Settings :: Lude.Maybe H265Settings,
    h264Settings :: Lude.Maybe H264Settings,
    mpeg2Settings :: Lude.Maybe Mpeg2Settings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VideoCodecSettings' with the minimum fields required to make a request.
--
-- * 'frameCaptureSettings' - Undocumented field.
-- * 'h264Settings' - Undocumented field.
-- * 'h265Settings' - Undocumented field.
-- * 'mpeg2Settings' - Undocumented field.
mkVideoCodecSettings ::
  VideoCodecSettings
mkVideoCodecSettings =
  VideoCodecSettings'
    { frameCaptureSettings = Lude.Nothing,
      h265Settings = Lude.Nothing,
      h264Settings = Lude.Nothing,
      mpeg2Settings = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'frameCaptureSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsFrameCaptureSettings :: Lens.Lens' VideoCodecSettings (Lude.Maybe FrameCaptureSettings)
vcsFrameCaptureSettings = Lens.lens (frameCaptureSettings :: VideoCodecSettings -> Lude.Maybe FrameCaptureSettings) (\s a -> s {frameCaptureSettings = a} :: VideoCodecSettings)
{-# DEPRECATED vcsFrameCaptureSettings "Use generic-lens or generic-optics with 'frameCaptureSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'h265Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsH265Settings :: Lens.Lens' VideoCodecSettings (Lude.Maybe H265Settings)
vcsH265Settings = Lens.lens (h265Settings :: VideoCodecSettings -> Lude.Maybe H265Settings) (\s a -> s {h265Settings = a} :: VideoCodecSettings)
{-# DEPRECATED vcsH265Settings "Use generic-lens or generic-optics with 'h265Settings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'h264Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsH264Settings :: Lens.Lens' VideoCodecSettings (Lude.Maybe H264Settings)
vcsH264Settings = Lens.lens (h264Settings :: VideoCodecSettings -> Lude.Maybe H264Settings) (\s a -> s {h264Settings = a} :: VideoCodecSettings)
{-# DEPRECATED vcsH264Settings "Use generic-lens or generic-optics with 'h264Settings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mpeg2Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsMpeg2Settings :: Lens.Lens' VideoCodecSettings (Lude.Maybe Mpeg2Settings)
vcsMpeg2Settings = Lens.lens (mpeg2Settings :: VideoCodecSettings -> Lude.Maybe Mpeg2Settings) (\s a -> s {mpeg2Settings = a} :: VideoCodecSettings)
{-# DEPRECATED vcsMpeg2Settings "Use generic-lens or generic-optics with 'mpeg2Settings' instead." #-}

instance Lude.FromJSON VideoCodecSettings where
  parseJSON =
    Lude.withObject
      "VideoCodecSettings"
      ( \x ->
          VideoCodecSettings'
            Lude.<$> (x Lude..:? "frameCaptureSettings")
            Lude.<*> (x Lude..:? "h265Settings")
            Lude.<*> (x Lude..:? "h264Settings")
            Lude.<*> (x Lude..:? "mpeg2Settings")
      )

instance Lude.ToJSON VideoCodecSettings where
  toJSON VideoCodecSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("frameCaptureSettings" Lude..=) Lude.<$> frameCaptureSettings,
            ("h265Settings" Lude..=) Lude.<$> h265Settings,
            ("h264Settings" Lude..=) Lude.<$> h264Settings,
            ("mpeg2Settings" Lude..=) Lude.<$> mpeg2Settings
          ]
      )
