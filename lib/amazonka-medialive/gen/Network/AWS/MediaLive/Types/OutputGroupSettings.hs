-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputGroupSettings
  ( OutputGroupSettings (..),

    -- * Smart constructor
    mkOutputGroupSettings,

    -- * Lenses
    ogsMediaPackageGroupSettings,
    ogsMsSmoothGroupSettings,
    ogsRtmpGroupSettings,
    ogsMultiplexGroupSettings,
    ogsHlsGroupSettings,
    ogsArchiveGroupSettings,
    ogsUdpGroupSettings,
    ogsFrameCaptureGroupSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ArchiveGroupSettings
import Network.AWS.MediaLive.Types.FrameCaptureGroupSettings
import Network.AWS.MediaLive.Types.HlsGroupSettings
import Network.AWS.MediaLive.Types.MediaPackageGroupSettings
import Network.AWS.MediaLive.Types.MsSmoothGroupSettings
import Network.AWS.MediaLive.Types.MultiplexGroupSettings
import Network.AWS.MediaLive.Types.RtmpGroupSettings
import Network.AWS.MediaLive.Types.UdpGroupSettings
import qualified Network.AWS.Prelude as Lude

-- | Output Group Settings
--
-- /See:/ 'mkOutputGroupSettings' smart constructor.
data OutputGroupSettings = OutputGroupSettings'
  { mediaPackageGroupSettings ::
      Lude.Maybe MediaPackageGroupSettings,
    msSmoothGroupSettings ::
      Lude.Maybe MsSmoothGroupSettings,
    rtmpGroupSettings :: Lude.Maybe RtmpGroupSettings,
    multiplexGroupSettings ::
      Lude.Maybe MultiplexGroupSettings,
    hlsGroupSettings :: Lude.Maybe HlsGroupSettings,
    archiveGroupSettings ::
      Lude.Maybe ArchiveGroupSettings,
    udpGroupSettings :: Lude.Maybe UdpGroupSettings,
    frameCaptureGroupSettings ::
      Lude.Maybe FrameCaptureGroupSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputGroupSettings' with the minimum fields required to make a request.
--
-- * 'archiveGroupSettings' - Undocumented field.
-- * 'frameCaptureGroupSettings' - Undocumented field.
-- * 'hlsGroupSettings' - Undocumented field.
-- * 'mediaPackageGroupSettings' - Undocumented field.
-- * 'msSmoothGroupSettings' - Undocumented field.
-- * 'multiplexGroupSettings' - Undocumented field.
-- * 'rtmpGroupSettings' - Undocumented field.
-- * 'udpGroupSettings' - Undocumented field.
mkOutputGroupSettings ::
  OutputGroupSettings
mkOutputGroupSettings =
  OutputGroupSettings'
    { mediaPackageGroupSettings = Lude.Nothing,
      msSmoothGroupSettings = Lude.Nothing,
      rtmpGroupSettings = Lude.Nothing,
      multiplexGroupSettings = Lude.Nothing,
      hlsGroupSettings = Lude.Nothing,
      archiveGroupSettings = Lude.Nothing,
      udpGroupSettings = Lude.Nothing,
      frameCaptureGroupSettings = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'mediaPackageGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsMediaPackageGroupSettings :: Lens.Lens' OutputGroupSettings (Lude.Maybe MediaPackageGroupSettings)
ogsMediaPackageGroupSettings = Lens.lens (mediaPackageGroupSettings :: OutputGroupSettings -> Lude.Maybe MediaPackageGroupSettings) (\s a -> s {mediaPackageGroupSettings = a} :: OutputGroupSettings)
{-# DEPRECATED ogsMediaPackageGroupSettings "Use generic-lens or generic-optics with 'mediaPackageGroupSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'msSmoothGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsMsSmoothGroupSettings :: Lens.Lens' OutputGroupSettings (Lude.Maybe MsSmoothGroupSettings)
ogsMsSmoothGroupSettings = Lens.lens (msSmoothGroupSettings :: OutputGroupSettings -> Lude.Maybe MsSmoothGroupSettings) (\s a -> s {msSmoothGroupSettings = a} :: OutputGroupSettings)
{-# DEPRECATED ogsMsSmoothGroupSettings "Use generic-lens or generic-optics with 'msSmoothGroupSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rtmpGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsRtmpGroupSettings :: Lens.Lens' OutputGroupSettings (Lude.Maybe RtmpGroupSettings)
ogsRtmpGroupSettings = Lens.lens (rtmpGroupSettings :: OutputGroupSettings -> Lude.Maybe RtmpGroupSettings) (\s a -> s {rtmpGroupSettings = a} :: OutputGroupSettings)
{-# DEPRECATED ogsRtmpGroupSettings "Use generic-lens or generic-optics with 'rtmpGroupSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'multiplexGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsMultiplexGroupSettings :: Lens.Lens' OutputGroupSettings (Lude.Maybe MultiplexGroupSettings)
ogsMultiplexGroupSettings = Lens.lens (multiplexGroupSettings :: OutputGroupSettings -> Lude.Maybe MultiplexGroupSettings) (\s a -> s {multiplexGroupSettings = a} :: OutputGroupSettings)
{-# DEPRECATED ogsMultiplexGroupSettings "Use generic-lens or generic-optics with 'multiplexGroupSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsHlsGroupSettings :: Lens.Lens' OutputGroupSettings (Lude.Maybe HlsGroupSettings)
ogsHlsGroupSettings = Lens.lens (hlsGroupSettings :: OutputGroupSettings -> Lude.Maybe HlsGroupSettings) (\s a -> s {hlsGroupSettings = a} :: OutputGroupSettings)
{-# DEPRECATED ogsHlsGroupSettings "Use generic-lens or generic-optics with 'hlsGroupSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'archiveGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsArchiveGroupSettings :: Lens.Lens' OutputGroupSettings (Lude.Maybe ArchiveGroupSettings)
ogsArchiveGroupSettings = Lens.lens (archiveGroupSettings :: OutputGroupSettings -> Lude.Maybe ArchiveGroupSettings) (\s a -> s {archiveGroupSettings = a} :: OutputGroupSettings)
{-# DEPRECATED ogsArchiveGroupSettings "Use generic-lens or generic-optics with 'archiveGroupSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'udpGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsUdpGroupSettings :: Lens.Lens' OutputGroupSettings (Lude.Maybe UdpGroupSettings)
ogsUdpGroupSettings = Lens.lens (udpGroupSettings :: OutputGroupSettings -> Lude.Maybe UdpGroupSettings) (\s a -> s {udpGroupSettings = a} :: OutputGroupSettings)
{-# DEPRECATED ogsUdpGroupSettings "Use generic-lens or generic-optics with 'udpGroupSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'frameCaptureGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsFrameCaptureGroupSettings :: Lens.Lens' OutputGroupSettings (Lude.Maybe FrameCaptureGroupSettings)
ogsFrameCaptureGroupSettings = Lens.lens (frameCaptureGroupSettings :: OutputGroupSettings -> Lude.Maybe FrameCaptureGroupSettings) (\s a -> s {frameCaptureGroupSettings = a} :: OutputGroupSettings)
{-# DEPRECATED ogsFrameCaptureGroupSettings "Use generic-lens or generic-optics with 'frameCaptureGroupSettings' instead." #-}

instance Lude.FromJSON OutputGroupSettings where
  parseJSON =
    Lude.withObject
      "OutputGroupSettings"
      ( \x ->
          OutputGroupSettings'
            Lude.<$> (x Lude..:? "mediaPackageGroupSettings")
            Lude.<*> (x Lude..:? "msSmoothGroupSettings")
            Lude.<*> (x Lude..:? "rtmpGroupSettings")
            Lude.<*> (x Lude..:? "multiplexGroupSettings")
            Lude.<*> (x Lude..:? "hlsGroupSettings")
            Lude.<*> (x Lude..:? "archiveGroupSettings")
            Lude.<*> (x Lude..:? "udpGroupSettings")
            Lude.<*> (x Lude..:? "frameCaptureGroupSettings")
      )

instance Lude.ToJSON OutputGroupSettings where
  toJSON OutputGroupSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("mediaPackageGroupSettings" Lude..=)
              Lude.<$> mediaPackageGroupSettings,
            ("msSmoothGroupSettings" Lude..=) Lude.<$> msSmoothGroupSettings,
            ("rtmpGroupSettings" Lude..=) Lude.<$> rtmpGroupSettings,
            ("multiplexGroupSettings" Lude..=) Lude.<$> multiplexGroupSettings,
            ("hlsGroupSettings" Lude..=) Lude.<$> hlsGroupSettings,
            ("archiveGroupSettings" Lude..=) Lude.<$> archiveGroupSettings,
            ("udpGroupSettings" Lude..=) Lude.<$> udpGroupSettings,
            ("frameCaptureGroupSettings" Lude..=)
              Lude.<$> frameCaptureGroupSettings
          ]
      )
