{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputSettings
  ( OutputSettings (..),

    -- * Smart constructor
    mkOutputSettings,

    -- * Lenses
    osMultiplexOutputSettings,
    osArchiveOutputSettings,
    osRtmpOutputSettings,
    osMediaPackageOutputSettings,
    osHlsOutputSettings,
    osFrameCaptureOutputSettings,
    osUdpOutputSettings,
    osMsSmoothOutputSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ArchiveOutputSettings
import Network.AWS.MediaLive.Types.FrameCaptureOutputSettings
import Network.AWS.MediaLive.Types.HlsOutputSettings
import Network.AWS.MediaLive.Types.MediaPackageOutputSettings
import Network.AWS.MediaLive.Types.MsSmoothOutputSettings
import Network.AWS.MediaLive.Types.MultiplexOutputSettings
import Network.AWS.MediaLive.Types.RtmpOutputSettings
import Network.AWS.MediaLive.Types.UdpOutputSettings
import qualified Network.AWS.Prelude as Lude

-- | Output Settings
--
-- /See:/ 'mkOutputSettings' smart constructor.
data OutputSettings = OutputSettings'
  { multiplexOutputSettings :: Lude.Maybe MultiplexOutputSettings,
    archiveOutputSettings :: Lude.Maybe ArchiveOutputSettings,
    rtmpOutputSettings :: Lude.Maybe RtmpOutputSettings,
    mediaPackageOutputSettings :: Lude.Maybe MediaPackageOutputSettings,
    hlsOutputSettings :: Lude.Maybe HlsOutputSettings,
    frameCaptureOutputSettings :: Lude.Maybe FrameCaptureOutputSettings,
    udpOutputSettings :: Lude.Maybe UdpOutputSettings,
    msSmoothOutputSettings :: Lude.Maybe MsSmoothOutputSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputSettings' with the minimum fields required to make a request.
--
-- * 'multiplexOutputSettings' -
-- * 'archiveOutputSettings' -
-- * 'rtmpOutputSettings' -
-- * 'mediaPackageOutputSettings' -
-- * 'hlsOutputSettings' -
-- * 'frameCaptureOutputSettings' -
-- * 'udpOutputSettings' -
-- * 'msSmoothOutputSettings' -
mkOutputSettings ::
  OutputSettings
mkOutputSettings =
  OutputSettings'
    { multiplexOutputSettings = Lude.Nothing,
      archiveOutputSettings = Lude.Nothing,
      rtmpOutputSettings = Lude.Nothing,
      mediaPackageOutputSettings = Lude.Nothing,
      hlsOutputSettings = Lude.Nothing,
      frameCaptureOutputSettings = Lude.Nothing,
      udpOutputSettings = Lude.Nothing,
      msSmoothOutputSettings = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'multiplexOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osMultiplexOutputSettings :: Lens.Lens' OutputSettings (Lude.Maybe MultiplexOutputSettings)
osMultiplexOutputSettings = Lens.lens (multiplexOutputSettings :: OutputSettings -> Lude.Maybe MultiplexOutputSettings) (\s a -> s {multiplexOutputSettings = a} :: OutputSettings)
{-# DEPRECATED osMultiplexOutputSettings "Use generic-lens or generic-optics with 'multiplexOutputSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'archiveOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osArchiveOutputSettings :: Lens.Lens' OutputSettings (Lude.Maybe ArchiveOutputSettings)
osArchiveOutputSettings = Lens.lens (archiveOutputSettings :: OutputSettings -> Lude.Maybe ArchiveOutputSettings) (\s a -> s {archiveOutputSettings = a} :: OutputSettings)
{-# DEPRECATED osArchiveOutputSettings "Use generic-lens or generic-optics with 'archiveOutputSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rtmpOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osRtmpOutputSettings :: Lens.Lens' OutputSettings (Lude.Maybe RtmpOutputSettings)
osRtmpOutputSettings = Lens.lens (rtmpOutputSettings :: OutputSettings -> Lude.Maybe RtmpOutputSettings) (\s a -> s {rtmpOutputSettings = a} :: OutputSettings)
{-# DEPRECATED osRtmpOutputSettings "Use generic-lens or generic-optics with 'rtmpOutputSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mediaPackageOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osMediaPackageOutputSettings :: Lens.Lens' OutputSettings (Lude.Maybe MediaPackageOutputSettings)
osMediaPackageOutputSettings = Lens.lens (mediaPackageOutputSettings :: OutputSettings -> Lude.Maybe MediaPackageOutputSettings) (\s a -> s {mediaPackageOutputSettings = a} :: OutputSettings)
{-# DEPRECATED osMediaPackageOutputSettings "Use generic-lens or generic-optics with 'mediaPackageOutputSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osHlsOutputSettings :: Lens.Lens' OutputSettings (Lude.Maybe HlsOutputSettings)
osHlsOutputSettings = Lens.lens (hlsOutputSettings :: OutputSettings -> Lude.Maybe HlsOutputSettings) (\s a -> s {hlsOutputSettings = a} :: OutputSettings)
{-# DEPRECATED osHlsOutputSettings "Use generic-lens or generic-optics with 'hlsOutputSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'frameCaptureOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osFrameCaptureOutputSettings :: Lens.Lens' OutputSettings (Lude.Maybe FrameCaptureOutputSettings)
osFrameCaptureOutputSettings = Lens.lens (frameCaptureOutputSettings :: OutputSettings -> Lude.Maybe FrameCaptureOutputSettings) (\s a -> s {frameCaptureOutputSettings = a} :: OutputSettings)
{-# DEPRECATED osFrameCaptureOutputSettings "Use generic-lens or generic-optics with 'frameCaptureOutputSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'udpOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUdpOutputSettings :: Lens.Lens' OutputSettings (Lude.Maybe UdpOutputSettings)
osUdpOutputSettings = Lens.lens (udpOutputSettings :: OutputSettings -> Lude.Maybe UdpOutputSettings) (\s a -> s {udpOutputSettings = a} :: OutputSettings)
{-# DEPRECATED osUdpOutputSettings "Use generic-lens or generic-optics with 'udpOutputSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'msSmoothOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osMsSmoothOutputSettings :: Lens.Lens' OutputSettings (Lude.Maybe MsSmoothOutputSettings)
osMsSmoothOutputSettings = Lens.lens (msSmoothOutputSettings :: OutputSettings -> Lude.Maybe MsSmoothOutputSettings) (\s a -> s {msSmoothOutputSettings = a} :: OutputSettings)
{-# DEPRECATED osMsSmoothOutputSettings "Use generic-lens or generic-optics with 'msSmoothOutputSettings' instead." #-}

instance Lude.FromJSON OutputSettings where
  parseJSON =
    Lude.withObject
      "OutputSettings"
      ( \x ->
          OutputSettings'
            Lude.<$> (x Lude..:? "multiplexOutputSettings")
            Lude.<*> (x Lude..:? "archiveOutputSettings")
            Lude.<*> (x Lude..:? "rtmpOutputSettings")
            Lude.<*> (x Lude..:? "mediaPackageOutputSettings")
            Lude.<*> (x Lude..:? "hlsOutputSettings")
            Lude.<*> (x Lude..:? "frameCaptureOutputSettings")
            Lude.<*> (x Lude..:? "udpOutputSettings")
            Lude.<*> (x Lude..:? "msSmoothOutputSettings")
      )

instance Lude.ToJSON OutputSettings where
  toJSON OutputSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("multiplexOutputSettings" Lude..=)
              Lude.<$> multiplexOutputSettings,
            ("archiveOutputSettings" Lude..=) Lude.<$> archiveOutputSettings,
            ("rtmpOutputSettings" Lude..=) Lude.<$> rtmpOutputSettings,
            ("mediaPackageOutputSettings" Lude..=)
              Lude.<$> mediaPackageOutputSettings,
            ("hlsOutputSettings" Lude..=) Lude.<$> hlsOutputSettings,
            ("frameCaptureOutputSettings" Lude..=)
              Lude.<$> frameCaptureOutputSettings,
            ("udpOutputSettings" Lude..=) Lude.<$> udpOutputSettings,
            ("msSmoothOutputSettings" Lude..=)
              Lude.<$> msSmoothOutputSettings
          ]
      )
