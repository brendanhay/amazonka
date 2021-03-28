{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.OutputSettings
  ( OutputSettings (..)
  -- * Smart constructor
  , mkOutputSettings
  -- * Lenses
  , osArchiveOutputSettings
  , osFrameCaptureOutputSettings
  , osHlsOutputSettings
  , osMediaPackageOutputSettings
  , osMsSmoothOutputSettings
  , osMultiplexOutputSettings
  , osRtmpOutputSettings
  , osUdpOutputSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.ArchiveOutputSettings as Types
import qualified Network.AWS.MediaLive.Types.FrameCaptureOutputSettings as Types
import qualified Network.AWS.MediaLive.Types.HlsOutputSettings as Types
import qualified Network.AWS.MediaLive.Types.MediaPackageOutputSettings as Types
import qualified Network.AWS.MediaLive.Types.MsSmoothOutputSettings as Types
import qualified Network.AWS.MediaLive.Types.MultiplexOutputSettings as Types
import qualified Network.AWS.MediaLive.Types.RtmpOutputSettings as Types
import qualified Network.AWS.MediaLive.Types.UdpOutputSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Output Settings
--
-- /See:/ 'mkOutputSettings' smart constructor.
data OutputSettings = OutputSettings'
  { archiveOutputSettings :: Core.Maybe Types.ArchiveOutputSettings
  , frameCaptureOutputSettings :: Core.Maybe Types.FrameCaptureOutputSettings
  , hlsOutputSettings :: Core.Maybe Types.HlsOutputSettings
  , mediaPackageOutputSettings :: Core.Maybe Types.MediaPackageOutputSettings
  , msSmoothOutputSettings :: Core.Maybe Types.MsSmoothOutputSettings
  , multiplexOutputSettings :: Core.Maybe Types.MultiplexOutputSettings
  , rtmpOutputSettings :: Core.Maybe Types.RtmpOutputSettings
  , udpOutputSettings :: Core.Maybe Types.UdpOutputSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputSettings' value with any optional fields omitted.
mkOutputSettings
    :: OutputSettings
mkOutputSettings
  = OutputSettings'{archiveOutputSettings = Core.Nothing,
                    frameCaptureOutputSettings = Core.Nothing,
                    hlsOutputSettings = Core.Nothing,
                    mediaPackageOutputSettings = Core.Nothing,
                    msSmoothOutputSettings = Core.Nothing,
                    multiplexOutputSettings = Core.Nothing,
                    rtmpOutputSettings = Core.Nothing,
                    udpOutputSettings = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'archiveOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osArchiveOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe Types.ArchiveOutputSettings)
osArchiveOutputSettings = Lens.field @"archiveOutputSettings"
{-# INLINEABLE osArchiveOutputSettings #-}
{-# DEPRECATED archiveOutputSettings "Use generic-lens or generic-optics with 'archiveOutputSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'frameCaptureOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osFrameCaptureOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe Types.FrameCaptureOutputSettings)
osFrameCaptureOutputSettings = Lens.field @"frameCaptureOutputSettings"
{-# INLINEABLE osFrameCaptureOutputSettings #-}
{-# DEPRECATED frameCaptureOutputSettings "Use generic-lens or generic-optics with 'frameCaptureOutputSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osHlsOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe Types.HlsOutputSettings)
osHlsOutputSettings = Lens.field @"hlsOutputSettings"
{-# INLINEABLE osHlsOutputSettings #-}
{-# DEPRECATED hlsOutputSettings "Use generic-lens or generic-optics with 'hlsOutputSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mediaPackageOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osMediaPackageOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe Types.MediaPackageOutputSettings)
osMediaPackageOutputSettings = Lens.field @"mediaPackageOutputSettings"
{-# INLINEABLE osMediaPackageOutputSettings #-}
{-# DEPRECATED mediaPackageOutputSettings "Use generic-lens or generic-optics with 'mediaPackageOutputSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'msSmoothOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osMsSmoothOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe Types.MsSmoothOutputSettings)
osMsSmoothOutputSettings = Lens.field @"msSmoothOutputSettings"
{-# INLINEABLE osMsSmoothOutputSettings #-}
{-# DEPRECATED msSmoothOutputSettings "Use generic-lens or generic-optics with 'msSmoothOutputSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'multiplexOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osMultiplexOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe Types.MultiplexOutputSettings)
osMultiplexOutputSettings = Lens.field @"multiplexOutputSettings"
{-# INLINEABLE osMultiplexOutputSettings #-}
{-# DEPRECATED multiplexOutputSettings "Use generic-lens or generic-optics with 'multiplexOutputSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rtmpOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osRtmpOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe Types.RtmpOutputSettings)
osRtmpOutputSettings = Lens.field @"rtmpOutputSettings"
{-# INLINEABLE osRtmpOutputSettings #-}
{-# DEPRECATED rtmpOutputSettings "Use generic-lens or generic-optics with 'rtmpOutputSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'udpOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUdpOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe Types.UdpOutputSettings)
osUdpOutputSettings = Lens.field @"udpOutputSettings"
{-# INLINEABLE osUdpOutputSettings #-}
{-# DEPRECATED udpOutputSettings "Use generic-lens or generic-optics with 'udpOutputSettings' instead"  #-}

instance Core.FromJSON OutputSettings where
        toJSON OutputSettings{..}
          = Core.object
              (Core.catMaybes
                 [("archiveOutputSettings" Core..=) Core.<$> archiveOutputSettings,
                  ("frameCaptureOutputSettings" Core..=) Core.<$>
                    frameCaptureOutputSettings,
                  ("hlsOutputSettings" Core..=) Core.<$> hlsOutputSettings,
                  ("mediaPackageOutputSettings" Core..=) Core.<$>
                    mediaPackageOutputSettings,
                  ("msSmoothOutputSettings" Core..=) Core.<$> msSmoothOutputSettings,
                  ("multiplexOutputSettings" Core..=) Core.<$>
                    multiplexOutputSettings,
                  ("rtmpOutputSettings" Core..=) Core.<$> rtmpOutputSettings,
                  ("udpOutputSettings" Core..=) Core.<$> udpOutputSettings])

instance Core.FromJSON OutputSettings where
        parseJSON
          = Core.withObject "OutputSettings" Core.$
              \ x ->
                OutputSettings' Core.<$>
                  (x Core..:? "archiveOutputSettings") Core.<*>
                    x Core..:? "frameCaptureOutputSettings"
                    Core.<*> x Core..:? "hlsOutputSettings"
                    Core.<*> x Core..:? "mediaPackageOutputSettings"
                    Core.<*> x Core..:? "msSmoothOutputSettings"
                    Core.<*> x Core..:? "multiplexOutputSettings"
                    Core.<*> x Core..:? "rtmpOutputSettings"
                    Core.<*> x Core..:? "udpOutputSettings"
