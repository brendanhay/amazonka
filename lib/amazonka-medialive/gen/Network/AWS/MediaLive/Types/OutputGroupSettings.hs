{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ogsArchiveGroupSettings,
    ogsFrameCaptureGroupSettings,
    ogsHlsGroupSettings,
    ogsMediaPackageGroupSettings,
    ogsMsSmoothGroupSettings,
    ogsMultiplexGroupSettings,
    ogsRtmpGroupSettings,
    ogsUdpGroupSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.ArchiveGroupSettings as Types
import qualified Network.AWS.MediaLive.Types.FrameCaptureGroupSettings as Types
import qualified Network.AWS.MediaLive.Types.HlsGroupSettings as Types
import qualified Network.AWS.MediaLive.Types.MediaPackageGroupSettings as Types
import qualified Network.AWS.MediaLive.Types.MsSmoothGroupSettings as Types
import qualified Network.AWS.MediaLive.Types.MultiplexGroupSettings as Types
import qualified Network.AWS.MediaLive.Types.RtmpGroupSettings as Types
import qualified Network.AWS.MediaLive.Types.UdpGroupSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Output Group Settings
--
-- /See:/ 'mkOutputGroupSettings' smart constructor.
data OutputGroupSettings = OutputGroupSettings'
  { archiveGroupSettings :: Core.Maybe Types.ArchiveGroupSettings,
    frameCaptureGroupSettings :: Core.Maybe Types.FrameCaptureGroupSettings,
    hlsGroupSettings :: Core.Maybe Types.HlsGroupSettings,
    mediaPackageGroupSettings :: Core.Maybe Types.MediaPackageGroupSettings,
    msSmoothGroupSettings :: Core.Maybe Types.MsSmoothGroupSettings,
    multiplexGroupSettings :: Core.Maybe Types.MultiplexGroupSettings,
    rtmpGroupSettings :: Core.Maybe Types.RtmpGroupSettings,
    udpGroupSettings :: Core.Maybe Types.UdpGroupSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputGroupSettings' value with any optional fields omitted.
mkOutputGroupSettings ::
  OutputGroupSettings
mkOutputGroupSettings =
  OutputGroupSettings'
    { archiveGroupSettings = Core.Nothing,
      frameCaptureGroupSettings = Core.Nothing,
      hlsGroupSettings = Core.Nothing,
      mediaPackageGroupSettings = Core.Nothing,
      msSmoothGroupSettings = Core.Nothing,
      multiplexGroupSettings = Core.Nothing,
      rtmpGroupSettings = Core.Nothing,
      udpGroupSettings = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'archiveGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsArchiveGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe Types.ArchiveGroupSettings)
ogsArchiveGroupSettings = Lens.field @"archiveGroupSettings"
{-# DEPRECATED ogsArchiveGroupSettings "Use generic-lens or generic-optics with 'archiveGroupSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'frameCaptureGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsFrameCaptureGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe Types.FrameCaptureGroupSettings)
ogsFrameCaptureGroupSettings = Lens.field @"frameCaptureGroupSettings"
{-# DEPRECATED ogsFrameCaptureGroupSettings "Use generic-lens or generic-optics with 'frameCaptureGroupSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsHlsGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe Types.HlsGroupSettings)
ogsHlsGroupSettings = Lens.field @"hlsGroupSettings"
{-# DEPRECATED ogsHlsGroupSettings "Use generic-lens or generic-optics with 'hlsGroupSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mediaPackageGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsMediaPackageGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe Types.MediaPackageGroupSettings)
ogsMediaPackageGroupSettings = Lens.field @"mediaPackageGroupSettings"
{-# DEPRECATED ogsMediaPackageGroupSettings "Use generic-lens or generic-optics with 'mediaPackageGroupSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'msSmoothGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsMsSmoothGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe Types.MsSmoothGroupSettings)
ogsMsSmoothGroupSettings = Lens.field @"msSmoothGroupSettings"
{-# DEPRECATED ogsMsSmoothGroupSettings "Use generic-lens or generic-optics with 'msSmoothGroupSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'multiplexGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsMultiplexGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe Types.MultiplexGroupSettings)
ogsMultiplexGroupSettings = Lens.field @"multiplexGroupSettings"
{-# DEPRECATED ogsMultiplexGroupSettings "Use generic-lens or generic-optics with 'multiplexGroupSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rtmpGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsRtmpGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe Types.RtmpGroupSettings)
ogsRtmpGroupSettings = Lens.field @"rtmpGroupSettings"
{-# DEPRECATED ogsRtmpGroupSettings "Use generic-lens or generic-optics with 'rtmpGroupSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'udpGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsUdpGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe Types.UdpGroupSettings)
ogsUdpGroupSettings = Lens.field @"udpGroupSettings"
{-# DEPRECATED ogsUdpGroupSettings "Use generic-lens or generic-optics with 'udpGroupSettings' instead." #-}

instance Core.FromJSON OutputGroupSettings where
  toJSON OutputGroupSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("archiveGroupSettings" Core..=) Core.<$> archiveGroupSettings,
            ("frameCaptureGroupSettings" Core..=)
              Core.<$> frameCaptureGroupSettings,
            ("hlsGroupSettings" Core..=) Core.<$> hlsGroupSettings,
            ("mediaPackageGroupSettings" Core..=)
              Core.<$> mediaPackageGroupSettings,
            ("msSmoothGroupSettings" Core..=) Core.<$> msSmoothGroupSettings,
            ("multiplexGroupSettings" Core..=) Core.<$> multiplexGroupSettings,
            ("rtmpGroupSettings" Core..=) Core.<$> rtmpGroupSettings,
            ("udpGroupSettings" Core..=) Core.<$> udpGroupSettings
          ]
      )

instance Core.FromJSON OutputGroupSettings where
  parseJSON =
    Core.withObject "OutputGroupSettings" Core.$
      \x ->
        OutputGroupSettings'
          Core.<$> (x Core..:? "archiveGroupSettings")
          Core.<*> (x Core..:? "frameCaptureGroupSettings")
          Core.<*> (x Core..:? "hlsGroupSettings")
          Core.<*> (x Core..:? "mediaPackageGroupSettings")
          Core.<*> (x Core..:? "msSmoothGroupSettings")
          Core.<*> (x Core..:? "multiplexGroupSettings")
          Core.<*> (x Core..:? "rtmpGroupSettings")
          Core.<*> (x Core..:? "udpGroupSettings")
