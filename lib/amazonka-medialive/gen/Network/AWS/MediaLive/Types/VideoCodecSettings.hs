{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoCodecSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.VideoCodecSettings
  ( VideoCodecSettings (..)
  -- * Smart constructor
  , mkVideoCodecSettings
  -- * Lenses
  , vcsFrameCaptureSettings
  , vcsH264Settings
  , vcsH265Settings
  , vcsMpeg2Settings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.FrameCaptureSettings as Types
import qualified Network.AWS.MediaLive.Types.H264Settings as Types
import qualified Network.AWS.MediaLive.Types.H265Settings as Types
import qualified Network.AWS.MediaLive.Types.Mpeg2Settings as Types
import qualified Network.AWS.Prelude as Core

-- | Video Codec Settings
--
-- /See:/ 'mkVideoCodecSettings' smart constructor.
data VideoCodecSettings = VideoCodecSettings'
  { frameCaptureSettings :: Core.Maybe Types.FrameCaptureSettings
  , h264Settings :: Core.Maybe Types.H264Settings
  , h265Settings :: Core.Maybe Types.H265Settings
  , mpeg2Settings :: Core.Maybe Types.Mpeg2Settings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VideoCodecSettings' value with any optional fields omitted.
mkVideoCodecSettings
    :: VideoCodecSettings
mkVideoCodecSettings
  = VideoCodecSettings'{frameCaptureSettings = Core.Nothing,
                        h264Settings = Core.Nothing, h265Settings = Core.Nothing,
                        mpeg2Settings = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'frameCaptureSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsFrameCaptureSettings :: Lens.Lens' VideoCodecSettings (Core.Maybe Types.FrameCaptureSettings)
vcsFrameCaptureSettings = Lens.field @"frameCaptureSettings"
{-# INLINEABLE vcsFrameCaptureSettings #-}
{-# DEPRECATED frameCaptureSettings "Use generic-lens or generic-optics with 'frameCaptureSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'h264Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsH264Settings :: Lens.Lens' VideoCodecSettings (Core.Maybe Types.H264Settings)
vcsH264Settings = Lens.field @"h264Settings"
{-# INLINEABLE vcsH264Settings #-}
{-# DEPRECATED h264Settings "Use generic-lens or generic-optics with 'h264Settings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'h265Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsH265Settings :: Lens.Lens' VideoCodecSettings (Core.Maybe Types.H265Settings)
vcsH265Settings = Lens.field @"h265Settings"
{-# INLINEABLE vcsH265Settings #-}
{-# DEPRECATED h265Settings "Use generic-lens or generic-optics with 'h265Settings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mpeg2Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsMpeg2Settings :: Lens.Lens' VideoCodecSettings (Core.Maybe Types.Mpeg2Settings)
vcsMpeg2Settings = Lens.field @"mpeg2Settings"
{-# INLINEABLE vcsMpeg2Settings #-}
{-# DEPRECATED mpeg2Settings "Use generic-lens or generic-optics with 'mpeg2Settings' instead"  #-}

instance Core.FromJSON VideoCodecSettings where
        toJSON VideoCodecSettings{..}
          = Core.object
              (Core.catMaybes
                 [("frameCaptureSettings" Core..=) Core.<$> frameCaptureSettings,
                  ("h264Settings" Core..=) Core.<$> h264Settings,
                  ("h265Settings" Core..=) Core.<$> h265Settings,
                  ("mpeg2Settings" Core..=) Core.<$> mpeg2Settings])

instance Core.FromJSON VideoCodecSettings where
        parseJSON
          = Core.withObject "VideoCodecSettings" Core.$
              \ x ->
                VideoCodecSettings' Core.<$>
                  (x Core..:? "frameCaptureSettings") Core.<*>
                    x Core..:? "h264Settings"
                    Core.<*> x Core..:? "h265Settings"
                    Core.<*> x Core..:? "mpeg2Settings"
