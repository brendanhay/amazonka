{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelectorSettings
  ( VideoSelectorSettings (..),

    -- * Smart constructor
    mkVideoSelectorSettings,

    -- * Lenses
    vssVideoSelectorPid,
    vssVideoSelectorProgramId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.VideoSelectorPid as Types
import qualified Network.AWS.MediaLive.Types.VideoSelectorProgramId as Types
import qualified Network.AWS.Prelude as Core

-- | Video Selector Settings
--
-- /See:/ 'mkVideoSelectorSettings' smart constructor.
data VideoSelectorSettings = VideoSelectorSettings'
  { videoSelectorPid :: Core.Maybe Types.VideoSelectorPid,
    videoSelectorProgramId :: Core.Maybe Types.VideoSelectorProgramId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VideoSelectorSettings' value with any optional fields omitted.
mkVideoSelectorSettings ::
  VideoSelectorSettings
mkVideoSelectorSettings =
  VideoSelectorSettings'
    { videoSelectorPid = Core.Nothing,
      videoSelectorProgramId = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'videoSelectorPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssVideoSelectorPid :: Lens.Lens' VideoSelectorSettings (Core.Maybe Types.VideoSelectorPid)
vssVideoSelectorPid = Lens.field @"videoSelectorPid"
{-# DEPRECATED vssVideoSelectorPid "Use generic-lens or generic-optics with 'videoSelectorPid' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'videoSelectorProgramId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssVideoSelectorProgramId :: Lens.Lens' VideoSelectorSettings (Core.Maybe Types.VideoSelectorProgramId)
vssVideoSelectorProgramId = Lens.field @"videoSelectorProgramId"
{-# DEPRECATED vssVideoSelectorProgramId "Use generic-lens or generic-optics with 'videoSelectorProgramId' instead." #-}

instance Core.FromJSON VideoSelectorSettings where
  toJSON VideoSelectorSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("videoSelectorPid" Core..=) Core.<$> videoSelectorPid,
            ("videoSelectorProgramId" Core..=)
              Core.<$> videoSelectorProgramId
          ]
      )

instance Core.FromJSON VideoSelectorSettings where
  parseJSON =
    Core.withObject "VideoSelectorSettings" Core.$
      \x ->
        VideoSelectorSettings'
          Core.<$> (x Core..:? "videoSelectorPid")
          Core.<*> (x Core..:? "videoSelectorProgramId")
