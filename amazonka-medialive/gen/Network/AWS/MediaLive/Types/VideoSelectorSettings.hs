{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelectorSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.VideoSelectorPid
import Network.AWS.MediaLive.Types.VideoSelectorProgramId

-- | Video Selector Settings
--
-- /See:/ 'newVideoSelectorSettings' smart constructor.
data VideoSelectorSettings = VideoSelectorSettings'
  { videoSelectorPid :: Core.Maybe VideoSelectorPid,
    videoSelectorProgramId :: Core.Maybe VideoSelectorProgramId
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VideoSelectorSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'videoSelectorPid', 'videoSelectorSettings_videoSelectorPid' - Undocumented member.
--
-- 'videoSelectorProgramId', 'videoSelectorSettings_videoSelectorProgramId' - Undocumented member.
newVideoSelectorSettings ::
  VideoSelectorSettings
newVideoSelectorSettings =
  VideoSelectorSettings'
    { videoSelectorPid =
        Core.Nothing,
      videoSelectorProgramId = Core.Nothing
    }

-- | Undocumented member.
videoSelectorSettings_videoSelectorPid :: Lens.Lens' VideoSelectorSettings (Core.Maybe VideoSelectorPid)
videoSelectorSettings_videoSelectorPid = Lens.lens (\VideoSelectorSettings' {videoSelectorPid} -> videoSelectorPid) (\s@VideoSelectorSettings' {} a -> s {videoSelectorPid = a} :: VideoSelectorSettings)

-- | Undocumented member.
videoSelectorSettings_videoSelectorProgramId :: Lens.Lens' VideoSelectorSettings (Core.Maybe VideoSelectorProgramId)
videoSelectorSettings_videoSelectorProgramId = Lens.lens (\VideoSelectorSettings' {videoSelectorProgramId} -> videoSelectorProgramId) (\s@VideoSelectorSettings' {} a -> s {videoSelectorProgramId = a} :: VideoSelectorSettings)

instance Core.FromJSON VideoSelectorSettings where
  parseJSON =
    Core.withObject
      "VideoSelectorSettings"
      ( \x ->
          VideoSelectorSettings'
            Core.<$> (x Core..:? "videoSelectorPid")
            Core.<*> (x Core..:? "videoSelectorProgramId")
      )

instance Core.Hashable VideoSelectorSettings

instance Core.NFData VideoSelectorSettings

instance Core.ToJSON VideoSelectorSettings where
  toJSON VideoSelectorSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("videoSelectorPid" Core..=)
              Core.<$> videoSelectorPid,
            ("videoSelectorProgramId" Core..=)
              Core.<$> videoSelectorProgramId
          ]
      )
