{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.VideoSelectorPid
import Network.AWS.MediaLive.Types.VideoSelectorProgramId
import qualified Network.AWS.Prelude as Prelude

-- | Video Selector Settings
--
-- /See:/ 'newVideoSelectorSettings' smart constructor.
data VideoSelectorSettings = VideoSelectorSettings'
  { videoSelectorPid :: Prelude.Maybe VideoSelectorPid,
    videoSelectorProgramId :: Prelude.Maybe VideoSelectorProgramId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      videoSelectorProgramId = Prelude.Nothing
    }

-- | Undocumented member.
videoSelectorSettings_videoSelectorPid :: Lens.Lens' VideoSelectorSettings (Prelude.Maybe VideoSelectorPid)
videoSelectorSettings_videoSelectorPid = Lens.lens (\VideoSelectorSettings' {videoSelectorPid} -> videoSelectorPid) (\s@VideoSelectorSettings' {} a -> s {videoSelectorPid = a} :: VideoSelectorSettings)

-- | Undocumented member.
videoSelectorSettings_videoSelectorProgramId :: Lens.Lens' VideoSelectorSettings (Prelude.Maybe VideoSelectorProgramId)
videoSelectorSettings_videoSelectorProgramId = Lens.lens (\VideoSelectorSettings' {videoSelectorProgramId} -> videoSelectorProgramId) (\s@VideoSelectorSettings' {} a -> s {videoSelectorProgramId = a} :: VideoSelectorSettings)

instance Prelude.FromJSON VideoSelectorSettings where
  parseJSON =
    Prelude.withObject
      "VideoSelectorSettings"
      ( \x ->
          VideoSelectorSettings'
            Prelude.<$> (x Prelude..:? "videoSelectorPid")
            Prelude.<*> (x Prelude..:? "videoSelectorProgramId")
      )

instance Prelude.Hashable VideoSelectorSettings

instance Prelude.NFData VideoSelectorSettings

instance Prelude.ToJSON VideoSelectorSettings where
  toJSON VideoSelectorSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("videoSelectorPid" Prelude..=)
              Prelude.<$> videoSelectorPid,
            ("videoSelectorProgramId" Prelude..=)
              Prelude.<$> videoSelectorProgramId
          ]
      )
