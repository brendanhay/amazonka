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
-- Module      : Amazonka.MediaLive.Types.VideoSelectorSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.VideoSelectorSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.VideoSelectorPid
import Amazonka.MediaLive.Types.VideoSelectorProgramId
import qualified Amazonka.Prelude as Prelude

-- | Video Selector Settings
--
-- /See:/ 'newVideoSelectorSettings' smart constructor.
data VideoSelectorSettings = VideoSelectorSettings'
  { videoSelectorPid :: Prelude.Maybe VideoSelectorPid,
    videoSelectorProgramId :: Prelude.Maybe VideoSelectorProgramId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON VideoSelectorSettings where
  parseJSON =
    Data.withObject
      "VideoSelectorSettings"
      ( \x ->
          VideoSelectorSettings'
            Prelude.<$> (x Data..:? "videoSelectorPid")
            Prelude.<*> (x Data..:? "videoSelectorProgramId")
      )

instance Prelude.Hashable VideoSelectorSettings where
  hashWithSalt _salt VideoSelectorSettings' {..} =
    _salt
      `Prelude.hashWithSalt` videoSelectorPid
      `Prelude.hashWithSalt` videoSelectorProgramId

instance Prelude.NFData VideoSelectorSettings where
  rnf VideoSelectorSettings' {..} =
    Prelude.rnf videoSelectorPid
      `Prelude.seq` Prelude.rnf videoSelectorProgramId

instance Data.ToJSON VideoSelectorSettings where
  toJSON VideoSelectorSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("videoSelectorPid" Data..=)
              Prelude.<$> videoSelectorPid,
            ("videoSelectorProgramId" Data..=)
              Prelude.<$> videoSelectorProgramId
          ]
      )
