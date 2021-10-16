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
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorPid
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelectorPid where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Video Selector Pid
--
-- /See:/ 'newVideoSelectorPid' smart constructor.
data VideoSelectorPid = VideoSelectorPid'
  { -- | Selects a specific PID from within a video source.
    pid :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoSelectorPid' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pid', 'videoSelectorPid_pid' - Selects a specific PID from within a video source.
newVideoSelectorPid ::
  VideoSelectorPid
newVideoSelectorPid =
  VideoSelectorPid' {pid = Prelude.Nothing}

-- | Selects a specific PID from within a video source.
videoSelectorPid_pid :: Lens.Lens' VideoSelectorPid (Prelude.Maybe Prelude.Natural)
videoSelectorPid_pid = Lens.lens (\VideoSelectorPid' {pid} -> pid) (\s@VideoSelectorPid' {} a -> s {pid = a} :: VideoSelectorPid)

instance Core.FromJSON VideoSelectorPid where
  parseJSON =
    Core.withObject
      "VideoSelectorPid"
      ( \x ->
          VideoSelectorPid' Prelude.<$> (x Core..:? "pid")
      )

instance Prelude.Hashable VideoSelectorPid

instance Prelude.NFData VideoSelectorPid

instance Core.ToJSON VideoSelectorPid where
  toJSON VideoSelectorPid' {..} =
    Core.object
      (Prelude.catMaybes [("pid" Core..=) Prelude.<$> pid])
